----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicRenaming.all;


entity UnitRegManager is
port(
    clk: in std_logic;
    
    renameAccepting: out std_logic;
    --frontSendingIn: in std_logic;
    frontData: in BufferEntryArray;
    frontCtrl: in ControlPacket;


    bqPointer: in SmallNumber;
    sqPointer: in SmallNumber;
    lqPointer: in SmallNumber;
    bqPointerSeq: in SmallNumber;


    nextAccepting: in std_logic;

    renamedArgsInt: out RenameInfoArray(0 to PIPE_WIDTH-1);
    renamedArgsFloat: out RenameInfoArray(0 to PIPE_WIDTH-1);
    
    renamedDataLiving: out InstructionSlotArray(0 to PIPE_WIDTH-1);
    renamedCtrl: out ControlPacket;

    --renamedSending: out std_logic;


    --sendingFromROB: in std_logic;
    robData: in ControlPacketArray(0 to PIPE_WIDTH-1);
    robCtrl: in ControlPacket;

    commitArgInfoI: in RenameInfoArray(0 to PIPE_WIDTH-1);
    commitArgInfoF: in RenameInfoArray(0 to PIPE_WIDTH-1);

    renameGroupCtrNextOut: out InsTag;

	events: in EventState;
	events_T: in EventState;

    dbState: in DbCoreState
);
end UnitRegManager;


architecture Behavioral of UnitRegManager is
    alias execEventSignal is events.execCausing.full;
    alias lateEventSignal is events.lateCausing.full;

    signal renamedBase, renamedDataLivingPre: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, robSendingDelayed, frontLastSending, renameFull, renamedSendingSig, hasBranch,
               renameLockState, renameLockStateNext, renameLockEnd, renameLockEndDelayed, renameLockRelease, renameLockReleaseDelayed, renameLockEndDelayedNext,
                 ch0, ch1, ch2, ch3, ch4, ch5
               : std_logic := '0';

    signal renameGroupCtr, renameGroupCtrNext, commitGroupCtr, commitGroupCtrNext: InsTag := INITIAL_GROUP_TAG;

    signal renameCtr, renameCtrNext: Word := (others => '0'); -- DB

    signal commitGroupCtrInc, commitGroupCtrIncNext: InsTag := INITIAL_GROUP_TAG_INC;

    signal newIntDests, newFloatDests, physStableInt, physStableFloat, zeroDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newIntDestPointer, newFloatDestPointer: SmallNumber := (others => '0');
    signal newIntSources, newIntSources_NR, newIntSourcesAlt, newFloatSources, newFloatSourcesAlt, zeroSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
    signal newSourceSelectorInt, newSourceSelectorFloat, zeroSelector: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0'); 

    signal ctrl: ControlPacket := DEFAULT_CONTROL_PACKET;

        constant INITIAL_INSTRUCTION_TAGS: InstructionTags := (
            renameIndex => INITIAL_GROUP_TAG,
            intPointer => (others => '0'),
            floatPointer => (others => '0'),
            bqPointer => (others => '0'),
            sqPointer => (others => '0'),
            lqPointer => (others => '0'),
            bqPointerSeq => (others => '0')
        );

    signal baseTags, renameTags, renameTagsNext, nextTags, commitTags, commitTagsUpdated: InstructionTags := INITIAL_INSTRUCTION_TAGS;

    -- DEBUG
    signal newProducersInt, newProducersFloat, zeroProducers: InsTagArray(0 to 3*PIPE_WIDTH-1) := (others => (others => 'U'));

    alias frontSendingIn is frontCtrl.full;
    alias sendingFromROB is robCtrl.full;

    function getSpecialActionSlot(insVec: InstructionSlotArray; frontData: BufferEntryArray) return SpecificOp is
       variable res: SpecificOp := frontData(0).specificOperation;
    begin
       for i in PIPE_WIDTH-1 downto 0 loop
           if (insVec(i).full and frontData(i).specialAction) = '1' then
               res := frontData(i).specificOperation;
               res.system := SysOp'val(slv2u(res.bits));
               exit;
           end if;
       end loop;

       return res;
    end function;

    function DB_updateTags(isa: InstructionSlotArray; renameGroupCtrNext: InsTag; renameCtr: Word)
    return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := isa;
        variable tag: InsTag := renameGroupCtrNext;
        variable ctr: Word := renameCtr;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            tag := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            ctr := addInt(renameCtr, i);
            res(i).ins.dbInfo := DB_addTag(res(i).ins.dbInfo, tag);
            res(i).ins.dbInfo := DB_addRenameCounter(res(i).ins.dbInfo, ctr);
        end loop;

        return res;
    end function;


    function updateTags(ia: InstructionSlotArray; tags: InstructionTags) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := ia;
        constant takeVecInt: std_logic_vector(0 to PIPE_WIDTH-1) := getRenamedInt(res);
        constant takeVecFloat: std_logic_vector(0 to PIPE_WIDTH-1) := getRenamedFloat(res);
       	constant fullMask: std_logic_vector(0 to PIPE_WIDTH-1) := extractFullMask(res);
       	constant stores: std_logic_vector(0 to PIPE_WIDTH-1) := getStoreMask1(res) and fullMask;
       	constant loads: std_logic_vector(0 to PIPE_WIDTH-1) := getLoadMask1(res) and fullMask;
       	constant branches: std_logic_vector(0 to PIPE_WIDTH-1) := getBranchMask1(res) and fullMask;
    begin
        -- Setting tags
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.tags.renameIndex := tags.renameIndex or i2slv(i, TAG_SIZE);
            res(i).ins.tags.intPointer := addInt(tags.intPointer, countOnes(takeVecInt(0 to i))); -- Don't increment pointer on ops which use no destination!
            res(i).ins.tags.floatPointer := addInt(tags.floatPointer, countOnes(takeVecFloat(0 to i)));

            res(i).ins.tags.bqPointer := tags.bqPointer;
            res(i).ins.tags.sqPointer := addIntTrunc(tags.sqPointer, countOnes(stores(0 to i-1)), SQ_PTR_SIZE + 1);
            res(i).ins.tags.lqPointer := addIntTrunc(tags.lqPointer, countOnes(loads(0 to i-1)), LQ_PTR_SIZE + 1);
            --res(i).ins.tags.bqPointerSeq := addIntTrunc(tags.bqPointerSeq, countOnes(branches(0 to i-1)), BQ_SEQ_PTR_SIZE + 1);
        end loop;

        return res;
    end function;

    function renameGroupBase(ia: BufferEntryArray; baseTags: InstructionTags;
                            renameCtr: Word; -- DB!                           
                            dbtrap: std_logic)
    return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := getInsSlotArray(ia);
    begin
        res := updateTags(res, baseTags);

        res := DB_updateTags(res, baseTags.renameIndex, renameCtr);

        -- TODO: Why do we cancel ops after event? Rethink, maybe this step is needed just because of some bad design elsewhere
        --       The OOO already has to deal with dynamically arising events
        res := suppressAfterEvent(res, ia);
        return res;
    end function;

    function restoreRenameIndex(content: InstructionSlotArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := content;
    begin
        for i in 1 to PIPE_WIDTH-1 loop
            res(i).ins.tags.renameIndex := clearTagLow(res(0).ins.tags.renameIndex) or i2slv(i, TAG_SIZE);
        end loop;
    
        return res;
    end function;

    function makeOutputCtrl(ctrl: ControlPacket; renamedData: InstructionSlotArray; sending, hasBranch: std_logic) return ControlPacket is
        variable res: ControlPacket := ctrl;
    begin
        res.full := sending;
        res.controlInfo.firstBr := hasBranch;
        return res;
    end function;

    signal inputRenameInfoInt, inputRenameInfoFloat, resultRenameInfoInt, resultRenameInfoFloat, storedRenameInfoInt, storedRenameInfoFloat,
                      commitArgInfoIntDelayed, commitArgInfoFloatDelayed: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
begin
    eventSig <= execEventSignal or lateEventSignal;

    inputRenameInfoInt <= getRenameInfo(frontData, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector);
    inputRenameInfoFloat <= getRenameInfo(frontData, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector, true);

    resultRenameInfoInt <= getRenameInfo(frontData, newIntDests, newIntSources, newIntSourcesAlt, newProducersInt, newSourceSelectorInt);
    resultRenameInfoFloat <= getRenameInfo(frontData, newFloatDests, newFloatSources, newFloatSourcesAlt, newProducersFloat, newSourceSelectorFloat, true);

    frontLastSending <= frontSendingIn and not eventSig;

    baseTags <= (
        renameIndex => renameGroupCtrNext,
        sqPointer => sqPointer,
        lqPointer => lqPointer,
        bqPointer => bqPointer,
        bqPointerSeq => --bqPointerSeq,
                        sn(0),
        intPointer => newIntDestPointer,
        floatPointer => newFloatDestPointer
    );

        renameTags <= (
            renameIndex => renameGroupCtr,
            sqPointer => sqPointer,
            lqPointer => lqPointer,
            bqPointer => bqPointer,
            bqPointerSeq => --bqPointerSeq,
                            sn(0),
            intPointer => newIntDestPointer,
            floatPointer => newFloatDestPointer
        );


    renamedBase <= renameGroupBase( frontData, baseTags,
                                    renameCtr, -- DB!
                                    '0' --dbtrapOn
                                    );

    renamedDataLiving <= restoreRenameIndex(renamedDataLivingPre);

    renameGroupCtrNext <=   commitGroupCtr when lateEventSignal = '1'
                       else clearTagLow(events.execTags.renameIndex) when execEventSignal = '1'
                       else addInt(renameGroupCtr, PIPE_WIDTH) when frontLastSending = '1'
                       else renameGroupCtr;
    
    renameCtrNext <= addInt(renameCtr, countOnes(extractFullMask(renamedBase))) when frontLastSending = '1'
                       else renameCtr;

    commitGroupCtrNext <= commitGroupCtrInc when sendingFromROB = '1' else commitGroupCtr;
    commitGroupCtrIncNext <= addInt(commitGroupCtrInc, PIPE_WIDTH) when sendingFromROB = '1' else commitGroupCtrInc;

    -- Re-allow renaming when everything from rename/exec is committed - reg map will be well defined now
    renameLockRelease <= '1' when commitGroupCtr = renameGroupCtr else '0';
        -- CAREFUL, CHECK: when the counters are equal, renaming can be resumed, but renameLockRelease
        --                      takes effect in next cycle, so before tha cycle renaming is still stopped.
        --                         Should compare to commitCtrNext instead?
        --                         But remember that rewinding GPR map needs a cycle, and before it happens,
        --                         renaming can't be done! So this delay may be caused by this problem.

    renameLockStateNext <= '1' when eventSig = '1'
                    else   '0' when renameLockReleaseDelayed = '1'
                    else   renameLockState;

    renameLockEndDelayedNext <= renameLockStateNext and renameLockRelease;

    renamedSendingSig <= renameFull and nextAccepting;

    -- TODO: introduce a common ControlPacket representing the group to offload all control data from group ops?
    COMMON_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then           
            if frontLastSending = '1' then
                renameFull <= '1';
                renamedDataLivingPre <= renamedBase;
                hasBranch <= frontData(0).firstBr;

                ctrl.full <= '1';
                ctrl.op <= getSpecialActionSlot(renamedBase, frontData);

                storedRenameInfoInt <= resultRenameInfoInt;
                storedRenameInfoFloat <= resultRenameInfoFloat;            

                DB_trackSeqNum(renamedBase);
            elsif renamedSendingSig = '1' then
                renameFull <= '0';
                ctrl.full <= '0';
            end if;

            if eventSig = '1' then
                renameFull <= '0';
                ctrl.full <= '0';
            end if;

            renameGroupCtr <= renameGroupCtrNext;
            renameCtr <= renameCtrNext;

            commitGroupCtr <= commitGroupCtrNext;
            commitGroupCtrInc <= commitGroupCtrIncNext;

            renameLockState <= renameLockStateNext;

            commitArgInfoIntDelayed <= commitArgInfoI;
            commitArgInfoFloatDelayed <= commitArgInfoF;
            robSendingDelayed <= sendingFromROB;

            renameLockReleaseDelayed <= renameLockRelease;
            renameLockEndDelayed <= renameLockEndDelayedNext;

        end if;
    end process;

        POINTERS: block
            use work.LogicQueues.all;
            signal R_tags --, C_tags
                    : InstructionTags := (renameIndex => INITIAL_GROUP_TAG, intPointer => (others => '0'), floatPointer => (others => '0'), others => sn(0));

            function updateTags(tags: InstructionTags; frontData: BufferEntryArray; isa: InstructionSlotArray) return InstructionTags is
                variable res: InstructionTags := DEFAULT_INSTRUCTION_TAGS;
            begin
                res.renameIndex := addIntTrunc(tags.renameIndex, PIPE_WIDTH, TAG_SIZE);
                res.sqPointer := addIntTrunc(tags.sqPointer, countOnes(getStoreMask1(isa)), SQ_PTR_SIZE+1);
                res.lqPointer := addIntTrunc(tags.lqPointer, countOnes(getLoadMask1(isa)), LQ_PTR_SIZE+1);
                res.bqPointer := addIntTrunc(tags.bqPointer, std2int(frontData(0).firstBr), BQ_PTR_SIZE+1);
                if frontData(0).firstBr = '1' then
                --    res.bqPointerSeq <= addIntTrunc(tags.bqPointerSeq, countOnes(getBranchMask1(renamedBase)), BQ_SEQ_PTR_SIZE+1);
                end if;
                res.intPointer := addIntTrunc(tags.intPointer, countOnes(getRenamedInt(TMP_recodeMem(getInsSlotArray(frontData)))), SMALL_NUMBER_SIZE);
                res.floatPointer := addIntTrunc(tags.floatPointer, countOnes(getRenamedFloat(TMP_recodeMem(getInsSlotArray(frontData)))), SMALL_NUMBER_SIZE);
                return res;
            end function;

            function updateTagsCommit(tags: InstructionTags; robData: ControlPacketArray) return InstructionTags is
                variable res: InstructionTags := DEFAULT_INSTRUCTION_TAGS;
            begin
                res.renameIndex := addIntTrunc(tags.renameIndex, PIPE_WIDTH, TAG_SIZE);
                res.sqPointer := addIntTrunc(tags.sqPointer, countOnes(getCommittedEffectiveMask(robData, false)), SQ_PTR_SIZE+1);
                res.lqPointer := addIntTrunc(tags.lqPointer, countOnes(getCommittedEffectiveMask(robData, true)), LQ_PTR_SIZE+1);
                res.bqPointer := addIntTrunc(tags.bqPointer, std2int(robData(0).controlInfo.firstBr), BQ_PTR_SIZE+1);
                if robData(0).controlInfo.firstBr = '1' then
                --    res.bqPointerSeq <= addIntTrunc(tags.bqPointerSeq, countOnes(getBranchMask1(renamedBase)), BQ_SEQ_PTR_SIZE+1);
                end if;
                return res;
            end function;

            function updateTagsDelayedCommit(tags: InstructionTags; renamesInt, renamesFloat: RenameInfoArray) return InstructionTags is
                variable res: InstructionTags := DEFAULT_INSTRUCTION_TAGS;
            begin
                res.intPointer := addIntTrunc(tags.intPointer, countOnes(getVirtualIntDestSels(renamesInt)), SMALL_NUMBER_SIZE);
                res.floatPointer := addIntTrunc(tags.floatPointer, countOnes(getVirtualFloatDestSels(renamesFloat)), SMALL_NUMBER_SIZE);
                return res;
            end function;

                function updateTagsCommitAll(tags: InstructionTags;
                                             robData: ControlPacketArray;
                                             renamesInt, renamesFloat: RenameInfoArray;
                                             commit, commitDelayed: std_logic)
                return InstructionTags is
                    variable res: InstructionTags := tags;
                begin
                    if commit = '1' then
                        res.renameIndex := addIntTrunc(tags.renameIndex, PIPE_WIDTH, TAG_SIZE);
                        res.sqPointer := addIntTrunc(tags.sqPointer, countOnes(getCommittedEffectiveMask(robData, false)), SQ_PTR_SIZE+1);
                        res.lqPointer := addIntTrunc(tags.lqPointer, countOnes(getCommittedEffectiveMask(robData, true)), LQ_PTR_SIZE+1);
                        res.bqPointer := addIntTrunc(tags.bqPointer, std2int(robData(0).controlInfo.firstBr), BQ_PTR_SIZE+1);
                        if robData(0).controlInfo.firstBr = '1' then
                        --    res.bqPointerSeq <= addIntTrunc(tags.bqPointerSeq, countOnes(getBranchMask1(renamedBase)), BQ_SEQ_PTR_SIZE+1);
                        end if;
                    end if;

                    if commitDelayed = '1' then
                        res.intPointer := addIntTrunc(tags.intPointer, countOnes(getVirtualIntDestSels(renamesInt)), SMALL_NUMBER_SIZE);
                        res.floatPointer := addIntTrunc(tags.floatPointer, countOnes(getVirtualFloatDestSels(renamesFloat)), SMALL_NUMBER_SIZE);
                    end if;
                    
                    return res;
                end function;
            
        begin
            nextTags <= updateTags(renameTags, frontData, renamedBase);

            process (clk)
            begin
                if rising_edge(clk) then

                    if lateEventSignal = '1' then
                        R_tags <= commitTags;
                    elsif execEventSignal = '1' then
                        R_tags <= events.execTags;
                        R_tags.renameIndex <= clearTagLow(events.execTags.renameIndex);
                        R_tags.bqPointer <= addIntTrunc(events.execTags.bqPointer, 1, BQ_PTR_SIZE+1);
                    elsif frontLastSending = '1' then
                        R_tags <= nextTags;
                    end if;
 
                    commitTags <= updateTagsCommitAll(commitTags,
                                                        robData, commitArgInfoIntDelayed, commitArgInfoFloatDelayed,
                                                        sendingFromROB, robSendingDelayed);

                          ch3 <= bool2std(renameTags = R_tags);
                          --ch4 <= bool2std(commitTags = C_tags);
                          ch5 <= ch2;
                end if;
            end process;
            
                ch0 <= bool2std(renameTags.renameIndex = R_tags.renameIndex);
                ch1 <= bool2std(renameTags.intPointer = R_tags.intPointer);
                ch2 <= bool2std(renameTags.sqPointer = R_tags.sqPointer);
                --ch1 <= bool2std(baseTags = R_tags);
        end block;

    renamedArgsInt <= storedRenameInfoInt;
    renamedArgsFloat <= storedRenameInfoFloat;

    INT_MAPPER: entity work.RegisterMapper
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEndDelayed,
        
        reserveTag => renameGroupCtrNext,
        commitTag => commitGroupCtr,
        rewindTag => events.execTags.renameIndex,
        
        sendingToReserve => frontSendingIn,
        reserveInfoA => inputRenameInfoInt,
        newPhysDestsOrig => newIntDests,    -- MAPPING (from FREE LIST)
        
        sendingToCommit => robSendingDelayed,
        commitInfoA => commitArgInfoIntDelayed,
        newPhysSources => newIntSources,
        newPhysSources_NR => newIntSources_NR,
        newPhysSourcesAlt => newIntSourcesAlt,
        newPhysSourceSelector => newSourceSelectorInt,

        newProducers => newProducersInt,

        prevStablePhysDests => physStableInt  -- FOR MAPPING (to FREE LIST)
    );

    FP_MAPPER: if ENABLE_FP generate

        FLOAT_MAPPER: entity work.RegisterMapper
        generic map (IS_FP => true)
        port map(
            clk => clk, en => '0', reset => '0',
            
            rewind => renameLockEndDelayed,

            reserveTag => renameGroupCtrNext,
            commitTag => commitGroupCtr,
            rewindTag => events.execTags.renameIndex,

            sendingToReserve => frontSendingIn,
    
            reserveInfoA => inputRenameInfoFloat,
            newPhysDestsOrig => newFloatDests,
            
            sendingToCommit => robSendingDelayed,
            commitInfoA => commitArgInfoFloatDelayed,
    
            newPhysSources => newFloatSources,
            newPhysSources_NR => open,
            newPhysSourcesAlt => newFloatSourcesAlt,
            newPhysSourceSelector => newSourceSelectorFloat,
            
            newProducers => newProducersFloat,
    
            prevStablePhysDests => physStableFloat
        );
    end generate;
    
                            
    INT_FREE_LIST: entity work.RegisterFreeList(Behavioral)
    port map(
        clk => clk, reset => '0', en => '0',
        
        rewind => eventSig,
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal,
        causingPointer => events.execTags.intPointer,
        
        sendingToReserve => frontSendingIn, 
        takeAllow => frontSendingIn,

        reserveInfoA => inputRenameInfoInt,
        newPhysDests => newIntDests,
        newPhysDestPointer => newIntDestPointer,
    
        sendingToRelease => robSendingDelayed,
        releaseInfoA => commitArgInfoIntDelayed,
        physStableDelayed => physStableInt
    );
    
    FP_FREE_LIST: if ENABLE_FP generate
        FLOAT_FREE_LIST: entity work.RegisterFreeList(Behavioral)
        generic map( IS_FP => true)
        port map(
            clk => clk, reset => '0', en => '0',
            
            rewind => eventSig,
            execEventSignal => execEventSignal,
            lateEventSignal => lateEventSignal,        
            causingPointer => events.execTags.floatPointer,
            
            sendingToReserve => frontSendingIn, 
            takeAllow => frontSendingIn,	-- FROM SEQ
    
            reserveInfoA => inputRenameInfoFloat,
    
            newPhysDests => newFloatDests,			-- TO SEQ
            newPhysDestPointer => newFloatDestPointer, -- TO SEQ
        
            sendingToRelease => robSendingDelayed,  -- FROM SEQ
            releaseInfoA => commitArgInfoFloatDelayed,
            physStableDelayed => physStableFloat -- FOR MAPPING (from MAP)
        );
	end generate;

    renameAccepting <= not renameLockState;

    renamedCtrl <= makeOutputCtrl(ctrl, renamedDataLivingPre, renamedSendingSig, hasBranch);
    --renamedSending <= renamedSendingSig;   

    renameGroupCtrNextOut <= renameGroupCtrNext;
    
    -- pragma synthesis off
    RENAME_DB: block

    begin

        COMMON_SYNCHRONOUS: process(clk)
        begin
            if rising_edge(clk) then

            end if;
        end process;
    end block;
    -- pragma synthesis on
           
end Behavioral;
