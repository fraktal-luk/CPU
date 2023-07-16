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
    frontSendingIn: in std_logic;
    frontData: in BufferEntryArray;

    aluMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    mulMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    memMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    branchMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    storeMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    loadMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    intStoreMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    floatStoreMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    fpMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);

    renamedArgsInt: out RenameInfoArray(0 to PIPE_WIDTH-1);
    renamedArgsFloat: out RenameInfoArray(0 to PIPE_WIDTH-1);
    
    renamedDataLiving: out InstructionSlotArray(0 to PIPE_WIDTH-1);

    renamedSending: out std_logic;
    
    nextAccepting: in std_logic;
    
    renamingBr: out std_logic;

    bqPointer: in SmallNumber;
    sqPointer: in SmallNumber;
    lqPointer: in SmallNumber;
    bqPointerSeq: in SmallNumber;

    newPhysDestsOut: out PhysNameArray(0 to PIPE_WIDTH-1);
    newFloatDestsOut: out PhysNameArray(0 to PIPE_WIDTH-1);
    
    specialOut: out SpecificOp;
    
    commitArgInfoI: in RenameInfoArray(0 to PIPE_WIDTH-1);
    commitArgInfoF: in RenameInfoArray(0 to PIPE_WIDTH-1);
    sendingFromROB: in std_logic;
   
    --commitGroupCtrIn: in InsTag;
    renameGroupCtrNextOut: out InsTag;
  
    execCausing: in ControlPacket;
    
    execEventSignal: in std_logic;
    lateEventSignal: in std_logic;
    
    dbState: in DbCoreState
);
end UnitRegManager;


architecture Behavioral of UnitRegManager is
    signal renamedBase, renamedDataLivingPre: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, robSendingDelayed, frontLastSending, renamedSendingSig,
               renameLockState, renameLockStateNext, renameLockEnd, renameLockEndDelayed, renameLockRelease, renameLockReleaseDelayed, renameLockEndDelayedNext,     
               renameFull,
                 ch0, ch1, ch2
               : std_logic := '0';

    signal renameGroupCtr, renameGroupCtrNext: InsTag := INITIAL_GROUP_TAG; -- This is rewinded on events
    signal renameCtr, renameCtrNext: Word := (others => '0');

    signal commitGroupCtr, commitGroupCtrNext,   commitGroupCtrIn: InsTag := INITIAL_GROUP_TAG;
    signal commitGroupCtrInc, commitGroupCtrIncNext: InsTag := INITIAL_GROUP_TAG_INC;


    signal newIntDests, newFloatDests, physStableInt, physStableFloat, zeroDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newIntDestPointer, newFloatDestPointer: SmallNumber := (others => '0');
    signal newIntSources, newIntSources_NR, newIntSourcesAlt, newFloatSources, newFloatSourcesAlt, zeroSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
    signal newSourceSelectorInt, newSourceSelectorFloat, zeroSelector: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0'); 

    --signal specialActionSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;    
    signal specialOperation: SpecificOp := DEFAULT_SPECIFIC_OP;

        -- DEBUG
    	signal newProducersInt, newProducersFloat, zeroProducers: InsTagArray(0 to 3*PIPE_WIDTH-1) := (others => (others => 'U'));

    ------------
        -- Debug functions
        function DB_addTag(dbi: InstructionDebugInfo; tag: InsTag) return InstructionDebugInfo is
            variable res: InstructionDebugInfo := dbi;
        begin
            -- pragma synthesis off
            res.tag := tag;
            -- pragma synthesis on
            return res;
        end function;
    ---------------

          function getInsSlot(elem: BufferEntry) return InstructionSlot is
              variable res: InstructionSlot := DEFAULT_INS_SLOT;
          begin
              res.full := elem.full;
              res.ins.dbInfo := elem.dbInfo;

              res.ins.specificOperation := unfoldOp(elem.specificOperation);

              res.ins.typeInfo := elem.classInfo;
              res.ins.typeInfo.useSQ := elem.classInfo.secCluster;

              res.ins.constantArgs := elem.constantArgs;
              res.ins.virtualArgSpec := elem.argSpec; 


              res.ins.controlInfo.firstBr_T := elem.firstBr;
              res.ins.controlInfo.specialAction_T := elem.specialAction;

              return res;
          end function;

          function getInsSlotArray(elemVec: BufferEntryArray) return InstructionSlotArray is
              variable res: InstructionSlotArray(elemVec'range);
          begin
              for i in res'range loop
                  res(i) := getInsSlot(elemVec(i));
              end loop;
              return res;
          end function;


    function classifyForDispatch(insVec: InstructionSlotArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
        variable div, mul: boolean := false;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            div := insVec(i).ins.specificOperation.arith = opDivU
                 or insVec(i).ins.specificOperation.arith = opDivS                     
                 or insVec(i).ins.specificOperation.arith = opRemU
                 or insVec(i).ins.specificOperation.arith = opRemS;
            mul := insVec(i).ins.specificOperation.arith = opMul
                  or insVec(i).ins.specificOperation.arith = opMulhU
                  or insVec(i).ins.specificOperation.arith = opMulhS;
        
                res(i).ins.dispatchInfo.useDiv := bool2std(div and (insVec(i).ins.specificOperation.subpipe = ALU));
            
            if (insVec(i).ins.specificOperation.subpipe = ALU) then
                if mul or div then
                    res(i).ins.dispatchInfo.useMul := '1';
                else
                    res(i).ins.dispatchInfo.useAlu := '1';
                end if;
            elsif insVec(i).ins.specificOperation.subpipe = FP then
                res(i).ins.dispatchInfo.useFP := '1';
            elsif insVec(i).ins.specificOperation.subpipe = Mem then
                res(i).ins.dispatchInfo.useMem := '1';

			    if (insVec(i).ins.specificOperation.memory = opLoad or insVec(i).ins.specificOperation.memory = opLoadSys) then 
                        res(i).ins.typeInfo.useLQ := '1';
                elsif (insVec(i).ins.specificOperation.memory = opStore or insVec(i).ins.specificOperation.memory = opStoreSys) then
                        res(i).ins.typeInfo.useSQ := '1';
                    if res(i).ins.typeInfo.useFP = '1' then
                        res(i).ins.dispatchInfo.storeFP := '1';
                    else
                        res(i).ins.dispatchInfo.storeInt := '1';
                    end if;
                end if;

            end if;
        end loop;
        
        return res;
    end function;


    function hasSyncEvent(ctrl: InstructionControlInfo_T) return std_logic is
    begin
        return ctrl.specialAction_T;
    end function;


    function getSpecialActionSlot(insVec: InstructionSlotArray) return SpecificOp is
       variable res: SpecificOp := insVec(0).ins.specificOperation;
    begin
       for i in PIPE_WIDTH-1 downto 0 loop
           if (insVec(i).full and hasSyncEvent(insVec(i).ins.controlInfo)) = '1' then
               res := insVec(i).ins.specificOperation;
               res.system := SysOp'val(slv2u(res.bits));
               exit;
           end if;
       end loop;

       return res;
    end function;

    function renameGroupBase(
                            ia: BufferEntryArray;
                            newIntDests: PhysNameArray;
                            newFloatDests: PhysNameArray;
                            renameGroupCtrNext: InsTag;
                            newIntDestPointer: SmallNumber;
                            newFloatDestPointer: SmallNumber;
                            bqPointer: SmallNumber;
                            sqPointer: SmallNumber;
                            lqPointer: SmallNumber;
                            bqPointerSeq: SmallNumber;
                            renameCtr: Word;                               
                            dbtrap: std_logic)
     return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := TMP_recodeMem(getInsSlotArray(ia));
        variable reserveSelSig, takeVecInt, takeVecFloat, stores, loads, branches: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
        variable nToTake: integer := 0;
        variable newGprTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));    
        variable newNumberTags: InsTagArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
        variable tag: InsTag := renameGroupCtrNext;
       	variable found: boolean := false;
       	constant fullMask: std_logic_vector(0 to PIPE_WIDTH-1) := extractFullMask(res);
    begin
        stores := getStoreMask1(res) and fullMask;
        loads := getLoadMask1(res) and fullMask;
        branches := getBranchMask1(res) and fullMask;
        
        -- Assign dest registers
        for i in 0 to PIPE_WIDTH-1 loop
            takeVecInt(i) := res(i).ins.virtualArgSpec.intDestSel;
            takeVecFloat(i) := res(i).ins.virtualArgSpec.floatDestSel; 
        end loop;

        -- Setting tags
        for i in 0 to PIPE_WIDTH-1 loop
            tag := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            res(i).ins.dbInfo := DB_addTag(res(i).ins.dbInfo, tag);

            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            res(i).ins.tags.intPointer := addInt(newIntDestPointer, countOnes(takeVecInt(0 to i))); -- Don't increment pointer on ops which use no destination!
            res(i).ins.tags.floatPointer := addInt(newFloatDestPointer, countOnes(takeVecFloat(0 to i)));
            
            res(i).ins.tags.bqPointer := bqPointer;     
            res(i).ins.tags.sqPointer := addIntTrunc(sqPointer, countOnes(stores(0 to i-1)), SQ_PTR_SIZE + 1);
            res(i).ins.tags.lqPointer := addIntTrunc(lqPointer, countOnes(loads(0 to i-1)), LQ_PTR_SIZE + 1);
            res(i).ins.tags.bqPointerSeq := addIntTrunc(bqPointerSeq, countOnes(branches(0 to i-1)), BQ_SEQ_PTR_SIZE + 1);
        end loop;

        res := classifyForDispatch(res);

        -- TODO: Why do we cancel ops after event? Rethink, maybe this step is needed just because of some bad design elsewhere
        --       The OOO already has to deal with dynamically arising events

        -- If found special instruction or exception, kill next ones
        for i in 0 to PIPE_WIDTH-1 loop
            if found then
                res(i).full := '0';
            end if;

            if res(i).full /= '1' then
                res(i).ins.typeInfo := DEFAULT_CLASS_INFO;
                res(i).ins.dispatchInfo := DEFAULT_CLASS_INFO_DISPATCH;                            
            end if;

            if hasSyncEvent(res(i).ins.controlInfo) = '1' then
                found := true;
            end if;
        end loop;

        return res;
    end function;


    function getRenameInfo( ia: BufferEntryArray;
                            newPhysDests, newPhysSources, newPhysSourcesStable: PhysNameArray;
                            newProducers: InsTagArray; -- TODO: split to some DB function
                            newSourceSelector: std_logic_vector; constant IS_FP: boolean := false)
    return RenameInfoArray is
        variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
        variable dests: PhysNameArray(0 to PIPE_WIDTH-1) := assignDests(ia, newPhysDests, IS_FP);
        variable va: InstructionArgSpec := DEFAULT_ARG_SPEC;
        variable ca: InstructionConstantArgs := DEFAULT_CONSTANT_ARGS;
        variable depVec: DependencyVec;
    begin
        depVec := findDeps(ia);
        if IS_FP then
            depVec := getRealDepVecFloat(ia, depVec);
        else
            depVec := getRealDepVecInt(ia, depVec);
        end if;
        
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).dbInfo := ia(i).dbInfo;
            va := ia(i).argSpec;
            ca := ia(i).constantArgs;
            
            if IS_FP then        
                res(i).destSel := va.floatDestSel;
                res(i).destSelFP := va.floatDestSel;
            else    
                res(i).destSel := va.intDestSel;
            end if;
            
            res(i).virtualDest := va.dest(4 downto 0);
            res(i).physicalDest := dests(i);

            for j in 0 to 2 loop
                if IS_FP then
                    res(i).argStates(j).sel := va.floatArgSel(j);
                else
                    res(i).argStates(j).sel := va.intArgSel(j);
                end if;
                
                res(i).argStates(j).const := (va.intArgSel(j) and not isNonzero(va.args(j)(4 downto 0))) -- int r0
                                                        or (not va.intArgSel(j) and not va.floatArgSel(j))             -- not used
                                                        or (bool2std(j = 1) and ca.immSel);
                res(i).argStates(j).virtual := va.args(j)(4 downto 0);
                
                res(i).argStates(j).deps := depVec(i)(j);
            end loop;

                if ia(i).specificOperation.subpipe = Mem then
                    res(i) := swapArgs12(res(i));
                end if;

            for j in 0 to 2 loop
                res(i).dbDepTags(j) := newProducers(3*i + j);

                res(i).argStates(j).physical := newPhysSources(3*i + j);
                res(i).argStates(j).physicalStable := newPhysSourcesStable(3*i + j);

                res(i).argStates(j).physicalNew := newPhysSources(3*i + j);
                
                res(i).argStates(j).sourceStable := newSourceSelector(3*i+ j);
                res(i).argStates(j).sourceNew := isNonzero(res(i).argStates(j).deps);
                res(i).argStates(j).sourceReady := '0';

                for k in PIPE_WIDTH-1 downto 0 loop
                    if res(i).argStates(j).deps(k) = '1' then
                        res(i).argStates(j).physicalNew := dests(k);
                        exit;
                    end if;
                end loop;
            end loop;
                
                if QQQ = 1 then
                    res(i).argStates(2) := DEFAULT_ARG_RENAME_STATE;
                end if;
        end loop;

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

    signal inputRenameInfoInt, inputRenameInfoFloat, resultRenameInfoInt, resultRenameInfoFloat, storedRenameInfoInt, storedRenameInfoFloat,
                      commitArgInfoIntDelayed, commitArgInfoFloatDelayed: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
                      
                      
                      
        procedure DB_trackSeqNum(renamed: InstructionSlotArray) is
        begin
            -- pragma synthesis off
            if DB_OP_TRACKING then
                for i in renamed'range loop
                    if renamed(i).ins.dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                        report "";
                        report "DEBUG: Tracked seqNum renamed: " & --integer'image(slv2u(DB_TRACKED_SEQ_NUM));
                                                                             work.CpuText.slv2hex(DB_TRACKED_SEQ_NUM);

                        report "";
                        
                        return;
                    end if;
                end loop;
            end if;
            -- pragma synthesis on
        end procedure;
begin

            commitGroupCtrIn <= commitGroupCtr;

    inputRenameInfoInt <= getRenameInfo(frontData, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector);
    inputRenameInfoFloat <= getRenameInfo(frontData, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector, true);

    resultRenameInfoInt <= getRenameInfo(frontData, newIntDests, newIntSources, newIntSourcesAlt, newProducersInt, newSourceSelectorInt);
    resultRenameInfoFloat <= getRenameInfo(frontData, newFloatDests, newFloatSources, newFloatSourcesAlt, newProducersFloat, newSourceSelectorFloat, true);

    frontLastSending <= frontSendingIn and not eventSig;

    eventSig <= execEventSignal or lateEventSignal;

    renamedBase <= renameGroupBase( frontData,
                                    newIntDests, 
                                    newFloatDests,
                                    renameGroupCtrNext,
                                    newIntDestPointer,
                                    newFloatDestPointer,
                                    bqPointer,
                                    sqPointer,
                                    lqPointer,
                                    bqPointerSeq,
                                    renameCtr,
                                    '0' --dbtrapOn
                                    );

    renamedDataLiving <= restoreRenameIndex(renamedDataLivingPre);

    renameGroupCtrNext <=   commitGroupCtrIn when lateEventSignal = '1'
                       else clearTagLow(execCausing.tags.renameIndex) when execEventSignal = '1'
                       else addInt(renameGroupCtr, PIPE_WIDTH) when frontLastSending = '1'
                       else renameGroupCtr;
    
    renameCtrNext <= addInt(renameCtr, countOnes(extractFullMask(renamedBase))) when frontLastSending = '1'
                       else renameCtr;



        commitGroupCtrNext <= commitGroupCtrInc when sendingFromROB = '1' else commitGroupCtr;
        commitGroupCtrIncNext <= addInt(commitGroupCtrInc, PIPE_WIDTH) when sendingFromROB = '1' else commitGroupCtrInc;

            ch0 <= bool2std(commitGroupCtr = commitGroupCtrIn);


    -- Re-allow renaming when everything from rename/exec is committed - reg map will be well defined now
    renameLockRelease <= '1' when commitGroupCtrIn = renameGroupCtr else '0';
        -- CAREFUL, CHECK: when the counters are equal, renaming can be resumed, but renameLockRelease
        --                      takes effect in next cycle, so before tha cycle renaming is still stopped.
        --                         Should compare to commitCtrNext instead?
        --                         But remember that rewinding GPR map needs a cycle, and before it happens,
        --                         renaming can't be done! So this delay may be caused by this problem.

    renameLockStateNext <= '1' when eventSig = '1'
                    else   '0' when renameLockReleaseDelayed = '1'
                    else   renameLockState;

    renameLockEndDelayedNext <= renameLockStateNext and renameLockRelease;

    renamedSendingSig <= renameFull and nextAccepting and not eventSig;

    COMMON_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then                
            if frontLastSending = '1' then
                specialOperation <= getSpecialActionSlot(renamedBase);
            end if;
            
            if frontLastSending = '1' then
                renameFull <= '1';
                renamedDataLivingPre <= renamedBase;
                
                DB_trackSeqNum(renamedBase);
                
            elsif renamedSendingSig = '1' then
                renameFull <= '0';
            end if;
    
            if eventSig = '1' then
                renameFull <= '0';
            end if;
    
            renameGroupCtr <= renameGroupCtrNext;
            renameCtr <= renameCtrNext;

                commitGroupCtr <= commitGroupCtrNext;
                commitGroupCtrInc <= commitGroupCtrIncNext;

            -- Lock when exec part causes event
            if execEventSignal = '1' or lateEventSignal = '1' then -- CAREFUL
                renameLockState <= '1';    
            elsif renameLockReleaseDelayed = '1' then
                renameLockState <= '0';
            end if;
         
            commitArgInfoIntDelayed <= commitArgInfoI;
            commitArgInfoFloatDelayed <= commitArgInfoF;
            robSendingDelayed <= sendingFromROB;
            
            renameLockReleaseDelayed <= renameLockRelease;
            renameLockEndDelayed <= renameLockEndDelayedNext;
            
            if frontLastSending = '1' then             
                storedRenameInfoInt <= resultRenameInfoInt;
                storedRenameInfoFloat <= resultRenameInfoFloat;            
            end if;

        end if;
    end process;

    renamedArgsInt <= storedRenameInfoInt;
    renamedArgsFloat <= storedRenameInfoFloat;

    INT_MAPPER: entity work.RegisterMapper
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEndDelayed,
        
            reserveTag => renameGroupCtrNext,
            commitTag => commitGroupCtrIn,
            rewindTag => execCausing.tags.renameIndex,
        
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
                commitTag => commitGroupCtrIn,
                rewindTag => execCausing.tags.renameIndex,

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
        clk => clk,
        reset => '0',
        en => '0',
        
        rewind => eventSig,
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal,
        causingPointer => execCausing.tags.intPointer,
        
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
            clk => clk,
            reset => '0',
            en => '0',
            
            rewind => eventSig,
            execEventSignal => execEventSignal,
            lateEventSignal => lateEventSignal,        
            causingPointer => execCausing.tags.floatPointer,
            
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
	
    specialOut <= specialOperation;
	
    newPhysDestsOut <= newIntDests;
    newFloatDestsOut <= newFloatDests; 
    renameAccepting <= not renameLockState;
 
    renamedSending <= renamedSendingSig;   

    renamingBr <= frontLastSending and frontData(0).firstBr;

    aluMaskRe <= getAluMask1(renamedBase);
    mulMaskRe <= getMulMask1(renamedBase);
    memMaskRe <= getMemMask1(renamedBase);
    branchMaskRe <= getBranchMask1(renamedBase);
    loadMaskRe <= getLoadMask1(renamedBase);
    storeMaskRe <= getStoreMask1(renamedBase);
    fpMaskRe <= getFpMask1(renamedBase);
    intStoreMaskRe <= getIntStoreMask1(renamedBase);
    floatStoreMaskRe <= getFloatStoreMask1(renamedBase);
    
    renameGroupCtrNextOut <= renameGroupCtrNext;
    
    RENAME_DB: block
        signal gapSig: std_logic := '0';
        signal gapFirst, gapLast: Word := (others => 'U');
        signal lastSeqNum: Word := (others => '0');
    begin
    
        COMMON_SYNCHRONOUS: process(clk)
            procedure DB_handleGroup(ia: BufferEntryArray; signal lastSeqNum: inout Word; signal gapSig: out std_logic) is--; signal gapSig: out std_logic) is
                variable any: boolean := false;
                variable firstNewSeqNum, lastNewSeqNum, gap: Word := (others => '0');
                
            begin
                -- pragma synthesis off
            
                for i in 0 to PIPE_WIDTH-1 loop
                    if ia(i).full = '1' then
                        if not any then
                            firstNewSeqNum := ia(i).dbInfo.seqNum;
                        end if;
                        lastNewSeqNum := ia(i).dbInfo.seqNum;
                        any := true;
                    end if;
                end loop;
                
                gap := sub(firstNewSeqNum, lastSeqNum);
                if slv2u(gap) /= 1 then
                    gapSig <= '1';
                    gapFirst <= addInt(lastSeqNum, 1);
                    gapLast <= addInt(firstNewSeqNum, -1);
                else
                    gapSig <= '0';
                    gapFirst <= (others => 'U');
                    gapLast <= (others => 'U');
                end if;
                
                lastSeqNum <= lastNewSeqNum;
                
                -- pragma synthesis on
            end procedure;
        begin
            if rising_edge(clk) then
               if frontSendingIn = '1' then
                   
                   -- DEBUG
                   --robDataCommitted <= robDataCommittedNext;
                   DB_handleGroup(frontData, lastSeqNum, gapSig);
               end if;
            end if;
        end process;       
    
    end block;          
end Behavioral;
