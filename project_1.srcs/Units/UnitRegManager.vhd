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
    
    sendingFromROB: in std_logic;
    robData: in ControlPacketArray(0 to PIPE_WIDTH-1);

    commitArgInfoI: in RenameInfoArray(0 to PIPE_WIDTH-1);
    commitArgInfoF: in RenameInfoArray(0 to PIPE_WIDTH-1);
   
    renameGroupCtrNextOut: out InsTag;
  
    execCausing: in ControlPacket;
    
    execEventSignal: in std_logic;
    lateEventSignal: in std_logic;
    
    dbState: in DbCoreState
);
end UnitRegManager;


architecture Behavioral of UnitRegManager is
    signal renamedBase, renamedDataLivingPre: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, robSendingDelayed, frontLastSending, renameFull, renamedSendingSig, robSendingPrev,
               renameLockState, renameLockStateNext, renameLockEnd, renameLockEndDelayed, renameLockRelease, renameLockReleaseDelayed, renameLockEndDelayedNext,
                 ch0, ch1, ch2
               : std_logic := '0';

    signal renameGroupCtr, renameGroupCtrNext, commitGroupCtr, commitGroupCtrNext: InsTag := INITIAL_GROUP_TAG;

    signal renameCtr, renameCtrNext: Word := (others => '0');

    signal commitGroupCtrInc, commitGroupCtrIncNext: InsTag := INITIAL_GROUP_TAG_INC;

    signal newIntDests, newFloatDests, physStableInt, physStableFloat, zeroDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newIntDestPointer, newFloatDestPointer: SmallNumber := (others => '0');
    signal newIntSources, newIntSources_NR, newIntSourcesAlt, newFloatSources, newFloatSourcesAlt, zeroSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
    signal newSourceSelectorInt, newSourceSelectorFloat, zeroSelector: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0'); 

    signal specialOperation: SpecificOp := DEFAULT_SPECIFIC_OP;

    -- DEBUG
    signal newProducersInt, newProducersFloat, zeroProducers: InsTagArray(0 to 3*PIPE_WIDTH-1) := (others => (others => 'U'));

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

    function suppressAfterEvent(isa: InstructionSlotArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := isa;
        variable found: boolean := false;
    begin
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


    function getRenamedInt(ia: InstructionSlotArray) return std_logic_vector is
        variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            res(i) := ia(i).ins.virtualArgSpec.intDestSel;
        end loop;
        return res;
    end function;

    function getRenamedFloat(ia: InstructionSlotArray) return std_logic_vector is
        variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            res(i) := ia(i).ins.virtualArgSpec.intDestSel;
        end loop;
        return res;
    end function;


    function updateTags(
                            ia: InstructionSlotArray;
                            renameGroupCtrNext: InsTag;
                            newIntDestPointer: SmallNumber;
                            newFloatDestPointer: SmallNumber;
                            bqPointer: SmallNumber;
                            sqPointer: SmallNumber;
                            lqPointer: SmallNumber;
                            bqPointerSeq: SmallNumber;
                            --renameCtr: Word;                               
                            dbtrap: std_logic)
     return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := ia;

        variable takeVecInt, takeVecFloat: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
       	constant fullMask: std_logic_vector(0 to PIPE_WIDTH-1) := extractFullMask(res);
       	constant stores: std_logic_vector(0 to PIPE_WIDTH-1) := getStoreMask1(res) and fullMask;
       	constant loads: std_logic_vector(0 to PIPE_WIDTH-1) := getLoadMask1(res) and fullMask;
       	constant branches: std_logic_vector(0 to PIPE_WIDTH-1) := getBranchMask1(res) and fullMask;
    begin

        -- Assign dest registers
        for i in 0 to PIPE_WIDTH-1 loop
            takeVecInt(i) := res(i).ins.virtualArgSpec.intDestSel;
            takeVecFloat(i) := res(i).ins.virtualArgSpec.floatDestSel; 
        end loop;

        -- Setting tags
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            res(i).ins.tags.intPointer := addInt(newIntDestPointer, countOnes(takeVecInt(0 to i))); -- Don't increment pointer on ops which use no destination!
            res(i).ins.tags.floatPointer := addInt(newFloatDestPointer, countOnes(takeVecFloat(0 to i)));

            res(i).ins.tags.bqPointer := bqPointer;     
            res(i).ins.tags.sqPointer := addIntTrunc(sqPointer, countOnes(stores(0 to i-1)), SQ_PTR_SIZE + 1);
            res(i).ins.tags.lqPointer := addIntTrunc(lqPointer, countOnes(loads(0 to i-1)), LQ_PTR_SIZE + 1);
            res(i).ins.tags.bqPointerSeq := addIntTrunc(bqPointerSeq, countOnes(branches(0 to i-1)), BQ_SEQ_PTR_SIZE + 1);
        end loop;

        return res;
    end function;

    function renameGroupBase(
                            ia: BufferEntryArray;
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
    begin
        res := updateTags(  res,
                            renameGroupCtrNext,
                            newIntDestPointer,
                            newFloatDestPointer,
                            bqPointer,
                            sqPointer,
                            lqPointer,
                            bqPointerSeq,
                            dbtrap);

        res := DB_updateTags(res, renameGroupCtrNext, renameCtr);

        res := classifyForDispatch(res);

        -- TODO: Why do we cancel ops after event? Rethink, maybe this step is needed just because of some bad design elsewhere
        --       The OOO already has to deal with dynamically arising events
        res := suppressAfterEvent(res);

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

begin
    eventSig <= execEventSignal or lateEventSignal;

    --commitGroupCtrIn <= commitGroupCtr;

    inputRenameInfoInt <= getRenameInfo(frontData, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector);
    inputRenameInfoFloat <= getRenameInfo(frontData, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector, true);

    resultRenameInfoInt <= getRenameInfo(frontData, newIntDests, newIntSources, newIntSourcesAlt, newProducersInt, newSourceSelectorInt);
    resultRenameInfoFloat <= getRenameInfo(frontData, newFloatDests, newFloatSources, newFloatSourcesAlt, newProducersFloat, newSourceSelectorFloat, true);

    frontLastSending <= frontSendingIn and not eventSig;


    renamedBase <= renameGroupBase( frontData,
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

    renameGroupCtrNext <=   commitGroupCtr when lateEventSignal = '1'
                       else clearTagLow(execCausing.tags.renameIndex) when execEventSignal = '1'
                       else addInt(renameGroupCtr, PIPE_WIDTH) when frontLastSending = '1'
                       else renameGroupCtr;
    
    renameCtrNext <= addInt(renameCtr, countOnes(extractFullMask(renamedBase))) when frontLastSending = '1'
                       else renameCtr;

    commitGroupCtrNext <= commitGroupCtrInc when sendingFromROB = '1' else commitGroupCtr;
    commitGroupCtrIncNext <= addInt(commitGroupCtrInc, PIPE_WIDTH) when sendingFromROB = '1' else commitGroupCtrInc;

            ch0 <= bool2std(commitGroupCtr = commitGroupCtr);

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
                specialOperation <= getSpecialActionSlot(renamedBase);        
                storedRenameInfoInt <= resultRenameInfoInt;
                storedRenameInfoFloat <= resultRenameInfoFloat;            

                DB_trackSeqNum(renamedBase);
            elsif renamedSendingSig = '1' then
                renameFull <= '0';
            end if;

            if eventSig = '1' then
                renameFull <= '0';
            end if;

            robSendingPrev <= sendingFromROB;

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
            signal R_ri: InsTag := (others => '0');
            signal C_ri: InsTag := (others => '0');
            signal R_sq, R_lq, R_bq, R_bqs, R_int, R_float: SmallNumber := sn(0);
            signal C_sq, C_lq, C_bq, C_bqs, C_int, C_float: SmallNumber := sn(0);
        begin

            process (clk)
            begin
                if rising_edge(clk) then
                    if lateEventSignal = '1' then
                        R_ri <= C_ri;
                        R_sq <= C_sq;
                        R_lq <= C_lq;
                        R_bq <= C_bq;
                        R_bqs <= C_bqs;
                        R_int <= C_float;
                        R_float <= C_float;
                    elsif execEventSignal = '1' then
                        R_ri <= execCausing.tags.renameIndex;
                        R_sq <= execCausing.tags.sqPointer;
                        R_lq <= execCausing.tags.lqPointer;
                        R_bq <= execCausing.tags.bqPointer;
                        R_bqs <= execCausing.tags.bqPointerSeq;
                        R_int <= execCausing.tags.intPointer;
                        R_float <= execCausing.tags.floatPointer;
                    elsif frontLastSending = '1' then
                        R_ri <= addIntTrunc(R_ri, PIPE_WIDTH, TAG_SIZE);
                        R_sq <= addIntTrunc(R_sq, countOnes(getStoreMask1(TMP_recodeMem(getInsSlotArray(frontData)))), SQ_PTR_SIZE+1);
                        R_lq <= addIntTrunc(R_sq, countOnes(getLoadMask1(TMP_recodeMem(getInsSlotArray(frontData)))), LQ_PTR_SIZE+1);
                        R_bq <= addIntTrunc(R_bq, countOnes(getBranchMask1(TMP_recodeMem(getInsSlotArray(frontData)))), LQ_PTR_SIZE+1);
                        R_bqs <= addIntTrunc(R_bqs, std2int(frontData(0).firstBr), LQ_PTR_SIZE+1);
                        R_int <= addIntTrunc(R_int, countOnes(getRenamedInt(TMP_recodeMem(getInsSlotArray(frontData)))), SMALL_NUMBER_SIZE);
                        R_float <= addIntTrunc(R_float, countOnes(getRenamedFloat(TMP_recodeMem(getInsSlotArray(frontData)))), SMALL_NUMBER_SIZE);
                    end if;

                    if sendingFromROB = '1' then
                        C_ri <= addIntTrunc(C_ri, PIPE_WIDTH, TAG_SIZE);
                        C_sq <= addIntTrunc(C_sq, countOnes(getCommittedEffectiveMask(robData, false)), SQ_PTR_SIZE+1);
                        C_lq <= addIntTrunc(C_lq, countOnes(getCommittedEffectiveMask(robData, true)), LQ_PTR_SIZE+1);
                        C_bq <= addIntTrunc(C_bq, std2int(robData(0).controlInfo.firstBr), BQ_PTR_SIZE+1);
                        C_bqs <= addIntTrunc(C_bqs, countOnes(getCommittedMaskBr(robData)), BQ_SEQ_PTR_SIZE+1);
                    end if;

                    if robSendingPrev = '1' then
                        C_int <= addIntTrunc(C_int, countOnes(getVirtualFloatDestSels(commitArgInfoIntDelayed)), SMALL_NUMBER_SIZE);
                        C_float <= addIntTrunc(C_float, countOnes(getVirtualFloatDestSels(commitArgInfoFloatDelayed)), SMALL_NUMBER_SIZE);
                    end if;
                end if;
            end process;
        end block;



    renamedArgsInt <= storedRenameInfoInt;
    renamedArgsFloat <= storedRenameInfoFloat;

    INT_MAPPER: entity work.RegisterMapper
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEndDelayed,
        
        reserveTag => renameGroupCtrNext,
        commitTag => commitGroupCtr,
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
            commitTag => commitGroupCtr,
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
