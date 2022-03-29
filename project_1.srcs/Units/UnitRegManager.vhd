----------------------------------------------------------------------------------

----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicRenaming.all;


entity UnitRegManager is
port(
    clk: in std_logic;
    
    renameAccepting: out std_logic;
    frontLastSendingIn: in std_logic;
    frontDataLastLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
        
        TMP_spMaskedDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1);

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
    
    specialActionOut: out InstructionSlot;
    
    robDataLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    sendingFromROB: in std_logic;
   
    commitGroupCtr: in InsTag;
  
    execCausing: in InstructionState;
    lateCausing: in InstructionState;
    
    execEventSignal: in std_logic;
    lateEventSignal: in std_logic
);
end UnitRegManager;


architecture Behavioral of UnitRegManager is
    signal stageDataRenameIn, stageDataRenameIn_T, stageDataRenameInFloat, renamedDataLivingPre, renamedDataLivingPre_T, stageDataToCommit, stageDataCommitInt, stageDataCommitFloat,
                renamedBase, stageDataToCommitDelayed
    
               : InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, robSendingDelayed, sendingCommitInt, frontLastSending, renamedSendingSig, sendingCommitFloat,
               renameLockState, renameLockStateNext, renameLockEnd, renameLockEndDelayed, renameLockRelease, renameLockReleaseDelayed, renameLockEndDelayedNext,
               
               T_renameFull, T_renameSending,  ch0, ch1, ch2
               : std_logic := '0';
 
    signal renameGroupCtr, renameGroupCtrNext: InsTag := INITIAL_GROUP_TAG; -- This is rewinded on events
    signal renameCtr, renameCtrNext: Word := (others => '0');

    signal newIntDests, newFloatDests, physStableInt, physStableFloat: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newIntDestPointer, newFloatDestPointer: SmallNumber := (others => '0');
    signal newIntSources, newIntSources_NR, newIntSourcesAlt, newFloatSources, newFloatSourcesAlt: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
    signal newSourceSelectorInt, newSourceSelectorFloat: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0'); 
    
    --signal renamedBase, stageDataToCommitDelayed: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal depVec, depVecPrev: DependencyVec := DEFAULT_DEP_VEC;

    signal specialActionSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;    
      
    function renameGroupBase(   insVec: InstructionSlotArray;
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
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
        variable reserveSelSig, takeVecInt, takeVecFloat, stores, loads, branches: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
        variable nToTake: integer := 0;
        variable newGprTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));    
        variable newNumberTags: InsTagArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
       	variable found: boolean := false;
    begin
        stores := getStoreMask(TMP_recodeMem(insVec));
        loads := getLoadMask(TMP_recodeMem(insVec));
        branches := getBranchMask(insVec);
        
        -- Assign dest registers
        for i in 0 to PIPE_WIDTH-1 loop
            if res(i).ins.virtualArgSpec.floatDestSel = '1' then
                res(i).ins.physicalArgSpec.dest := newFloatDests(countOnes(takeVecFloat)); -- how many used before
            else
                res(i).ins.physicalArgSpec.dest := newIntDests(countOnes(takeVecInt)); -- how many used before
            end if;
            takeVecInt(i) := insVec(i).ins.virtualArgSpec.intDestSel;
            takeVecFloat(i) := insVec(i).ins.virtualArgSpec.floatDestSel;
            
            res(i).ins.physicalArgSpec.intDestSel := res(i).ins.virtualArgSpec.intDestSel;
            res(i).ins.physicalArgSpec.floatDestSel := res(i).ins.virtualArgSpec.floatDestSel;     
        end loop;

        -- Setting tags
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            res(i).ins.tags.renameCtr := addInt(renameCtr, i);
            res(i).ins.tags.intPointer := addInt(newIntDestPointer, countOnes(takeVecInt(0 to i)));
                                                                         -- Don't increment pointer on ops which use no destination!
            res(i).ins.tags.floatPointer := addInt(newFloatDestPointer, countOnes(takeVecFloat(0 to i)));
            
            res(i).ins.tags.bqPointer := bqPointer;     
            res(i).ins.tags.sqPointer := addIntTrunc(sqPointer, countOnes(stores(0 to i-1)), SQ_PTR_SIZE + 1);
            res(i).ins.tags.lqPointer := addIntTrunc(lqPointer, countOnes(loads(0 to i-1)), LQ_PTR_SIZE + 1);
            res(i).ins.tags.bqPointerSeq := addIntTrunc(bqPointerSeq, countOnes(branches(0 to i-1)), BQ_PTR_SIZE + 2 + 1); -- CAREFUL, TODO: define BQ_SEQ_PTR_SIZE
        end loop;

        -- If found special instruction or exception, kill next ones
        for i in 0 to PIPE_WIDTH-1 loop
            if found then
                if res(i).full = '1' then
                    res(i).ins.controlInfo.ignored := '1';
                end if;
                res(i).full := '0';
            end if;
            
            if res(i).full = '0' then
                -- CAREFUL: needed for correct operation of StoreQueue + LQ
                res(i).ins.classInfo.secCluster := '0';
                res(i).ins.classInfo.useLQ := '0';            
            end if;
            
            if hasSyncEvent(res(i).ins) = '1' then
                found := true;
            end if;
        end loop;
        
        if CLEAR_DEBUG_INFO then
            for i in 0 to PIPE_WIDTH-1 loop
				-- KEEP renameIndex + argSpec + specificOperation
                   
                res(i) := clearAbstractInfo(res(i));
                
                -- Reduce operation to raw bits (remove redundancy)
                res(i).ins.specificOperation.arith := opAnd;
                res(i).ins.specificOperation.memory := opLoad;
                res(i).ins.specificOperation.float := opMove;
                res(i).ins.specificOperation.system := opNone;
                
                res(i) := clearDbCounters(res(i));

                if i > 0 then -- High bits are the same as in slot 0, low bits are constant equal to i
                    res(i).ins.tags.renameIndex := (others => '0');
                end if;
                
                -- TODO: this is unused anyway
                res(i).ins.controlInfo.newEvent := '0';
                res(i).ins.controlInfo.hasInterrupt := '0';
                res(i).ins.controlInfo.refetch := '0';
                res(i).ins.controlInfo.orderViolation := '0';
                res(i).ins.controlInfo.tlbMiss := '0';
                res(i).ins.controlInfo.sqMiss := '0';
                if i /= 0 then -- TMP!
                    res(i).ins.controlInfo.firstBr := '0';
                end if;
            end loop;           
        end if;

        return res;
    end function;

    function findDeps(insVec: InstructionSlotArray) return DependencyVec is
        variable res: DependencyVec := DEFAULT_DEP_VEC;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if insVec(i).ins.virtualArgSpec.args(k)(4 downto 0) = insVec(j).ins.virtualArgSpec.dest(4 downto 0) -- name match       
                    then
                        res(i)(k)(j) := '1';                   
                    end if;
                end loop;
            end loop;
        end loop;
        
        return res;
    end function;
    
    function renameGroupInt(insVec: InstructionSlotArray;
                            newPhysSources: PhysNameArray;
                            depVec: DependencyVec) 
    return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
    begin      

        return res;
    end function;
 
    function renameGroupFloat(insVec: InstructionSlotArray;
                              newFloatSources: PhysNameArray;
                              depVec: DependencyVec)
    return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
        variable tmpArgSpec: InstructionArgSpec := DEFAULT_ARG_SPEC;
    begin
        for i in res'range loop
            if res(i).ins.classInfo.fpRename = '0' then
                res(i).full := '0';
            end if;
            
            if CLEAR_DEBUG_INFO then
                -- Leave only physicalArgs
                tmpArgSpec := res(i).ins.physicalArgSpec;
                res(i).ins := DEFAULT_INS_STATE;
                res(i).ins.physicalArgSpec := tmpArgSpec;             
            end if;
            
        end loop;
            
        return res;
    end function;

    function getRealDepVecInt(insVec: InstructionSlotArray; depVec: DependencyVec) return DependencyVec is
        variable res: DependencyVec := (others => (others => (others => '0')));
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if depVec(i)(k)(j) = '1'
                        and insVec(i).ins.virtualArgSpec.intArgSel(k) = '1' and insVec(j).ins.virtualArgSpec.intDestSel = '1' -- intSel match
                    then
                        res(i)(k)(j) := '1';
                        exit;                        
                    end if;
                end loop;
            end loop;                     
    
        end loop;        
        return res;
    end function;

    function getRealDepVecFloat(insVec: InstructionSlotArray; depVec: DependencyVec) return DependencyVec is
        variable res: DependencyVec := (others => (others => (others => '0')));
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if depVec(i)(k)(j) = '1'
                        and insVec(i).ins.virtualArgSpec.floatArgSel(k) = '1' and insVec(j).ins.virtualArgSpec.floatDestSel = '1'
                    then
                        res(i)(k)(j) := '1';
                        exit;                   
                    end if;
                end loop;
            end loop;                     
    
        end loop;        
        return res;
    end function;   

    function classifyForDispatch(insVec: InstructionSlotArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            if insVec(i).ins.specificOperation.subpipe = ALU then
                res(i).ins.classInfo.useAlu := '1';
            elsif insVec(i).ins.specificOperation.subpipe = FP then
                res(i).ins.classInfo.useFP := '1';
            elsif insVec(i).ins.specificOperation.subpipe = Mem then
                res(i).ins.classInfo.useMem := '1';
			    if (insVec(i).ins.specificOperation.memory = opLoad or insVec(i).ins.specificOperation.memory = opLoadSys) then 
                    res(i).ins.classInfo.useLQ := '1';
                elsif (insVec(i).ins.specificOperation.memory = opStore or insVec(i).ins.specificOperation.memory = opStoreSys) then
                    res(i).ins.classInfo.useSQ := '1';
                    if res(i).ins.classInfo.fpRename = '1' then
                        res(i).ins.classInfo.storeFP := '1';
                    else
                        res(i).ins.classInfo.storeInt := '1';
                    end if;
                end if;
            
            end if;
        
            if insVec(i).full /= '1' then
                res(i).ins.classInfo.mainCluster := '0';
                res(i).ins.classInfo.secCluster := '0';
                res(i).ins.classInfo.fpRename := '0';
                res(i).ins.classInfo.branchIns := '0';
                res(i).ins.classInfo.useLQ := '0';
                res(i).ins.classInfo.useSQ := '0';
                res(i).ins.classInfo.storeInt := '0';
                res(i).ins.classInfo.storeFP := '0';
                res(i).ins.classInfo.useAlu := '0';
                res(i).ins.classInfo.useMem := '0';
            end if;
        end loop;
        
        return res;
    end function;

    function getRenameInfo(insVec: InstructionSlotArray; newPhysDests, newPhysSources, newPhysSourcesStable: PhysNameArray; newSourceSelector: std_logic_vector; constant IS_FP: boolean := false)
    return RenameInfoArray is
        variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
        variable dests: PhysNameArray(0 to PIPE_WIDTH-1) := assignDests(insVec, newPhysDests, IS_FP);
        variable depVec: DependencyVec;
    begin
        depVec := findDeps(insVec);
        if IS_FP then
            depVec := getRealDepVecFloat(insVec, depVec);
        else
            depVec := getRealDepVecInt(insVec, depVec);
        end if;
        
        for i in 0 to PIPE_WIDTH-1 loop
            if IS_FP then        
                res(i).destSel := insVec(i).ins.virtualArgSpec.floatDestSel;
                res(i).destSelFP := insVec(i).ins.virtualArgSpec.floatDestSel;
            else    
                res(i).destSel := insVec(i).ins.virtualArgSpec.intDestSel;
            end if;
            
            res(i).virtualDest := insVec(i).ins.virtualArgSpec.dest(4 downto 0);
            res(i).physicalDest := dests(i);

            if IS_FP then
                res(i).sourceSel := insVec(i).ins.virtualArgSpec.floatArgSel;
            else
                res(i).sourceSel := insVec(i).ins.virtualArgSpec.intArgSel;
            end if;
                
            for j in 0 to 2 loop
                res(i).sourceConst(j) :=   (insVec(i).ins.virtualArgSpec.intArgSel(j) and not isNonzero(insVec(i).ins.virtualArgSpec.args(j)(4 downto 0))) -- int r0
                                        or (not insVec(i).ins.virtualArgSpec.intArgSel(j) and not insVec(i).ins.virtualArgSpec.floatArgSel(j))             -- not used
                                        or (bool2std(j = 1) and insVec(i).ins.constantArgs.immSel);                                                        -- imm

                res(i).virtualSources(j) := insVec(i).ins.virtualArgSpec.args(j)(4 downto 0);
                
                res(i).physicalSources(j) := newPhysSources(3*i + j);
                res(i).physicalSourcesStable(j) := newPhysSourcesStable(3*i + j);
            end loop;

            res(i).deps := depVec(i);
            res(i).physicalSourcesNew := res(i).physicalSources;
                                
            for j in 0 to 2 loop
                res(i).sourcesNew(j) := isNonzero(res(i).deps(j));
                for k in PIPE_WIDTH-1 downto 0 loop
                    if res(i).deps(j)(k) = '1' then
                        res(i).physicalSourcesNew(j) := dests(k);
                        exit;
                    end if;
                end loop;
            end loop;
            
            res(i).sourcesStable := newSourceSelector(3*i to 3*i + 2);            
            res(i).sourcesReady := (others => '0');
        end loop;
        return res;
    end function;

    function postprocessRenamed(insVec: InstructionSlotArray; depVec: DependencyVec) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
    begin

        return res;
    end function;   
    
    signal inputRenameInfoInt, inputRenameInfoFloat, storedRenameInfoInt, storedRenameInfoFloat: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
begin
    inputRenameInfoInt <= getRenameInfo(frontDataLastLiving, newIntDests, newIntSources, newIntSourcesAlt, newSourceSelectorInt);
    inputRenameInfoFloat <= getRenameInfo(frontDataLastLiving, newFloatDests, newFloatSources, newFloatSourcesAlt, newSourceSelectorFloat, true);

    frontLastSending <= frontLastSendingIn and not eventSig;

    eventSig <= execEventSignal or lateEventSignal;

    depVec <= findDeps(frontDataLastLiving);

    renamedBase <= renameGroupBase(frontDataLastLiving,
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

    stageDataRenameIn <=        renameGroupInt(     renamedBase, newIntSources, depVec); 
    stageDataRenameInFloat <=   renameGroupFloat(   renamedBase, newFloatSources, depVec); -- like above
    -- TODO: ^ or assign dests above, not in renameGroupBase, to keep Int and FP path separate, and merge them ony when going to ROB - it could be good for layout

    stageDataRenameIn_T <= classifyForDispatch(TMP_recodeMem(stageDataRenameIn));

    renamedDataLiving <= postprocessRenamed(restoreRenameIndex(renamedDataLivingPre), depVecPrev);

    renameGroupCtrNext <=   commitGroupCtr when lateEventSignal = '1'
                       else clearTagLow(execCausing.tags.renameIndex) when execEventSignal = '1'
                       else addInt(renameGroupCtr, PIPE_WIDTH) when frontLastSending = '1'
                       else renameGroupCtr;
    
    renameCtrNext <= addInt(renameCtr, countOnes(extractFullMask(stageDataRenameIn))) when frontLastSending = '1'
                       else renameCtr;
    
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


               -- T_renameSending <= T_renameFull and nextAccepting and not eventSig;
                    renamedSendingSig <= T_renameFull and nextAccepting and not eventSig;


                  ch0 <=  renamedSendingSig xnor T_renameSending;
                  ch1 <= bool2std(renamedDataLivingPre_T = renamedDataLivingPre);

    COMMON_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then                
            if frontLastSending = '1' then
                specialActionSlot <= getSpecialActionSlot(renamedBase);
            end if;
            
            if frontLastSending = '1' then
                T_renameFull <= '1';
                renamedDataLivingPre <= stageDataRenameIn_T;
            elsif -- T_renameSending = '1' then
                  renamedSendingSig = '1' then
                T_renameFull <= '0';
                -- renamedDataLivingPre_T <= (others => DEFAULT_INS_SLOT);
            end if;
    
            if eventSig = '1' then
                T_renameFull <= '0';
            end if;
    
            renameGroupCtr <= renameGroupCtrNext;
            renameCtr <= renameCtrNext;
    
            -- Lock when exec part causes event
            if execEventSignal = '1' or lateEventSignal = '1' then -- CAREFUL
                renameLockState <= '1';    
            elsif renameLockReleaseDelayed = '1' then
                renameLockState <= '0';
            end if;
         
            stageDataToCommitDelayed <= stageDataToCommit;
            robSendingDelayed <= sendingFromROB;
            
            renameLockReleaseDelayed <= renameLockRelease;
            renameLockEndDelayed <= renameLockEndDelayedNext;
            
            if frontLastSending = '1' then             
                storedRenameInfoInt <= inputRenameInfoInt;
                storedRenameInfoFloat <= inputRenameInfoFloat;
            
                depVecPrev <= depVec;
            end if;

            sendingCommitInt <= sendingFromROB;
            stageDataCommitInt <= stageDataToCommit;
        end if;
    end process;

    renamedArgsInt <= storedRenameInfoInt;
    renamedArgsFloat <= storedRenameInfoFloat;

    stageDataToCommit <= setDestFlags(robDataLiving);

    sendingCommitFloat <= sendingCommitInt;
    stageDataCommitFloat <= stageDataCommitInt;

    INT_MAPPER: entity work.RegisterMapper
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEndDelayed,
        causingInstruction => DEFAULT_INSTRUCTION_STATE,
        
        sendingToReserve => frontLastSendingIn,
        stageDataToReserve => frontDataLastLiving,
        newPhysDestsOrig => newIntDests,    -- MAPPING (from FREE LIST)
        
        sendingToCommit => robSendingDelayed,   
        stageDataToCommit => stageDataToCommitDelayed,
        
        newPhysSources => newIntSources,
        newPhysSources_NR => newIntSources_NR,
        newPhysSourcesAlt => newIntSourcesAlt,
        newPhysSourceSelector => newSourceSelectorInt,
        prevStablePhysDests => physStableInt  -- FOR MAPPING (to FREE LIST)
    );
    
    FLOAT_MAPPER: entity work.RegisterMapper
    generic map (IS_FP => true)
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEndDelayed,
        causingInstruction => DEFAULT_INSTRUCTION_STATE,
        
        sendingToReserve => frontLastSendingIn,
        stageDataToReserve => frontDataLastLiving,
        newPhysDestsOrig => newFloatDests,
        
        sendingToCommit => robSendingDelayed,
        stageDataToCommit => stageDataToCommitDelayed,
        
        newPhysSources => newFloatSources,
        newPhysSources_NR => open,
        newPhysSourcesAlt => newFloatSourcesAlt,
        newPhysSourceSelector => newSourceSelectorFloat,
        prevStablePhysDests => physStableFloat
    );

                            
    INT_FREE_LIST: entity work.RegisterFreeList(Behavioral)
    port map(
        clk => clk,
        reset => '0',
        en => '0',
        
        rewind => eventSig,
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal,
        causingPointer => execCausing.tags.intPointer,
        
        sendingToReserve => frontLastSendingIn, 
        takeAllow => frontLastSendingIn,
        stageDataToReserve => frontDataLastLiving,
        
        newPhysDests => newIntDests,
        newPhysDestPointer => newIntDestPointer,
    
        sendingToRelease => sendingCommitInt,
        stageDataToRelease => stageDataCommitInt,
        
        physStableDelayed => physStableInt
    );
    
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
        
        sendingToReserve => frontLastSendingIn, 
        takeAllow => frontLastSendingIn,	-- FROM SEQ
        stageDataToReserve => frontDataLastLiving,
        
        newPhysDests => newFloatDests,			-- TO SEQ
        newPhysDestPointer => newFloatDestPointer, -- TO SEQ
    
        sendingToRelease => sendingCommitFloat,  -- FROM SEQ
        stageDataToRelease => stageDataCommitFloat,  -- FROM SEQ
        
        physStableDelayed => physStableFloat -- FOR MAPPING (from MAP)
    );
	
	specialActionOut <= specialActionSlot;
	
    newPhysDestsOut <= newIntDests;
    newFloatDestsOut <= newFloatDests; 
    renameAccepting <= not renameLockState;
 
    renamedSending <= renamedSendingSig;   
    
    renamingBr <= frontLastSending and frontDataLastLiving(0).ins.controlInfo.firstBr;
         
         TMP_MASKED_OUT: for i in 0 to PIPE_WIDTH-1 generate
            TMP_spMaskedDataOut(i) <= (renamedBase(i).full, frontDataLastLiving(i).ins);
         end generate;
         
end Behavioral;
