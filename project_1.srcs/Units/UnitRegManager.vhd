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
    frontData: in BufferEntryArray;
    
    branchMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    storeMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);
    loadMaskRe: out std_logic_vector(0 to PIPE_WIDTH-1);

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
   
    commitGroupCtr: in InsTag;
  
    execCausing: in ControlPacket;
    
    execEventSignal: in std_logic;
    lateEventSignal: in std_logic;
    
        dbState: in DbCoreState
);
end UnitRegManager;


architecture Behavioral of UnitRegManager is
    signal stageDataRenameIn, renamedDataLivingPre, renamedBase, frontDataISL, TMP_spMaskedData: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, robSendingDelayed, frontLastSending, renamedSendingSig,
               renameLockState, renameLockStateNext, renameLockEnd, renameLockEndDelayed, renameLockRelease, renameLockReleaseDelayed, renameLockEndDelayedNext,     
               renameFull,
                 ch0, ch1, ch2
               : std_logic := '0';
 
    signal renameGroupCtr, renameGroupCtrNext: InsTag := INITIAL_GROUP_TAG; -- This is rewinded on events
    signal renameCtr, renameCtrNext: Word := (others => '0');

    signal newIntDests, newFloatDests, physStableInt, physStableFloat, zeroDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newIntDestPointer, newFloatDestPointer: SmallNumber := (others => '0');
    signal newIntSources, newIntSources_NR, newIntSourcesAlt, newFloatSources, newFloatSourcesAlt, zeroSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
    signal newSourceSelectorInt, newSourceSelectorFloat, zeroSelector: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0'); 

    signal specialActionSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;    

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

      
    function renameGroupBase(
                            ia: BufferEntryArray;
                            insVec: InstructionSlotArray;
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
        variable tag: InsTag := renameGroupCtrNext;
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
                tag := renameGroupCtrNext or i2slv(i, TAG_SIZE);
                res(i).ins.dbInfo := DB_addTag(res(i).ins.dbInfo, tag);-- renameGroupCtrNext or i2slv(i, TAG_SIZE);
        
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

        return res;
    end function;

    function findDeps(ia: BufferEntryArray) return DependencyVec is
        variable res: DependencyVec := DEFAULT_DEP_VEC;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if ia(i).argSpec.args(k)(4 downto 0) = ia(j).argSpec.dest(4 downto 0) -- name match       
                    then
                        res(i)(k)(j) := '1';                   
                    end if;
                end loop;
            end loop;
        end loop;
        
        return res;
    end function;


    function getRealDepVecInt(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec is
        variable res: DependencyVec := (others => (others => (others => '0')));
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if depVec(i)(k)(j) = '1' and ia(i).argSpec.intArgSel(k) = '1' and ia(j).argSpec.intDestSel = '1' -- intSel match
                    then
                        res(i)(k)(j) := '1';
                        exit;                        
                    end if;
                end loop;
            end loop;                     
    
        end loop;        
        return res;
    end function;

    function getRealDepVecFloat(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec is
        variable res: DependencyVec := (others => (others => (others => '0')));
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if depVec(i)(k)(j) = '1' and ia(i).argSpec.floatArgSel(k) = '1' and ia(j).argSpec.floatDestSel = '1'
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

    function getRenameInfo( ia: BufferEntryArray; isa: InstructionSlotArray;
                            newPhysDests, newPhysSources, newPhysSourcesStable: PhysNameArray;
                            newProducers: InsTagArray;
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
                res(i).dbInfo := isa(i).ins.dbInfo;
        
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

            if IS_FP then
                res(i).sourceSel := va.floatArgSel;
            else
                res(i).sourceSel := va.intArgSel;
            end if;
                
            for j in 0 to 2 loop
                res(i).sourceConst(j) :=   (va.intArgSel(j) and not isNonzero(va.args(j)(4 downto 0))) -- int r0
                                        or (not va.intArgSel(j) and not va.floatArgSel(j))             -- not used
                                        or (bool2std(j = 1) and ca.immSel);                            -- imm

                res(i).virtualSources(j) := va.args(j)(4 downto 0);
                
                res(i).physicalSources(j) := newPhysSources(3*i + j);
                res(i).physicalSourcesStable(j) := newPhysSourcesStable(3*i + j);
                
                    res(i).dbDepTags(j) := newProducers(3*i + j);
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

    function getRenameInfoSC(insVec: InstructionSlotArray; constant IS_FP: boolean := false)
    return RenameInfoArray is
        variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
        variable va, pa: InstructionArgSpec := DEFAULT_ARG_SPEC;
        variable ca: InstructionConstantArgs := DEFAULT_CONSTANT_ARGS;
        variable depVec: DependencyVec;
    begin

        for i in 0 to PIPE_WIDTH-1 loop
            va := insVec(i).ins.virtualArgSpec;
            pa := insVec(i).ins.physicalArgSpec;
            ca := insVec(i).ins.constantArgs;
        
            if IS_FP then        
                res(i).destSel := va.floatDestSel;
                res(i).destSelFP := va.floatDestSel;
                
                res(i).psel := pa.floatDestSel;
            else
                res(i).destSel := va.intDestSel;
                res(i).psel := pa.intDestSel;
            end if;
            
            
            res(i).virtualDest := va.dest(4 downto 0);
            res(i).physicalDest := pa.dest;

            if IS_FP then
                res(i).sourceSel := va.floatArgSel;
            else
                res(i).sourceSel := va.intArgSel;
            end if;

            for j in 0 to 2 loop
                res(i).sourceConst(j) :=   (va.intArgSel(j) and not isNonzero(va.args(j)(4 downto 0))) -- int r0
                                        or (not va.intArgSel(j) and not va.floatArgSel(j))             -- not used
                                        or (bool2std(j = 1) and ca.immSel);                            -- imm
            end loop;

            res(i).deps := depVec(i);
            res(i).physicalSourcesNew := res(i).physicalSources;
                                
            for j in 0 to 2 loop
                res(i).sourcesNew(j) := isNonzero(res(i).deps(j));
                for k in PIPE_WIDTH-1 downto 0 loop
                    if res(i).deps(j)(k) = '1' then
                        exit;
                    end if;
                end loop;
            end loop;
            
            res(i).sourcesReady := (others => '0');
        end loop;
        return res;
    end function;
    
    signal inputRenameInfoInt, inputRenameInfoFloat, resultRenameInfoInt, resultRenameInfoFloat, storedRenameInfoInt, storedRenameInfoFloat,
                      commitArgInfoIntDelayed, commitArgInfoFloatDelayed: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
begin

    frontDataISL <= getInsSlotArray(frontData);

    inputRenameInfoInt <= getRenameInfo(frontData,    renamedBase, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector);
    inputRenameInfoFloat <= getRenameInfo(frontData,  renamedBase, zeroDests, zeroSources, zeroSources, zeroProducers, zeroSelector, true);

    resultRenameInfoInt <= getRenameInfo(frontData,   renamedBase, newIntDests, newIntSources, newIntSourcesAlt, newProducersInt, newSourceSelectorInt);
    resultRenameInfoFloat <= getRenameInfo(frontData, renamedBase, newFloatDests, newFloatSources, newFloatSourcesAlt, newProducersFloat, newSourceSelectorFloat, true);

    frontLastSending <= frontLastSendingIn and not eventSig;

    eventSig <= execEventSignal or lateEventSignal;

    renamedBase <= renameGroupBase( frontData,
                                    frontDataISL,
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

    stageDataRenameIn <= classifyForDispatch(TMP_recodeMem(renamedBase));

    renamedDataLiving <= restoreRenameIndex(renamedDataLivingPre);

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

    renamedSendingSig <= renameFull and nextAccepting and not eventSig;

    COMMON_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then                
            if frontLastSending = '1' then
                specialActionSlot <= getSpecialActionSlot(renamedBase);
            end if;
            
            if frontLastSending = '1' then
                renameFull <= '1';
                renamedDataLivingPre <= stageDataRenameIn;
            elsif renamedSendingSig = '1' then
                renameFull <= '0';
            end if;
    
            if eventSig = '1' then
                renameFull <= '0';
            end if;
    
            renameGroupCtr <= renameGroupCtrNext;
            renameCtr <= renameCtrNext;
    
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
        
        sendingToReserve => frontLastSendingIn,
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
    
    FLOAT_MAPPER: entity work.RegisterMapper
    generic map (IS_FP => true)
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEndDelayed,
        
        sendingToReserve => frontLastSendingIn,

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

        reserveInfoA => inputRenameInfoInt,
        newPhysDests => newIntDests,
        newPhysDestPointer => newIntDestPointer,
    
        sendingToRelease => robSendingDelayed,
        releaseInfoA => commitArgInfoIntDelayed,
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

        reserveInfoA => inputRenameInfoFloat,

        newPhysDests => newFloatDests,			-- TO SEQ
        newPhysDestPointer => newFloatDestPointer, -- TO SEQ
    
        sendingToRelease => robSendingDelayed,  -- FROM SEQ
        releaseInfoA => commitArgInfoFloatDelayed,
        physStableDelayed => physStableFloat -- FOR MAPPING (from MAP)
    );
	
    specialOut <= specialActionSlot.ins.specificOperation;
	
    newPhysDestsOut <= newIntDests;
    newFloatDestsOut <= newFloatDests; 
    renameAccepting <= not renameLockState;
 
    renamedSending <= renamedSendingSig;   
    
    renamingBr <= frontLastSending and frontDataISL(0).ins.controlInfo.firstBr;
         
         TMP_MASKED_OUT: for i in 0 to PIPE_WIDTH-1 generate
            TMP_spMaskedData(i) <= (renamedBase(i).full, frontDataISL(i).ins);
         end generate;

    branchMaskRe <= getBranchMask(TMP_spMaskedData);
    loadMaskRe <= getLoadMask(TMP_recodeMem(TMP_spMaskedData));
    storeMaskRe <= getStoreMask(TMP_recodeMem(TMP_spMaskedData));
end Behavioral;
