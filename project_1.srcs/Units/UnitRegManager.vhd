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


entity UnitRegManager is
port(
    clk: in std_logic;
    
    renameAccepting: out std_logic;
    frontLastSending: in std_logic;
    frontDataLastLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    
    renamedDataLiving: out InstructionSlotArray(0 to PIPE_WIDTH-1);
    renamedDataLivingFloat: out InstructionSlotArray(0 to PIPE_WIDTH-1);    
    renamedSending: out std_logic;
    
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
    signal stageDataRenameIn, stageDataRenameInFloat, renamedDataLivingPre, renamedDataLivingFloatPre,
               stageDataToCommit, stageDataCommitInt, stageDataCommitFloat: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, sendingCommitInt, sendingCommitFloat, renameLockState, renameLockEnd, renameLockRelease: std_logic := '0';
 
    signal renameGroupCtr, renameGroupCtrNext: InsTag := INITIAL_GROUP_TAG; -- This is rewinded on events
    signal renameCtr, renameCtrNext: Word := (others => '0');

    signal newIntDests, newFloatDests, physStableInt, physStableFloat: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newIntDestPointer, newFloatDestPointer: SmallNumber := (others => '0');
    signal newIntSources, newFloatSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
    
    signal renamedBase: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    
    
    type DependencySpec is array(0 to 2) of std_logic_vector(0 to PIPE_WIDTH-1); 
    type DependencyVec is array(0 to PIPE_WIDTH-1) of DependencySpec;--  std_logic_vector(0 to 2);
    
    constant DEFAULT_DEP_VEC: DependencyVec := (others => (others => (others => '0')));

    signal depVec: DependencyVec := DEFAULT_DEP_VEC;
    
    signal specialActionSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    
    
    function renameGroupBase(insVec: InstructionSlotArray;
                                newIntDests: PhysNameArray;
                                newFloatDests: PhysNameArray;                                
                                renameGroupCtrNext: InsTag;
                                newIntDestPointer: SmallNumber;
                                newFloatDestPointer: SmallNumber;
                                renameCtr: Word;                               
                                dbtrap: std_logic
                                ) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
        variable reserveSelSig, takeVecInt, takeVecFloat: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
        variable nToTake: integer := 0;
        variable newGprTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));    
        variable newNumberTags: InsTagArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
       	variable found: boolean := false;
    begin      
        -- Assign dest registers
        for i in 0 to PIPE_WIDTH-1 loop
            if res(i).ins.virtualArgSpec.floatDestSel = '1' then
                res(i).ins.physicalArgSpec.dest := newFloatDests(countOnes(takeVecFloat)); -- how many used before
            else
                res(i).ins.physicalArgSpec.dest := newIntDests(countOnes(takeVecInt)); -- how many used before
            end if;
            takeVecInt(i) := insVec(i).ins.virtualArgSpec.intDestSel;
            takeVecFloat(i) := insVec(i).ins.virtualArgSpec.floatDestSel;
            
            res(i).ins.physicalArgSpec.intArgSel := res(i).ins.virtualArgSpec.intArgSel;
            res(i).ins.physicalArgSpec.intDestSel := res(i).ins.virtualArgSpec.intDestSel;
            res(i).ins.physicalArgSpec.floatArgSel := res(i).ins.virtualArgSpec.floatArgSel;
            res(i).ins.physicalArgSpec.floatDestSel := res(i).ins.virtualArgSpec.floatDestSel;     
        end loop;

        -- Setting tags
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            res(i).ins.tags.renameCtr := addInt(renameCtr, i);
            res(i).ins.tags.intPointer := addInt(newIntDestPointer, countOnes(takeVecInt(0 to i)));
                                                                         -- Don't increment pointer on ops which use no destination!
            res(i).ins.tags.floatPointer := addInt(newFloatDestPointer, countOnes(takeVecFloat(0 to i)));
        end loop;

        -- Setting 'complete' for ops not using Exec resources
        for i in 0 to PIPE_WIDTH-1 loop        -- TEMP!                           
		    res(i).ins.controlInfo.completed := not res(i).ins.classInfo.mainCluster;
            res(i).ins.controlInfo.completed2 := not res(i).ins.classInfo.secCluster;
        end loop;
                
        -- If found special instruction or exception, kill next ones
        for i in 0 to PIPE_WIDTH-1 loop
            if found then
                res(i).full := '0';
                    -- CAREFUL: needed for correct operation of StoreQueue
                    res(i).ins.classInfo.secCluster := '0';
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
                                newIntDests: PhysNameArray;
                                depVec: DependencyVec
                                ) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
    begin      
        -- Assign src registers
        for i in 0 to PIPE_WIDTH-1 loop                    
            res(i).ins.physicalArgSpec.args(0) := newPhysSources(3*i+0);
            res(i).ins.physicalArgSpec.args(1) := newPhysSources(3*i+1);
            res(i).ins.physicalArgSpec.args(2) := newPhysSources(3*i+2);      
        end loop;  
        
        -- Overwrite sources depending on destinations of this group
        for i in 0 to PIPE_WIDTH-1 loop
           for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if depVec(i)(k)(j) = '1'   
                        and res(i).ins.virtualArgSpec.intArgSel(k) = res(j).ins.virtualArgSpec.intDestSel -- intSel match
                    then
                        res(i).ins.physicalArgSpec.args(k) := res(j).ins.physicalArgSpec.dest;
                        exit;             
                    end if;
                end loop;
            end loop;

        end loop;        
          
        return res;
    end function;
    
 
    function renameGroupFloat(insVec: InstructionSlotArray;
                                newFloatSources: PhysNameArray;
                                newFloatDests: PhysNameArray;
                                depVec: DependencyVec
                                ) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
        variable tmpArgSpec: InstructionArgSpec := DEFAULT_ARG_SPEC;
    begin
        -- Assign src registers
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.physicalArgSpec.args(0) := newFloatSources(3*i+0);
            res(i).ins.physicalArgSpec.args(1) := newFloatSources(3*i+1);
            res(i).ins.physicalArgSpec.args(2) := newFloatSources(3*i+2);           
        end loop;
        
        -- Overwrite sources depending on destinations of this group
        -- TODO: arg name comparisons are shared with int registers! Reuse the same comparators
        for i in 0 to PIPE_WIDTH-1 loop
            for k in 0 to 2 loop -- For each of 3 possible source arguments
                for j in PIPE_WIDTH-1 downto 0 loop
                    if j >= i then
                        next;
                    end if;
                    
                    if depVec(i)(k)(j) = '1'
                        and res(i).ins.virtualArgSpec.floatArgSel(k) = res(j).ins.virtualArgSpec.floatDestSel -- intSel match
                    then
                        res(i).ins.physicalArgSpec.args(k) := res(j).ins.physicalArgSpec.dest;
                        exit;                   
                    end if;
                end loop;
            end loop;                     

        end loop;

        -- TODO: move this somewhere else?
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
    
begin
    eventSig <= execEventSignal or lateEventSignal;

    depVec <= findDeps(frontDataLastLiving);

    renamedBase <= renameGroupBase(frontDataLastLiving,
                                    newIntDests, 
                                    newFloatDests,
                                    renameGroupCtrNext,
                                    newIntDestPointer,
                                    newFloatDestPointer,
                                    renameCtr,
                                    '0' --dbtrapOn
                                    );

    stageDataRenameIn <=        renameGroupInt(     renamedBase, newIntSources,   newIntDests,   depVec);  -- TODO: dests already assigned in renameGroupBase - remove here? 
    stageDataRenameInFloat <=   renameGroupFloat(   renamedBase, newFloatSources, newFloatDests, depVec); -- like above
    -- TODO: ^ or assign dests above, not in renameGroupBase, to keep Int and FP path separate, and merge them ony when going to ROB - it could be good for layout
                                                                                                                            
    SUBUNIT_RENAME_INT: entity work.GenericStage(Behavioral)--Renaming)
    generic map(
        USE_CLEAR => '0',
        WIDTH => PIPE_WIDTH
    )
    port map(
        clk => clk, reset => '0', en => '0',
        
        -- Interface with front
        prevSending => frontLastSending,    
        stageDataIn => stageDataRenameIn,
        
        acceptingOut => open,
        
        -- Interface with IQ
        nextAccepting => '1',
        sendingOut => renamedSending,
        stageDataOut => renamedDataLivingPre,
        
        -- Event interface
        execEventSignal => '0',
        lateEventSignal => eventSig, -- because Exec is always older than Rename     
        execCausing => DEFAULT_INSTRUCTION_STATE
    );
    
    
    SUBUNIT_RENAME_FLOAT: entity work.GenericStage(Behavioral)--Renaming)
    generic map(
        USE_CLEAR => '0',
        WIDTH => PIPE_WIDTH
    )
    port map(
        clk => clk, reset => '0', en => '0',
        
        -- Interface with front
        prevSending => frontLastSending,    
        stageDataIn => stageDataRenameInFloat,
        
        acceptingOut => open,
        
        -- Interface with IQ
        nextAccepting => '1',
        sendingOut => open,
        stageDataOut => renamedDataLivingFloatPre,
        
        -- Event interface
        execEventSignal => '0',
        lateEventSignal => eventSig, -- because Exec is always older than Rename     
        execCausing => DEFAULT_INSTRUCTION_STATE
    );
    
    renamedDataLiving <= restoreRenameIndex(renamedDataLivingPre);
    renamedDataLivingFloat <= restoreRenameIndex(renamedDataLivingFloatPre);
    
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
    
    renameLockEnd <= renameLockState and renameLockRelease;
        
    COMMON_SYNCHRONOUS: process(clk)     
    begin
        if rising_edge(clk) then                
            if frontLastSending = '1' then
                specialActionSlot <= getSpecialActionSlot(renamedBase);
            end if;
    
            renameGroupCtr <= renameGroupCtrNext;
            renameCtr <= renameCtrNext;
    
            -- Lock when exec part causes event
            if execEventSignal = '1' or lateEventSignal = '1' then -- CAREFUL
                renameLockState <= '1';    
            elsif renameLockRelease = '1' then
                renameLockState <= '0';
            end if;                    
        end if;    
    end process;
    
    stageDataToCommit <= setDestFlags(robDataLiving);
    
    SUBUNIT_COMMIT_INT: entity work.GenericStage(Behavioral)
    generic map(
        WIDTH => PIPE_WIDTH
    )
    port map(
        clk => clk, reset => '0', en => '0',
        
        -- Interface with CQ
        prevSending => sendingFromROB,
        stageDataIn => stageDataToCommit,
        acceptingOut => open, -- unused but don't remove
        
        -- Interface with hypothetical further stage
        nextAccepting => '1',
        sendingOut => sendingCommitInt,
        stageDataOut => stageDataCommitInt,
        
        -- Event interface
        execEventSignal => '0', -- CAREFUL: committed cannot be killed!
        lateEventSignal => '0',    
        execCausing => execCausing
    );
        
    sendingCommitFloat <= sendingCommitInt;
    stageDataCommitFloat <= stageDataCommitInt;

    INT_MAPPER: entity work.RegisterMapper
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEnd,    -- FROM SEQ
        causingInstruction => DEFAULT_INSTRUCTION_STATE,
        
        sendingToReserve => frontLastSending,
        stageDataToReserve => frontDataLastLiving,
        newPhysDests => newIntDests,    -- MAPPING (from FREE LIST)
        
        sendingToCommit => sendingFromROB,
        stageDataToCommit => stageDataToCommit,
        physCommitDests_TMP => (others => (others => '0')), -- CAREFUL: useless input?
        
        prevNewPhysDests => open,
        newPhysSources => newIntSources,    -- TO SEQ
        
        prevStablePhysDests => physStableInt,  -- FOR MAPPING (to FREE LIST)
        stablePhysSources => open           
    );
    
    FLOAT_MAPPER: entity work.RegisterMapper
    generic map (IS_FP => true)
    port map(
        clk => clk, en => '0', reset => '0',
        
        rewind => renameLockEnd,    -- FROM SEQ
        causingInstruction => DEFAULT_INSTRUCTION_STATE,
        
        sendingToReserve => frontLastSending,
        stageDataToReserve => frontDataLastLiving,
        newPhysDests => newFloatDests,    -- MAPPING (from FREE LIST)
        
        sendingToCommit => sendingFromROB,
        stageDataToCommit => stageDataToCommit,
        physCommitDests_TMP => (others => (others => '0')), -- CAREFUL: useless input?
        
        prevNewPhysDests => open,
        newPhysSources => newFloatSources,    -- TO SEQ
        
        prevStablePhysDests => physStableFloat,  -- FOR MAPPING (to FREE LIST)
        stablePhysSources => open           
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
        
        sendingToReserve => frontLastSending, 
        takeAllow => frontLastSending,	-- FROM SEQ
        auxTakeAllow => renameLockEnd,
        stageDataToReserve => frontDataLastLiving,
        
        newPhysDests => newIntDests,			-- TO SEQ
        newPhysDestPointer => newIntDestPointer, -- TO SEQ
    
        sendingToRelease => sendingCommitInt,  -- FROM SEQ
        stageDataToRelease => stageDataCommitInt,  -- FROM SEQ
        
        physStableDelayed => physStableInt -- FOR MAPPING (from MAP)
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
        
        sendingToReserve => frontLastSending, 
        takeAllow => frontLastSending,	-- FROM SEQ
        auxTakeAllow => renameLockEnd,
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
end Behavioral;
