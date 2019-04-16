----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 01.12.2018 23:35:07
-- Design Name: 
-- Module Name: UnitRegManager - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
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
    
    robDataLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    sendingFromROB: in std_logic;
   
    commitGroupCtr: in InsTag;
    commitCtr: in InsTag;
  
    execCausing: in InstructionState;
    lateCausing: in InstructionState;
    
    execEventSignal: in std_logic;
    lateEventSignal: in std_logic
);
end UnitRegManager;


architecture Behavioral of UnitRegManager is
    signal stageDataRenameIn, stageDataRenameInFloat,
                stageDataCommitInt, stageDataCommitFloat: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, sendingCommitInt, sendingCommitFloat, renameLockState, renameLockEnd, renameLockRelease: std_logic := '0';
 
    signal renameCtr, renameCtrNext: InsTag := INITIAL_RENAME_CTR;
    signal renameGroupCtr, renameGroupCtrNext: InsTag := INITIAL_GROUP_TAG;

    signal newIntDests, newFloatDests, physStableInt, physStableFloat: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newIntDestPointer, newFloatDestPointer: SmallNumber := (others => '0');
    signal newIntSources, newFloatSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal causingPtrInt, causingPtrFloat: SmallNumber := (others => '0');
    
    signal renamedBase: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    function renameGroupBase(insVec: InstructionSlotArray;
                                newPhysSources: PhysNameArray;
                                newIntDests: PhysNameArray;
                                newFloatDests: PhysNameArray;                                
                                renameCtr: InsTag;
                                renameGroupCtrNext: InsTag;
                                newIntDestPointer: SmallNumber;
                                newFloatDestPointer: SmallNumber;                                
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
                           
            --res(i).ins.physicalArgSpec.args(0) := newPhysSources(3*i+0);
            --res(i).ins.physicalArgSpec.args(1) := newPhysSources(3*i+1);
            --res(i).ins.physicalArgSpec.args(2) := newPhysSources(3*i+2);      
        end loop;


        -- Setting tags
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            res(i).ins.tags.renameSeq := i2slv(slv2u(renameCtr) + i, TAG_SIZE);
            res(i).ins.tags.intPointer := i2slv(slv2u(newIntDestPointer) + countOnes(takeVecInt(0 to i)), SMALL_NUMBER_SIZE); 
                                                                         -- Don't increment pointer on ops which use no destination!
            res(i).ins.tags.floatPointer := i2slv(slv2u(newFloatDestPointer) + countOnes(takeVecFloat(0 to i)), SMALL_NUMBER_SIZE); 
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
            end if;
    
            if     res(i).ins.controlInfo.specialAction = '1'
                or res(i).ins.controlInfo.hasException = '1'
                or res(i).ins.controlInfo.dbtrap = '1'
            then
                found := true;
            end if;
        end loop;
        
        
--        -- Overwrite sources depending on destinations of this group
--        for i in 0 to PIPE_WIDTH-1 loop
--            for j in PIPE_WIDTH-1 downto 0 loop
--                if j >= i then
--                    next;
--                end if;
                
--                if res(i).ins.virtualArgSpec.args(0)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0) -- name match
--                    and res(i).ins.virtualArgSpec.intArgSel(0) = res(j).ins.virtualArgSpec.intDestSel -- intSel match
--                then
--                    res(i).ins.physicalArgSpec.args(0) := res(j).ins.physicalArgSpec.dest;
--                    exit;                   
--                end if;
--            end loop;

--            for j in PIPE_WIDTH-1 downto 0 loop
--                if j >= i then
--                    next;
--                end if;
                
--                if res(i).ins.virtualArgSpec.args(1)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
--                    and res(i).ins.virtualArgSpec.intArgSel(1) = res(j).ins.virtualArgSpec.intDestSel
--                then
--                    res(i).ins.physicalArgSpec.args(1) := res(j).ins.physicalArgSpec.dest;
--                    exit;                 
--                end if;
--            end loop;
            
--            for j in PIPE_WIDTH-1 downto 0 loop
--                if j >= i then
--                    next;
--                end if;
                
--                if res(i).ins.virtualArgSpec.args(2)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
--                    and res(i).ins.virtualArgSpec.intArgSel(2) = res(j).ins.virtualArgSpec.intDestSel
--                then
--                    res(i).ins.physicalArgSpec.args(2) := res(j).ins.physicalArgSpec.dest;
--                    exit;                  
--                end if;
--            end loop;                        

--        end loop;        
          
        return res;
    end function;
       
    function renameGroupInt(insVec: InstructionSlotArray;
                                newPhysSources: PhysNameArray;
                                newIntDests: PhysNameArray;
                                newFloatDests: PhysNameArray;                                
                                renameCtr: InsTag;
                                renameGroupCtrNext: InsTag;
                                newIntDestPointer: SmallNumber;
                                newFloatDestPointer: SmallNumber;                                
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
--            if res(i).ins.virtualArgSpec.floatDestSel = '1' then
--                res(i).ins.physicalArgSpec.dest := newFloatDests(countOnes(takeVecFloat)); -- how many used before
--            else
--                res(i).ins.physicalArgSpec.dest := newIntDests(countOnes(takeVecInt)); -- how many used before
--            end if;
--            takeVecInt(i) := insVec(i).ins.virtualArgSpec.intDestSel;
--            takeVecFloat(i) := insVec(i).ins.virtualArgSpec.floatDestSel;
            
--            res(i).ins.physicalArgSpec.intArgSel := res(i).ins.virtualArgSpec.intArgSel;
--            res(i).ins.physicalArgSpec.intDestSel := res(i).ins.virtualArgSpec.intDestSel;
--            res(i).ins.physicalArgSpec.floatArgSel := res(i).ins.virtualArgSpec.floatArgSel;
--            res(i).ins.physicalArgSpec.floatDestSel := res(i).ins.virtualArgSpec.floatDestSel;
                           
            res(i).ins.physicalArgSpec.args(0) := newPhysSources(3*i+0);
            res(i).ins.physicalArgSpec.args(1) := newPhysSources(3*i+1);
            res(i).ins.physicalArgSpec.args(2) := newPhysSources(3*i+2);      
        end loop;


--        -- Setting tags
--        for i in 0 to PIPE_WIDTH-1 loop
--            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
--            res(i).ins.tags.renameSeq := i2slv(slv2u(renameCtr) + i, TAG_SIZE);
--            res(i).ins.tags.intPointer := i2slv(slv2u(newIntDestPointer) + countOnes(takeVecInt(0 to i)), SMALL_NUMBER_SIZE); 
--                                                                         -- Don't increment pointer on ops which use no destination!
--            res(i).ins.tags.floatPointer := i2slv(slv2u(newFloatDestPointer) + countOnes(takeVecFloat(0 to i)), SMALL_NUMBER_SIZE); 
--        end loop;

--        -- Setting 'complete' for ops not using Exec resources
--        for i in 0 to PIPE_WIDTH-1 loop        -- TEMP!                           
--		    res(i).ins.controlInfo.completed := not res(i).ins.classInfo.mainCluster;
--            res(i).ins.controlInfo.completed2 := not res(i).ins.classInfo.secCluster;
--        end loop;
                
--        -- If found special instruction or exception, kill next ones
--        for i in 0 to PIPE_WIDTH-1 loop
--            if found then
--                res(i).full := '0';
--            end if;
    
--            if     res(i).ins.controlInfo.specialAction = '1'
--                or res(i).ins.controlInfo.hasException = '1'
--                or res(i).ins.controlInfo.dbtrap = '1'
--            then
--                found := true;
--            end if;
--        end loop;
        
        
        -- Overwrite sources depending on destinations of this group
        for i in 0 to PIPE_WIDTH-1 loop
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(0)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0) -- name match
                    and res(i).ins.virtualArgSpec.intArgSel(0) = res(j).ins.virtualArgSpec.intDestSel -- intSel match
                then
                    res(i).ins.physicalArgSpec.args(0) := res(j).ins.physicalArgSpec.dest;
                    exit;                   
                end if;
            end loop;

            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(1)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
                    and res(i).ins.virtualArgSpec.intArgSel(1) = res(j).ins.virtualArgSpec.intDestSel
                then
                    res(i).ins.physicalArgSpec.args(1) := res(j).ins.physicalArgSpec.dest;
                    exit;                 
                end if;
            end loop;
            
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(2)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
                    and res(i).ins.virtualArgSpec.intArgSel(2) = res(j).ins.virtualArgSpec.intDestSel
                then
                    res(i).ins.physicalArgSpec.args(2) := res(j).ins.physicalArgSpec.dest;
                    exit;                  
                end if;
            end loop;                        

        end loop;        
          
        return res;
    end function;
    
 
    function renameGroupFloat(insVec: InstructionSlotArray;
                                newFloatSources: PhysNameArray;
                                newFloatDests: PhysNameArray;
                                renameCtr: InsTag;
                                renameGroupCtrNext: InsTag;
                                --newPhysDestPointer: SmallNumber;
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
--            if res(i).ins.virtualArgSpec.floatDestSel = '1' then
--                res(i).ins.physicalArgSpec.dest := newFloatDests(countOnes(takeVecFloat)); -- how many used before
--            end if;
--            --takeVecInt(i) := insVec(i).ins.virtualArgSpec.intDestSel;
--            takeVecFloat(i) := insVec(i).ins.virtualArgSpec.floatDestSel;
            
            
            res(i).ins.physicalArgSpec.args(0) := newFloatSources(3*i+0);
            res(i).ins.physicalArgSpec.args(1) := newFloatSources(3*i+1);
            res(i).ins.physicalArgSpec.args(2) := newFloatSources(3*i+2);
            
            --res(i).ins.physicalArgSpec.intArgSel := res(i).ins.virtualArgSpec.intArgSel;
            --res(i).ins.physicalArgSpec.intDestSel := res(i).ins.virtualArgSpec.intDestSel;
            --    res(i).ins.physicalArgSpec.floatArgSel := res(i).ins.virtualArgSpec.floatArgSel;
            --    res(i).ins.physicalArgSpec.floatDestSel := res(i).ins.virtualArgSpec.floatDestSel;            
        end loop;
        
        -- Overwrite sources depending on destinations of this group
        -- TODO: arg name comparisons are shared with int registers! Reuse the same comparators
        for i in 0 to PIPE_WIDTH-1 loop
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(0)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0) -- name match
                    and res(i).ins.virtualArgSpec.floatArgSel(0) = res(j).ins.virtualArgSpec.floatDestSel -- intSel match
                then
                    res(i).ins.physicalArgSpec.args(0) := res(j).ins.physicalArgSpec.dest;
                    exit;                   
                end if;
            end loop;

            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(1)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
                    and res(i).ins.virtualArgSpec.floatArgSel(1) = res(j).ins.virtualArgSpec.floatDestSel
                then
                    res(i).ins.physicalArgSpec.args(1) := res(j).ins.physicalArgSpec.dest;
                    exit;                 
                end if;
            end loop;
            
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(2)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
                    and res(i).ins.virtualArgSpec.floatArgSel(2) = res(j).ins.virtualArgSpec.floatDestSel
                then
                    res(i).ins.physicalArgSpec.args(2) := res(j).ins.physicalArgSpec.dest;
                    exit;                  
                end if;
            end loop;                        

        end loop;        
        
        
--        -- Setting tags
--        for i in 0 to PIPE_WIDTH-1 loop
--            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
--            res(i).ins.tags.renameSeq := i2slv(slv2u(renameCtr) + i, TAG_SIZE);
--            --res(i).ins.tags.intPointer := i2slv(slv2u(newPhysDestPointer) + countOnes(takeVecInt(0 to i)), SMALL_NUMBER_SIZE); 
--                                                                         -- Don't increment pointer on ops which use no destination!
--            --res(i).ins.tags.floatPointer := i2slv(slv2u(newFloatDestPointer) + countOnes(takeVecFloat(0 to i)), SMALL_NUMBER_SIZE); 
--        end loop;

--        -- Setting 'complete' for ops not using Exec resources
--        for i in 0 to PIPE_WIDTH-1 loop        -- TEMP!                           
--            res(i).ins.controlInfo.completed := not res(i).ins.classInfo.mainCluster;
--            res(i).ins.controlInfo.completed2 := not res(i).ins.classInfo.secCluster;
--        end loop;
                
--        -- If found special instruction or exception, kill next ones
--        for i in 0 to PIPE_WIDTH-1 loop
--            if found then
--                res(i).full := '0';
--            end if;
    
--            if     res(i).ins.controlInfo.specialAction = '1'
--                or res(i).ins.controlInfo.hasException = '1'
--                or res(i).ins.controlInfo.dbtrap = '1'
--            then
--                found := true;
--            end if;
--        end loop;

        -- TODO: move this somewhere else?
        for i in res'range loop
            if res(i).ins.classInfo.fpRename = '0' then
                res(i).full := '0';
            end if;
        end loop;
            
        return res;
    end function;    
begin
    eventSig <= execEventSignal or lateEventSignal;

    renamedBase <= renameGroupBase(frontDataLastLiving,
                                                          newIntSources, 
                                                          newIntDests, 
                                                                newFloatDests, -- TEMP! change to float dests
                                                            renameCtr,
                                                            renameGroupCtrNext,
                                                            newIntDestPointer,
                                                            newFloatDestPointer,
                                                            '0' --dbtrapOn
                                                            );

    stageDataRenameIn <= renameGroupInt(renamedBase,
                                                          newIntSources, 
                                                          newIntDests, 
                                                                newFloatDests, -- TEMP! change to float dests
                                                            renameCtr,
                                                            renameGroupCtrNext,
                                                            newIntDestPointer,
                                                            newFloatDestPointer,
                                                            '0' --dbtrapOn
                                                            );

    stageDataRenameInFloat <= renameGroupFloat(renamedBase,
                                                          newFloatSources, -- TEMP! change to float 
                                                                newFloatDests, -- TEMP! change to float dests
                                                            renameCtr,
                                                            renameGroupCtrNext,
                                                            --(others => '0'), -- TEMP!
                                                            '0' --dbtrapOn
                                                            );
                                                                                                                            
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
            stageDataOut => renamedDataLiving,
            
            -- Event interface
            execEventSignal => '0',
            lateEventSignal => eventSig, -- bcause Exec is always older than Rename     
            execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
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
            sendingOut => open,--renamedSending,
            stageDataOut => renamedDataLivingFloat,
            
            -- Event interface
            execEventSignal => '0',
            lateEventSignal => eventSig, -- bcause Exec is always older than Rename     
            execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
        );
        
            renameGroupCtrNext <= 
                                    commitGroupCtr when lateEventSignal = '1'
                               else clearTagLow(execCausing.tags.renameIndex) when execEventSignal = '1'
                               else i2slv(slv2u(renameGroupCtr) + PIPE_WIDTH, TAG_SIZE) when frontLastSending = '1'
                               else renameGroupCtr;
                                                                                                       
            renameCtrNext <=  
                                    commitCtr when lateEventSignal = '1'
                               else execCausing.tags.renameSeq when execEventSignal = '1'
                               else i2slv(slv2u(renameCtr) + countOnes(extractFullMask(stageDataRenameIn)),
                                                                                TAG_SIZE)
                                                                             when frontLastSending = '1'
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
                    renameCtr <= renameCtrNext;
                    renameGroupCtr <= renameGroupCtrNext;
        
                    -- Lock when exec part causes event
                    if execEventSignal = '1' or lateEventSignal = '1' then -- CAREFUL
                        renameLockState <= '1';    
                    elsif renameLockRelease = '1' then
                        renameLockState <= '0';
                    end if;                    
                end if;    
            end process;

        SUBUNIT_COMMIT_INT: entity work.GenericStage(Behavioral)
        generic map(
            WIDTH => PIPE_WIDTH
        )
        port map(
            clk => clk, reset => '0', en => '0',
            
            -- Interface with CQ
            prevSending => sendingFromROB,
            stageDataIn => robDataLiving,
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
         
        SUBUNIT_COMMIT_FLOAT: entity work.GenericStage(Behavioral)
        generic map(
            WIDTH => PIPE_WIDTH
        )
        port map(
            clk => clk, reset => '0', en => '0',
            
            -- Interface with CQ
            prevSending => sendingFromROB,
            stageDataIn => robDataLiving,
            acceptingOut => open, -- unused but don't remove
            
            -- Interface with hypothetical further stage
            nextAccepting => '1',
            sendingOut => sendingCommitFloat,
            stageDataOut => stageDataCommitFloat,
            
            -- Event interface
            execEventSignal => '0', -- CAREFUL: committed cannot be killed!
            lateEventSignal => '0',    
            execCausing => execCausing
        );
          
        INT_MAPPER: entity work.RegisterMapper
        port map(
            clk => clk, en => '0', reset => '0',
            
            rewind => renameLockEnd,    -- FROM SEQ
            causingInstruction => DEFAULT_INSTRUCTION_STATE,
            
            sendingToReserve => frontLastSending,
            stageDataToReserve => frontDataLastLiving,
            newPhysDests => newIntDests,    -- MAPPING (from FREE LIST)

            sendingToCommit => sendingFromROB,
            stageDataToCommit => robDataLiving,
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
            stageDataToCommit => robDataLiving,
            physCommitDests_TMP => (others => (others => '0')), -- CAREFUL: useless input?
            
            prevNewPhysDests => open,
            newPhysSources => newFloatSources,    -- TO SEQ
            
            prevStablePhysDests => physStableFloat,  -- FOR MAPPING (to FREE LIST)
            stablePhysSources => open            
        );
        
            causingPtrInt <=    lateCausing.tags.intPointer when lateEventSignal = '1'
                        else execCausing.tags.intPointer;
            causingPtrFloat <=    lateCausing.tags.floatPointer when lateEventSignal = '1'
                                    else execCausing.tags.floatPointer;
                                    
			INT_FREE_LIST: entity work.RegisterFreeList(Behavioral)
			port map(
				clk => clk,
				reset => '0',
				en => '0',
				
				rewind => eventSig,
				causingPointer => causingPtrInt,
				
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
				causingPointer => causingPtrFloat,
				
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
			        
        newPhysDestsOut <= newIntDests;
        renameAccepting <= not renameLockState;
end Behavioral;
