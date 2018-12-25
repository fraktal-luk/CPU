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
    renamedSending: out std_logic;
    
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
    signal stageDataRenameIn,
                stageDataCommit: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal eventSig, sendingCommit, renameLockState, renameLockEnd, renameLockRelease: std_logic := '0';
 
    signal renameCtr, renameCtrNext: InsTag := INITIAL_RENAME_CTR;
    signal renameGroupCtr, renameGroupCtrNext: InsTag := INITIAL_GROUP_TAG;

    signal newPhysDests, physStable: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal newPhysDestPointer: SmallNumber := (others => '0');
    signal newPhysSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal causingPtr: SmallNumber := (others => '0');
       
    function renameGroup(insVec: InstructionSlotArray;
                                newPhysSources: PhysNameArray;
                                newPhysDests: PhysNameArray;
                                renameCtr: InsTag;
                                renameGroupCtrNext: InsTag;
                                newPhysDestPointer: SmallNumber;
                                dbtrap: std_logic
                                ) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
        variable reserveSelSig, takeVec: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
        variable nToTake: integer := 0;
        variable newGprTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));    
        variable newNumberTags: InsTagArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
       	variable found: boolean := false;
    begin
        
        -- Assign dest registers
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.physicalArgSpec.dest := newPhysDests(countOnes(takeVec)); -- how many used before
            takeVec(i) := insVec(i).ins.virtualArgSpec.intDestSel;
            res(i).ins.physicalArgSpec.args(0) := newPhysSources(0);
            res(i).ins.physicalArgSpec.args(1) := newPhysSources(1);
            res(i).ins.physicalArgSpec.args(2) := newPhysSources(2);
            
            res(i).ins.physicalArgSpec.intArgSel := res(i).ins.virtualArgSpec.intArgSel;
            res(i).ins.physicalArgSpec.intDestSel := res(i).ins.virtualArgSpec.intDestSel;
        end loop;
        
        -- Overwrite sources depending on destinations of this group
        for i in 0 to PIPE_WIDTH-1 loop
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(0)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
                    and res(i).ins.virtualArgSpec.intArgSel(0) = res(j).ins.virtualArgSpec.intDestSel
                then
                    res(i).ins.physicalArgSpec.args(0) := newPhysSources(j);                    
                end if;
            end loop;

            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(1)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
                    and res(i).ins.virtualArgSpec.intArgSel(1) = res(j).ins.virtualArgSpec.intDestSel
                then
                    res(i).ins.physicalArgSpec.args(1) := newPhysSources(j);                    
                end if;
            end loop;
            
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if res(i).ins.virtualArgSpec.args(2)(4 downto 0) = res(j).ins.virtualArgSpec.dest(4 downto 0)
                    and res(i).ins.virtualArgSpec.intArgSel(2) = res(j).ins.virtualArgSpec.intDestSel
                then
                    res(i).ins.physicalArgSpec.args(2) := newPhysSources(j);                    
                end if;
            end loop;                        

        end loop;        
        
        
        -- Setting tags
        for i in 0 to PIPE_WIDTH-1 loop
            res(i).ins.tags.renameIndex := renameGroupCtrNext or i2slv(i, TAG_SIZE);
            res(i).ins.tags.renameSeq := i2slv(slv2u(renameCtr) + i, TAG_SIZE);
            res(i).ins.tags.intPointer := i2slv(slv2u(newPhysDestPointer) + countOnes(takeVec(0 to i-1)),
                                                                SMALL_NUMBER_SIZE); 
                                           -- Don't increment intPointer on ops which use no destination!
                                   -- TODO: FP pointer
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
        return res;
    end function;
begin
    eventSig <= execEventSignal or lateEventSignal;

    stageDataRenameIn <= renameGroup(frontDataLastLiving,
                                                          newPhysSources, newPhysDests, 
                                                            renameCtr,
                                                            renameGroupCtrNext,
                                                            newPhysDestPointer,
                                                            '0' --dbtrapOn
                                                            );
                                                                
        SUBUNIT_RENAME: entity work.GenericStage(Behavioral)--Renaming)
        generic map(
            USE_CLEAR => '0',
            WIDTH => PIPE_WIDTH
        )
        port map(
            clk => clk, reset => '0', en => '0',
            
            -- Interface with front
            prevSending => frontLastSending,    
            stageDataIn => stageDataRenameIn,
            
            acceptingOut => renameAccepting,
            
            -- Interface with IQ
            nextAccepting => '1',
            sendingOut => renamedSending,
            stageDataOut => renamedDataLiving,
            
            -- Event interface
            execEventSignal => '0',
            lateEventSignal => eventSig, -- bcause Exec is always older than Rename     
            execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
        );


            renameGroupCtrNext <= 
                                    commitGroupCtr when lateEventSignal = '1'
                               else execCausing.tags.renameIndex when execEventSignal = '1'
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

        SUBUNIT_COMMIT: entity work.GenericStage(Behavioral)
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
            sendingOut => sendingCommit,
            stageDataOut => stageDataCommit,
            
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
            newPhysDests => newPhysDests,    -- MAPPING (from FREE LIST)

            sendingToCommit => sendingFromROB,
            stageDataToCommit => robDataLiving,
            physCommitDests_TMP => (others => (others => '0')), -- CAREFUL: useless input?
            
            prevNewPhysDests => open,
            newPhysSources => newPhysSources,    -- TO SEQ
            
            prevStablePhysDests => physStable,  -- FOR MAPPING (to FREE LIST)
            stablePhysSources => open            
        );
        

            causingPtr <=    lateCausing.tags.intPointer when lateEventSignal = '1'
                        else execCausing.tags.intPointer;

			INT_FREE_LIST: entity work.RegisterFreeList(Behavioral)
			port map(
				clk => clk,
				reset => '0',
				en => '0',
				
				rewind => eventSig,
				causingPointer => causingPtr,
				
				sendingToReserve => frontLastSending, 
				takeAllow => frontLastSending,	-- FROM SEQ
				auxTakeAllow => renameLockEnd,
				stageDataToReserve => frontDataLastLiving,
				
				newPhysDests => newPhysDests,			-- TO SEQ
				newPhysDestPointer => newPhysDestPointer, -- TO SEQ

				sendingToRelease => sendingCommit,  -- FROM SEQ
				stageDataToRelease => stageDataCommit,  -- FROM SEQ
				
				physStableDelayed => physStable -- FOR MAPPING (from MAP)
			);
        
end Behavioral;
