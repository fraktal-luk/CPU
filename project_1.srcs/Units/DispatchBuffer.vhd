
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;


entity DispatchBuffer is
    Port ( clk : in STD_LOGIC;
           
           specialAction: in InstructionSlot;
           dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
           
           nextAccepting: in std_logic;
           
           accepting: out std_logic;
           
           prevSending: in std_logic;
           
           sending: out std_logic;
           dataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1);
           specialOut: out InstructionSlot;
           
           execEventSignal: in std_logic;
           lateEventSignal: in std_logic;          
           
           empty : out STD_LOGIC);
end DispatchBuffer;


architecture Behavioral of DispatchBuffer is
    signal queueData0, queueData1: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
    signal special0, special1: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    signal fullMask: std_logic_vector(0 to 1) := (others => '0');
    signal isSending: std_logic := '0';   
begin
    
    isSending <= nextAccepting and fullMask(0) and not (execEventSignal or lateEventSignal);
    
    SYNCH: process (clk)
        variable fullMaskNew0: std_logic := '0';
    begin
        fullMaskNew0 := fullMask(0);
        if rising_edge(clk) then
            
            if isSending = '1' then
                queueData0 <= queueData1;
                special0 <= special1;
                --fullMask(0) <= fullMask(1);
                fullMaskNew0 := fullMask(1);
                fullMask(1) <= '0';
            end if;
            
            if prevSending = '1' then
                if --fullMask(0) = '1' and nextAccepting = '0' then -- This means full(0) and not sending
                   fullMaskNew0 = '1' then
                    queueData1 <= dataIn;
                    special1 <= specialAction;
                    fullMask(1) <= '1';
                else
                    queueData0 <= dataIn;
                    special0 <= specialAction;
                    fullMaskNew0 := '1';
                end if;
                
                if fullMask(0) = '1' and fullMask(1) = '1' then -- If buffer full but putting more
                    report "DispatchBuffer overflow!" severity failure;
                end if;
            end if;
            
            fullMask(0) <= fullMaskNew0;
            
            if execEventSignal = '1' or lateEventSignal = '1' then
                fullMask <= (others => '0');
            end if;
           
--           for i in 0 to PIPE_WIDTH-1 loop
               
--           end loop;
           
           
           if CLEAR_DEBUG_INFO then
                for i in 0 to PIPE_WIDTH-1 loop
                   queueData0(i).ins.ip <= (others => '0');
                   queueData0(i).ins.bits <= (others => '0');
                   queueData0(i).ins.result <= (others => '0');
                   queueData0(i).ins.target <= (others => '0');
                    
                   --queueData0(i).ins.constantArgs <= DEFAULT_CONSTANT_ARGS;
                 
                   --queueData0(i).ins.operation <= (System, sysUndef); --!! Operation must be known to UnitSequencer after commit
                    
--                         res(j).ops(i).ins.virtualArgSpec.intArgSel := (others => '0');
--                         res(j).ops(i).ins.virtualArgSpec.floatArgSel := (others => '0');
--                         res(j).ops(i).ins.virtualArgSpec.args := (others => (others => '0'));
        
--                         res(j).ops(i).ins.physicalArgSpec.intArgSel := (others => '0');
--                         res(j).ops(i).ins.physicalArgSpec.floatArgSel := (others => '0');
--                         res(j).ops(i).ins.physicalArgSpec.args := (others => (others => '0'));
                    
--                         res(j).ops(i).ins.virtualArgSpec.dest := (others => '0');  -- separate RAM
--                         res(j).ops(i).ins.physicalArgSpec.dest := (others => '0'); -- separate RAM
                    
                         
                        -- res(j).ops(i).ins.virtualArgSpec.intDestSel := '0';
                        -- res(j).ops(i).ins.physicalArgSpec.intDestSel := '0';
                    
                    --res(slv2u(j)).ops(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;
                     queueData0(i).ins.tags.fetchCtr <= (others => '0');
                     queueData0(i).ins.tags.decodeCtr <= (others => '0');
                     queueData0(i).ins.tags.renameCtr <= (others => '0');
                     if i > 0 then -- As in RegisterManager
                        queueData0(i).ins.tags.renameIndex <= (others => '0');
                     end if;
                     --    queueData0(i).ins.tags.intPointer <= (others => '0');
                     --    queueData0(i).ins.tags.floatPointer <= (others => '0');
        
        
                     queueData0(i).ins.tags.commitCtr <= (others => '0');                   
                end loop;
            end if;    

        end if;
    end process;
    
    sending <= isSending;
    dataOut <= --queueData0;
                restoreRenameIndex(queueData0);
    specialOut <= special0;

    accepting <= not fullMask(0); -- Don't allow more if anything needed to be buffered!
    empty <= not fullMask(0); -- CAREFUL: same as accepting but accepting refers to stage BEFORE Rename while empty is needed by flow FROM Rename
end Behavioral;
