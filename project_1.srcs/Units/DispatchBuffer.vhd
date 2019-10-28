
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
           
           dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
           
           nextAccepting: in std_logic;
           
           accepting: out std_logic;
           
           prevSending: in std_logic;
           
           sending: out std_logic;
           dataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1);
           
           execEventSignal: in std_logic;
           lateEventSignal: in std_logic;          
           
           empty : out STD_LOGIC);
end DispatchBuffer;


architecture Behavioral of DispatchBuffer is
    signal queueData0, queueData1: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
    signal fullMask: std_logic_vector(0 to 1) := (others => '0');
    signal isSending: std_logic := '0';
begin
    
    isSending <= nextAccepting and fullMask(0) and not (execEventSignal or lateEventSignal);
    
    SYNCH: process (clk)
    begin
        if rising_edge(clk) then
            
            if isSending = '1' then
                queueData0 <= queueData1;
                fullMask(0) <= fullMask(1);
                fullMask(1) <= '0';
            end if;
            
            if prevSending = '1' then
                if fullMask(0) = '1' and nextAccepting = '0' then -- This means full(0) and not sending
                    queueData1 <= dataIn;
                    fullMask(1) <= '1';
                else
                    queueData0 <= dataIn;
                    fullMask(0) <= '1';
                end if;
                
                if fullMask(0) = '1' and fullMask(1) = '1' then -- If buffer full but putting more
                    report "DispatchBuffer overflow!" severity error;
                end if;
            end if;
            
            if execEventSignal = '1' or lateEventSignal = '1' then
                fullMask <= (others => '0');
            end if;
            
        end if;
    end process;
    
    sending <= isSending;
    dataOut <= queueData0;
    
    accepting <= not fullMask(0); -- Don't allow more if anything needed to be buffered!
end Behavioral;
