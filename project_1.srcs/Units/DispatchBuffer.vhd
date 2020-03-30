
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;


entity DispatchBuffer is
    generic (
        IS_FP: boolean := false
    );
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
                --queueData0 <= queueData1;
                --special0 <= special1;
                --fullMask(0) <= fullMask(1);
                fullMaskNew0 := '0';--fullMask(1);
                --fullMask(1) <= '0';
            end if;
            
            if prevSending = '1' then
                --if fullMaskNew0 = '1' then
                --    queueData1 <= dataIn;
                --    special1 <= specialAction;
                --    fullMask(1) <= '1';
                --else
                    queueData0 <= dataIn;
                    special0 <= specialAction;
                    fullMaskNew0 := '1';
                --end if;
                
                if fullMask(0) = '1' and fullMask(1) = '1' then -- If buffer full but putting more
                    report "DispatchBuffer overflow!" severity failure;
                end if;
            end if;
            
            fullMask(0) <= fullMaskNew0;
            
            if execEventSignal = '1' or lateEventSignal = '1' then
                fullMask <= (others => '0');
            end if;
   
           if CLEAR_DEBUG_INFO then
                for i in 0 to PIPE_WIDTH-1 loop
                   queueData0(i).ins.ip <= (others => '0');
                   queueData0(i).ins.bits <= (others => '0');
                   queueData0(i).ins.result <= (others => '0');
                   queueData0(i).ins.target <= (others => '0');
                    
                     queueData0(i).ins.tags.fetchCtr <= (others => '0');
                     queueData0(i).ins.tags.decodeCtr <= (others => '0');
                     queueData0(i).ins.tags.renameCtr <= (others => '0');
                     if i > 0 then -- As in RegisterManager
                        queueData0(i).ins.tags.renameIndex <= (others => '0');
                     end if;
                     
                     --   queueData0(i).ins.tags.intPointer <= (others => '0');
                     --   queueData0(i).ins.tags.floatPointer <= (others => '0');

                     queueData0(i).ins.tags.commitCtr <= (others => '0');
                     
                     -----
                   queueData1(i).ins.ip <= (others => '0');
                     queueData1(i).ins.bits <= (others => '0');
                     queueData1(i).ins.result <= (others => '0');
                     queueData1(i).ins.target <= (others => '0');
                      
                       queueData1(i).ins.tags.fetchCtr <= (others => '0');
                       queueData1(i).ins.tags.decodeCtr <= (others => '0');
                       queueData1(i).ins.tags.renameCtr <= (others => '0');
                       if i > 0 then -- As in RegisterManager
                          queueData1(i).ins.tags.renameIndex <= (others => '0');
                       end if;
                       
                       --   queueData1(i).ins.tags.intPointer <= (others => '0');
                       --   queueData1(i).ins.tags.floatPointer <= (others => '0');
  
                       queueData1(i).ins.tags.commitCtr <= (others => '0');
                      ------ 

                           special0.ins.classInfo <= DEFAULT_CLASS_INFO;
                           
                           special0.ins.ip <= (others => '0');
                           special0.ins.bits <= (others => '0');       
                           special0.ins.target <= (others => '0');
                           special0.ins.result <= (others => '0');
                           
                           special0.ins.tags <= DEFAULT_INSTRUCTION_TAGS;
                           special0.ins.constantArgs <= DEFAULT_CONSTANT_ARGS;
                           special0.ins.virtualArgSpec <= DEFAULT_ARG_SPEC;
                           special0.ins.physicalArgSpec <= DEFAULT_ARG_SPEC;


                           special1.ins.classInfo <= DEFAULT_CLASS_INFO;
                           
                           special1.ins.ip <= (others => '0');
                           special1.ins.bits <= (others => '0');       
                           special1.ins.target <= (others => '0');
                           special1.ins.result <= (others => '0');
                           
                           special1.ins.tags <= DEFAULT_INSTRUCTION_TAGS;
                           special1.ins.constantArgs <= DEFAULT_CONSTANT_ARGS;
                           special1.ins.virtualArgSpec <= DEFAULT_ARG_SPEC;
                           special1.ins.physicalArgSpec <= DEFAULT_ARG_SPEC;

                                            
                     if IS_FP then
                        queueData0(i).ins.constantArgs <= DEFAULT_CONSTANT_ARGS;
                        queueData0(i).ins.specificOperation <= DEFAULT_SPECIFIC_OP;
                        queueData0(i).ins.virtualArgSpec <= DEFAULT_ARG_SPEC;
                        
                        queueData0(i).ins.tags <= DEFAULT_INSTRUCTION_TAGS;
                        
                        queueData0(i).ins.controlInfo <= DEFAULT_CONTROL_INFO;
                        queueData0(i).ins.classInfo <= DEFAULT_CLASS_INFO;
                        
                        queueData0(i).ins.physicalArgSpec.intDestSel <= '0';
                        queueData0(i).ins.physicalArgSpec.floatDestSel <= '0';
                        queueData0(i).ins.physicalArgSpec.intArgSel <= (others => '0');
                        queueData0(i).ins.physicalArgSpec.floatArgSel <= (others => '0');
                        
                        special0 <= DEFAULT_INSTRUCTION_SLOT;                       
                        
                        --
                        queueData1(i).ins.constantArgs <= DEFAULT_CONSTANT_ARGS;
                        queueData1(i).ins.specificOperation <= DEFAULT_SPECIFIC_OP;
                        queueData1(i).ins.virtualArgSpec <= DEFAULT_ARG_SPEC;
                        
                        queueData1(i).ins.tags <= DEFAULT_INSTRUCTION_TAGS;
                        
                        queueData1(i).ins.controlInfo <= DEFAULT_CONTROL_INFO;
                        queueData1(i).ins.classInfo <= DEFAULT_CLASS_INFO;
                        
                        queueData1(i).ins.physicalArgSpec.intDestSel <= '0';
                        queueData1(i).ins.physicalArgSpec.floatDestSel <= '0';
                        queueData1(i).ins.physicalArgSpec.intArgSel <= (others => '0');
                        queueData1(i).ins.physicalArgSpec.floatArgSel <= (others => '0');
                        
                        special1 <= DEFAULT_INSTRUCTION_SLOT;                                                                       
                     end if;          
                end loop;
            end if;    

        end if;
    end process;
    
    sending <= isSending;
    dataOut <= restoreRenameIndex(queueData0);
    specialOut <= special0;

    accepting <= '1';--not fullMask(0); -- Don't allow more if anything needed to be buffered!
    empty <= not fullMask(0); -- CAREFUL: same as accepting but accepting refers to stage BEFORE Rename while empty is needed by flow FROM Rename
end Behavioral;
