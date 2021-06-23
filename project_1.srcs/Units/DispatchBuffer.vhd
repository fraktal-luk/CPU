
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
    signal queueData0: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
    signal special0: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    signal fullMask: std_logic_vector(0 to 0) := (others => '0');
    signal isSending: std_logic := '0';
    
    -- TODO: remove IS_FP (UNUSED) unless turns out useful
    function TMP_clearSlot(isl: InstructionSlot; i: integer; fp: boolean) return InstructionSlot is
        variable res: InstructionSlot := isl;
    begin 
         res := clearAbstractInfo(res);
         res := clearDbCounters(res);   

         if i > 0 then -- As in RegisterManager
            res.ins.tags.renameIndex := (others => '0');
         end if;
                                 
         if fp then            
            res.ins := DEFAULT_INS_STATE;
            res.ins.physicalArgSpec.dest := isl.ins.physicalArgSpec.dest;
            res.ins.physicalArgSpec.args := isl.ins.physicalArgSpec.args; 
         end if;
   
        return res;
    end function;
    
    function TMP_clearSpecialSlot(isl: InstructionSlot) return InstructionSlot is
        variable res: InstructionSlot := DEFAULT_INS_SLOT;
    begin
        res.full := isl.full;
        
        if not IS_FP then
            res.ins.controlInfo := isl.ins.controlInfo;
            res.ins.specificOperation := isl.ins.specificOperation; -- TODO: leave only Special part?
        end if;
         
        return res;
    end function;
      
begin
    
    isSending <= nextAccepting and fullMask(0);-- and not (execEventSignal or lateEventSignal);
    
    SYNCH: process (clk)
        variable fullMaskNew0: std_logic := '0';
        variable queueDataTemp0: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
        variable specialTemp0: InstructionSlot := DEFAULT_INS_SLOT;
    begin
        if rising_edge(clk) then
            fullMaskNew0 := fullMask(0);        
            queueDataTemp0 := queueData0;
            specialTemp0 := special0;
            
            if isSending = '1' then
                fullMaskNew0 := '0';
            end if;
            
            if prevSending = '1' then
                queueDataTemp0 := dataIn;
                specialTemp0 := specialAction;
                fullMaskNew0 := '1';
            end if;
            
            fullMask(0) <= fullMaskNew0;
            
            if execEventSignal = '1' or lateEventSignal = '1' then
                fullMask <= (others => '0');
            end if;
   
           if CLEAR_DEBUG_INFO then
                for i in 0 to PIPE_WIDTH-1 loop                    
                    queueDataTemp0(i) := TMP_clearSlot(queueDataTemp0(i), i, IS_FP);                           
                end loop;
                
                specialTemp0 := TMP_clearSpecialSlot(specialTemp0);
            end if;    

            queueData0 <= queueDataTemp0;
            special0 <= specialTemp0;               
        end if;
    end process;
    
    sending <= isSending;
    dataOut <= restoreRenameIndex(queueData0);
    specialOut <= special0;

    accepting <= '1';--not fullMask(0); -- Don't allow more if anything needed to be buffered!
    empty <= not fullMask(0); -- CAREFUL: same as accepting but accepting refers to stage BEFORE Rename while empty is needed by flow FROM Rename
end Behavioral;


architecture Transparent of DispatchBuffer is
begin
    sending <= prevSending;
    dataOut <= dataIn;
    specialOut <= specialAction;
    
    accepting <= '1'; 
    empty <= '1';
end Transparent;
