----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:12:19 04/24/2016 
-- Design Name: 
-- Module Name:    SubunitHbuffer - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;

use work.CoreConfig.all;

use work.PipelineGeneral.all;

use work.LogicFront.all;


entity InstructionBuffer is
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSending: in std_logic;
		nextAccepting: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;

		stageDataIn: in InstructionSlotArray(0 to FETCH_WIDTH-1);
		acceptingOut: out std_logic;
		sendingOut: out std_logic;
		stageDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1)
	);
end InstructionBuffer;



architecture Implem of InstructionBuffer is
	
	signal queueData, queueDataNext: InstructionSlotArray(0 to IBUFFER_SIZE-1)
								:= (others => DEFAULT_INSTRUCTION_SLOT);

    function bufferDataNext(content: InstructionSlotArray; newContent: InstructionSlotArray;
                            nextAccepting, prevSending, kill: std_logic)
    return InstructionSlotArray is
        constant LEN: positive := content'length;
        variable res: InstructionSlotArray(0 to LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        variable fullMask, fillMask, remainingMask, nextMask: std_logic_vector(0 to LEN-1) := (others => '0');
        variable remainingMaskExt: std_logic_vector(0 to LEN + 3) := (others => '0');
        variable inputMask, inputMaskComp: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
        --variable sel: std_logic_vector(1 downto 0) := "00";
        --variable remainingMaskM1, remainingMaskM2, remainingMaskM3, remainingMaskM4,
        --            rM3, rM2, rM1: std_logic := '0';
        --variable remv: std_logic_vector(0 to 2) := "000"; 
    begin
        fullMask := extractFullMask(content);
        inputMask := extractFullMask(newContent);
        
        inputMaskComp := compactMask(inputMask);
        
        if nextAccepting = '1' then -- sending; shift mask by 4
            remainingMaskExt(0 to LEN - 1) := fullMask;
            remainingMaskExt(0 to 3) := (others => '1');
        else
            remainingMaskExt(4 to LEN + 3) := fullMask;
            remainingMaskExt(0 to 3) := (others => '1');
        end if;
        
        for i in 0 to LEN-1 loop
              
            --remv := remainingMaskExt(i+1 to i+3);
            --sel := getSelector(remv, inputMask(0 to 2));
                -- elemNew := getNewElem(newContent, inputMask(0 to 2), remainingMaskExt(i+1 to i+3));
            
            if remainingMaskExt(i + 4) = '1' then  -- !! equivalent to remainingMask(i), where '1' for i < 0    
                if nextAccepting = '1' and i + 4 < LEN then
                    res(i).ins := content(i+4).ins;
                else
                    res(i).ins := content(i).ins;
                end if;
            else
                --res(i).ins := newContent(slv2u(sel)).ins;
                    res(i) := getNewElem(remainingMaskExt(i+1 to i+3), newContent);
                        res(i).ins.controlInfo.skipped := '0'; -- By definition skipped words don't go to this buffer
            end if;
            
            -- No events before decoding; newEvent flag set for branches must be cleared.
            --  Meanwhile, branch taken/not taken state must be retained
            res(i).ins.controlInfo.newEvent := '0';
            
            fillMask(i) := '0';
            for k in 0 to 3 loop -- Further beyond end requires more ful inputs to be filled:
                --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
                fillMask(i) := fillMask(i) or (remainingMaskExt(i + 3-k) and inputMaskComp(k));
            end loop;
            
            nextMask(i) := (remainingMaskExt(i + 4) or (fillMask(i) and prevSending)) and not kill;
            
            res(i).full := nextMask(i);
        end loop;
        
        return res;
    end function;

begin
    
    queueDataNext <= bufferDataNext(queueData, stageDataIn, nextAccepting, prevSending, execEventSignal);
    
    acceptingOut <= not queueData(IBUFFER_SIZE-4).full;
    
    sendingOut <= nextAccepting and queueData(0).full and not execEventSignal; -- Send if nonempty & not killed
    stageDataOut <= queueData(0 to 3);
    
	BUFF_CLOCKED: process(clk)
	begin					
		if rising_edge(clk) then
            queueData <= queueDataNext;
		end if;
	end process;	

end Implem;
