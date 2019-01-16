----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:48:52 08/07/2016 
-- Design Name: 
-- Module Name:    ReorderBuffer - Behavioral 
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

use work.LogicROB.all;



entity ReorderBuffer is
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		lateEventSignal: in std_logic;
		--execEventSignal: in std_logic;
		--execCausing: in InstructionState; -- Redundant cause we have inputs from all Exec ends? 
		
		commitGroupCtr: in InsTag;
		--commitGroupCtrNext: in InsTag;

		execEndSigs1: in InstructionSlotArray(0 to 3);
		execEndSigs2: in InstructionSlotArray(0 to 3);
		
		inputData: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		prevSending: in std_logic;
		acceptingOut: out std_logic;
		acceptingMore: out std_logic;
		
		nextAccepting: in std_logic;
		sendingOut: out std_logic; 
		
		outputData: out InstructionSlotArray(0 to PIPE_WIDTH-1)
	);	
end ReorderBuffer;



architecture Behavioral of ReorderBuffer is
	signal fullMask: --, TMP_mask, TMP_ckEnForInput, TMP_sendingMask, TMP_killMask, TMP_livingMask, TMP_maskNext:
				std_logic_vector(0 to ROB_SIZE-1) := (others => '0');

    signal content, contentNext: ReorderBufferArray := DEFAULT_ROB_ARRAY;

	signal isSending: std_logic := '0';
	signal execEvent: std_logic := '0'; -- depends on input in slot referring to branch ops
		
	--signal numKilled: SmallNumber := (others => '0');

	--	signal execEnds: InstructionStateArray(0 to 3) := (others => DEFAULT_INSTRUCTION_STATE);
	--	signal execReady: std_logic_vector(0 to 3) := (others => '0');
	--	signal execEnds2: InstructionStateArray(0 to 3) := (others => DEFAULT_INSTRUCTION_STATE);
	--	signal execReady2: std_logic_vector(0 to 3) := (others => '0');
	
            constant ROB_HAS_RESET: std_logic := '0';
            constant ROB_HAS_EN: std_logic := '0';

	constant PTR_MASK_TAG: InsTag := i2slv(ROB_SIZE-1, TAG_SIZE);
	constant PTR_MASK_SN: SmallNumber := i2slv(ROB_SIZE-1, SMALL_NUMBER_SIZE);
	
	function getNextRobContent(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           execInfo1, execInfo2: InstructionSlotArray;
	                           sends, receiving, execEvent, lateEventSignal: std_logic;
	                           startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable killMask: std_logic_vector(0 to ROB_SIZE-1) := (others => '0');
	   variable ptr1, ptr2,  iv, tagHiPart, tagLoPart: SmallNumber := (others => '0');
	begin
	   -- if late event: clear all 'full' bits, endPtr = startPtr
	   -- elseif execEvent: endPtr = causingPtr, clear 'full' bits that are "behind" causingPtr
	
	   if sends = '1' then
	       res(slv2u(startPtr)).full := '0'; -- CAREFUL: don't get index out of bounds
	   end if;
	   
	   if receiving = '1' then
	       res(slv2u(endPtr)).full := '1'; -- CAREFUL: don't get index out of bounds
	       res(slv2u(endPtr)).ops := newGroup;
	   end if;
	
	   killMask := getMaskBetween(ROB_SIZE, causingPtr, endPtr, '0'); -- This has '1' also at 'equal' position!
	   killMask(slv2u(causingPtr)) := '0'; -- CAREFUL: don't get index out of bounds
	   
	   for i in 0 to ROB_SIZE-1 loop
	       iv := i2slv(i, SMALL_NUMBER_SIZE);
	       
	       -- Update group!
	       -- NOTE: tag comparison for slot will be simplified because tag bits directly show ROB slot:
	       --          [ upper bits | ROB slot | index in group]
	       --            (getTagHighSN([tag]) and PTR_MASK) == iv ?
	       --             getTagLowSN == [group index] ? // this is within updateOpGroup  
	       for j in 0 to 3 loop
	           ptr1 := getTagHighSN(execInfo1(j).ins.tags.renameIndex) and PTR_MASK_SN;
               ptr2 := getTagHighSN(execInfo2(j).ins.tags.renameIndex) and PTR_MASK_SN;
	           if ptr1 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo1(j), 0);
	           end if;
	           if ptr2 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo2(j), 1);
	           end if;
	       end loop;
	       
	       if ((execEvent and killMask(i)) or lateEventSignal) = '1' then
	           res(i).full := '0';
	       end if;
	   end loop;
	   
	   return res;
	end function;
	
	signal startPtr, endPtr, acmPtr, 
	       causingPtr: SmallNumber := (others => '0');
	
begin
	execEvent <= execEndSigs2(0).full and execEndSigs2(0).ins.controlInfo.newEvent;
	causingPtr <= getTagHighSN(execEndSigs2(0).ins.tags.renameIndex) and PTR_MASK_SN; -- TEMP!
	
	contentNext <= getNextRobContent(content, inputData,
	                                 execEndSigs1, execEndSigs2,
	                                 isSending, prevSending,
	                                 execEvent, lateEventSignal,
	                                 startPtr, endPtr, causingPtr);
							
	SYNCHRONOUS: process (clk)
	begin
		if rising_edge(clk) then	
            content <= contentNext;
            
            if isSending = '1' then
                startPtr <= addSN(startPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;
            
            if lateEventSignal = '1' then
                endPtr <= startPtr;
            elsif execEvent = '1' then
                endPtr <= addSN(causingPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            elsif prevSending = '1' then
                endPtr <= addSN(endPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;
		end if;		
	end process;
	
	isSending <= groupCompleted(content(slv2u(startPtr)).ops) and content(slv2u(startPtr)).full and nextAccepting;

	acceptingOut <= not content(slv2u(endPtr)).full; -- When a free place exists
    acmPtr <= subSN(endPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
    acceptingMore <= not content(slv2u(acmPtr)).full;
								
	outputData <= content(slv2u(startPtr)).ops;

	sendingOut <= isSending;
end Behavioral;
