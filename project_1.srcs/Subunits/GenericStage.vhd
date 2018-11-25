----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    02:24:40 05/07/2016 
-- Design Name: 
-- Module Name:    GenericStageMulti - Behavioral 
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

use work.BasicTypes.all;
use work.Helpers.all;

use work.InstructionState.all;
use work.PipelineGeneral.all;


entity GenericStage is
	generic(
		USE_CLEAR: std_logic := '1';
		COMPARE_TAG: std_logic := '0';
		WIDTH: natural := 1
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSending: in std_logic;
		nextAccepting: in std_logic;

		stageDataIn: in InstructionSlotArray(0 to WIDTH-1);
		
		acceptingOut: out std_logic;
		sendingOut: out std_logic;
		stageDataOut: out InstructionSlotArray(0 to WIDTH-1);

		execEventSignal: in std_logic;
		lateEventSignal: in std_logic;
		execCausing: in InstructionState
	);
end GenericStage;



architecture Behavioral of GenericStage is
	--signal flowDrive: FlowDriveSimple := (others=>'0');
	--signal flowResponse: FlowResponseSimple := (others=>'0');		
	signal before, full, kill, living, sending: std_logic := '0';
	signal stageData, stageDataNext: InstructionSlotArray(0 to WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
begin
	stageDataNext <= stageArrayNext(stageData, stageDataIn,
								living, sending, prevSending,
								kill and USE_CLEAR);

	PIPE_CLOCKED: process(clk) 
	begin
		if rising_edge(clk) then
		  stageData <= stageDataNext;
		  
		  if ((living and not sending) or prevSending) = '1' then
		      full <= '1';--(living and not sending) or prevSending;
		  else
		      full <= '0';
		  end if;
		end if;
	end process;

--	SIMPLE_SLOT_LOGIC: entity work.SimpleSlotLogic port map(
--		clk => clk, reset => reset, en => en,
--		flowDrive => flowDrive,
--		flowResponse => flowResponse
--	);

	--flowDrive.prevSending <= prevSending;
	--flowDrive.nextAccepting <= nextAccepting;

    living <= full and not kill;
    sending <= living and nextAccepting;

	before <= (not COMPARE_TAG) or CMP_tagBefore(execCausing.tags.renameIndex, stageData(0).ins.tags.renameIndex);
	kill <= (before and execEventSignal) or lateEventSignal;
	
	--flowDrive.kill <= kill;

	acceptingOut <= --flowResponse.accepting;
	                not living or sending;	
	sendingOut <= --flowResponse.sending;
	               sending;
	stageDataOut <= stageData;	
end Behavioral;


architecture Bypassed of GenericStage is
begin
	acceptingOut <= nextAccepting;		
	sendingOut <= prevSending;
	stageDataOut <= stageDataIn;
end Bypassed;
