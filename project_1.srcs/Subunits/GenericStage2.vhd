----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.InstructionState.all;
use work.PipelineGeneral.all;


entity GenericStage2 is
	generic(
		USE_CLEAR: std_logic := '1';
		COMPARE_TAG: std_logic := '0';
		KEEP_DEST: std_logic := '0';
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

            events: in EventState

--		execEventSignal: in std_logic;
--		lateEventSignal: in std_logic;
--		execCausing: in InstructionState
	);
end GenericStage2;


architecture Behavioral of GenericStage2 is
	signal before, full, kill, living, sending: std_logic := '0';
	signal stageData, stageDataNext: InstructionSlotArray(0 to WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
begin
	stageDataNext <= stageArrayNext(stageData, stageDataIn,
								    living, sending, prevSending,
								    kill and USE_CLEAR, KEEP_DEST);

	PIPE_CLOCKED: process(clk) 
	begin
		if rising_edge(clk) then
		  stageData <= stageDataNext;
		  
		  -- CAREFUL: This is important because signal combinations can be different from {'0', '1'} on initialisation.
		  --          Simple assigment: full <= ((living and not sending) or prevSending); fails if we don't have a binary value.
		  --          TODO? Can it be guaranteed that there is also a binary value form the beginning?
		  if ((living and not sending) or prevSending) = '1' then
		      full <= '1';
		  else
		      full <= '0';
		  end if;
		end if;
	end process;

    living <= full and not kill;
    sending <= living and nextAccepting;

	before <= (not COMPARE_TAG) or compareTagBefore(events.execCausing.tags.renameIndex, stageData(0).ins.tags.renameIndex);
	kill <= (before and events.execEvent) or events.lateEvent;

	acceptingOut <= nextAccepting or not living;	
	sendingOut <= sending;
	stageDataOut <= stageData;	
end Behavioral;


architecture Bypassed of GenericStage2 is
begin
	acceptingOut <= nextAccepting;		
	sendingOut <= prevSending;
	stageDataOut <= stageDataIn;
end Bypassed;
