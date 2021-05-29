----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;

use work.LogicIssue.all;


entity IssueStage is
	generic(USE_IMM: boolean := true;
	        REGS_ONLY: boolean := false;
	        DELAY_ONLY: boolean := false;
	        TMP_DELAY: boolean := false);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		prevSending: in std_logic;
	 	nextAccepting: in std_logic;

		input: in SchedulerEntrySlot;
		
		acceptingOut: out std_logic;
		output: out SchedulerEntrySlot;
		
		--execEventSignal: in std_logic;
		--lateEventSignal: in std_logic;
		--execCausing: in InstructionState;
		
		  events: in EventState;
		
		fni: in ForwardingInfo;
		regValues: in MwordArray(0 to 2)		
	);
end IssueStage;


architecture Alternative of IssueStage is
	signal inputDataWithArgs, dispatchDataUpdated: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	signal sendingOut: std_logic := '0';
	signal stageDataSaved, stageDataIn: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	
	signal argState: SchedulerState := DEFAULT_SCHEDULER_STATE;		
begin
	inputDataWithArgs <= getDispatchArgValues(input.ins, input.state, fni, prevSending, 
											  USE_IMM, REGS_ONLY, DELAY_ONLY, TMP_DELAY);	
	stageDataIn <= (prevSending, inputDataWithArgs.ins);
	
	BASIC_LOGIC: entity work.GenericStage(Behavioral)
	generic map(
		COMPARE_TAG => '1'
	)
	port map(
		clk => clk, reset => reset, en => en,
		
		prevSending => prevSending,
		nextAccepting => nextAccepting,
		
		stageDataIn(0) => stageDataIn,
		acceptingOut => acceptingOut,
		sendingOut => sendingOut,
		stageDataOut(0) => stageDataSaved,
		
		execEventSignal => --execEventSignal,
		                      events.execEvent,
		lateEventSignal => --lateEventSignal,
		                      events.lateEvent,
		execCausing => --execCausing
		                  events.execCausing
	);
	
	SAVE_SCH_STATE: process(clk)
	begin
		if rising_edge(clk) then
		    if nextAccepting = '1' then -- CAREFUL: this is to enable stalling 
			    argState <= inputDataWithArgs.state;
			end if; 
		end if;
	end process;

	dispatchDataUpdated <= updateDispatchArgs(stageDataSaved.ins, argState, fni.values0, regValues);

	output <= (sendingOut, dispatchDataUpdated.ins, dispatchDataUpdated.state);	
end Alternative;


