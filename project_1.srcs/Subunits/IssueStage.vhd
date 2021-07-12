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
	        TMP_DELAY: boolean := false;
	        NEW_RR: boolean := false);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		prevSending: in std_logic;
	 	nextAccepting: in std_logic;

		input: in SchedulerEntrySlot;
		
		acceptingOut: out std_logic;
		output: out SchedulerEntrySlot;

		events: in EventState;
		
		fni: in ForwardingInfo;
		regValues: in MwordArray(0 to 2)		
	);
end IssueStage;


architecture Alternative of IssueStage is
	signal inputDataWithArgs, inputData_TMP, dispatchDataUpdated: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	signal sendingOut: std_logic := '0';
	signal stageDataSaved, stageDataIn: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	
	signal argState, argStateNext: SchedulerState := DEFAULT_SCHEDULER_STATE;
	
	--   signal TMP_argReads0, TMP_argReads1: IntArray(0 to 2);		
begin
    inputData_TMP <= TMP_prepareDispatchSlot(input, prevSending);
	inputDataWithArgs <= getDispatchArgValues(inputData_TMP, fni, prevSending, USE_IMM, REGS_ONLY);-- when not TMP_DELAY 

    argStateNext <= inputData_TMP.state when TMP_DELAY
               else inputDataWithArgs.state; 
    
	stageDataIn <= (prevSending, inputData_TMP.ins);

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
		
		execEventSignal => events.execEvent,
		lateEventSignal => events.lateEvent,
		execCausing => events.execCausing
	);
	
	SAVE_SCH_STATE: process(clk)
	begin
		if rising_edge(clk) then
		    if nextAccepting = '1' then -- CAREFUL: this is to enable stalling 
			    argState <= argStateNext;
			end if; 
		end if;
	end process;

    dispatchDataUpdated.state <= updateDispatchArgs(argState, fni.values0, regValues, TMP_DELAY, REGS_ONLY);

	output <= (sendingOut, stageDataSaved.ins, dispatchDataUpdated.state);	
end Alternative;


