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
	        DELAY_ONLY: boolean := false);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		prevSending: in std_logic;
	 	nextAccepting: in std_logic;

		input: in SchedulerEntrySlot;
		
		acceptingOut: out std_logic;
		output: out SchedulerEntrySlot;
		
		execEventSignal: in std_logic;
		lateEventSignal: in std_logic;
		execCausing: in InstructionState;
		
			fni: in ForwardingInfo;
		
		--resultTags: in PhysNameArray(0 to N_RES_TAGS-1);
		--resultVals: in MwordArray(0 to N_RES_TAGS-1);
		regValues: in MwordArray(0 to 2)		
	);
end IssueStage;


architecture Alternative of IssueStage is
	signal inputDataWithArgs, dispatchDataUpdated, inputDataWithArgs_T, dispatchDataUpdated_T:
						SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	signal lockSend: std_logic := '0';
	
	signal sendingOut: std_logic := '0';
	signal stageDataSaved, stageDataIn: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	
    
	signal argState: SchedulerState := DEFAULT_SCHEDULER_STATE;
	--	signal ch0: std_logic := '0';
		
begin

	inputDataWithArgs <= getDispatchArgValues(input.ins, input.state, fni,-- resultTags, resultVals,
														prevSending, USE_IMM,
														REGS_ONLY, DELAY_ONLY);
	
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
		
		execEventSignal => execEventSignal,
		lateEventSignal => lateEventSignal,
		execCausing => execCausing
	);

		
	SAVE_SCH_STATE: process(clk)
	begin
		if rising_edge(clk) then
		    if nextAccepting = '1' then -- CAREFUL: this is to enable stalling 
			    argState <= inputDataWithArgs.state;
			end if; 
		end if;
	end process;

	dispatchDataUpdated <= updateDispatchArgs(stageDataSaved.ins, argState,
															--resultVals(0 to 2),--N_NEXT_RES_TAGS-1),
															fni.values0,
															regValues);

	-- CAREFUL: this does nothing. To make it work:
	--											nextAcceptingEffective <= nextAccepting and not lockSend
	--lockSend <= BLOCK_ISSUE_WHEN_MISSING and isNonzero(dispatchDataUpdated.state.missing);
	output <= (sendingOut, dispatchDataUpdated.ins, dispatchDataUpdated.state);	
end Alternative;


