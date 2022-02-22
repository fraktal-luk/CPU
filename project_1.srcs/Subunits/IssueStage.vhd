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
	signal full, sendingOut: std_logic := '0';
	signal argState, argStateNext: SchedulerState := DEFAULT_SCHEDULER_STATE;
begin
    inputData_TMP <= TMP_prepareDispatchSlot(input, prevSending);
	inputDataWithArgs <= getDispatchArgValues(inputData_TMP, fni, prevSending, USE_IMM, REGS_ONLY, TMP_DELAY);-- when not TMP_DELAY
	
	SAVE_SCH_STATE: process(clk)
	begin
		if rising_edge(clk) then
		    if nextAccepting = '1' then -- CAREFUL: this is to enable stalling 
			    argState <= inputDataWithArgs.state;

			    full <= prevSending;
			end if;
			
			if events.lateEvent = '1' then
			    full <= '0';
			end if;
		end if;
	end process;

    output.full <= full and nextAccepting;
    output.state <= updateDispatchArgs(argState, fni.values0, regValues, TMP_DELAY, REGS_ONLY);

end Alternative;
