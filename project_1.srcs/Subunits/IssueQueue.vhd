----------------------------------------------------------------------------------

--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;

use work.LogicIssue.all;

use work.ForwardingNetwork.all;


entity IssueQueue is
	generic(
	    NAME: string;
		IQ_SIZE: natural := 12;
		DONT_MATCH1: boolean := false;
		FORWARDING: ForwardingModeArray(0 to 2) := (others => (-100, false));  -- Can be used immediately
		FORWARDING1: ForwardingModeArray(0 to 2) := (others => (-100, false));
		FORWARDING_D: ForwardingModeArray(0 to 2) := (others => (-100, false)); -- Can be used with 1 cycle delay
		IGNORE_MEM_FAIL: boolean := false;
		      WAKEUP_SPEC: WakeupSpec := DEFAULT_WAKEUP_SPEC;
		      USE_WAKEUP_MODES: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSendingOK: in std_logic;
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);
        TMP_newTags: in SmallNumberArray(0 to RENAME_W-1);

		nextAccepting: in std_logic;

		events: in EventState;

		fni: in ForwardingInfo;
		readyRegFlags: in std_logic_vector(0 to 3*PIPE_WIDTH-1);

		schedulerOut: out SchedulerEntrySlot;
        outputSignals: out IssueQueueSignals;

        freedMask: out std_logic_vector(0 to IQ_SIZE-1);
        usedMask: out std_logic_vector(0 to IQ_SIZE-1);

        dbState: in DbCoreState		
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
    constant QUEUE_SIZE_EXT: natural := IQ_SIZE;-- + PIPE_WIDTH;

    constant N_BANKS: natural := 4;
    constant BANK_SIZE: natural := QUEUE_SIZE_EXT / N_BANKS;

    signal bankCounts: SmallNumberArray(0 to 3) := (others => (others => '0'));
    -- For future development: selects bank for each input element. Becomes relevant when load balancing among banks is introduced
    signal TMP_inputDirs: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    
    signal recoveryCounter: SmallNumber := (others => '0');

    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdated_N, queueContentUpdated_2, queueContentUpdatedSel, queueContentUpdatedSel_NEW
            : SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);

    signal ageMatrix, ageMatrixNext: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
    signal insertionLocs: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal fma, fmaSelection: ForwardingMatchesArray(0 to QUEUE_SIZE_EXT - 1) := (others => DEFAULT_FORWARDING_MATCHES);

    signal controlSigs: SlotControlArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SLOT_CONTROL);
    signal fullMask, trialMask, readyMaskLive, killMask, readyMaskAll, selMask, selMask1, selMask2, selMask3, selMask4, depMemE1_0, depMemE1_1, depAluRR_0, depAluRR_1
        : std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
    signal anyReadyFull, anyReadyLive, sends, sendingKilled, maybeSent, maybeSent2, maybeSent3, maybeSent4,
                    isSent, isSent2, isSent3, isSent4, sentKilled, sentKilled1, sentKilled2, sentKilled3, sentKilled4, isEmpty: std_logic := '0';
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    signal selectedIqTag: SmallNumber := (others => '0');

    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';

    
        constant CFG_WAIT: work.LogicIssue.SchedulerUpdateConfig := (false, false, IGNORE_MEM_FAIL, FORWARDING_D, false);
        constant CFG_SEL: work.LogicIssue.SchedulerUpdateConfig :=  (false, false, IGNORE_MEM_FAIL, FORWARDING, false);

        constant CFG_WAIT_Alt: work.LogicIssue.SchedulerUpdateConfig := (false, false, IGNORE_MEM_FAIL, FORWARDING_D, true);


        signal wa: WakeupInfoArray(0 to IQ_SIZE-1);
        
        signal wups, wupsSelection: WakeupStructArray2D(0 to IQ_SIZE-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));
begin

    fma <= findForwardingMatchesArray(queueContent, fni, false, "000");
        TMP_WUP: if USE_WAKEUP_MODES generate
          wa <= getWakeupArray(queueContent, fni, WAKEUP_SPEC, CFG_WAIT); -- CFG_WAIT is needed for 'ignoreMemFail')
        end generate;

    wups <= fma2wups(fma, CFG_WAIT, false);
    wupsSelection <= fma2wups(fma, CFG_SEL, true);

    queueContentUpdatedSel <= updateSchedulerArray_S_NEW(queueContent, fni, wupsSelection, fni.memFail, CFG_SEL);


    queueContentUpdated <= updateSchedulerArray_N(queueContent, fni, wups, fni.memFail, CFG_WAIT);

    queueContentUpdated_2 <= iqNext_NS(queueContentUpdated, sends, killMask, trialMask, selMask, fni.memFail);

    insertionLocs <= getNewLocs_N(fullMask, TMP_newTags, newArr);
    queueContentNext <= iqNext_NS_2(queueContentUpdated_2, prepareNewArr(newArr, readyRegFlags), prevSendingOK, insertionLocs);

    ageMatrixNext <= updateAgeMatrix(ageMatrix, insertionLocs, fullMask);


    QUEUE_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then
            queueContent <= queueContentNext;
            ageMatrix <= ageMatrixNext;

            selMask1 <= selMask;
            selMask2 <= selMask1;
            selMask3 <= selMask2;
            selMask4 <= selMask3;

            sentKilled <= sendingKilled;
            sentKilled1 <= sendingKilled;
            sentKilled2 <= isNonzero(killMask and selMask1);
            sentKilled3 <= isNonzero(killMask and selMask2);
            sentKilled4 <= isNonzero(killMask and selMask3);                        

            isSent <= sends;
            isSent2 <= isSent;
            isSent3 <= isSent2;
            isSent4 <= isSent3;

            maybeSent <= anyReadyFull;
            maybeSent2 <= maybeSent;
            maybeSent3 <= maybeSent2;
            maybeSent4 <= maybeSent3;
        end if;
    end process;

    controlSigs <= getControlSignals(queueContentUpdatedSel, events);

    -- Vector signals
    killMask <= getKilledVec(controlSigs);
    trialMask <= getTrialVec(controlSigs);
    fullMask <= getFullVec(controlSigs);
    readyMaskAll <= getReadyVec(controlSigs);
    readyMaskLive <= getReadyLiveVec(controlSigs);

    -- Scalar signals
    isEmpty <= not isNonzero(fullMask);
    anyReadyLive <= isNonzero(readyMaskLive);
    anyReadyFull <= isNonzero(readyMaskAll);
    sends <= anyReadyFull and nextAccepting;
    sendingKilled <= isNonzero(killMask and selMask);

        bankCounts <= getBankCounts(fullMask);

    -- Selection for issue
    selMask <= getSelMask(readyMaskAll, ageMatrix);
    selectedSlot <= queueSelect(queueContentUpdatedSel, selMask);  
    selectedIqTag <= sn(getFirstOnePosition(selMask));

    schedulerOut <= getSchedEntrySlot(selectedSlot, sends, selectedIqTag);

    outputSignals <=   (sending => sends,
                        cancelled => sentKilled or fni.memFail,
                        ready => anyReadyLive,
                        empty => isEmpty,
                        killSel => sendingKilled,
                        killSel1 => sentKilled1,
                        killSel2 => sentKilled2,
                        killSel3 => sentKilled3
                        );

    freedMask <= getFreedVec(controlSigs);
    usedMask <= fullMask;

    COUNTERS_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then                
            if events.lateEvent = '1' or events.execEvent = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;

            recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here    
        end if;
    end process;


    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate
        process (clk)
            use std.textio.all;
            use work.Assembler.all;

            function getArgString(argState: ArgumentState) return string is
                variable immValue: Hword := argState.value;
            begin
                if argState.imm = '1' then
                    if IMM_AS_REG then
                        immValue(PhysName'length-2 downto 0) := argState.reg(6 downto 0);
                    end if;
                    return "{#" & integer'image(slv2u(immValue)) & "}";
                elsif argState.zero = '1' then
                    return "{zero}";
                else
                    if argState.waiting = '1' then
                        return "{" & natural'image(slv2u(argState.reg)) & " [0]}";
                    else
                        return "{" & natural'image(slv2u(argState.reg)) & " [1]}";                
                    end if;
                end if;

            end function;

            procedure printContent is
               file outFile: text open write_mode is "issue_queue" & NAME & ".txt";
               variable preRow, currentLine: line := null;
            begin
                for i in 0 to IQ_SIZE-1 loop
                    currentLine := null;
                    write(currentLine, natural'image(i) & ":  ");
                    if queueContent(i).dynamic.full /= '1' then
                        writeline(outFile, currentLine);
                        next;
                    end if;

                    write(currentLine, natural'image(slv2u(queueContent(i).dynamic.renameIndex)));
                    write(currentLine, string'(", "));
                    write(currentLine, std_logic'image(queueContent(i).dynamic.issued));
                    write(currentLine, string'(", "));

                    write(currentLine, getArgString(queueContent(i).dynamic.argStates(0)));
                    write(currentLine, string'(", "));
                    write(currentLine, getArgString(queueContent(i).dynamic.argStates(1)));

                    write(currentLine, string'(" // "));

                    write(currentLine, disasmWord(queueContent(i).static.dbInfo.bits));
                    writeline(outFile, currentLine);
                end loop;
            end procedure;

        begin

            if rising_edge(clk) then
                if DB_LOG_EVENTS then
                    if dbState.dbSignal = '1' then
                        report "IQ reporting ";
                        printContent;
                    end if;
                end if;
            end if;
        end process;

    end generate;
    -- pragma synthesis on

end Behavioral;
