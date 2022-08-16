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
		FORWARDING: ForwardingModeArray := (0 => (-100, false));  -- Can be used immediately
		FORWARDING1: ForwardingModeArray := (0 => (-100, false));
		FORWARDING_D: ForwardingModeArray := (0 => (-100, false)); -- Can be used with 1 cycle delay
		IGNORE_MEM_FAIL: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSendingOK: in std_logic;
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);

		nextAccepting: in std_logic;

		events: in EventState;
		
		fni: in ForwardingInfo;
		readyRegFlags: in std_logic_vector(0 to 3*PIPE_WIDTH-1);

        memFail: in std_logic;
        memDepFail: in std_logic; -- UNUSED (inside functions)

		acceptingMore: out std_logic;
		acceptingOut: out std_logic;
		
		schedulerOut: out SchedulerEntrySlot;
        outputSignals: out IssueQueueSignals;
        
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
    
    signal nFull, nFullNext, nIn, nOut, recoveryCounter: SmallNumber := (others => '0');

    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);

    signal ageMatrix, ageMatrixNext: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
    signal insertionLocs: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal fma: ForwardingMatchesArray(0 to QUEUE_SIZE_EXT - 1) := (others => DEFAULT_FORWARDING_MATCHES);

    signal controlSigs: SlotControlArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SLOT_CONTROL);
    signal fullMask, trialMask, readyMaskLive, killMask, readyMaskAll, selMask, selMask1, selMask2, selMask3, selMask4,
            depMemE1_0, depMemE1_1, depAluRR_0, depAluRR_1
        : std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
    signal anyReadyFull, anyReadyLive, sends, sendingKilled, isSent, isSent2, sentKilled, sentKilled1, sentKilled2, sentKilled3, sentKilled4, isEmpty, isFull, isAlmostFull: std_logic := '0';
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    signal dispatchDataNew: SchedulerState := DEFAULT_SCHED_STATE;       

    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';

        function TMP_depMemE1(content: SchedulerInfoArray; arg: natural) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable st: ArgumentState := DEFAULT_ARGUMENT_STATE;
        begin
            for i in res'range loop
                st := content(i).dynamic.argStates(arg);
                if content(i).dynamic.full = '1' and st.used = '1' and st.srcPipe(1 downto 0) = "10" and st.srcStage(1 downto 0) = "11" then
                    res(i) := '1';
                end if;
            end loop;
            return res;
        end function;

        function TMP_depAluRR(content: SchedulerInfoArray; arg: natural) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable st: ArgumentState := DEFAULT_ARGUMENT_STATE;
        begin
            for i in res'range loop
                st := content(i).dynamic.argStates(arg);
                if content(i).dynamic.full = '1' and st.used = '1' and st.srcPipe(1 downto 0) = "00" and st.srcStage(1 downto 0) = "00" then
                    res(i) := '1';
                end if;
            end loop;
            return res;
        end function;
begin
    nFullNext <=        sub(i2slv(countOnes(fullMask), SMALL_NUMBER_SIZE), nOut) when slv2u(recoveryCounter) = 1
                  else  add(nFull, sub(nIn, nOut));

    nIn <= i2slv(countOnes(extractFullMask(newArr)), SMALL_NUMBER_SIZE) when prevSendingOK = '1' else (others => '0'); 
    nOut <= i2slv(1, SMALL_NUMBER_SIZE) when (isSent and not sentKilled) = '1' else (others => '0');

    fma <= findForwardingMatchesArray(queueContent, fni, "000");

    queueContentUpdated <= updateSchedulerArray(queueContent, fni, fma, false, false, DONT_MATCH1, FORWARDING_D, memFail, IGNORE_MEM_FAIL);
    queueContentUpdatedSel <= updateSchedulerArray(queueContent, fni, fma, false, true, DONT_MATCH1, FORWARDING, memFail, IGNORE_MEM_FAIL);

    insertionLocs <= getNewLocsBanked(fullMask);

    queueContentNext <= iqNext_NS(queueContentUpdated, newArr, prevSendingOK, sends, killMask, trialMask, selMask, readyRegFlags, insertionLocs, memFail);
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

    COUNTERS_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then               
            nFull <= nFullNext;
                
            if events.lateEvent = '1' or events.execEvent = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
            recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here    
       
            isFull <= cmpGtU(nFullNext, QUEUE_SIZE_EXT - PIPE_WIDTH);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE_EXT - 2*PIPE_WIDTH);
        end if;
    end process;

    dispatchDataNew <= getSchedEntrySlot(selectedSlot, sends);

    schedulerOut <= dispatchDataNew;
    acceptingOut <= acceptingBanked(bankCounts, BANK_SIZE);
    acceptingMore <= acceptingMoreBanked(bankCounts, BANK_SIZE);

    outputSignals <=   (sending => sends,
                        cancelled => sentKilled or memFail,
                        ready => anyReadyLive,
                        empty => isEmpty,
                        killSel => sendingKilled,
                        killSel1 => sentKilled1,
                        killSel2 => sentKilled2,
                        killSel3 => sentKilled3
                        );

    -- pragma synthesis off
    DEBUG_HANDLING: process (clk)
        use std.textio.all;
        use work.Assembler.all;

--        function getDynamicContentString(dyn: DynamicOpInfo) return string is
--            variable res: line;
--        begin
--            if dyn.full = '1' then
--                write(res, natural'image(slv2u(dyn.dbInfo.tag)));
--                write(res, string'(": "));
--                write(res, std_logic'image(dyn.completed0));
--                if dyn.secCluster = '1' then
--                    write(res, std_logic'image(dyn.completed1));
--                else
--                    write(res, string'("'-'"));
--                end if;
--                write(res, string'(" "));
--                if dyn.hasEvent = '1' then
--                    write(res, string'("E : "));
--                else
--                    write(res, string'("  : "));
--                end if;
                
--                write(res, disasmWord(dyn.dbInfo.bits));
                
--                return res.all;
--            else
--                return "-------------------------------------";
--            end if;
--        end function;
    
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
                write(currentLine, std_logic'image(queueContent(i).dynamic.argStates(0).waiting));
                write(currentLine, std_logic'image(queueContent(i).dynamic.argStates(1).waiting));
                
                writeline(outFile, currentLine);
            end loop;
        end procedure;
        
    begin
        
        if rising_edge(clk) then
            if dbState.dbSignal = '1' then
                report "IQ reporting ";
                --printContent;
            end if;
        end if;
    end process;
    -- pragma synthesis on

end Behavioral;
