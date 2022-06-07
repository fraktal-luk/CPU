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
		IQ_SIZE: natural := 12;
		--IS_FP: boolean := false;
		--ALT_INPUT: boolean := false;
		DONT_MATCH1: boolean := false;
		FORWARDING: ForwardingModeArray := (0 => (-100, false));  -- Can be used immediately
		FORWARDING1: ForwardingModeArray := (0 => (-100, false));
		FORWARDING_D: ForwardingModeArray := (0 => (-100, false)) -- Can be used with 1 cycle delay
		--NONSHIFT: boolean := true --
		                     -- false
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

		acceptingMore: out std_logic;
		acceptingOut: out std_logic;
		
		schedulerOut: out SchedulerEntrySlot;
        outputSignals: out IssueQueueSignals		
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
    signal fullMask, trialMask, readyMaskLive, killMask, readyMaskAll, selMask, selMask1, selMask2, selMask3, selMask4: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
    signal anyReadyFull, anyReadyLive, sends, sendingKilled, isSent, isSent2, sentKilled, sentKilled1, sentKilled2, sentKilled3, sentKilled4, isEmpty, isFull, isAlmostFull: std_logic := '0';
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    signal dispatchDataNew: SchedulerState := DEFAULT_SCHED_STATE;       
                              
    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';

begin
    nFullNext <=        sub(i2slv(countOnes(fullMask), SMALL_NUMBER_SIZE), nOut) when slv2u(recoveryCounter) = 1
                  else  add(nFull, sub(nIn, nOut));

    nIn <= i2slv(countOnes(extractFullMask(newArr)), SMALL_NUMBER_SIZE) when prevSendingOK = '1' else (others => '0'); 
    nOut <= i2slv(1, SMALL_NUMBER_SIZE) when (isSent and not sentKilled) = '1' else (others => '0');

    fma <= findForwardingMatchesArray(queueContent, fni, "000");

    queueContentUpdated <= updateSchedulerArray(queueContent, fni, fma, false, false, DONT_MATCH1, FORWARDING_D);
    queueContentUpdatedSel <= updateSchedulerArray(queueContent, fni, fma, false, true, DONT_MATCH1, FORWARDING);

    insertionLocs <= getNewLocsBanked(fullMask);

    queueContentNext <= iqNext_NS(queueContentUpdated, newArr, prevSendingOK, sends, killMask, trialMask, selMask, readyRegFlags, insertionLocs, 0);
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
                        cancelled => sentKilled,
                        ready => anyReadyLive,
                        empty => isEmpty,
                        killSel => sendingKilled,
                        killSel1 => sentKilled1,
                        killSel2 => sentKilled2,
                        killSel3 => sentKilled3
                        );

end Behavioral;
