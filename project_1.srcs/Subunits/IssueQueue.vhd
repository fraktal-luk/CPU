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
		FORWARDING: ForwardingModeArray(0 to 2) := (others => (-100, false));  -- Can be used immediately
		FORWARDING_D: ForwardingModeArray(0 to 2) := (others => (-100, false)); -- Can be used with 1 cycle delay
		IGNORE_MEM_FAIL: boolean := false;
		      WAKEUP_SPEC: WakeupSpec := DEFAULT_WAKEUP_SPEC;
		      WTF_MEM_FAIL: std_logic := '1'
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		prevSendingOK: in std_logic;
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);
        TMP_newTags: in SmallNumberArray(0 to RENAME_W-1);

        inReady: in std_logic;
        inMask: std_logic_vector;

        TMP_outTagsPre: out SmallNumberArray(0 to RENAME_W-1);
        TMP_outTags: out SmallNumberArray(0 to RENAME_W-1);

        accept: out std_logic;

		nextAccepting: in std_logic;
        unlockDiv: in std_logic;
            
		events: in EventState;

        bypass: in BypassState;

		schedulerOut: out SchedulerState;
        outputSignals: out IssueQueueSignals;

        dbState: in DbCoreState		
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
    constant N_BANKS: natural := PIPE_WIDTH;
    constant BANK_SIZE: natural := IQ_SIZE / N_BANKS;

    constant CFG_WAIT: SchedulerUpdateConfig := (false, false, IGNORE_MEM_FAIL, FORWARDING_D, false);
    constant CFG_SEL: SchedulerUpdateConfig :=  (false, false, IGNORE_MEM_FAIL, FORWARDING, false);

    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdated_2, queueContentUpdatedSel: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);


        signal queueSel2, queueSel4, queueSel8, queueSel16: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);


    signal ageMatrix: slv2D(0 to IQ_SIZE-1, 0 to IQ_SIZE-1) := (others => (others => '0'));
    signal insertionLocs: slv2D(0 to IQ_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal wups, wupsSelection: WakeupStructArray2D(0 to IQ_SIZE-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));  

    signal fullMask, killMask, freedMask, readyMask, selMask, selMask1, selMask2,   selMaskH: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');

    signal anyReadyFull, sends, sendingKilled, sentKilled, sentTrial1, sentTrial2: std_logic := '0';

    signal inTags: SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));

    signal recoveryCounter: SmallNumber := (others => '0');

    signal selectedSlot, selectedSlot_N: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    signal selectedIqTag: SmallNumber := (others => '0');

    alias memFail is bypass.memFail;
                
    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';

begin

    ALLOCATOR: entity work.QueueAllocator
    generic map(
        QUEUE_SIZE => 12, BANK_SIZE => 3
    )
    port map(
        clk => clk, evt => events,

        inReady => inReady,
        inMask => inMask,

        TMP_outTags => inTags,
        TMP_outTagsPre => TMP_outTagsPre,

        accept => accept,

        iqUsed => fullMask,
        iqFreed => freedMask
    );

    TMP_outTags <= inTags;

    wups <= getSlowWakeups(queueContent, bypass, CFG_WAIT);

    FAST_WAKEUP: if ENABLE_FAST_WAKEUP generate
        wupsSelection <= getFastWakeups(queueContent, bypass, CFG_SEL);
    end generate;

    queueContentUpdatedSel <= updateSchedulerArray_S(queueContent, wupsSelection, memFail, CFG_SEL);

    freedMask <= getFreedMask(queueContent);
    fullMask <= getFullMask(queueContent);

    QUEUE_CTRL: block
        signal sendingTrial: std_logic := '0';
        signal trialMask, trialUpdatedMask: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');
    begin
        trialMask <= getTrialMask(queueContent, events);
        trialUpdatedMask <= getTrialUpdatedMask(queueContent);

        killMask <= (others => '1') when events.lateEvent = '1' 
               else trialUpdatedMask when events.execEvent = '1'
               else (others => '0');

        queueContentUpdated <= updateSchedulerArray(queueContent, wups, memFail, CFG_WAIT);
        queueContentUpdated_2 <= updateQueueState(queueContentUpdated, sends,
                                                killMask, trialMask, selMask,
                                                memFail, unlockDiv);

        sendingTrial <= isNonzero(selMask and trialUpdatedMask);
        sendingKilled <= killFollower(sendingTrial, events);

        sentTrial1 <= isNonzero(selMask1 and trialUpdatedMask);
        sentTrial2 <= isNonzero(selMask2 and trialUpdatedMask);
    end block;

    insertionLocs <= getNewLocs(fullMask, inTags, newArr);
    queueContentNext <= storeInput(queueContentUpdated_2, newArr, prevSendingOK, insertionLocs);

    readyMask <= getReadyMask(queueContentUpdatedSel);

    -- Scalar signals
    anyReadyFull <= isNonzero(readyMask);
    sends <= anyReadyFull and nextAccepting;

    -- Selection for issue
    selMask <= getSelMask(readyMask, ageMatrix);
               -- selMaskH;
        selMaskH <= getSelMask_H(readyMask, ageMatrix);

        ch0 <= bool2std(selMaskH = selMask);
            ch1 <= bool2std(selectedSlot_N.dynamic.renameIndex = selectedSlot.dynamic.renameIndex);
            ch2 <= bool2std(selectedSlot_N.dynamic = selectedSlot.dynamic);
            ch3 <= bool2std(selectedSlot_N.static = selectedSlot.static);

    selectedIqTag <= getIssueTag(sends, selMask);
    selectedSlot <= queueSelect(queueContentUpdatedSel, selMask);
                    --selectedSlot_N;
        selectedSlot_N <= queueSelect_N(queueContentUpdatedSel, readyMask, ageMatrix);

    schedulerOut <= getSchedEntrySlot(selectedSlot, sends, selectedIqTag);

    QUEUE_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then
            queueContent <= queueContentNext;
            ageMatrix <= updateAgeMatrix(ageMatrix, insertionLocs, fullMask);

            selMask1 <= selMask;
            selMask2 <= selMask1;

            sentKilled <= sendingKilled;
        end if;
    end process;

    outputSignals <=   (
                        sending => sends,
                        sentKilled => sentKilled,
                        trialPrev1 => sentTrial1,
                        trialPrev2 => sentTrial2
                        ,
                           cancelled_D => sentKilled or (memFail)
                        );

    COUNTERS_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then                
            if events.lateEvent = '1' or events.execEvent = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addIntTrunc(recoveryCounter, -1, 1);
            end if;
        end if;
    end process;

    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate

        DB_DATA: block
            use work.IqViewing.all;

            signal prevKillMask: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');
            signal currentStates, prevStates: IqStateArray(0 to IQ_SIZE-1) := (others => empty);
            signal queueContentPrev: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal lastEvents: IqEventArray(0 to IQ_SIZE-1) := (others => none);
        begin
            currentStates <= getCurrentStates(queueContent);

            process (clk)
            begin
                if rising_edge(clk) then
                    prevKillMask <= killMask;
                    prevStates <= currentStates;
                    queueContentPrev <= queueContent;

                    DB_reportEvents(queueContentNext, lastEvents);

                    if DB_LOG_EVENTS then
                        if dbState.dbSignal = '1' then
                            report "IQ reporting ";
                            printContent(NAME, queueContent);
                        end if;
                    end if;
                end if;
            end process;

            ALT_LAST_EVENTS: for i in 0 to IQ_SIZE-1 generate
                lastEvents(i) <= TMP_lastEvent(queueContent(i), queueContentPrev(i), prevKillMask(i));
            end generate;
        end block;

    end generate;
    -- pragma synthesis on

end Behavioral;
