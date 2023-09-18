----------------------------------------------------------------------------------

--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;

use work.LogicIssue.all;
use work.LogicArgRead.all;

use work.ForwardingNetwork.all;


entity IssueQueue is
	generic(
	    NAME: string;
		IQ_SIZE: natural := 12;
		FORWARDING: ForwardingModeArray(0 to 2) := (others => (-100, false));  -- Can be used immediately
		FORWARDING_D: ForwardingModeArray(0 to 2) := (others => (-100, false)); -- Can be used with 1 cycle delay
		IGNORE_MEM_FAIL: boolean := false;
		      WAKEUP_SPEC: WakeupSpec := DEFAULT_WAKEUP_SPEC
		   --   WTF_MEM_FAIL: std_logic := '1'
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		prevSendingOK: in std_logic;
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);

        inReady: in std_logic;
        inMask: std_logic_vector;

        TMP_outTagsPre: out SmallNumberArray(0 to RENAME_W-1);
        TMP_outTags: out SmallNumberArray(0 to RENAME_W-1);

        accept: out std_logic;

		nextAccepting: in std_logic;
        unlockDiv: in std_logic;
            
		events: in EventState;

        bypass: in BypassState;

        schedulerOut_Fast: out SchedulerState;
        schedulerOut_Slow: out SchedulerState;

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

    signal ageMatrix: slv2D(0 to IQ_SIZE-1, 0 to IQ_SIZE-1) := (others => (others => '0'));
    signal insertionLocs: slv2D(0 to IQ_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal wups, wupsSelection: WakeupStructArray2D(0 to IQ_SIZE-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));  

    signal fullMask, killMask, freedMask, freedMask_N, readyMask, selMask, selMask1, selMask2, selMaskH, trialMaskAll, TMP_trialMask1, TMP_trialMask2: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');

    signal anyReadyFull, sends, sent, sendingKilled, sentKilled, sentTrial1, sentTrial2, TMP_trial1, TMP_trial2: std_logic := '0';

    signal inTags: SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));

    signal recoveryCounter: SmallNumber := (others => '0');

    signal selectedSlot, selectedSlot_Slow: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    signal selectedIqTag, prevIqTag, prevPrevIqTag: SmallNumber := (others => '0');

    signal schedulerOutSig, issuedFastState, issuedFastStateU, issuedSlowState: SchedulerState := DEFAULT_SCHED_STATE;
    signal outSigs: IssueQueueSignals := (others => '0');

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

        outTags => inTags,
        outTagsPre => TMP_outTagsPre,

        accept => accept,

        iqUsed => fullMask,
        iqFreed => freedMask
    );

    TMP_outTags <= inTags;

    wups <= getSlowWakeups(queueContent, bypass, CFG_WAIT);

    FAST_WAKEUP: if ENABLE_FAST_WAKEUP generate
        wupsSelection <= getFastWakeups(queueContent, bypass, CFG_SEL);
    end generate;

    queueContentUpdatedSel <= updateQueueArgs_S(queueContent, wupsSelection, CFG_SEL);

    freedMask <= getFreedMask(queueContent);
    freedMask_N <= getFreedMask_N(queueContent);
    fullMask <= getFullMask(queueContent);

    QUEUE_CTRL: block
        signal sendingTrial: std_logic := '0';
        signal trialMask, trialUpdatedMask, retractMask0, retractMask1, pullbackMask,  retractMask0_N, retractMask1_N, pullbackMask_N: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');
        signal updates: SchedulerUpdateArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_UPDATE);  

        function getUpdates(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig;
                            killMask, trialMask, trialUpdatedMask, readyMask, selMask, retractMask0, retractMask1, pullbackMask: std_logic_vector        
        ) return SchedulerUpdateArray is
            variable res: SchedulerUpdateArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_UPDATE);
        begin
            for i in 0 to IQ_SIZE-1 loop
                res(i).kill := killMask(i);
                res(i).trial := trialMask(i);
                --freed
                res(i).retract(0) := retractMask0(i);
                res(i).retract(1) := retractMask1(i);
                res(i).pullback := pullbackMask(i);
                --suspend
                --resume
                res(i).ready := readyMask(i);
                res(i).selected := selMask(i);
            end loop;
            return res;
        end function;

        function getPullbackMask0(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable depends: std_logic := '0';       
        begin
            for i in res'range loop
                depends := dependsOnMemHit(content(i).dynamic.argStates(0), config.fp);
                res(i) := depends and memFail and not bool2std(config.ignoreMemFail);
            end loop;
            return res;
        end function;

        function getPullbackMask1(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable depends: std_logic := '0';       
        begin
            for i in res'range loop
                depends := dependsOnMemHit(content(i).dynamic.argStates(1), config.fp);
                res(i) := depends and memFail and not bool2std(config.ignoreMemFail);
            end loop;
            return res;
        end function;

        function getPullbackMask(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable depends: std_logic := '0';       
        begin
            for i in res'range loop
                res(i) := content(i).dynamic.status_N.issued0 and memFail; -- and not bool2std(config.ignoreMemFail);
            end loop;
            return res;
        end function;



        function getPullbackMask0_N(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable depends: std_logic := '0';       
        begin
            for i in res'range loop
                depends := content(i).dynamic.argStates(0).poison.isOn and content(i).dynamic.argStates(0).poison.degrees(2);
                res(i) := depends and memFail and not bool2std(config.ignoreMemFail);
            end loop;
            return res;
        end function;

        function getPullbackMask1_N(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable depends: std_logic := '0';       
        begin
            for i in res'range loop
                depends := content(i).dynamic.argStates(1).poison.isOn and content(i).dynamic.argStates(1).poison.degrees(2);
                res(i) := depends and memFail and not bool2std(config.ignoreMemFail);
            end loop;
            return res;
        end function;

        function getIssuedMask(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable depends: std_logic := '0';       
        begin
            for i in res'range loop
                res(i) := content(i).dynamic.status_N.issued0 or content(i).dynamic.status_N.issued1 or content(i).dynamic.status_N.issued2;
            end loop;
            return res;
        end function;

    begin
        trialMask <= getTrialMask(queueContent, events);

        retractMask0 <= getPullbackMask0(queueContent, memFail, CFG_WAIT);
        retractMask1 <= getPullbackMask1(queueContent, memFail, CFG_WAIT);
        pullbackMask <= getPullbackMask(queueContent, memFail, CFG_WAIT);

            retractMask0_N <= getPullbackMask0_N(queueContent, memFail, CFG_WAIT);
            retractMask1_N <= getPullbackMask1_N(queueContent, memFail, CFG_WAIT);
            pullbackMask_N <= --getPullbackMask_N(queueContent, memFail, CFG_WAIT);
                                (retractMask0_N or retractMask1_N) and getIssuedMask(queueContent, memFail, CFG_WAIT);

            trialMaskAll <= trialMask or not fullMask; -- empty slots are "on trial" because new ops are younger than Exec
            TMP_trialMask1 <= trialMaskAll and selMask;
            TMP_trialMask2 <= trialMaskAll and selMask1;

                ch0 <= bool2std(retractMask0_N = retractMask0);
                ch1 <= bool2std(retractMask1_N = retractMask1);
                ch2 <= bool2std(pullbackMask_N = pullbackMask);

        trialUpdatedMask <= getTrialUpdatedMask(queueContent);

        killMask <= (others => '1') when events.lateCausing.full = '1' 
               else trialUpdatedMask when events.execCausing.full = '1'
               else (others => '0');


            updates <= getUpdates(queueContent, memFail, CFG_WAIT,
                                    killMask, trialMask, trialUpdatedMask, readyMask, selMask,
                                        retractMask0, retractMask1, pullbackMask
                                    );

        queueContentUpdated <= updateQueueArgs(queueContent, wups, updates, memFail, CFG_WAIT);
        queueContentUpdated_2 <= updateQueueState(queueContentUpdated, nextAccepting, sends,
                                                updates,
                                                memFail, unlockDiv);

        sendingTrial <= isNonzero(selMask and trialUpdatedMask);
        sendingKilled <= killFollower(sendingTrial, events);

        sentTrial1 <= --isNonzero(selMask1 and trialUpdatedMask);
                        TMP_slowSelect(trialUpdatedMask, prevIqTag);
        sentTrial2 <= --isNonzero(selMask2 and trialUpdatedMask);
                        TMP_slowSelect(trialUpdatedMask, prevPrevIqTag);
    end block;

    insertionLocs <= getNewLocs(fullMask, inTags, newArr);
    queueContentNext <= storeInput(queueContentUpdated_2, newArr, prevSendingOK, events.execCausing.full or events.lateCausing.full, insertionLocs);

    readyMask <= getReadyMask(queueContentUpdatedSel);

    -- Scalar signals
    anyReadyFull <= isNonzero(readyMask);
    sends <= anyReadyFull and nextAccepting;

    -- Selection for issue
    selMaskH <= getSelMask_H(readyMask, ageMatrix);
    selMask <= selMaskH;

    selectedIqTag <= getIssueTag(sends, selMask);
    selectedSlot <= queueSelect_N(queueContentUpdatedSel, readyMask, ageMatrix);
                    --queueSelect(queueContentUpdatedSel, selMask);

    schedulerOutSig <= getSchedEntrySlot(selectedSlot, sends, selectedIqTag);

    issuedFastStateU <= updateIssueStage(issuedFastState, outSigs, events);
    schedulerOut_Fast <= issuedFastStateU;

    selectedSlot_Slow <= TMP_slowSelect(queueContent, prevIqTag);
    issuedSlowState <= getSchedEntrySlot(selectedSlot_Slow, sent, getIssueTag(sent, selMask1));
    schedulerOut_Slow <= issuedSlowState;

    QUEUE_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then
            queueContent <= queueContentNext;
            ageMatrix <= updateAgeMatrix(ageMatrix, insertionLocs, fullMask);

            selMask1 <= selMask;
            selMask2 <= selMask1;

            sent <= sends;
            sentKilled <= sendingKilled;

            TMP_trial1 <= isNonzero(TMP_trialMask1);
            TMP_trial2 <= isNonzero(TMP_trialMask2);

            issuedFastState <= getIssueStage(schedulerOutSig, outSigs, events);
            prevIqTag <= getIssueTag('0', selMask);
            prevPrevIqTag <= prevIqTag;
        end if;
    end process;

    outSigs <=   (
            sending => sends,
            sentKilled => sentKilled,
            trialPrev1 => '0',--sentTrial1,
                          --  TMP_trial1,
            trialPrev2 => '0'--sentTrial2
                          --  TMP_trial2
            );
    outputSignals <= outSigs;
         --   ch0 <= '0';

    COUNTERS_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then                
            if events.lateCausing.full = '1' or events.execCausing.full = '1' then
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
