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

		events_T: in EventState;
		events: in EventState;

		prevSendingOK: in std_logic;
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);

        inReady: in std_logic;
        inMask: std_logic_vector;

        TMP_outTagsPre: out SmallNumberArray(0 to RENAME_W-1);
        TMP_outTags: out SmallNumberArray(0 to RENAME_W-1);

        accept: out std_logic;

		nextAccepting: in std_logic;
        unlockDiv: in std_logic;

        bypass: in BypassState;

        schedulerOut_Fast: out SchedulerState;
        schedulerOut_Slow: out SchedulerState;
            outEP: out ExecPacket;
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

    signal fullMask, trialMask, killMask, freedMask, freedMask_N, freeingMask, readyMask, selMask, selMask1, selMask2, selMaskH,
              retractMask0, retractMask1, pullbackMask, pullbackMask_N,
                trialMaskAll, TMP_trialMask1, TMP_trialMask2: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');

    signal anyReadyFull, sends, sent, sendingKilled, sentKilled, sentTrial1, sentTrial2, TMP_trial1, TMP_trial2: std_logic := '0';

    signal inTags: SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));

    signal prevIqTag, prevPrevIqTag: SmallNumber := (others => '0');

    signal issuedFastState, issuedFast, issuedFastStateU, issuedSlowState: SchedulerState := DEFAULT_SCHED_STATE;
    signal outSigs: IssueQueueSignals := (others => '0');

    signal selectedEP, outEPSig: ExecPacket := DEFAULT_EXEC_PACKET;

    function killIssued(ep: ExecPacket; memFail, sentKilled: std_logic) return ExecPacket is
        variable res: ExecPacket := ep;
    begin
        if (squashOnMemFail(memFail) or sentKilled) = '1' then
            res.full := '0';
            res.killed := '1';
        end if;
        return res;
    end function;

    alias memFail is bypass.memFail;

    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';

begin

    ALLOCATOR: entity work.QueueAllocator
    generic map(
        QUEUE_SIZE => 12, BANK_SIZE => 3
    )
    port map(
        clk => clk, evt => events_T,

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
        signal trialUpdatedMask: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');
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
                depends := resolving(content(i).dynamic.argStates(0).poison);
                res(i) := depends and memFail and not bool2std(config.ignoreMemFail);
            end loop;
            return res;
        end function;

        function getPullbackMask1_N(content: SchedulerInfoArray; memFail: std_logic; config: SchedulerUpdateConfig) return std_logic_vector is
            variable res: std_logic_vector(content'range) := (others => '0');
            variable depends: std_logic := '0';       
        begin
            for i in res'range loop
                depends := resolving(content(i).dynamic.argStates(1).poison);
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
            freeingMask <= getFreedMask_N(queueContentUpdated_2);


        trialMask <= getTrialMask(queueContent, events_T);

        retractMask0 <= getPullbackMask0_N(queueContent, memFail, CFG_WAIT);
        retractMask1 <= getPullbackMask1_N(queueContent, memFail, CFG_WAIT);

        pullbackMask <= getPullbackMask(queueContent, memFail, CFG_WAIT);
        pullbackMask_N <= (retractMask0 or retractMask1) and getIssuedMask(queueContent, memFail, CFG_WAIT);

                trialMaskAll <= trialMask or not fullMask; -- empty slots are "on trial" because new ops are younger than Exec
                TMP_trialMask1 <= trialMaskAll and selMask;
                TMP_trialMask2 <= trialMaskAll and selMask1;

              ch2 <= bool2std(pullbackMask_N = pullbackMask);

        trialUpdatedMask <= getTrialUpdatedMask(queueContent);

        killMask <= (others => '1') when events_T.lateCausing.full = '1' 
               else trialUpdatedMask when events_T.execCausing.full = '1'
               else (others => '0');


        updates <= getUpdates(queueContent, memFail, CFG_WAIT,
                              killMask, trialMask, trialUpdatedMask, readyMask, selMask,
                              retractMask0, retractMask1, pullbackMask
                                                          --pullbackMask_N
                              );

        queueContentUpdated <= updateQueueArgs(queueContent, wups, updates, memFail, CFG_WAIT);
        queueContentUpdated_2 <= updateQueueState(queueContentUpdated, nextAccepting, sends,
                                                    updates, memFail, unlockDiv);

        sendingTrial <= isNonzero(selMask and trialUpdatedMask);
        sendingKilled <= killFollower(sendingTrial, events_T);

        sentTrial1 <= TMP_slowSelect(trialUpdatedMask, prevIqTag);
        sentTrial2 <= TMP_slowSelect(trialUpdatedMask, prevPrevIqTag);
    end block;

    insertionLocs <= getNewLocs(fullMask, inTags, newArr);
    queueContentNext <= storeInput(queueContentUpdated_2, newArr, prevSendingOK, events_T.execCausing.full or events_T.lateCausing.full, insertionLocs);

    readyMask <= getReadyMask(queueContentUpdatedSel);

    -- Scalar signals
    anyReadyFull <= isNonzero(readyMask);
    sends <= anyReadyFull and nextAccepting;

    -- Selection for issue
    selMaskH <= getSelMask_H(readyMask, ageMatrix);
    selMask <= selMaskH;


    TMP_ABC: block
        signal schedulerOutSig: SchedulerState := DEFAULT_SCHED_STATE;
        signal selectedSlot_Slow: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
        signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
        signal selectedIqTag: SmallNumber := (others => '0');
    begin
        -- local
        selectedIqTag <= getIssueTag(sends, selMask);
        selectedSlot <= queueSelect_N(queueContentUpdatedSel, readyMask, ageMatrix);

        -- local
        schedulerOutSig <= getSchedEntrySlot(selectedSlot, sends, selectedIqTag);
        issuedFast <= getIssueStage(schedulerOutSig, outSigs, events_T);

        selectedEP <= makeEP(schedulerOutSig);

        issuedFastStateU <= updateIssueStage(issuedFastState, outSigs, events_T);

        -- local
        selectedSlot_Slow <= TMP_slowSelect(queueContent, prevIqTag);
        issuedSlowState <= getSchedEntrySlot(selectedSlot_Slow, sent, getIssueTag(sent, selMask1));
    end block;

    schedulerOut_Fast <= issuedFastStateU;    
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

            issuedFastState <= issuedFast;
 
            outEPSig <= updateEP(selectedEP, events_T);
            if squashOnMemFail(events_T.memFail) = '1' then
                outEPSig.full <= '0';
                outEPSig.killed <= '1';
            end if;
            
            prevIqTag <= getIssueTag('0', selMask);
            prevPrevIqTag <= prevIqTag;
        end if;
    end process;

    outEP <= killIssued(outEPSig, events_T.memFail, sentKilled);

    outSigs <=   (
            sending => sends,
            sentKilled => sentKilled,
                trialPrev1 => '0',
                trialPrev2 => '0'
            );
    outputSignals <= outSigs;

    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate
        use work.IqViewing.all;

        signal prevKillMask: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');
        signal currentStates, prevStates: IqStateArray(0 to IQ_SIZE-1) := (others => empty);
        signal queueContentPrev: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);
        signal lastEvents: IqEventArray(0 to IQ_SIZE-1) := (others => none);

        signal states: EntryStateArray(0 to IQ_SIZE-1) := (others => empty);
    begin
        currentStates <= getCurrentStates(queueContent);

        process (clk)
        begin
            if rising_edge(clk) then
                prevKillMask <= killMask;
                prevStates <= currentStates;
                queueContentPrev <= queueContent;

                DB_reportEvents(queueContentNext, lastEvents);


                if (prevSendingOK and not events_T.execCausing.full and not events_T.lateCausing.full) = '1' then
                    DB_writeStates(states, insertionLocs);
                end if;
                
                if sends = '1' then
                    DB_issueStates(states, selMask);
                end if;

                -- Retractions:
                DB_retractStates(states, pullbackMask, retractMask0, retractMask1);

                DB_freeStates(states, freeingMask);

                if (events_T.execCausing.full or events_T.lateCausing.full) = '1' then
                    DB_killStates(states, events_T.lateCausing.full, killMask);
                end if;

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

    end generate;
    -- pragma synthesis on

end Behavioral;
