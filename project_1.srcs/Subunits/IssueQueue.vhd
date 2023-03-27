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
		      WAKEUP_SPEC: WakeupSpec := DEFAULT_WAKEUP_SPEC
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

		events: in EventState;

        bypass: in BypassState;

		schedulerOut: out SchedulerState;
        outputSignals: out IssueQueueSignals;

        dbState: in DbCoreState		
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
    constant N_BANKS: natural := 4;
    constant BANK_SIZE: natural := IQ_SIZE / N_BANKS;

    constant CFG_WAIT: SchedulerUpdateConfig := (false, false, IGNORE_MEM_FAIL, FORWARDING_D, false);
    constant CFG_SEL: SchedulerUpdateConfig :=  (false, false, IGNORE_MEM_FAIL, FORWARDING, false);

    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdated_2, queueContentUpdatedSel: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);

    signal ageMatrix, ageMatrixNext: slv2D(0 to IQ_SIZE-1, 0 to IQ_SIZE-1) := (others => (others => '0'));
    signal insertionLocs: slv2D(0 to IQ_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal wups, wupsSelection: WakeupStructArray2D(0 to IQ_SIZE-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));  

    signal fullMask, freedMask, readyMask, selMask, selMask1, selMask2: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');

    signal anyReadyFull, sends, sendingKilled, sentKilled, sentTrial1_T, sentTrial2_T: std_logic := '0';

    -- DB?
    --signal wa: WakeupInfoArray(0 to IQ_SIZE-1);

    signal TMP_tags: SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));

    signal recoveryCounter: SmallNumber := (others => '0');

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

        TMP_outTags => TMP_tags,
        TMP_outTagsPre => TMP_outTagsPre,

        accept => accept,

        iqUsed => fullMask,
        iqFreed => freedMask
    );

    TMP_outTags <= TMP_tags;

    wups <= getSlowWakeups(queueContent, bypass, CFG_WAIT);

    FAST_WAKEUP: if ENABLE_FAST_WAKEUP generate
        wupsSelection <= getFastWakeups_N2(queueContent, bypass, CFG_SEL);
    end generate;

    queueContentUpdatedSel <= updateSchedulerArray_S_NEW(queueContent, wupsSelection, memFail, CFG_SEL);

    freedMask <= getFreedMask(queueContent);
    fullMask <= getFullMask(queueContent);
        
    QUEUE_CTRL: block
        signal sendingTrial: std_logic := '0';
        signal trialMask, trialUpdatedMask, killMask: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');
    begin
        trialMask <= getTrialMask(queueContent, events);
        trialUpdatedMask <= getTrialUpdatedMask(queueContent);

        killMask <=   (others => '1') when events.lateEvent = '1' 
              else trialUpdatedMask  when events.execEvent = '1'
              else (others => '0');
    
        queueContentUpdated <= updateSchedulerArray_N(queueContent, wups, memFail, CFG_WAIT);
        queueContentUpdated_2 <= iqNext_NS(queueContentUpdated, sends,
                                    killMask, trialMask, selMask,
                                    memFail);

        sendingTrial <= isNonzero(selMask and trialUpdatedMask);
        sendingKilled <= (sendingTrial and events.execEvent) or events.lateEvent;

        sentTrial1_T <= isNonzero(selMask1 and trialUpdatedMask);
        sentTrial2_T <= isNonzero(selMask2 and trialUpdatedMask);
    end block;


    insertionLocs <= getNewLocs_N(fullMask, TMP_tags, newArr);
    queueContentNext <= iqNext_NS_2(queueContentUpdated_2, newArr, prevSendingOK, insertionLocs);

    ageMatrixNext <= updateAgeMatrix(ageMatrix, insertionLocs, fullMask);

    readyMask <= getReadyMask(queueContentUpdatedSel);

    -- Scalar signals
    anyReadyFull <= isNonzero(readyMask);
    sends <= anyReadyFull and nextAccepting;


    -- Selection for issue
    selMask <= getSelMask(readyMask, ageMatrix);

    -- TODO: cleanup
    TAG_OUT: block
        signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
        signal selectedIqTag: SmallNumber := (others => '0');

        signal tg: SmallNumber := sn(0);
        
        function getTagLowPart(selMask: std_logic_vector) return SmallNumber is
            variable res: SmallNumber := sn(-1);
        begin
            for i in selMask'range loop
                if selMask(i) = '1' then
                    res := sn(i);
                end if;
            end loop;
            res(7 downto 4) := (others => '0');
            return res;
        end function;
        
    begin
        tg <= getTagLowPart(selMask);

        selectedIqTag(4) <= sends;
        selectedIqTag(3 downto 0) <= tg(3 downto 0);
        
        selectedSlot <= queueSelect(queueContentUpdatedSel, selMask);
        schedulerOut <= getSchedEntrySlot(selectedSlot, sends, selectedIqTag);
    end block;


    QUEUE_SYNCHRONOUS: process(clk)
    begin
        if rising_edge(clk) then
            queueContent <= queueContentNext;
            ageMatrix <= ageMatrixNext;

            selMask1 <= selMask;
            selMask2 <= selMask1;

            sentKilled <= sendingKilled;


            DB_reportEvents(queueContentNext);
        end if;
    end process;

    outputSignals <=   (empty => '0',
                        ready => '0',
                        sending => sends,
                        cancelled => sentKilled or memFail, --
                        killFollower => (sentTrial2_T and events.execEvent) or events.lateEvent,
                        killFollowerNext => (sentTrial1_T and events.execEvent) or events.lateEvent
                        );

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
    
        DB_DATA: block
            signal lastEvents: IqEventArray(0 to IQ_SIZE-1) := (others => none);
            signal currentStates: IqStateArray(0 to IQ_SIZE-1) := (others => empty);
        begin
        
            currentStates <= getCurrentStates(queueContent);

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
                        write(currentLine, std_logic'image(queueContent(i).dynamic.status.issued));
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
                
                    lastEvents <= getLastEvents(queueContentNext);
                
                    if DB_LOG_EVENTS then
                        if dbState.dbSignal = '1' then
                            report "IQ reporting ";
                            printContent;
                        end if;
                    end if;
                end if;
            end process;
    
        end block;

    end generate;
    -- pragma synthesis on

end Behavioral;
