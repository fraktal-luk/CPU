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

    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);

    signal ageMatrix, ageMatrixNext: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
    signal insertionLocs: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal fma: ForwardingMatchesArray(0 to QUEUE_SIZE_EXT - 1) := (others => DEFAULT_FORWARDING_MATCHES);

    signal controlSigs: SlotControlArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SLOT_CONTROL);
    signal fullMask, trialMask, readyMaskLive, killMask, readyMaskAll, selMask, selMask1, selMask2, selMask3, selMask4,
            depMemE1_0, depMemE1_1, depAluRR_0, depAluRR_1
        : std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
    signal anyReadyFull, anyReadyLive, sends, sendingKilled, maybeSent, maybeSent2, maybeSent3, maybeSent4,
                    isSent, isSent2, isSent3, isSent4, sentKilled, sentKilled1, sentKilled2, sentKilled3, sentKilled4, isEmpty: std_logic := '0';
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    signal selectedIqTag: SmallNumber := (others => '0');

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
    
        constant CFG_WAIT: work.LogicIssue.SchedulerUpdateConfig := (false, false, DONT_MATCH1, IGNORE_MEM_FAIL, FORWARDING_D, false);
        constant CFG_SEL: work.LogicIssue.SchedulerUpdateConfig :=  (false, true,  DONT_MATCH1, IGNORE_MEM_FAIL, FORWARDING, false);

        constant CFG_WAIT_Alt: work.LogicIssue.SchedulerUpdateConfig := (false, false, DONT_MATCH1, IGNORE_MEM_FAIL, FORWARDING_D, true);        
        signal wakeups, wakeups_Alt: WakeupInfoArray(0 to QUEUE_SIZE_EXT-1);
        
 
        --                type ArgWakeup is record
        --                    active: std_logic;
        --                    mode: WakeupMode;
        --                    producer: InsTag;
        --                    iqTag: SmallNumber;
                            
        --                    match: std_logic;
        --                    pipe:  SmallNumber;
        --                    stage: SmallNumber;
        --                end record;
                    
                    
        --                type WakeupInfo is record
        --                    active: std_logic; -- if happening this cycle
        --                    arg0: ArgWakeup;
        --                    arg1: ArgWakeup;
        --                end record;       
        function getWakeup(argState: ArgumentState; fni: ForwardingInfo; constant MODES: WakeupSpec; constant MODE_IND: natural) return ArgWakeup is
            variable res: ArgWakeup;
            variable mode: WakeupMode := NONE;
            constant N_SRCS: natural := MODES'length(2);
            variable matched, matchedM3, matchedM2, matchedM1: std_logic := '0';
            --variable pos: natural := 0;
            variable iqTagFull, iqTagFullM2, iqTagFullM1: SmallNumber := sn(0);
        begin
            for i in 0 to N_SRCS-1 loop
                mode := MODES(MODE_IND, i);
                
                case mode is
                    when FAST =>
                        iqTagFull := fni.iqTagsM2(i);
                        iqTagFull := iqTagFull or sn(16*(1+i));
                        matched := bool2std(argState.iqTag = iqTagFull);
                        if (matched and argState.waiting) = '1' then
                            res.active := '1';
                            res.mode := FAST;
                            --res.producer := 
                            res.iqTag := iqTagFull;
                            res.match := '1';
                            res.pipe := sn(i);
                            res.stage := X"00";
                        end if;
                        
                    when SLOW =>
                        matched := bool2std(argState.reg = fni.nextTagsM3(i));
                        if (matched and argState.waiting) = '1' then
                            res.active := '1';
                            res.mode := SLOW;
                            --res.producer := 
                            res.iqTag := fni.iqTagsM3(i);
                            res.match := '1';
                            res.pipe := sn(i);
                            res.stage := X"03";
                        end if;
                        
                    when REG =>
                        matched := bool2std(argState.reg = fni.tags0(i));
                        if (matched and argState.waiting) = '1' then
                            res.active := '1';
                            res.mode := REG;
                            --res.producer := 
                            res.iqTag := fni.iqTags0(i);
                            res.match := '1';
                            res.pipe := sn(i);
                            res.stage := X"02";
                        end if;
                        
                    when INIT_FAST => ------------------------------------------------
                        iqTagFullM2 := fni.iqTagsM2(i);
                        iqTagFullM2 := iqTagFull or sn(16*(1+i));
                        matchedM2 := bool2std(argState.iqTag = iqTagFullM2);
                        iqTagFullM1 := fni.iqTagsM1(i);
                        iqTagFullM1 := iqTagFullM1 or sn(16*(1+i));
                        matchedM1 := bool2std(argState.iqTag = iqTagFullM1);
                        matched := matchedM2 or matchedM1;
                        if (matched and argState.waiting) = '1' then
                            res.active := '1';
                            res.mode := INIT_FAST;
                            --res.producer := 
                            res.match := '1';
                            res.pipe := sn(i);
                            
                            if matchedM1 = '1' then
                                res.iqTag := iqTagFullM1;
                                res.stage := X"01";
                            else
                                res.iqTag := iqTagFullM2;
                                res.stage := X"00";
                            end if;
                        end if;

                    when INIT_SLOW => -----------------------------------------------
                        matchedM3 := bool2std(argState.reg = fni.nextTagsM3(i));
                        matchedM2 := bool2std(argState.reg = fni.nextTagsM2(i));
                        matchedM1 := bool2std(argState.reg = fni.nextTagsM1(i));
                        matched := matchedM3 or matchedM2 or matchedM1; 
                        if (matched and argState.waiting) = '1' then
                            res.active := '1';
                            res.mode := INIT_SLOW;
                            --res.producer := 
                            res.match := '1';
                            res.pipe := sn(i);
                            
                            if matchedM1 = '1' then
                                res.iqTag := fni.iqTagsM1(i);
                                res.stage := X"01";
                            elsif matchedM2 = '1' then
                                res.iqTag := fni.iqTagsM2(i);
                                res.stage := X"00";                      
                            else
                                res.iqTag := fni.iqTagsM3(i);
                                res.stage := X"03";
                            end if;
                        end if;
                        
                    when INIT_REG => ------------------------------------------------
                        matched := bool2std(argState.reg = fni.tags0(i) or argState.reg = fni.tags0(i));
                        if (matched and argState.waiting) = '1' then
                            res.active := '1';
                            res.mode := INIT_REG;
                            --res.producer := 
                            res.iqTag := fni.iqTags0(i);
                            res.match := '1';
                            res.pipe := sn(i);
                            res.stage := X"02";
                        end if;                        
                    
                    when CONST =>
                        res.mode := CONST;
                    
                    when others =>
                end case;
                
            end loop;
            return res;
        end function;
        
        function getWakeupArray(content: SchedulerInfoArray; fni: ForwardingInfo; constant WAKEUP_SPEC: WakeupSpec; constant CFG: SchedulerUpdateConfig) return WakeupInfoArray is
            constant LEN: natural := content'length; 
            variable res: WakeupInfoArray(content'range);
            
        begin

            for i in 0 to LEN-1 loop
                res(i).arg0 := getWakeup(content(i).dynamic.argStates(0), fni, WAKEUP_SPEC, 0);
                res(i).arg1 := getWakeup(content(i).dynamic.argStates(1), fni, WAKEUP_SPEC, 1);
                res(i).active := res(i).arg0.active or res(i).arg1.active;
            end loop;
            
            return res;
        end function;
        
        signal wa: WakeupInfoArray(0 to IQ_SIZE-1);
begin

        wakeups <= getWakeupInfoA(queueContent, newArr, prevSendingOK, insertionLocs, fni, readyRegFlags, CFG_WAIT);
        wakeups_Alt <= getWakeupInfoA(queueContent, newArr, prevSendingOK, insertionLocs, fni, readyRegFlags, CFG_WAIT_ALT);

    fma <= findForwardingMatchesArray(queueContent, fni, CFG_SEL, "000");
        TMP_WUP: if USE_WAKEUP_MODES generate
          wa <= getWakeupArray(queueContent, fni, WAKEUP_SPEC, CFG_WAIT); -- CFG_WAIT is needed for 'ignoreMemFail')
        end generate;

    queueContentUpdated <= updateSchedulerArray(queueContent, fni, fma, fni.memFail, CFG_WAIT);
    queueContentUpdatedSel <= updateSchedulerArray(queueContent, fni, fma, fni.memFail, CFG_SEL);

    insertionLocs <= getNewLocs_N(fullMask, TMP_newTags, newArr);

    queueContentNext <= iqNext_NS(queueContentUpdated, newArr, prevSendingOK, sends, killMask, trialMask, selMask, readyRegFlags, insertionLocs, fni.memFail);
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
