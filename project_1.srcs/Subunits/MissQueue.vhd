

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicQueues.all;


entity MissQueue is
	generic(
		QUEUE_SIZE: integer := 8;
		IS_LOAD_QUEUE: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		acceptingOut: out std_logic; -- UNUSED
		almostFull: out std_logic;   -- UNUSED
		acceptAlloc: out std_logic;
		
   	    prevSendingRe: in std_logic;
		prevSending: in std_logic;

        renameMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        inputMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        systemMask: in std_logic_vector(0 to PIPE_WIDTH-1);

        renamedPtr: out SmallNumber;

		compareAddressEarlyInput: in ExecResult;
        compareAddressEarlyInput_Ctrl: in ControlPacket;
            earlyInput: in ExecPacket;

		compareAddressInput: in ExecResult;
		compareAddressCtrl: in ControlPacket;
            lateInput: in ExecPacket;

		storeValueResult: in ExecResult;

        selectedDataOutput: out ControlPacket;
        selectedDataResult: out ExecResult;
            selectedEP: out ExecPacket;

		committing: in std_logic;
        commitMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        commitEffectiveMask: in std_logic_vector(0 to PIPE_WIDTH-1);

		events: in EventState;

		nextAccepting: in std_logic;		

		committedEmpty: out std_logic;
		committedSending: out std_logic;
		committedDataOut: out ControlPacket;
		
		dbState: in DbCoreState
	);
end MissQueue;

architecture DefaultMQ of MissQueue is

    alias lateEventSignal is events.lateCausing.full;
    alias execEventSignal is events.execCausing.full;

    type MQ_Entry is record
        full: std_logic;
        ready: std_logic;
        active: std_logic;
        
        tag: InsTag;
        lqPointer: SmallNumber;
        sqPointer: SmallNumber;
        
        dest: PhysName;
        adr: Mword;
        sqTag: SmallNumber;
        op: SpecificOp;
        
        fp: std_logic;
        tlbMiss: std_logic;
        dataMiss: std_logic;
        sqMiss: std_logic;
        
        TMP_cnt: SmallNumber;
        
        dbInfo: InstructionDebugInfo;
    end record;
    
    constant DEFAULT_MQ_ENTRY: MQ_Entry := (
        full => '0',
        ready => '0',
        active => '0',
        tag => (others => '0'),
        lqPointer => (others => '0'),
        sqPointer => (others => '0'),
        dest => (others => '0'),
        adr => (others => '0'),
        sqTag => (others => '0'),
        op => DEFAULT_SPECIFIC_OP,
        fp => '0',
        tlbMiss => '0',
        dataMiss => '0',
        sqMiss => '0',
        
        TMP_cnt => (others => '0'),
        
        dbInfo => DEFAULT_DEBUG_INFO);
    
    type MQ_EntryArray is array(integer range <>) of MQ_Entry;
    
    signal lateSending, canSend, sending, sending1, sending2, sending3, isFull, isAlmostFull: std_logic := '0';

    signal queueContent: MQ_EntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_MQ_ENTRY);    
    signal addresses, tags, renameTags: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    
    signal fullMask, fullMask_T, killMask, wakeupMask, selectMask --,-- 
            ,inputFullMask, outputFullMask3
            : std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0'); 
    signal writePtr, selPtr0, selPtr1, selPtr2, selPtr3, nFull --, nFullNext --, nIn, nInRe, nOut --, --nCommitted, nCommittedEffective --, recoveryCounter
    : SmallNumber := (others => '0');

    signal selValid0, selValid1, selValid2, selValid3: std_logic := '0';

    signal adrInWord, tagInWord: Mword := (others => '0');

    signal selectedEPSig: ExecPacket := DEFAULT_EXEC_PACKET;


        function makeOutputData(entry: MQ_Entry; adr, tagWord, renameTagWord: Mword; sending2, lateEventSignal: std_logic) return ControlPacket is
            variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
        begin
            res.controlInfo.c_full := sending2 and entry.full and not lateEventSignal;
            res.tag := renameTagWord(TAG_SIZE-1 downto 0);

            res.target := adr;
            res.op := entry.op;
            res.classInfo.useFP := entry.fp;
            res.controlInfo.tlbMiss := entry.tlbMiss;
            res.controlInfo.dataMiss := entry.dataMiss;
            res.controlInfo.sqMiss := entry.sqMiss;
            res.tags.renameIndex := entry.tag;
            res.tags.lqPointer := tagWord(31 downto 24);
            res.tags.sqPointer := tagWord(23 downto 16);

            res.dbInfo := entry.dbInfo;

            return res;
        end function;

        function makeOutputResult(entry: MQ_Entry; tagWord: Mword) return ExecResult is
            variable res: ExecResult := DEFAULT_EXEC_RESULT;
        begin
            res.dest := tagWord(15 downto 8);

            res.dbInfo := entry.dbInfo;

            return res;
        end function;


    function TMP_getNewIndex(fullMask: std_logic_vector) return natural is
        variable res: natural := 0;
    begin
        for i in 0 to fullMask'length-1 loop
            if fullMask(i) /= '1' then
                return i;
            end if;
        end loop;
        
        return res;
    end function;
    
    function getKillMask(content: MQ_EntryArray; execEvent, lateEvent: std_logic; tag: InsTag) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        if lateEvent = '1' then
            res := (others => '1');
            return res;
        elsif execEvent /= '1' then
            return res;
        end if;

        for i in 0 to res'length-1 loop
            if compareTagBefore(tag, content(i).tag) = '1' and content(i).full = '1' then
                res(i) := '1';
            end if;
        end loop;
        
        return res;
    end function;

        function checkReady(entry: MQ_entry; svResult: ExecResult) return std_logic is
        begin
            -- CAREFUL: compare SQ pointers ignoring the high bit of StoreData sqPointer (used for ordering, not indexing the content!)
            -- TODO!!: (2 downto 0) to be replaced by proper expression (based on SQ size?)
            return entry.active and entry.sqMiss and svResult.full and bool2std(entry.sqTag(2 downto 0) = svResult.dest(2 downto 0));
        end function;

    function getWakeupMask(content: MQ_EntryArray; sv: ExecResult) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        for i in 0 to QUEUE_SIZE-1 loop
            res(i) := checkReady(content(i), sv);
        end loop;
        return res;
    end function;

    function getSelectMask(content: MQ_EntryArray) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        for i in 0 to content'length - 1 loop
            if content(i).full = '1' and content(i).active = '1' and content(i).ready = '1' then
                res(i) := '1';
            end if;
        end loop;

        res := getFirstOne(res);
        return res;
    end function;

    function maskFromIndex(index: SmallNumber; constant len: natural) return std_logic_vector is
        variable res: std_logic_vector(0 to len-1) := (others => '0');
        constant indexI: natural := slv2u(index);
    begin
        res(indexI) := '1';
        return res;
    end function;

    function TMP_firstOnePtr(v: std_logic_vector) return SmallNumber is
        variable res: SmallNumber := (others => '0');
    begin
        for i in 0 to v'length-1 loop
            if v(i) = '1' then
                return i2slv(i, SMALL_NUMBER_SIZE);
            end if;
        end loop;
        return res;
    end function;

    signal prevSendingEarly: std_logic := '0';

    signal ch0, ch1, ch2, ch3: std_logic := '0'; 
begin
        ch0 <= bool2std(fullMask_T /= fullMask);


    prevSendingEarly <= compareAddressEarlyInput_Ctrl.--controlInfo.c_full;
                                                      full;

    canSend <= '1';

    lateSending <= compareAddressInput.full;

    writePtr <= i2slv(TMP_getNewIndex(fullMask), SMALL_NUMBER_SIZE);
    killMask <= getKillMask(queueContent, execEventSignal, lateEventSignal, events.execCausing.tag);
    inputFullMask <= maskFromIndex(writePtr, QUEUE_SIZE) when --lateSending = '1' else (others => '0');
                                                              prevSendingEarly = '1' else (others => '0');
    -- completion and subsequent removal from queue is triggered by:
    --      L1 fill in case of Cache miss ops
    --      SQ value fill in case of missed SQ forwarding 

        wakeupMask <= getWakeupMask(queueContent, storeValueResult);
        
    selectMask <= getSelectMask(queueContent);

    sending <= canSend and isNonzero(selectMask);

    selPtr0 <= TMP_firstOnePtr(selectMask);
    selValid0 <= sending;

    outputFullMask3 <= maskFromIndex(selPtr3, QUEUE_SIZE) when selValid3 = '1' else (others => '0');


    READY_MASK: for i in 0 to QUEUE_SIZE-1 generate
        fullMask(i) <= queueContent(i).full;
    end generate;

    nFull <= i2slv(countOnes(fullMask), SMALL_NUMBER_SIZE); 

    process (clk)
        procedure updateEarly(signal content: inout MQ_EntryArray; ind: natural; ctl: ControlPacket) is
        begin
            content(ind).full <= '1';
            content(ind).active <= '0';
            content(ind).ready <= '0';
            content(ind).tag <= ctl.tags.renameIndex;
            content(ind).TMP_cnt <= sn(0);--(others => '0');
            content(ind).dbInfo <= ctl.dbInfo;
        end procedure;

        procedure updateLate(signal content: inout MQ_EntryArray; ind: natural; ctl: ControlPacket; res: ExecResult) is
        begin
            content(ind).active <= '1';
            content(ind).adr <= compareAddressCtrl.ip;
            content(ind).sqTag <= compareAddressCtrl.target(SMALL_NUMBER_SIZE-1 downto 0);

            content(ind).dest <= compareAddressInput.dest;

            content(ind).lqPointer <= compareAddressCtrl.tags.lqPointer;
            content(ind).sqPointer <= compareAddressCtrl.tags.sqPointer;

            content(ind).op <= compareAddressCtrl.op;

            content(ind).fp <= compareAddressCtrl.classInfo.useFP;
            content(ind).tlbMiss <= compareAddressCtrl.controlInfo.tlbMiss;
            content(ind).dataMiss <= compareAddressCtrl.controlInfo.dataMiss;
            content(ind).sqMiss <= compareAddressCtrl.controlInfo.sqMiss;
        end procedure;

        procedure remove(signal content: inout MQ_EntryArray; ind: natural) is
        begin
            content(ind).full <= '0';
            content(ind).active <= '0';
            content(ind).ready <= '0';
            content(ind).dbInfo <= DEFAULT_DEBUG_INFO;
        end procedure;

        function isFillTime(entry: MQ_entry) return std_logic is
        begin
            return entry.full and not entry.active and not entry.ready and bool2std(entry.TMP_cnt = sn(2));
        end function; 


        procedure DB_logTrackedOp(entry: MQ_Entry; txt: string) is
        begin
            -- pragma synthesis off
            if DB_OP_TRACKING and entry.dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                report "";
                report "DEBUG: " & txt & ": " & work.CpuText.slv2hex(DB_TRACKED_SEQ_NUM);
                report "";
            end if;
            -- pragma synthesis on
        end procedure;
    begin
        if rising_edge(clk) then

            selPtr1 <= selPtr0;
            selPtr2 <= selPtr1;
            selPtr3 <= selPtr2;
        
            selValid1 <= selValid0;
            selValid2 <= selValid1;
            selValid3 <= selValid2;
            
            sending1 <= sending;
            sending2 <= sending1;
            sending3 <= sending2;

            for i in 0 to QUEUE_SIZE-1 loop

                if --checkReady(queueContent(i), storeValueResult) = '1' then -- Wakeup for dependents on SQ
                   wakeupMask(i) = '1' then
                    queueContent(i).ready <= '1';
                end if;

                if (canSend and selectMask(i)) = '1' then
                    queueContent(i).active <= '0';
                end if;

                -- TODO!!: take care of this semi-redundant statement and command ordering! 
                --    queueContent(i).full <= (fullMask(i) and not killMask(i) and not outputFullMask3(i)) or inputFullMask(i);
                    fullMask_T(i) <= (fullMask(i) and not killMask(i) and not outputFullMask3(i)) or inputFullMask(i);
                queueContent(i).TMP_cnt <= addIntTrunc(queueContent(i).TMP_cnt, 1, 3);

                -- Update late part of slot or free it if no miss
                if isFillTime(queueContent(i)) = '1' then
                    if lateSending = '1' then
                        updateLate(queueContent, i, compareAddressCtrl, compareAddressInput);

                        addresses(i) <= compareAddressCtrl.ip;
                        tags(i) <= tagInWord; -- CAREFUL: tag duplicated. Impact on efficiency not known (redundant memory but don't need output mux for FF data) 
                        renameTags(i)(TAG_SIZE-1 downto 0) <= compareAddressCtrl.tag;

                        DB_logTrackedOp(queueContent(i), "put to MQ");
                    else
                        remove(queueContent, i);
                    end if;

                end if;

                if killMask(i) = '1' then
                    remove(queueContent, i);
                    DB_logTrackedOp(queueContent(i), "flushed from MQ");
                end if;
            end loop;

            if prevSendingEarly = '1' then
                updateEarly(queueContent, p2i(writePtr, QUEUE_SIZE), compareAddressEarlyInput_Ctrl);
            end if;

            if sending3 = '1' then
                remove(queueContent, p2i(selPtr3, QUEUE_SIZE));
                
                DB_logTrackedOp(queueContent(p2i(selPtr3, QUEUE_SIZE)), "sent from MQ");
            end if;

            isFull <= cmpGtU(nFull, QUEUE_SIZE-3);
            isAlmostFull <= cmpGtU(nFull, QUEUE_SIZE-5);
        end if;

    end process;

    tagInWord <= compareAddressCtrl.tags.lqPointer & compareAddressCtrl.tags.sqPointer & compareAddressInput.dest & X"00";

    OUTPUTS: block
        signal outEntrySig: MQ_Entry := DEFAULT_MQ_ENTRY;
        signal adrOutWord, tagOutWord, renameTagOutWord: Mword := (others => '0');
        signal full3: std_logic := '0';
    begin
        process (clk)
        begin
            if rising_edge(clk) then
                outEntrySig <= queueContent(p2i(selPtr1, QUEUE_SIZE));
                adrOutWord <= addresses(p2i(selPtr1, QUEUE_SIZE));
                tagOutWord <= tags(p2i(selPtr1, QUEUE_SIZE));
                renameTagOutWord <= renameTags(p2i(selPtr1, QUEUE_SIZE));
                
                --selectedEPSig <= 
            end if;
        end process;
    
--        outEntrySig <= queueContent(p2i(selPtr2, QUEUE_SIZE));
--        adrOutWord <= addresses(p2i(selPtr2, QUEUE_SIZE));
--        tagOutWord <= tags(p2i(selPtr2, QUEUE_SIZE));
--        renameTagOutWord <= renameTags(p2i(selPtr2, QUEUE_SIZE));

        full3 <= queueContent(p2i(selPtr2, QUEUE_SIZE)).full;

        selectedDataOutput <= makeOutputData(outEntrySig, adrOutWord, tagOutWord, renameTagOutWord, sending2 and full3, lateEventSignal);
        selectedDataResult <= makeOutputResult(outEntrySig, tagOutWord);

        selectedEP <= selectedEPSig;
    end block;

    committedSending <= sending1; -- Indication to block normal mem issue

    almostFull <= isAlmostFull;
    acceptingOut <= not isFull;
    
    
    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate
        --use work.MemQueueViewing.all;

        type EntryState is (empty, allocated, waiting, ready);
        type EntryStateArray is array(0 to QUEUE_SIZE-1) of EntryState;
        
        signal states: EntryStateArray := (others => empty);

        procedure stateError(msg: string; oldVal: EntryState; ind: natural) is
        begin
            report msg & "; at (" & natural'image(ind) & ") = " &  EntryState'image(oldVal) severity failure;
        end procedure;
        
        signal written1, written2, written3: std_logic := '0';
        signal wp1, wp2, wp3: SmallNumber := sn(0);
        
        procedure writeState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when empty => newVal := allocated;
                when others => stateError("WRITE: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure confirmState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when allocated => newVal := waiting;
                when others => stateError("CONFIRM: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure unconfirmState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when allocated => newVal := empty;
                when others => stateError("UNCONFIRM: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure updateState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when waiting => newVal := ready;
                when others => stateError("UPDATE: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure issueState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                    -- Possible empty slot when removing issued op - may have been killed.
                when empty => -- TODO: use issuedCtr for entries like in IQ, this case will become unneeded (and incorrect)
                when ready => newVal := empty;
                when others => stateError("ISSUE: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure killState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when others => newVal := empty;
            end case;
            table(ind) <= newVal;
        end procedure;


        procedure DB_writeStates(signal table: inout EntryStateArray; newP: SmallNumber) is
        begin
             --       report "Writing n: " & natural'image(n);
             --table(p2i(newP, QUEUE_SIZE)) <= allocated;
             writeState(table, p2i(newP, QUEUE_SIZE));
        end procedure;

        procedure DB_confirmStates(signal table: inout EntryStateArray; newP: SmallNumber) is
        begin
             --table(p2i(newP, QUEUE_SIZE)) <= waiting;
             confirmState(table, p2i(newP, QUEUE_SIZE));
        end procedure;

        procedure DB_unconfirmStates(signal table: inout EntryStateArray; newP: SmallNumber) is
        begin
             --table(p2i(newP, QUEUE_SIZE)) <= empty;
             unconfirmState(table, p2i(newP, QUEUE_SIZE));
        end procedure;

        procedure DB_updateStates(signal table: inout EntryStateArray; en: std_logic; mask: std_logic_vector) is
            variable dest: SmallNumber := sn(0);
        begin
            for i in 0 to QUEUE_SIZE-1 loop
                if mask(i) = '1' then
                    --table(i) <= ready;
                    updateState(table, i);
                end if;
            end loop;
        end procedure;

        procedure DB_issueStates(signal table: inout EntryStateArray; issueP: SmallNumber) is
        begin
             --table(p2i(issueP, QUEUE_SIZE)) <= empty;
             issueState(table, p2i(issueP, QUEUE_SIZE));
        end procedure;

        procedure DB_killStates(signal table: inout EntryStateArray; lateEvent: std_logic; mask: std_logic_vector) is
            variable dest: SmallNumber := sn(0);
        begin
            if lateEvent = '1' then
                for i in 0 to QUEUE_SIZE-1 loop
                    killState(table, i);
                end loop;
                --table <= (others => empty);
                return;
            end if;
        
            for i in 0 to QUEUE_SIZE-1 loop
                if mask(i) = '1' then
                    --table(i) <= empty;
                    killState(table, i);
                end if;
            end loop;
        end procedure;

    begin

        process (clk)
        begin
            if rising_edge(clk) then
                wp1 <= writePtr;
                wp2 <= wp1;
                wp3 <= wp2;
            
                written2 <= written1;
                written3 <= written2;
            
                if prevSendingEarly = '1' then
                    written1 <= '1';
                    
                    DB_writeStates(states, writePtr);
                else
                    written1 <= '0';
                end if;

                if (written3 and not lateInput.killed) = '1' then
                    if lateSending = '1' then
                        DB_confirmStates(states, wp3);
                    else
                        DB_unconfirmStates(states, wp3);
                    end if;
                end if;

                if storeValueResult.full = '1' then
                    DB_updateStates(states, '1', wakeupMask);
                end if;

                if sending3 = '1' then
                    DB_issueStates(states, selPtr3);
                end if;

                if (events.lateCausing.full or events.execCausing.full) = '1' then
                    DB_killStates(states, events.lateCausing.full, killMask);
                end if;

                if DB_LOG_EVENTS then
                    if dbState.dbSignal = '1' then
                        report "MQ reporting ";
                        --printContent(queueContent_NS, addresses, storeValues, pStart, pTagged, getNamePrefix(IS_LOAD_QUEUE));
                    end if;
                end if;
            end if;
        end process;
    end generate;
    -- pragma synthesis on
end DefaultMQ;
