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
		IQ_SIZE: natural := 8;
		IS_FP: boolean := false;
		ALT_INPUT: boolean := false;
		DONT_MATCH1: boolean := false;
		FORWARDING: ForwardingModeArray := (0 => (-100, false));  -- Can be used immediately
		FORWARDING1: ForwardingModeArray := (0 => (-100, false));
		FORWARDING_D: ForwardingModeArray := (0 => (-100, false)); -- Can be used with 1 cycle delay
		NONSHIFT: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSendingOK: in std_logic;
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);
		
		  newArr_Alt: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		  newArrOut: out SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
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
    constant QUEUE_SIZE_EXT: natural := IQ_SIZE + PIPE_WIDTH;
    constant STATIC_ARRAY_SIZE: natural := 8;

    signal newArr_T: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);

    signal queueContent: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
        
        
        signal	   nFull_C,    nFull, nFullNext, nIn, nOut, recoveryCounter: SmallNumber := (others => '0');

        type slv2D is array(natural range <>, natural range <>) of std_logic;
        signal TMP_ageMatrix, TMP_ageMatrixNext, selMatrix: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
        signal TMP_insertionLocs: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

        function TMP_getNewLocs(fullMask: std_logic_vector) return slv2D is
            variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
            variable cnt: natural := 0;
        begin
            for i in 0 to QUEUE_SIZE_EXT-1 loop
                if fullMask(i) /= '1' then
                    res(i, cnt) := '1';
                    cnt := cnt + 1;
                    if cnt = PIPE_WIDTH then
                        exit;
                    end if;
                end if;
            end loop;            
            return res;
        end function;

        function TMP_updateAgeMatrix(ageMatrix, insertionLocs: slv2D; fullMask: std_logic_vector) return slv2D is
            variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := ageMatrix;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                -- insert current fullMask into proper row    
                for j in 0 to QUEUE_SIZE_EXT-1 loop
                    if insertionLocs(j, i) = '1' then
                        -- Sorry, assigment doesn;t work for ranges in >= 2D
                        for k in 0 to QUEUE_SIZE_EXT-1 loop
                            res(j, k) := fullMask(k); -- Maybe setting to all ones would work too?
                        end loop;
                        
                        -- Preceding in this group are also masked!
                        for p in 0 to PIPE_WIDTH-1 loop
                            if p = i then
                                exit;
                            end if;
                            
                            for k in 0 to QUEUE_SIZE_EXT-1 loop
                                if insertionLocs(k, p) = '1' then
                                    res(j, k) := '1';
                                    exit;
                                end if;
                            end loop;
                        end loop;
                        
                        -- Clear all dependencies on this new op
                        for k in 0 to QUEUE_SIZE_EXT-1 loop
                            res(k, j) := '0';
                        end loop;
                        
                        exit;
                    end if;
                end loop;
            end loop;
            return res;
        end function;

    function TMP_getSelMask(readyMask: std_logic_vector; ageMatrix: slv2D) return std_logic_vector is
        variable res: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := readyMask;--(others => '0');
        variable si: SchedulerInfo;
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            for j in 0 to QUEUE_SIZE_EXT-1 loop
                if j = i then
                    next;
                end if;
                
                if (readyMask(j) and ageMatrix(i, j)) = '1' then
                    res(i) := '0';
                    exit;
                end if;
            end loop;
        end loop;
             
        return res;
    end function;

    function TMP_getSelMatrix(readyMask: std_logic_vector; ageMatrix: slv2D) return slv2D is
        variable res: slv2D(0 to QUEUE_SIZE_EXT-1,0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
        variable si: SchedulerInfo;
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            for j in 0 to QUEUE_SIZE_EXT-1 loop
                res(i, j) := readyMask(j) and ageMatrix(i, j);
            end loop;
        end loop;
             
        return res;
    end function;

        signal queueContent_NS, queueContentRR_NS, queueContentNext_NS, queueContentUpdated_NS, queueContentUpdatedSel_NS:
            SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);

        signal fma_NS: ForwardingMatchesArray(0 to IQ_SIZE + PIPE_WIDTH -1) := (others => DEFAULT_FORWARDING_MATCHES);

        signal fullMask_NS, trialMask_NS, readyMaskLive_NS, killMask_NS, readyMaskAll_NS, selMask_NS, selMask_TrNS: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        signal controlSigs_NS: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    
        signal selectedSlot_NS: SchedulerInfo := DEFAULT_SCHEDULER_INFO;


    signal fullMask: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');

	signal anyReadyFull, anyReadyLive, sends, sendingKilled, isSent, isSent2, sentKilled, isEmpty: std_logic := '0';
    	signal anyReadyFull_NS, anyReadyLive_NS, sends_NS, sendingKilled_NS, isSent_NS, isSent2_NS, sentKilled_NS, isEmpty_NS: std_logic := '0';
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
	signal dispatchDataNew_T: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

    	signal dispatchDataNew_NS: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;


    signal S_firstFree: natural := 0;       
    signal S_selectedStatic, S_selectedStaticRestored: StaticInfo := DEFAULT_STATIC_INFO;
                
        signal isFull, isAlmostFull: std_logic := '0';
                                  
        signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';


    function updateRenameIndex(content: SchedulerInfoArray) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(content'range) := content;
        variable earlyStage: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1);
    begin
        earlyStage := restoreRenameIndex(earlyStage);

        res(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1) := earlyStage;
        return res;
    end function;
    
    function assignStaticPtr(arr: SchedulerInfoArray; p: natural) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(arr'range) := arr;
    begin
        for i in res'range loop
            res(i).dynamic.staticPtr := i2slv(p, SMALL_NUMBER_SIZE);
        end loop;
        return res;
    end function;
    
    function mergeWithStatic(si: SchedulerInfo; st: StaticInfo; tag: InsTag) return SchedulerInfo is
        variable res: SchedulerInfo := si;
        constant subIndV: std_logic_vector(1 downto 0) := si.dynamic.renameIndex(1 downto 0);
        constant subInd: natural := slv2u(subIndV);
        constant wordInd: natural := 8*(3-subInd); 
    begin
        res.static := st;
        res.static.bqPointerSeq := si.static.bqPointerSeq;
        res.dynamic.renameIndex(TAG_SIZE-1 downto LOG2_PIPE_WIDTH) := tag(TAG_SIZE-1 downto LOG2_PIPE_WIDTH);

        return res;
    end function;


    function TMP_trSelMask(selMask: std_logic_vector; queueContent, queueContent_NS: SchedulerInfoArray) return std_logic_vector is
        variable res: std_logic_vector(selMask'range) := (others => '0');
        variable si: SchedulerInfo;
    begin
        if isNonzero(selMask) = '0' then
            return res;
        end if;
        
        for i in selMask'range loop
            if selMask(i) = '1' then
                si := queueContent(i);
                exit;
            end if;
        end loop;

        for i in selMask'range loop
            if queueContent_NS(i).dynamic.renameIndex = si.dynamic.renameIndex and queueContent_NS(i).dynamic.full = si.dynamic.full then
                res(i) := '1';
                exit;
            end if;
        end loop;
       
        return res;
    end function;

begin
    newArr_T <= assignStaticPtr(newArr, S_firstFree);
    
        ch0 <= bool2std(selMask_TrNS = selMask_NS);
        ch1 <= bool2std(nFull = nFull_C);
        ch2 <= bool2std(dispatchDataNew_NS = dispatchDataNew_T);
    
        nFull_C <= i2slv(countOnes(fullMask_NS), SMALL_NUMBER_SIZE);
    
        nFullNext <=        sub(i2slv(countOnes(fullMask_NS), SMALL_NUMBER_SIZE), nOut) when slv2u(recoveryCounter) = 1
                      else  add(nFull, sub(nIn, nOut));

        nIn <= i2slv(countOnes(extractFullMask(newArr_T)), SMALL_NUMBER_SIZE) when prevSendingOK = '1' else (others => '0'); 
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when (isSent_NS and not sentKilled_NS) = '1' else (others => '0');
    
    MANAGEMENT: block
        signal trialMask, readyMaskLive, killMask, readyMaskAll, selMask: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        signal queueContentRR, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
        signal fma: ForwardingMatchesArray(0 to IQ_SIZE + PIPE_WIDTH -1) := (others => DEFAULT_FORWARDING_MATCHES);
        signal controlSigs: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    begin
    
        QUEUE_SYNCHRONOUS: process(clk)
        begin
            if rising_edge(clk) then        
                if not NONSHIFT then
                    queueContent <= queueContentNext;
                end if;
                
                    queueContent_NS <= queueContentNext_NS;
                    TMP_ageMatrix <= TMP_ageMatrixNext;
                        
                sentKilled <= sendingKilled;            
                isSent <= sends;
                isSent2 <= isSent;
                    
                    sentKilled_NS <= sendingKilled_NS;            
                    isSent_NS <= sends_NS;
                    isSent2_NS <= isSent_NS;
                
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

        fma <= findForwardingMatchesArray(queueContent, fni);
            fma_NS <= findForwardingMatchesArray(queueContent_NS, fni);
    
        controlSigs <= getControlSignals(queueContentUpdatedSel, events);               
    
        -- Vector signals
        killMask <= getKilledVec(controlSigs);
        trialMask <= getTrialVec(controlSigs);
        fullMask <= getFullVec(controlSigs);
        readyMaskAll <= getReadyVec(controlSigs);
        readyMaskLive <= getReadyLiveVec(controlSigs);        
        selMask <= getSelectedVec(controlSigs);
    
        -- Scalar signals
        anyReadyLive <= isNonzero(readyMaskLive);
        anyReadyFull <= isNonzero(readyMaskAll);
        sends <= anyReadyFull and nextAccepting;
        sendingKilled <= isNonzero(killMask and selMask);


            controlSigs_NS <= getControlSignals(queueContentUpdatedSel_NS, events);               
        
            -- Vector signals
            killMask_NS <= getKilledVec(controlSigs_NS);
            trialMask_NS <= getTrialVec(controlSigs_NS);
            fullMask_NS <= getFullVec(controlSigs_NS);
            readyMaskAll_NS <= getReadyVec(controlSigs_NS);
            readyMaskLive_NS <= getReadyLiveVec(controlSigs_NS);
            
            -- Scalar signals
            anyReadyLive_NS <= isNonzero(readyMaskLive_NS);
            anyReadyFull_NS <= isNonzero(readyMaskAll_NS);
            sends_NS <= anyReadyFull_NS and nextAccepting;
            sendingKilled_NS <= isNonzero(killMask_NS and selMask_NS);          
            
            
            -- TMP: find by tag where the op is corresponding to selection from shifting queue      
            selMask_TrNS <= --getSelectedVec(controlSigs_NS);
                            TMP_trSelMask(selMask, queueContent, queueContent_NS);
            selMask_NS <= TMP_getSelMask(readyMaskAll_NS, TMP_ageMatrix);
            
            selMatrix <= TMP_getSelMatrix(readyMaskAll_NS, TMP_ageMatrix);
            
        isEmpty <= not isNonzero(fullMask);
            isEmpty_NS <= not isNonzero(fullMask_NS);
    
        -- Content manipulation
        queueContentRR <= updateRenameIndex(queueContent);
        
        queueContentUpdated <= updateSchedulerArray(queueContentRR, fni, fma, false, false, DONT_MATCH1, FORWARDING_D, FORWARDING_D);
        queueContentUpdatedSel <= updateSchedulerArray(queueContentRR, fni, fma, false, true, DONT_MATCH1, FORWARDING, FORWARDING1);
    
        queueContentNext <= iqNext_N2(queueContentUpdated, newArr_T, prevSendingOK, sends, killMask, trialMask, selMask, readyRegFlags, 0);


            TMP_ageMatrixNext <= TMP_updateAgeMatrix(TMP_ageMatrix, TMP_insertionLocs, fullMask_NS);
            TMP_insertionLocs <= TMP_getNewLocs(fullMask_NS);

            queueContentRR_NS <= (queueContent_NS);

            queueContentUpdated_NS <= updateSchedulerArray(queueContentRR_NS, fni, fma_NS, false, false, DONT_MATCH1, FORWARDING_D, FORWARDING_D);
            queueContentUpdatedSel_NS <= updateSchedulerArray(queueContentRR_NS, fni, fma_NS, false, true, DONT_MATCH1, FORWARDING, FORWARDING1);
    
            queueContentNext_NS <= iqNext_NS(queueContentUpdated_NS, newArr_T, prevSendingOK, sends_NS, killMask_NS, trialMask_NS, selMask_NS, readyRegFlags, 0);
    
        selectedSlot <= prioSelect16(queueContentUpdatedSel, readyMaskAll);
            selectedSlot_NS <= prioSelect16(queueContentUpdatedSel_NS, selMask_NS);
    
    end block;

    dispatchDataNew_T <= getSchedEntrySlot(selectedSlot);
        dispatchDataNew_NS <= getSchedEntrySlot(selectedSlot_NS);


    WHEN_SH: if not NONSHIFT generate
        -- Output signals
        schedulerOut <= TMP_restoreState(sends, dispatchDataNew_T.ins, dispatchDataNew_T.state);

        acceptingOut <= not isNonzero(fullMask(4 to 7));
        acceptingMore <= not isNonzero(fullMask(0 to 7));
    
        outputSignals <=   (sending => sends,
                            cancelled => sentKilled,
                            ready => anyReadyLive,
                            empty => isEmpty);    
    end generate;

    WHEN_NSH: if NONSHIFT generate
        -- Output signals
        schedulerOut <= TMP_restoreState(sends_NS, dispatchDataNew_NS.ins, dispatchDataNew_NS.state);

        acceptingOut <= not isFull;
        acceptingMore <= not isAlmostFull;
    
        outputSignals <=   (sending => sends_NS,
                            cancelled => sentKilled_NS,
                            ready => anyReadyLive_NS,
                            empty => isEmpty_NS);
    end generate;

            ch3 <= bool2std(sends = sends_NS and sentKilled = sentKilled_NS and anyReadyLive = anyReadyLive_NS and isEmpty = isEmpty_NS);
            --ch4 <= bool2std();
    
    STATIC_PART_CONTROL: block
        type StaticMaskArray is array(0 to STATIC_ARRAY_SIZE-1) of std_logic_vector(0 to PIPE_WIDTH-1);
        type StaticInfoArray2D is array(integer range<>, integer range<>) of StaticInfo;
    
        signal S_tagArray, S_tagArrayNext: InsTagArray(0 to STATIC_ARRAY_SIZE-1) := (others => (others => '0'));
        signal S_fullMask, S_fullMaskNext, S_killMask: std_logic_vector(0 to STATIC_ARRAY_SIZE-1) := (others => '0');
        signal S_lqPtr, S_sqPtr, S_bqPtr, S_immHigh: WordArray(0 to STATIC_ARRAY_SIZE-1) := (others => (others => '0'));  
        signal S_lqWord, S_sqWord, S_bqWord, S_immHighWord: Word := (others => '0');

        signal staticMasks, staticMasksNext: StaticMaskArray := (others => (others => '0'));
        signal S_staticContent: StaticInfoArray2D(0 to STATIC_ARRAY_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => DEFAULT_STATIC_INFO));
 
        signal issuedTag, issuedTag1, issuedTag2: InsTag := (others => '0');

        signal S_firstFreePrev, S_issued, S_issued1, S_issued2, S_issued3: natural := 0;
        signal S_selectedTag: InsTag := (others => '0'); 
           
        function S_updateTagArray(arr: InsTagArray; ind: natural; newTag: InsTag) return InsTagArray is
            variable res: InsTagArray(arr'range) := arr;
        begin
            res(ind) := newTag;
            return res;
        end function;
 
        function S_updateFullMask(mask: std_logic_vector; prevSending: std_logic; indSet: natural; staticMasks: StaticMaskArray;
                                     events: EventState;   S_killMask: std_logic_vector) return std_logic_vector is
            variable res: std_logic_vector(mask'range) := mask;
        begin
            for i in 0 to STATIC_ARRAY_SIZE-1 loop
                if isNonzero(staticMasks(i)) = '0' then
                    res(i) := '0';
                end if;
            end loop;

            if prevSending = '1' then
                res(indSet) := '1';
            end if;         
            
            if events.lateEvent = '1' then
                res := (others => '0');
            elsif events.execEvent = '1' then
                res := res and not S_killMask;
            end if;
            
            return res;
        end function;       

        function S_clearStaticMaskRow(staticMasks: StaticMaskArray; ind, sub: natural) return StaticMaskArray is
            variable res: StaticMaskArray := staticMasks;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                if i > sub then
                    res(ind)(i) := '0';
                end if;
            end loop;
            
            return res;
        end function;

        function S_updateStaticMasks(staticMasks: StaticMaskArray;
                                     events: EventState; prevSending, isSent2: std_logic; S_issued2, S_firstFree: natural; issuedTag2: InsTag; S_killMask: std_logic_vector;
                                        newArr: SchedulerInfoArray) return StaticMaskArray is
            variable res: StaticMaskArray := staticMasks;
            constant tagLow: std_logic_vector(LOG2_PIPE_WIDTH-1 downto 0) := issuedTag2(LOG2_PIPE_WIDTH-1 downto 0); 
        begin
            if isSent2 = '1' then
                res(S_issued2)(slv2u(tagLow)) := '0';
            end if;
        
            if events.lateEvent = '1' then
                res := (others => (others => '0'));
            elsif events.execEvent = '1' then
                res := S_clearStaticMaskRow(res, S_issued2, slv2u(tagLow));
            elsif prevSending = '1' then
                res(S_firstFree) := extractFullMask(newArr);
            end if;
            
            return res;
        end function;
        
        function S_getKillMask(S_tagMask: InsTagArray; events: EventState) return std_logic_vector is
            variable res: std_logic_vector(0 to STATIC_ARRAY_SIZE-1) := (others => '0');
        begin
            for i in res'range loop
                res(i) := compareTagBefore(events.execCausing.tags.renameIndex, S_tagMask(i));
            end loop;
            return res;
        end function;

        function findFreeStaticSlots(fullMask: std_logic_vector) return IntArray is
            variable res: IntArray(0 to PIPE_WIDTH-1) := (others => 0);
            variable j: natural := 0; 
        begin
            for i in fullMask'range loop
                if fullMask(i) /= '1' then
                    res(j) := i;
                    if j = 3 then
                        exit;
                    else
                        j := j + 1;
                    end if;                    
                end if;
            end loop;
            
            return res;
        end function;

        function restoreStatic(si: SchedulerInfo; st: StaticInfo; tag: InsTag; lqWord, sqWord, bqWord, immHighWord: Word) return StaticInfo is
            variable res: StaticInfo := st;
            constant subIndV: std_logic_vector(1 downto 0) := si.dynamic.renameIndex(1 downto 0);
            constant subInd: natural := slv2u(subIndV);
            constant wordInd: natural := 8*(3-subInd); 
        begin
            res.bqPointerSeq := si.static.bqPointerSeq;
        
            res.sqPointer := sqWord(wordInd+7 downto wordInd);
            res.lqPointer := lqWord(wordInd+7 downto wordInd);
            res.bqPointer := bqWord(wordInd+7 downto wordInd);
            res.immValue(15 downto 8) := immHighWord(wordInd+7 downto wordInd);
            return res;
        end function;                   
    begin
        issuedTag <= selectedSlot.dynamic.renameIndex;
    
        S_tagArrayNext <= S_updateTagArray(S_tagArray, S_firstFree, newArr(0).dynamic.renameIndex);
        S_fullMaskNext <= S_updateFullMask(S_fullMask, prevSendingOK, S_firstFree, staticMasks, events, S_killMask);

        S_firstFree <= findFreeStaticSlots(S_fullMask)(0);

        S_killMask <= S_getKillMask(S_tagArray, events);
        
        staticMasksNext <= S_updateStaticMasks(staticMasks, events, prevSendingOK, isSent2, S_issued2, S_firstFree, issuedTag2, S_killMask, newArr);

        S_issued <= slv2u(selectedSlot.dynamic.staticPtr);
        
        S_selectedStatic <= S_staticContent(slv2u(selectedSlot.dynamic.staticPtr), slv2u(selectedSlot.dynamic.renameIndex(1 downto 0)));
        S_selectedTag <= S_tagArray(slv2u(selectedSlot.dynamic.staticPtr));
        
        S_lqWord <= S_lqPtr(slv2u(selectedSlot.dynamic.staticPtr));
        S_sqWord <= S_sqPtr(slv2u(selectedSlot.dynamic.staticPtr));
        S_bqWord <= S_bqPtr(slv2u(selectedSlot.dynamic.staticPtr));
        S_immHighWord <= S_immHigh(slv2u(selectedSlot.dynamic.staticPtr));
        
        
	    STATIC_QUEUE_SYNCHRONOUS: process(clk)
        begin
           if rising_edge(clk) then
               staticMasks <= staticMasksNext;
    
               if prevSendingOK = '1' then
                   S_staticContent(S_firstFree, 0) <= newArr(0).static;
                   S_staticContent(S_firstFree, 1) <= newArr(1).static;
                   S_staticContent(S_firstFree, 2) <= newArr(2).static;
                   S_staticContent(S_firstFree, 3) <= newArr(3).static;
                   
                   S_lqPtr(S_firstFree) <= newArr(0).static.lqPointer & newArr(1).static.lqPointer & newArr(2).static.lqPointer & newArr(3).static.lqPointer;
                   S_sqPtr(S_firstFree) <= newArr(0).static.sqPointer & newArr(1).static.sqPointer & newArr(2).static.sqPointer & newArr(3).static.sqPointer;
                   S_bqPtr(S_firstFree) <= newArr(0).static.bqPointer & newArr(1).static.bqPointer & newArr(2).static.bqPointer & newArr(3).static.bqPointer;
                   S_immHigh(S_firstFree) <= newArr(0).static.immValue(15 downto 8) & newArr(1).static.immValue(15 downto 8)
                                         & newArr(2).static.immValue(15 downto 8) & newArr(3).static.immValue(15 downto 8);
               end if;
    
               S_firstFreePrev <= S_firstFree;
                            
               S_tagArray <= S_tagArrayNext;
               S_fullMask <= S_fullMaskNext;
    
               S_issued1 <= S_issued;
               S_issued2 <= S_issued1;
               S_issued3 <= S_issued2;
               
               issuedTag1 <= issuedTag;
               issuedTag2 <= issuedTag1;    
           end if;
        end process;

        S_selectedStaticRestored <= restoreStatic(selectedSlot, S_selectedStatic, S_selectedTag, S_lqWord, S_sqWord, S_bqWord, S_immHighWord);
    end block;

end Behavioral;
