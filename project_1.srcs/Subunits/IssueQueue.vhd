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
		FORWARDING_D: ForwardingModeArray := (0 => (-100, false)) -- Can be used with 1 cycle delay
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
              
    signal queueContentExt_T: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
    signal fullMaskExt_T: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');

	signal anyReadyFull_T, anyReadyLive_T, sends_T, sendingKilled_T, isSent, isSent2, sentKilled_T, isEmpty: std_logic := '0';
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
	signal dispatchDataNew_T: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

    signal S_firstFree: natural := 0;       
    signal S_selectedStatic, S_selectedStaticRestored: StaticInfo := DEFAULT_STATIC_INFO;
                                  
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

begin
    newArr_T <= assignStaticPtr(newArr, S_firstFree);
    
    MANAGEMENT: block
        signal trialMask, readyMaskLiveExt_T, killMaskExt_T, readyMaskAllExt_T, selMaskExt_T: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        signal queueContentExtRR_T, queueContentExtNext_T, queueContentUpdatedExt_T, queueContentUpdatedSelExt_T: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
        signal fmaExt_T: ForwardingMatchesArray(0 to IQ_SIZE + PIPE_WIDTH -1) := (others => DEFAULT_FORWARDING_MATCHES);
        signal controlSigs_T: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    begin
    
        QUEUE_SYNCHRONOUS: process(clk)
        begin
            if rising_edge(clk) then        
                queueContentExt_T <= queueContentExtNext_T;
    
                sentKilled_T <= sendingKilled_T;            
                isSent <= sends_T;
                isSent2 <= isSent;
    
            end if;
        end process;

        fmaExt_T <= findForwardingMatchesArray(queueContentExt_T, fni);
    
        controlSigs_T <= getControlSignals(queueContentUpdatedSelExt_T, events);               
    
        -- Vector signals
        killMaskExt_T <= getKilledVec(controlSigs_T);
        trialMask <= getTrialVec(controlSigs_T);
        fullMaskExt_T <= getFullVec(controlSigs_T);
        readyMaskAllExt_T <= getReadyVec(controlSigs_T);
        readyMaskLiveExt_T <= getReadyLiveVec(controlSigs_T);        
        selMaskExt_T <= getSelectedVec(controlSigs_T);
    
        -- Scalar signals
        anyReadyLive_T <= isNonzero(readyMaskLiveExt_T);
        anyReadyFull_T <= isNonzero(readyMaskAllExt_T);
        sends_T <= anyReadyFull_T and nextAccepting;
        sendingKilled_T <= isNonzero(killMaskExt_T and selMaskExt_T);
    
        isEmpty <= not isNonzero(fullMaskExt_T);
    
        -- Content manipulation
        queueContentExtRR_T <= updateRenameIndex(queueContentExt_T);
        
        queueContentUpdatedExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, false, DONT_MATCH1, FORWARDING_D, FORWARDING_D);
        queueContentUpdatedSelExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, true, DONT_MATCH1, FORWARDING, FORWARDING1);
    
        queueContentExtNext_T <= iqNext_N2(queueContentUpdatedExt_T, newArr_T, prevSendingOK, sends_T, killMaskExt_T, trialMask, selMaskExt_T, readyRegFlags, 0);
    
        selectedSlot <= prioSelect16(queueContentUpdatedSelExt_T, readyMaskAllExt_T);
    
    end block;
    
    dispatchDataNew_T <= getSchedEntrySlot(selectedSlot);

    -- Output signals
	schedulerOut <= TMP_restoreState(sends_T, dispatchDataNew_T.ins, dispatchDataNew_T.state);

	acceptingOut <= not isNonzero(fullMaskExt_T(4 to 7));
	acceptingMore <= not isNonzero(fullMaskExt_T(0 to 7));

    outputSignals <=   (sending => sends_T,
                        cancelled => sentKilled_T,
                        ready => anyReadyLive_T,
                        empty => isEmpty);


    
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
