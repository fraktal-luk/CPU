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
    
    signal fullMaskExt, fullMaskExtNext, killMaskExt, trialMask, trialMask_T, livingMaskExt, readyMaskAllExt, readyMaskFullExt, readyMaskLiveExt, readyMaskLiveExt_T,
           cancelledMaskExt, selMaskExt, selMaskExtPrev, fullMaskExt_T, killMaskExt_T, readyMaskAllExt_T, selMaskExt_T: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others=>'0');
               
    signal queueContentExt, queueContentExtNext, queueContentUpdatedExt, queueContentUpdatedSelExt,
           queueContentExt_T, queueContentExtRR_T, queueContentExtNext_T, queueContentExtNext_N, queueContentUpdatedExt_T, queueContentUpdatedSelExt_T
        : SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    
    signal issuedTag, issuedTag1, issuedTag2: InsTag := (others => '0');
    
        signal staticContent: StaticInfoArray(0 to STATIC_ARRAY_SIZE-1) := (others => DEFAULT_STATIC_INFO); 
        signal staticContentFull: std_logic_vector(0 to STATIC_ARRAY_SIZE-1) := (others => '0');
        signal staticContentIndices: IntArray(0 to STATIC_ARRAY_SIZE-1) := (others => 0);
        signal staticRenameIndices: InsTagArray(0 to STATIC_ARRAY_SIZE-1) := (others => (others => '0'));
        
        signal newStaticSlots: IntArray(0 to PIPE_WIDTH-1) := (0, 1, 2, 3);
        
        signal issuedStaticPtr, lastLivingStaticPtr: SmallNumber := (others => '0');
        signal issuedVirtualPtr, lastLivingVirtualPtr, maxVirtualPtr: natural := 0;
        
        signal S_tagArray, S_tagArrayNext: InsTagArray(0 to STATIC_ARRAY_SIZE-1) := (others => (others => '0'));
        signal S_fullMask, S_fullMaskNext, S_killMask: std_logic_vector(0 to STATIC_ARRAY_SIZE-1) := (others => '0');
        
        signal S_firstFree, S_firstFreePrev, S_issued, S_issued1, S_issued2, S_issued3: natural := 0;
        signal S_firstDynamic, S_lastDynamic: natural := 0;
        signal S_firstTag, S_lastTag, S_selectedTag: InsTag := (others => '0');
        
        signal S_selectedStatic: StaticInfo := DEFAULT_STATIC_INFO;
        
        type StaticMaskArray is array(0 to STATIC_ARRAY_SIZE-1) of std_logic_vector(0 to PIPE_WIDTH-1);
        
        signal staticMasks, staticMasksNext: StaticMaskArray := (others => (others => '0'));
      
        attribute ram_style: string;
        
                type StaticInfoArray2D is array(integer range<>, integer range<>) of StaticInfo;
                signal S_staticContent: StaticInfoArray2D(0 to STATIC_ARRAY_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => DEFAULT_STATIC_INFO)); 

                signal S_lqPtr, S_sqPtr, S_bqPtr, S_immHigh: WordArray(0 to STATIC_ARRAY_SIZE-1) := (others => (others => '0'));
                
                signal S_lqWord, S_sqWord, S_bqWord, S_immHighWord: Word := (others => '0');

        --attribute ram_style of S_staticContent: signal is "distributed";

        
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
        
        
        function S_findTag(arr: InsTagArray; tag: InsTag) return natural is
        begin
            for i in 0 to arr'length-1 loop
                if arr(i) = tag then
                    return i;
                end if;
            end loop;
            
            return 0;
        end function;
        
        function S_getKillMask(S_tagMask: InsTagArray; events: EventState) return std_logic_vector is
            variable res: std_logic_vector(0 to STATIC_ARRAY_SIZE-1) := (others => '0');
        begin
            for i in res'range loop
                res(i) := compareTagBefore(events.execCausing.tags.renameIndex, S_tagMask(i));
            end loop;
            return res;
        end function;
        
        
        
        function findFirstFull(fullMask: std_logic_vector) return natural is
        begin
            for i in fullMask'range loop
                if fullMask(i) = '1' then
                   return i;
                end if;
            end loop;
            
            return 0;
        end function;

        function findLastFull(fullMask: std_logic_vector) return natural is
        begin
            for i in fullMask'reverse_range loop
                if fullMask(i) = '1' then
                   return i;
                end if;
            end loop;
            
            return 0;
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
        
        function updateFullStaticSlots(fullMask: std_logic_vector; newSlots: IntArray; newMask: std_logic_vector) return std_logic_vector is
            variable res: std_logic_vector(fullMask'range) := fullMask;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                if newMask(i) = '1' then
                    res(newSlots(i)) := '1';
                end if;
            end loop;
            
            return res;
        end function;

        function updateStaticContent(content: StaticInfoArray; newSlots: IntArray; newMask: std_logic_vector; newData: SchedulerInfoArray) return StaticInfoArray is
            variable res: StaticInfoArray(content'range) := content;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                if newMask(i) = '1' then
                    res(newSlots(i)) := newData(i).static;
                end if;
            end loop;
            
            return res;
        end function;        

        function updateStaticIndices(content: IntArray; newSlots: IntArray; newMask: std_logic_vector; newData: SchedulerInfoArray;
                                      issue: std_logic; issueIndex: natural; maxIndex: natural) return IntArray is
            variable res: IntArray(content'range) := content;
            variable highest: natural := maxIndex;
        begin
            --for
            
            
        
            for i in 0 to PIPE_WIDTH-1 loop
                if newMask(i) = '1' then
                    res(newSlots(i)) := highest;
                    highest := highest + 1;
                end if;
            end loop;
            
            return res;
        end function;
                                  
	signal anyReadyFull_T, anyReadyFullMain, anyReadyLive_T, sends, sends_T, sendingKilled_T, isSent, isSent2, sentKilled, sentKilled_T: std_logic := '0';
	signal dispatchDataNew, dispatchDataNew_T, dispatchDataNew_T2: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

    signal fmaExt_T: ForwardingMatchesArray(0 to IQ_SIZE + PIPE_WIDTH -1) := (others => DEFAULT_FORWARDING_MATCHES);
   
    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';
    
    type SlotControl is record
        full: std_logic;
        issued: std_logic;
        killed: std_logic;
            killed_T: std_logic;
        trial: std_logic;
            trial_T: std_logic;
        trialUpdated: std_logic;
        living: std_logic;
        ready: std_logic;
        readyFull: std_logic;
        readyLiving: std_logic;
        selected: std_logic;
    end record;
    
    type SlotControlArray is array(natural range <>) of SlotControl;
    
    signal controlSigs, controlSigsPrev, controlSigs_T: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    
    function compareIndBefore(tagA, tagB: SmallNumber; PTR_SIZE: natural) return std_logic is
        variable tC: SmallNumber := (others => '0');
    begin
        tC := sub(tagA, tagB);
        return tC(PTR_SIZE-1);
    end function;    

    function getControlSignals(content: SchedulerInfoArray; events: EventState) return SlotControlArray is
        variable res: SlotControlArray(content'range);        
        variable readyFullVec, selectedVec: std_logic_vector(content'range) := (others => '0');
    begin
        for i in res'range loop        
            res(i).full := content(i).dynamic.full;
            res(i).issued := content(i).dynamic.issued;
            
            res(i).trial := killByTag(compareTagBefore(events.preExecCausing.tags.renameIndex, content(i).dynamic.renameIndex), '1', '0');
               res(i).trial_T := killByTag(compareIndBefore(events.preExecCausing.tags.bqPointerSeq, content(i).static.bqPointerSeq, 6) , '1', '0'); -- TODO: temp value of PTR_SIZE!
            if false then -- Use bqPointerSeq to flush IQ
               res(i).trial := res(i).trial_T;
            end if;
                
            res(i).trialUpdated := content(i).dynamic.trial;
            
            res(i).killed := (res(i).trialUpdated and events.execEvent) or events.lateEvent;

            res(i).living := res(i).full and not res(i).killed;

            res(i).ready := not isNonzero(content(i).dynamic.missing(0 to 1)) and content(i).dynamic.active;            
            res(i).readyFull := res(i).ready;
            res(i).readyLiving := res(i).ready and res(i).living;

            readyFullVec(i) := res(i).readyFull;
            
        end loop;
        
        selectedVec := getFirstOne(readyFullVec);
        
        for i in res'range loop
            res(i).selected := selectedVec(i);
        end loop;
        
        return res;
    end function;

    function getFullVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).full;
        end loop;
        return res;
    end function;

    function getLivingVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).living;
        end loop;
        return res;
    end function;

    function getKilledVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).killed;
        end loop;
        return res;
    end function;

        function getKilledVec_T(arr: SlotControlArray) return std_logic_vector is
            variable res: std_logic_vector(arr'range) := (others => '0');
        begin
            for i in res'range loop
                res(i) := arr(i).killed_T;
            end loop;
            return res;
        end function;

    function getTrialVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).trial;
        end loop;
        return res;
    end function;

        function getTrialVec_T(arr: SlotControlArray) return std_logic_vector is
            variable res: std_logic_vector(arr'range) := (others => '0');
        begin
            for i in res'range loop
                res(i) := arr(i).trial_T;
            end loop;
            return res;
        end function;

    function getTrialUpdatedVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).trialUpdated;
        end loop;
        return res;
    end function;
    
    function getReadyVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).ready;
        end loop;
        return res;
    end function;

    function getReadyFullVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).readyFull;
        end loop;
        return res;
    end function;
    
    function getReadyLiveVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).readyLiving;
        end loop;
        return res;
    end function;    
    
    function getSelectedVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).selected;
        end loop;
        return res;
    end function;
    
    function updateRegStatus(content: SchedulerInfoArray; rrf: std_logic_vector; firstFree: natural) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(content'range) := content;
        variable earlyStage: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1);
    begin
        earlyStage := updateRR(earlyStage, rrf);
        
        for i in 0 to PIPE_WIDTH-1 loop
        --    earlyStage(i).dynamic.staticPtr := i2slv(firstFree, SMALL_NUMBER_SIZE);
        end loop;
        
        res(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1) := earlyStage;
        return res;
    end function;

    signal rrfStored: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
    
    function assignStaticPtr(arr: SchedulerInfoArray; p: natural) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(arr'range) := arr;
    begin
        for i in res'range loop
            res(i).dynamic.staticPtr := i2slv(p, SMALL_NUMBER_SIZE);
        end loop;
        return res;
    end function;
    
    function mergeWithStatic(si: SchedulerInfo; st: StaticInfo; tag: InsTag; lqWord, sqWord, bqWord, immHighWord: Word) return SchedulerInfo is
        variable res: SchedulerInfo := si;
        constant subInd: natural := slv2u(si.dynamic.renameIndex(LOG2_PIPE_WIDTH-1 downto 0));
        constant wordInd: natural := 8*subInd; 
    begin
        res.static := st;
            res.static.bqPointerSeq := si.static.bqPointerSeq;
        res.dynamic.renameIndex(TAG_SIZE-1 downto LOG2_PIPE_WIDTH) := tag(TAG_SIZE-1 downto LOG2_PIPE_WIDTH);
        
            --res.static.sqPointer := sqWord(wordInd+7 downto wordInd);
            --res.static.lqPointer := lqWord(wordInd+7 downto wordInd);
            --res.static.bqPointer := bqWord(wordInd+7 downto wordInd);
            --res.static.immValue(15 downto 8) := immHighWord(wordInd+7 downto wordInd);
        return res;
    end function;
    
    function mergeWithStatic2(si: SchedulerInfo; st: StaticInfo; tag: InsTag; lqWord, sqWord, bqWord, immHighWord: Word) return SchedulerInfo is
        variable res: SchedulerInfo := si;
        constant subIndV: std_logic_vector(1 downto 0) := si.dynamic.renameIndex(1 downto 0);
        constant subInd: natural := slv2u(subIndV);
        constant wordInd: natural := 8*(3-subInd); 
    begin
        res.static := st;
            res.static.bqPointerSeq := si.static.bqPointerSeq;
        res.dynamic.renameIndex(TAG_SIZE-1 downto LOG2_PIPE_WIDTH) := tag(TAG_SIZE-1 downto LOG2_PIPE_WIDTH);
        
            res.static.sqPointer := sqWord(wordInd+7 downto wordInd);
            res.static.lqPointer := lqWord(wordInd+7 downto wordInd);
            res.static.bqPointer := bqWord(wordInd+7 downto wordInd);
            res.static.immValue(15 downto 8) := immHighWord(wordInd+7 downto wordInd);
        return res;
    end function;
begin
        newArr_T <= assignStaticPtr(newArr, S_firstFree);

            ch0 <= bool2std(trialMask = trialMask_T);

    fmaExt_T <= findForwardingMatchesArray(queueContentExt_T, fni);

    INPUT_SYNCHRONOUS: process(clk)
        variable rm: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
    begin
        if rising_edge(clk) then
            rm := (newArr(0).dynamic.full, newArr(0).dynamic.full, newArr(0).dynamic.full,
                   newArr(1).dynamic.full, newArr(1).dynamic.full, newArr(1).dynamic.full,
                   newArr(2).dynamic.full, newArr(2).dynamic.full, newArr(2).dynamic.full,
                   newArr(3).dynamic.full, newArr(3).dynamic.full, newArr(3).dynamic.full
                   );
            if prevSendingOK = '1' then
                rrfStored <= readyRegFlags and rm;
            else
                rrfStored <= (others => '0');
            end if;
        end if;
    end process;
  
        S_firstTag <= queueContentExt_T(S_firstDynamic).dynamic.renameIndex;
        S_lastTag  <= queueContentExt_T(S_lastDynamic).dynamic.renameIndex;
        S_tagArrayNext <= S_updateTagArray(S_tagArray, S_firstFree, newArr(0).dynamic.renameIndex);
        S_fullMaskNext <= S_updateFullMask(S_fullMask, prevSendingOK, S_firstFree, staticMasks, events, S_killMask);

        S_firstFree <= findFreeStaticSlots(S_fullMask)(0);

        S_firstDynamic <= findFirstFull(fullMaskExt_T);
        S_lastDynamic <= findLastFull(fullMaskExt_T);


            S_killMask <= S_getKillMask(S_tagArray, events);

	QUEUE_SYNCHRONOUS: process(clk)
	begin
		if rising_edge(clk) then		
            queueContentExt_T <= queueContentExtNext_T;

			sentKilled_T <= sendingKilled_T;			
			isSent <= sends_T;
			isSent2 <= isSent;
			
			
			S_firstFreePrev <= S_firstFree;
			     --if isSent2 = '1' then
			        -- staticMasks(S_issued2)() <= '0';
			         
			     --end if;
			 
			 staticMasks <= staticMasksNext;
			     
			     if events.lateEvent = '1' then
			         staticContentFull <= (others => '0');
			         
			         
			         --  staticMasks <= (others => (others => '0'));
			     elsif events.execEvent = '1' then
			         
			          -- static masks cleared based on comparison with causing renameIndex
			          
			         -- staticMasks <= S_clearStaticMaskRow(staticMasks, S_issued2, events.execCausing.tags.renameIndex);
			           
			     elsif prevSendingOK = '1' then
			         staticContentFull <= updateFullStaticSlots(staticContentFull, newStaticSlots, extractFullMask(newArr));
			         staticContent <= updateStaticContent(staticContent, newStaticSlots, extractFullMask(newArr), newArr);
			         staticContentIndices <= updateStaticIndices(staticContentIndices, newStaticSlots, extractFullMask(newArr), newArr,
			                                                     '0', 0, 0);
			                                                     
			         --   staticMasks(S_firstFree) <= extractFullMask(newArr);
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

			     -- 
			     
			     --if isSent = '1' or prevSendingOK = '1' then
			     
			         S_tagArray <= S_tagArrayNext;
			         S_fullMask <= S_fullMaskNext;
			     --end if;
			     S_issued1 <= S_issued;
			     S_issued2 <= S_issued1;
			     S_issued3 <= S_issued2;
			     
			     issuedTag1 <= issuedTag;
			     issuedTag2 <= issuedTag1;
		end if;
	end process;

        staticMasksNext <= S_updateStaticMasks(staticMasks, events, prevSendingOK, isSent2, S_issued2, S_firstFree, issuedTag2, S_killMask, newArr);

        S_issued <= slv2u(selectedSlot.dynamic.staticPtr);
        issuedTag <= selectedSlot.dynamic.renameIndex;

            newStaticSlots <= findFreeStaticSlots(staticContentFull);

    controlSigs_T <= getControlSignals(queueContentUpdatedSelExt_T, events);               

    -- Vector signals
    killMaskExt_T <= getKilledVec(controlSigs_T);
    trialMask <= getTrialVec(controlSigs_T);
        trialMask_T <= getTrialVec_T(controlSigs_T);
    fullMaskExt_T <= getFullVec(controlSigs_T);
    readyMaskAllExt_T <= getReadyVec(controlSigs_T);
    readyMaskLiveExt_T <= getReadyLiveVec(controlSigs_T);        
    selMaskExt_T <= getSelectedVec(controlSigs_T);

    -- Scalar signals
    anyReadyLive_T <= isNonzero(readyMaskLiveExt_T);
    anyReadyFull_T <= isNonzero(readyMaskAllExt_T);
    sends_T <= anyReadyFull_T and nextAccepting;
    sendingKilled_T <= isNonzero(killMaskExt_T and selMaskExt_T);

    -- Content manipulation
    queueContentExtRR_T <= updateRegStatus(queueContentExt_T, rrfStored, S_firstFreePrev);
    queueContentUpdatedExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, false, DONT_MATCH1, FORWARDING_D, FORWARDING_D);
    queueContentUpdatedSelExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, true, DONT_MATCH1, FORWARDING, FORWARDING1);

    queueContentExtNext_T <= iqNext_N2(queueContentUpdatedExt_T, newArr_T, prevSendingOK, sends_T, killMaskExt_T, trialMask, selMaskExt_T, 0);
    queueContentExtNext_N <= iqNext_N(queueContentUpdatedExt_T, newArr_T, prevSendingOK, sends_T, killMaskExt_T, selMaskExt_T, 0);

        selectedSlot <= prioSelect16(queueContentUpdatedSelExt_T, readyMaskAllExt_T);
        
        S_selectedStatic <= S_staticContent(slv2u(selectedSlot.dynamic.staticPtr), slv2u(selectedSlot.dynamic.renameIndex(1 downto 0)));
        S_selectedTag <= S_tagArray(slv2u(selectedSlot.dynamic.staticPtr));
        
        S_lqWord <= S_lqPtr(slv2u(selectedSlot.dynamic.staticPtr));
        S_sqWord <= S_sqPtr(slv2u(selectedSlot.dynamic.staticPtr));
        S_bqWord <= S_bqPtr(slv2u(selectedSlot.dynamic.staticPtr));
        S_immHighWord <= S_immHigh(slv2u(selectedSlot.dynamic.staticPtr));
        
    -- Output signals
    dispatchDataNew_T <= getSchedEntrySlot(selectedSlot);
        dispatchDataNew_T2 <= getSchedEntrySlot(mergeWithStatic2(selectedSlot, S_selectedStatic, S_selectedTag, S_lqWord, S_sqWord, S_bqWord, S_immHighWord));

        ch1 <= bool2std(mergeWithStatic(selectedSlot, S_selectedStatic, S_selectedTag, S_lqWord, S_sqWord, S_bqWord, S_immHighWord) = 
                        mergeWithStatic2(selectedSlot, S_selectedStatic, S_selectedTag, S_lqWord, S_sqWord, S_bqWord, S_immHighWord));

	acceptingOut <= not isNonzero(fullMaskExt_T(4 to 7));               
	acceptingMore <= not isNonzero(fullMaskExt_T(0 to 7));
	schedulerOut <= TMP_restoreState(sends_T, dispatchDataNew_T.ins, dispatchDataNew_T.state);
	
    outputSignals <=            
                      (sending => sends_T,
                        cancelled => sentKilled_T,
                        ready => anyReadyLive_T,
                        empty => not isNonzero(fullMaskExt_T));

end Behavioral;
