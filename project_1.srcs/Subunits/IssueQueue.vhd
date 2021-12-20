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
    constant STATIC_ARRAY_SIZE: natural := 16;

    signal fullMaskExt, fullMaskExtNext, killMaskExt, trialMask, trialMask_T, livingMaskExt, readyMaskAllExt, readyMaskFullExt, readyMaskLiveExt, readyMaskLiveExt_T,
           cancelledMaskExt, selMaskExt, selMaskExtPrev, fullMaskExt_T, killMaskExt_T, readyMaskAllExt_T, selMaskExt_T: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others=>'0');
               
    signal queueContentExt, queueContentExtNext, queueContentUpdatedExt, queueContentUpdatedSelExt,
           queueContentExt_T, queueContentExtRR_T, queueContentExtNext_T, queueContentExtNext_N, queueContentUpdatedExt_T, queueContentUpdatedSelExt_T
        : SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
    
        signal staticContent: StaticInfoArray(0 to STATIC_ARRAY_SIZE-1) := (others => DEFAULT_STATIC_INFO); 
        signal staticContentFull: std_logic_vector(0 to STATIC_ARRAY_SIZE-1) := (others => '0');
        signal staticContentIndices: IntArray(0 to STATIC_ARRAY_SIZE-1) := (others => 0);
        
        signal newStaticSlots: IntArray(0 to PIPE_WIDTH-1) := (0, 1, 2, 3);
        
        signal issuedStaticPtr, lastLivingStaticPtr: SmallNumber := (others => '0');
        signal issuedVirtualPtr, lastLivingVirtualPtr, maxVirtualPtr: natural := 0;
        
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
            for i in 0 to PIPE_WIDTH-1 loop
                if newMask(i) = '1' then
                    res(newSlots(i)) := highest;
                    highest := highest + 1;
                end if;
            end loop;
            
            return res;
        end function;
                                  
	signal anyReadyFull_T, anyReadyFullMain, anyReadyLive_T, sends, sends_T, sendingKilled_T, isSent, sentKilled, sentKilled_T: std_logic := '0';
	signal dispatchDataNew, dispatchDataNew_T: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

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
                    res(i).trial := res(i).trial_T;
                
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
    
    function updateRegStatus(content: SchedulerInfoArray; rrf: std_logic_vector) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(content'range) := content;
        variable earlyStage: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1);
    begin
        earlyStage := updateRR(earlyStage, rrf);
        res(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1) := earlyStage;
        return res;
    end function;

    signal rrfStored: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
begin

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
        
	QUEUE_SYNCHRONOUS: process(clk) 	
	begin
		if rising_edge(clk) then		
            queueContentExt_T <= queueContentExtNext_T;

			sentKilled_T <= sendingKilled_T;			
			isSent <= sends;
			
			
			     if false then
			     
			     elsif prevSendingOK = '1' then
			         staticContentFull <= updateFullStaticSlots(staticContentFull, newStaticSlots, extractFullMask(newArr));
			         staticContent <= updateStaticContent(staticContent, newStaticSlots, extractFullMask(newArr), newArr);
			         staticContentIndices <= updateStaticIndices(staticContentIndices, newStaticSlots, extractFullMask(newArr), newArr,
			                                                     '0', 0, 0);
			     end if;
		end if;
	end process;

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
    queueContentExtRR_T <= updateRegStatus(queueContentExt_T, rrfStored);
    queueContentUpdatedExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, false, DONT_MATCH1, FORWARDING_D, FORWARDING_D);
    queueContentUpdatedSelExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, true, DONT_MATCH1, FORWARDING, FORWARDING1);

    queueContentExtNext_T <= iqNext_N2(queueContentUpdatedExt_T, newArr, prevSendingOK, sends_T, killMaskExt_T, trialMask, selMaskExt_T, 0);
    queueContentExtNext_N <= iqNext_N(queueContentUpdatedExt_T, newArr, prevSendingOK, sends_T, killMaskExt_T, selMaskExt_T, 0);

    -- Output signals
    dispatchDataNew_T <= getSchedEntrySlot(prioSelect16(queueContentUpdatedSelExt_T, readyMaskAllExt_T));

	acceptingOut <= not isNonzero(fullMaskExt_T(4 to 7));               
	acceptingMore <= not isNonzero(fullMaskExt_T(0 to 7));
	schedulerOut <= TMP_restoreState(sends_T, dispatchDataNew_T.ins, dispatchDataNew_T.state);
	
    outputSignals <=            
                      (sending => sends_T,
                        cancelled => sentKilled_T,
                        ready => anyReadyLive_T,
                        empty => not isNonzero(fullMaskExt_T));

end Behavioral;
