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
		FORWARDING: ForwardingModeArray := (0 => (-100, false));  -- Can be used immediately
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

	signal fullMask, fullMaskNext, killMask, killMaskPrev, livingMask, readyMaskAll, readyMaskFull, readyMaskLive,
	               selMask, selMaskPrev: std_logic_vector(0 to IQ_SIZE-1) := (others=>'0');	


    signal inputStagePreRR, inputStageUpdated, inputStageUpdatedSel: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);


        signal fullMaskExt, fullMaskExtNext, killMaskExt, killMaskExtPrev, livingMaskExt, readyMaskAllExt, readyMaskFullExt, readyMaskLiveExt,
                   cancelledMaskExt, selMaskExt, selMaskExtPrev,
                   fullMaskExt_T, killMaskExt_T, readyMaskAllExt_T, selMaskExt_T
                   
                   : std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others=>'0');
                   
        signal livingMaskInput, selMaskInput: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
               
        signal queueContentExt, queueContentExtNext, queueContentUpdatedExt, queueContentUpdatedSelExt: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);

        signal queueContentExt_T, queueContentExtRR_T, queueContentExtNext_T,   qc0, qc1, qc2, qc3, queueContentUpdatedExt_T, queueContentUpdatedSelExt_T
        : SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);

                                                                                                                                                       
	signal anyReadyAll, anyReadyFull, anyReadyFullMain, anyReadyLive, sends, sendsMainQueue, sendsInputStage, sendingKilled, isSent, isSentMainQueue, isSentMainQueue_T, sentKilled,
	           acceptingMain, inputStageSending: std_logic := '0';
	signal dispatchDataNew, dispatchDataNew_T: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

    signal fma: ForwardingMatchesArray(0 to IQ_SIZE-1) := (others => DEFAULT_FORWARDING_MATCHES);
    signal fmaExt, fmaExt_T: ForwardingMatchesArray(0 to IQ_SIZE + PIPE_WIDTH -1) := (others => DEFAULT_FORWARDING_MATCHES);
   
    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';
    
    type SlotControl is record
        full: std_logic;
        issued: std_logic;
        killed: std_logic;
        living: std_logic;
        ready: std_logic;
        readyFull: std_logic;
        readyLiving: std_logic;
        selected: std_logic;
    end record;
    
    type SlotControlArray is array(natural range <>) of SlotControl;
    
    signal controlSigs, controlSigsPrev, controlSigs_T: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    
    function getControlSignals(content: SchedulerInfoArray; events: EventState) return SlotControlArray is
        variable res: SlotControlArray(content'range);
        
        variable readyFullVec, selectedVec: std_logic_vector(content'range) := (others => '0');
    begin
        for i in res'range loop        
            res(i).full := content(i).dynamic.full;
            res(i).issued := content(i).dynamic.issued;
            res(i).killed := killByTag(compareTagBefore(events.execCausing.tags.renameIndex, content(i).dynamic.renameIndex), events.execEvent, events.lateEvent);
            res(i).living := res(i).full and not res(i).killed;
            
            res(i).ready := not isNonzero(content(i).dynamic.missing(0 to 1)) and content(i).dynamic.active;
            
            res(i).readyFull := res(i).ready;-- and res(i).full;
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
    
    function getSentMainQueue(content: SchedulerInfoArray) return std_logic is
    begin
        for i in content'range loop
            if (content(i).dynamic.full and content(i).dynamic.issued) = '1' then
                return '1';
            end if;
        end loop;
        
        return '0';
    end function;
    
    function updateRegStatus(content: SchedulerInfoArray; rrf: std_logic_vector) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(content'range) := content;
        variable earlyStage: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1);
    begin
        earlyStage := updateRR(earlyStage, rrf);
        res(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1) := earlyStage;
        return res;
    end function;
    
    signal fmaInputStage: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);    
    signal inputStage, inputStage_T, inputStageNext: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);          
    signal inputStageAny, inputStageLivingAny, inputReadingAny: std_logic := '0';
    signal killMaskInput, TST_rm0, TST_rm1: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    
        signal rrfStored: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
begin
    fma <= findForwardingMatchesArray(queueContent, fni);
    fmaInputStage <= findForwardingMatchesArray(inputStage, fni);    

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
    		queueContent <= queueContentNext;
        		queueContentExt_T <= queueContentExtNext_T;
            inputStagePreRR <= inputStageNext;
			
			selMaskPrev <= selMask;
			killMaskPrev <= killMask;
			sentKilled <= sendingKilled;
			
			isSent <= sends;
			isSentMainQueue_T <= sendsMainQueue or (sends and inputStageSending); -- if sending not from Main part but sends and is moved to Main, it will hold		
		end if;
	end process;	


    controlSigs <= getControlSignals(queueContentUpdatedSel & inputStageUpdatedSel, events);               
    controlSigs_T <= getControlSignals(queueContentUpdatedSelExt_T, events);               

    -- Vector signals
        killMask <= killMaskExt(0 to IQ_SIZE-1);
        fullMask <= fullMaskExt(0 to IQ_SIZE-1);
        livingMask <= livingMaskExt(0 to IQ_SIZE-1);
        readyMaskAll <= readyMaskAllExt(0 to IQ_SIZE-1);
        readyMaskFull <= readyMaskFullExt(0 to IQ_SIZE-1);                   	
        readyMaskLive <= readyMaskLiveExt(0 to IQ_SIZE-1);
        selMask <= selMaskExt(0 to IQ_SIZE-1);
        
        killMaskExt <= getKilledVec(controlSigs);
            killMaskExt_T <= getKilledVec(controlSigs_T);
        fullMaskExt <= getFullVec(controlSigs);
            fullMaskExt_T <= getFullVec(controlSigs_T);
        livingMaskExt <= getLivingVec(controlSigs);
        
        readyMaskAllExt <= getReadyVec(controlSigs);
            readyMaskAllExt_T <= getReadyVec(controlSigs_T);
        readyMaskFullExt <= getReadyFullVec(controlSigs);
        readyMaskLiveExt <= getReadyLiveVec(controlSigs);
        
        selMaskExt <= getSelectedVec(controlSigs);
            selMaskExt_T <= getSelectedVec(controlSigs_T);
   
        killMaskInput <= getKillMask(inputStage, events.execCausing, events.execEvent, events.lateEvent);
        livingMaskInput <= extractFullMask(inputStage) and not killMaskInput;
        selMaskInput <= selMaskExt(IQ_SIZE to IQ_SIZE + PIPE_WIDTH - 1);
    
    -- Scalar signals
        sendsMainQueue <= anyReadyFullMain and nextAccepting;
        sendsInputStage <= anyReadyFull and not anyReadyFullMain and nextAccepting; -- to Issue
    
        inputStageSending <= inputStageAny and acceptingMain and not events.execEvent and not events.lateEvent; -- to main queue
    
        isSentMainQueue <= getSentMainQueue(queueContent);
    
        anyReadyLive <= isNonzero(readyMaskLiveExt);
        anyReadyFull <= isNonzero(readyMaskFullExt);
        anyReadyFullMain <= isNonzero(readyMaskFull);
    
        sends <= anyReadyFull and nextAccepting;
        sendingKilled <= isNonzero(killMaskExt and selMaskExt);

        inputStageAny <= isNonzero(extractFullMask(inputStage));

    -- Content manipulation
    queueContentUpdated <= updateSchedulerArray(queueContent, fni, fma, false, false, FORWARDING_D);
    queueContentUpdatedSel <= updateSchedulerArray(queueContent, fni, fma, false, true, FORWARDING);
    
    queueContentNext <= iqContentNext(queueContentUpdated, inputStageUpdated,
                                      killMask, selMask,
                                      livingMaskInput, selMaskInput,    
                                      sendsMainQueue, -- this can be from whole queue because selMask points to slot if it is to be moved
                                      isSentMainQueue,
                                        sendsInputStage,
                                        '0',
                                      '0',
                                      inputStageSending
                                      );
        
        
        queueContentExtRR_T <= updateRegStatus(queueContentExt_T, rrfStored);
        queueContentUpdatedExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, false, FORWARDING_D);
        queueContentUpdatedSelExt_T <= updateSchedulerArray(queueContentExtRR_T, fni, fmaExt_T, false, true, FORWARDING);       
        queueContentExtNext_T <= iqNext_T(queueContentUpdatedExt_T, newArr, prevSendingOK, sends, killMaskExt_T, selMaskExt_T  , 0);

                qc0 <= iqNext_T(queueContentExt_T, newArr, prevSendingOK, sends, killMaskExt_T, selMaskExt_T  , 1);
                qc1 <= iqNext_T(queueContentExt_T, newArr, prevSendingOK, sends, killMaskExt_T, selMaskExt_T  , 2);
                qc2 <= iqNext_T(queueContentExt_T, newArr, prevSendingOK, sends, killMaskExt_T, selMaskExt_T  , 3);
                qc3 <= iqNext_T(queueContentExt_T, newArr, prevSendingOK, sends, killMaskExt_T, selMaskExt_T  , 4);
        
    inputStage <= updateRR(inputStagePreRR, rrfStored);
        
    inputStageUpdated <= updateSchedulerArray(inputStage, fni, fmaInputStage, false, false, FORWARDING_D);
    inputStageUpdatedSel <= updateSchedulerArray(inputStage, fni, fmaInputStage, false, true, FORWARDING); 

    inputStageNext <= iqInputStageNext(inputStageUpdated, newArr, selMaskInput, livingMaskInput, prevSendingOK, inputStageSending, sendsInputStage, events.execEvent, events.lateEvent);


    queueContentExt <= queueContent & inputStage;
    fmaExt <= fma & fmaInputStage;

    queueContentUpdatedSelExt(0 to IQ_SIZE-1) <= queueContentUpdatedSel;
    queueContentUpdatedSelExt(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1) <= inputStageUpdatedSel;

    -- Output signals
    dispatchDataNew <= getSchedEntrySlot(prioSelect16(queueContentUpdatedSelExt, readyMaskAllExt));
        dispatchDataNew_T <= getSchedEntrySlot(prioSelect16(queueContentUpdatedSelExt_T, readyMaskAllExt_T));
        ch0 <= bool2std(dispatchDataNew = dispatchDataNew_T);
        
	acceptingMain <= not fullMask(IQ_SIZE-PIPE_WIDTH);

	acceptingOut <= acceptingMain;
	acceptingMore <= not fullMask(IQ_SIZE-2*PIPE_WIDTH);

	schedulerOut <= TMP_restoreState(sends, dispatchDataNew.ins, dispatchDataNew.state);
	
    outputSignals <= (sending => sends,
                      cancelled => sentKilled,
                      ready => anyReadyLive,
                      empty => not fullMask(0));
    
    VIEW: if VIEW_ON generate
        use work.Viewing.all;
        
        signal prevReadyMask, issuedMask, remainMask: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');      
        signal flowSig: IntArray(0 to IQ_SIZE-1) := (others => -1);

        subtype ReadyVec is std_logic_vector(0 to 2);
        type WakeupTable is array(0 to IQ_SIZE-1) of ReadyVec;
        signal wakeup, wakeupSel: WakeupTable := (others => (others => '0'));
        
        
        signal queueText: InsStringArray(0 to IQ_SIZE-1);
        signal inputStageText: InsStringArray(0 to PIPE_WIDTH-1);
        signal iqText: InsStringArray(0 to IQ_SIZE-1) := (others => (others => ' '));            
    begin
    
        --fullMaskNext <= extractFullMask(queueContentNext_T);
    
        -- Monitor:
        -- fma - forward matches array
        
        -- compare readyMask with previous one, remember that some may have shifted and some not 
        -- 
        process (clk)
            variable tmpFull, newFull: natural := 0;
            variable flow: IntArray(0 to IQ_SIZE-1) := (others => -1);
            variable thisReadyVec, prevReadyVec: std_logic_vector(0 to 2) := (others => '0');          
        begin
            if rising_edge(clk) then
                tmpFull := countOnes(fullMask);
                newFull := countOnes(fullMaskNext); 
            
                prevReadyMask <= readyMaskFull;
                
                if std2bool(isNonzero(issuedMask)) then -- something is removed
                    tmpFull := tmpFull - 1;
                end if;

                flow := (others => -1);
                
                for i in 0 to IQ_SIZE-1 loop
                    if remainMask(i) = '1' then
                        flow(i) := 0; -- remaining
                    else
                        flow(i) := 1; -- moved
                    end if;
                    
                    if i >= tmpFull then
                        flow(i) := 2; -- new
                    end if;
                    if i >= newFull then
                        flow(i) := -1; -- empty
                    end if;
                    
                    if killMask(i) = '1' then
                        flow(i) := -2; -- killed
                    end if;
                end loop;                
                
                flowSig <= flow;
                
                
                -- Look at wakeups and check where they come from and for which register
                for i in 0 to IQ_SIZE-1 loop
                    for j in 0 to 2 loop
                        if wakeup(i)(j) = '1' then
                            -- fma(i).
                            -- ...
                        end if;
                        
                        if wakeupSel(i)(j) = '1' then
                            -- 
                            -- ...
                        end if;                        
                    end loop;
                end loop;
                
            end if;
        end process;
        
        
        WAKEUP_VECS: for i in 0 to IQ_SIZE-1 generate
            --wakeup(i) <= not queueContentUpdated_T(i).state.missing and queueContent_T(i).state.missing when fullMask(i) = '1' else (others => '0');
            --wakeupSel(i) <= not queueContentUpdatedSel_T(i).state.missing and queueContent_T(i).state.missing when fullMask(i) = '1' else (others => '0');        
        end generate;

        --iqText <= getInsStringArray(queueContent_T, args);
        --queueText <= getInsStringArray(queueContent_T);
        --inputStageText <= getInsStringArray(inputStage);
    end generate;
	   	   
end Behavioral;
