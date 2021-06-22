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



entity IssueQueue is
	generic(
		IQ_SIZE: natural := 8;
		IS_FP: boolean := false;
		ALT_INPUT: boolean := false;
		  USE_NEW_SIGS: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSendingOK: in std_logic;
		--newArr_N: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);
		
		  newArr_Alt: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		  newArrOut: out SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		nextAccepting: in std_logic;
		
		--lateEventSignal: in std_logic;
		--execEventSignal: in std_logic;
		--execCausing: in InstructionState;
		      events: in EventState;
		
		fni: in ForwardingInfo;
		waitingFM: in ForwardingMap;
		selectionFM: in ForwardingMap;
		readyRegFlags: in std_logic_vector(0 to 3*PIPE_WIDTH-1);

		
		acceptingMore: out std_logic;
		acceptingOut: out std_logic;
		
		queuesAccepting: in std_logic;
		
		--empty: out std_logic;
		
		--anyReady: out std_logic;
		--sentCancelled: out std_logic;		
		schedulerOut: out SchedulerEntrySlot;
		--sending: out std_logic;
		
		--anyReady_A: out std_logic;
        --sentCancelled_A: out std_logic;        
        --schedulerOut_A: out SchedulerEntrySlot;
        --sending_A: out std_logic;
        
            outputSignals: out IssueQueueSignals		
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
    constant QUEUE_SIZE_EXT: natural := IQ_SIZE + PIPE_WIDTH;

	signal fullMask, fullMaskNext, killMask, killMaskPrev, livingMask, readyMaskAll, readyMaskFull, readyMaskLive,
	               selMask, selMaskPrev: std_logic_vector(0 to IQ_SIZE-1) := (others=>'0');	


    signal inputStagePreRR, inputStageUpdated: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);


        signal fullMaskExt, fullMaskExtNext, killMaskExt, killMaskExtPrev, livingMaskExt, readyMaskAllExt, readyMaskFullExt, readyMaskLiveExt,
                   cancelledMaskExt, selMaskExt, selMaskExtPrev: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others=>'0');
                   
        signal livingMaskInput: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
               
        signal queueContentExt, queueContentExtNext, queueContentUpdatedExt, queueContentUpdatedSelExt: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
                                                                                                                                                       
	signal anyReadyAll, anyReadyFull, anyReadyLive, sends, sendingKilled, isSent, sentKilled, inputStageSending: std_logic := '0';
	signal dispatchDataNew: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

    signal fma: ForwardingMatchesArray(0 to IQ_SIZE-1) := (others => DEFAULT_FORWARDING_MATCHES);
   
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
    
    signal controlSigs, controlSigsPrev: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    
    function getControlSignals(content: SchedulerInfoArray; events: EventState) return SlotControlArray is
        variable res: SlotControlArray(content'range);
        
        variable readyFullVec, selectedVec: std_logic_vector(content'range) := (others => '0');
    begin
        for i in res'range loop        
            res(i).full := content(i).dynamic.full;
            res(i).issued := content(i).dynamic.issued;
            res(i).killed := killByTag(compareTagBefore(events.execCausing.tags.renameIndex, content(i).dynamic.renameIndex), events.execEvent, events.lateEvent);
            res(i).living := res(i).full and not res(i).killed;
            
            res(i).ready := not isNonzero(content(i).dynamic.missing(0 to 1)) and not content(i).dynamic.issued
                                and bool2std(i < IQ_SIZE);
            
            res(i).readyFull := res(i).ready and res(i).full;
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
begin

    INPUT_STAGE: block
        signal fmaInputStage: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);    
        signal inputStage, inputStageNext: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);          
        signal inputStageAny, inputStageLivingAny, inputReadingAny: std_logic := '0';
        signal selMaskInput, killMaskInput: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');        
    begin
        inputStage <= updateRR(inputStagePreRR, readyRegFlags); -- TODO: restoreRenameIndex also in Nonshift architecture when it's used!

        fmaInputStage <= findForwardingMatchesArray(inputStage, fni);
        inputStageUpdated <= updateSchedulerArray(inputStage, fni, fmaInputStage, waitingFM, false);                   
            
            -- TODO: use the fact that the have the same high tag part?
            killMaskInput <= getKillMask(inputStage, events.execCausing, events.execEvent, events.lateEvent);
            livingMaskInput <= extractFullMask(inputStage) and not killMaskInput;

        inputStageSending <= inputStageAny and queuesAccepting and not events.execEvent and not events.lateEvent;
        
        XYZ_NEW: if USE_NEW_SIGS generate
            selMaskInput <= selMaskExt(IQ_SIZE to IQ_SIZE + PIPE_WIDTH - 1);
        end generate;
        
        inputStageNext <= iqInputStageNext(inputStageUpdated, newArr, selMaskInput, prevSendingOK, inputStageSending, events.execEvent, events.lateEvent);
        inputStageAny <= isNonzero(extractFullMask(inputStage));

        INPUT_SYNCHRONOUS: process(clk)
        begin
            if rising_edge(clk) then
                inputStagePreRR <= inputStageNext;
            end if;
        end process;
    end block;
        
	QUEUE_SYNCHRONOUS: process(clk) 	
	begin
		if rising_edge(clk) then		
    		queueContent <= queueContentNext;
			
			selMaskPrev <= selMask;
			killMaskPrev <= killMask;
			sentKilled <= sendingKilled;
			
			isSent <= sends;			
		end if;
	end process;	

    controlSigs <= getControlSignals(queueContentUpdatedSel & inputStageUpdated, events);

    -- Vector signals
    killMask <= getKillMask(queueContent, events.execCausing, events.execEvent, events.lateEvent);
    fullMask <= extractFullMask(queueContent);
    livingMask <= fullMask and not killMask;

    readyMaskAll <= extractReadyMask(queueContentUpdatedSel); -- USED for selection
    readyMaskFull <= readyMaskAll and fullMask;	
    readyMaskLive <= readyMaskAll and livingMask;

        selMask <= getFirstOne(readyMaskFull);
        
        killMaskExt <= getKilledVec(controlSigs);
        fullMaskExt <= getFullVec(controlSigs);
        livingMaskExt <= getLivingVec(controlSigs);
        
        readyMaskAllExt <= getReadyVec(controlSigs);
        readyMaskFullExt <= getReadyFullVec(controlSigs);
        readyMaskLiveExt <= getReadyLiveVec(controlSigs);
        
        selMaskExt <= getSelectedVec(controlSigs);
    

    
            -- Scalar signals
            OLD_SIGS: if not USE_NEW_SIGS generate
                anyReadyLive <= isNonzero(readyMaskLive);
                anyReadyFull <= isNonzero(readyMaskFull);
            
                sends <= anyReadyFull and nextAccepting;
                sendingKilled <= isNonzero(killMask and selMask);
            end generate;

            NEW_SIGS: if USE_NEW_SIGS generate
                anyReadyLive <= isNonzero(readyMaskLiveExt);
                anyReadyFull <= isNonzero(readyMaskFullExt);
            
                sends <= anyReadyFull and nextAccepting;
                sendingKilled <= isNonzero(killMaskExt and selMaskExt);
            end generate;            

    queueContentNext <= iqContentNext(queueContentUpdated, inputStageUpdated, 
                                      killMask, selMask,
                                      livingMaskInput,                 
                                      sends, isSent,
                                      '0',
                                      inputStageSending
                                      );

    fma <= findForwardingMatchesArray(queueContent, fni);

    queueContentUpdated <= updateSchedulerArray(queueContent, fni, fma, waitingFM, false);
    queueContentUpdatedSel <= updateSchedulerArray(queueContent, fni, fma, selectionFM, false);


        queueContentUpdatedSelExt(0 to IQ_SIZE-1) <= queueContentUpdatedSel;
        queueContentUpdatedSelExt(IQ_SIZE to IQ_SIZE + PIPE_WIDTH-1) <= inputStageUpdated;

    OLD_ISSUE: if not USE_NEW_SIGS generate
        dispatchDataNew <= getSchedEntrySlot(prioSelect(queueContentUpdatedSel, readyMaskAll));
    end generate;
    
    NEW_ISSUE: if USE_NEW_SIGS generate
        dispatchDataNew <= getSchedEntrySlot(prioSelect(queueContentUpdatedSelExt, readyMaskAllExt));
    end generate;
    
    
	acceptingOut <= not fullMask(IQ_SIZE-PIPE_WIDTH);
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
