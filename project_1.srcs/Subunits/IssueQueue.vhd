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
		ALT_INPUT: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSendingOK: in std_logic;
		newArr: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		  newArr_Alt: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		  newArrOut: out SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		nextAccepting: in std_logic;
		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;
		fni: in ForwardingInfo;
		waitingFM: in ForwardingMap;
		selectionFM: in ForwardingMap; 
		readyRegFlags: in std_logic_vector(0 to 3*PIPE_WIDTH-1);

		sentCancelled: out std_logic;
		
		acceptingMore: out std_logic;
		acceptingOut: out std_logic;
		
		queuesAccepting: in std_logic;
		
		empty: out std_logic;
		
		anyReady: out std_logic;
		schedulerOut: out SchedulerEntrySlot;
		sending: out std_logic
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
	signal queueData: InstructionStateArray(0 to IQ_SIZE-1)  := (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, fullMaskNext, killMask, killMaskPrev, livingMask, readyMask, readyMaskLive,
	                   stayMask, selMask, selMaskPrev, cancelledMask, issuedMask, remainMask: std_logic_vector(0 to IQ_SIZE-1) := (others=>'0');	

	signal queueContent, queueContent_T, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerEntrySlotArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
	signal newContent, newContentRR, newSchedData, inputStagePreRR, inputStage, inputStageUpdated, inputStageNext:
	                                                                                   SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);

	signal anyReadyFull, anyReadyLive, sends, sendPossible, sendingKilled, sent, sentKilled, sentUnexpected, inputStageAny, inputStageLivingAny,
	               inputReadingAny, anyCancelled, inputStageSending, inputStageMoving, acceptingForInputStage: std_logic := '0';
	signal dispatchDataNew: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

    signal fma: ForwardingMatchesArray(0 to IQ_SIZE-1) := (others => DEFAULT_FORWARDING_MATCHES);
    signal fmaInputStage: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);


        function T_clear(content: SchedulerEntrySlotArray) return SchedulerEntrySlotArray is
        	variable res: SchedulerEntrySlotArray(0 to IQ_SIZE-1) := content;
        begin
            for i in 0 to res'right loop
                            --        res(i).ins.physicalArgSpec := DEFAULT_ARG_SPEC;
                     res(i).ins.physicalArgSpec.args(0) := (others => '0');
                end loop;            
            return res;
        end function;


	-- Select item at first '1', or the last one if all zeros
	function prioSelect(elems: SchedulerEntrySlotArray; selVec: std_logic_vector) return SchedulerEntrySlot is
		variable ind, ind0, ind1: std_logic_vector(2 downto 0) := "000";
		variable ch0, ch1: SchedulerEntrySlot;
	begin
		if selVec(0 to 3) = "0000" then
			ind(2) := '1';
		else
			ind(2) := '0';
		end if;
		
		if selVec(0) = '1' then
			ch0 := elems(0);
		elsif selVec(1) = '1' then
			ch0 := elems(1);
		elsif selVec(2) = '1' then
			ch0 := elems(2);
		else
			ch0 := elems(3);
		end if;

		if selVec(4) = '1' then
			ch1 := elems(4);
		elsif selVec(5) = '1' then
			ch1 := elems(5);
		elsif selVec(6) = '1' then
			ch1 := elems(6);
		else
			ch1 := elems(7);
		end if;

		if ind(2) = '0' then
			return ch0;
		else
			return ch1;
		end if;
	end function;


	function clearOutput(elem: SchedulerEntrySlot) return SchedulerEntrySlot is
		variable res: SchedulerEntrySlot := elem;
    begin
	    -- Clear unused fields       
        if CLEAR_DEBUG_INFO then
            res.ins := clearAbstractInfo(res.ins);
        end if;

        res.ins.controlInfo.completed := '0';
        res.ins.controlInfo.completed2 := '0';
    
        res.ins.controlInfo.newEvent := '0';
        res.ins.controlInfo.hasInterrupt := '0';
    
	   return res;
	end function;
	
	function TMP_setUntil(selVec: std_logic_vector; nextAccepting: std_logic) return std_logic_vector is
		variable res: std_logic_vector(0 to selVec'length-1) := (others => '0');
	begin
		for i in res'range loop
			if (selVec(i) and nextAccepting) = '1' then
				exit;
			else
				res(i) := '1';
			end if;
		end loop;
		return res;
	end function;

	function TMP_getIssuedMask(elems: SchedulerEntrySlotArray) return std_logic_vector is
        variable res: std_logic_vector(0 to elems'length-1) := (others => '0');
	begin
		for i in 0 to elems'length-1 loop
		    res(i) := elems(i).state.issued;
		end loop;
		return res;
    end function;

	function iqInputStageNext(content, newContent: SchedulerEntrySlotArray; prevSending, isSending, execEventSignal, lateEventSignal: std_logic) return SchedulerEntrySlotArray is
	   variable res: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := content;
	begin
	   if execEventSignal = '1' or lateEventSignal = '1' then
	       for i in 0 to PIPE_WIDTH-1 loop
               res(i).full := '0';
           end loop; 	       
	   elsif prevSending = '1' then
	       res := newContent;	       
	   elsif isSending = '1' then -- Clearing everything - sent to main queue
	       for i in 0 to PIPE_WIDTH-1 loop
	           res(i).full := '0';
	       end loop;    
	   end if;
	   
	   return res;
	end function;
	
	function updateRR(newContent: SchedulerEntrySlotArray; rr: std_logic_vector) return SchedulerEntrySlotArray is
	   variable res: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := newContent;
       variable rrf: std_logic_vector(0 to 2) := (others=>'0');      	   
	begin
	   for i in 0 to PIPE_WIDTH-1 loop
	       rrf := rr(3*i to 3*i + 2);                                              
           res(i).state.missing := res(i).state.missing and not rrf;	       
	   end loop;   
	   return res;
	end function;
    	
begin

	fmaInputStage <= findForwardingMatchesArray(inputStage, fni);    
    inputStage <= updateRR(restoreRenameIndexSch(inputStagePreRR), readyRegFlags); -- TODO: restoreRenameIndex also in Nonshift architecture when it's used!

        inputStageUpdated <= updateSchedulerArray(inputStage, fni, fmaInputStage, waitingFM, true, false) when not ALT_INPUT
                         else newArr_Alt;
        newArrOut <= inputStageUpdated;
    
    inputStageSending <= inputStageAny and queuesAccepting and not execEventSignal and not lateEventSignal;
    
    inputStageNext <= iqInputStageNext(inputStageUpdated, newContent, prevSendingOK, inputStageSending, execEventSignal, lateEventSignal);
    inputReadingAny <= prevSendingOK and isNonzero(extractFullMask(newArr));
    inputStageAny <= isNonzero(extractFullMask(inputStage));
    inputStageLivingAny <= inputStageAny and not execEventSignal and not lateEventSignal;
        
        cancelledMask <= (killMaskPrev and selMaskPrev);
        anyCancelled <= isNonzero(cancelledMask);
        
	QUEUE_SYNCHRONOUS: process(clk) 	
	begin
		if rising_edge(clk) then		
			queueContent <= queueContentNext;
			
			selMaskPrev <= selMask;
			killMaskPrev <= killMask;
			sentKilled <= sendingKilled;
			
            inputStagePreRR <= inputStageNext;
			
		end if;
	end process;	

	livingMask <= fullMask and not killMask;

	fullMask <= extractFullMask(queueContent);
    queueData <= extractData(queueContent);

    fullMaskNext <= extractFullMask(queueContentNext);

	sends <= anyReadyLive and nextAccepting when not TMP_PARAM_SIMPLIFY_ISSUE -- CHECK: can we use full instead of living?
	   else  anyReadyFull and nextAccepting;
	sendPossible <= anyReadyFull and nextAccepting; -- Includes ops that would send but are killed
	
	dispatchDataNew <= clearOutput(clearDestIfEmpty(prioSelect(queueContentUpdatedSel, readyMask), not sends));
	--stayMask <= TMP_setUntil(readyMask, nextAccepting);

    newContent <= newArr;
            
            selMask <= getFirstOne(readyMask);
            remainMask <= TMP_setUntil(issuedMask, '1'); 
            issuedMask <= TMP_getIssuedMask(queueContent);
                sent <= isNonzero(issuedMask);
                sendingKilled <= isNonzero(killMask and selMask);
            
            queueContentNext <= iqContentNext(queueContentUpdated, inputStageUpdated,
                                              remainMask, fullMask, livingMask, selMask, issuedMask,                                             
                                              sends, sent,
                                              sentUnexpected,
                                              inputStageSending
                                              );
					
	-- TODO: below could be optimized because some code is shared (comparators!)
	fma <= findForwardingMatchesArray(queueContent, fni);
	
	
	       queueContent_t <= T_clear(queueContent);
	
	queueContentUpdated <= updateSchedulerArray(queueContent, fni, fma, waitingFM, true, false);
	queueContentUpdatedSel <= updateSchedulerArray(queueContent_T, fni, fma, selectionFM, false, false);

	readyMask <= extractReadyMaskNew(queueContentUpdatedSel) and fullMask;	
	readyMaskLive <= readyMask and livingMask;

	
	killMask <= --getKillMask(queueData, fullMask, execCausing, execEventSignal, lateEventSignal);
	            getKillMask(queueContent, fullMask, execCausing, execEventSignal, lateEventSignal);
	
	acceptingForInputStage <= not fullMask(IQ_SIZE-PIPE_WIDTH);
	acceptingOut <= not fullMask(IQ_SIZE-PIPE_WIDTH);-- and not inputStageAny;
	               
	acceptingMore <= not fullMask(IQ_SIZE-2*PIPE_WIDTH);
	
	anyReadyLive <= isNonzero(readyMaskLive);
	anyReadyFull <= isNonzero(readyMask);
	
	anyReady <= anyReadyLive; -- OUTPUT
	
	schedulerOut <= --(sends, dispatchDataNew.ins, dispatchDataNew.state);
	                TMP_restoreState(sends, dispatchDataNew.ins, dispatchDataNew.state);
	sending <= sends;
    sentCancelled <= sentKilled when not TMP_PARAM_SIMPLIFY_ISSUE
                else anyCancelled;
    
    -- CAREFUL! If queue becomes noncollapsing, we'll need to see all full bits! 
    empty <= not fullMask(0); -- not isNonzero(fullMask);
    
    VIEW: if VIEW_ON generate
        use work.Viewing.all;
        
        signal prevReadyMask: std_logic_vector(0 to IQ_SIZE-1) := (others => '0');      
        signal flowSig: IntArray(0 to IQ_SIZE-1) := (others => -1);

        subtype ReadyVec is std_logic_vector(0 to 2);
        type WakeupTable is array(0 to IQ_SIZE-1) of ReadyVec;
        signal wakeup, wakeupSel: WakeupTable := (others => (others => '0'));
        
        
        signal queueText: InsStringArray(0 to IQ_SIZE-1);
        signal inputStageText: InsStringArray(0 to PIPE_WIDTH-1);
        signal iqText: InsStringArray(0 to IQ_SIZE-1) := (others => (others => ' '));            
    begin
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
            
                prevReadyMask <= readyMask;
                
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
            wakeup(i) <= not queueContentUpdated(i).state.missing and queueContent(i).state.missing when fullMask(i) = '1' else (others => '0');
            wakeupSel(i) <= not queueContentUpdatedSel(i).state.missing and queueContent(i).state.missing when fullMask(i) = '1' else (others => '0');        
        end generate;

        iqText <= getInsStringArray(queueContent, args);        
        queueText <= getInsStringArray(queueContent);
        inputStageText <= getInsStringArray(inputStage);
    end generate;
	   	   
end Behavioral;
