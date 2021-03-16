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
		  newArr_N: in SchedulerInfoArray(0 to PIPE_WIDTH-1);
		
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

		
		acceptingMore: out std_logic;
		acceptingOut: out std_logic;
		
		queuesAccepting: in std_logic;
		
		empty: out std_logic;
		
		anyReady: out std_logic;
		sentCancelled: out std_logic;		
		schedulerOut: out SchedulerEntrySlot;
		sending: out std_logic;
		
		anyReady_A: out std_logic;
        sentCancelled_A: out std_logic;        
        schedulerOut_A: out SchedulerEntrySlot;
        sending_A: out std_logic		
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
	signal fullMask, fullMaskNext, killMask, killMaskPrev, livingMask, readyMaskAll, readyMaskFull, readyMaskLive,
	               cancelledMask, selMask, selMaskPrev: std_logic_vector(0 to IQ_SIZE-1) := (others=>'0');	

    signal inputStagePreRR, inputStageUpdated: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
    signal queueContent, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerInfoArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);
                                                                                                                                                       
	signal anyReadyAll, anyReadyFull, anyReadyLive, sends, sends_A, sends_AN,
	            sendPossible, sendingKilled, sent, sent_A, isSent, isSent_A, sentKilled, sendingEmpty, sentEmpty, 
	            sentUnexpected, anyCancelled, anyCancelled_A, inputStageSending, inputStageMoving, acceptingForInputStage: std_logic := '0';
	signal dispatchDataNew: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

    signal fma: ForwardingMatchesArray(0 to IQ_SIZE-1) := (others => DEFAULT_FORWARDING_MATCHES);

--	-- Select item at first '1', or the last one if all zeros
--	function prioSelect(elems: SchedulerEntrySlotArray; selVec: std_logic_vector) return SchedulerEntrySlot is
--		variable ind, ind0, ind1: std_logic_vector(2 downto 0) := "000";
--		variable ch0, ch1: SchedulerEntrySlot;
--	begin
--		if selVec(0 to 3) = "0000" then
--			ind(2) := '1';
--		else
--			ind(2) := '0';
--		end if;
		
--		if selVec(0) = '1' then
--			ch0 := elems(0);
--		elsif selVec(1) = '1' then
--			ch0 := elems(1);
--		elsif selVec(2) = '1' then
--			ch0 := elems(2);
--		else
--			ch0 := elems(3);
--		end if;

--		if selVec(4) = '1' then
--			ch1 := elems(4);
--		elsif selVec(5) = '1' then
--			ch1 := elems(5);
--		elsif selVec(6) = '1' then
--			ch1 := elems(6);
--		else
--			ch1 := elems(7);
--		end if;

--		if ind(2) = '0' then
--			return ch0;
--		else
--			return ch1;
--		end if;
--	end function;


	function prioSelect(elems: SchedulerInfoArray; selVec: std_logic_vector) return SchedulerInfo is
		variable ind, ind0, ind1: std_logic_vector(2 downto 0) := "000";
		variable ch0, ch1: SchedulerInfo;
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
        res.ins.controlInfo.newEvent := '0';
        res.ins.controlInfo.hasInterrupt := '0';
	   return res;
	end function;

    function iqInputStageNext(content, newContent: SchedulerInfoArray; prevSending, isSending, execEventSignal, lateEventSignal: std_logic) return SchedulerInfoArray is
       variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content;
    begin
       if execEventSignal = '1' or lateEventSignal = '1' then
           for i in 0 to PIPE_WIDTH-1 loop
               res(i).dynamic.full := '0';
           end loop; 	       
       elsif prevSending = '1' then
           res := newContent;	       
       elsif isSending = '1' then -- Clearing everything - sent to main queue
           for i in 0 to PIPE_WIDTH-1 loop
               res(i).dynamic.full := '0';
           end loop;    
       end if;
       
       return res;
    end function;

    function updateRR(newContent: SchedulerInfoArray; rr: std_logic_vector) return SchedulerInfoArray is
       variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := newContent;
       variable rrf: std_logic_vector(0 to 2) := (others=>'0');      	   
    begin
       for i in 0 to PIPE_WIDTH-1 loop
           rrf := rr(3*i to 3*i + 2);                                              
           res(i).dynamic.missing := res(i).dynamic.missing and not rrf;	       
       end loop;   
       return res;
    end function;

    function restoreRenameIndexSch(content: SchedulerInfoArray) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content;
    begin
        for i in 1 to PIPE_WIDTH-1 loop
            res(i).dynamic.renameIndex := clearTagLow(res(0).dynamic.renameIndex) or i2slv(i, TAG_SIZE);
        end loop;
    
        return res;
    end function;

        function getKillMask(content: SchedulerInfoArray; fullMask: std_logic_vector;
                                    causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
        return std_logic_vector is
            variable res: std_logic_vector(0 to content'length-1);
        begin
            for i in 0 to content'length-1 loop
                res(i) := killByTag(compareTagBefore(causing.tags.renameIndex, content(i).dynamic.renameIndex),
                                            execEventSig, lateEventSig);-- and fullMask(i);
            end loop;
            return res;
        end function;
   
    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';
begin

    INPUT_STAGE: block
        signal fmaInputStage: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);    
        signal inputStage, inputStageNext: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);          
        signal inputStageAny, inputStageLivingAny, inputReadingAny: std_logic := '0';        
    begin
        inputStage <= updateRR(restoreRenameIndexSch(inputStagePreRR), readyRegFlags); -- TODO: restoreRenameIndex also in Nonshift architecture when it's used!

        fmaInputStage <= findForwardingMatchesArray(inputStage, fni);
        inputStageUpdated <= updateSchedulerArray(inputStage, fni, fmaInputStage, waitingFM, true, false);                   
  
        inputStageSending <= inputStageAny and queuesAccepting and not execEventSignal and not lateEventSignal;

        inputStageNext <= iqInputStageNext(inputStageUpdated, newArr_N, prevSendingOK, inputStageSending, execEventSignal, lateEventSignal);
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
			sentEmpty <= sendingEmpty;
			
			isSent <= sends;			
			isSent_A <= sends_A;			
		end if;
	end process;	


	killMask <= getKillMask(queueContent, fullMask, execCausing, execEventSignal, lateEventSignal);
 	fullMask <= extractFullMask(queueContent);
	livingMask <= fullMask and not killMask;

    readyMaskAll <= extractReadyMaskNew(queueContentUpdatedSel);
	readyMaskFull <= readyMaskAll and fullMask;	
	readyMaskLive <= readyMaskAll and livingMask;
	
	anyReadyLive <= isNonzero(readyMaskLive);
    anyReadyFull <= isNonzero(readyMaskFull);
    anyReadyAll <= isNonzero(readyMaskAll);
    	

    sendingKilled <= isNonzero(killMask and selMask);
    cancelledMask <= (killMaskPrev and selMaskPrev);
    anyCancelled <= sentKilled;
    anyCancelled_A <= sentKilled or sentEmpty;
 

	sends <= anyReadyFull and nextAccepting;
    sends_A <= anyReadyAll and nextAccepting;

    dispatchDataNew <= getSchedEntrySlot(prioSelect(queueContentUpdatedSel, readyMaskAll));
  
    selMask <= getFirstOne(readyMaskFull);
    sent <= isSent;
    sent_A <= isSent_A;

    sendingEmpty <= (anyReadyAll and not anyReadyFull) and nextAccepting;


    queueContentNext <= iqContentNext_N(queueContentUpdated, inputStageUpdated, 
                                          killMask, selMask,                  
                                          sends, sent,
                                          sentUnexpected,
                                          inputStageSending
                                          );

    fma <= findForwardingMatchesArray(queueContent, fni);

    queueContentUpdated <= updateSchedulerArray(queueContent, fni, fma, waitingFM, true, false);
    queueContentUpdatedSel <= updateSchedulerArray(queueContent, fni, fma, selectionFM, false, false);

	acceptingForInputStage <= not fullMask(IQ_SIZE-PIPE_WIDTH);
	acceptingOut <= not fullMask(IQ_SIZE-PIPE_WIDTH);-- and not inputStageAny;	               
	acceptingMore <= not fullMask(IQ_SIZE-2*PIPE_WIDTH);	
	
	anyReady <= anyReadyLive; -- OUTPUT
	schedulerOut <= clearOutput(clearDestIfEmpty(TMP_restoreState(sends, dispatchDataNew.ins, dispatchDataNew.state)));
	sending <= sends;
    sentCancelled <= anyCancelled;
    
	    anyReady_A <= anyReadyAll;
        sending_A <= sends_A;
        sentCancelled_A <= anyCancelled_A;
                    
    -- CAREFUL! If queue becomes noncollapsing, we'll need to see all full bits! 
    empty <= not fullMask(0); -- not isNonzero(fullMask);
    
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
