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
		--newArr_N: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);
		
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
   
    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';
begin

    INPUT_STAGE: block
        signal fmaInputStage: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);    
        signal inputStage, inputStageNext: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);          
        signal inputStageAny, inputStageLivingAny, inputReadingAny: std_logic := '0';        
    begin
        inputStage <= updateRR(inputStagePreRR, readyRegFlags); -- TODO: restoreRenameIndex also in Nonshift architecture when it's used!

        fmaInputStage <= findForwardingMatchesArray(inputStage, fni);
        inputStageUpdated <= updateSchedulerArray(inputStage, fni, fmaInputStage, waitingFM, true, false);                   
  
        inputStageSending <= inputStageAny and queuesAccepting and not execEventSignal and not lateEventSignal;

        inputStageNext <= iqInputStageNext(inputStageUpdated, newArr, prevSendingOK, inputStageSending, execEventSignal, lateEventSignal);
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

    readyMaskAll <= extractReadyMask(queueContentUpdatedSel);
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


    queueContentNext <= iqContentNext(queueContentUpdated, inputStageUpdated, 
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
	
	schedulerOut <= TMP_restoreState(sends, dispatchDataNew.ins, dispatchDataNew.state);
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
