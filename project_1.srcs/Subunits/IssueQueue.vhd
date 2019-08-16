----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    20:07:12 05/05/2016 
-- Design Name: 
-- Module Name:    SubunitIQBuffer - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
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
		IQ_SIZE: natural := 8
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSendingOK: in std_logic;
		newArr: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		nextAccepting: in std_logic;
		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;
		fni: ForwardingInfo;
		waitingFM: ForwardingMap;
		selectionFM: ForwardingMap; 
		readyRegFlags: in std_logic_vector(0 to 3*PIPE_WIDTH-1);
		
		sentCancelled: out std_logic;
		
		acceptingMore: out std_logic;
		acceptingOut: out std_logic;
		
		anyReady: out std_logic;
		schedulerOut: out SchedulerEntrySlot;
		sending: out std_logic
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
	signal queueData: InstructionStateArray(0 to IQ_SIZE-1)  := (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, fullMaskNext, killMask, livingMask, readyMask, readyMaskLive, stayMask, selMask, issuedMask, remainMask: std_logic_vector(0 to IQ_SIZE-1) := (others=>'0');	

	signal queueContent, queueContentNext, queueContent_N, queueContentNext_N: SchedulerEntrySlotArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
	signal queueContentUpdated, queueContentUpdatedSel: SchedulerEntrySlotArray(0 to IQ_SIZE-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
	signal newContent, newSchedData: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
				
	signal anyReadyFull, anyReadyLive, sends, sendPossible, sendingKilled, sentKilled: std_logic := '0';
	signal dispatchDataNew: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

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
	
	function TMP_clearDestIfEmpty(elem: SchedulerEntrySlot; sends: std_logic) return SchedulerEntrySlot is
		variable res: SchedulerEntrySlot := elem;
	begin
		if sends = '0' then
			res.ins.physicalArgSpec.dest := (others => '0');
		end if;
		
	    -- Clear unused fields
        res.ins.bits := (others => '0');
        res.ins.result := (others => '0');
        res.ins.target := (others => '0');        

        res.ins.controlInfo.completed := '0';
        res.ins.controlInfo.completed2 := '0';
        res.ins.ip := (others => '0');
    
        res.ins.controlInfo.newEvent := '0';
        res.ins.controlInfo.hasInterrupt := '0';
        res.ins.controlInfo.exceptionCode := (others => '0');		
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

			signal ch0, ch1, ch2: std_logic := '0';
begin

	QUEUE_SYNCHRONOUS: process(clk) 	
	begin
		if rising_edge(clk) then		
			queueContent <= queueContentNext;
			     queueContent_N <= queueContentNext_N;
			     issuedMask <= selMask;
			     sentKilled <= sendingKilled;
		end if;
	end process;	

	livingMask <= fullMask and not killMask;

	fullMask <= extractFullMask(queueContent);
    queueData <= extractData(queueContent);

	sends <= anyReadyLive and nextAccepting;
	sendPossible <= anyReadyFull and nextAccepting; -- Includes ops that would send but are killed
	
	dispatchDataNew <= TMP_clearDestIfEmpty(prioSelect(queueContentUpdatedSel, readyMask), sends);
	stayMask <= TMP_setUntil(readyMask, nextAccepting);

    newContent <= newArr;

	queueContentNext <= iqContentNext(queueContentUpdated, newContent,
									  stayMask, fullMask, livingMask,
									  sendPossible, sends,
									  prevSendingOK);
            
            
            selMask <= getFirstOne(readyMask);
            remainMask <= TMP_setUntil(issuedMask, '1'); 
            
                sendingKilled <= isNonzero(killMask and selMask);
            
            queueContentNext_N <= iqContentNext_N(queueContentUpdated, newContent,
                                              remainMask, fullMask, livingMask, selMask, issuedMask,
                                              
                                              sendPossible, '1', -- TEMP: sent = '1'
                                              prevSendingOK);
					
	-- TODO: below could be optimized because some code is shared (comparators!)
	queueContentUpdated <= --updateForWaitingArrayFNI(queueContent, readyRegFlags, fni);
	                       updateSchedulerArray(queueContent, readyRegFlags, fni, waitingFM, true);
	queueContentUpdatedSel <= --updateForSelectionArrayFNI(queueContent, readyRegFlags, fni);
	                       updateSchedulerArray(queueContent, readyRegFlags, fni, selectionFM, false);

	readyMask <= extractReadyMaskNew(queueContentUpdatedSel) and fullMask;	
	readyMaskLive <= readyMask and livingMask;

	
	killMask <= getKillMask(queueData, fullMask, execCausing, execEventSignal, lateEventSignal); 
	acceptingOut <= --not isNonzero(fullMask(IQ_SIZE-PIPE_WIDTH to IQ_SIZE-1));
	                   not fullMask(IQ_SIZE-PIPE_WIDTH); -- Equivalent and much better because in collapsing queue mask is continuous!
	acceptingMore <= --not isNonzero(fullMask(IQ_SIZE-2*PIPE_WIDTH to IQ_SIZE-PIPE_WIDTH-1));
	                   not fullMask(IQ_SIZE-2*PIPE_WIDTH);
	
	anyReadyLive <= isNonzero(readyMaskLive);
	anyReadyFull <= isNonzero(readyMask);
	
	anyReady <= anyReadyLive; -- OUTPUT
	
	schedulerOut <= (sends, dispatchDataNew.ins, dispatchDataNew.state);
	sending <= sends;
	   sentCancelled <= sentKilled;
end Behavioral;
