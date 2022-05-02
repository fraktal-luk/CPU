----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicQueues.all;


entity StoreQueue is
	generic(
		QUEUE_SIZE: integer := 8;
		IS_LOAD_QUEUE: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		acceptingOut: out std_logic;
		almostFull: out std_logic;
		
   	    prevSendingRe: in std_logic;
		prevSending: in std_logic;

        renameMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        inputMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        systemMask: in std_logic_vector(0 to PIPE_WIDTH-1);

        renamedPtr: out SmallNumber;

		storeValuePtr: in SmallNumber;
		storeValueResult: in ExecResult;

		compareAddressInput_N: in ExecResult;
		compareAddressInputOp: in SpecificOp;
		
        compareIndexInput: in SmallNumber;
        preCompareOp: in SpecificOp;

        selectedDataOutput_N: out ControlPacket;

		committing: in std_logic;
        commitMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        commitEffectiveMask: in std_logic_vector(0 to PIPE_WIDTH-1);

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
    	execCausing_N: in ExecResult;

		nextAccepting: in std_logic;		

		committedEmpty: out std_logic;
		committedSending: out std_logic;
		committedDataOut_N: out ControlPacket
	);
end StoreQueue;


architecture Behavioral of StoreQueue is
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

	signal addressMatchMask, memOpMask, newerLQ, newerRegLQ, newerNextLQ, olderNextSQ_T, olderRegSQ_T, olderSQ_T, addressMatchMask_T: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal adrPtr, pSelect, pStartLong, pStartLongNext, pDrainLong, pDrainLongNext, pDrainLongPrev,
           pTaggedLong, pTaggedLongNext, pFlushLong, pRenamedLong, pRenamedLongNext,
	       pStartLongEffective, pStartLongEffectiveNext,
	       nFull, nFullNext, nIn, nInRe, nOut, nCommitted, nCommittedEffective, recoveryCounter: SmallNumber := (others => '0');

    signal addresses, storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));

    signal queueContent, queueContentShifting, queueContentShiftingNext: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);

    signal isFull, isAlmostFull, drainReq, drainEqual, drainEffectiveEqual, nowCancelled, allowDrain, isSending, isDrainingPrev, isSelected: std_logic := '0';
    signal memEmpty: std_logic := '1'; -- CAREFUL! Starts with '1'

    signal drainOutput, selectedOutput: InstructionState := DEFAULT_INS_STATE;
    signal drainValue, selectedValue, drainAddress, selectedAddress, selectionAddress: Mword := (others => '0');

    signal selectedEntry, drainEntry: QueueEntry := DEFAULT_QUEUE_ENTRY;

    signal updateResult: ExecResult := DEFAULT_EXEC_RESULT;

    signal selectedOutputSig_N, committedOutputSig_N: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal ch0, ch1, ch2, ch3, chi, chii: std_logic := '0';

    function shiftQueueContent(content: QueueEntryArray; startPtrNext, nFullNext: SmallNumber; ev, prevSending, draining, compareInputFull: std_logic;
                               sqPtr: SmallNumber; op: SpecificOp; adr: Mword)
    return QueueEntryArray is
        variable res: QueueEntryArray(0 to content'length-1) := content;
        constant LEN: natural := content'length;
        variable currentPtr: SmallNumber := (others => '0');
    begin
        if draining = '1' then -- Move forward     
            res(0 to LEN-2) := res(1 to LEN-1);
            res(LEN-1).completedA := '0';
        end if;

        currentPtr := subTruncZ(sqPtr, startPtrNext, QUEUE_PTR_SIZE);

        if compareInputFull = '1'
            and isStoreOp(op) = '1' then
            res(slv2u(currentPtr)).completedA := '1';
        end if;

        if compareInputFull = '1' 
            and isStoreMemOp(op) = '1' then
            res(slv2u(currentPtr)).address := adr;
        end if;
        
        if ev = '1' then
            -- Clear 'adr completed' flag for empty slots
            for i in 0 to LEN-1 loop
                if i >= slv2u(nFullNext) then
                    res(i).completedA := '0';
                end if;
            end loop;
        elsif prevSending = '1' then
            -- move ptr, but it's handled by pointer mechanics
        end if;
        
        return res;
    end function;

begin

    NEW_DEV: block   
    begin   
        -- Ptr for random access updating
        adrPtr <= compareAddressInput_N.dest;
    
        -- Read ptr determinded by address matching - SQ only??
        pSelect <=  addTruncZ( findNewestMatchIndex(olderSQ_T, (others => '0'), nFull, QUEUE_PTR_SIZE), pDrainLongPrev, QUEUE_PTR_SIZE);
    
        -- Forw. matching
        
        -- LQ only
        addressMatchMask <= getAddressMatching(queueContent, compareAddressInput_N.value) and getAddressCompleted(queueContent);       
        memOpMask <= getWhichMemOp(queueContent);
               -- TODO: could/should be pStartLongNext?
        newerNextLQ <= cmpIndexAfter(pStartLong, pTaggedLong, compareIndexInput, QUEUE_SIZE, PTR_MASK_SN) and memOpMask
                                                                         when isStoreMemOp(preCompareOp) = '1' else (others => '0');
        newerLQ <=     newerRegLQ and addressMatchMask;

        -- SQ only
        addressMatchMask_T <= getAddressMatching(queueContentShifting, compareAddressInput_N.value) and getAddressCompleted(queueContentShifting);

        olderNextSQ_T <= cmpIndexBefore((others => '0'), nFull, subTruncZ(compareIndexInput, pDrainLongPrev, QUEUE_SIZE), QUEUE_SIZE, PTR_MASK_SN) -- TODO: nFull is not correct 
                                                                         when isLoadMemOp(preCompareOp) = '1' else (others => '0');
        olderSQ_T <=   olderRegSQ_T and addressMatchMask_T;

        updateResult.full <= compareAddressInput_N.full and isLoadOp(compareAddressInputOp) when IS_LOAD_QUEUE
                        else compareAddressInput_N.full and isStoreOp(compareAddressInputOp);
        updateResult.dest <= compareAddressInput_N.dest;
        updateResult.value <= compareAddressInput_N.value;

        process (clk)
        begin
            if rising_edge(clk) then
                --------
                -- SQ only
                queueContentShifting <= shiftQueueContent(queueContentShifting, pDrainLong, nFullNext, execEventSignal or lateEventSignal,
                                                            prevSending, isDrainingPrev, compareAddressInput_N.full, adrPtr, compareAddressInputOp, compareAddressInput_N.value);
                -------
         
                -- Front input
                if prevSending = '1' then
                    updateOnInput(queueContent, pTaggedLong, inputMask, systemMask, IS_LOAD_QUEUE);
                end if;
                
                -- E. adr update
                if compareAddressInput_N.full = '1' then
                    updateAddress_N(queueContent, updateResult, IS_LOAD_QUEUE);
                    addresses(p2i(adrPtr, QUEUE_SIZE)) <= compareAddressInput_N.value;
                end if;
                
                -- E. val update
                if storeValueResult.full = '1' and not IS_LOAD_QUEUE then
                    updateValue(queueContent, storeValuePtr);
                    storeValues(p2i(storeValuePtr, QUEUE_SIZE)) <= storeValueResult.value;
                end if;
          
          end if;
          
          if rising_edge(clk) then

                -- ERROR! isNonzero(mask) has to take into acount whether the match is with a full entry, that is [pDrain:pTagged) for SQ, [pStart:pTagged) for LQ
                if not IS_LOAD_QUEUE then
                    isSelected <= compareAddressInput_N.full and isNonzero(olderSQ_T);
                else
                    isSelected <= compareAddressInput_N.full and isNonzero(newerLQ);
                end if;

                selectedEntry <= queueContent(p2i(pSelect, QUEUE_SIZE));
                selectedValue <= storeValues(p2i(pSelect, QUEUE_SIZE));
                selectedAddress <= addresses(p2i(pSelect, QUEUE_SIZE));
                                
                -- D. outputs
                drainEntry <= queueContent(p2i(pDrainLong, QUEUE_SIZE));
                drainValue <= storeValues(p2i(pDrainLong, QUEUE_SIZE));
                drainAddress <= addresses(p2i(pDrainLong, QUEUE_SIZE));
            end if;
        end process;
        
        -- E. out
        selectedOutput <= getDrainOutput_T(selectedEntry, selectedAddress, selectedValue);    
        
        -- D. out
        drainOutput <= getDrainOutput_T(drainEntry, drainAddress, drainValue);   
    end block;

    pStartLongEffectiveNext <= addTruncZ(pStartLongEffective, nCommittedEffective, QUEUE_PTR_SIZE+1) when committing = '1'
                        else pStartLongNext when nowCancelled = '1' and drainEqual = '1'
                        else pStartLongEffective;
    pDrainLongNext <= addIntTrunc(pDrainLong, 1, QUEUE_PTR_SIZE+1) when drainReq = '1' else pDrainLong;
    pStartLongNext <= addTruncZ(pStartLong, nCommitted, QUEUE_PTR_SIZE+1) when committing = '1' else pStartLong;

    pTaggedLongNext <= pStartLongNext when lateEventSignal = '1'
            else   pFlushLong when execEventSignal = '1' 
            else   addIntTrunc(pTaggedLong, countOnes(inputMask), QUEUE_PTR_SIZE+1) when prevSending = '1'
            else   pTaggedLong;
                            
    pRenamedLongNext <= pStartLong when lateEventSignal = '1'
            else       pFlushLong when execEventSignal = '1'
            else       addIntTrunc(pRenamedLong, slv2u(nInRe), QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
            else       pRenamedLong;
    
    pFlushLong <= execCausing_N.dest;
   
     process (clk)   
     begin
		if rising_edge(clk) then
		      
            isDrainingPrev <= drainReq;
		    allowDrain <= not (nowCancelled or (not drainEqual and drainEffectiveEqual));
 
            -- TODO: reanalyze nowCancelled?
            -- D. out ctrl
            if drainEqual = '1' then
                if nowCancelled = '1' then
                    nowCancelled <= '0';
                end if;
            else
                if drainEffectiveEqual = '1' then
                    nowCancelled <= '1';
                end if;
            end if;

            if not IS_LOAD_QUEUE then -- SQ            
                memEmpty <= getQueueEmpty(pDrainLong, pTaggedLongNext, QUEUE_PTR_SIZE);
            else
                memEmpty <= getQueueEmpty(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);
            end if;

            olderRegSQ_T <= olderNextSQ_T;
            newerRegLQ <= newerNextLQ;


            pDrainLong <= pDrainLongNext;
            pDrainLongPrev <= pDrainLong;

            pStartLong <= pStartLongNext;
            pStartLongEffective <= pStartLongEffectiveNext;

            pTaggedLong <= pTaggedLongNext;
            pRenamedLong <= pRenamedLongNext;

            --- ctr management
	        nFull <= nFullNext;
   
   	        isFull <= cmpGtU(nFullNext, QUEUE_SIZE-4);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE-8);
   
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
            recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here       
		end if;
	end process;
				
	nIn <= i2slv( countOnes(inputMask), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');
		
	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);
    	
	    nInRe <= i2slv(countOnes(renameMask), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    		
        nOut <= nCommitted when committing = '1' else (others => '0');
    end generate;
    
    STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pDrainLong, pTaggedLongNext, QUEUE_PTR_SIZE);
	    -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
        nInRe <= i2slv(countOnes(renameMask), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    	
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isDrainingPrev = '1' else (others => '0');		  
    end generate;

    --CAREFUL: this is only for SQ
    nCommitted <= i2slv(countOnes(commitMask), SMALL_NUMBER_SIZE);
    nCommittedEffective <= i2slv(countOnes(commitEffectiveMask), SMALL_NUMBER_SIZE);

    drainEffectiveEqual <= bool2std(pStartLongEffective = pDrainLong);
    drainEqual <= bool2std(pStartLong = pDrainLong);
    drainReq <= not drainEqual;


    -- Acc sigs
	acceptingOut <= not isFull;
	almostFull <= isAlmostFull;

    renamedPtr <= pRenamedLong;

    -- E. output
    selectedDataOutput_N <= selectedOutputSig_N;

    WHEN_LQ: if IS_LOAD_QUEUE generate
        selectedOutputSig_N.controlInfo.full <= isSelected;           
    end generate;

    WHEN_SQ: if not IS_LOAD_QUEUE generate
        selectedOutputSig_N.controlInfo.full <= isSelected;
        selectedOutputSig_N.controlInfo.newEvent <= selectedOutput.controlInfo.newEvent;
        selectedOutputSig_N.controlInfo.firstBr <= selectedOutput.controlInfo.firstBr;
        selectedOutputSig_N.controlInfo.sqMiss <= selectedOutput.controlInfo.sqMiss;
        selectedOutputSig_N.op <= selectedOutput.specificOperation;
        selectedOutputSig_N.target <= selectedOutput.target;
        selectedOutputSig_N.nip <= selectedOutput.result;
        
        committedOutputSig_N.controlInfo.full <= isDrainingPrev and allowDrain;

        committedOutputSig_N.op <= drainOutput.specificOperation;
        committedOutputSig_N.target <= drainOutput.target;
        committedOutputSig_N.nip <= drainOutput.result;
    end generate;

	-- D. output (ctrl)
    committedDataOut_N <= committedOutputSig_N;
	committedEmpty <= bool2std(pStartLong = pDrainLongPrev);
	committedSending <= isDrainingPrev;

end Behavioral;
