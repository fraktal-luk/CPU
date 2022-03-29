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

        dataInRe: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);

        renamedPtr: out SmallNumber;

		storeValuePtr: in SmallNumber;
		storeValueResult: in ExecResult;
		
		compareAddressInput: in InstructionSlot;  -- sqPointer, result, op
        compareIndexInput: in SmallNumber;
        preCompareOp: in SpecificOp;

		selectedDataOutput: out InstructionSlot;   -- result, target, ctrl, op

		committing: in std_logic;
		robData: in InstructionSlotArray(0 to PIPE_WIDTH-1);		

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;   -- pointers
		
		nextAccepting: in std_logic;		

		committedEmpty: out std_logic;
		committedSending: out std_logic;
		committedDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1)  -- similar to selectedDataOutput
	);
end StoreQueue;


architecture Behavioral of StoreQueue is
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;
	
	signal addressMatchMask, memOpMask, newerLQ, newerRegLQ, newerNextLQ, olderNextSQ_T, olderRegSQ_T, olderSQ_T, addressMatchMask_T: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
	
	signal pStart, pStartNext, pDrain, pDrainNext, pDrainPrev, pTagged, pTaggedNext, pFlush, storePtr, adrPtr, pSelect, pRenamed, pRenamedNext,
	       pStartEffective, pStartEffectiveNext,
           pStartLong, pStartLongNext, pDrainLong, pDrainLongNext, pDrainLongPrev, pTaggedLong, pTaggedLongNext, pFlushLong, pRenamedLong, pRenamedLongNext,
	       pStartLongEffective, pStartLongEffectiveNext,
	       nFull, nFullNext, nIn, nInRe, nOut, nCommitted, nCommittedEffective, recoveryCounter: SmallNumber := (others => '0');

    signal addresses, storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    
    signal queueContentShifting, queueContentShiftingNext: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);

    signal isFull, isAlmostFull, drainReq, drainEqual, drainEffectiveEqual, nowCancelled, allowDrain, isSending, isDrainingPrev, isSelected: std_logic := '0';
    signal memEmpty: std_logic := '1'; -- CAREFUL! Starts with '1'

    signal drainOutput, selectedOutput: InstructionState := DEFAULT_INS_STATE;
    signal drainValue, selectedValue, drainAddress, selectedAddress: Mword := (others => '0');

    signal queueContent: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);
    signal selectedEntry, drainEntry: QueueEntry;
    signal selectionAddress: Mword := (others => '0');

    signal ch0, ch1, ch2, ch3, chi, chii: std_logic := '0';


    function shiftQueueContent(content: QueueEntryArray; startPtrNext, nFullNext: SmallNumber;
                               ev, prevSending, draining: std_logic; compareAddressInput: InstructionSlot; sqPtr: SmallNumber; op: SpecificOp; adr: Mword) return QueueEntryArray is
        variable res: QueueEntryArray(0 to content'length-1) := content;
        constant LEN: natural := content'length;
        variable currentPtr: SmallNumber := (others => '0');
    begin
        if draining = '1' then -- Move forward     
            res(0 to LEN-2) := res(1 to LEN-1);
            res(LEN-1).completedA := '0';
        end if;

        currentPtr := subTruncZ(sqPtr,--compareAddressInput.ins.tags.sqPointer,
                                startPtrNext, QUEUE_PTR_SIZE);

        if compareAddressInput.full = '1' and --isStoreOp(compareAddressInput.ins) = '1' then
                                                isStoreOp(op) = '1' then
            res(slv2u(currentPtr)).completedA := '1';
        end if;

        if compareAddressInput.full = '1' and --isStoreMemOp(compareAddressInput.ins) = '1' then
                                                isStoreMemOp(op) = '1' then
            res(slv2u(currentPtr)).address := --compareAddressInput.ins.result;
                                              adr;
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
    
        -----
        -- Ptrs for random access updating
        adrPtr <= compareAddressInput.ins.tags.sqPointer and PTR_MASK_SN;
        storePtr <= storeValuePtr and PTR_MASK_SN;
        ----
    
        -- Read ptr determinded by address matching - SQ only??
        pSelect <=  addTruncZ( findNewestMatchIndex(olderSQ_T, (others => '0'), nFull, QUEUE_PTR_SIZE), pDrainPrev, QUEUE_PTR_SIZE);
    
        -- Forw. matching
        
        -- LQ only
        addressMatchMask <= getAddressMatching(queueContent, compareAddressInput.ins.result) and getAddressCompleted(queueContent);       
        memOpMask <= getWhichMemOp(queueContent);
               -- TODO: could/should be pStartLongNext?
        newerNextLQ <= cmpIndexAfter(pStartLong, pTaggedLong, compareIndexInput, QUEUE_SIZE, PTR_MASK_SN) and memOpMask
                                                                         when isStoreMemOp(preCompareOp) = '1' else (others => '0');
        newerLQ <=     newerRegLQ and addressMatchMask;


        -- SQ only
        addressMatchMask_T <= getAddressMatching(queueContentShifting, compareAddressInput.ins.result) and getAddressCompleted(queueContentShifting);

        olderNextSQ_T <= cmpIndexBefore((others => '0'), nFull, subTruncZ(compareIndexInput, pDrainLongPrev, QUEUE_SIZE), QUEUE_SIZE, PTR_MASK_SN) -- TODO: nFull is not correct 
                                                                         when isLoadMemOp(preCompareOp) = '1' else (others => '0');
        olderSQ_T <=   olderRegSQ_T and addressMatchMask_T;


        process (clk)
        begin
            if rising_edge(clk) then

                --------
                -- SQ only
                queueContentShifting <= shiftQueueContent(queueContentShifting, pDrainLong, nFullNext, execEventSignal or lateEventSignal,
                                                            prevSending, isDrainingPrev, compareAddressInput, adrPtr, compareAddressInput.ins.specificOperation,
                                                            compareAddressInput.ins.result);
                -------
         
                -- Front input
                if prevSending = '1' then
                    updateOnInput(queueContent, pTagged, dataIn, IS_LOAD_QUEUE);
                end if;
                
                -- E. adr update
                if compareAddressInput.full = '1' then
                    updateAddress(queueContent, compareAddressInput, IS_LOAD_QUEUE);
                    addresses(slv2u(adrPtr)) <= compareAddressInput.ins.result;
                end if;
                
                -- E. val update
                if storeValueResult.full = '1' and not IS_LOAD_QUEUE then
                    updateValue(queueContent, storeValuePtr);
                    storeValues(slv2u(storePtr)) <= storeValueResult.value;
                end if;
          
          end if;
          
          if rising_edge(clk) then

                -- ERROR! isNonzero(mask) has to take into acount whether the match is with a full entry, that is [pDrain:pTagged) for SQ, [pStart:pTagged) for LQ
                if not IS_LOAD_QUEUE then
                    isSelected <= compareAddressInput.full and isNonzero(olderSQ_T);
                else
                    isSelected <= compareAddressInput.full and isNonzero(newerLQ);
                end if;

                selectedEntry <= queueContent(slv2u(pSelect));
                selectedValue <= storeValues(slv2u(pSelect));
                selectedAddress <= addresses(slv2u(pSelect));
                                
                -- D. outputs
                drainEntry <= queueContent(slv2u(pDrain));
                drainValue <= storeValues(slv2u(pDrain));
                drainAddress <= addresses(slv2u(pDrain));
            end if;
        end process;
        
        -- E. out
        selectedOutput <= getDrainOutput_T(selectedEntry, selectedAddress, selectedValue);    
        
        -- D. out
        drainOutput <= getDrainOutput_T(drainEntry, drainAddress, drainValue);   
    end block;
    

    pDrain <= pDrainLong and PTR_MASK_SN;
    pDrainPrev <= pDrainLongPrev and PTR_MASK_SN;
    pStart <= pStartLong and PTR_MASK_SN;
    pStartEffective <= pStartLongEffective and PTR_MASK_SN;
    pTagged <= pTaggedLong and PTR_MASK_SN;
    pRenamed <= pRenamedLong and PTR_MASK_SN;
    pFlush <=   pFlushLong and PTR_MASK_SN;
  

    pStartEffectiveNext <= pStartLongEffectiveNext and PTR_MASK_SN;
    pDrainNext <= pDrainLongNext and PTR_MASK_SN;
    pStartNext <= pStartLongNext and PTR_MASK_SN;
    pTaggedNext <= pTaggedLongNext and PTR_MASK_SN;
    pRenamedNext <= pRenamedLongNext and PTR_MASK_SN;

    pStartLongEffectiveNext <= addTruncZ(pStartLongEffective, nCommittedEffective, QUEUE_PTR_SIZE+1) when committing = '1'
                        else pStartLongNext when nowCancelled = '1' and drainEqual = '1'
                        else pStartLongEffective;
    pDrainLongNext <= addIntTrunc(pDrainLong, 1, QUEUE_PTR_SIZE+1) when drainReq = '1' else pDrainLong;
    pStartLongNext <= addTruncZ(pStartLong, nCommitted, QUEUE_PTR_SIZE+1) when committing = '1' else pStartLong;

    pTaggedLongNext <= pStartLongNext when lateEventSignal = '1'
            else   pFlushLong when execEventSignal = '1' 
            else   addIntTrunc(pTaggedLong, countOnes(extractFullMask(dataIn)), QUEUE_PTR_SIZE+1) when prevSending = '1'
            else   pTaggedLong;
                            
    pRenamedLongNext <= pStartLong when lateEventSignal = '1'
            else       pFlushLong when execEventSignal = '1'
            else       addIntTrunc(pRenamedLong, slv2u(nInRe), QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
            else       pRenamedLong;
    
    pFlushLong <= execCausing.tags.lqPointer when IS_LOAD_QUEUE
             else execCausing.tags.sqPointer;
   
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
				
	nIn <= i2slv( countOnes(extractFullMask(dataIn)), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');
		
	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);
    	
	    nInRe <= i2slv(countOnes(getLoadMask(TMP_recodeMem(dataInRe))), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    		
        nOut <= nCommitted when committing = '1' else (others => '0');
    end generate;
    
    STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pDrainLong, pTaggedLongNext, QUEUE_PTR_SIZE);
	    -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
        nInRe <= i2slv(countOnes(getStoreMask(TMP_recodeMem(dataInRe))), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    	
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isDrainingPrev = '1' else (others => '0');		  
    end generate;

    --CAREFUL: this is only for SQ
    nCommitted <= getNumCommitted(robData, IS_LOAD_QUEUE);
    nCommittedEffective <= getNumCommittedEffective(robData, IS_LOAD_QUEUE);
    
    drainEffectiveEqual <= bool2std(pDrain = pStartEffective);
    drainEqual <= bool2std(pDrain = pStart);
    drainReq <= not drainEqual;


    -- Acc sigs
	acceptingOut <= not isFull;
	almostFull <= isAlmostFull;

    renamedPtr <= pRenamedLong;

    -- E. output
    selectedDataOutput <= (isSelected, selectedOutput) when not IS_LOAD_QUEUE
                    else  (isSelected, DEFAULT_INS_STATE);

	-- D. output (ctrl)
	committedEmpty <= bool2std(pStart = pDrainPrev);
	committedSending <= isDrainingPrev;
	committedDataOut(0) <= (isDrainingPrev and allowDrain, drainOutput);
	committedDataOut(1 to PIPE_WIDTH-1) <= (others => DEFAULT_INSTRUCTION_SLOT);
	
end Behavioral;
