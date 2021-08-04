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

		storeValueInput: in InstructionSlot;
		compareAddressInput: in InstructionSlot;
        compareIndexInput: in SmallNumber;
            preCompareAddressInput: InstructionSlot;

		selectedDataOutput: out InstructionSlot;

		committing: in std_logic;
		robData: in InstructionSlotArray(0 to PIPE_WIDTH-1);		
		--groupCtrInc: in InsTag;

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;
		
		nextAccepting: in std_logic;		

		committedEmpty: out std_logic;
		committedSending: out std_logic;
		committedDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1)
	);
end StoreQueue;


architecture Behavioral of StoreQueue is
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;
	
	signal addressMatchMask, memOpMask, newerLQ, olderSQ, newerRegLQ, olderRegSQ, newerNextLQ, olderNextSQ: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
	
	signal pStart, pStartNext, pDrain, pDrainNext, pDrainPrev, pTagged, pTaggedNext, pFlush, storePtr, pSelect, pSelectShifting, pRenamed, pRenamedNext,
	       pStartEffective, pStartEffectiveNext,
           pStartLong, pStartLongNext, pDrainLong, pDrainLongNext, pDrainLongPrev, pTaggedLong, pTaggedLongNext, pFlushLong, pRenamedLong, pRenamedLongNext,
	       pStartLongEffective, pStartLongEffectiveNext,
	       nFull, nFullNext, nIn, nOut, nCommitted, nCommittedEffective, nInRe, recoveryCounter, TMP_count: SmallNumber := (others => '0');

    signal addresses, storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    signal cmpAddresses: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    signal cmpAddressCompleted: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
    
    signal isFull, isAlmostFull, drainReq, drainEqual, drainEffectiveEqual, nowCancelled, allowDrain, isSending, isDraining, isDrainingPrev, isSelected: std_logic := '0';
    signal memEmpty: std_logic := '1'; -- CAREFUL! Starts with '1'

    signal drainOutput, selectedOutput: InstructionState := DEFAULT_INS_STATE;
    signal drainValue, selectedValue, drainAddress, selectedAddress: Mword := (others => '0');

    signal ch0, ch1, ch2, ch3, chi, chii: std_logic := '0';
begin
    NEW_CMP_ADR: block
        function cmpAddressesNext(content: MwordArray; startPtrNext, endPtr, nFullNext: SmallNumber; ev, prevSending, draining: std_logic; compareAddressInput: InstructionSlot) return MwordArray is
            variable res: MwordArray(content'range) := content;
            constant LEN: natural := content'length;
            --variable newStartPtr: SmallNumber := startPtr;
            variable currentPtr: SmallNumber := (others => '0');
        begin
            if draining = '1' then
                -- Move forward
                res(0 to LEN-2) := res(1 to LEN-1);
                --newStartPtr := addIntTrunc(startPtr, -1, QUEUE_PTR_SIZE+1);
                
                -- clear 'completed' for last slot
            end if;
            
            currentPtr := subTruncZ(compareAddressInput.ins.tags.sqPointer, startPtrNext, QUEUE_PTR_SIZE);
            
            if compareAddressInput.full = '1' and isStoreMemOp(compareAddressInput.ins) = '1' then -- CAREFUL: includes being not system op
                res(slv2u(currentPtr)) := compareAddressInput.ins.result;
                -- write 'completed' flag
            end if;
            
            if ev = '1' then
                -- Clear 'adr completed' flag for empty slots
                for i in 0 to LEN-1 loop
                    if i >= slv2u(nFullNext) then
                        -- clear 'completed' for slot [i]
                    end if;
                end loop;
            elsif prevSending = '1' then
                -- move ptr, but it's handled by pointer mechanics
                null;
            end if;
            
            return res;
        end function;

        function cmpAddressesCompletedNext(content: std_logic_vector; startPtrNext, endPtr, nFullNext: SmallNumber; ev, prevSending, draining: std_logic; compareAddressInput: InstructionSlot)
        return std_logic_vector is
            variable res: std_logic_vector(content'range) := content;
            constant LEN: natural := content'length;
            --variable newStartPtr: SmallNumber := startPtr;
            variable currentPtr: SmallNumber := (others => '0');
        begin
            if draining = '1' then
                -- Move forward
                res(0 to LEN-2) := res(1 to LEN-1);
                --newStartPtr := addIntTrunc(startPtr, -1, QUEUE_PTR_SIZE+1);
                res(LEN-1) := '0';
            end if;
            
            currentPtr := subTruncZ(compareAddressInput.ins.tags.sqPointer, startPtrNext, QUEUE_PTR_SIZE);
            
            if compareAddressInput.full = '1' and isStoreOp(compareAddressInput.ins) = '1' then
                res(slv2u(currentPtr)) := '1';
                -- write 'completed' flag
            end if;
            
            if ev = '1' then
                -- Clear 'adr completed' flag for empty slots
                for i in 0 to LEN-1 loop
                    if i >= slv2u(nFullNext) then
                        res(i) := '0';
                    end if;
                end loop;
            elsif prevSending = '1' then
                -- move ptr, but it's handled by pointer mechanics
                null;
            end if;
            
            return res;
        end function;
        
        signal olderNextSQ_T, olderRegSQ_T, olderSQ_T, memOpMask_T, addressMatchMask_T: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
    begin
        addressMatchMask_T <= getAddressMatching(cmpAddresses, compareAddressInput.ins.result) and cmpAddressCompleted;
        
        olderNextSQ_T <= cmpIndexBefore((others => '0'), nFull, subTruncZ(compareIndexInput, pDrainLongPrev, QUEUE_SIZE), QUEUE_SIZE, PTR_MASK_SN) -- TODO: nFull is not correct 
                                                                         when isLoadMemOp(preCompareAddressInput.ins) = '1' else (others => '0');

        olderSQ_T <=     olderRegSQ_T and addressMatchMask_T;-- when isLoadMemOp(compareAddressInput.ins) = '1' else (others => '0'); -- Only those with known address
    
        process (clk)
        begin
            if rising_edge(clk) then
                cmpAddresses <= cmpAddressesNext(cmpAddresses, pDrainLong, pTagged, nFullNext, execEventSignal or lateEventSignal, prevSending, isDrainingPrev, compareAddressInput);
                cmpAddressCompleted <= cmpAddressesCompletedNext(
                            cmpAddressCompleted, pDrainLong, pTagged, nFullNext, execEventSignal or lateEventSignal, prevSending, isDrainingPrev, compareAddressInput);
                olderRegSQ_T <= olderNextSQ_T;
            end if;
        end process;
    end block;

    NEW_DEV: block
         signal queueContent: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);
         signal selectedEntry, drainEntry: QueueEntry;
         signal selectionAddress: MWord := (others => '0');   
    begin
        drainOutput <= getDrainOutput_T(drainEntry, drainValue);
        selectedOutput <= getDrainOutput_T(selectedEntry, selectedValue);

        addressMatchMask <= getAddressMatching(queueContent, compareAddressInput.ins.result) and getAddressCompleted(queueContent);-- and getWhichMemOp(queueContent);
        memOpMask <= getWhichMemOp(queueContent);
        
        process (clk)
        begin
            if rising_edge(clk) then
                if prevSending = '1' then
                    updateOnInput(queueContent, pTagged, dataIn, IS_LOAD_QUEUE);
                end if;
                
                if compareAddressInput.full = '1' then
                    updateAddress(queueContent, compareAddressInput, IS_LOAD_QUEUE);
                end if;
                
                if storeValueInput.full = '1' and not IS_LOAD_QUEUE then
                    updateValue(queueContent, storeValueInput);
                end if;                               
                
                --selectionAddress <= compareAddressInput.ins.target;
                selectedEntry <= queueContent(slv2u(pSelect));
                selectedValue <= storeValues(slv2u(pSelect));
                selectedAddress <= addresses(slv2u(pSelect));
                
                    -- ERROR! isNonzero(mask) has to take into acount whether the match is with a full entry, that is [pDrain:pTagged) for SQ, [pStart:pTagged) for LQ
                if not IS_LOAD_QUEUE then
                    isSelected <= compareAddressInput.full-- and isLoadMemOp(compareAddressInput.ins)
                                                             and isNonzero(olderSQ);
                else
                    isSelected <= compareAddressInput.full-- and isStoreMemOp(compareAddressInput.ins) and isNonzero(newerLQ);
                                                           and isNonzero(newerLQ);
                end if;
                
                drainEntry <= queueContent(slv2u(pDrain));
                drainValue <= storeValues(slv2u(pDrain));
                drainAddress <= addresses(slv2u(pDrain));
            end if;
        end process;
    end block;

    --CAREFUL: this is only for SQ
    drainEqual <= bool2std(pDrain = pStart);
    drainEffectiveEqual <= bool2std(pDrain = pStartEffective);       
    nCommitted <= getNumCommitted(robData, IS_LOAD_QUEUE);
    nCommittedEffective <= getNumCommittedEffective(robData, IS_LOAD_QUEUE);

    pStartEffectiveNext <= pStartLongEffectiveNext and PTR_MASK_SN;
    pDrainNext <= pDrainLongNext and PTR_MASK_SN;
    pStartNext <= pStartLongNext and PTR_MASK_SN;
    pTaggedNext <= pTaggedLongNext and PTR_MASK_SN;
    pRenamedNext <= pRenamedLongNext and PTR_MASK_SN;

    pStartLongEffectiveNext <= addTruncZ(pStartLongEffective, nCommittedEffective, QUEUE_PTR_SIZE+1) when committing = '1'
                        else pStartLongNext when nowCancelled = '1' and drainEqual = '1'
                        else pStartLongEffective;
    pDrainLongNext <= addIntTrunc(pDrainLong, 1, QUEUE_PTR_SIZE+1) when drainReq = '1' else pDrainLong;
    pStartLongNext <= addTruncZ(pStartLong, nCommitted, QUEUE_PTR_SIZE+1) when committing = '1' else pStartLong; --pStartNewNext;        

    pTaggedLongNext <= pStartLongNext when lateEventSignal = '1'
            else   pFlushLong when execEventSignal = '1' 
            else   addIntTrunc(pTaggedLong, countOnes(extractFullMask(dataIn)), QUEUE_PTR_SIZE+1) when prevSending = '1'
            else   pTaggedLong;
                            
    pRenamedLongNext <= pStartLong when lateEventSignal = '1'
            else       pFlushLong when execEventSignal = '1'
            else       addIntTrunc(pRenamedLong, slv2u(nInRe), QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
            else       pRenamedLong;

    pFlush <=   execCausing.tags.lqPointer and PTR_MASK_SN when IS_LOAD_QUEUE
           else execCausing.tags.sqPointer and PTR_MASK_SN;
    
    pFlushLong <= execCausing.tags.lqPointer when IS_LOAD_QUEUE
             else execCausing.tags.sqPointer;

    storePtr <= storeValueInput.ins.tags.sqPointer and PTR_MASK_SN;
    drainReq <= not drainEqual;

    ADR_FORW: block
    begin
                                -- TODO: could/should be pStartLongNext?
        newerNextLQ <= cmpIndexAfter(pStartLong, pTaggedLong, compareIndexInput, QUEUE_SIZE, PTR_MASK_SN) and memOpMask when isStoreMemOp(preCompareAddressInput.ins) = '1' else (others => '0');
        olderNextSQ <= cmpIndexBefore(pDrainLong, pTaggedLong, compareIndexInput, QUEUE_SIZE, PTR_MASK_SN) and memOpMask when isLoadMemOp(preCompareAddressInput.ins) = '1' else (others => '0');

        newerLQ <=     newerRegLQ and addressMatchMask;-- when isStoreMemOp(compareAddressInput.ins) = '1' else (others => '0'); -- Only those with known address
        olderSQ <=     olderRegSQ and addressMatchMask;-- when isLoadMemOp(compareAddressInput.ins) = '1' else (others => '0'); -- Only those with known address

        WHEN_SQ: if not IS_LOAD_QUEUE generate
            -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
           pSelect <=   findNewestMatchIndex(olderSQ,  pDrainPrev, pTagged, nFull, QUEUE_PTR_SIZE);
           --     TMP_count <= sub(pTaggedLong, pDrainLongPrev);
        end generate;
            
        process (clk)
            begin
                if rising_edge(clk) then
                    newerRegLQ <= newerNextLQ;
                    olderRegSQ <= olderNextSQ;                             
                end if;      
         end process;              
    end block;

    pDrain <= pDrainLong and PTR_MASK_SN;
    pDrainPrev <= pDrainLongPrev and PTR_MASK_SN;

    pStart <= pStartLong and PTR_MASK_SN;
    pStartEffective <= pStartLongEffective and PTR_MASK_SN;

    pTagged <= pTaggedLong and PTR_MASK_SN;
    pRenamed <= pRenamedLong and PTR_MASK_SN;
     
     process (clk)   
     begin
		if rising_edge(clk) then
            pDrainLong <= pDrainLongNext;
            pDrainLongPrev <= pDrainLong;

            pStartLong <= pStartLongNext;
            pStartLongEffective <= pStartLongEffectiveNext;

            pTaggedLong <= pTaggedLongNext;
            pRenamedLong <= pRenamedLongNext;
		      
		    -- value updating
            isDrainingPrev <= isDraining;		    
		    allowDrain <= not (nowCancelled or (not drainEqual and drainEffectiveEqual));

                if compareAddressInput.full = '1' then
                    updateAddressArr(addresses, compareAddressInput, IS_LOAD_QUEUE);
                end if;
            
            if storeValueInput.full = '1' then
                storeValues(slv2u(storePtr)) <= storeValueInput.ins.result;
            end if;

            if drainEqual = '0' and drainEffectiveEqual = '1' then
                nowCancelled <= '1';
            end if;
            
            if nowCancelled = '1' and drainEqual = '1' then
                nowCancelled <= '0';
            end if;


            --- ctr management
	        isFull <= cmpGtU(nFullNext, QUEUE_SIZE-4);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE-8);

	        nFull <= nFullNext;

            if not IS_LOAD_QUEUE then -- SQ            
                memEmpty <= getQueueEmpty(pDrainLong, pTaggedLongNext, QUEUE_PTR_SIZE);
            else
                memEmpty <= getQueueEmpty(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);
            end if;
            
            
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
        
    isDraining <= drainReq;
     
	acceptingOut <= not isFull;
	almostFull <= isAlmostFull;

    renamedPtr <= pRenamedLong;


    selectedDataOutput <= (isSelected, selectedOutput) when not IS_LOAD_QUEUE
                    else  (isSelected, DEFAULT_INS_STATE);
	
	committedEmpty <= bool2std(pStart = pDrainPrev);
	committedSending <= isDrainingPrev;

	committedDataOut(0) <= (isDrainingPrev and allowDrain, drainOutput);
	committedDataOut(1 to PIPE_WIDTH-1) <= (others => DEFAULT_INSTRUCTION_SLOT);
	               	               
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
       signal DUMMY_mask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '1');
    begin       
       --queueText <= getInsStringArray(makeSlotArray(content, DUMMY_mask), transfer);
    end generate;
	
end Behavioral;
