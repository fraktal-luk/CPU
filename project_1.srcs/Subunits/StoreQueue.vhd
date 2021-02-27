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
		
		--acceptingBr: out std_logic;
		  prevSendingRe: in std_logic;
		prevSending: in std_logic;
		--	prevSendingBr: in std_logic;
		  dataInRe: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		--	dataInBr: in InstructionSlotArray(0 to PIPE_WIDTH-1);

            renamedPtr: out SmallNumber;

		storeValueInput: in InstructionSlot;
		compareAddressInput: in InstructionSlot;
        compareTagInput:    in InsTag;
            compareIndexInput: in SmallNumber;

		selectedDataOutput: out InstructionSlot;

		committing: in std_logic;
		robData: in InstructionSlotArray(0 to PIPE_WIDTH-1);		
		groupCtrInc: in InsTag;

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;
		
		nextAccepting: in std_logic;		
		sendingSQOut: out std_logic;
		dataOutV: out InstructionSlotArray(0 to PIPE_WIDTH-1);
		
		committedEmpty: out std_logic;
		committedSending: out std_logic;
		committedDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1)
	);
end StoreQueue;


architecture Behavioral of StoreQueue is
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;
	
	signal addressMatchMask, newerLQ, olderSQ, newerRegLQ, olderRegSQ, newerNextLQ, olderNextSQ: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	--signal selectedDataSlot, selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	
	signal pStart, pStartNext, pDrain, pDrainNext, pDrainPrev, pTagged, pTaggedNext, pFlush, storePtr, pSelect, pRenamed, pRenamedNext,
	       pStartEffective, pStartEffectiveNext,
           pStartLong, pStartLongNext, pDrainLong, pDrainLongNext, pDrainLongPrev, pTaggedLong, pTaggedLongNext, pFlushLong, pRenamedLong, pRenamedLongNext,
	       pStartLongEffective, pStartLongEffectiveNext: SmallNumber := (others => '0');
	       	
	signal nFull, nFullNext, nFull_T, nFullNext_T, nFullRestored, nIn, nOut, nCommitted, nCommittedEffective, nInRe: SmallNumber := (others => '0');
	signal recoveryCounter: SmallNumber := (others => '0');

    signal storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
     
    signal isFull, isAlmostFull, drainReq, drainEqual, drainEffectiveEqual, nowCancelled, allowDrain, isSending, isDraining, isDrainingPrev: std_logic := '0';
    signal memEmpty: std_logic := '1'; -- CAREFUL! Starts with '1'

    signal drainOutput, selectedOutput: InstructionState := DEFAULT_INS_STATE;
    signal isSelected: std_logic := '0';

    function getQueueEmpty(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return std_logic is
        constant xored: SmallNumber := pStart xor pEnd;
        constant template: SmallNumber := (others => '0');
    begin
        return bool2std(xored(QUEUE_PTR_SIZE downto 0) = template(QUEUE_PTR_SIZE downto 0));
    end function;


    function getNumFull(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber is
        constant diff: SmallNumber := subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE);
        constant xored: SmallNumber := pStart xor pEnd;        
        variable result: SmallNumber := diff;
    begin
        result(QUEUE_PTR_SIZE) := xored(QUEUE_PTR_SIZE) and not isNonzero(xored(QUEUE_PTR_SIZE-1 downto 0));
        return result;      
    end function;

    signal ch0, ch1, ch2, ch3, chi, chii: std_logic := '0';
begin
    NEW_DEV: block
         signal queueContent: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);
         signal selectedEntry, drainEntry: QueueEntry;     
    begin
        drainOutput <= getDrainOutput_T(drainEntry);
        selectedOutput <= getDrainOutput_T(selectedEntry);

        addressMatchMask <= getAddressMatching(queueContent, compareAddressInput.ins.result) and getAddressCompleted(queueContent) and getWhichMemOp(queueContent);
        
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
                
                selectedEntry <= queueContent(slv2u(pSelect));
                
                    -- ERROR! isNonzero(mask) has to take into acount whether the match is with a full entry, that is [pDrain:pTagged) for SQ, [pStart:pTagged) for LQ
                if not IS_LOAD_QUEUE then
                    isSelected <= compareAddressInput.full and isLoadMemOp(compareAddressInput.ins) and isNonzero(olderSQ);
                else
                    isSelected <= compareAddressInput.full and isStoreMemOp(compareAddressInput.ins) and isNonzero(newerLQ);
                end if;
                
                drainEntry <= queueContent(slv2u(pDrain));
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
    
        pFlushLong <=   execCausing.tags.lqPointer when IS_LOAD_QUEUE
               else execCausing.tags.sqPointer;

 
    storePtr <= storeValueInput.ins.tags.sqPointer and PTR_MASK_SN;
    drainReq <= not drainEqual;


    ADR_FORW: block
    begin
        newerNextLQ <= TMP_cmpIndexAfter(pStartLong, pTaggedLong, compareIndexInput, QUEUE_SIZE, PTR_MASK_SN);
        olderNextSQ <= TMP_cmpIndexBefore(pDrainLongPrev, pTaggedLong, compareIndexInput, QUEUE_SIZE, PTR_MASK_SN);

        newerLQ <=     newerRegLQ and addressMatchMask when isStoreMemOp(compareAddressInput.ins) = '1'
                  else (others => '0'); -- Only those with known address
        olderSQ <=     olderRegSQ and addressMatchMask when isLoadMemOp(compareAddressInput.ins) = '1'
                  else (others => '0'); -- Only those with known address

        WHEN_SQ: if not IS_LOAD_QUEUE generate
            -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
           pSelect <=   findNewestMatchIndex2(olderSQ,  pDrainPrev, pTagged, QUEUE_PTR_SIZE);
        end generate;
            
        process (clk)
            begin
                if rising_edge(clk) then
                    newerRegLQ <= newerNextLQ;
                    olderRegSQ <= olderNextSQ;                                            
                end if;      
         end process;              
    end block;

        ch0 <= memEmpty;
        ch1 <= --(not pStartLong(QUEUE_PTR_SIZE) xor pTaggedLong(QUEUE_PTR_SIZE)) and bool2std(pStart = pTagged);
                getQueueEmpty(pStartLong, pTaggedLong, QUEUE_PTR_SIZE);
        ch2 <= --(not pDrainLongPrev(QUEUE_PTR_SIZE) xor pTaggedLong(QUEUE_PTR_SIZE)) and bool2std(pDrainPrev = pTagged);
                getQueueEmpty(pDrainLongPrev, pTaggedLong, QUEUE_PTR_SIZE);        
        ch3 <= not ch0 xor ch1;
        chi <= not ch0 xor ch2;

	WHEN_LQ: if IS_LOAD_QUEUE generate	
	   nInRe <= i2slv(countOnes(getLoadMask(TMP_recodeMem(dataInRe))), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');
	end generate;
	
	WHEN_SQ: if not IS_LOAD_QUEUE generate
	    -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
        nInRe <= i2slv(countOnes(getStoreMask(TMP_recodeMem(dataInRe))), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');	   	   
	end generate;


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
                        
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
	        
	        recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
	        
	        isFull <= cmpGtU(nFullNext, QUEUE_SIZE-4);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE-8);

	        nFull <= nFullNext;

            if not IS_LOAD_QUEUE then -- SQ            
                memEmpty <= getQueueEmpty(pDrainLong, pTaggedLongNext, QUEUE_PTR_SIZE);
            else
                memEmpty <= getQueueEmpty(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);
            end if;        
--            -- TODO: include execEventSignal in both cases!
--            if not IS_LOAD_QUEUE then -- SQ
--               if prevSending = '1' and isNonzero(extractFullMask(dataIn)) = '1' then 
--                  memEmpty <= '0';
--               end if;
                
--               if lateEventSignal = '1' and pStart = pDrainPrev then
--                  memEmpty <= '1';
--               end if;
               
--               if execEventSignal = '1' and pFlush = pStartNext then -- if execEventSignal, content can't grow
--                  memEmpty <= '1';
--               end if;
               
--	           if isDrainingPrev = '1' and (prevSending = '0' or isNonzero(extractFullMask(dataIn)) = '0') and pDrain = pTagged then
--	              memEmpty  <= '1';
--	           end if;
             
--             else -- LQ
--                if prevSending = '1' and isNonzero(extractFullMask(dataIn)) = '1' then 
--                   memEmpty <= '0';
--                end if;
                 
--                if lateEventSignal = '1' then
--                   memEmpty <= '1';
--                end if;

--                if execEventSignal = '1' and pFlush = pStartNext then -- if execEventSignal, content can't grow
--                   memEmpty <= '1';
--                end if;
                
--                if committing = '1' and (prevSending = '0' or isNonzero(extractFullMask(dataIn)) = '0') and pStartNext = pTagged then
--                   memEmpty  <= '1';
--                end if;	           
--	        end if;
		end if;
	end process;

	nFullNext <=  nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
				else subTruncZ(add(nFull, nIn), nOut, QUEUE_CAP_SIZE);
	nIn <= i2slv( countOnes(extractFullMask(dataIn)), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');
		
		
		
	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
    	   nFullNext_T <= getNumFull(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);
	
	
        nOut <= nCommitted when committing = '1'
                else (others => '0');
        nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pTagged and memEmpty = '0'
                           else subTruncZ(pTagged, pStartNext, QUEUE_PTR_SIZE); -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch 
    end generate;
    
    STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
    	   nFullNext_T <= getNumFull(pDrainLong, pTaggedLongNext, QUEUE_PTR_SIZE);
    
    
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isDrainingPrev = '1'
              else (others => '0');		  
        nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pDrainPrev = pTagged and memEmpty = '0'
                            else subTruncZ(pTagged, pDrain, QUEUE_PTR_SIZE); -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch
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
