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

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INSTRUCTION_STATE);
	
	signal addressMatchMask, newerLQ, olderSQ, newerRegLQ, olderRegSQ, newerNextLQ, olderNextSQ, tmpTagCmpMask,
	                           newerLQ_T, olderSQ_T, newerRegLQ_T, olderRegSQ_T, newerNextLQ_T, olderNextSQ_T: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot, selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	
	signal pStart, pStartNext, pDrain, pDrainNext, pDrainPrev, pTagged, pAll,  pFlush, storePtr, pSelect, pRenamed, pRenamedNext,
	       pStartEffective, pStartEffectiveNext, causingPtr: SmallNumber := (others => '0');	
	signal nFull, nFullNext, nFullRestored, nIn, nOut, nCommitted, nCommittedEffective, nInRe: SmallNumber := (others => '0');
	signal recoveryCounter: SmallNumber := (others => '0');

    signal storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    signal drainValue, selectedValue: Mword := (others => '0');
    signal drainData: InstructionState := DEFAULT_INSTRUCTION_STATE;
              
    signal isFull, isAlmostFull, drainReq, drainEqual, drainEffectiveEqual, nowCancelled, allowDrain, isSending, isDraining, isDrainingPrev: std_logic := '0';
    signal memEmpty: std_logic := '1'; -- CAREFUL! Starts with '1'

    signal ch0, ch1, ch2, ch3: std_logic := '0';


    function getMatchedAddresses(content: InstructionStateArray; cmpIns: InstructionSlot) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        for i in res'range loop
            if content(i).target = cmpIns.ins.result then
                res(i) := '1';
            end if;
        end loop;
        return res;
    end function;
    

    function getNewContentSQ(content: InstructionStateArray; dataIn: InstructionSlotArray;
				                prevSending: std_logic;
				                pTagged: SmallNumber;
				                storeValueInput, storeAddressInput: InstructionSlot;
				                isLQ: boolean; matchingNewerLoads: std_logic_vector
				                )
    return InstructionStateArray is
        variable res: InstructionStateArray(0 to QUEUE_SIZE-1) := content;
        variable slot: InstructionSlot;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    constant IS_STORE_OP: boolean := std2bool(isStoreOp(storeAddressInput.ins));
	    constant IS_LOAD_OP: boolean := std2bool(isLoadOp(storeAddressInput.ins));
    begin
        for i in 0 to QUEUE_SIZE-1 loop
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pTagged) and PTR_MASK_SN;    
           case diff(1 downto 0) is
               when "00" =>
                    remv := "111";
               when "01" =>
                    remv := "110";
               when "10" =>
                    remv := "100";
               when others =>
                    remv := "000";                                                                                       
           end case;
           
           if slv2u(diff) < countOnes(extractFullMask(dataIn)) and prevSending = '1' then
                slot := getNewElem(remv, dataIn);
                        --dataIn(slv2u(diff(1 downto 0)));
                res(i) := slot.ins;          
                res(i).tags := slot.ins.tags;
                res(i).specificOperation := slot.ins.specificOperation;
                res(i).controlInfo.completed := '0';
                res(i).controlInfo.completed2 := '0';
                res(i).controlInfo.firstBr := '0';                                  
           end if;
           
           -- Mark loads which break data dependence on older stores
           if isLQ and matchingNewerLoads(i) = '1' then
               res(i).controlInfo.orderViolation := '1';
               res(i).controlInfo.newEvent := '1';
           end if;
        end loop;

        if prevSending = '1' then
            res(slv2u(pTagged)).controlInfo.firstBr := '1';
        end if;

        -- Update target after mem execution
        for i in 0 to QUEUE_SIZE-1 loop
           if content(i).tags.renameIndex = storeValueInput.ins.tags.renameIndex
               and storeValueInput.full = '1'
           then
               res(i).controlInfo.completed2 := '1'; -- data completed
               res(i).result := storeValueInput.ins.result;
           end if;
           
           if content(i).tags.renameIndex = storeAddressInput.ins.tags.renameIndex
               and storeAddressInput.full = '1'
               and ((IS_STORE_OP and not isLQ) or (IS_LOAD_OP and isLQ))
           then
               res(i).controlInfo.completed := '1'; -- address completed           
               res(i).target := storeAddressInput.ins.result;
               res(i).controlInfo.orderViolation := '0';
           end if;                      
        end loop;

        return res;
    end function;
    
    function getStoreDataIndex(content: InstructionStateArray; storeValueInput: InstructionSlot)
    return SmallNumber is
    begin
        for i in 0 to QUEUE_SIZE-1 loop
           if content(i).tags.renameIndex = storeValueInput.ins.tags.renameIndex
           then
               return i2slv(i, SMALL_NUMBER_SIZE) and PTR_MASK_SN;
           end if;                            
        end loop;

        return (others => '0');
    end function;


	function TMP_cmpTagsBefore(content: InstructionStateArray; tag: InsTag)
	return std_logic_vector is
		variable res: std_logic_vector(0 to content'length-1) := (others => '0');
	begin
		for i in 0 to res'length-1 loop
			res(i) := compareTagBefore(content(i).tags.renameIndex, tag); -- If grTag < tag then diff(high) = '1'
		end loop;
		return res;
	end function;

	function TMP_cmpTagsAfter(content: InstructionStateArray; tag: InsTag)
	return std_logic_vector is
		variable res: std_logic_vector(0 to content'length-1) := (others => '0');
	begin
		for i in 0 to res'length-1 loop
		    -- CAREFUL: correct function, args are swapped
			res(i) := compareTagBefore(tag, content(i).tags.renameIndex); -- If grTag > tag then diff(high) = '1'
		end loop;
		return res;
	end function;


        function TMP_cmpIndexBefore(pStart, pEnd, index: SmallNumber)
        return std_logic_vector is
            variable res: std_logic_vector(0 to content'length-1) := (others => '0');
            variable iv: SmallNumber := (others => '0'); 
        begin
            -- A) if index > start then i >= start && i < index
            -- B) if index < start then i >= start || i < index
            -- C) if index = start then none -> can be coalesced into A):
            -- A') if index >= start then i >= start && i < index    =>    i >= start && i < start   =>   i empty
        
            for i in 0 to res'length-1 loop
                iv := i2slv(i, SMALL_NUMBER_SIZE);
                if cmpGeU(index, pStart) = '1' then
                    res(i) := cmpGeU(iv, pStart) and cmpLtU(iv, index);
                else
                    res(i) := cmpGeU(iv, pStart) or cmpLtU(iv, index);
                end if;
                --res(i) := compareTagBefore(content(i).tags.renameIndex, tag); -- If grTag < tag then diff(high) = '1'
            end loop;
            return res;
        end function;

        function TMP_cmpIndexAfter(pStart, pEnd, index: SmallNumber)
        return std_logic_vector is
            variable res: std_logic_vector(0 to content'length-1) := (others => '0');
            variable iv: SmallNumber := (others => '0'); 
        begin
            -- A) if index > end then i < end || i => index
            -- B) if index < end then i < end && i => index
            -- C) if index = end then all (because in this case start = end and queue is full; otherwise index = end wouldn't be possible)
            --           -> can be coalesced into A):
            -- A') if index >= end then i < end || i => index    =>    i < end || i => end   =>   i all
        
            for i in 0 to res'length-1 loop
                iv := i2slv(i, SMALL_NUMBER_SIZE);
                if cmpGeU(index, pEnd) = '1' then
                    res(i) := cmpLtU(iv, pEnd) or cmpGeU(iv, index);
                else
                    res(i) := cmpLtU(iv, pEnd) and cmpGeU(iv, index);
                end if;
                --res(i) := compareTagBefore(content(i).tags.renameIndex, tag); -- If grTag < tag then diff(high) = '1'
            end loop;
            return res;
        end function;


    
    function whichDataCompleted(content: InstructionStateArray) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        for i in res'range loop
            res(i) := content(i).controlInfo.completed2;
        end loop;
        return res;
    end function;


    function whichAddressCompleted(content: InstructionStateArray) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        for i in res'range loop
            res(i) := content(i).controlInfo.completed;
        end loop;
        return res;
    end function;


    function findNewestMatchIndex2(content: InstructionStateArray;
                                         olderSQ: std_logic_vector; pStart, pEnd: SmallNumber)
    return SmallNumber is
        constant LEN: integer := content'length;      
        variable tmpVec1: std_logic_vector(0 to LEN-1) := (others => '0');
        variable tmpVecExt: std_logic_vector(0 to 2*LEN-1) := (others => '0');
        
        variable res: SmallNumber := (others => '0');
        variable nShift, count: natural := 0;
    begin
        -- Shift by pStart
        nShift := slv2u(pStart);
        count := slv2u(subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE));
        
        tmpVecExt := olderSQ & olderSQ;
        
        for i in 0 to LEN-1 loop
            tmpVec1(i) := tmpVecExt(i + nShift);
        end loop;
        
        -- Find first index
        for i in LEN-1 downto 0 loop
            if tmpVec1(i) = '1' and i < count then
                res := i2slv(i, SMALL_NUMBER_SIZE);
                exit;
            end if;
        end loop;
        -- Add pStart
        res := addTruncZ(res, pStart, QUEUE_PTR_SIZE);
        return res;
    end function;

    function findNewestMatchIndex3(content: InstructionStateArray;
                                         olderSQ: std_logic_vector; pStart, pEnd: SmallNumber;
                                         allow: std_logic)
    return InstructionSlot is
        constant LEN: integer := content'length;
        variable contentExt: InstructionStateArray(0 to 2*content'length-1) := content & content;    
        variable tmpVec1: std_logic_vector(0 to LEN-1) := (others => '0');
        variable tmpVecExt: std_logic_vector(0 to 2*LEN-1) := (others => '0');
        
        variable res: InstructionSlot := ('0', content(LEN-1));
        variable nShift, count, nPos: natural := 0;
    begin
        -- Shift by pStart
        nShift := slv2u(pStart);
        count := slv2u(subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE));
        
        tmpVecExt := olderSQ & olderSQ;
        
        for i in 0 to LEN-1 loop
            tmpVec1(i) := tmpVecExt(i + nShift);
        end loop;
        
        -- Find first index
        for i in LEN-1 downto 0 loop
            if tmpVec1(i) = '1' and i < count then
                res.ins := contentExt(i + nShift);
                res.full := allow;
                exit;
            end if;
        end loop;

        return res;
    end function;


    function findOldestMatchIndex(content: InstructionStateArray;
                                         newerLQ: std_logic_vector; pStart, pEnd: SmallNumber;
                                         cmpInput: InstructionSlot)
    return InstructionSlot is
        constant LEN: integer := content'length;
        variable contentExt: InstructionStateArray(0 to 2*content'length-1) := content & content;     
        variable tmpVec1: std_logic_vector(0 to LEN-1) := (others => '0');
        variable tmpVecExt: std_logic_vector(0 to 2*LEN-1) := (others => '0');
        
        variable res: InstructionSlot := ('0', content(LEN-1));
        variable nShift, count, nPos: natural := 0;
    begin
        -- Shift by pStart
        nShift := slv2u(pStart);
        count := slv2u(subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE));
        
        tmpVecExt := newerLQ & newerLQ;
        
        for i in 0 to LEN-1 loop
            tmpVec1(i) := tmpVecExt(i + nShift);
        end loop;
        
        -- Find first index
        for i in 0 to LEN-1 loop
            if tmpVec1(i) = '1' and i < count then
                res.ins := contentExt(i + nShift);
                res.full := cmpInput.full;
                exit;
            end if;
        end loop;

        return res;
    end function;

     function getNumCommittedEffective(robData: InstructionSlotArray; isLQ: boolean) return SmallNumber is
        variable res: SmallNumber := (others => '0');
        variable k: integer := 0;
        variable found: boolean := false;
     begin
        for i in 0 to PIPE_WIDTH-1 loop
            if robData(i).full = '1' and hasSyncEvent(robData(i).ins) = '1' then
                exit;
            end if;
            
            if isLQ then
                if robData(i).full = '1' and robData(i).ins.classInfo.useLQ = '1' then
                    k := k + 1;
                end if;            
            else
                if robData(i).full = '1' and robData(i).ins.classInfo.secCluster = '1' then
                    k := k + 1;
                end if;
            end if;
         
        end loop;
        return i2slv(k, SMALL_NUMBER_SIZE);
     end function;
     
     function getNumCommitted(robData: InstructionSlotArray; isLQ: boolean) return SmallNumber is
        variable res: SmallNumber := (others => '0');
        variable k: integer := 0;
        variable found: boolean := false;
     begin
        for i in 0 to PIPE_WIDTH-1 loop
            -- A redirected branch cuts a group in SQ, so it must stop there
            if robData(i).ins.controlInfo.newEvent = '1' and hasSyncEvent(robData(i).ins) = '0' then
                exit;
            end if;
        
            -- Not only full, because exceptions clear following 'full' bits
            if isLQ then
                if robData(i).ins.classInfo.useLQ = '1' then
                    k := k + 1;
                end if;
            else
                if robData(i).ins.classInfo.secCluster = '1' then
                    k := k + 1;
                end if;
            end if;
        end loop;
        return i2slv(k, SMALL_NUMBER_SIZE);
     end function;     
      
      function getNewPtr(content: InstructionStateArray; causing: InstructionState; pStart, pEnd: SmallNumber) return SmallNumber is
         constant LEN: natural := content'length;
         variable res: SmallNumber := pEnd;
         variable nShift, count, nPos: natural := 0;
         
         variable tmpVec, tmpVec1, tmpVec2: std_logic_vector(0 to LEN-1) := (others => '0');
         variable tmpVecExt: std_logic_vector(0 to 2*LEN-1) := (others => '0');
                  
      begin
          -- Shift by pStart
          nShift := slv2u(pStart);
          count := slv2u(subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE));
          
          for i in 0 to LEN-1 loop
             tmpVec(i) := compareTagBefore(causing.tags.renameIndex, content(i).tags.renameIndex);
          end loop;

          tmpVecExt := tmpVec & tmpVec;
          
          -- Shift kill mask
          for i in 0 to LEN-1 loop
              tmpVec1(i) := tmpVecExt(i + nShift);
          end loop;         
          
          for i in 0 to LEN-1 loop
              if tmpVec1(i) = '1' and i < count then
                  res := i2slv(i + nShift, SMALL_NUMBER_SIZE);
                  exit;
              end if;
          end loop;
            
         return res and PTR_MASK_SN; 
      end function;
    
    
--    function TMP_getLoadMask(insVec: InstructionSlotArray) return std_logic_vector is
--        variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
--    begin
--        for i in 0 to PIPE_WIDTH-1 loop
--            res(i) := insVec(i).full 
--        end loop;
--        return res;
--    end function;
begin
    causingPtr <= getCausingPtr(content, execCausing);


   --CAREFUL: this is only for SQ
    drainEqual <= bool2std(pDrain = pStart);
    drainEffectiveEqual <= bool2std(pDrain = pStartEffective);       
    nCommitted <= getNumCommitted(robData, IS_LOAD_QUEUE);
    nCommittedEffective <= getNumCommittedEffective(robData, IS_LOAD_QUEUE);


    pStartNext <= addTruncZ(pStart, nCommitted, QUEUE_PTR_SIZE) when committing = '1' else pStart; --pStartNewNext;        
    
    pStartEffectiveNext <= addTruncZ(pStartEffective, nCommittedEffective, QUEUE_PTR_SIZE) when committing = '1'
                        else pStartNext when nowCancelled = '1' and drainEqual = '1'
                        else pStartEffective;

    pDrainNext <= addIntTrunc(pDrain, 1, QUEUE_PTR_SIZE) when drainReq = '1' else pDrain;
    	
	contentNext <=
				getNewContentSQ(content, dataIn,
				                prevSending,
				                pTagged,
				                storeValueInput,
				                compareAddressInput,
				                IS_LOAD_QUEUE, newerLQ);

                pRenamedNext <= pStart when lateEventSignal = '1'
                        else       pFlush when execEventSignal = '1'
                        else       addIntTrunc(pRenamed, slv2u(nInRe), QUEUE_PTR_SIZE) when prevSendingRe = '1'
                        else       pRenamed;


    pFlush <= getNewPtr(content, execCausing, pStart, pTagged);
    storePtr <= getStoreDataIndex(content, storeValueInput);
            
    drainReq <= not drainEqual;--bool2std(pDrain /= pStart);
            
            
	newerLQ <=     newerRegLQ and addressMatchMask and whichAddressCompleted(content) when isStoreMemOp(compareAddressInput.ins) = '1'
	          else (others => '0'); -- Only those with known address
	olderSQ <=     olderRegSQ and addressMatchMask and whichAddressCompleted(content) when isLoadMemOp(compareAddressInput.ins) = '1'
	          else (others => '0'); -- Only those with known address
	
	--newerNextLQ <= TMP_cmpTagsAfter(content, compareTagInput);
	--olderNextSQ <= TMP_cmpTagsBefore(content, compareTagInput);
	
	    newerNextLQ <= newerNextLQ_T;
        olderNextSQ <= olderNextSQ_T;	   
	
	addressMatchMask <= getMatchedAddresses(content, compareAddressInput);       


        newerLQ_T <=     newerRegLQ_T and addressMatchMask and whichAddressCompleted(content) when isStoreMemOp(compareAddressInput.ins) = '1'
                  else (others => '0'); -- Only those with known address
        olderSQ_T <=     olderRegSQ_T and addressMatchMask and whichAddressCompleted(content) when isLoadMemOp(compareAddressInput.ins) = '1'
                  else (others => '0'); -- Only those with known address
        
	WHEN_LQ: if IS_LOAD_QUEUE generate
	       newerNextLQ_T <= tmpTagCmpMask;
	       tmpTagCmpMask <= TMP_cmpIndexAfter(pStart, pTagged, compareIndexInput);
	
	       nInRe <= i2slv(countOnes(getLoadMask(TMP_recodeMem(dataInRe))), SMALL_NUMBER_SIZE);
	   selectedDataSlot <= findOldestMatchIndex(content, newerLQ, pStart, pTagged, compareAddressInput);	       
	end generate;
	
	WHEN_SQ: if not IS_LOAD_QUEUE generate
	       olderNextSQ_T <= tmpTagCmpMask;
	       tmpTagCmpMask <= TMP_cmpIndexBefore(pStart, pTagged, compareIndexInput);
	
	       nInRe <= i2slv(countOnes(getStoreMask(TMP_recodeMem(dataInRe))), SMALL_NUMBER_SIZE);	   
	    -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
       pSelect <=   findNewestMatchIndex2(content, olderSQ,  pDrainPrev, pTagged);
       selectedDataSlot <=   findNewestMatchIndex3(content, olderSQ,  pDrainPrev, pTagged, compareAddressInput.full);	   
	end generate;
	
	       --ch0 <= bool2std(TMP_selectedSlot = selectedDataSlot) or not selectedDataSlot.full;
	       --ch1 <= bool2std(nCommitted = nOut) or not isSending;
	       --ch2 <= bool2std(memEmpty /= isNonzero(taggedMask));
	       --ch3 <= bool2std(pFlush = pFlush_C);

	process (clk)
	begin
		if rising_edge(clk) then
			content <= contentNext;

            isDrainingPrev <= isDraining;
        
            newerRegLQ <= newerNextLQ;
            olderRegSQ <= olderNextSQ;
                newerRegLQ_T <= newerNextLQ_T;
                olderRegSQ_T <= olderNextSQ_T;
			
			selectedDataOutputSig <= selectedDataSlot;
            
            if storeValueInput.full = '1' then
                storeValues(slv2u(storePtr)) <= storeValueInput.ins.result;
            end if;
            
            pDrain <= pDrainNext;
            pDrainPrev <= pDrain;
            
            if drainReq = '1' then
                drainValue <= storeValues(slv2u(pDrain));
                drainData <= content(slv2u(pDrain));
            end if;
            
            if true then    
                selectedValue <= storeValues(slv2u(pSelect));
            end if;
        
            pStart <= pStartNext;
            pStartEffective <= pStartEffectiveNext;
            
            if drainEqual = '0' and drainEffectiveEqual = '1' then
                nowCancelled <= '1';
            end if;
            
            if nowCancelled = '1' and drainEqual = '1' then
                nowCancelled <= '0';
            end if;
            
            allowDrain <= not (nowCancelled or (not drainEqual and drainEffectiveEqual));
                
                
            if lateEventSignal = '1' then
                pTagged <= pStartNext;
            elsif execEventSignal = '1' then
               pTagged <= pFlush;
            elsif prevSending = '1' then -- + N
                pTagged <= addIntTrunc(pTagged, countOnes(extractFullMask(dataIn)), QUEUE_PTR_SIZE);
            end if;
            
                pRenamed <= pRenamedNext;
            
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
	        
	        recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
	        
	        isFull <= cmpGtU(nFullNext, QUEUE_SIZE-4);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE-8);

	        nFull <= nFullNext;
            
            -- TODO: include execEventSignal in both cases!
            if not IS_LOAD_QUEUE then -- SQ
               if prevSending = '1' and isNonzero(extractFullMask(dataIn)) = '1' then 
                  memEmpty <= '0';
               end if;
                
               if lateEventSignal = '1' and pStart = pDrainPrev then
                  memEmpty <= '1';
               end if;
               
               if execEventSignal = '1' and pFlush = pStartNext then -- if execEventSignal, content can't grow
                  memEmpty <= '1';
               end if;
               
	           if isDrainingPrev = '1' and (prevSending = '0' or isNonzero(extractFullMask(dataIn)) = '0') and pDrain = pTagged then
	              memEmpty  <= '1';
	           end if;
             
             else -- LQ
                if prevSending = '1' and isNonzero(extractFullMask(dataIn)) = '1' then 
                   memEmpty <= '0';
                end if;
                 
                if lateEventSignal = '1' then
                   memEmpty <= '1';
                end if;

                if execEventSignal = '1' and pFlush = pStartNext then -- if execEventSignal, content can't grow
                   memEmpty <= '1';
                end if;
                
                if committing = '1' and (prevSending = '0' or isNonzero(extractFullMask(dataIn)) = '0') and pStartNext = pTagged then
                   memEmpty  <= '1';
                end if;	           
	        end if;
		end if;
	end process;

	nFullNext <=  nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
				else subTruncZ(add(nFull, nIn), nOut, QUEUE_CAP_SIZE);
	nIn <= i2slv( countOnes(extractFullMask(dataIn)), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');
		
	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
        nOut <= nCommitted when committing = '1'
                else (others => '0');
        nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pTagged and memEmpty = '0'
                           else subTruncZ(pTagged, pStartNext, QUEUE_PTR_SIZE); -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch 
    end generate;
    
    STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isDrainingPrev = '1'
              else (others => '0');		  
        nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pDrainPrev = pTagged and memEmpty = '0'
                            else subTruncZ(pTagged, pDrain, QUEUE_PTR_SIZE); -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch
    end generate;
    
    isDraining <= drainReq;
     

	acceptingOut <= not isFull;
	almostFull <= isAlmostFull;

        renamedPtr <= pRenamed;

    COMMIT_OUTPUTS: if VIEW_ON generate
	   --dataOutV <= dataOutSigFinal;	
	   --sendingSQOut <= isSending;
    end generate;

	selectedDataOutput <= (selectedDataOutputSig.full, setInstructionResult(selectedDataOutputSig.ins, selectedValue)) when not IS_LOAD_QUEUE
	               else    selectedDataOutputSig;
	
	committedEmpty <= bool2std(pStart = pDrainPrev);
	committedSending <= isDrainingPrev;
	                       
	committedDataOut(1 to PIPE_WIDTH-1) <= (others => DEFAULT_INSTRUCTION_SLOT);
	committedDataOut(0) <= (isDrainingPrev and allowDrain, setInstructionResult(drainData, drainValue));
	               	               
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
       signal DUMMY_mask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '1');
    begin       
       queueText <= getInsStringArray(makeSlotArray(content, DUMMY_mask), transfer);
    end generate;
	
end Behavioral;
