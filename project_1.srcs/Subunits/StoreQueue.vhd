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
		
		prevSending: in std_logic;
		--	prevSendingBr: in std_logic;
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		--	dataInBr: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		storeValueInput: in InstructionSlot;
		compareAddressInput: in InstructionSlot;
        compareTagInput:    in InsTag;

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

	signal isSending, isDraining: std_logic := '0';							

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, taggedMask, killMask, livingMask, frontMask, sendingMask, inputMask, drainMask, drainMaskNext,
			 committedMask, committedMaskNext, fullMaskNext, taggedMaskNext, sqCmpMask, lqCmpMask, addressMatchMask,
			 cancelMask, cancelledMask, cancelledMaskNext,
			 scMask, drainMaskNC: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
	
	signal taggedLivingMask, fullOrCommittedMask, matchedMask,
	           newerLQ, olderSQ, newerRegLQ, olderRegSQ, newerNextLQ, olderNextSQ: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot, selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	signal dataOutSig, dataOutSigNext, dataOutSigFinal, dataDrainSig, dataDrainSigNC: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	
	signal pStart, pStartNext, pDrain, pDrainNext, pTagged, pAll, causingPtr: SmallNumber := (others => '0');	
	   signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0');
	   signal recoveryCounter: SmallNumber := (others => '0');
	   signal isFull, isAlmostFull: std_logic := '0'; 	

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
                                taggedMask: std_logic_vector;
				                prevSending: std_logic;
				                inputMask: std_logic_vector;
				                pTagged: SmallNumber;
				                storeValueInput, storeAddressInput: InstructionSlot;
				                isLQ: boolean; matchingNewerLoads: std_logic_vector
				                )
    return InstructionStateArray is
        variable res: InstructionStateArray(0 to QUEUE_SIZE-1) := content;
        variable slot: InstructionSlot;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    variable im: std_logic_vector(0 to QUEUE_SIZE-1) := inputMask;
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
           
           if im(i) = '1' then
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
           if taggedMask(i) = '1' -- !! Prevent instruction with r.i. = 0 form updating untagged entries! 
               and content(i).tags.renameIndex = storeValueInput.ins.tags.renameIndex
               and storeValueInput.full = '1'
           then
               res(i).controlInfo.completed2 := '1'; -- data completed
               res(i).result := storeValueInput.ins.result;
           end if;
           
           if taggedMask(i) = '1' -- !! Prevent instruction with r.i. = 0 form updating untagged entries! 
               and content(i).tags.renameIndex = storeAddressInput.ins.tags.renameIndex
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
    
    function getStoreDataIndex(content: InstructionStateArray; dataIn: InstructionSlotArray;
                                taggedMask: std_logic_vector;
				                prevSending: std_logic;
				                inputMask: std_logic_vector;
				                pTagged: SmallNumber;
				                storeValueInput, storeAddressInput: InstructionSlot;
				                isLQ: boolean; matchingNewerLoads: std_logic_vector
				                )
    return natural is
        variable res: InstructionStateArray(0 to QUEUE_SIZE-1) := content;
        variable slot: InstructionSlot;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    variable im: std_logic_vector(0 to QUEUE_SIZE-1) := inputMask;
	    constant IS_STORE_OP: boolean := std2bool(isStoreOp(storeAddressInput.ins));
	    constant IS_LOAD_OP: boolean := std2bool(isLoadOp(storeAddressInput.ins));
    begin
        -- Update target after mem execution
        for i in 0 to QUEUE_SIZE-1 loop
           if content(i).tags.renameIndex = storeValueInput.ins.tags.renameIndex
           then
               return i;
           end if;
                                
        end loop;

        return 0;
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
    
		-- To find what to forward from StoreQueue
    function findNewestMatch(content: InstructionStateArray;
                                     olderSQ, cmpMask: std_logic_vector; pStart: SmallNumber;
                                     ins: InstructionState)
    return std_logic_vector is
        constant LEN: integer := cmpMask'length;        
        variable res, res1, res2, older, before: std_logic_vector(0 to LEN-1) := (others => '0');
        variable indices, rawIndices: SmallNumberArray(0 to LEN-1) := (others => (others => '0'));
        variable matchBefore: std_logic := '0';       
        variable tmpVec, tmpVec1, tmpVec2: std_logic_vector(0 to LEN-1) := (others => '0');
    begin
        -- From qs we must check which are older than ins
        --older := olderSQ;-- TMP_cmpTagsBefore(content, ins.tags.renameIndex) and whichAddressCompleted(content); -- only those with known address
        before := setToOnes(olderSQ, slv2u(pStart));
        -- Use priority enc. to find last in the older ones. But they may be divided:
        --        V  1 1 1 0 0 0 0 1 1 1 and cmp  V
        --           0 1 0 0 0 0 0 1 0 1
        -- and then there are 2 runs of bits and those at the enc must be ignored (r older than first run)
        
        -- If there's a match before pStart, it is younger than those at or after pStart
        tmpVec1 := cmpMask and olderSQ;
        tmpVec2 := cmpMask and olderSQ and before;
            tmpVec := cmpMask and olderSQ and before;
        matchBefore := isNonzero(tmpVec2);
        
        res1 := invertVec(getFirstOne(invertVec(tmpVec1)));
        res2 := invertVec(getFirstOne(invertVec(tmpVec2)));
        
        if matchBefore = '1' then -- Ignore those after            
            res := res2;
        else -- Don't ignore any matches           
            res := res1;
        end if;
        
        return res;
    end function;

    -- To check what in the LoadQueue has an error
    function findOldestMatch(content: InstructionStateArray;
                                     newerLQ, cmpMask: std_logic_vector; pStart: SmallNumber;
                                     ins: InstructionState)
    return std_logic_vector is
        constant LEN: integer := cmpMask'length;
        variable res, newer, areAtOrAfter: std_logic_vector(0 to LEN-1) := (others => '0');
        variable indices, rawIndices: SmallNumberArray(0 to LEN-1) := (others => (others => '0'));
        variable matchAtOrAfter: std_logic := '0';
        
        variable tmpVec: std_logic_vector(0 to LEN-1) := (others => '0');
    begin
        -- From qs we must check which are newer than ins
        newer := newerLQ;-- TMP_cmpTagsAfter(content, ins.tags.renameIndex) and whichAddressCompleted(content); -- Only those with known address 
        areAtOrAfter := not setToOnes(newer, slv2u(pStart));
        -- Use priority enc. to find first in the newer ones. But they may be divided:
        --		V  1 1 1 0 0 0 0 1 1 1 and cmp  V
        --		   0 1 0 0 0 0 0 1 0 1
        -- and then there are 2 runs of bits and those at the enc must be ignored (r newer than first run)
        
        -- So, elems at the end are ignored when those conditions cooccur:
        --		pStart > ins.groupTag and [match exists that match.groupTag < ins.groupTag]
        tmpVec := cmpMask and newer and areAtOrAfter;
        matchAtOrAfter := isNonzero(tmpVec);
        
        if matchAtOrAfter = '1' then
            -- Ignore those before
            tmpVec := cmpMask and newer and areAtOrAfter;
            res := getFirstOne(tmpVec);
        else
            -- Don't ignore any matches
            tmpVec := cmpMask and newer;
            res := getFirstOne(tmpVec);
        end if;
        
        return res;
    end function;
    
    function selectWithMask(content: InstructionStateArray; mask: std_logic_vector; compareValid: std_logic) return InstructionSlot is
        variable res: InstructionSlot := ('0', content(0));
    begin       
        for i in 0 to content'length-1 loop
            if mask(i) = '1' then
                res := (compareValid, content(i));
                exit;
            end if;
        end loop;
        
        return res;
    end function;
    
        signal matchIndex, storeIndex: natural range 0 to QUEUE_SIZE-1 := 0;
        --signal doUpdate: std_logic := '0';
        signal storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
        signal TMP_drain: std_logic := '0';
        signal TMP_drainPtr, TMP_selectPtr: natural := 0;
        signal TMP_drainValue, TMP_selectValue: Mword := (others => '0');
        
        
        signal ch0, ch1, ch2, ch3: std_logic := '0';
begin
    causingPtr <= getCausingPtr(content, execCausing);
    
	-- in shifting queue this would be shfited by nSend
	frontMask <= getSendingMask(content, taggedLivingMask, groupCtrInc);
	cancelMask <= getCancelMask(content, taggedLivingMask, groupCtrInc, robData);
	scMask <= sendingMask and cancelMask;
	
	cancelledMaskNext <= (cancelledMask and not drainMask) or scMask;
	
	sendingMask <= frontMask when committing = '1' else (others => '0');

	killMask <= getKillMask(content, taggedMask, execCausing, execEventSignal, lateEventSignal);
	livingMask <= fullMask when (lateEventSignal = '0' and execEventSignal = '0') else taggedLivingMask;
    taggedLivingMask <= taggedMask and not killMask;
				
    inputMask <= getInputMask(taggedMask, extractFullMask(dataIn), prevSending, pTagged, PTR_MASK_SN);

	taggedMaskNext <= (taggedLivingMask and not sendingMask) or inputMask;
	committedMaskNext <= (committedMask or sendingMask) and not drainMask;

	fullOrCommittedMask <= taggedMask or committedMask;
	   
	-- TODO: this won't work if the queue is allowed to become full of 'committed'. If it could, change to [set '1' on drainP when startP ~= drainP]
	drainMask <= committedMask and not (committedMask(QUEUE_SIZE-1) & committedMask(0 to QUEUE_SIZE-2)); -- First '1' bit of committedMask
	
	contentNext <=
				getNewContentSQ(content, dataIn,
				                taggedMask,
				                prevSending,
				                inputMask,				             
				                pTagged,
				                storeValueInput,
				                compareAddressInput,
				                IS_LOAD_QUEUE, newerLQ
				                                    );
            storeIndex <= 				getStoreDataIndex(content, dataIn,
                                                                    taggedMask,
                                                                    prevSending,
                                                                    inputMask,                             
                                                                    pTagged,
                                                                    storeValueInput,
                                                                    compareAddressInput,
                                                                    IS_LOAD_QUEUE, newerLQ
                                                                                        );
            
            TMP_drain <= bool2std(TMP_drainPtr /= slv2u(pStart));
            
                ch0 <= bool2std(isDraining = TMP_drain);
                ch1 <= bool2std(TMP_drainValue = dataDrainSig(0).ins.result) or not dataDrainSig(0).full;
                ch2 <= bool2std(TMP_selectValue = selectedDataOutputSig.ins.result) or not selectedDataOutputSig.full;
                 
            
    drainMaskNC <= drainMask and not cancelledMask;
 
	dataDrainSig <= getWindow(content, drainMask, pDrain, PIPE_WIDTH);				                
	dataDrainSigNC <= getWindow(content, drainMaskNC, pDrain, PIPE_WIDTH);	
    dataOutSigNext <= getWindow(content, taggedMask, pStartNext, PIPE_WIDTH);

	newerLQ <=     newerRegLQ and addressMatchMask and whichAddressCompleted(content) when isStoreMemOp(compareAddressInput.ins) = '1'
	          else (others => '0'); -- Only those with known address
	olderSQ <=     olderRegSQ and addressMatchMask and whichAddressCompleted(content) when isLoadMemOp(compareAddressInput.ins) = '1'
	          else (others => '0'); -- Only those with known address
	
	   newerNextLQ <= TMP_cmpTagsAfter(content, compareTagInput);
	   olderNextSQ <= TMP_cmpTagsBefore(content, compareTagInput);
	
	addressMatchMask <= getMatchedAddresses(content, compareAddressInput);	
	matchedMask <= findOldestMatch(content, newerLQ, taggedMask,           pStart, compareAddressInput.ins) when IS_LOAD_QUEUE 
	                                                 -- TODO: above - not necessary to find oldest, each younger load can be "poisoned" and cause an event on Commit
	         else  findNewestMatch(content, olderSQ, fullOrCommittedMask,  pStart, compareAddressInput.ins);
	
	   TMP_selectPtr <= getFirstOnePosition(matchedMask);
	
	selectedDataSlot <= selectWithMask(content, matchedMask, compareAddressInput.full); -- Not requiring that it be a load (for SQ) (overlaping stores etc.)
    pStartNext <= addIntTrunc(pStart, getNumberToSend(dataOutSig, groupCtrInc, committing), QUEUE_PTR_SIZE);
    pDrainNext <= pDrain when isDraining = '0' else addIntTrunc(pDrain, 1, QUEUE_PTR_SIZE);
            	
	process (clk)
	begin
		if rising_edge(clk) then
            fullMask <= fullMaskNext;
            taggedMask <= taggedMaskNext;
			content <= contentNext;
	        committedMask <= committedMaskNext;
			
            cancelledMask <= cancelledMaskNext;
        
            newerRegLQ <= newerNextLQ;
            olderRegSQ <= olderNextSQ;
			
			selectedDataOutputSig <= selectedDataSlot;
            dataOutSig <= dataOutSigNext;
            
                if storeValueInput.full = '1' then
                    storeValues(storeIndex) <= storeValueInput.ins.result;
                end if;
            
                if TMP_drain = '1' then
                    TMP_drainValue <= storeValues(TMP_drainPtr);
                    TMP_drainPtr <= (TMP_drainPtr + 1) mod QUEUE_SIZE;
                end if;
                
                if true then    
                    TMP_selectValue<= storeValues(TMP_selectPtr mod QUEUE_SIZE);
                end if;
            
            pDrain <= pDrainNext;        
            pStart <= pStartNext;
            
            if lateEventSignal = '1' then
                pTagged <= pStartNext;
            elsif execEventSignal = '1' then
                pTagged <= addIntTrunc(pTagged, -countOnes(killMask), QUEUE_PTR_SIZE);
            elsif prevSending = '1' then -- + N
                pTagged <= addIntTrunc(pTagged, countOnes(inputMask), QUEUE_PTR_SIZE);
            end if;

            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
	        
	           recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
	        
	        isFull <= cmpGtU(nFullNext, QUEUE_SIZE-4);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE-8);

	        nFull <= nFullNext;	           
		end if;
	end process;

	nFullNext <=  nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
				else subTruncZ(add(nFull, nIn), nOut, QUEUE_CAP_SIZE);
	nIn <= i2slv( countOnes(inputMask), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');
		
	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
        nOut <= i2slv(countOnes(extractFullMask(dataOutSigFinal)), SMALL_NUMBER_SIZE) when isSending = '1'
                else (others => '0');	
        nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pTagged and fullMask(0) = '1'
                           else subTruncZ(pTagged, pStartNext, QUEUE_PTR_SIZE); -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch 
    end generate;
    
    STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isDraining = '1'
              else (others => '0');		  
        nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pDrainNext = pTagged and fullMask(0) = '1' 
                            else subTruncZ(pTagged, pDrainNext, QUEUE_PTR_SIZE); -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch
    end generate;
    
    isDraining <= dataDrainSig(0).full;
    dataOutSigFinal <= getSendingArray(dataOutSig, groupCtrInc, committing);
     
    isSending <= dataOutSigFinal(0).full;	

	acceptingOut <= not isFull;
	almostFull <= isAlmostFull;

	dataOutV <= dataOutSigFinal;	
	sendingSQOut <= isSending;

	selectedDataOutput <= --selectedDataOutputSig;
	                       (selectedDataOutputSig.full, setInstructionResult(selectedDataOutputSig.ins, TMP_selectValue));
	
	committedEmpty <= not isNonzero(committedMask);
	committedSending <= isDraining;
	--committedDataOut <= (0 => dataDrainSigNC(0), others => DEFAULT_INSTRUCTION_SLOT);
	committedDataOut(1 to PIPE_WIDTH-1) <= (others => DEFAULT_INSTRUCTION_SLOT);
	committedDataOut(0) <= (dataDrainSigNC(0).full, --setInstructionResult(dataDrainSigNC(0).ins, TMP_drainValue));
	                                                dataDrainSigNC(0).ins); -- TODO
	
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       queueText <= getInsStringArray(makeSlotArray(content, fullOrCommittedMask), transfer);
    end generate;
	
end Behavioral;
