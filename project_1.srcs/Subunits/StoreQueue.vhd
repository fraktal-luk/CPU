----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:57:56 12/11/2016 
-- Design Name: 
-- Module Name:    MemoryUnit - Behavioral 
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
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;


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

		selectedDataOutput: out InstructionSlot;

		committing: in std_logic;
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
	signal isSending, isDraining, isSending_T: std_logic := '0';							

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1)
															:= (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, taggedMask, killMask, livingMask, frontMask, drainMask, sendingMask, drainMaskNext, inputMask,
			 committedMask, committedMaskNext, fullMaskNext, taggedMaskNext: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
	
	signal taggedLivingMask, fullOrCommittedMask, matchedMask, newerLQ, olderSQ: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	signal selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;

	signal pDrain, pStart, pDrainNext, pStartNext, pStartNext_T, pTagged, pAll, causingPtr, pAcc, pAccMore: SmallNumber := (others => '0');
	
	signal dataOutSig, dataOutSigOld, dataOutSigFinal, dataOutSigNext: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	signal dataDrainSig: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
	
	   signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0');
	   signal recoveryCounter: SmallNumber := (others => '0');
	   signal isFull, isAlmostFull: std_logic := '0'; 	

	
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
	
	function getCausingPtr(content: InstructionStateArray; causing: InstructionState) return SmallNumber is
	   variable res: SmallNumber := (others => '0');
	begin
	   for i in content'range loop
	       if content(i).tags.renameIndex = causing.tags.renameIndex then
	           res := i2slv(i, SMALL_NUMBER_SIZE);
	           exit;
	       end if;
	   end loop;
	   
	   return res;
	end function;
	
	function getInputMask(mask, newMask: std_logic_vector; prevSending: std_logic; ptr: SmallNumber)
	return std_logic_vector is
	   constant LEN: natural := mask'length;
	   variable newMaskComp: std_logic_vector(0 to newMask'length-1) := compactMask(newMask);
	   variable res: std_logic_vector(0 to LEN-1) := (others => '0');
	   variable remainingMaskExt: std_logic_vector(0 to LEN + 4 - 1) := (others => '0');
	   variable remv: std_logic_vector(0 to 3) := "0000";
	   variable pLoc: natural := slv2u(ptr);
	   variable diff: SmallNumber := (others => '0');
	begin
	   
       remainingMaskExt(4 to LEN + 3) := mask;
       remainingMaskExt(0 to 3) := (others => '1');
       
       if prevSending = '0' then
           return res; -- Stays empty
       end if;
       
       for i in 0 to LEN-1 loop
           diff := subSN(i2slv(i, SMALL_NUMBER_SIZE), ptr) and PTR_MASK_SN;
           if slv2u(diff) >= 4 then
               remv := (others => '0');
           else
               case diff(1 downto 0) is
                   when "00" =>
                        remv := "1111";
                   when "01" =>
                        remv := "1110";
                   when "10" =>
                        remv := "1100";
                   when others =>
                        remv := "1000";                                                                                       
               end case;
           end if;
           --             report integer'image(slv2u(remv));
           --remv := remainingMaskExt(i+1 to i+3);
           
           res(i) := '0';
           for k in 0 to 3 loop -- Further beyond end requires more ful inputs to be filled:
               --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
               res(i) := res(i) or (remv(3-k) and newMaskComp(k));
           end loop;           	   
       end loop;
       
	   return res;
	end function;
	
	function getSendingMask(content: InstructionStateArray; mask: std_logic_vector;
	                tag: InsTag) return std_logic_vector is
	   variable res: std_logic_vector(0 to content'length-1) := (others => '0');
	begin
	   for i in 0 to QUEUE_SIZE-1 loop
           if getTagHighSN(content(i).tags.renameIndex) = getTagHighSN(tag) then
             res(i) := mask(i);
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
        --variable sel: natural range 0 to 3 := 0;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    --variable compMask, compMaskBr: std_logic_vector(0 to 3) := "0000";
	    variable im: std_logic_vector(0 to QUEUE_SIZE-1) := inputMask;
	    constant isStoreOp: boolean := (storeAddressInput.ins.operation = (Memory, store) or storeAddressInput.ins.operation = (System, sysMtc));
	    constant isLoadOp: boolean := (storeAddressInput.ins.operation = (Memory, load) or storeAddressInput.ins.operation = (System, sysMfc));
    begin
        --compMask := compactMask(extractFullMask(dataIn));
        --compMaskBr := compactMask(extractFullMask(dataInBr));

        for i in 0 to QUEUE_SIZE-1 loop
        
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pTagged) and PTR_MASK_SN;
           --sel := slv2u(diff(1 downto 0));
           
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
           
           --sel := slv2u(getSelector(remv, extractFullMask(dataIn)(0 to 2)));
           if im(i) = '1' then
               --res(i).tags := dataIn(sel).ins.tags;
               --res(i).operation := dataIn(sel).ins.operation;
                    slot := getNewElem(remv, dataIn);
                    res(i) := slot.ins;          
                    res(i).tags := slot.ins.tags;
                    res(i).operation := slot.ins.operation;
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
               res(i).controlInfo.completed2 := '1'; -- address completed
               res(i).result := storeValueInput.ins.result;
           end if;
           
           if taggedMask(i) = '1' -- !! Prevent instruction with r.i. = 0 form updating untagged entries! 
               and content(i).tags.renameIndex = storeAddressInput.ins.tags.renameIndex
               and storeAddressInput.full = '1'
               and ((isStoreOp and not isLQ) or (isLoadOp and isLQ))
           then
               res(i).controlInfo.completed := '1'; -- data completed           
               res(i).target := storeAddressInput.ins.result;
                res(i).controlInfo.orderViolation := '0';
           end if;                      
        end loop;

        return res;
    end function;
    
    function getWindow(content: InstructionStateArray; mask: std_logic_vector;
                        p: SmallNumber; n: natural)
    return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to n-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        variable pContent, pWindow: natural := 0;
    begin
        pContent := slv2u(p);
        pWindow := 0;
        
        for i in 0 to n-1 loop
            pContent := slv2u(p) + i;
            if pContent >= content'length then
                pContent := pContent - content'length;
            end if;
            res(i).ins := content(pContent);
            res(i).full := mask(pContent);
        end loop;
        
        return res;
    end function;
    
    function selectDataSlot(content: InstructionStateArray; taggedMask: std_logic_vector;
                            compareAddressInput: InstructionSlot)
    return InstructionSlot is 
        variable res: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;    
    begin
        for i in 0 to QUEUE_SIZE-1 loop 
            res.ins := content(i);
            if content(i).tags.renameIndex = compareAddressInput.ins.tags.renameIndex
                and compareAddressInput.full = '1' and taggedMask(i) = '1'
            then
                res.full := '1';
                exit;
            end if;
        end loop;
        return res;
    end function;


	function TMP_cmpTagsBefore(content: InstructionStateArray; tag: InsTag)
	return std_logic_vector is
		variable res: std_logic_vector(0 to content'length-1) := (others => '0');
		variable diff: SmallNumber := (others => '0');
	begin
		for i in 0 to res'length-1 loop
			res(i) := CMP_tagBefore(content(i).tags.renameIndex, tag); -- If grTag < tag then diff(high) = '1'
		end loop;
		return res;
	end function;

	function TMP_cmpTagsAfter(content: InstructionStateArray; tag: InsTag)
	return std_logic_vector is
		variable res: std_logic_vector(0 to content'length-1) := (others => '0');
		variable diff: SmallNumber := (others => '0');
	begin
		for i in 0 to res'length-1 loop
		    -- CAREFUL: correct function, args are swapped
			res(i) := CMP_tagBefore(tag, content(i).tags.renameIndex); -- If grTag > tag then diff(high) = '1'
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
        variable res, older, before: std_logic_vector(0 to LEN-1) := (others => '0');
        variable indices, rawIndices: SmallNumberArray(0 to LEN-1) := (others => (others => '0'));
        variable matchBefore: std_logic := '0';
        
        variable tmpVec: std_logic_vector(0 to LEN-1) := (others => '0');
    begin
        -- From qs we must check which are older than ins
        --indices := getQueueIndicesFrom(LEN, pStart);
        --rawIndices := getQueueIndicesFrom(LEN, (others => '0'));
        older := olderSQ;-- TMP_cmpTagsBefore(content, ins.tags.renameIndex) and whichAddressCompleted(content); -- only those with known address
        before := setToOnes(older, slv2u(pStart));
        -- Use priority enc. to find last in the older ones. But they may be divided:
        --        V  1 1 1 0 0 0 0 1 1 1 and cmp  V
        --           0 1 0 0 0 0 0 1 0 1
        -- and then there are 2 runs of bits and those at the enc must be ignored (r older than first run)
        
        -- If there's a match before pStart, it is younger than those at or after pStart
        tmpVec := cmpMask and older and before;
        matchBefore := isNonzero(tmpVec);
        
        if matchBefore = '1' then
            -- Ignore those after
            tmpVec := cmpMask and older and before;
            res := invertVec(getFirstOne(invertVec(tmpVec)));
        else
            -- Don't ignore any matches
            tmpVec := cmpMask and older;
            res := invertVec(getFirstOne(invertVec(tmpVec)));
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
    
    
    function getNumberToSendSQ(dataOutSig: InstructionSlotArray(0 to PIPE_WIDTH-1); nextCommitTag: InsTag; committing: std_logic) return integer is
       variable res: integer := 0;
    begin
       if getTagHighSN(dataOutSig(0).ins.tags.renameIndex) /= getTagHighSN(nextCommitTag) or committing = '0' then
           return 0;
       end if;        
       
       -- So there's a matching tag. Count full slots up to a 'redirect' mark or stop when new 'start' mark is met
       -- (the first 'start' mark on elem 0 is there always and we ignore it!
       for i in 0 to PIPE_WIDTH-1 loop
           if dataOutSig(i).full = '0' then
               exit;
           end if;
           
           if dataOutSig(i).ins.controlInfo.firstBr = '1' and i /= 0 then
               exit;
           end if;           
           
           res := res + 1;
           
           if dataOutSig(i).ins.controlInfo.newEvent = '1' then
               exit;
           end if;  
       end loop;
       
       return res;
    end function;

    function getSendingArray(dataOutSig: InstructionSlotArray(0 to PIPE_WIDTH-1); nextCommitTag: InsTag; committing: std_logic) return InstructionSlotArray is
       variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := dataOutSig;
    begin
       for i in 0 to PIPE_WIDTH-1 loop
           res(i).full := '0';
       end loop;
    
       if getTagHighSN(dataOutSig(0).ins.tags.renameIndex) /= getTagHighSN(nextCommitTag) or committing = '0' then
           return res;
       end if;        
       
       -- So there's a matching tag. Count full slots up to a 'redirect' mark or stop when new 'start' mark is met
       -- (the first 'start' mark on elem 0 is there always and we ignore it!
       for i in 0 to PIPE_WIDTH-1 loop
           if dataOutSig(i).full = '0' then
               exit;
           end if;
           
           if dataOutSig(i).ins.controlInfo.firstBr = '1' and i /= 0 then
               exit;
           end if;           
           
           res(i).full := '1';
           
           if dataOutSig(i).ins.controlInfo.newEvent = '1' then
               exit;
           end if;  
       end loop;
       
       return res;
    end function;
  
begin

    causingPtr <= getCausingPtr(content, execCausing);
    
	-- in shifting queue this would be shfited by nSend
	frontMask <= getSendingMask(content, taggedLivingMask, groupCtrInc);
	sendingMask <= frontMask when committing = '1' else (others => '0');

	killMask <= getKillMask(content, taggedMask, execCausing, execEventSignal, lateEventSignal);
	livingMask <= fullMask when (lateEventSignal = '0' and execEventSignal = '0') 
	         else taggedLivingMask;	
    taggedLivingMask <= taggedMask and not killMask;
				
    inputMask <= getInputMask(taggedMask, extractFullMask(dataIn), prevSending, pTagged);				
    --inputMaskBr <= getInputMask(fullMask, extractFullMask(dataInBr), prevSendingBr, pAll);
				
	--fullMaskNext <= (livingMask and not sendingMask) or inputMaskBr;
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
				                (compareAddressInput.full, compareAddressInput.ins),
				                IS_LOAD_QUEUE, newerLQ
				                                    );

	dataDrainSig <= getWindow(content, drainMask, pDrain, 1);				                
	dataOutSigOld <= getWindow(content, frontMask, pStart, PIPE_WIDTH);

        dataOutSigNext <= getWindow(content, taggedMask, pStartNext_T, PIPE_WIDTH);

	newerLQ <= TMP_cmpTagsAfter(content, compareAddressInput.ins.tags.renameIndex) and whichAddressCompleted(content) when compareAddressInput.ins.operation = (Memory, store)
	                   else (others => '0'); -- Only those with known address
	olderSQ <= TMP_cmpTagsBefore(content, compareAddressInput.ins.tags.renameIndex) and whichAddressCompleted(content) when compareAddressInput.ins.operation = (Memory, load)
	                   else (others => '0'); -- Only those with known address
	
	matchedMask <= findOldestMatch(content, newerLQ, taggedMask,          pStart, compareAddressInput.ins) when IS_LOAD_QUEUE 
	                                                 -- TODO: above - not necessary to find oldest, each younger load can be "poisoned" and cause an event on Commit
	         else  findNewestMatch(content, olderSQ, fullOrCommittedMask, pStart, compareAddressInput.ins);
	
	selectedDataSlot <= selectWithMask(content, matchedMask, compareAddressInput.full); -- Not requiring that it be a load (for SQ) (overlaping stores etc.)
	                           --selectDataSlot(content, taggedMask, compareAddressInput);

            --if isSending = '1' then
                pStartNext <= --pStart when isSending = '0' else addSN(pStart, i2slv(countOnes(extractFullMask(dataOutSigOld)), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
                                pStartNext_T;
            --end if;
                pStartNext_T <= addSN(pStart, i2slv(getNumberToSendSQ(dataOutSig, groupCtrInc, committing), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;

            pDrainNext <= pDrain when isDraining = '0' else addSN(pDrain, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            	
	process (clk)
	begin
		if rising_edge(clk) then
--			TMP_mask <= TMP_maskNext;
            fullMask <= fullMaskNext;
            taggedMask <= taggedMaskNext;
			content <= contentNext;
	        committedMask <= committedMaskNext;
			
			selectedDataOutputSig <= selectedDataSlot;--(selectedSendingSig, selectedData);

            dataOutSig <= dataOutSigNext;

            pDrain <= pDrainNext;
            
--            if isSending = '1' then
--                pStart <= addSN(pStart,
--                             i2slv(countOnes(extractFullMask(dataOutSig)), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
--            end if;
            pStart <= pStartNext;
            
            if lateEventSignal = '1' then
                pTagged <= pStartNext;
            elsif execEventSignal = '1' then
                pTagged <= subSN(pTagged, i2slv(countOnes(killMask), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            elsif prevSending = '1' then -- + N
                pTagged <= addSN(pTagged, i2slv(countOnes(inputMask), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;


            
            
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif recoveryCounter /= i2slv(0, SMALL_NUMBER_SIZE) then
                recoveryCounter <= subSN(recoveryCounter, i2slv(1, SMALL_NUMBER_SIZE));
            end if;
	        
	        nFull <= nFullNext;
	        
            if --nFullNext > QUEUE_SIZE-4 then
                cmpGreaterUnsignedSN(nFullNext, i2slv(QUEUE_SIZE-4, SMALL_NUMBER_SIZE)) = '1' then
                isFull <= '1';
                isAlmostFull <= '1';
            elsif --nFullNext > QUEUE_SIZE-8 then
                cmpGreaterUnsignedSN(nFullNext, i2slv(QUEUE_SIZE-8, SMALL_NUMBER_SIZE)) = '1' then
                isFull <= '0';
                isAlmostFull <= '1';
            else
                isFull <= '0';
                isAlmostFull <= '0';
            end if;	    
   
		end if;
	end process;
        N_FULL_NEXT: block
            constant QUEUE_SIZE_MASK: SmallNumber := i2slv(2*QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
            signal flowDiff: SmallNumber := (others => '0');            
        begin
            nFullNext <=  nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
                    else  --nFull + nIn - nOut;
                          --subSN(addSN(nFull, nIn), nOut);
                          flowDiff and QUEUE_SIZE_MASK;
            flowDiff <= subSN(addSN(nFull, nIn), nOut);
        end block;

	    nIn <= i2slv( countOnes(inputMask), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');
	        
        LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
            constant TAG_DIFF_SIZE_MASK: SmallNumber := i2slv(2*QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
            signal tagDiff: SmallNumber := (others => '0');
        begin
           nOut <= i2slv(countOnes(extractFullMask(--dataOutSigOld
                                                    dataOutSigFinal)), SMALL_NUMBER_SIZE) when isSending_T = '1'
          else (others => '0');        
        
           nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pTagged and fullMask(0) = '1'
                           else tagDiff and TAG_DIFF_SIZE_MASK;
                           tagDiff <= subSN(pTagged, pStartNext); -- TODO: modulo to make it positive           
        end generate;
    
        STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
            constant QUEUE_SIZE_MASK: SmallNumber := i2slv(2*QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
            signal tagDiff: SmallNumber := (others => '0');
        begin        
	        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isDraining = '1'
              else (others => '0');
                  
           nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pDrainNext = pTagged and fullMask(0) = '1' 
                else tagDiff and QUEUE_SIZE_MASK;
                tagDiff <= subSN(pTagged, pDrainNext); -- TODO: modulo to make it positive
        end generate;


    isDraining <= dataDrainSig(0).full;
	isSending <= committing and dataOutSigOld(0).full;
	
    dataOutSigFinal <= getSendingArray(dataOutSig, groupCtrInc, committing);
     
        isSending_T <= dataOutSigFinal(0).full;	
	
	dataOutV <= --dataOutSigOld;
	               dataOutSigFinal;

    -- Accept when 4 free slot exist
    pAcc <= subSN(pStart, i2slv(4, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
    pAccMore <= subSN(pStart, i2slv(8, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
    
	acceptingOut <= --not taggedMask(slv2u(pAcc)) and (not committedMask(slv2u(pAcc)) or bool2std(IS_LOAD_QUEUE));
	               not isFull;
	almostFull <= --taggedMask(slv2u(pAccMore)) or (committedMask(slv2u(pAcc)) and not bool2std(IS_LOAD_QUEUE)); -- TODO: more efficient full/almost full management (whole Core level)
                    isAlmostFull;
	sendingSQOut <= --isSending;
	               isSending_T;

	selectedDataOutput <= selectedDataOutputSig;
	
	committedEmpty <= not isNonzero(committedMask);
	committedSending <= isDraining;
	committedDataOut <= (0 => dataDrainSig(0), others => DEFAULT_INSTRUCTION_SLOT);
end Behavioral;
