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


entity BranchQueue is
	generic(
		QUEUE_SIZE: integer := 8
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		acceptingOut: out std_logic;
		almostFull: out std_logic;
		
		acceptingBr: out std_logic;
		
		prevSending: in std_logic;
			prevSendingBr: in std_logic;
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
			dataInBr: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		storeValueInput: in InstructionSlot;
		compareAddressInput: in InstructionSlot;

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
        committedDataOut: out InstructionSlot		
	);
end BranchQueue;


architecture Behavioral of BranchQueue is    
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

	signal isSending: std_logic := '0';

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, taggedMask, killMask, livingMask, frontMask, sendingMask, inputMask, inputMaskBr,
			 fullMaskNext, taggedMaskNext, taggedLivingMask, matchMask, matchMaskUpdate: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot, selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	
	signal dataOutSig, dataOutSigNext, dataOutSigFinal: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

	signal pStart, pStartNext, pTagged, pAll, causingPtr: SmallNumber := (others => '0');
    signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0');
    signal recoveryCounter: SmallNumber := (others => '0');
    signal isFull, isAlmostFull: std_logic := '0';
	

    function getNewContentBr(content: InstructionStateArray; dataIn, dataInBr: InstructionSlotArray;
                                taggedMask, fullMask: std_logic_vector;
				                prevSending, prevSendingBr: std_logic;
				                inputMask, inputMaskBr: std_logic_vector;
				                pTagged, pAll: SmallNumber;
				                matchMaskUpdate: std_logic_vector;
				                storeValueInput: InstructionSlot
				                )
    return InstructionStateArray is
        variable res: InstructionStateArray(0 to QUEUE_SIZE-1) := content;
        variable slot: InstructionSlot;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    variable im: std_logic_vector(0 to QUEUE_SIZE-1) := inputMask;
	    variable imBr: std_logic_vector(0 to QUEUE_SIZE-1) := inputMaskBr;	    
    begin

        -- TODO, CHECK: clear newEvent if unneeded? Other controlInfo except frontBranch, confirmedBranch, firstBr?
        -- TODO: add 'completed' flag for debug?  

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
               res(i).tags := slot.ins.tags;               
               res(i).controlInfo.firstBr := '0'; -- This is '1' only for the first branch in group!               
           end if;
        end loop;

        -- Set firstBr bit for the first entry in new group
        if prevSending = '1' then
            res(slv2u(pTagged)).controlInfo.firstBr := '1';
        end if;

        for i in 0 to QUEUE_SIZE-1 loop
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pAll) and PTR_MASK_SN;
           
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
           
           if imBr(i) = '1' then
              slot := getNewElem(remv, dataInBr);
                     --dataInBr(slv2u(diff(1 downto 0)));  
              res(i) := slot.ins;                       
           end if;
        end loop;

        -- Update target after branch execution
        for i in 0 to QUEUE_SIZE-1 loop
           if taggedMask(i) = '1' -- !! Prevent instruction with renmeIndex. = 0 from updating untagged entries! 
               and matchMaskUpdate(i) = '1'
               and storeValueInput.full = '1'
           then
               res(i).target := storeValueInput.ins.target;
               res(i).controlInfo.confirmedBranch := storeValueInput.ins.controlInfo.confirmedBranch;
               res(i).controlInfo.newEvent := storeValueInput.ins.controlInfo.newEvent;
           end if;
           
           if CLEAR_DEBUG_INFO then
              res(i) := clearRawInfo(res(i));
              res(i) := clearDbCounters(res(i));
              
              res(i).specificOperation := DEFAULT_SPECIFIC_OP;

              res(i).constantArgs := DEFAULT_CONSTANT_ARGS;
              res(i).virtualArgSpec := DEFAULT_ARG_SPEC;
              res(i).physicalArgSpec := DEFAULT_ARG_SPEC;
             end if;                    
        end loop;

        return res;
    end function;

    function getNewContentRT(content: InstructionStateArray; dataIn, dataInBr: InstructionSlotArray;
                                taggedMask, fullMask: std_logic_vector;
				                prevSending, prevSendingBr: std_logic;
				                inputMask, inputMaskBr: std_logic_vector;
				                pTagged, pAll: SmallNumber;
				                matchMaskUpdate: std_logic_vector;
				                storeValueInput: InstructionSlot
				                )
    return InstructionStateArray is
        variable res: InstructionStateArray(0 to QUEUE_SIZE-1) := content;
        variable slot: InstructionSlot;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    variable im: std_logic_vector(0 to QUEUE_SIZE-1) := inputMask;
	    variable imBr: std_logic_vector(0 to QUEUE_SIZE-1) := inputMaskBr;	    
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
               res(i).tags.renameIndex := slot.ins.tags.renameIndex;               
               res(i).controlInfo.firstBr := '0'; -- This is '1' only for the first branch in group!               
           end if;
        end loop;

        -- Set firstBr bit for the first entry in new group
        if prevSending = '1' then
            res(slv2u(pTagged)).controlInfo.firstBr := '1';
        end if;

        for i in 0 to QUEUE_SIZE-1 loop
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pAll) and PTR_MASK_SN;
           
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
           
           if imBr(i) = '1' then
              slot := getNewElem(remv, dataInBr);
                     --dataInBr(slv2u(diff(1 downto 0)));  
              res(i) := slot.ins;
              
              if CLEAR_DEBUG_INFO then
                 res(i) := DEFAULT_INS_STATE;
                 res(i).tags.renameIndex := slot.ins.tags.renameIndex;
              end if;
                   
           end if;
        end loop;

        return res;
    end function;




    function findMatchIndex(mask: std_logic_vector)
    return SmallNumber is
        variable res: SmallNumber := (others => '0');
        variable nShift, nPos: natural := 0;
    begin
        res := i2slv(getFirstOnePosition(mask), SMALL_NUMBER_SIZE);
        
        return res and PTR_MASK_SN;
    end function;
    
    function findSendingSlotIndex(content: InstructionStateArray; pStart: SmallNumber; ctrInc: InsTag) return SmallNumber is
        variable res: SmallNumber := (others => '0');
        variable mask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
        variable tmpVecExt: std_logic_vector(0 to 2*QUEUE_SIZE-1) := (others => '0');
        variable tmpVec, tmpVec1, tmpVec2: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');        
        variable nShift, nPos: natural := 0;   
    begin
        for i in 0 to QUEUE_SIZE-1 loop
            mask(i) := bool2std(getTagHighSN(content(i).tags.renameIndex) = getTagHighSN(ctrInc));       
        end loop;
        
        -- Shift by pStart
        nShift := slv2u(pStart);
        
        tmpVecExt := mask & mask;
        
        for i in 0 to QUEUE_SIZE-1 loop
            tmpVec1(i) := tmpVecExt(i + nShift);
        end loop;        
        
        for i in QUEUE_SIZE-1 downto 0 loop
            if tmpVec1(i) = '1' then
                res := i2slv(i, SMALL_NUMBER_SIZE);
                exit;
            end if;
        end loop;
        -- Add pStart
        res := add(res, pStart);

        return res and PTR_MASK_SN;        
    end function;
    
        
        signal TMP_updatePtr, TMP_sendingPtr: SmallNumber := (others => '0');
        signal TMP_sendingTarget: Mword := (others => '0');
        
        signal matchIndex, matchIndexUpdate: natural range 0 to QUEUE_SIZE-1 := 0;
        signal doUpdate: std_logic := '0';
        signal writtenTargets: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    
    signal registerTargetTable, contentNextRT: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INS_STATE);

    -- TODO: deduplicate from Ibuffer    
    subtype PipeStage is InstructionSlotArray(0 to PIPE_WIDTH-1);
    type PipeStageArray is array(natural range <>) of PipeStage;
    
begin
    causingPtr <= getCausingPtr(content, execCausing);
    
	-- in shifting queue this would be shfited by nSend
	frontMask <= getSendingMask(content, taggedLivingMask, groupCtrInc);
	sendingMask <= frontMask when committing = '1' else (others => '0');

	killMask <= getKillMask(content, taggedMask, execCausing, execEventSignal, lateEventSignal);
	livingMask <= fullMask when (lateEventSignal = '0' and execEventSignal = '0') 
	         else taggedLivingMask;	
    taggedLivingMask <= taggedMask and not killMask;
				
    inputMask <= getInputMask(taggedMask, extractFullMask(dataIn), prevSending, pTagged, PTR_MASK_SN);				
    inputMaskBr <= getInputMask(fullMask, extractFullMask(dataInBr), prevSendingBr, pAll, PTR_MASK_SN);
				
	fullMaskNext <= (livingMask and not sendingMask) or inputMaskBr;
	taggedMaskNext <= (taggedLivingMask and not sendingMask) or inputMask;
	
	contentNext <=
				getNewContentBr(content, dataIn, dataInBr,
				                taggedMask, fullMask,
				                prevSending, prevSendingBr,
				                inputMask, inputMaskBr,				             
				                pTagged, pAll,
				                matchMaskUpdate,
				                storeValueInput);

        contentNextRT <=
                    getNewContentRT(registerTargetTable, dataIn, dataInBr,
                                    taggedMask, fullMask,
                                    prevSending, prevSendingBr,
                                    inputMask, inputMaskBr,				             
                                    pTagged, pAll,
                                    matchMaskUpdate,
                                    storeValueInput);

				                
	dataOutSigNext <= getWindow(content, taggedMask, pStartNext, PIPE_WIDTH);
	selectedDataSlot <= selectBranchDataSlot(content, taggedMask, matchMask, compareAddressInput);
	matchMask <= getMatchingTags(content, compareAddressInput.ins.tags.renameIndex);
	   matchIndex <= getMatchingIndex(content, compareAddressInput.ins.tags.renameIndex);
	
    pStartNext <= addIntTrunc(pStart, getNumberToSend(dataOutSig, groupCtrInc, committing), QUEUE_PTR_SIZE);
	
	process (clk)
	begin
		if rising_edge(clk) then	
            fullMask <= fullMaskNext;
            taggedMask <= taggedMaskNext;
			content <= contentNext;
            pStart <= pStartNext;
			
			selectedDataOutputSig <= selectedDataSlot;
            dataOutSig <= dataOutSigNext;
            
            matchMaskUpdate <= matchMask;
               matchIndexUpdate <= matchIndex;
               doUpdate <= compareAddressInput.full;
            
               if doUpdate = '1' then
                  writtenTargets(matchIndexUpdate) <= storeValueInput.ins.target;       
               end if;
            
            if lateEventSignal = '1' then
                pTagged <= pStartNext;
            elsif execEventSignal = '1' then
                pTagged <= addIntTrunc(causingPtr, 1, QUEUE_PTR_SIZE);
            elsif prevSending = '1' then
                pTagged <= addIntTrunc(pTagged, countOnes(inputMask), QUEUE_PTR_SIZE);                
            end if;

            if lateEventSignal = '1' then
                pAll <= pStartNext;
            elsif execEventSignal = '1' then
                pAll <= addIntTrunc(causingPtr, 1, QUEUE_PTR_SIZE);       
            elsif prevSendingBr = '1' then
                pAll <= addIntTrunc(pAll, countOnes(inputMaskBr), QUEUE_PTR_SIZE);                
            end if;
               
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
            
            isFull <=       cmpGtU(nFullNext, QUEUE_SIZE-4);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE-8);
            nFull <= nFullNext;                     
		end if;
	end process;

    nFullNext <= nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
                else subTruncZ(add(nFull, nIn), nOut, QUEUE_CAP_SIZE);

    nIn <= i2slv( countOnes(extractFullMask(dataInBr)), SMALL_NUMBER_SIZE ) when prevSendingBr = '1' else (others => '0');

    nOut <= i2slv(countOnes(extractFullMask(dataOutSigFinal)), SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');              
    nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pAll and fullMask(0) = '1'
                else subTruncZ(pAll, pStartNext, QUEUE_PTR_SIZE);  -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch
    
    dataOutSigFinal <= getSendingArray(dataOutSig, groupCtrInc, committing);

    isSending <= dataOutSigFinal(0).full;
	acceptingOut <= '1';
	acceptingBr <= not isAlmostFull;     
 
	dataOutV <= dataOutSigFinal;                   
	sendingSQOut <= isSending;

	selectedDataOutput <= selectedDataSlot;
	almostFull <= '0'; -- TODO: is it deprecated?
	
	
	   TMP_updatePtr <= findMatchIndex(matchMaskUpdate);

        TMP_sendingPtr <= findSendingSlotIndex(content, pStartNext, groupCtrInc);

	   TMP_REG_TARGETS: process (clk)
	       variable nReceiving: natural := 0;
	   begin
	       if rising_edge(clk) then
	           registerTargetTable <= contentNextRT;
	           
	           if storeValueInput.full = '1' then
	               registerTargetTable(slv2u(TMP_updatePtr)).target <= storeValueInput.ins.target;
	           end if;
	           
	           TMP_sendingTarget <= registerTargetTable(slv2u(TMP_sendingPtr)).target;
	           
	       end if;
	
	   end process;
	
	ALL_BRANCHES: block
	   signal allBranches: PipeStageArray(0 to QUEUE_SIZE-1) := (others => (others => DEFAULT_INS_SLOT));
	   signal allGroupTargets: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INS_STATE);
	   
	   signal startPtr, startPtrNext, endPtr, taggedPtr, cmpMatchedPtr, storingMatchedPtr: natural := 0;
	   signal allBranchOutput: PipeStage := (others => DEFAULT_INS_SLOT);
	   signal allGroupTargetOutput: InstructionState := DEFAULT_INS_STATE;
	   
	   signal comparedMatchingSlot, selectedSlot: InstructionSlot := DEFAULT_INS_SLOT;
	   
	   signal accepting, committingBr: std_logic := '0';
	   
	   signal memEmpty, taggedEmpty: std_logic := '1';
	   
	           signal ch0, ch1, ch2, ch3: std_logic := '0';
	   
	   function getMatchingSlot(content: PipeStageArray; tag: InsTag; startPtr: natural) return natural is
	       variable res: natural := 0;
	       variable mask, maskTmp: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0'); 
	       variable maskExt: std_logic_vector(0 to 2*QUEUE_SIZE-1) := (others => '0'); 
	   begin
	       for i in 0 to QUEUE_SIZE-1 loop
	           if getTagHigh(content(i)(0).ins.tags.renameIndex) = getTagHigh(tag) then
	               mask(i) := '1';
	           end if;
	       end loop;
	       maskExt := mask & mask;
	       
	       for i in 0 to QUEUE_SIZE-1 loop
	           maskTmp(i) := maskExt(i + startPtr);
	       end loop;
	       
	       for i in 0 to QUEUE_SIZE-1 loop
               if maskTmp(i) = '1' then
                   res := i + startPtr;
                   exit;
               end if;
           end loop;	       

	       return res mod QUEUE_SIZE;
	   end function;
	   
--	   function findCmpSlot(allBranches: PipeStageArray; compareAddressInput: InstructionSlot) return InstructionSlot is
--	       variable res: InstructionSlot := DEFAULT_INS_SLOT;
--	   begin
--	       for i in 0 to QUEUE_SIZE-1 loop
--	           if getTagHigh(compareAddressInput.ins.tags.renameIndex) = getTagHigh(allBranches(i)(0).ins.tags.renameIndex) then
	               
--	           end if;
--	       end loop;
	       
--	       return res;
--	   end function;
	   
	   function getMatchedSlot(allBranches: PipeStageArray; slotPtr: natural; cmpAdrSlot: InstructionSlot) return InstructionSlot is
	       variable res: InstructionSlot := DEFAULT_INS_SLOT;
	       variable lowPtr: natural := 0;
	   begin
	       lowPtr := slv2u(getTagLow(cmpAdrSlot.ins.tags.renameIndex));
	       res := allBranches(slotPtr)(lowPtr);
	       res.full := cmpAdrSlot.full;
	       return res;
	   end function;
	   
	       function TMP_leftAlign(insVec: InstructionSlotArray) return InstructionSlotArray is
	           variable res: InstructionSlotArray(insVec'range) := insVec;
	           variable resExt: InstructionSlotArray(0 to 2*PIPE_WIDTH-1) := insVec & insVec;
	           variable nShift: natural := slv2u(insVec(0).ins.tags.renameIndex(1 downto 0)); 
	       begin
	           for i in 0 to PIPE_WIDTH-1 loop
	               res(i) := resExt(i + nShift);
	           end loop;
	           
	           return res;
	       end function;
	   
	begin
	        --   selectedDataSlot <= comparedMatchingSlot; -- !!!!!
	
	
	       comparedMatchingSlot <= --findCmpSlot(allBranches, compareAddressInput);
	                               getMatchedSlot(allBranches, cmpMatchedPtr, compareAddressInput);
	
	       cmpMatchedPtr <= getMatchingSlot(allBranches, compareAddressInput.ins.tags.renameIndex, startPtr);
	
	       storingMatchedPtr <= getMatchingSlot(allBranches, storeValueInput.ins.tags.renameIndex, startPtr); -- TODO: use compareAddressInput with 1 cycle delay?
	
	       committingBr <= committing and -- TODO: include signal that is true only when group begin committed uses a slot in this queue
	                                       bool2std(getTagHighSN(allBranchOutput(0).ins.tags.renameIndex) = getTagHighSN(groupCtrInc))
	                                       and not taggedEmpty;
	   
	   accepting <= bool2std(startPtr /= ((endPtr + 1) mod QUEUE_SIZE));
	   --accepting <= bool2std(startPtr /= endPtr) or memEmpty;
	   
	   startPtrNext <= (startPtr + 1) mod QUEUE_SIZE when committingBr = '1' else startPtr;
	   
	   
	             ch0 <= bool2std(selectedDataSlot.ins.result = comparedMatchingSlot.ins.result) or not selectedDataSlot.full;
	             --ch1 <= bool2std(selectedDataSlot.ins.target = comparedMatchingSlot.ins.target) or not selectedDataSlot.full;
	             ch1 <= bool2std(selectedDataSlot = comparedMatchingSlot) or not selectedDataSlot.full;
	   
	   SYNCH: process (clk)
	   begin
	       if rising_edge(clk) then
	           selectedSlot <= comparedMatchingSlot;
	       
	           if lateEventSignal = '1' then
	               endPtr <= startPtr;
	               taggedPtr <= startPtr;
	               memEmpty <= '1';
	               taggedEmpty <= '1';
	           elsif execEventSignal = '1' then
	               endPtr <= (storingMatchedPtr + 1) mod QUEUE_SIZE; -- !!!
	               taggedPtr <= (storingMatchedPtr + 1) mod QUEUE_SIZE; -- !!!
	               memEmpty <= '0'; -- ???
	               taggedEmpty <= '0';
	           else
	               if prevSendingBr = '1' and isNonzero(extractFullMask(dataInBr)) = '1' then
                       allBranches(endPtr) <= TMP_leftAlign(dataInBr);
                       allGroupTargets(endPtr) <= DEFAULT_INS_STATE;
                       endPtr <= (endPtr + 1) mod QUEUE_SIZE;
                       memEmpty <= '0';
                   end if;
                   
                   if prevSending = '1' and isNonzero(extractFullMask(dataIn)) = '1' then
                       allBranches(taggedPtr)(0).ins.tags.renameIndex <= dataIn(0).ins.tags.renameIndex;
                       allGroupTargets(taggedPtr).tags.renameIndex <= dataIn(0).ins.tags.renameIndex;
                       taggedPtr <= (taggedPtr + 1) mod QUEUE_SIZE;
                       taggedEmpty <= '0';                       
                   end if;
	           end if;
	           
	           if true then
	              allBranchOutput <= allBranches(startPtrNext);
	              allGroupTargetOutput <= allGroupTargets(startPtrNext);	              
	           end if;
	           
	               if storeValueInput.full = '1' then
	                   allGroupTargets(storingMatchedPtr).target <= storeValueInput.ins.target;
	               end if;

	           
	           if committingBr = '1' and (prevSending = '0' or isNonzero(extractFullMask(dataIn)) = '0')  and startPtrNext = endPtr then -- that is memDraining
	               memEmpty <= '1';
	           end if;
	           
	           if committingBr = '1' and (prevSendingBr = '0' or isNonzero(extractFullMask(dataInBr)) = '0') and startPtrNext = taggedPtr then -- that is memDraining
                   taggedEmpty <= '1';
               end if;
               	           
	           startPtr <= startPtrNext;
--	           if committing = '1' then
--	               startPtr <= startPtr + 1;
--	           end if;
	       end if;
	   end process;
	       
	       
	       committedDataOut <= (committingBr, allGroupTargetOutput);
	end block;
	
	
	
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       queueText <= getInsStringArray(makeSlotArray(content, fullMask), control);       
    end generate;

end Behavioral;
