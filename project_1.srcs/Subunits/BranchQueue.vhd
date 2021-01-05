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
		
		prevSendingRe: in std_logic;
		
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		dataInBr: in InstructionSlotArray(0 to PIPE_WIDTH-1);

            bqPtrOut: out SmallNumber;

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

	signal selectedDataSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	

	signal pStart, pStartNext, pTagged, pRenamed, pRenamedNext, pTaggedNext, pEnd, pSelect, pCausing: SmallNumber := (others => '0');
    signal isFull, isAlmostFull, isSending: std_logic := '0';
    
        signal TMP_fullMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
    
    signal recoveryCounter: SmallNumber := (others => '0');
begin

	SYNCH: process (clk)
	begin
		if rising_edge(clk) then
               
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
            recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here                   
		end if;
	end process;

	acceptingOut <= '1';
 
	sendingSQOut <= isSending;

	selectedDataOutput <= selectedDataSlot;
	almostFull <= '0'; -- TODO: is it deprecated?

	ALL_BRANCHES: block
	   signal allBranches: PipeStageArray(0 to QUEUE_SIZE-1) := (others => (others => DEFAULT_INS_SLOT));
	   signal allGroupTargets: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INS_STATE);
	   
	   signal allBranchOutput: PipeStage := (others => DEFAULT_INS_SLOT);
	   signal allGroupTargetOutput: InstructionState := DEFAULT_INS_STATE;	   
	   
	   signal accepting, committingBr: std_logic := '0';	   
	   signal memEmpty, taggedEmpty: std_logic := '1'; -- CAREFUL: starting with '1' 
	   
	   signal targetArray, ipArray: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
	   signal targetOutput, ipOutputA: Mword := (others => '0');
 
       signal trg0, trg1, trg2, trg3, res0, res1, res2, res3: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
       signal intp0, intp1, intp2, intp3, floatp0, floatp1, floatp2, floatp3: SmallNumberArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
       signal trgs, ress: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
       signal intps, floatps: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
       

	       signal ch0, ch1, ch2, ch3: std_logic := '0';
        
       function prepareInput(insVec: InstructionSlotArray) return InstructionSlotArray is
           variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
       begin
           for i in 0 to PIPE_WIDTh-1 loop
               
           end loop;
       
           return res;
       end function;
        
	   function getMatchingPtr(content: PipeStageArray; tag: InsTag; startPtr: SmallNumber; taggedMask: std_logic_vector) return SmallNumber is
	       variable res: SmallNumber := (others => '0');
	       variable mask, maskTmp: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0'); 
	       variable maskExt: std_logic_vector(0 to 2*QUEUE_SIZE-1) := (others => '0'); 
	   begin
	       for i in 0 to QUEUE_SIZE-1 loop
	           if      getTagHigh(content(i)(0).ins.tags.renameIndex) = getTagHigh(tag) 
	              and  taggedMask(i) = '1' then
	               mask(i) := '1';
	           end if;
	       end loop;
	       maskExt := mask & mask;
	       
	       for i in 0 to QUEUE_SIZE-1 loop
	           maskTmp(i) := --maskExt(i + slv2u(startPtr));
	                         maskExt(i);          
	       end loop;
	       
	       for i in 0 to QUEUE_SIZE-1 loop
               if maskTmp(i) = '1' then
                   res := --addIntTrunc(startPtr, i, QUEUE_PTR_SIZE);
                            i2slv(i, SMALL_NUMBER_SIZE);
                   exit;
               end if;
           end loop;

	       return res;
	   end function;

	   
	   function getMatchedSlot(allBranches: PipeStageArray; slotPtr: SmallNumber; cmpAdrSlot: InstructionSlot; ipBase: MWord; trgs, ress: MwordArray;
	                           intps, floatps: SmallNumberArray
	   ) return InstructionSlot is
	       variable res: InstructionSlot := DEFAULT_INS_SLOT;
	       variable lowPtr: natural := 0;
	       variable resLow: Mword := (others => '0');
	       variable tmpNumI, tmpNumF: SmallNumber := (others => '0');
	   begin
	       lowPtr := slv2u(getTagLow(cmpAdrSlot.ins.tags.renameIndex));
	       res := allBranches(slv2u(slotPtr))(lowPtr);
	       res.full := cmpAdrSlot.full;

           res.ins.ip := trgs(lowPtr);
               -- !!! this doesn't work for register branches
               
           if not TMP_PARAM_COMPRESS_RETURN then
               res.ins.result := ress(lowPtr);
           else  
               resLow(ALIGN_BITS downto 0) :=  ress(lowPtr)(ALIGN_BITS downto 0);

               if resLow(ALIGN_BITS) = '1' then
                   res.ins.result(MWORD_SIZE-1 downto ALIGN_BITS) := addInt(ipBase(MWORD_SIZE-1 downto ALIGN_BITS), 1);
               else
                   res.ins.result(MWORD_SIZE-1 downto ALIGN_BITS) := ipBase(MWORD_SIZE-1 downto ALIGN_BITS);                   
               end if;              
               res.ins.result(ALIGN_BITS-1 downto 0) := resLow(ALIGN_BITS-1 downto 0);
           end if;
                                
           res.ins.target := trgs(lowPtr);
           
           if not TMP_PARAM_COMPRESS_PTRS then
               res.ins.tags.intPointer := intps(lowPtr);
               res.ins.tags.floatPointer := floatps(lowPtr);
           else
                tmpNumI(1 downto 0) := intps(lowPtr)(1 downto 0);
                tmpNumF(1 downto 0) := floatps(lowPtr)(1 downto 0);
                if lowPtr = 0 then
                    res.ins.tags.intPointer := intps(0);
                    res.ins.tags.floatPointer := floatps(0);                    
                else
                    res.ins.tags.intPointer := add(intps(0), tmpNumI);
                    res.ins.tags.floatPointer := add(floatps(0), tmpNumF);             
                end if;
           end if;
           
           --    res.ins.controlInfo := DEFAULT_CONTROL_INFO;
           --    res.ins.controlInfo.frontBranch := allBranches(slv2u(slotPtr))(lowPtr).ins.controlInfo.frontBranch;
           
	       return res;
	   end function;
        
       function calcTaggedMask(pStartNext, pTaggedNext: SmallNumber) return std_logic_vector is
           variable res: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
           constant start: natural := slv2u(pStartNext);
           constant tagged: natural := slv2u(pTaggedNext);
       begin
           -- if pStart < pEnd:   i => pStart AND  i < pEnd 
           -- if pStart > pEnd:   i >= pStart OR i < pEnd
           -- if pStart = pEnd:   memFull    // when BQ empty this should never matter because no branches can be in Exec part -> compareAddressInput.full = '0'  
           
           -- Lets merge conditions 2 and 3: if pStart >= pEnd: i >= pStart OR i < pEnd; if pStart = pEnd then check condition is always true
           
           for i in 0 to QUEUE_SIZE-1 loop
               if start < tagged then
                   res(i) := bool2std(i >= start and i < tagged);
               else
                   res(i) := bool2std(i >= start or i < tagged);               
               end if;
           end loop;
           
           return res;
       end function;
        
        
            signal TMP_tags: InsTagArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
            signal matchingTags, TMP_taggedMask, TMP_taggedMaskNext: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
            signal TMP_nmt: natural := 0;
            
            signal allBranchesInputTmp: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
	begin
	           QQQ: for i in 0 to QUEUE_SIZE-1 generate
	               TMP_tags(i) <= allBranches(i)(0).ins.tags.renameIndex;
	               matchingTags(i) <= bool2std(getTagHigh(allBranches(i)(0).ins.tags.renameIndex) = getTagHigh(compareAddressInput.ins.tags.renameIndex)) --;
	                           and    TMP_taggedMask(i);
	           end generate;
	           TMP_nmt <= countOnes(matchingTags);
	       
	       -- CAREFUL: it may look like full when queue is empty. It's never used in such situarion because it's used only for finding matching entry for branch op.
	       --          When queue is empty, no branch ops are allowed to be in Exec. 
	       TMP_taggedMaskNext <= calcTaggedMask(pStartNext, pTaggedNext);
	
       dataOutV <= allBranchOutput; -- !!!
       isSending <= committingBr;

       selectedDataSlot <= getMatchedSlot(allBranches, pSelect, compareAddressInput, ipOutputA, trgs, ress, intps, floatps);
        
       ipOutputA <= ipArray(slv2u(pSelect));
       trgs <= (trg0(slv2u(pSelect)), trg1(slv2u(pSelect)), trg2(slv2u(pSelect)), trg3(slv2u(pSelect)));
       ress <= (res0(slv2u(pSelect)), res1(slv2u(pSelect)), res2(slv2u(pSelect)), res3(slv2u(pSelect)));
       intps <= (intp0(slv2u(pSelect)), intp1(slv2u(pSelect)), intp2(slv2u(pSelect)), intp3(slv2u(pSelect)));
       floatps <= (floatp0(slv2u(pSelect)), floatp1(slv2u(pSelect)), floatp2(slv2u(pSelect)), floatp3(slv2u(pSelect)));
            
       pSelect <= --getMatchingPtr(allBranches, compareAddressInput.ins.tags.renameIndex, pStart, TMP_taggedMask);
                  compareAddressInput.ins.tags.bqPointer;


       -- TODO: introduce bit in ROB which indicated whether the ROB entry uses a slot in this queue  
       committingBr <= committing and robData(0).ins.controlInfo.firstBr and not taggedEmpty;
   
	   accepting <= bool2std(pStart /= addIntTrunc(pEnd, 2, QUEUE_PTR_SIZE)) and bool2std(pStart /= addIntTrunc(pEnd, 1, QUEUE_PTR_SIZE)); -- Need 2 reserve slots because one group could be on the way
	   
       pStartNext <= addIntTrunc(pStart, 1, QUEUE_PTR_SIZE) when committingBr = '1' else pStart;
        
                pTaggedNext <= pStart when lateEventSignal = '1'
                    else       addIntTrunc(pCausing, 1, QUEUE_PTR_SIZE) when execEventSignal = '1'
                    else       addIntTrunc(pTagged, 1, QUEUE_PTR_SIZE) when prevSending = '1' and dataIn(0).ins.controlInfo.firstBr = '1'
                    else       pTagged; 
                
                pRenamedNext <= pStart when lateEventSignal = '1'
                        else       addIntTrunc(pCausing, 1, QUEUE_PTR_SIZE) when execEventSignal = '1'
                        else       addIntTrunc(pRenamed, 1, QUEUE_PTR_SIZE) when prevSendingRe = '1'
                        else       pRenamed; 
                    
                                    
                
                
            allBranchesInputTmp <= prepareInput(dataInBr);

        
	   SYNCH: process (clk)
	   begin
	       if rising_edge(clk) then	           
	           pCausing <= pSelect;
	           
	           if lateEventSignal = '1' then
	               pEnd <= pStart;
	               --pTagged <= pStart;
	               memEmpty <= '1';
	               taggedEmpty <= '1';
	           elsif execEventSignal = '1' then
	               pEnd <= addIntTrunc(pCausing, 1, QUEUE_PTR_SIZE);
	               --pTagged <= addIntTrunc(pCausing, 1, QUEUE_PTR_SIZE);	                   
	               memEmpty <= '0'; -- ???
	               taggedEmpty <= '0';
	           else	           
                   if prevSendingBr = '1' and dataInBr(0).ins.controlInfo.firstBr = '1' then
                       --allBranches(slv2u(pEnd)) <= prepareInput(dataInBr);
                         for i in 0 to PIPE_WIDTH-1 loop
                            allBranches(slv2u(pEnd))(i).full <= allBranchesInputTmp(i).full;                         
                            allBranches(slv2u(pEnd))(i).ins.controlInfo <= allBranchesInputTmp(i).ins.controlInfo;
                         end loop;
                       
                                    -- TODO: here don't write fields which aren't provided by this input (renameIndex etc.)
                       
                            ipArray(slv2u(pEnd)) <= dataInBr(0).ins.ip;
                            
                            trg0(slv2u(pEnd)) <= dataInBr(0).ins.target;
                            trg1(slv2u(pEnd)) <= dataInBr(1).ins.target;
                            trg2(slv2u(pEnd)) <= dataInBr(2).ins.target;
                            trg3(slv2u(pEnd)) <= dataInBr(3).ins.target;
                            
                            res0(slv2u(pEnd)) <= dataInBr(0).ins.result;
                            res1(slv2u(pEnd)) <= dataInBr(1).ins.result;
                            res2(slv2u(pEnd)) <= dataInBr(2).ins.result;
                            res3(slv2u(pEnd)) <= dataInBr(3).ins.result;
                            
                       
                       pEnd <= addIntTrunc(pEnd, 1, QUEUE_PTR_SIZE);
                       memEmpty <= '0';
                   end if;
                   
                   
                   if prevSending = '1' and dataIn(0).ins.controlInfo.firstBr = '1' then
                       allBranches(slv2u(pTagged))(0).ins.tags.renameIndex <= dataIn(0).ins.tags.renameIndex;
                       for i in 0 to PIPE_WIDTH-1 loop
                           allBranches(slv2u(pTagged))(i).ins.tags.intPointer <= dataIn(i).ins.tags.intPointer;
                           allBranches(slv2u(pTagged))(i).ins.tags.floatPointer <= dataIn(i).ins.tags.floatPointer;
                       end loop;

                            intp0(slv2u(pTagged)) <= dataIn(0).ins.tags.intPointer;
                            intp1(slv2u(pTagged)) <= dataIn(1).ins.tags.intPointer;
                            intp2(slv2u(pTagged)) <= dataIn(2).ins.tags.intPointer;
                            intp3(slv2u(pTagged)) <= dataIn(3).ins.tags.intPointer;
                            
                            floatp0(slv2u(pTagged)) <= dataIn(0).ins.tags.floatPointer;
                            floatp1(slv2u(pTagged)) <= dataIn(1).ins.tags.floatPointer;
                            floatp2(slv2u(pTagged)) <= dataIn(2).ins.tags.floatPointer;
                            floatp3(slv2u(pTagged)) <= dataIn(3).ins.tags.floatPointer;
                       
                       allGroupTargets(slv2u(pTagged)).tags.renameIndex <= dataIn(0).ins.tags.renameIndex;
                       --pTagged <= addIntTrunc(pTagged, 1, QUEUE_PTR_SIZE);
                       taggedEmpty <= '0';                       
                   end if;                
	           end if;
	           
	           if true then
	              allBranchOutput <= allBranches(slv2u(pStartNext));
	              allGroupTargetOutput <= allGroupTargets(slv2u(pStartNext));
	              targetOutput <= targetArray(slv2u(pStartNext));	              
	           end if;
	           
               if storeValueInput.full = '1' then
                   allGroupTargets(slv2u(pCausing)).target <= storeValueInput.ins.target;
                   targetArray(slv2u(pCausing)) <= storeValueInput.ins.target;
                   allGroupTargets(slv2u(pCausing)).controlInfo <= storeValueInput.ins.controlInfo;
                       allGroupTargets(slv2u(pCausing)).controlInfo.confirmedBranch <= storeValueInput.ins.controlInfo.confirmedBranch;
               end if;

               if committingBr = '1' and (prevSendingBr = '0' or dataInBr(0).ins.controlInfo.firstBr = '0')  and pStartNext = pEnd then -- that is memDraining
                   memEmpty <= '1';
               end if;
               
               if committingBr = '1' and (prevSending = '0' or dataIn(0).ins.controlInfo.firstBr = '0')  and pStartNext = pEnd then -- that is memDraining
                   taggedEmpty <= '1';
               end if;
               	           
	           pStart <= pStartNext;
	               pTagged <= pTaggedNext;
	               TMP_taggedMask <= TMP_taggedMaskNext;
	               
	               pRenamed <= pRenamedNext;
	       end if;
	   end process;

       committedDataOut <= (committingBr, setInstructionTarget(allGroupTargetOutput, targetOutput));	       
       acceptingBr <= accepting;
       
            ch0 <= not compareAddressInput.full or bool2std(compareAddressInput.ins.tags.bqPointer = pSelect);
       
	end block;
	
	       bqPtrOut <= pRenamed;
	

	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       --queueText <= getInsStringArray(makeSlotArray(content, fullMask), control);       
    end generate;

end Behavioral;
