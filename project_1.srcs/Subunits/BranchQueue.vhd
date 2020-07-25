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

	signal selectedDataSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	

	signal pStart, pStartNext, pTagged, pEnd, pSelect, pCausing: SmallNumber := (others => '0');
    signal recoveryCounter: SmallNumber := (others => '0');
    signal isFull, isAlmostFull: std_logic := '0';

    -- TODO: deduplicate from Ibuffer    
--    subtype PipeStage is InstructionSlotArray(0 to PIPE_WIDTH-1);
--    type PipeStageArray is array(natural range <>) of PipeStage;
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
	   signal comparedMatchingSlot, selectedSlot: InstructionSlot := DEFAULT_INS_SLOT;
	   
	   signal accepting, committingBr: std_logic := '0';
	   
	   signal memEmpty, taggedEmpty: std_logic := '1';
   
	           signal ch0, ch1, ch2, ch3: std_logic := '0';
	   
	   signal targetArray, ipArray: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
	   signal targetOutput, ipOutputA: Mword := (others => '0');
        
        signal trg0, trg1, trg2, trg3: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
        signal res0, res1, res2, res3: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
        signal intp0, intp1, intp2, intp3: SmallNumberArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
        signal floatp0, floatp1, floatp2, floatp3: SmallNumberArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
        signal trgs, ress: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
        signal intps, floatps: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
       
        
	   function getMatchingPtr(content: PipeStageArray; tag: InsTag; startPtr: SmallNumber) return SmallNumber is
	       variable res: SmallNumber := (others => '0');
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
	           maskTmp(i) := maskExt(i + slv2u(startPtr));
	       end loop;
	       
	       for i in 0 to QUEUE_SIZE-1 loop
               if maskTmp(i) = '1' then
                   res := addIntTrunc(startPtr, i, QUEUE_PTR_SIZE);
                   exit;
               end if;
           end loop;	       

	       return res;
	   end function;

	   
	   function getMatchedSlot(allBranches: PipeStageArray; slotPtr: SmallNumber; cmpAdrSlot: InstructionSlot; trgs, ress: MwordArray;
	                           intps, floatps: SmallNumberArray
	   ) return InstructionSlot is
	       variable res: InstructionSlot := DEFAULT_INS_SLOT;
	       variable lowPtr: natural := 0;
	   begin
	       lowPtr := slv2u(getTagLow(cmpAdrSlot.ins.tags.renameIndex));
	       res := allBranches(slv2u(slotPtr))(lowPtr);
	       res.full := cmpAdrSlot.full;
	           --res.ip(MWORD_SIZE-1 downto ALIGN_BITS) := startIP(MWORD_SIZE-1 downto ALIGN_BITS);
	           res.ins.ip := trgs(lowPtr);
	               -- !!! this doesn't work for register branches
	               res.ins.result := ress(lowPtr);
	               res.ins.target := trgs(lowPtr);
	               
	               res.ins.tags.intPointer := intps(lowPtr);
	               res.ins.tags.floatPointer := floatps(lowPtr);
	       return res;
	   end function;
   
--       function TMP_leftAlign(insVec: InstructionSlotArray) return InstructionSlotArray is
--           variable res: InstructionSlotArray(insVec'range) := insVec;
--           variable resExt: InstructionSlotArray(0 to 2*PIPE_WIDTH-1) := insVec & insVec;
--           variable nShift: natural := slv2u(insVec(0).ins.tags.renameIndex(1 downto 0)); 
--       begin
--           for i in 0 to PIPE_WIDTH-1 loop
--               res(i) := resExt(i + nShift);
--           end loop;
           
--           return res;
--       end function;
	   
	   function TMP_usesBQ(robData: PipeStage) return std_logic is
	   begin
	       for i in 0 to PIPE_WIDTH-1 loop
	           if robData(i).full = '1' and robData(i).ins.classInfo.branchIns = '1' then
	               return '1';
	           end if;
	       end loop;
	       return '0';
	   end function;
	   
	   
	   --signal trg0, trg1, trg2, trg3: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
	   
	   signal TMP_committingBr: std_logic := '0';
	begin

       selectedDataSlot <= comparedMatchingSlot; -- !!!!!
       dataOutV <= allBranchOutput; -- !!!
       isSending <= committingBr;

       comparedMatchingSlot <= getMatchedSlot(allBranches, pSelect, compareAddressInput, trgs, ress, intps, floatps);
        
            ipOutputA <= ipArray(slv2u(pSelect));
            trgs <= (trg0(slv2u(pSelect)), trg1(slv2u(pSelect)), trg2(slv2u(pSelect)), trg3(slv2u(pSelect)));
            ress <= (res0(slv2u(pSelect)), res1(slv2u(pSelect)), res2(slv2u(pSelect)), res3(slv2u(pSelect)));
            intps <= (intp0(slv2u(pSelect)), intp1(slv2u(pSelect)), intp2(slv2u(pSelect)), intp3(slv2u(pSelect)));
            floatps <= (floatp0(slv2u(pSelect)), floatp1(slv2u(pSelect)), floatp2(slv2u(pSelect)), floatp3(slv2u(pSelect)));
            
       pSelect <= getMatchingPtr(allBranches, compareAddressInput.ins.tags.renameIndex, pStart);

       -- TODO: introduce bit in ROB which indicated whether the ROB entry uses a slot in this queue  
       committingBr <= committing and bool2std(getTagHighSN(allBranchOutput(0).ins.tags.renameIndex) = getTagHighSN(groupCtrInc)) and not taggedEmpty when not TMP_PARAM_USE_FIRSTBR_FLAG
              else     committing and robData(0).ins.controlInfo.firstBr and not taggedEmpty;
   
            TMP_committingBr <= TMP_usesBQ(robData) and committing;
   
	   accepting <= bool2std(pStart /= addIntTrunc(pEnd, 2, QUEUE_PTR_SIZE)) and bool2std(pStart /= addIntTrunc(pEnd, 1, QUEUE_PTR_SIZE)); -- Need 2 reserve slots because one group could be on the way
	   
       pStartNext <= addIntTrunc(pStart, 1, QUEUE_PTR_SIZE) when committingBr = '1' else pStart;

	   SYNCH: process (clk)
	   begin
	       if rising_edge(clk) then
	           selectedSlot <= comparedMatchingSlot;
	           
	           pCausing <= pSelect;
	           
	           if lateEventSignal = '1' then
	               pEnd <= pStart;
	               pTagged <= pStart;
	               memEmpty <= '1';
	               taggedEmpty <= '1';
	           elsif execEventSignal = '1' then
	               pEnd <= addIntTrunc(pCausing, 1, QUEUE_PTR_SIZE);
	               pTagged <= addIntTrunc(pCausing, 1, QUEUE_PTR_SIZE);	                   
	               memEmpty <= '0'; -- ???
	               taggedEmpty <= '0';
	           else
	           
	               if TMP_PARAM_USE_FIRSTBR_FLAG then
                       if prevSendingBr = '1' and dataInBr(0).ins.controlInfo.firstBr = '1' then
                           allBranches(slv2u(pEnd)) <= dataInBr;
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
                   
                   else
                       if prevSendingBr = '1' and isNonzero(extractFullMask(dataInBr)) = '1' then
                                                  -- dataInBr(0).ins.controlInfo.firstBr = '1' then
                           allBranches(slv2u(pEnd)) <= dataInBr;
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
                   end if;
                   
                   if TMP_PARAM_USE_FIRSTBR_FLAG then
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
                           pTagged <= addIntTrunc(pTagged, 1, QUEUE_PTR_SIZE);
                           taggedEmpty <= '0';                       
                       end if;
                   else    
                       if prevSending = '1' and isNonzero(extractFullMask(dataIn)) = '1' then
                                                --    dataIn(0).ins.controlInfo.firstBr = '1' then
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
                           pTagged <= addIntTrunc(pTagged, 1, QUEUE_PTR_SIZE);
                           taggedEmpty <= '0';                       
                       end if;
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
               end if;

	           if TMP_PARAM_USE_FIRSTBR_FLAG then	           
                   if committingBr = '1' and (prevSendingBr = '0' or dataInBr(0).ins.controlInfo.firstBr = '0')  and pStartNext = pEnd then -- that is memDraining
                       memEmpty <= '1';
                   end if;
                   
                   if committingBr = '1' and (prevSending = '0' or dataIn(0).ins.controlInfo.firstBr = '0')  and pStartNext = pEnd then -- that is memDraining
                       taggedEmpty <= '1';
                   end if;
                   
                else   
                   if committingBr = '1' and (prevSendingBr = '0' or isNonzero(extractFullMask(dataInBr)) = '0')  and pStartNext = pEnd then -- that is memDraining
                       memEmpty <= '1';
                   end if;
                   
                   if committingBr = '1' and (prevSending = '0' or isNonzero(extractFullMask(dataIn)) = '0') and pStartNext = pEnd then -- that is memDraining
                       taggedEmpty <= '1';
                   end if;
               end if;
               	           
	           pStart <= pStartNext;
	       end if;
	   end process;

       committedDataOut <= (committingBr, setInstructionTarget(allGroupTargetOutput, targetOutput));	       
       acceptingBr <= accepting;
	end block;
	
	
	
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       --queueText <= getInsStringArray(makeSlotArray(content, fullMask), control);       
    end generate;

end Behavioral;
