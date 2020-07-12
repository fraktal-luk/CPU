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

	signal pStart, pStartNext, pTagged, pAll, causingPtr: SmallNumber := (others => '0');
    signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0');
    signal recoveryCounter: SmallNumber := (others => '0');
    signal isFull, isAlmostFull: std_logic := '0';

    -- TODO: deduplicate from Ibuffer    
    subtype PipeStage is InstructionSlotArray(0 to PIPE_WIDTH-1);
    type PipeStageArray is array(natural range <>) of PipeStage;
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

       selectedDataSlot <= comparedMatchingSlot; -- !!!!!
       dataOutV <= allBranchOutput; -- !!!
       isSending <= committingBr;

       comparedMatchingSlot <= getMatchedSlot(allBranches, cmpMatchedPtr, compareAddressInput);

       cmpMatchedPtr <= getMatchingSlot(allBranches, compareAddressInput.ins.tags.renameIndex, startPtr);

       storingMatchedPtr <= getMatchingSlot(allBranches, storeValueInput.ins.tags.renameIndex, startPtr); -- TODO: use compareAddressInput with 1 cycle delay?
        
       -- TODO: introduce bit in ROB which indicated whether the ROB entry uses a slot in this queue  
       committingBr <= committing and bool2std(getTagHighSN(allBranchOutput(0).ins.tags.renameIndex) = getTagHighSN(groupCtrInc))
                                       and not taggedEmpty;
   
	   accepting <= bool2std(startPtr /= ((endPtr + 2) mod QUEUE_SIZE)) and bool2std(startPtr /= ((endPtr + 1) mod QUEUE_SIZE)); -- Need 2 reserve slots because one group could be on the way
	   
	   startPtrNext <= (startPtr + 1) mod QUEUE_SIZE when committingBr = '1' else startPtr;

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
                       --allGroupTargets(endPtr) <= DEFAULT_INS_STATE;
                       endPtr <= (endPtr + 1) mod QUEUE_SIZE;
                       memEmpty <= '0';
                   end if;
                   
                   if prevSending = '1' and isNonzero(extractFullMask(dataIn)) = '1' then
                       allBranches(taggedPtr)(0).ins.tags.renameIndex <= dataIn(0).ins.tags.renameIndex;
                       for i in 0 to PIPE_WIDTH-1 loop
                           allBranches(taggedPtr)(i).ins.tags.intPointer <= dataIn(i).ins.tags.intPointer;
                           allBranches(taggedPtr)(i).ins.tags.floatPointer <= dataIn(i).ins.tags.floatPointer;
                       end loop;
                       
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
                   allGroupTargets(storingMatchedPtr).controlInfo <= storeValueInput.ins.controlInfo;
               end if;

	           
	           if committingBr = '1' and (prevSendingBr = '0' or isNonzero(extractFullMask(dataInBr)) = '0')  and startPtrNext = endPtr then -- that is memDraining
	               memEmpty <= '1';
	           end if;
	           
	           if committingBr = '1' and (prevSending = '0' or isNonzero(extractFullMask(dataIn)) = '0') and startPtrNext = taggedPtr then -- that is memDraining
                   taggedEmpty <= '1';
               end if;
               	           
	           startPtr <= startPtrNext;
	       end if;
	   end process;

       committedDataOut <= (committingBr, allGroupTargetOutput);	       
       acceptingBr <= accepting;
	end block;
	
	
	
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       --queueText <= getInsStringArray(makeSlotArray(content, fullMask), control);       
    end generate;

end Behavioral;
