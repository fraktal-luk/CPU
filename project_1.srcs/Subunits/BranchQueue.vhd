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

	signal pStart, pStartNext, pEnd, pEndNext, pStartLong, pStartLongNext, pEndLong, pEndLongNext, pTagged,
	       pRenamed, pRenamedNext, pTaggedNext, pTaggedLong, pTaggedLongNext, pRenamedLong, pRenamedLongNext,
	       pSelect, pCausing, pSelectLong, pCausingLong: SmallNumber := (others => '0');
    signal isFull, isAlmostFull, isSending: std_logic := '0';
    
        signal TMP_fullMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
    
    signal recoveryCounter: SmallNumber := (others => '0');
    
    -- TODO: functions duplicated from STORE_QUEUE. To clean
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
	   
	   signal accepting, committingBr: std_logic := '0';	   
	   signal memEmpty, taggedEmpty: std_logic := '1'; -- CAREFUL: starting with '1' 
	   
	   signal targetArray, ipArray: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
	   signal targetOutput, ipOutputA: Mword := (others => '0');
 
       signal trg0, trg1, trg2, trg3, res0, res1, res2, res3: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
       signal intp0, intp1, intp2, intp3, floatp0, floatp1, floatp2, floatp3: SmallNumberArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
       signal trgs, ress: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
       signal intps, floatps: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
       

	       signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';
        	   
	   function getMatchedSlot(allBranches: PipeStageArray; slotPtr: SmallNumber; cmpAdrSlot: InstructionSlot; ipBase: MWord; trgs, ress: MwordArray;
	                           intps, floatps: SmallNumberArray)
	   return InstructionSlot is
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
  
	       return res;
	   end function;

       signal allBranchesInputTmp: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
	begin	
       isSending <= committingBr;

       selectedDataSlot <= getMatchedSlot(allBranches, pSelect, compareAddressInput, ipOutputA, trgs, ress, intps, floatps);
        
       ipOutputA <= ipArray(slv2u(pSelect));
       trgs <= (trg0(slv2u(pSelect)), trg1(slv2u(pSelect)), trg2(slv2u(pSelect)), trg3(slv2u(pSelect)));
       ress <= (res0(slv2u(pSelect)), res1(slv2u(pSelect)), res2(slv2u(pSelect)), res3(slv2u(pSelect)));
       intps <= (intp0(slv2u(pSelect)), intp1(slv2u(pSelect)), intp2(slv2u(pSelect)), intp3(slv2u(pSelect)));
       floatps <= (floatp0(slv2u(pSelect)), floatp1(slv2u(pSelect)), floatp2(slv2u(pSelect)), floatp3(slv2u(pSelect)));
            
       pSelect <= compareAddressInput.ins.tags.bqPointer and PTR_MASK_SN;

           pSelectLong <= compareAddressInput.ins.tags.bqPointer;


       -- TODO: introduce bit in ROB which indicated whether the ROB entry uses a slot in this queue  
       committingBr <= committing and robData(0).ins.controlInfo.firstBr and not taggedEmpty;
   
	   accepting <= bool2std(pStart /= addIntTrunc(pEnd, 2, QUEUE_PTR_SIZE)) and bool2std(pStart /= addIntTrunc(pEnd, 1, QUEUE_PTR_SIZE)); -- Need 2 reserve slots because one group could be on the way

            pStartNext <= pStartLongNext and PTR_MASK_SN;
            pTaggedNext <= pTaggedLongNext and PTR_MASK_SN;
            pRenamedNext <= pRenamedLongNext and PTR_MASK_SN;
            pEndNext <= pEndLongNext and PTR_MASK_SN;

            pStart <= pStartLong and PTR_MASK_SN;
            pTagged <= pTaggedLong and PTR_MASK_SN;
            pRenamed <= pRenamedLong and PTR_MASK_SN;
            pEnd <= pEndLong and PTR_MASK_SN;

               pStartLongNext <= addIntTrunc(pStartLong, 1, QUEUE_PTR_SIZE+1) when committingBr = '1' else pStartLong;
            
                pTaggedLongNext <= pStartLong when lateEventSignal = '1'
                    else       addIntTrunc(pCausingLong, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
                    else       addIntTrunc(pTaggedLong, 1, QUEUE_PTR_SIZE+1) when prevSending = '1' and dataIn(0).ins.controlInfo.firstBr = '1'
                    else       pTaggedLong;
                
                pRenamedLongNext <= pStartLong when lateEventSignal = '1'
                    else       addIntTrunc(pCausingLong, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
                    else       addIntTrunc(pRenamedLong, 1, QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
                    else       pRenamedLong;
         
        
                pEndLongNext <= pStartLong when lateEventSignal = '1'
                    else    addIntTrunc(pCausingLong, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
                    else    addIntTrunc(pEndLong, 1, QUEUE_PTR_SIZE+1) when prevSendingBr = '1' and dataInBr(0).ins.controlInfo.firstBr = '1'
                    else    pEndLong;


        allBranchesInputTmp <= dataInBr;


        
	   SYNCH: process (clk)
	   begin
	       if rising_edge(clk) then	           
	           pCausing <= pSelect;
	           pCausingLong <= pSelectLong;
          
               if prevSendingBr = '1' and dataInBr(0).ins.controlInfo.firstBr = '1' then
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
               end if;
                               
               if prevSending = '1' and dataIn(0).ins.controlInfo.firstBr = '1' then

                        intp0(slv2u(pTagged)) <= dataIn(0).ins.tags.intPointer;
                        intp1(slv2u(pTagged)) <= dataIn(1).ins.tags.intPointer;
                        intp2(slv2u(pTagged)) <= dataIn(2).ins.tags.intPointer;
                        intp3(slv2u(pTagged)) <= dataIn(3).ins.tags.intPointer;
                        
                        floatp0(slv2u(pTagged)) <= dataIn(0).ins.tags.floatPointer;
                        floatp1(slv2u(pTagged)) <= dataIn(1).ins.tags.floatPointer;
                        floatp2(slv2u(pTagged)) <= dataIn(2).ins.tags.floatPointer;
                        floatp3(slv2u(pTagged)) <= dataIn(3).ins.tags.floatPointer;                    
               end if;                
	           
	           if true then
	              targetOutput <= targetArray(slv2u(pStartNext));	              
	           end if;
	           
               if storeValueInput.full = '1' then
                   targetArray(slv2u(pCausing)) <= storeValueInput.ins.target;
               end if;

               	           
--	           pStart <= pStartNext;
--               pTagged <= pTaggedNext;
--               pEnd <= pEndNext;
--               pRenamed <= pRenamedNext;
               
	           pStartLong <= pStartLongNext;
               pTaggedLong <= pTaggedLongNext;
               pEndLong <= pEndLongNext;
               pRenamedLong <= pRenamedLongNext;
               
               memEmpty <= getQueueEmpty(pStartLongNext, pEndLongNext, QUEUE_PTR_SIZE);              
               taggedEmpty <= getQueueEmpty(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);              
	       end if;
	   end process;

       committedDataOut <= (committingBr, setInstructionTarget(DEFAULT_INS_STATE, targetOutput));	       
       acceptingBr <= accepting;
       
--            ch0 <= not compareAddressInput.full or bool2std(compareAddressInput.ins.tags.bqPointer = pSelect);
       
--                ch3 <= not memEmpty xor ch1;
--                ch4 <= not taggedEmpty xor ch2;
        ch0 <= bool2std((pStartLong and PTR_MASK_SN) = pStart);
        ch1 <= bool2std((pTaggedLong and PTR_MASK_SN) = pTagged);
        ch2 <= bool2std((pEndLong and PTR_MASK_SN) = pEnd);
        ch3 <= bool2std((pRenamedLong and PTR_MASK_SN) = pRenamed);
       
	end block;
	
	bqPtrOut <= pRenamedLong;
	

	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       --queueText <= getInsStringArray(makeSlotArray(content, fullMask), control);       
    end generate;

end Behavioral;
