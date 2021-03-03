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
use work.LogicBQ.all;


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

               signal intps_T, floatps_T: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
       
            constant STATIC0_SIZE: natural := 3*PIPE_WIDTH-2;
            signal staticInput0, staticOutput0: std_logic_vector(STATIC0_SIZE-1 downto 0) := (others => '0');
            
            type StaticMem0 is array(0 to BQ_SIZE-1) of std_logic_vector(STATIC0_SIZE-1 downto 0);
            
            type FrontBranchesMem0 is array(0 to BQ_SIZE-1) of std_logic_vector(0 to PIPE_WIDTH-1);
            
            signal staticMemContent0: StaticMem0 := (others => (others => '0'));
            signal frontBranchesContent0: FrontBranchesMem0 := (others => (others => '0'));

            constant DEST_FLAGS_SIZE: natural := 2*PIPE_WIDTH-2; 


            function getStaticInput0(insVec: InstructionSlotArray) return std_logic_vector is
                variable res: std_logic_vector(STATIC0_SIZE-1 downto 0);-- := (others => '0');
                --variable v3: std_logic_vector(2 to 0);
            begin            
                for i in 0 to PIPE_WIDTH-2 loop -- -2 cause [0] is not used
                    res(i) := insVec(1+i).ins.virtualArgSpec.intDestSel;
                    res(i + PIPE_WIDTH-1) := insVec(1+i).ins.virtualArgSpec.floatDestSel;
                end loop;
                
                for i in 0 to PIPE_WIDTH-1 loop
                    res(DEST_FLAGS_SIZE + i) := insVec(i).ins.controlInfo.frontBranch;
                end loop;
                     
                return res;
            end function;
            
            function getTakenVecInt(static0: std_logic_vector) return std_logic_vector is
                variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
            begin
                for i in 0 to PIPE_WIDTH-2 loop
                    res(1 + i) := static0(i);
                end loop;
                return res;
            end function;

            function getTakenVecFloat(static0: std_logic_vector) return std_logic_vector is
                variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
            begin
                for i in 0 to PIPE_WIDTH-2 loop
                    res(1 + i) := static0(i + PIPE_WIDTH-1);
                end loop;
                return res;
            end function;
            
            function getFrontBranch(static0: std_logic_vector) return std_logic_vector is
                variable res: std_logic_vector(0 to PIPE_WIDTH-1);
            begin
                for i in 0 to PIPE_WIDTH-1 loop
                    res(i) := static0(i + DEST_FLAGS_SIZE);
                end loop;
                return res;
            end function;            

            
            
	       signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';
        	   
	   function getMatchedSlot(allBranches: PipeStageArray; slotPtr: SmallNumber; cmpAdrSlot: InstructionSlot; ipBase: MWord; trgs, ress: MwordArray;
	                           intps, floatps: SmallNumberArray; frontBranches, intTakeVec, floatTakeVec: std_logic_vector)
	   return InstructionSlot is
	       variable res, storedIns: InstructionSlot := DEFAULT_INS_SLOT;
	       variable lowPtr: natural := 0;
	       variable resLow: Mword := (others => '0');
	       variable tmpNumI, tmpNumF: SmallNumber := (others => '0');
	       
	       --variable intTakeVec, floatTakeVec: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	   begin
	       lowPtr := slv2u(getTagLow(cmpAdrSlot.ins.tags.renameIndex));
	       storedIns := allBranches(slv2u(slotPtr))(lowPtr);
	       
	       res.ins.controlInfo.frontBranch := --storedIns.ins.controlInfo.frontBranch;
	                                           frontBranches(lowPtr);
	       res.ins.controlInfo.confirmedBranch := storedIns.ins.controlInfo.confirmedBranch;
	       
	       
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
                for i in 1 to PIPE_WIDTH-1 loop
                    --intTakeVec(i) := intps(i)(7);
                    --floatTakeVec(i) := floatps(i)(7);
                end loop;
           
                tmpNumI(1 downto 0) := intps(lowPtr)(1 downto 0);
                tmpNumF(1 downto 0) := floatps(lowPtr)(1 downto 0);
                if lowPtr = 0 then
                    res.ins.tags.intPointer := intps(0);
                    res.ins.tags.floatPointer := floatps(0);                  
                else
                    res.ins.tags.intPointer := --add(intps(0), tmpNumI);
                                                 addInt(intps(0), countOnes(intTakeVec(1 to lowPtr)));
                    res.ins.tags.floatPointer := --add(floatps(0), tmpNumF);
                                                 addInt(floatps(0), countOnes(floatTakeVec(1 to lowPtr)));                                 
                end if;
           end if;
  
	       return res;
	   end function;


               function extractFrontBr(stage: PipeStage)
               return std_logic_vector is
	               variable res: std_logic_vector(0 to PIPE_WIDTH-1);
	           begin    
	               for i in 0 to PIPE_WIDTH-1 loop
	                   res(i) := stage(i).ins.controlInfo.frontBranch;
	               end loop;
	               return res;
	           end function;

               function getFrontBr(allBranches: PipeStageArray; slotPtr: SmallNumber; cmpAdrSlot: InstructionSlot; ipBase: MWord; trgs, ress: MwordArray;
                                       intps, floatps: SmallNumberArray)
               return std_logic_vector is
	               variable res: std_logic_vector(0 to PIPE_WIDTH-1);
	           begin    
	               for i in 0 to PIPE_WIDTH-1 loop
	                   res(i) := allBranches(slv2u(slotPtr))(i).ins.controlInfo.frontBranch;
	               end loop;
	               return res;
	           end function;
	           
	           

               function TMP_unfoldIntPs(allBranches: PipeStageArray; slotPtr: SmallNumber; cmpAdrSlot: InstructionSlot; ipBase: MWord; trgs, ress: MwordArray;
                                       intps, floatps: SmallNumberArray; intTakeVec, floatTakeVec: std_logic_vector)
               return SmallNumberArray is
                   variable res: SmallNumberArray(0 to PIPE_WIDTH-1);
                   variable lowPtr: natural := 0;
                   variable resLow: Mword := (others => '0');
                   variable tmpNumI, tmpNumF: SmallNumber := (others => '0');
                   
                   --variable intTakeVec, floatTakeVec: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
               begin
--                        for i in 1 to PIPE_WIDTH-1 loop
--                            intTakeVec(i) := allBranches(slv2u(slotPtr))(i).ins.virtualArgSpec.intDestSel;
--                            floatTakeVec(i) := allBranches(slv2u(slotPtr))(i).ins.virtualArgSpec.floatDestSel;
--                        end loop;

                        for i in 0 to PIPE_WIDTH-1 loop
                            if i = 0 then
                                res(i) := intps(0);
                            else
                                res(i) := addInt(intps(0), countOnes(intTakeVec(1 to i)));                                 
                            end if;
                        end loop;
          
                   return res;
               end function;

               function TMP_unfoldFloatPs(allBranches: PipeStageArray; slotPtr: SmallNumber; cmpAdrSlot: InstructionSlot; ipBase: MWord; trgs, ress: MwordArray;
                                       intps, floatps: SmallNumberArray; intTakeVec, floatTakeVec: std_logic_vector)
               return SmallNumberArray is
                   variable res: SmallNumberArray(0 to PIPE_WIDTH-1);
                   variable lowPtr: natural := 0;
                   variable resLow: Mword := (others => '0');
                   variable tmpNumI, tmpNumF: SmallNumber := (others => '0');
                   
                   --variable intTakeVec, floatTakeVec: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
               begin
--                        for i in 1 to PIPE_WIDTH-1 loop
--                            intTakeVec(i) := allBranches(slv2u(slotPtr))(i).ins.virtualArgSpec.intDestSel;
--                            floatTakeVec(i) := allBranches(slv2u(slotPtr))(i).ins.virtualArgSpec.floatDestSel;
--                        end loop;

                        for i in 0 to PIPE_WIDTH-1 loop
                            if i = 0 then
                                res(i) := floatps(0);
                            else
                                res(i) := addInt(floatps(0), countOnes(floatTakeVec(1 to i)));                                 
                            end if;
                        end loop;
          
                   return res;
               end function;

       signal allBranchesInputTmp: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
       
                   signal intTakeVec, floatTakeVec, frontBranches, frontBranches_T: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
       
	begin	
       isSending <= committingBr;

       selectedDataSlot <= getMatchedSlot(allBranches, pSelect, compareAddressInput, ipOutputA, trgs, ress, intps, floatps,
                                        frontBranches_T, intTakeVec, floatTakeVec);
        
              frontBranches <= getFrontBr(allBranches, pSelect, compareAddressInput, ipOutputA, trgs, ress, intps, floatps);
        
                frontBranches_T <= frontBranchesContent0(slv2u(pSelect));--getFrontBranch(staticOutput0);
                intTakeVec <= getTakenVecInt(staticOutput0);
                floatTakeVec <= getTakenVecFloat(staticOutput0);
        
               intps_T <= TMP_unfoldIntPs(allBranches, pSelect, compareAddressInput, ipOutputA, trgs, ress, intps, floatps, intTakeVec, floatTakeVec);
               floatps_T <= TMP_unfoldFloatPs(allBranches, pSelect, compareAddressInput, ipOutputA, trgs, ress, intps, floatps, intTakeVec, floatTakeVec);
        
        
            staticInput0 <= getStaticInput0(dataIn);
        
       ipOutputA <= ipArray(slv2u(pSelect));
       trgs <= (trg0(slv2u(pSelect)), trg1(slv2u(pSelect)), trg2(slv2u(pSelect)), trg3(slv2u(pSelect)));
       ress <= (res0(slv2u(pSelect)), res1(slv2u(pSelect)), res2(slv2u(pSelect)), res3(slv2u(pSelect)));
       intps <= (intp0(slv2u(pSelect)), intp1(slv2u(pSelect)), intp2(slv2u(pSelect)), intp3(slv2u(pSelect)));
       floatps <= (floatp0(slv2u(pSelect)), floatp1(slv2u(pSelect)), floatp2(slv2u(pSelect)), floatp3(slv2u(pSelect)));
       
            staticOutput0 <= staticMemContent0(slv2u(pSelect));
       
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
                        allBranches(slv2u(pEnd))(i).ins.controlInfo.frontBranch <= allBranchesInputTmp(i).ins.controlInfo.frontBranch;
                        allBranches(slv2u(pEnd))(i).ins.controlInfo.confirmedBranch <= allBranchesInputTmp(i).ins.controlInfo.confirmedBranch;
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
                        
                        
                        frontBranchesContent0(slv2u(pEnd)) <= extractFrontBr(dataInBr);
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
                        
                        staticMemContent0(slv2u(pTagged)) <= staticInput0;                  
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
--        ch0 <= bool2std((pStartLong and PTR_MASK_SN) = pStart);
--        ch1 <= bool2std((pTaggedLong and PTR_MASK_SN) = pTagged);
--        ch2 <= bool2std((pEndLong and PTR_MASK_SN) = pEnd);
--        ch3 <= bool2std((pRenamedLong and PTR_MASK_SN) = pRenamed);
       
        ch0 <= bool2std(intps = intps_T);
        ch1 <= bool2std(floatps = floatps_T);
        ch2 <= bool2std(frontBranches = frontBranches_T);
        
	end block;
	
	bqPtrOut <= pRenamedLong;
	

	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       --queueText <= getInsStringArray(makeSlotArray(content, fullMask), control);       
    end generate;

end Behavioral;
