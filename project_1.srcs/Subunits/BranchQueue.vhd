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

		--dataInRe: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		  branchMaskRe: in std_logic_vector(0 to PIPE_WIDTH-1);		
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		--dataInBr: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    		dataInBr_N: in ControlPacketArray(0 to PIPE_WIDTH-1);

        renamedPtr: out SmallNumber;

        bqPtrOut: out SmallNumber;

		storeValueInput_N: in ExecResult;
        compareAddressQuickInput_N: in ExecResult;
        compareQuickPtr: in SmallNumber;

        selectedDataOutput_N: out ControlPacket; -- result, target, control info, tags

		committing: in std_logic;
		robData: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
        execCausing_N: in ExecResult;

		nextAccepting: in std_logic;  -- UNUSED	
		sendingSQOut: out std_logic;
		
		committedEmpty: out std_logic;
        committedSending: out std_logic;
        committedDataOut_N: out ExecResult
	);
end BranchQueue;


architecture Behavioral of BranchQueue is    
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

	signal selectedDataSlot, selectedDataSlotPre: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	

	signal --pStart_S, pStartNext_S, pEnd_S, pEndNext_S, 
	 pStartLong, pStartLongNext, pEndLong, pEndLongNext,
	-- pTagged_S,
	    --   pRenamed_S, pRenamedNext_S, pTaggedNext_S, 
	       pTaggedLong, pTaggedLongNext, pRenamedLong, pRenamedLongNext,
	       --pSelectPrev_S, pSelect_S, pCausing_S, 
	       pSelectLong, pSelectLongPrev, pCausingLong, pCausingLongPrev,
            pRenamedSeqLong, pRenamedSeqLongNext, pStartSeqLong, pStartSeqLongNext, pFlushSeqLong: SmallNumber := (others => '0');

    signal isFull, isAlmostFull, accepting, committingBr, earlyInputSending, lateInputSending: std_logic := '0';	   
	signal memEmpty, taggedEmpty: std_logic := '1'; -- CAREFUL: starting with '1' 
	   
    signal nInRe, nOut, nCommitted, nCommittedEffective: SmallNumber := (others => '0');   
    	       
    signal recoveryCounter: SmallNumber := (others => '0');

	signal targetArray: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    signal targetOutput: Mword := (others => '0');

    signal earlyInfoMem: EarlyInfoArray(0 to BQ_SIZE-1) := (others => DEFAULT_EARLY_INFO);
     
    signal lateInfoMem: LateInfoArray(0 to BQ_SIZE-1) := (others => DEFAULT_LATE_INFO);
    
    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0'; 
begin
       
       earlyInputSending <= prevSendingBr and --dataInBr(0).ins.controlInfo.firstBr;
                                                dataInBr_N(0).controlInfo.firstBr;
       lateInputSending <= prevSending and dataIn(0).ins.controlInfo.firstBr;

       RW: block
           signal earlySerialInput, earlySerialSelected:  std_logic_vector(EARLY_INFO_SIZE-1 downto 0) := (others => '0');
           signal lateSerialInput, lateSerialSelected:  std_logic_vector(LATE_INFO_SIZE-1 downto 0) := (others => '0');
           signal earlySerialMem: EarlyInfoSerialArray := (others => (others => '0'));
           signal lateSerialMem: LateInfoSerialArray := (others => (others => '0'));
           
           signal earlyInput, earlySelected: EarlyInfo := DEFAULT_EARLY_INFO;
           signal lateInput, lateSelected: LateInfo := DEFAULT_LATE_INFO;      
       begin

           earlyInput <= --getEarlyInfo(dataInBr);
                            getEarlyInfo_N(dataInBr_N);
           lateInput <= getLateInfo(dataIn);
           
           earlySerialInput <= serializeEarlyInfo(earlyInput);
           lateSerialInput <= serializeLateInfo(lateInput);

       
           SYNCH: process (clk)
           begin
               if rising_edge(clk) then               
                   -- Write early data
                   if earlyInputSending = '1' then                   
                       earlySerialMem(p2i(pEndLong, QUEUE_SIZE)) <= earlySerialInput;
                   end if;
    
                   -- Write late data
                   if lateInputSending = '1' then
                       lateSerialMem(p2i(pTaggedLong, QUEUE_SIZE)) <= lateSerialInput;                 
                   end if;
    
                   -- Write target array
                   if storeValueInput_N.full = '1' then
                       targetArray(p2i(pCausingLong, QUEUE_SIZE)) <= storeValueInput_N.value;
                   end if;   
                end if;
                
                if rising_edge(clk) then               
        
                   -- Read C.: target array
                   if true then
                      targetOutput <= targetArray(p2i(pStartLongNext, QUEUE_SIZE));                  
                   end if;
                   
                   -- Read Exec: all arrays
                   selectedDataSlot <= selectedDataSlotPre;             
               end if;
           end process;
    
           earlySerialSelected <= earlySerialMem(p2i(pSelectLong, QUEUE_SIZE));
           lateSerialSelected <= lateSerialMem(p2i(pSelectLong, QUEUE_SIZE));
    
           earlySelected <= deserializeEarlyInfo(earlySerialSelected);
           lateSelected <= deserializeLateInfo(lateSerialSelected);
    
           selectedDataSlotPre <= getMatchedSlot(--pSelect_S,
                                                 compareAddressQuickInput_N.full, compareAddressQuickInput_N.tag, earlySelected, lateSelected);
        end block;

--        pSelect_S <= compareQuickPtr and PTR_MASK_SN;
        pSelectLong <= compareQuickPtr;
       

--        pStartNext_S <= pStartLongNext and PTR_MASK_SN;
--        pTaggedNext_S <= pTaggedLongNext and PTR_MASK_SN;
--        pRenamedNext_S <= pRenamedLongNext and PTR_MASK_SN;
  --      pEndNext_S <= pEndLongNext and PTR_MASK_SN;

 --       pStart_S <= pStartLong and PTR_MASK_SN;
--        pTagged_S <= pTaggedLong and PTR_MASK_SN;
--        pRenamed_S <= pRenamedLong and PTR_MASK_SN;
--        pEnd_S <= pEndLong and PTR_MASK_SN;

        pStartLongNext <= addIntTrunc(pStartLong, 1, QUEUE_PTR_SIZE+1) when committingBr = '1' else pStartLong;
    
        pTaggedLongNext <= pStartLong when lateEventSignal = '1'
            else       addIntTrunc(pCausingLongPrev, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
            else       addIntTrunc(pTaggedLong, 1, QUEUE_PTR_SIZE+1) when lateInputSending = '1'
            else       pTaggedLong;
        
        pRenamedLongNext <= pStartLong when lateEventSignal = '1'
            else       addIntTrunc(pCausingLongPrev, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
            else       addIntTrunc(pRenamedLong, 1, QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
            else       pRenamedLong;

        pEndLongNext <= pStartLong when lateEventSignal = '1'
            else    addIntTrunc(pCausingLongPrev, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
            else    addIntTrunc(pEndLong, 1, QUEUE_PTR_SIZE+1) when earlyInputSending = '1'
            else    pEndLong;

        --  CAREFUL! 2 more bits than main pointers because 4 slots in ach row; depends on pipe width (TODO: make automatic) 
        pStartSeqLongNext <= addTruncZ(pStartSeqLong, nCommitted, QUEUE_PTR_SIZE + 2 + 1) when committing = '1' else pStartSeqLong;   
    
        pRenamedSeqLongNext <= pStartSeqLong when lateEventSignal = '1'
                    else       pFlushSeqLong when execEventSignal = '1'
                    else       addIntTrunc(pRenamedSeqLong, slv2u(nInRe), QUEUE_PTR_SIZE + 2 +1) when prevSendingRe = '1' -- CAREFUL: ptr size here also
                    else       pRenamedSeqLong;

        pFlushSeqLong <= execCausing_N.dest;


	   SYNCH_POINTERS: process (clk)
	   begin
	       if rising_edge(clk) then
--               pSelectPrev_S <= pSelect_S;
               pSelectLongPrev <= pSelectLong;

--	           pCausing_S <= pSelectPrev_S;
	           pCausingLong <= pSelectLongPrev;
	           
	           pCausingLongPrev <= pCausingLong;
	           
	           pStartLong <= pStartLongNext;
               pTaggedLong <= pTaggedLongNext;
               pEndLong <= pEndLongNext;
               pRenamedLong <= pRenamedLongNext;

               pStartSeqLong <= pStartSeqLongNext;
               pRenamedSeqLong <= pRenamedSeqLongNext;
                    
               -- State flag update
               memEmpty <= getQueueEmpty(pStartLongNext, pEndLongNext, QUEUE_PTR_SIZE);              
               taggedEmpty <= getQueueEmpty(pStartLongNext, pTaggedLongNext, QUEUE_PTR_SIZE);
                  
               if lateEventSignal = '1' or execEventSignal = '1' then
                   recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
               elsif isNonzero(recoveryCounter) = '1' then
                   recoveryCounter <= addInt(recoveryCounter, -1);
               end if;
               
               recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here                   
           end if;
       end process;


       nCommitted <= getNumCommittedBr(robData);
       nCommittedEffective <= getNumCommittedEffectiveBr(robData);

       nInRe <= --i2slv(countOnes(getBranchMask((dataInRe))), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    		
                 i2slv(countOnes(branchMaskRe), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    		
       nOut <= nCommitted when committing = '1' else (others => '0');

       -- Accepting sigs
       --accepting <= bool2std(pStart_S /= addIntTrunc(pEnd_S, 2, QUEUE_PTR_SIZE)) and bool2std(pStart_S /= addIntTrunc(pEnd_S, 1, QUEUE_PTR_SIZE)); -- Need 2 reserve slots because one group could be on the way
       --accepting <= bool2std(addIntTrunc(pStart_S, 0, QUEUE_PTR_SIZE) /= addIntTrunc(pEnd_S, 2, QUEUE_PTR_SIZE)) and bool2std(addIntTrunc(pStart_S, 0, QUEUE_PTR_SIZE) /= addIntTrunc(pEnd_S, 1, QUEUE_PTR_SIZE)); -- Need 2 reserve slots because one group could be on the way
       accepting <= bool2std(addIntTrunc(pStartLong, 0, QUEUE_PTR_SIZE) /= addIntTrunc(pEndLong, 2, QUEUE_PTR_SIZE)) and bool2std(addIntTrunc(pStartLong, 0, QUEUE_PTR_SIZE) /= addIntTrunc(pEndLong, 1, QUEUE_PTR_SIZE)); -- Need 2 reserve slots because one group could be on the way

       -- C. out
       committingBr <= committing and robData(0).ins.controlInfo.firstBr and not taggedEmpty;
       committedDataOut_N.full <= committingBr;
       committedDataOut_N.value <= targetOutput;

    -- Acc sigs
	acceptingOut <= '1';
	almostFull <= '0'; -- TODO: is it deprecated?
    acceptingBr <= accepting;       

    -- E. out
    selectedDataOutput_N.controlInfo <= selectedDataSlot.ins.controlInfo;
    selectedDataOutput_N.tags <= selectedDataSlot.ins.tags;
    selectedDataOutput_N.ip <= selectedDataSlot.ins.ip;
    selectedDataOutput_N.target <= selectedDataSlot.ins.target;
    selectedDataOutput_N.nip <= selectedDataSlot.ins.result;

    -- C. out
	sendingSQOut <= committingBr;
	
	bqPtrOut <= pRenamedLong;

    renamedPtr <= pRenamedSeqLong;

end Behavioral;
