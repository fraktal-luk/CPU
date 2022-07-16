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

		branchMaskRe: in std_logic_vector(0 to PIPE_WIDTH-1);		
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
        dataInBr: in ControlPacketArray(0 to PIPE_WIDTH-1);

        renamedPtr: out SmallNumber;

        bqPtrOut: out SmallNumber;

		storeValueInput: in ExecResult;
        compareAddressQuickInput: in ExecResult;
        compareQuickPtr: in SmallNumber;

        selectedDataOutput: out ControlPacket; -- result, target, control info, tags

		committing: in std_logic;
		    
        commitBr: in std_logic;
        commitMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        commitEffectiveMask: in std_logic_vector(0 to PIPE_WIDTH-1);

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
        execCausing: in ExecResult;

		nextAccepting: in std_logic;  -- UNUSED	
		sendingSQOut: out std_logic;
		
		committedEmpty: out std_logic;
        committedSending: out std_logic;
        committedDataOut: out ExecResult;
        
            dbState: in DbCoreState
	);
end BranchQueue;


architecture Behavioral of BranchQueue is    
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

	signal selectedDataSlot, selectedDataSlotPre: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	

	signal pStart, pStartNext, pEnd, pEndNext, pTagged, pTaggedNext, pRenamed, pRenamedNext, pSelect, pSelectPrev, pCausing, pCausingPrev,
           pRenamedSeq, pRenamedSeqNext, pStartSeq, pStartSeqNext, pFlushSeq: SmallNumber := (others => '0');

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
       
    earlyInputSending <= prevSendingBr and dataInBr(0).controlInfo.firstBr;
    lateInputSending <= prevSending and dataIn(0).ins.controlInfo.firstBr;
    
    RW: block
       signal earlySerialInput, earlySerialSelected:  std_logic_vector(EARLY_INFO_SIZE-1 downto 0) := (others => '0');
       signal lateSerialInput, lateSerialSelected:  std_logic_vector(LATE_INFO_SIZE-1 downto 0) := (others => '0');
       signal earlySerialMem: EarlyInfoSerialArray := (others => (others => '0'));
       signal lateSerialMem: LateInfoSerialArray := (others => (others => '0'));
       
       signal earlyInput, earlySelected: EarlyInfo := DEFAULT_EARLY_INFO;
       signal lateInput, lateSelected: LateInfo := DEFAULT_LATE_INFO;      
    begin
    
       earlyInput <= getEarlyInfo(dataInBr);
       lateInput <= getLateInfo(dataIn);
       
       earlySerialInput <= serializeEarlyInfo(earlyInput);
       lateSerialInput <= serializeLateInfo(lateInput);
    
    
       SYNCH: process (clk)
       begin
           if rising_edge(clk) then               
               -- Write early data
               if earlyInputSending = '1' then                   
                   earlySerialMem(p2i(pEnd, QUEUE_SIZE)) <= earlySerialInput;
               end if;
    
               -- Write late data
               if lateInputSending = '1' then
                   lateSerialMem(p2i(pTagged, QUEUE_SIZE)) <= lateSerialInput;                 
               end if;
    
               -- Write target array
               if storeValueInput.full = '1' then
                   targetArray(p2i(pCausing, QUEUE_SIZE)) <= storeValueInput.value;
               end if;   
            end if;
            
            if rising_edge(clk) then
               -- Read C.: target array
               if true then
                  targetOutput <= targetArray(p2i(pStartNext, QUEUE_SIZE));                  
               end if;
               
               -- Read Exec: all arrays
               selectedDataSlot <= selectedDataSlotPre;             
           end if;
       end process;
    
       earlySerialSelected <= earlySerialMem(p2i(pSelect, QUEUE_SIZE));
       lateSerialSelected <= lateSerialMem(p2i(pSelect, QUEUE_SIZE));
    
       earlySelected <= deserializeEarlyInfo(earlySerialSelected);
       lateSelected <= deserializeLateInfo(lateSerialSelected);
    
       selectedDataSlotPre <= getMatchedSlot(compareAddressQuickInput.full, compareAddressQuickInput.tag, earlySelected, lateSelected);
    end block;

    pSelect <= compareQuickPtr;

    pStartNext <= addIntTrunc(pStart, 1, QUEUE_PTR_SIZE+1) when committingBr = '1' else pStart;

    pTaggedNext <= pStart when lateEventSignal = '1'
        else       addIntTrunc(pCausingPrev, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
        else       addIntTrunc(pTagged, 1, QUEUE_PTR_SIZE+1) when lateInputSending = '1'
        else       pTagged;
    
    pRenamedNext <= pStart when lateEventSignal = '1'
        else       addIntTrunc(pCausingPrev, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
        else       addIntTrunc(pRenamed, 1, QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
        else       pRenamed;

    pEndNext <= pStart when lateEventSignal = '1'
        else    addIntTrunc(pCausingPrev, 1, QUEUE_PTR_SIZE+1) when execEventSignal = '1'
        else    addIntTrunc(pEnd, 1, QUEUE_PTR_SIZE+1) when earlyInputSending = '1'
        else    pEnd;

    --  CAREFUL! 2 more bits than main pointers because 4 slots in ach row; depends on pipe width (TODO: make automatic) 
    pStartSeqNext <= addTruncZ(pStartSeq, nCommitted, QUEUE_PTR_SIZE + 2 + 1) when committing = '1' else pStartSeq;   

    pRenamedSeqNext <= pStartSeq when lateEventSignal = '1'
                else       pFlushSeq when execEventSignal = '1'
                else       addIntTrunc(pRenamedSeq, slv2u(nInRe), QUEUE_PTR_SIZE + 2 + 1) when prevSendingRe = '1' -- CAREFUL: ptr size here also
                else       pRenamedSeq;

    pFlushSeq <= execCausing.dest;


    SYNCH_POINTERS: process (clk)
    begin
       if rising_edge(clk) then
           pSelectPrev <= pSelect;
    
           pCausing <= pSelectPrev;
           
           pCausingPrev <= pCausing;
           
           pStart <= pStartNext;
           pTagged <= pTaggedNext;
           pEnd <= pEndNext;
           pRenamed <= pRenamedNext;
    
           pStartSeq <= pStartSeqNext;
           pRenamedSeq <= pRenamedSeqNext;
                
           -- State flag update
           memEmpty <= getQueueEmpty(pStartNext, pEndNext, QUEUE_PTR_SIZE);              
           taggedEmpty <= getQueueEmpty(pStartNext, pTaggedNext, QUEUE_PTR_SIZE);
              
           if lateEventSignal = '1' or execEventSignal = '1' then
               recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
           elsif isNonzero(recoveryCounter) = '1' then
               recoveryCounter <= addInt(recoveryCounter, -1);
           end if;
           
           recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here                   
       end if;
    end process;
    
    nCommitted <= i2slv(countOnes(commitMask), SMALL_NUMBER_SIZE);
    nCommittedEffective <= i2slv(countOnes(commitEffectiveMask), SMALL_NUMBER_SIZE);
                
    nInRe <= i2slv(countOnes(branchMaskRe), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    		
    nOut <= nCommitted when committing = '1' else (others => '0');
    
    -- Accepting sigs
    accepting <=  bool2std(addIntTrunc(pStart, 0, QUEUE_PTR_SIZE) /= addIntTrunc(pEnd, 2, QUEUE_PTR_SIZE))
             and bool2std(addIntTrunc(pStart, 0, QUEUE_PTR_SIZE) /= addIntTrunc(pEnd, 1, QUEUE_PTR_SIZE)); -- Need 2 reserve slots because one group could be on the way
    
    -- C. out
    committingBr <= committing and commitBr and not taggedEmpty;
    committedDataOut.full <= committingBr;
    committedDataOut.value <= targetOutput;

    -- Acc sigs
    acceptingOut <= '1';
    almostFull <= '0'; -- TODO: is it deprecated?
    acceptingBr <= accepting;       
    
    -- E. out
    selectedDataOutput.controlInfo <= selectedDataSlot.ins.controlInfo;
    selectedDataOutput.tags <= selectedDataSlot.ins.tags;
    selectedDataOutput.ip <= selectedDataSlot.ins.ip_D;
    selectedDataOutput.target <= selectedDataSlot.ins.target_D;
    selectedDataOutput.nip <= selectedDataSlot.ins.result_D;
    
    -- C. out
    sendingSQOut <= committingBr;
    
    bqPtrOut <= pRenamed;
    
    renamedPtr <= pRenamedSeq;

end Behavioral;
