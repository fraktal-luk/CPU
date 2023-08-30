----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
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

        selectedDataOutput: out ControlPacket; -- result, target, control info, tags

		committing: in std_logic;

        commitBr: in std_logic;
        commitMask: in std_logic_vector(0 to PIPE_WIDTH-1);

		--lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
        execCausing: in ExecResult;

		events: in EventState;

		nextAccepting: in std_logic;  -- UNUSED	

        committedDataOut: out ExecResult;

        dbState: in DbCoreState
	);
end BranchQueue;


architecture Behavioral of BranchQueue is
    alias lateEventSignal is events.lateEvent;
    --alias execEventSignal is events.execEvent;
 
	constant PTR_MASK_SN: SmallNumber := sn(QUEUE_SIZE-1);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

	signal pStart, pStartNext, pEnd, pEndNext, pTagged, pTaggedNext, pRenamed, pRenamedNext, pSelectPrev, pCausing, pCausingPrev,
           pRenamedSeq, pRenamedSeqNext, pStartSeq, pStartSeqNext: SmallNumber := (others => '0');

    signal canAccept, earlyInputSending, lateInputSending, committingBr: std_logic := '0';	   
	signal taggedEmpty: std_logic := '1'; -- CAREFUL: starting with '1'

    signal nFull, nFullNext: SmallNumber := sn(0);   

	signal selectedDataSlot: ControlPacket := DEFAULT_CONTROL_PACKET;	
    signal targetOutput: Mword := (others => '0');

    alias pSelect is compareAddressQuickInput.dest;
    alias pFlushSeq is execCausing.dest;

    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0'; 
begin
    earlyInputSending <= prevSendingBr and dataInBr(0).controlInfo.firstBr;
    lateInputSending <= prevSending and dataIn(0).ins.controlInfo.firstBr_T;

    RW: block
       signal earlySerialInput, earlySerialSelected:  std_logic_vector(EARLY_INFO_SIZE-1 downto 0) := (others => '0');
       signal lateSerialInput, lateSerialSelected:  std_logic_vector(LATE_INFO_SIZE-1 downto 0) := (others => '0');

       signal earlySerialMem: EarlyInfoSerialArray := (others => (others => '0'));
       signal lateSerialMem: LateInfoSerialArray := (others => (others => '0'));
       signal targetArray: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));

       signal earlySelected: EarlyInfo := DEFAULT_EARLY_INFO;
       signal lateSelected: LateInfo := DEFAULT_LATE_INFO;      
    begin
       earlySerialInput <= serializeEarlyInfo(getEarlyInfo(dataInBr));
       lateSerialInput <= serializeLateInfo(getLateInfo(dataIn));

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

               -- Read C.: target array
               targetOutput <= targetArray(p2i(pStartNext, QUEUE_SIZE));

               -- Read Exec: all arrays
               --selectedDataSlot <= getMatchedSlot(compareAddressQuickInput.full, compareAddressQuickInput.tag, earlySelected, lateSelected);
           end if;
       end process;
               selectedDataSlot <= getMatchedSlot(compareAddressQuickInput.full, compareAddressQuickInput.tag, earlySelected, lateSelected);


       earlySerialSelected <= earlySerialMem(p2i(pSelect, QUEUE_SIZE));
       earlySelected <= deserializeEarlyInfo(earlySerialSelected);

       lateSerialSelected <= lateSerialMem(p2i(pSelect, QUEUE_SIZE));
       lateSelected <= deserializeLateInfo(lateSerialSelected);
    end block;

    pStartNext <= addIntTrunc(pStart, 1, QUEUE_CAP_SIZE) when committingBr = '1' else pStart;

    pTaggedNext <= pStart when lateEventSignal = '1'
        else       addIntTrunc(pCausingPrev, 1, QUEUE_CAP_SIZE) when execEventSignal = '1'
        else       addIntTrunc(pTagged, 1, QUEUE_CAP_SIZE) when lateInputSending = '1'
        else       pTagged;

    pRenamedNext <= pStart when lateEventSignal = '1'
        else       addIntTrunc(pCausingPrev, 1, QUEUE_CAP_SIZE) when execEventSignal = '1'
        else       addIntTrunc(pRenamed, 1, QUEUE_CAP_SIZE) when prevSendingRe = '1'
        else       pRenamed;

    pEndNext <= pStart when lateEventSignal = '1'
        else    addIntTrunc(pCausingPrev, 1, QUEUE_CAP_SIZE) when execEventSignal = '1'
        else    addIntTrunc(pEnd, 1, QUEUE_CAP_SIZE) when earlyInputSending = '1'
        else    pEnd;

    pStartSeqNext <= addTruncZ(pStartSeq, countSN(commitMask), BQ_SEQ_PTR_SIZE + 1) when committing = '1' else pStartSeq;

    pRenamedSeqNext <= pStartSeq when lateEventSignal = '1'
            else       pFlushSeq when execEventSignal = '1'
            else       addTruncZ(pRenamedSeq, countSN(branchMaskRe), BQ_SEQ_PTR_SIZE + 1) when prevSendingRe = '1'
            else       pRenamedSeq;

    nFullNext <= getNumFull(pStartNext, pEndNext, QUEUE_PTR_SIZE);


    SYNCH_POINTERS: process (clk)
    begin
       if rising_edge(clk) then
           pSelectPrev <= pSelect;
           pCausing <= --pSelectPrev;
                        pSelect;
           pCausingPrev <= --pCausing;
                            pSelect;

           pStart <= pStartNext;
           pTagged <= pTaggedNext;
           pRenamed <= pRenamedNext;
           pEnd <= pEndNext;

           pStartSeq <= pStartSeqNext;
           pRenamedSeq <= pRenamedSeqNext;

           -- State flag update
           taggedEmpty <= getQueueEmpty(pStartNext, pTaggedNext, QUEUE_PTR_SIZE);

           nFull <= nFullNext;
           canAccept <= not cmpGtU(nFullNext, QUEUE_SIZE-2); -- 2 free slots needed because 1 cycle delay in frontend sending to BQ
       end if;
    end process;

    -- C. out
    committingBr <= committing and commitBr and not taggedEmpty; -- CHECK, TODO: why tagged empty is needed, doesn't Sequencer know it from ROB group?
    committedDataOut.full <= committingBr;
    committedDataOut.value <= targetOutput;

    -- Acc sigs
    acceptingOut <= '1';
    acceptingBr <= canAccept;

    -- E. out
    selectedDataOutput <= selectedDataSlot;

    -- C. out    
    bqPtrOut <= pRenamed;
    renamedPtr <= pRenamedSeq;

end Behavioral;
