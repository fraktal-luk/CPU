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

		--prevSendingBr: in std_logic;
        dataInBr: in ControlPacketArray(0 to PIPE_WIDTH-1);
        ctrlInBr: in ControlPacket;
        
		frontSending: in std_logic;
		branchMaskFront: in std_logic_vector(0 to PIPE_WIDTH-1);		

		renamedSending: in std_logic;
		renamedDataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
        renamedCtrl: in ControlPacket;


        renamedPtr: out SmallNumber;
        bqPtrOut: out SmallNumber;

		storeValueInput: in ExecResult;
        compareAddressQuickInput: in ExecResult;

        selectedDataOutput: out ControlPacket; -- result, target, control info, tags

		committing: in std_logic;

        commitBr: in std_logic;
        commitMask: in std_logic_vector(0 to PIPE_WIDTH-1);


		events: in EventState;

		nextAccepting: in std_logic;  -- UNUSED	

        committedDataOut: out ExecResult;

        dbState: in DbCoreState
	);
end BranchQueue;


architecture Behavioral of BranchQueue is
    alias lateEventSignal is events.lateCausing.full;
    alias execEventSignal is events.execCausing.full;

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
    alias pFlushSeq is events.execCausing.dest; -- TODO: incorrect?

    --alias prevSendingBr is ctrlInBr.full;

    signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';
begin
    earlyInputSending <= ctrlInBr.full and dataInBr(0).controlInfo.firstBr;
    lateInputSending <= --renamedSending and renamedCtrl.controlInfo.firstBr;
                        renamedCtrl.full and renamedCtrl.controlInfo.firstBr;

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
       lateSerialInput <= serializeLateInfo(getLateInfo(renamedDataIn));

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
        else       addIntTrunc(pRenamed, 1, QUEUE_CAP_SIZE) when frontSending = '1'
        else       pRenamed;

    pEndNext <= pStart when lateEventSignal = '1'
        else    addIntTrunc(pCausingPrev, 1, QUEUE_CAP_SIZE) when execEventSignal = '1'
        else    addIntTrunc(pEnd, 1, QUEUE_CAP_SIZE) when earlyInputSending = '1'
        else    pEnd;

    pStartSeqNext <= addTruncZ(pStartSeq, countSN(commitMask), BQ_SEQ_PTR_SIZE + 1) when committing = '1' else pStartSeq;

    pRenamedSeqNext <= pStartSeq when lateEventSignal = '1'
            else       pFlushSeq when execEventSignal = '1'
            else       addTruncZ(pRenamedSeq, countSN(branchMaskFront), BQ_SEQ_PTR_SIZE + 1) when frontSending = '1'
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


    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate

        type EntryState is (empty, allocated, waiting, done);
        type EntryRow is array(0 to PIPE_WIDTH-1) of EntryState;
        type EntryRowArray is array(0 to QUEUE_SIZE-1) of EntryRow;

        signal states: EntryRowArray := (others => (others => empty));

--        signal wp1, wp2, wp3: SmallNumber := sn(0);

            procedure DB_writeEarlyStates(signal table: inout EntryRowArray; allocP: SmallNumber; input: ControlPacketArray) is
            begin
                for i in 0 to PIPE_WIDTH-1 loop
                    if input(i).full = '1' then
                        table(p2i(allocP, QUEUE_SIZE))(i) <= allocated;
                    end if;
                end loop;
            end procedure;

            procedure DB_writeLateStates(signal table: inout EntryRowArray; endP: SmallNumber; input: InstructionSlotArray) is
            begin
                for i in 0 to PIPE_WIDTH-1 loop
                    if input(i).full = '1' then
                        table(p2i(endP, QUEUE_SIZE))(i) <= waiting;
                    end if;
                end loop;
            end procedure;

            procedure DB_commitStates(signal table: inout EntryRowArray; startP: SmallNumber) is
            begin
                table(p2i(startP, QUEUE_SIZE)) <= (others => empty);
            end procedure;
            
            
            procedure DB_killStates(signal table: inout EntryRowArray; startP: SmallNumber; bqp: SmallNumber; tag: InsTag; lateEvent, execEvent: std_logic) is
                variable row, col: natural := 0;
                variable tagHigh, tagHighTrunc: SmallNumber;
            begin
                if lateEvent = '1' then
                    table <= (others => (others => empty));
                    return;
                end if;

                if execEvent /= '1' then
                    return;
                end if;

                tagHigh := getTagHighSN(tag);
                tagHighTrunc := --tagHigh and PTR_MASK_SN;
                                bqp and PTR_MASK_SN;
                row := slv2u(tagHighTrunc);
                col := slv2u(getTagLow(tag));

                loop
                    col := col + 1;
                    if col = PIPE_WIDTH then
                        exit;
                    end if;
                    table(row)(col) <= empty;
                end loop;
                
                loop
                    row := (row + 1) mod QUEUE_SIZE;
                    if row = p2i(startP, QUEUE_SIZE) then
                        exit;
                    end if;
                    table(row) <= (others => empty);
                end loop;
            end procedure;

    begin

        process (clk)
        begin
            if rising_edge(clk) then

                if earlyInputSending = '1' then
                    DB_writeEarlyStates(states, pEnd, dataInBr);
                end if;

                if lateInputSending = '1' then
                    DB_writeLateStates(states, pTagged, renamedDataIn);
                end if;


                if committing = '1' then
                    DB_commitStates(states, pStart);
                end if;


                if (events.execCausing.full or events.lateCausing.full) = '1' then
                    DB_killStates(states, pStart, events.execTags.bqPointer, events.execCausing.tag, events.lateCausing.full, events.execCausing.full);
                end if;


                if DB_LOG_EVENTS then
                    if dbState.dbSignal = '1' then
                        report "BQ reporting ";
                        --printContent(queueContent_NS, addresses, storeValues, pStart, pTagged, getNamePrefix(IS_LOAD_QUEUE));
                    end if;
                end if;
            end if;
        end process;
    end generate;
    -- pragma synthesis on

end Behavioral;
