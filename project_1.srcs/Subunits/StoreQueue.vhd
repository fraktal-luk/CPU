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
use work.LogicQueues.all;


entity StoreQueue is
	generic(
		QUEUE_SIZE: integer := 8;
		IS_LOAD_QUEUE: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		acceptingOut: out std_logic; -- UNUSED
		almostFull: out std_logic;   -- UNUSED
		acceptAlloc: out std_logic;
		
   	    prevSendingRe: in std_logic;
		prevSending: in std_logic;

        renameMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        inputMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        systemMask: in std_logic_vector(0 to PIPE_WIDTH-1);

        renamedPtr: out SmallNumber;

		compareAddressEarlyInput: in ExecResult;
        compareAddressEarlyInput_Ctrl: in ControlPacket;

		compareAddressInput: in ExecResult;
		compareAddressCtrl: in ControlPacket;

		storeValueResult: in ExecResult;

        selectedDataOutput: out ControlPacket;
        selectedDataResult: out ExecResult;

		committing: in std_logic;
        commitMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        commitEffectiveMask: in std_logic_vector(0 to PIPE_WIDTH-1);

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
    	execCausing: in ExecResult;

		nextAccepting: in std_logic;		

		committedEmpty: out std_logic;
		committedSending: out std_logic;
		committedDataOut: out ControlPacket;
		
		dbState: in DbCoreState
	);
end StoreQueue;


architecture Behavioral of StoreQueue is
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

	signal addressMatchMask, newerLQ, newerRegLQ, newerNextLQ, olderNextSQ, olderRegSQ, olderSQ, olderSQ_Early: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal adrPtr, adrPtrEarly, adrPtrPrev, pSelect, pSelectPrev, pSelectEarly_Base, pSelectEarly_BasePrev, pSelectEarly, pSelectEarlyPrev, pStart, pStartNext,
	       pDrain, pDrainNext, pDrainPrev, pDrainPrevPrev,
           pTagged, pTaggedNext, pRenamed, pRenamedNext, nFull, nFullNext, nAlloc, nAllocNext, nIn, nInRe, nOut, nCommitted, nCommittedEffective, recoveryCounter: SmallNumber := (others => '0');

    signal queueContent, queueContentShifting: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);
    signal addresses, storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));

    signal canAlloc, drainReq, drainEqual, isSending, isDrainingPrev, isSelected, isSelectedNext, isSelected_Early, sqMissed, missing, committedEmptySig,
            dummy0: std_logic := '0';

    signal selectedOutputSig, committedOutputSig: ControlPacket := DEFAULT_CONTROL_PACKET;
    signal adrValuePrev: Mword := (others => '0');

    signal updateResult, selectedDataResultSig: ExecResult := DEFAULT_EXEC_RESULT;

	alias compareAddressInputOp is compareAddressCtrl.op;
	alias compareAddressInputEarlyOp is compareAddressEarlyInput_Ctrl.op;
	alias storeValuePtr is storeValueResult.dest;
    alias pFlush is execCausing.dest;

    alias adrValueEarly is compareAddressEarlyInput.value;
    alias adrValue is compareAddressInput.value;

        procedure DB_logFW(adrInput: ExecResult; pSelect: SmallNumber; IS_LOAD_QUEUE: boolean) is
        begin
             -- pragma synthesis off
            if DB_LSQ_TRACKING then
                report "";
                report "DEBUG: Store FW(" & boolean'image(IS_LOAD_QUEUE) & "): seqNum " & work.CpuText.w2hex(adrInput.dbInfo.seqNum) & " from entry " & work.CpuText.slv2hex(pSelect);
                report "";
            end if;
            -- pragma synthesis on
        end procedure;

    signal ch0, ch1, ch2, ch3, chi, chii: std_logic := '0';
begin
            ch0 <= '1';

    -- Ptr for random access updating
    adrPtr <= compareAddressCtrl.tags.lqPointer when IS_LOAD_QUEUE
         else compareAddressCtrl.tags.sqPointer;

    adrPtrEarly <= compareAddressEarlyInput_Ctrl.tags.lqPointer when IS_LOAD_QUEUE
              else compareAddressEarlyInput_Ctrl.tags.sqPointer;

    updateResult.full <= compareAddressInput.full and isLoadOp(compareAddressInputOp) when IS_LOAD_QUEUE
                    else compareAddressInput.full and isStoreOp(compareAddressInputOp);
    updateResult.dest <= adrPtr;
    updateResult.value <= adrValue;

    -- Read ptr determinded by address matching - SQ only
    pSelect <= addTruncZ(findNewestMatchIndex(olderSQ, sn(0), nFull, QUEUE_PTR_SIZE), pDrainPrev, QUEUE_PTR_SIZE) when false
          else addTruncZ(pSelectEarly_BasePrev, pDrainPrevPrev, QUEUE_PTR_SIZE);

    pSelectEarly <= addTruncZ(findNewestMatchIndex(olderSQ_Early, sn(0), nFull, QUEUE_PTR_SIZE), pDrainPrev, QUEUE_PTR_SIZE);
        pSelectEarly_Base <= findNewestMatchIndex(olderSQ_Early, sn(0), nFull, QUEUE_PTR_SIZE);

    -- LQ only
    LQ_MATCH: if IS_LOAD_QUEUE generate
    begin
        addressMatchMask <= getAddressMatching(queueContent, adrValue) and getAddressCompleted(queueContent);       
               -- TODO: could/should be pStartNext?
        newerNextLQ <= cmpIndexAfter(pStart, pTagged, compareAddressEarlyInput_Ctrl.tags.lqPointer, QUEUE_SIZE, PTR_MASK_SN) and getWhichMemOp(queueContent)
                                                                         when isStoreMemOp(compareAddressEarlyInput_Ctrl.op) = '1' else (others => '0');
        newerLQ <=     newerRegLQ and addressMatchMask;
    end generate;

    -- SQ only
    SQ_MATCH: if not IS_LOAD_QUEUE generate
    begin
        addressMatchMask <= getAddressMatching(queueContentShifting, adrValue) and getAddressCompleted(queueContentShifting);

        olderNextSQ <= cmpIndexBefore(sn(0), nFull, subTruncZ(compareAddressEarlyInput_Ctrl.tags.sqPointer, pDrainPrev, QUEUE_SIZE), QUEUE_SIZE, PTR_MASK_SN) -- TODO: nFull is not correct 
                                                                         when isLoadMemOp(compareAddressEarlyInput_Ctrl.op) = '1' else (others => '0');
        olderSQ <=   olderRegSQ and addressMatchMask;
        
        olderSQ_Early <= olderNextSQ and getAddressMatching_Low(queueContentShifting, adrValueEarly) and getAddressCompleted_Low(queueContentShifting);
    end generate;

    isSelectedNext <= isSelected_Early when not IS_LOAD_QUEUE
                 else compareAddressInput.full and isNonzero(newerLQ);


    QUEUE_DATA: block
        signal drainValue, selectedValue, drainAddress, selectedAddress: Mword := (others => '0');
        signal selectedEntry, drainEntry: QueueEntry := DEFAULT_QUEUE_ENTRY;
    begin

        QUEUE_DATA_SYNC: process (clk)
        begin
            if rising_edge(clk) then
                -- SQ only
                queueContentShifting <= shiftQueueContent(queueContentShifting, pDrain, nFullNext, execEventSignal or lateEventSignal, isDrainingPrev,
                                                          compareAddressInput.full, adrPtr, compareAddressInputOp, adrValue,
                                                          compareAddressEarlyInput.full, adrPtrEarly, compareAddressInputEarlyOp, adrValueEarly,
                                                          QUEUE_PTR_SIZE);

                -- Front input
                if prevSending = '1' then
                    updateOnInput(queueContent, pTagged, inputMask, systemMask, IS_LOAD_QUEUE);
                end if;

                -- E. adr update
                if compareAddressInput.full = '1' then
                    updateAddress(queueContent, updateResult, IS_LOAD_QUEUE);
                    addresses(p2i(adrPtr, QUEUE_SIZE))(31 downto 12) <= adrValue(31 downto 12);
                    addresses(p2i(adrPtr, QUEUE_SIZE))(11 downto 0) <= adrValue(11 downto 0);
                end if;

                -- E. val update
                if storeValueResult.full = '1' and not IS_LOAD_QUEUE then
                    updateValue(queueContent, storeValuePtr);
                    storeValues(p2i(storeValuePtr, QUEUE_SIZE)) <= storeValueResult.value;
                end if;

                selectedEntry <= queueContent(p2i(pSelect, QUEUE_SIZE));
                selectedValue <= storeValues(p2i(pSelect, QUEUE_SIZE));
                selectedAddress <= addresses(p2i(pSelect, QUEUE_SIZE));

                -- D. outputs
                drainEntry <= queueContent(p2i(pDrain, QUEUE_SIZE));
                drainValue <= storeValues(p2i(pDrain, QUEUE_SIZE));
                drainAddress <= addresses(p2i(pDrain, QUEUE_SIZE));
            end if;
        end process;

        selectedDataResultSig <= selDataRes(pSelectPrev);
        selectedOutputSig <= makeSelectedOutputLQ(isSelected) when IS_LOAD_QUEUE
                        else makeSelectedOutputSQ(getDrainOutput(selectedEntry, selectedAddress, selectedValue), isSelected, sqMissed);

        committedOutputSig <= makeCommittedOutputSQ(getDrainOutput(drainEntry, drainAddress, drainValue), isDrainingPrev);

    end block;


    CHECK_FW: block
        signal adrReady, adrMatch, adrMatchLast, tagMatchingLast, dataReady, adrMatchNormal, adrMatchRecent: std_logic := '0';
    begin
        adrReady <= queueContent(p2i(pSelect, QUEUE_SIZE)).completedA;
        adrMatch <= addressHighMatching(addresses(p2i(pSelect, QUEUE_SIZE)), adrValue);
        adrMatchNormal <= adrReady and adrMatch;

        adrMatchLast <= addressHighMatching(adrValuePrev, adrValue);
        tagMatchingLast <= --bool2std((pSelectEarlyPrev and PTR_MASK_SN) = (adrPtrPrev and PTR_MASK_SN));
                           bool2std((pSelect and PTR_MASK_SN) = (adrPtrPrev and PTR_MASK_SN));
        adrMatchRecent <= adrMatchLast and tagMatchingLast;
        
        dataReady <= queueContent(p2i(pSelect, QUEUE_SIZE)).completedV;
        
        missing <= isSelected_Early and (not (adrMatchNormal or adrMatchRecent) or not dataReady);
    end block;


    pDrainNext <= addIntTrunc(pDrain, 1, QUEUE_PTR_SIZE+1) when drainReq = '1' else pDrain;
    pStartNext <= addTruncZ(pStart, nCommittedEffective, QUEUE_PTR_SIZE+1) when committing = '1'
             else pStart;

    pTaggedNext <= pStart when lateEventSignal = '1'
            else   pFlush when execEventSignal = '1' 
            else   addIntTrunc(pTagged, countOnes(inputMask), QUEUE_PTR_SIZE+1) when prevSending = '1'
            else   pTagged;

    pRenamedNext <= pStart when lateEventSignal = '1'
            else    pFlush when execEventSignal = '1'
            else    addIntTrunc(pRenamed, slv2u(nInRe), QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
            else    pRenamed;

    process (clk)
    begin
        if rising_edge(clk) then

            isDrainingPrev <= drainReq;

            pDrain <= pDrainNext;
            pDrainPrev <= pDrain;
            pDrainPrevPrev <= pDrainPrev;

            pStart <= pStartNext;

            pTagged <= pTaggedNext;
            pRenamed <= pRenamedNext;

            --- ctr management
            nFull <= nFullNext;
            nAlloc <= nAllocNext;

            canAlloc <= not cmpGtU(nAllocNext, QUEUE_SIZE-4);

            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;

            recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here       
        end if;
    end process;

    process (clk)
    begin
        if rising_edge(clk) then
            olderRegSQ <= olderNextSQ;
            newerRegLQ <= newerNextLQ;
                                
            adrValuePrev <= adrValue;
            adrPtrPrev <= adrPtr;

            -- ERROR! isNonzero(mask) has to take into acount whether the match is with a full entry, that is [pDrain:pTagged) for SQ, [pStart:pTagged) for LQ
            if not IS_LOAD_QUEUE then
                isSelected_Early <= compareAddressEarlyInput.full and isNonzero(olderSQ_Early);
            end if;

            isSelected <= isSelectedNext;

            if isSelectedNext = '1' then
                DB_logFW(compareAddressInput, pSelect, IS_LOAD_QUEUE);
            end if;

            pSelectEarlyPrev <= pSelectEarly;
            pSelectEarly_BasePrev <= pSelectEarly_Base;

            pSelectPrev <= pSelect;

            sqMissed <= missing;

        end if;
    end process;


	nIn <= i2slv( countOnes(inputMask), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');

	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pStartNext, pTaggedNext, QUEUE_PTR_SIZE);
    	nAllocNext <= getNumFull(pStartNext, pRenamedNext, QUEUE_PTR_SIZE);

	    nInRe <= i2slv(countOnes(renameMask), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    		
        nOut <= nCommitted when committing = '1' else (others => '0'); -- TODO: use nCommittedEffective? also UNUSED
    end generate;

    STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pDrain, pTaggedNext, QUEUE_PTR_SIZE);
    	nAllocNext <= getNumFull(pDrain, pRenamedNext, QUEUE_PTR_SIZE);

	    -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
        nInRe <= i2slv(countOnes(renameMask), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    	
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isDrainingPrev = '1' else (others => '0');		  
    end generate;

    --CAREFUL: this is only for SQ
    nCommitted <= i2slv(countOnes(commitMask), SMALL_NUMBER_SIZE);
    nCommittedEffective <= i2slv(countOnes(commitEffectiveMask), SMALL_NUMBER_SIZE);


	committedEmptySig <= bool2std(pStart = pDrainPrev);
    drainEqual <= bool2std(pStart = pDrain);
    drainReq <= not drainEqual and not committedEmptySig;

    -- Acc sigs
    acceptAlloc <= canAlloc;
    renamedPtr <= pRenamed;


    selectedDataResult <= selectedDataResultSig;
    selectedDataOutput <= selectedOutputSig;

	-- D. output (ctrl)
    committedDataOut <= committedOutputSig;
    committedEmpty <= committedEmptySig;
	committedSending <= isDrainingPrev;


    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate
        use work.MemQueueViewing.all;

        process (clk)
        begin
            if rising_edge(clk) then
                if DB_LOG_EVENTS then
                    if dbState.dbSignal = '1' then
                        report "LSQ reporting ";
                        printContent(queueContent, addresses, storeValues, pStart, pTagged, getNamePrefix(IS_LOAD_QUEUE));
                    end if;         
                end if;
            end if;
        end process;
    end generate;
    -- pragma synthesis on
    
end Behavioral;
