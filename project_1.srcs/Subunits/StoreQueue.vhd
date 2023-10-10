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
            compareAddressEarlyEP: in ExecPacket;

		compareAddressInput: in ExecResult;
		compareAddressCtrl: in ControlPacket;
            compareAddressEP: in ExecPacket;

		storeValueResult: in ExecResult;
            storeValueEP: in ExecPacket;

        selectedDataOutput: out ControlPacket;
        selectedDataResult: out ExecResult;

		committing: in std_logic;
        commitEffectiveMask: in std_logic_vector(0 to PIPE_WIDTH-1);

		events: in EventState;

		nextAccepting: in std_logic;		

		committedEmpty: out std_logic;
		committedDataOut: out ControlPacket;

		dbState: in DbCoreState
	);
end StoreQueue;


architecture Behavioral of StoreQueue is
    alias lateEventSignal is events.lateCausing.full;
    alias execEventSignal is events.execCausing.full;

	constant PTR_MASK_SN: SmallNumber := sn(QUEUE_SIZE-1);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);

	signal addressMatchMask, adrMatchesLowNS, adrMatchesLowSh, validMaskNS, validMaskSh, amvNS, amvSh, amvOlderNS, amvOlderSh,
	           newerLQ, newerRegLQ, newerNextLQ, olderNextSQ_Early, olderSQ_Early: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal adrPtr, adrPtrEarly, adrPtrPrev, pSelect, pFlush,
	       pSelectPrev, pSelectEarly_Base, pSelectEarly_BasePrev, pSelectEarly, pSelectEarlyNS, pSelectEarlyPrevNS,
	       pStart, pStartNext, pDrain, pDrainNext, pDrainPrev, pDrainPrevPrev, pTagged, pTaggedNext, pRenamed, pRenamedNext, 
           nFull, nFullNext, nAlloc, nAllocNext: SmallNumber := sn(0);

    signal queueContent_NS, queueContentShifting: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);
    signal addresses, addressesLow, storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));

    signal canAlloc, drainReq, drainEqual, isSending, isDrainingPrev, isSelected, isSelectedNext, isSelected_Early, sqMissed, missing, committedEmptySig,
            notU, -- Careful: needed to prevent false drainReq on init
            dummy0: std_logic := '0';

    signal selectedOutputSig, committedOutputSig: ControlPacket := DEFAULT_CONTROL_PACKET;
    signal adrValuePrev: Mword := (others => '0');

    signal selectedDataResultSig: ExecResult := DEFAULT_EXEC_RESULT;

    alias adrValueEarly is compareAddressEarlyInput.value;
    alias adrValue is compareAddressInput.value;

    signal updateAdr, updateEarlyAdr, updateCompletedA, updateCompletedSysA, updateEarlyCompletedA, lookupEarly: std_logic := '0';

    constant USE_SHIFTING: boolean := false;

    signal ch0, ch1, ch2, ch3, chi, chii: std_logic := '0';
begin
    pFlush <= events.execTags.lqPointer when IS_LOAD_QUEUE
         else events.execTags.sqPointer;

    lookupEarly <= isStoreMemOp(compareAddressEarlyInput_Ctrl.op) when IS_LOAD_QUEUE
              else isLoadMemOp(compareAddressEarlyInput_Ctrl.op);
    updateEarlyCompletedA <= isLoadMemOp(compareAddressEarlyInput_Ctrl.op) when IS_LOAD_QUEUE
                   else isStoreMemOp(compareAddressEarlyInput_Ctrl.op);
    updateCompletedA <= isLoadMemOp(compareAddressCtrl.op) when IS_LOAD_QUEUE
                   else isStoreMemOp(compareAddressCtrl.op);
    updateCompletedSysA <= isLoadSysOp(compareAddressCtrl.op) when IS_LOAD_QUEUE
                      else isStoreSysOp(compareAddressCtrl.op);
    updateEarlyAdr <= isLoadOp(compareAddressEarlyInput_Ctrl.op) when IS_LOAD_QUEUE
                 else isStoreOp(compareAddressEarlyInput_Ctrl.op);
    updateAdr <= isLoadOp(compareAddressCtrl.op) when IS_LOAD_QUEUE
            else isStoreOp(compareAddressCtrl.op);


    adrPtrEarly <= compareAddressEarlyInput_Ctrl.tags.lqPointer when IS_LOAD_QUEUE
              else compareAddressEarlyInput_Ctrl.tags.sqPointer;
    -- Ptr for random access updating
    adrPtr <= compareAddressCtrl.tags.lqPointer when IS_LOAD_QUEUE
         else compareAddressCtrl.tags.sqPointer;


    pSelectEarly_Base <= findNewestMatchIndex(olderSQ_Early, sn(0), nFull, QUEUE_PTR_SIZE);
    pSelectEarly <= addTruncZ(pSelectEarly_Base, pDrainPrev, QUEUE_PTR_SIZE);

    pSelectEarlyNS <= findNewestMatchIndex_2(amvOlderNS, pDrainPrev, pTagged, QUEUE_PTR_SIZE);

    -- Read ptr determinded by address matching - SQ only
    pSelect <= addTruncZ(pSelectEarly_BasePrev, pDrainPrevPrev, QUEUE_PTR_SIZE) when USE_SHIFTING
          else pSelectEarlyPrevNS;
    
    addressMatchMask <= getAddressMatching(queueContent_NS, adrValue) and getAddressCompleted(queueContent_NS);

    adrMatchesLowSh <= getAddressMatching_Low(queueContentShifting, adrValueEarly) and getAddressCompleted_Low(queueContentShifting);
    adrMatchesLowNS <= getAddressMatching_Low(queueContent_NS, adrValueEarly) and getAddressCompleted_Low(queueContent_NS);

    validMaskSh <= cmpIndexBefore(sn(0), nFull, nFull, QUEUE_SIZE);
    validMaskNS <= cmpIndexBefore(pDrainPrev, pTagged, pTagged, QUEUE_SIZE);

    amvSh <= adrMatchesLowSh and validMaskSh;
    amvNS <= adrMatchesLowNS and validMaskNS;

    amvOlderSh <= amvSh and olderNextSQ_Early; 
    amvOlderNS <= amvNS and cmpIndexBefore(pDrainPrev, pTagged, adrPtrEarly, QUEUE_SIZE)-- when lookupEarly = '1' else (others => '0');
                                                                                         ;
    -- LQ only
    LQ_MATCH: if IS_LOAD_QUEUE generate
    begin
        newerNextLQ <= cmpIndexAfter(pStart, pTagged, adrPtrEarly, QUEUE_SIZE) when lookupEarly = '1' else (others => '0');
        newerLQ <=     newerRegLQ and addressMatchMask;
    end generate;

    -- SQ only
    SQ_MATCH: if not IS_LOAD_QUEUE generate
    begin
        olderNextSQ_Early <= cmpIndexBefore(sn(0), nFull, subTruncZ(adrPtrEarly, pDrainPrev, QUEUE_PTR_SIZE), QUEUE_SIZE) when lookupEarly = '1' else (others => '0');
        olderSQ_Early <= olderNextSQ_Early and adrMatchesLowSh;
    end generate;

    CHECK_FW: block
        signal adrReady, adrMatch, dataReady, adrMatchNormal: std_logic := '0';
    begin
        adrMatch <= addressHighMatching(addresses(p2i(pSelect, QUEUE_SIZE)), adrValue);
        adrReady <= queueContent_NS(p2i(pSelect, QUEUE_SIZE)).completedA;
        dataReady <= queueContent_NS(p2i(pSelect, QUEUE_SIZE)).completedV;

        adrMatchNormal <= adrReady and adrMatch and dataReady;

        missing <= isSelected_Early and not adrMatchNormal;
    end block;

    process (clk)
    begin
        if rising_edge(clk) then
            newerRegLQ <= newerNextLQ;

            adrValuePrev <= adrValue;
            adrPtrPrev <= adrPtr;

            -- ERROR! isNonzero(mask) has to take into acount whether the match is with a full entry, that is [pDrain:pTagged) for SQ, [pStart:pTagged) for LQ
            if not IS_LOAD_QUEUE then
                if USE_SHIFTING then
                    isSelected_Early <= compareAddressEarlyInput.full and isNonzero(olderSQ_Early);
                else
                    isSelected_Early <= compareAddressEarlyInput.full and isNonzero(amvOlderNS) --;
                                                                                                and lookupEarly;
                end if;
            end if;

            pSelectEarly_BasePrev <= pSelectEarly_Base;
            pSelectPrev <= pSelect; -- Only SQ?

            pSelectEarlyPrevNS <= pSelectEarlyNS;

            isSelected <= isSelectedNext;
            sqMissed <= missing;

            if isSelectedNext = '1' then
                DB_logFW(compareAddressInput, pSelect, IS_LOAD_QUEUE);
            end if;
        end if;
    end process;


    isSelectedNext <= isSelected_Early when not IS_LOAD_QUEUE
                 else compareAddressInput.full and isNonzero(newerLQ);

    QUEUE_DATA: block
        signal drainValue, selectedValue, drainAddress, selectedAddress: Mword := (others => '0');
        signal selectedEntry, drainEntry: QueueEntry := DEFAULT_QUEUE_ENTRY;

        signal qaPtr, qaEarlyPtr, oneIfDraining: SmallNumber := sn(0);
    begin
        oneIfDraining <= sn(1) when isDrainingPrev = '1' else sn(0);

        qaPtr <= subTruncZ(compareAddressCtrl.tags.sqPointer, pDrain, QUEUE_PTR_SIZE);
        qaEarlyPtr <= subTruncZ(compareAddressEarlyInput_Ctrl.tags.sqPointer, pDrain, QUEUE_PTR_SIZE);

        QUEUE_DATA_SYNC: process (clk)
        begin

            if rising_edge(clk) then
                -- SQ only
                if isDrainingPrev = '1' then -- Move forward     
                    shiftQueueContent_Drain(queueContentShifting);
                end if;

                -- Front input
                if prevSending = '1' then
                    updateOnInput(queueContent_NS, pTagged, inputMask, systemMask);
                    
                        updateOnInput(queueContentShifting, subTruncZ(nFull, oneIfDraining, QUEUE_PTR_SIZE), inputMask, systemMask); -- !!! 
                end if;

                -- Update early
                if compareAddressEarlyInput.full = '1' then
                    if updateEarlyCompletedA = '1' then
                        queueContentShifting(slv2u(qaEarlyPtr)).completedLowA <= '1';
                        queueContent_NS(p2i(adrPtrEarly, QUEUE_SIZE)).completedLowA <= '1';
                    end if;
                    
                    if updateEarlyAdr = '1' then
                        queueContentShifting(slv2u(qaEarlyPtr)).addressLow <= adrValueEarly;
                        queueContent_NS(p2i(adrPtrEarly, QUEUE_SIZE)).addressLow(11 downto 0) <= adrValueEarly(11 downto 0);

                        addressesLow(p2i(adrPtrEarly, QUEUE_SIZE))(11 downto 0) <= adrValueEarly(11 downto 0);
                    end if;
                end if;

                -- Update
                if compareAddressInput.full = '1' then 
                    if updateCompletedA = '1' then
                        queueContentShifting(slv2u(qaPtr)).completedA <= '1';
                        queueContent_NS(p2i(adrPtr, QUEUE_SIZE)).completedA <= '1';
                    end if;

                    if updateAdr = '1' then
                        queueContentShifting(slv2u(qaPtr)).address <= adrValue;
                        queueContent_NS(p2i(adrPtr, QUEUE_SIZE)).address <= adrValue;

                        addresses(p2i(adrPtr, QUEUE_SIZE))(31 downto 12) <= adrValue(31 downto 12);
                        addresses(p2i(adrPtr, QUEUE_SIZE))(11 downto 0) <= adrValue(11 downto 0);
                    end if;
                end if;

                -- E. val update
                if storeValueResult.full = '1' and not IS_LOAD_QUEUE then
                    queueContent_NS(p2i(storeValueResult.dest, QUEUE_SIZE)).completedV <= '1';
                    
                    storeValues(p2i(storeValueResult.dest, QUEUE_SIZE)) <= storeValueResult.value;
                end if;

                if (execEventSignal or lateEventSignal) = '1' then
                    shiftQueueContent_Evt(queueContentShifting, nFullNext);
                end if;


                selectedEntry <= queueContent_NS(p2i(pSelect, QUEUE_SIZE));
                selectedValue <= storeValues(p2i(pSelect, QUEUE_SIZE));
                selectedAddress <= addresses(p2i(pSelect, QUEUE_SIZE));

                -- D. outputs
                drainEntry <= queueContent_NS(p2i(pDrain, QUEUE_SIZE));
                drainValue <= storeValues(p2i(pDrain, QUEUE_SIZE));
                drainAddress <= addresses(p2i(pDrain, QUEUE_SIZE));
            end if;
        end process;

        selectedDataResultSig <= DEFAULT_EXEC_RESULT when IS_LOAD_QUEUE 
                        else selDataRes(pSelectPrev);
        selectedOutputSig <= makeSelectedOutputLQ(isSelected) when IS_LOAD_QUEUE -- In this case only 1 bit signal that a hit happens (mem order violation)
                        else makeSelectedOutputSQ(getDrainOutput(selectedEntry, selectedAddress, selectedValue), isSelected, sqMissed);
        committedOutputSig <= makeCommittedOutputSQ(getDrainOutput(drainEntry, drainAddress, drainValue), isDrainingPrev);
    end block;


    pDrainNext <= addIntTrunc(pDrain, 1, QUEUE_PTR_SIZE+1) when drainReq = '1' else pDrain;
    pStartNext <= addTruncZ(pStart, countSN(commitEffectiveMask), QUEUE_PTR_SIZE+1) when committing = '1'
             else pStart;

    pTaggedNext <= pStart when lateEventSignal = '1'
            else   pFlush when execEventSignal = '1'
            else   addTruncZ(pTagged, countSN(inputMask), QUEUE_PTR_SIZE+1) when prevSending = '1'
            else   pTagged;

    pRenamedNext <= pStart when lateEventSignal = '1'
            else    pFlush when execEventSignal = '1'
            else    addTruncZ(pRenamed, countSN(renameMask), QUEUE_PTR_SIZE+1) when prevSendingRe = '1'
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
        end if;
    end process;

	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pStartNext, pTaggedNext, QUEUE_PTR_SIZE);
    	nAllocNext <= getNumFull(pStartNext, pRenamedNext, QUEUE_PTR_SIZE);
    end generate;

    STORE_QUEUE_MANAGEMENT: if not IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pDrain, pTaggedNext, QUEUE_PTR_SIZE);
    	nAllocNext <= getNumFull(pDrain, pRenamedNext, QUEUE_PTR_SIZE);
	    -- CAREFUL: starting from pDrainPrev because its target+result is in output register, not yet written to cache
    end generate;

    ------------
    -- SQ
        notU <= '1'; -- Careful: needed to prevent false drainReq on init
    
	committedEmptySig <= bool2std(pStart = pDrainPrev);
    drainEqual <= bool2std(pStart = pDrain);
    drainReq <= not drainEqual and not committedEmptySig  and notU;
    -------------


    selectedDataResult <= selectedDataResultSig;
    selectedDataOutput <= selectedOutputSig;

	-- D. output (ctrl)
    committedDataOut <= committedOutputSig;
    committedEmpty <= committedEmptySig;

    -- Acc sigs
    acceptAlloc <= canAlloc;
    renamedPtr <= pRenamed;

    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate
        use work.MemQueueViewing.all;

        signal states: EntryStateArray(0 to QUEUE_SIZE-1) := (others => empty);

        signal drainReqPre: std_logic := '0';
    begin

        process (clk)
        begin
            if rising_edge(clk) then
                drainReqPre <= drainReq;
            
                if prevSending = '1' then
                    DB_writeStates(states, inputMask, pTagged, IS_LOAD_QUEUE);
                end if;
            
                if committing = '1' then
                    DB_commitStates(states, commitEffectiveMask, pStart, IS_LOAD_QUEUE);
                end if;
            
                if drainReqPre = '1' and not IS_LOAD_QUEUE then
                    DB_drainStates(states, pDrainPrev);
                end if;

                if ((compareAddressInput.full and not compareAddressEP.killed) or storeValueResult.full) = '1' then
                   DB_updateStates(states, (updateCompletedA or updateCompletedSysA) and compareAddressInput.full, storeValueResult.full, compareAddressCtrl, storeValueResult, IS_LOAD_QUEUE);
                end if;

                DB_eventStates(states, events, pStart, pDrainPrev, IS_LOAD_QUEUE);
            
                if DB_LOG_EVENTS then
                    if dbState.dbSignal = '1' then
                        report "LSQ reporting ";
                        printContent(queueContent_NS, addresses, storeValues, pStart, pTagged, getNamePrefix(IS_LOAD_QUEUE));
                    end if;
                end if;
            end if;
        end process;
    end generate;
    -- pragma synthesis on

end Behavioral;
