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


entity StoreQueue is
	generic(
		QUEUE_SIZE: integer := 8;
		IS_LOAD_QUEUE: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		acceptingOut: out std_logic;
		almostFull: out std_logic;
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

	signal adrPtr, adrPtrEarly, adrPtrPrev, pSelect, pSelectPrev, pSelectEarly, pSelectEarlyPrev,   pStart, pStartNext, pDrain, pDrainNext, pDrainPrev,
           pTagged, pTaggedNext, pRenamed, pRenamedNext, pStartEffective, pStartEffectiveNext,
	       nFull, nFullNext, nAlloc, nAllocNext, nIn, nInRe, nOut, nCommitted, nCommittedEffective, recoveryCounter: SmallNumber := (others => '0');

    signal queueContent, queueContentShifting, queueContentShiftingNext: QueueEntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_QUEUE_ENTRY);

    signal addresses, storeValues: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));

    signal canAlloc, drainReq, drainEqual, drainEffectiveEqual, fwMiss, fwMissPrev, fwMissAdr, fwMissAdrPrev, fwMissValue, fwMissValuePrev,
            allowDrain, isSending, isDrainingPrev, isSelected, isSelected_Early, sqMissed, missing, missingPrev: std_logic := '0';

    signal drainOutput, selectedOutput, selectedOutput_E, selectedOutputReg_E,  selectedOutputSig, committedOutputSig: ControlPacket := DEFAULT_CONTROL_PACKET;
    signal adrValuePrev, drainValue, selectedValue, selectedValue_E, drainAddress, selectedAddress, selectedAddress_E: Mword := (others => '0');

    signal selectedEntry, selectedEntry_E, selectedEntryReg_E,  drainEntry: QueueEntry := DEFAULT_QUEUE_ENTRY;

    signal updateResult: ExecResult := DEFAULT_EXEC_RESULT;

	alias compareAddressInputOp is compareAddressCtrl.op;
	alias compareAddressInputEarlyOp is compareAddressEarlyInput_Ctrl.op;
	alias storeValuePtr is storeValueResult.dest;
    alias pFlush is execCausing.dest;

    alias adrValueEarly is compareAddressEarlyInput.value;
    alias adrValue is compareAddressInput.value;

        signal amm_T, ammReg_T: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');


    signal ch0, ch1, ch2, ch3, chi, chii: std_logic := '0';
begin

    -- Ptr for random access updating
    adrPtr <= compareAddressCtrl.tags.lqPointer when IS_LOAD_QUEUE
         else compareAddressCtrl.tags.sqPointer;

    adrPtrEarly <= compareAddressEarlyInput_Ctrl.tags.lqPointer when IS_LOAD_QUEUE
         else compareAddressEarlyInput_Ctrl.tags.sqPointer;

    -- Read ptr determinded by address matching - SQ only
    pSelect <= addTruncZ(findNewestMatchIndex(olderSQ, sn(0), nFull, QUEUE_PTR_SIZE), pDrainPrev, QUEUE_PTR_SIZE) when false
                else pSelectEarlyPrev;
        pSelectEarly <= addTruncZ(findNewestMatchIndex(olderSQ_Early, sn(0), nFull, QUEUE_PTR_SIZE), pDrainPrev, QUEUE_PTR_SIZE);

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
            amm_T <= getAddressMatching_Low(queueContentShifting, adrValueEarly) and getAddressCompleted_Low(queueContentShifting);
            --ch0 <= bool2std(ammReg_T = addressMatchMask);
    
            olderSQ_Early <= olderNextSQ and amm_T;

        addressMatchMask <= getAddressMatching(queueContentShifting, adrValue) and getAddressCompleted(queueContentShifting);

        olderNextSQ <= cmpIndexBefore(sn(0), nFull, subTruncZ(compareAddressEarlyInput_Ctrl.tags.sqPointer, pDrainPrev, QUEUE_SIZE), QUEUE_SIZE, PTR_MASK_SN) -- TODO: nFull is not correct 
                                                                         when isLoadMemOp(compareAddressEarlyInput_Ctrl.op) = '1' else (others => '0');
        olderSQ <=   olderRegSQ and addressMatchMask;
    end generate;

           -- ch0 <= bool2std(pSelect /= pSelectEarlyPrev) and compareAddressInput.full;

    updateResult.full <= compareAddressInput.full and isLoadOp(compareAddressInputOp) when IS_LOAD_QUEUE
                    else compareAddressInput.full and isStoreOp(compareAddressInputOp);
    updateResult.dest <= adrPtr;
    updateResult.value <= adrValue;

    process (clk)
    begin
        if rising_edge(clk) then
            olderRegSQ <= olderNextSQ;
            newerRegLQ <= newerNextLQ;
                
                ammReg_T <= amm_T;
                
                adrValuePrev <= adrValue;
                adrPtrPrev <= adrPtr;

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


            -- ERROR! isNonzero(mask) has to take into acount whether the match is with a full entry, that is [pDrain:pTagged) for SQ, [pStart:pTagged) for LQ
            if not IS_LOAD_QUEUE then
                isSelected <= --compareAddressInput.full and isNonzero(olderSQ);
                                isSelected_Early;
                    isSelected_Early <= compareAddressEarlyInput.full and isNonzero(olderSQ_Early);
            else
                isSelected <= compareAddressInput.full and isNonzero(newerLQ);
            end if;

            selectedEntry <= queueContent(p2i(pSelect, QUEUE_SIZE));
            selectedValue <= storeValues(p2i(pSelect, QUEUE_SIZE));
            selectedAddress <= addresses(p2i(pSelect, QUEUE_SIZE));

            pSelectPrev <= pSelect;

                    selectedEntry_E <= queueContent(p2i(pSelectEarly, QUEUE_SIZE));
                    selectedValue_E <= storeValues(p2i(pSelectEarly, QUEUE_SIZE));
                    selectedAddress_E <= addresses(p2i(pSelectEarly, QUEUE_SIZE));
        
                    pSelectEarlyPrev <= pSelectEarly;

                    selectedOutputReg_E <= selectedOutput_E;
                      -- selectedEntryReg_E <=  selectedEntry_E;
                    

                
                fwMissPrev <= fwMiss;
                fwMissAdrPrev <= fwMissAdr;
                fwMissValuePrev <= fwMissValue;

                    missingPrev <= missing;

            -- D. outputs
            drainEntry <= queueContent(p2i(pDrain, QUEUE_SIZE));
            drainValue <= storeValues(p2i(pDrain, QUEUE_SIZE));
            drainAddress <= addresses(p2i(pDrain, QUEUE_SIZE));
        end if;
    end process;

    --    ch0 <= bool2std(missingPrev = sqMissed);

    CHECK_FW: block
        signal fw, adrReady, valueReady, adrMatch, adrMatchLast, tagMatchingLast, valueMissRef, dataReady,
                adrMatchNormal, adrMatchRecent: std_logic := '0';
    begin
        fw <= isSelected_Early;
        valueReady <= selectedEntry_E.completedV;
        adrReady <= --selectedEntry_E.completedA;
                        queueContent(p2i(pSelect, QUEUE_SIZE)).completedA;
        adrMatch <= addressHighMatching(--selectedAddress_E, adrValue);
                                        addresses(p2i(pSelect, QUEUE_SIZE)), adrValue);
        adrMatchLast <= addressHighMatching(adrValuePrev, adrValue);
        tagMatchingLast <= bool2std((pSelectEarlyPrev and PTR_MASK_SN) = (adrPtrPrev and PTR_MASK_SN));

        fwMiss <= fw and ((not (adrReady and adrMatch) and not (adrMatchLast and tagMatchingLast)) or not valueReady);
        fwMissAdr <= fw and ((not (adrReady and adrMatch) and not (adrMatchLast and tagMatchingLast)));
        fwMissValue <= fw and not valueReady;
        
        valueMissRef <= isSelected and not selectedEntry.completedV;
        
        
            adrMatchNormal <= adrReady and adrMatch;
            adrMatchRecent <= adrMatchLast and tagMatchingLast;
        
            dataReady <= queueContent(p2i(pSelect, QUEUE_SIZE)).completedV;
        
        missing <= fw and (not (adrMatchNormal or adrMatchRecent) or not dataReady);


        sqMissed <= --fwMissAdrPrev or valueMissRef;
                    missingPrev;
    end block;

    selectedOutput <= getDrainOutput(selectedEntry, selectedAddress, selectedValue);    
        selectedOutput_E <= getDrainOutput(selectedEntry_E, selectedAddress_E, selectedValue_E);    

    drainOutput <= getDrainOutput(drainEntry, drainAddress, drainValue);   



    pDrainNext <= addIntTrunc(pDrain, 1, QUEUE_PTR_SIZE+1) when drainReq = '1' else pDrain;
    pStartEffectiveNext <= addTruncZ(pStartEffective, nCommittedEffective, QUEUE_PTR_SIZE+1) when committing = '1'
                      --else pStartNext when nowCancelled = '1' and drainEqual = '1'
                      else pStartEffective;
    pStartNext <= --addTruncZ(pStart, nCommitted, QUEUE_PTR_SIZE+1) when committing = '1' else pStart;
                    pStartEffectiveNext;

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
            allowDrain <= --not (nowCancelled or (not drainEqual and drainEffectiveEqual));
                            '1';
            -- TODO: reanalyze nowCancelled?
            -- D. out ctrl
--            if drainEqual = '1' then
--            --    nowCancelled <= '0';
--            elsif drainEffectiveEqual = '1' then
--            --    nowCancelled <= '1';
--            end if;

            pDrain <= pDrainNext;
            pDrainPrev <= pDrain;

            pStart <= pStartNext;
            pStartEffective <= pStartEffectiveNext;

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

	nIn <= i2slv( countOnes(inputMask), SMALL_NUMBER_SIZE ) when prevSending = '1' else (others => '0');

	LOAD_QUEUE_MANAGEMENT: if IS_LOAD_QUEUE generate
    	nFullNext <= getNumFull(pStartNext, pTaggedNext, QUEUE_PTR_SIZE);
    	nAllocNext <= getNumFull(pStartNext, pRenamedNext, QUEUE_PTR_SIZE);

	    nInRe <= i2slv(countOnes(renameMask), SMALL_NUMBER_SIZE) when prevSendingRe = '1' else (others => '0');    		
        nOut <= nCommitted when committing = '1' else (others => '0');
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

    drainEffectiveEqual <= bool2std(pStartEffective = pDrain);
    drainEqual <= bool2std(pStart = pDrain);
    drainReq <= not drainEqual;


    -- Acc sigs
    acceptAlloc <= canAlloc;

    renamedPtr <= pRenamed;

    -- E. output
    selectedDataOutput <= selectedOutputSig;

    selectedDataResult.full <= '0';
    selectedDataResult.failed <= '0';
    selectedDataResult.dest <= pSelectPrev;
    selectedDataResult.tag <= (others => '0');
    selectedDataResult.value <= (others => '0');

    WHEN_LQ: if IS_LOAD_QUEUE generate
        selectedOutputSig.controlInfo.full <= isSelected;           
    end generate;

    WHEN_SQ: if not IS_LOAD_QUEUE generate
        selectedOutputSig.controlInfo.full <= isSelected;
        selectedOutputSig.controlInfo.newEvent <= selectedOutput.controlInfo.newEvent;
        selectedOutputSig.controlInfo.firstBr <= selectedOutput.controlInfo.firstBr;
        selectedOutputSig.controlInfo.sqMiss <= --selectedOutput.controlInfo.sqMiss;
                                                    sqMissed;
        
        selectedOutputSig.op <= selectedOutput.op;
        selectedOutputSig.target <= selectedOutput.target;
        selectedOutputSig.nip <= selectedOutput.nip;

        committedOutputSig.controlInfo.full <= isDrainingPrev and allowDrain;
        
        committedOutputSig.op <= drainOutput.op;
        committedOutputSig.target <= drainOutput.target;
        committedOutputSig.nip <= drainOutput.nip;
    end generate;

	-- D. output (ctrl)
    committedDataOut <= committedOutputSig;
	committedEmpty <= bool2std(pStart = pDrainPrev);
	committedSending <= isDrainingPrev;


    -- pragma synthesis off
    
    DEBUG_HANDLING: if DB_ENABLE generate
        process (clk)
            use std.textio.all;
            use work.Assembler.all;
    
            function getNamePrefix return string is
            begin
                if IS_LOAD_QUEUE then
                    return "l";
                else
                    return "s";
                end if;
            end function;
    
            constant NAME_PREFIX: string(1 to 1) := getNamePrefix;
    
            function getDynamicContentString(elem: QueueEntry; adr: Mword; value: Mword) return string is
                variable res: line;
            begin
                if true then --elem.full = '1' then
                    write(res, string'(": "));
                    write(res, std_logic'image(elem.completedA));
                    write(res, std_logic'image(elem.completedV));
                    write(res, string'(" @"));
                    
                    if elem.completedA = '1' then
                        write(res, natural'image(slv2u(adr)));
                    else
                        write(res, string'("????????"));
                    end if;
                    write(res, string'(": "));
                    
                    if elem.completedV = '1' then
                        write(res, natural'image(slv2u(value)));
                    else
                        write(res, string'("????????"));
                    end if;                
                    return res.all;
                else
                    return "-------------------------------------";
                end if;
            end function;
        
            procedure printContent is
               file outFile: text open write_mode is NAME_PREFIX & "q_content.txt";
               variable preRow, currentLine: line := null;
            begin
                for i in 0 to ROB_SIZE-1 loop
                    if p2i(pStart, QUEUE_SIZE) = i then
                        preRow := "start ";
                    elsif p2i(pTagged, QUEUE_SIZE) = i then
                        preRow := "end   ";
                    else
                        preRow := "      ";
                    end if;
                    
                    currentLine := null;
                    write(currentLine, preRow.all & natural'image(i) & "  ");
                    write(currentLine, getDynamicContentString(queueContent(i), addresses(i), storeValues(i)) & ",   ");
    
                    writeline(outFile, currentLine);
                end loop;
            end procedure;
            
        begin
            if rising_edge(clk) then
                if DB_LOG_EVENTS then
                    if dbState.dbSignal = '1' then
                        report "LSQ reporting ";
                        printContent;
                    end if;         
                end if;
            end if;
        end process;
    end generate;
    -- pragma synthesis on
    
end Behavioral;
