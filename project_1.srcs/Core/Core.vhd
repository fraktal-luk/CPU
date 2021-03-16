----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.Arith.all;

entity Core is
    generic(
        DEBUG_FILE_PREFIX: string := "CoreDB_"
    );
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           en : in  STD_LOGIC;
			  
		   -- address fot program mem
           iadrvalid: out std_logic;
		   iadr : out  Mword;
		   -- instruction input
		   ivalid: in std_logic;
           iin : in  WordArray(0 to PIPE_WIDTH-1);
			  
		   -- Mem load interface
		   dread: out std_logic;
           dadr : out  Mword;
		   dvalid: in std_logic;			  
           din : in  Mword;
			  
		   -- Mem store interface
		   dwrite: out std_logic;
		   doutadr: out Mword;
           dout : out  Mword;
			  
		   intallow: out std_logic;
		   intack: out std_logic;
		   -- Interrupt input (int0) and additional input (int1)
           int0 : in  STD_LOGIC;
           int1 : in  STD_LOGIC;
			  
		   filladr: in Mword;
		   fillready: in std_logic;
			  
		   -- Other buses for development 
           iaux : in  Mword;
           oaux : out  Mword			  
			  
		);
end Core;


architecture Behavioral of Core is
    signal pcDataSig, frontCausing, execCausing, lateCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;
    signal pcSending, frontAccepting, bpAccepting, bpSending, renameAccepting, frontLastSending,
                frontEventSignal, bqAccepting, bqSending, acceptingSQ, almostFullSQ, acceptingLQ, almostFullLQ, dbEmpty,
                canSendFront, canSendRename, canSendBuff: std_logic := '0';

            signal ch0, ch1, ch2, ch3, ch4: std_logic := '0';
    signal frontDataLastLiving, TMP_frontDataSpMasked,
            renamedDataLivingFloatPre, renamedDataMerged, renamedDataLivingMem, renamedDataLivingRe, renamedDataLivingFloatRe,
            renamedDataLivingReMem, renamedDataLivingMemBuff, renamedDataLivingBuff, dispatchBufferDataInt,-- dispatchBufferDataFloat,
            dataOutROB, renamedDataToBQ, renamedDataToSQ, renamedDataToLQ, bqData, bpData: 
                InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal bqCompare, bqSelected, bqUpdate, sqValueInput, sqAddressInput, sqSelectedOutput, lqAddressInput, lqSelectedOutput: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    
        signal bqPointer, lqPointer, sqPointer, preIndexSQ, preIndexLQ: SmallNumber := (others => '0');
    
    signal execOutputs1, execOutputs2: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);    

    signal execEventSignal, lateEventSignal, lateEventSetPC, sendingBranchIns: std_logic := '0';
    signal robSending, robAccepting, renamedSending, commitAccepting, oooAccepting, lsbrAccepting, lsbrAcceptingMore, renamedSendingBuff,
                issueQueuesAccepting, issueQueuesAcceptingMore,
                renameSendingBr, stopRename,
                queuesAccepting, queuesAcceptingMore, iqAcceptingI0, iqAcceptingM0, iqAcceptingF0, iqAcceptingS0, iqAcceptingSF0, dispatchAccepting,
                robAcceptingMore, iqAcceptingMoreI0, iqAcceptingMoreM0, iqAcceptingMoreF0, iqAcceptingMoreS0, iqAcceptingMoreSF0: std_logic := '0';
    signal commitGroupCtr, commitGroupCtrInc: InsTag := (others => '0');
    signal newIntDests, newFloatDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal intSignal: std_logic := '0';
    signal intType: std_logic_vector(0 to 1) := (others => '0');
    signal sysRegReadValue: Mword := (others => '0');
    signal sysRegReadSel: slv5 := (others => '0');
    signal preAguTag: InsTag := (others => '0');
    
    signal sbSending, sbEmpty, sysRegRead, sysRegSending: std_logic := '0';
    signal dataFromSB: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);
    
    signal specialAction, specialActionBuffOut, specialOutROB: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;

    signal committedOut: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal committedSending: std_logic := '0';
    
    signal lastEffectiveOut, bqTargetData: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    
    signal cycleCounter: Word := (others => '0');
    
    function mergeFP(dataInt: InstructionSlotArray; dataFloat: InstructionSlotArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := dataInt;
    begin
        for i in res'range loop
            res(i).full := dataFloat(i).full;
            res(i).ins.physicalArgSpec.args := dataFloat(i).ins.physicalArgSpec.args;
        end loop;
        return res;
    end function;    
begin

    MONITOR: process (clk)
    begin
        if rising_edge(clk) then
            cycleCounter <= addInt(cycleCounter, 1);
        end if;
    end process;

    intSignal <= int0 or int1;
    intType <= (int0, int1);

	UNIT_SEQUENCER: entity work.UnitSequencer(Behavioral)
	generic map(DEBUG_FILE_PREFIX => DEBUG_FILE_PREFIX)
    port map (
        clk => clk, reset => reset, en => '0',
        
        -- sys reg interface
        sysRegReadSel => sysRegReadSel,
        sysRegReadValue => sysRegReadValue,

        -- to front pipe
        frontAccepting => frontAccepting,
        pcDataLiving => pcDataSig,
        pcSending => pcSending,
        
        intAllowOut => intallow,
        intAckOut => intack,
        intRejOut => open,--
        -- Events in
        intSignal => intSignal,
        intType => intType,
        execEventSignal => execEventSignal,
        execCausing => execCausing,
        
        frontEventSignal => frontEventSignal,
        frontCausing => frontCausing,
        
        -- Events out
        lateEventOut => lateEventSignal,
        lateEventSetPC => lateEventSetPC,
        lateCausing => lateCausing,
        -- Data from front pipe interface        
        frontLastSending => frontLastSending,
        frontDataLastLiving => frontDataLastLiving,

        -- Interface from ROB
        commitAccepting => commitAccepting,
        sendingFromROB => robSending,    
        robDataLiving => dataOutROB,
        robSpecial => specialOutROB,
        ---
        dataFromBQV => bqData,
        bqTargetData => bqTargetData,

        sbSending => sbSending,
        dataFromSB => dataFromSB(0),
        sbEmpty => sbEmpty,

        commitGroupCtrOut => commitGroupCtr,
        commitGroupCtrIncOut => commitGroupCtrInc,
        
        committedOut => committedOut,
        committedSending => committedSending,
        
        lastEffectiveOut => lastEffectiveOut,
        
        doneSig => oaux(0),
        failSig => oaux(1)
    );
        
        
    iadr <= pcDataSig.ip;
    iadrvalid <= pcSending;
       
	UNIT_FRONT: entity work.UnitFront(Behavioral)
    port map(
        clk => clk, reset => '0', en => '0',
        
        iin => iin,
                    
        pcDataLiving => pcDataSig,
        pcSending => pcSending,    
        frontAccepting => frontAccepting,
    
        bpAccepting => bqAccepting,
        bpSending => bpSending,
        bpData => bpData,
    
        renameAccepting => canSendFront,
        dataLastLiving => frontDataLastLiving,
        lastSending => frontLastSending,
        
        frontEventSignal => frontEventSignal,
        frontCausing => frontCausing,
        
        execCausing => execCausing,
        lateCausing => lateCausing,
        
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal,
        lateEventSetPC => lateEventSetPC
    );    

            
    REGISTER_MANAGER: entity work.UnitRegManager(Behavioral)
    port map(
        clk => clk,
        renameAccepting => renameAccepting, -- to frontend
        frontLastSending => frontLastSending,
        frontDataLastLiving => frontDataLastLiving,
        
            TMP_spMaskedDataOut => TMP_frontDataSpMasked,
        
        nextAccepting => canSendRename,
        
        renamedDataLiving => renamedDataLivingRe,
        renamedDataLivingFloat => renamedDataLivingFloatPre,        
        renamedSending => renamedSending,

            renamingBr => renameSendingBr,

            bqPointer => bqPointer,
            sqPointer => sqPointer,
            lqPointer => lqPointer,

        robDataLiving => dataOutROB,
        sendingFromROB => robSending,
        
        newPhysDestsOut => newIntDests,
        newFloatDestsOut => newFloatDests,
            
        specialActionOut => specialAction,
            
        commitGroupCtr => commitGroupCtr,
		
		execCausing => execCausing,
        lateCausing => lateCausing,
        
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal
    );

    -- CAREFUL: this must block renaming of a group if there will be no place for it in IQs the next cycle;
    --          Because stalling at Rename is illegal, sending to Rename has to depend on free slots 
    --          after accounting for current group at Rename that will use some resources!  

    oooAccepting <= queuesAcceptingMore and renameAccepting;
    lsbrAccepting <= robAccepting and acceptingSQ and acceptingLQ;
    lsbrAcceptingMore <= robAcceptingMore and not almostFullSQ and not almostFullLQ;
    
        issueQueuesAccepting <= iqAcceptingI0 and iqAcceptingM0 and iqAcceptingS0 and iqAcceptingF0 and iqAcceptingSF0;
        issueQueuesAcceptingMore <= iqAcceptingMoreI0 and iqAcceptingMoreM0 and iqAcceptingMoreS0 and iqAcceptingMoreF0 and iqAcceptingMoreSF0;
    
    queuesAccepting <= lsbrAccepting and issueQueuesAccepting;
    queuesAcceptingMore <= lsbrAcceptingMore and issueQueuesAcceptingMore;

    -- From Rename we send to OOO if it accepts and DB is empty. If DB is not empty, we have to drain it first!
    renamedDataLivingBuff <= dispatchBufferDataInt;

    renamedDataLivingFloatRe <= mergeFP(renamedDataLivingRe, renamedDataLivingFloatPre);

    renamedDataMerged <= renamedDataLivingBuff;
    
    renamedDataLivingReMem <= TMP_recodeMem(renamedDataLivingRe);
    renamedDataLivingMemBuff <= TMP_recodeMem(renamedDataLivingBuff);

    DISPATCH_BUFFER: entity work.DispatchBuffer
    port map(
        clk => clk,
        
        specialAction => specialAction,
        nextAccepting => canSendBuff,
                                 
        accepting => dispatchAccepting,
        prevSending => renamedSending,
        dataIn => renamedDataLivingRe,            
        sending => renamedSendingBuff,
        dataOut => dispatchBufferDataInt,
        specialOut => specialActionBuffOut,
        
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal,        
        empty => dbEmpty            
    );


	REORDER_BUFFER: entity work.ReorderBuffer(Behavioral)
	port map(
		clk => clk, reset => '0', en => '0',
		
		lateEventSignal => lateEventSignal,
		
		commitGroupCtr => commitGroupCtr,

		execEndSigs1 => execOutputs1,
		execEndSigs2 => execOutputs2,
		
		inputSpecial => specialActionBuffOut,
		
		inputData => renamedDataMerged,
		prevSending => renamedSendingBuff,
		acceptingOut => robAccepting,
		acceptingMore => robAcceptingMore,
		
		nextAccepting => commitAccepting,
		sendingOut => robSending, 
		outputData => dataOutROB,
		outputSpecial => specialOutROB		
	);
   
         OLD_FLOW: if true generate
            canSendFront <= oooAccepting;
            canSendRename <= '1';
            canSendBuff <= queuesAccepting;
         end generate;
         
          NEW_FLOW: if false generate
             canSendFront <= renameAccepting and not stopRename;
             canSendRename <= dispatchAccepting;
             canSendBuff <= not stopRename;
          end generate;         
         
    STOP_RENAME: process (clk)
    begin
        if rising_edge(clk) then
            if queuesAcceptingMore = '0' then
                stopRename <= '1';
            elsif queuesAcceptingMore = '1' then
                stopRename <= '0';
            end if;
        end if;
    end process;
    

    MAIN_VIEW: if VIEW_ON generate
        use work.Viewing.all;
        
        signal frontEv, execEv, lateEv: InstructionSlot := DEFAULT_INS_SLOT;
        signal frontEvStr, execEvStr, lateEvStr: InsString := (others => ' ');
        signal renamedText, committedText: InsStringArray(0 to PIPE_WIDTH-1) := (others => (others => ' '));
        signal lastEffectiveText: InsString := (others => ' ');
        
        signal renamedIntTextRe, renamedFloatTextRe, renamedMergedText, renamedTextBQ, renamedTextLQ, renamedTextSQ: GenericStageView;   
        signal robOutText: GenericStageView;          
    begin
        frontEv <= (frontEventSignal, frontCausing);
        execEv  <= (execEventSignal,  execCausing);
        lateEv  <= (lateEventSignal,  lateCausing);

        renamedMergedText <= getInsStringArray(renamedDataMerged);
        
        renamedText <= getInsStringArray(renamedDataMerged, physDisasm);
        committedText <= getInsStringArray(committedOut, physDisasm);
        lastEffectiveText <= getInsString(lastEffectiveOut, physDisasm);
        
        renamedTextBQ <= getInsStringArray(renamedDataToBQ);
        renamedTextLQ <= getInsStringArray(renamedDataToLQ);
        renamedTextSQ <= getInsStringArray(renamedDataToSQ);
        
        
        renamedIntTextRe <= getInsStringArray(renamedDataLivingRe);
        renamedFloatTextRe <= getInsStringArray(renamedDataLivingFloatRe);
        
        robOutText <= getInsStringArray(dataOutROB);
        
        frontEvStr <= getInsString(frontEv, control);
        execEvStr <= getInsString(execEv, control);
        lateEvStr <= getInsString(lateEv, control);
                       
        process (clk)
        begin
           if rising_edge(clk) then 
--                if cmpGtU(cycleCounter, 10) = '1' then --and cmpLtU(cycleCounter, 1000) = '1' then
--                end if;                         
             end if;
        end process;
    end generate;

    TEMP_EXEC: block
        use work.LogicExec.all;
        
        -- TODO? Change syntax to array per subpipe rather then independent vars for each stage? Consider forking pipes like Mem 
        
        -- Selection from IQ and state after Issue stage
        signal slotSelI0, slotIssueI0,      slotIssueI0_A,
               slotSelI1, slotIssueI1,
               slotSelM0, slotIssueM0,
               slotSel3, slotIssue3,
               slotSelF0, slotIssueF0, slotRegReadF0,
               slotSel4, slotIssue4, slotSel5, slotIssue5, slotSel6, slotIssue6: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
        -- Exec stages
        signal slotI0_E0, slotI0_E1, slotI0_E2,
               slotI1_E0, slotI1_E1, slotI1_E2,
               slotM0_E0, slotM0_E1i, slotM0_E2i, -- Here logical stages get split into Int and FP
                          slotM0_E1f, slotM0_E2f,
               slot3_E0,  slot3_E1,slot3_E2,
               slotF0_E0, slotF0_E1, slotF0_E2,
               slot4_E0, slot4_E1, slot4_E2, slot5_E0, slot5_E1, slot5_E2: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
        -- Delay stages - after Exec
        signal slotI0_D0,  slotI0_D1,
               slotI1_D0,  slotI1_D1,
               slotM0_D0i, slotM0_D1i, -- Continued split of Int and FP
               slotM0_D0f, slotM0_D1f,               
               slot3_D0, slot3_D1,
               slotF0_D0, slotF0_D1, 
               slot4_D0, slot4_D1, slot5_D0, slot5_D1:
                            InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
        
        signal sendingSelI0, sendingIssueI0,
               sendingSelI1, sendingIssueI1,
               sendingSelM0, sendingIssueM0,
               sendingSelF0, sendingIssueF0, sendingRegReadF0: std_logic := '0';
        signal sendingI0_E0, sendingI0_E1, sendingI0_E2,
               sendingI1_E0, sendingI1_E1, sendingI1_E2,
               sendingM0_E0, sendingM0_E1, sendingM0_E2i,
                                           sendingM0_E2f,
               sendingF0_E0, sendingF0_E1, sendingF0_E2:
               std_logic := '0';
                
        signal sendingI0_D0, sendingI0_D1,
               sendingI1_D0, sendingI1_D1,
               sendingM0_D0i, sendingM0_D1i,         
               sendingM0_D0f, sendingM0_D1f,
               sendingF0_D0,  sendingF0_D1: std_logic := '0';                         

        ----
        signal schedDataI0, dataToQueueI0: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
        signal schedDataM0, dataToQueueM0: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);      
        signal schedDataF0, dataToQueueF0: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
        signal schedDataStoreValue, schedDataStoreValueFloat: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);

            signal NEW_ARR_DUMMY, newArrShared: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);

        signal dataToIssueStoreValue, dataToRegReadStoreValue, dataToExecIntStoreValue,
               dataToIssueFloatStoreValue, dataToRegReadFloatStoreValue, dataToExecFloatStoreValue,
               dataToExecStoreValue: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
        signal sendingToIssueStoreValue, sendingToRegReadStoreValue, sendingStoreValue, sendingToIssueFloatStoreValue: std_logic := '0';
        signal sentCancelledI0, sentCancelledI1, sentCancelledM0, sentCancelledM1, sentCancelledF0, sentCancelledSVI, sentCancelledSVF: std_logic := '0';

       signal emptyI0, emptyI1, emptyM0, emptyM1, emptySVI, emptySVF, emptyF0: std_logic := '0';  

       --==============----------
       signal intStoreMask, floatStoreMask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');            
       signal memMask, memMaskInt, memMaskFloat: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');

       signal sendingBranch: std_logic := '0'; -- Internal
       signal  dataOutMem0: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT); -- Outside
       signal sendingToAgu, sendingFromDLQ: std_logic := '0'; -- Outside block
       signal dataFromDLQ: InstructionState := DEFAULT_INSTRUCTION_STATE;
       signal memLoadValue: Mword := (others => '0'); -- MEM
      ----==============----------
       
       signal regsSelI0,           regsSelM0, regsSelS0, regsSelFloatA, regsSelFloatC, regsSelFS0, regsSelF0: PhysNameArray(0 to 2) := (others => (others => '0'));
       signal regValsI0, regValsB, regValsM0, regValsS0, regValsE, regValsFloatA, regValsFloatB, regValsFloatC, regValsFS0, regValsF0: MwordArray(0 to 2) := (others => (others => '0'));
       signal readyRegFlags, readyRegFlagsNext, readyRegFlagsSV, readyFloatFlags, readyFloatFlagsNext, readyRegFlagsFloatSV: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
        
       signal fni, fniFloat, fniEmpty: ForwardingInfo := DEFAULT_FORWARDING_INFO;
           
       -- Issue control 
       signal issuedStoreDataInt, issuedStoreDataFP, allowIssueStoreDataInt, allowIssueStoreDataFP, allowIssueStageStoreDataFP: std_logic := '0';
       signal memSubpipeSent, fp0subpipeSelected, lockIssueI0, allowIssueI0, lockIssueM0, allowIssueM0, lockIssueF0, allowIssueF0, memLoadReady: std_logic := '0';
                
       signal sendingToIntWriteQueue, sendingToFloatWriteQueue, sendingToIntRF, sendingToFloatRF: std_logic := '0';
       signal dataToIntWriteQueue, dataToFloatWriteQueue, dataToIntRF, dataToFloatRF: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
                                            
       signal fmaInt: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);                                                                      
    begin
        
        SUBPIPE_ALU: block
           signal dataToAlu, dataToBranch: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);           
           signal dataFromBranch: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
           signal branchData: InstructionState := DEFAULT_INSTRUCTION_STATE;
           
           signal inputDataArray: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
           signal staticInfoA: work.LogicIssue.StaticInfoArray(0 to PIPE_WIDTH-1);
           signal dynamicInfoA: work.LogicIssue.DynamicInfoArray(0 to PIPE_WIDTH-1);
           signal schedInfoA, schedInfoUpdatedA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => work.LogicIssue.DEFAULT_SCHEDULER_INFO);
        begin
            fmaInt <= work.LogicIssue.findForwardingMatchesArray(schedInfoA, fni);
        
            inputDataArray <= makeSlotArray(extractData(TMP_recodeALU(renamedDataLivingRe)), getAluMask(renamedDataLivingRe));
            schedInfoA <= work.LogicIssue.getIssueInfoArray(inputDataArray, true);
            schedInfoUpdatedA <= work.LogicIssue.updateSchedulerArray(schedInfoA, fni, fmaInt, ENQUEUE_FN_MAP, true, true);        
              
            IQUEUE_I0: entity work.IssueQueue(Behavioral)--UnitIQ
            generic map(
                IQ_SIZE => 8 --IQ_SIZES(4)
            )
            port map(
                clk => clk, reset => '0', en => '0',

                queuesAccepting => canSendBuff,
        
                acceptingOut => iqAcceptingI0,--iqAcceptingI0rr(4),
                acceptingMore => iqAcceptingMoreI0,
                prevSendingOK => renamedSending,
                newArr => dataToQueueI0,--,schArrays(4),
                    newArr_N => schedInfoUpdatedA,
                
                    newArr_Alt => NEW_ARR_DUMMY,
                    newArrOut => newArrShared,
                fni => fni,
                waitingFM => WAITING_FN_MAP,
                selectionFM => SELECTION_FN_MAP,
                readyRegFlags => readyRegFlags,
                nextAccepting => allowIssueI0,--issueAcceptingArr(4),
                execCausing => execCausing,
                lateEventSignal => lateEventSignal,
                execEventSignal => execEventSignal,
                
                anyReady => open,
                schedulerOut => slotSelI0,
                sending => sendingSelI0,
                sentCancelled => sentCancelledI0,
                
--                anyReady_A => open,
--                schedulerOut_A => slotSelI0,
--                sending_A => sendingSelI0,
--                sentCancelled_A => sentCancelledI0,

                empty => emptyI0
            );

            ISSUE_STAGE_I0: entity work.IssueStage
            generic map(USE_IMM => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => sendingSelI0,--sendingToIssueAlu,
                nextAccepting => '1',
        
                input => slotSelI0,-- dataToIssueAlu,
                
                acceptingOut => open,
                output => slotIssueI0,--dataToExecAlu,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fni,
                regValues => regValsI0 --(others => (others => '0'))     
            );
                ISSUE_STAGE_I0_A: entity work.IssueStage
                generic map(USE_IMM => true)
                port map(
                    clk => clk,
                    reset => '0',
                    en => '0',
            
                    prevSending => sendingSelI0,--sendingToIssueAlu,
                    nextAccepting => '1',
            
                    input => slotSelI0,-- dataToIssueAlu,
                    
                    acceptingOut => open,
                    output => slotIssueI0_A,--dataToExecAlu,
                    
                    execEventSignal => execEventSignal,
                    lateEventSignal => lateEventSignal,
                    execCausing => execCausing,
                    fni => fni,
                    regValues => regValsI0 --(others => (others => '0'))     
                );
                
            dataToAlu(0) <= (slotIssueI0.full and not sentCancelledI0, executeAlu(slotIssueI0.ins, slotIssueI0.state, bqSelected.ins, branchData));
          
            STAGE_I0_E0: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => dataToAlu(0).full,
                nextAccepting => '1',
                
                stageDataIn => dataToAlu,
                acceptingOut => open,
                sendingOut => sendingI0_E0,--sendingAlu,
                stageDataOut => slotI0_E0,-- dataOutAlu,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );      

            branchData <= basicBranch(slotIssueI0.ins, slotIssueI0.state, bqSelected.ins);                  
            
            dataToBranch(0) <= (slotIssueI0.full and not sentCancelledI0 and slotIssueI0.state.branchIns, branchData);
            sendingBranchIns <= dataToBranch(0).full;
            
            bqCompare <= (sendingBranchIns, slotIssueI0.ins);
            
            STAGE_I0_E0_BRANCH: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => dataToBranch(0).full,
                nextAccepting => '1',
                
                stageDataIn(0) => dataToBranch(0),
                acceptingOut => open,
                sendingOut => sendingBranch,
                stageDataOut(0) => dataFromBranch,
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => DEFAULT_INSTRUCTION_STATE-- execCausing                    
            );
            
            execEventSignal <= dataFromBranch.ins.controlInfo.newEvent and sendingBranch;
            execCausing <= clearDbCausing(dataFromBranch.ins);
            bqUpdate <= dataFromBranch;
        end block;
         
            
        SUBPIPE_MEM: block
           signal sendingIntLoad, sendingFloatLoad: std_logic := '0';
           signal dataToAgu, dataInMem0, dataInMemInt0, dataInMemFloat0, dataInMem1, dataInMemInt1, dataInMemFloat1: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);

           signal inputDataArray: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);           
           signal schedInfoA, schedInfoUpdatedA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1);                                         
        begin        
           memMaskInt <= getMemMask(renamedDataLivingRe);

           inputDataArray <= makeSlotArray(removeArg2(extractData(renamedDataLivingReMem)), memMaskInt);
           schedInfoA <= work.LogicIssue.getIssueInfoArray(inputDataArray, true);
           schedInfoUpdatedA <= work.LogicIssue.updateSchedulerArray(schedInfoA, fni, fmaInt, ENQUEUE_FN_MAP, true, true);        
                        
		   IQUEUE_MEM: entity work.IssueQueue(Behavioral)--UnitIQ
           generic map(
               IQ_SIZE => 8 --IQ_SIZES(4),
               ,ALT_INPUT => false --true
           )
           port map(
               clk => clk, reset => '0', en => '0',
       
               queuesAccepting => canSendBuff,
       
               acceptingOut => iqAcceptingM0,--iqAcceptingI0rr(4),
               acceptingMore => iqAcceptingMoreM0,
               sentCancelled => sentCancelledM0,               
               prevSendingOK => renamedSending,
               newArr => dataToQueueM0,--,schArrays(4),
                    newArr_N => schedInfoUpdatedA,
                    newArr_Alt => newArrShared,
               fni => fni,
               waitingFM => WAITING_FN_MAP,
               selectionFM => SELECTION_FN_MAP,
               readyRegFlags => readyRegFlags,
               nextAccepting =>allowIssueM0,--issueAcceptingArr(4),
               execCausing => execCausing,
               lateEventSignal => lateEventSignal,
               execEventSignal => execEventSignal,
               empty => emptyM0,
               anyReady => open,--iqReadyArr(4),
               schedulerOut => slotSelM0,
               sending => sendingSelM0
           );
    
           ISSUE_STAGE_MEM: entity work.IssueStage
           generic map(USE_IMM => true)
           port map(
               clk => clk,
               reset => '0',
               en => '0',
       
               prevSending => sendingSelM0,
               nextAccepting => '1',
       
               input => slotSelM0,
               
               acceptingOut => open,
               output => slotIssueM0,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing,
               
               fni => fni,
               regValues => regValsM0 --(others => (others => '0'))     
           );
               
           preAguTag <= slotIssueM0.ins.tags.renameIndex;
           preIndexSQ <= slotIssueM0.ins.tags.sqPointer;
           preIndexLQ <= slotIssueM0.ins.tags.lqPointer;
                                 
           sendingFromDLQ <= '0';          -- TEMP!
           dataFromDLQ <= DEFAULT_INSTRUCTION_STATE; -- TEMP!

           sendingToAgu <= (slotIssueM0.full and not sentCancelledM0) or sendingFromDLQ;
	       dataToAgu(0) <= (sendingToAgu, calcEffectiveAddress(slotIssueM0.ins, slotIssueM0.state, sendingFromDLQ, dataFromDLQ));
       
           STAGE_AGU: entity work.GenericStage(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
       
               prevSending => sendingToAgu,
               nextAccepting => '1',
               
               stageDataIn => dataToAgu,
               acceptingOut => open,
               sendingOut => sendingM0_E0,
               stageDataOut => slotM0_E0,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing
           );

	       dataInMem0(0) <= (sendingM0_E0, setDataCompleted(setAddressCompleted(slotM0_E0(0).ins, '0'), '0'));
           sqAddressInput <= dataInMem0(0);
           lqAddressInput <= dataInMem0(0);

           dataInMemInt0 <= clearFloatDest(dataInMem0);
           dataInMemFloat0 <= clearIntDest(dataInMem0);

           -- TLB lookup, Dcache access
	       STAGE_MEM0: entity work.GenericStage(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
               
               prevSending => sendingM0_E0,
               nextAccepting => '1',
               
               stageDataIn => dataInMemInt0,
               acceptingOut => open,
               sendingOut => sendingM0_E1,
               stageDataOut => slotM0_E1i,--dataAfterMemA,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing                
           );

	       STAGE_MEM0_FLOAT: entity work.GenericStage(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
               
               prevSending => sendingM0_E0,
               nextAccepting => '1',
               
               stageDataIn => dataInMemFloat0,
               acceptingOut => open,
               sendingOut => open,
               stageDataOut => slotM0_E1f,--dataAfterMemA,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing                
           );
                      
           dataOutMem0(0) <= mergePhysDests(slotM0_E1i(0), slotM0_E1f(0)); -- [dest := Int.dest | Float.dest];

           dataInMem1(0).full <= sendingM0_E1;
           dataInMem1(0).ins <= getLSResultData(dataOutMem0(0).ins,
                                                  '1', (others => '0'),
                                                  memLoadReady, memLoadValue,
                                                  sysRegSending, sysRegReadValue, 
                                                  sqSelectedOutput.full, sqSelectedOutput.ins,
                                                  lqSelectedOutput);
                                                  
          sendingIntLoad <= sendingM0_E1 and not dataOutMem0(0).ins.physicalArgSpec.floatDestSel; -- TODO: check exact conditions 
          sendingFloatLoad <= sendingM0_E1 and dataOutMem0(0).ins.physicalArgSpec.floatDestSel;
                      
          dataInMemInt1 <= clearFloatDest(dataInMem1); -- with zeroed dest when load is FP
          dataInMemFloat1 <= clearIntDest(dataInMem1); -- with zeroed dest when load is Int??
                                                  	       
           -- Source selection and verification
	       STAGE_MEM1: entity work.GenericStage(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
               
               prevSending => sendingM0_E1,-- sendingM0_E1,
               nextAccepting => '1',
               
               stageDataIn => dataInMemInt1,
               acceptingOut => open,
               sendingOut => sendingM0_E2i,
               stageDataOut => slotM0_E2i,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing                
           );
           
           -- Branching into FP cluster
           STAGE_MEM1_FLOAT: entity work.GenericStage(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
               
               prevSending => sendingM0_E1,--sendingM0_E1,
               nextAccepting => '1',
               
               stageDataIn => dataInMemFloat1,
               acceptingOut => open,
               sendingOut => sendingM0_E2f,
               stageDataOut => slotM0_E2f,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing                
           );
           
           -- TEMP mem interface    
		   dread <= slotM0_E0(0).full;
           dadr <= slotM0_E0(0).ins.result;
           sysRegReadSel <= slotM0_E0(0).ins.result(4 downto 0);
           sysRegRead <= sendingM0_E0 and isLoadSysOp(slotM0_E0(0).ins);
           
           memLoadReady <= dvalid;              
           memLoadValue <= din;      
        end block;   

        ------------------------
        readyRegFlagsSV <= (readyRegFlags(2), '0', '0', readyRegFlags(5), '0', '0', readyRegFlags(8), '0', '0', readyRegFlags(11), '0', '0');

        SUBPIPES_STORE_VALUE: block
            signal dataToStoreValueIQ, dataToStoreValueFloatIQ: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);             
            signal fmaIntSV, fmaFloatSV: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);
            signal sendingToRegReadI, sendingToRegReadF: std_logic := '0';

            signal inputDataArrayInt, inputDataArrayFloat: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);            
            signal schedInfoIntA, schedInfoUpdatedIntA, schedInfoFloatA, schedInfoUpdatedFloatA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1);
        begin
            -- CHECK: does it need to use 'sentCancelled' signal from IQs?

            inputDataArrayInt <= makeSlotArray(prepareForStoreValueIQ(extractData(renamedDataLivingReMem)), intStoreMask);
            schedInfoIntA <= work.LogicIssue.getIssueInfoArray(inputDataArrayInt, false);
            schedInfoUpdatedIntA <= work.LogicIssue.updateSchedulerArray(schedInfoIntA, fni, fmaIntSV, ENQUEUE_FN_MAP_SV, true, true);  

            inputDataArrayFloat <= makeSlotArray(prepareForStoreValueFloatIQ(extractData(renamedDataLivingReMem), extractData(renamedDataLivingFloatRe)), floatStoreMask);
            schedInfoFloatA <= work.LogicIssue.getIssueInfoArray(inputDataArrayFloat, false);
            schedInfoUpdatedFloatA <= work.LogicIssue.updateSchedulerArray(schedInfoFloatA, fni, fmaFloatSV, ENQUEUE_FN_MAP_FLOAT_SV, true, true);

            fmaIntSV <= work.LogicIssue.findForwardingMatchesArray(schedInfoIntA, fni);
            fmaFloatSV <= work.LogicIssue.findForwardingMatchesArray(schedInfoFloatA, fniFloat);
            
            intStoreMask <= getStoreMask(renamedDataLivingReMem) and not floatStoreMask;
            floatStoreMask <= getFloatStoreMask(renamedDataLivingReMem, renamedDataLivingFloatRe);
        
            IQUEUE_SV: entity work.IssueQueue(Behavioral)--UnitIQ
            generic map(
                IQ_SIZE => 8 --IQ_SIZES(4)
            )
            port map(
                clk => clk, reset => '0', en => '0',

                queuesAccepting => canSendBuff,
        
                acceptingOut => iqAcceptingS0,--iqAcceptingI0rr(4),
                acceptingMore => iqAcceptingMoreS0,
                sentCancelled => sentCancelledSVI,                
                prevSendingOK => renamedSending,
                newArr => dataToStoreValueIQ,--,schArrays(4),
                    newArr_N => schedInfoUpdatedIntA,
                    newArr_Alt => NEW_ARR_DUMMY,
                fni => fni,
                waitingFM => WAITING_FN_MAP_SV,
                selectionFM => DEFAULT_FORWARDING_MAP,      
                readyRegFlags => readyRegFlagsSV,
                nextAccepting => allowIssueStoreDataInt,--issueAcceptingArr(4),
                execCausing => execCausing,
                lateEventSignal => lateEventSignal,
                execEventSignal => execEventSignal,
                empty => emptySVI,
                anyReady => open,--iqReadyArr(4),
                schedulerOut => dataToIssueStoreValue,
                sending => sendingToIssueStoreValue
            );
     
            ISSUE_STAGE_SV: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => sendingToIssueStoreValue,
                nextAccepting => '1',
        
                input => dataToIssueStoreValue,
                
                acceptingOut => open,
                output => dataToRegReadStoreValue,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniEmpty,
                regValues => (others => (others => '0'))   
            );
            
            sendingToRegReadI <= dataToRegReadStoreValue.full and not sentCancelledSVI;
            
            REG_READ_STAGE_SV: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => sendingToRegReadI,
                nextAccepting => '1',
        
                input => dataToRegReadStoreValue,
                
                acceptingOut => open,
                output => dataToExecIntStoreValue,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniEmpty,
                regValues => regValsS0     
            );
      
            ------------------------------------
            readyRegFlagsFloatSV <= (readyFloatFlags(2), '0', '0', readyFloatFlags(5), '0', '0', readyFloatFlags(8), '0', '0', readyFloatFlags(11), '0', '0');
            
            
            IQUEUE_FLOAT_SV: entity work.IssueQueue(Behavioral)--UnitIQ
            generic map(
                IQ_SIZE => 8 -- CAREFUL: not IS_FP because doesn't have destination
            )
            port map(
                clk => clk, reset => '0', en => '0',

                queuesAccepting => canSendBuff,
       
                acceptingOut => iqAcceptingSF0,--iqAcceptingI0rr(4),
                acceptingMore => iqAcceptingMoreSF0,
                sentCancelled => sentCancelledSVF,                
                prevSendingOK => renamedSending,
                newArr => dataToStoreValueFloatIQ,--,schArrays(4),
                    newArr_N => schedInfoUpdatedFloatA,
                    newArr_Alt => NEW_ARR_DUMMY,                
                fni => fniFloat,
                waitingFM => WAITING_FN_MAP_FLOAT_SV,
                selectionFM => DEFAULT_FORWARDING_MAP,      
                readyRegFlags => readyRegFlagsFloatSV,
                nextAccepting => allowIssueStoreDataFP,--issueAcceptingArr(4),
                execCausing => execCausing,
                lateEventSignal => lateEventSignal,
                execEventSignal => execEventSignal,
                empty => emptySVF,
                anyReady => open,--iqReadyArr(4),
                schedulerOut => dataToIssueFloatStoreValue,--open,--dataToIssueStoreValue,
                sending => sendingToIssueFloatStoreValue --open --sendingToIssueStoreValue
            );
    
            ISSUE_STAGE_FLOAT_SV: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => sendingToIssueFloatStoreValue,
                nextAccepting => allowIssueStageStoreDataFP,
        
                input => dataToIssueFloatStoreValue,
                
                acceptingOut => open,
                output => dataToRegReadFloatStoreValue,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniEmpty,
                regValues => (others => (others => '0'))   
            );        

                sendingToRegReadF <= dataToRegReadFloatStoreValue.full and not sentCancelledSVF;
    
            REG_READ_STAGE_FLOAT_SV: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => sendingToRegReadF,
                nextAccepting => '1',
        
                input => dataToRegReadFloatStoreValue,
                
                acceptingOut => open,
                output => dataToExecFloatStoreValue,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniEmpty,
                regValues => regValsFS0     
            );
    
            dataToExecStoreValue <= getStoreDataOp(dataToExecFloatStoreValue) when dataToExecFloatStoreValue.full = '1'
                            else    getStoreDataOp(dataToExecIntStoreValue);
        end block;

        
        SUBPIPE_FP0: block
            signal dataToFpu0: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
            signal fmaF0: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);          
            signal inputDataArray: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);            
            signal schedInfoA, schedInfoUpdatedA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1);
        begin
            fmaF0 <= work.LogicIssue.findForwardingMatchesArray(schedInfoA, fniFloat);

            inputDataArray <= makeSlotArray(extractData(TMP_recodeFP(renamedDataLivingFloatRe)), getFpuMask(renamedDataLivingFloatRe));
            schedInfoA <= work.LogicIssue.getIssueInfoArray(inputDataArray, false);
            schedInfoUpdatedA <= work.LogicIssue.updateSchedulerArray(schedInfoA, fniFloat, fmaF0, ENQUEUE_FN_MAP_FLOAT, true, true);

            IQUEUE_F0: entity work.IssueQueue(Behavioral)--UnitIQ
            generic map(
                IQ_SIZE => 8,
                IS_FP => true
            )
            port map(
                clk => clk, reset => '0', en => '0',

                queuesAccepting => canSendBuff,
        
                acceptingOut => iqAcceptingF0,--iqAcceptingI0rr(4),
                acceptingMore => iqAcceptingMoreF0,
                sentCancelled => sentCancelledF0,                
                prevSendingOK => renamedSending,
                newArr => dataToQueueF0,--,schArrays(4),
                    newArr_N => schedInfoUpdatedA,
                    newArr_Alt => NEW_ARR_DUMMY,                
                fni => fniFloat,
                waitingFM => WAITING_FN_MAP_FLOAT,
                selectionFM => SELECTION_FN_MAP_FLOAT,
                readyRegFlags => readyFloatFlags,
                nextAccepting => allowIssueF0,--'1',--issueAcceptingArr(4),
                execCausing => execCausing,
                lateEventSignal => lateEventSignal,
                execEventSignal => execEventSignal,
                empty => emptyF0,
                anyReady => open,--iqReadyArr(4),
                schedulerOut => slotSelF0,--dataToIssueAlu,
                sending => sendingSelF0 --sendingToIssueAlu
            );

            ISSUE_STAGE_F0: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => false, DELAY_ONLY => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => sendingSelF0,
                nextAccepting => '1',
                                  --allowIssueStageStoreDataFP,               
                input => slotSelF0,
                
                acceptingOut => open,
                output => slotIssueF0,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniFloat,
                regValues => (others => (others => '0'))   
            );        
    
            REG_READ_STAGE_F0: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => false)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => slotIssueF0.full,
                nextAccepting => '1',
        
                input => slotIssueF0,
                
                acceptingOut => open,
                output => slotRegreadF0,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniFloat,
                regValues => regValsF0     
            );
          
            dataToFpu0(0) <= (slotRegReadF0.full and not sentCancelledF0, executeFpu(slotregReadF0.ins, slotregReadF0.state));
          
            STAGE_F0_E0: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => dataToFpu0(0).full,
                nextAccepting => '1',
                
                stageDataIn => dataToFpu0,
                acceptingOut => open,
                sendingOut => sendingF0_E0,--sendingAlu,
                stageDataOut => slotF0_E0,-- dataOutAlu,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing
            );     

            STAGE_F0_E1: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingF0_E0,
                nextAccepting => '1',
                
                stageDataIn => slotF0_E0,
                acceptingOut => open,
                sendingOut => sendingF0_E1,--sendingAlu,
                stageDataOut => slotF0_E1,-- dataOutAlu,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing
            );

            STAGE_F0_E2: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingF0_E1,
                nextAccepting => '1',
                
                stageDataIn => slotF0_E1,
                acceptingOut => open,
                sendingOut => sendingF0_E2,--sendingAlu,
                stageDataOut => slotF0_E2,-- dataOutAlu,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing
            );
  
        end block;
        
           
         sqValueInput <= -- CAREFUL: This implies that integer StoreData op value is lost when Int and FP are issued simultaneously. This must be prevented by scheduler!
           --        (dataToExecFloatStoreValue.full, setInstructionResult(dataToExecFloatStoreValue.ins, dataToExecFloatStoreValue.state.args(0))) when dataToExecFloatStoreValue.full = '1' 
           -- else    (dataToExecIntStoreValue.full,   setInstructionResult(dataToExecIntStoreValue.ins,   dataToExecIntStoreValue.state.args(0))); -- TEMP!!
                    (dataToExecStoreValue.full, setInstructionResult(dataToExecStoreValue.ins, dataToExecStoreValue.state.args(0)));
         
         -- StoreData issue control:
         -- When Int and FP store data issue at the same time, the port conflict is resolved thus:
         -- Both IQs are blocked for the next cycle, so combined issue rate is never higher that 1 per cycle
         -- FP op is stalled for 1 cycle at IssueStage - no problems appear with scheduling because this subpipe has no wakeup observers and reads ags only form RF 
         process (clk)
         begin
            if rising_edge(clk) then
                issuedStoreDataInt <= sendingToIssueStoreValue;
                issuedStoreDataFP <= sendingToIssueFloatStoreValue;
            end if;
         end process;
         
         allowIssueStageStoreDataFP <= allowIssueStoreDataInt; -- In this case happens to be equal
         
         allowIssueStoreDataInt <= not (issuedStoreDataInt and issuedStoreDataFP);
         allowIssueStoreDataFP <= allowIssueStoreDataInt;
         
         -------------------------------------------
         --DELAY_STAGES: block
         --begin
            STAGE_I0_D0: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingI0_E0,--sendingAlu,
                nextAccepting => '1',
                
                stageDataIn => slotI0_E0,--dataOutAlu,
                acceptingOut => open,
                sendingOut => sendingI0_D0,--sendingOutAluDelay,
                stageDataOut => slotI0_D0,--dataOutAluDelay,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );           

            STAGE_M0_D0: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingM0_E2i,
                nextAccepting => '1',
                
                stageDataIn => slotM0_E2i,
                acceptingOut => open,
                sendingOut => sendingM0_D0i,
                stageDataOut => slotM0_D0i,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );        

            STAGE_M0_D0F: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingM0_E2f,
                nextAccepting => '1',
                
                stageDataIn => slotM0_E2f,
                acceptingOut => open,
                sendingOut => sendingM0_D0f,
                stageDataOut => slotM0_D0f,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );
            
            -- After FP_LOAD_DELAY
            STAGE_M0_D1F: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingM0_D0f,
                nextAccepting => '1',
                
                stageDataIn => slotM0_D0f,
                acceptingOut => open,
                sendingOut => sendingM0_D1f,
                stageDataOut => slotM0_D1f,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );

            STAGE_F0_D0: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingF0_E2,--sendingAlu,
                nextAccepting => '1',
                
                stageDataIn => slotF0_E2,--dataOutAlu,
                acceptingOut => open,
                sendingOut => sendingF0_D0,--sendingOutAluDelay,
                stageDataOut => slotF0_D0,--dataOutAluDelay,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );
        --end block; -- DELAY_STAGES
                
            -- TEMP:
            SCHED_BLOCK: process(clk)
            begin
                if rising_edge(clk) then
                    assert (sendingI0_E0 and sendingM0_E2i) = '0' report "Int write queue conflict!" severity error;
                    memSubpipeSent <= sendingToAgu;
                    
                    fp0subpipeSelected <= sendingSelF0;
                end if;
            end process;
            
            lockIssueI0 <= memSubpipeSent;
            allowIssueI0 <= not lockIssueI0;
            
            -- TODO: issue locking for F0 subpipe - avoid WB collisions with FP load!
            lockIssueM0 <= fp0subpipeSelected;
            allowIssueM0 <= not lockIssueM0;
            
            lockIssueF0 <= '0';
            allowIssueF0 <= not lockIssueF0;
            
            -----
            
            sendingToIntWriteQueue <= sendingI0_E0 or sendingM0_E2i;
            dataToIntWriteQueue <= slotM0_E2i when sendingM0_E2i = '1' else slotI0_E0;
            
            INT_WRITE_QUEUE: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingToIntWriteQueue,
                nextAccepting => '1',
                
                stageDataIn => dataToIntWriteQueue,
                acceptingOut => open,
                sendingOut => sendingToIntRF,
                stageDataOut => dataToIntRF,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            ); 
            
                    
         execOutputs1(0) <= (sendingI0_E0, slotI0_E0(0).ins);
         execOutputs1(2) <= mergePhysDests(slotM0_E2i(0), slotM0_E2f(0)); --  [dest := Int.dest | Float.dest];             
         execOutputs1(3) <= (sendingF0_E2, slotF0_E2(0).ins);
            
         -- TODO: include mem hit in 'full' flag! Should merge some info from Float path??
         
         execOutputs2(2) <= (dataToExecStoreValue.full, dataToExecStoreValue.ins);

     
         regsSelI0 <= work.LogicRenaming.getPhysicalArgs((0 => ('1', slotSelI0.ins)));
         regsSelM0 <= work.LogicRenaming.getPhysicalArgs((0 => ('1', slotSelM0.ins)));        
         -- TEMP!
         regsSelS0 <= work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToRegReadStoreValue.ins)));
          
         -- Forwarding network
		 fni.nextTagsM1 <= (0 => slotIssueI0.ins.physicalArgSpec.dest, 2 => slotM0_E1i(0).ins.physicalArgSpec.dest, others => (others => '0'));        
		 fni.nextTagsM2 <= (                                           2 => slotM0_E0(0).ins.physicalArgSpec.dest,  others => (others => '0'));
         fni.tags0 <= (execOutputs1(0).ins.physicalArgSpec.dest, execOutputs1(1).ins.physicalArgSpec.dest, slotM0_E2i(0).ins.physicalArgSpec.dest);
         fni.tags1 <= (0 => slotI0_D0(0).ins.physicalArgSpec.dest, 2 => slotM0_D0i(0).ins.physicalArgSpec.dest, others => (others => '0'));
         fni.values0 <= (execOutputs1(0).ins.result, execOutputs1(1).ins.result, execOutputs1(2).ins.result);
         fni.values1 <= (0 => slotI0_D0(0).ins.result, 2 => slotM0_D0i(0).ins.result, others => (others => '0'));                 
                
                       
         regsSelFS0 <= work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToRegReadFloatStoreValue.ins)));
         regsSelF0 <= work.LogicRenaming.getPhysicalArgs((0 => ('1', slotIssueF0.ins)));

         -- NOTE: FP load path is 1 cycle longer, so different stages are involved here from those in Int datapath
         --      TODO: CHECK
         fniFloat.nextTagsM1 <= (0 => slotF0_E1(0).ins.physicalArgSpec.dest, 2 => slotM0_E2f(0).ins.physicalArgSpec.dest, others => (others => '0'));
         fniFloat.nextTagsM2 <= (0 => slotF0_E0(0).ins.physicalArgSpec.dest, 2 => slotM0_E1f(0).ins.physicalArgSpec.dest, others => (others => '0'));               
         fniFloat.tags0 <= (0 => slotF0_E2(0).ins.physicalArgSpec.dest, 2 => slotM0_D0f(0).ins.physicalArgSpec.dest, others => (others => '0'));
         fniFloat.tags1 <= (0 => slotF0_D0(0).ins.physicalArgSpec.dest, 2 => slotM0_D1f(0).ins.physicalArgSpec.dest, others => (others => '0'));
         fniFloat.values0 <= (0 => slotF0_E2(0).ins.result, 2 => slotM0_D0f(0).ins.result, others => (others => '0'));
         fniFloat.values1 <= (0 => slotF0_D0(0).ins.result, 2 => slotM0_D1f(0).ins.result, others => (others => '0'));
                

		 INT_REG_FILE: entity work.RegFile(Behavioral)
         generic map(WIDTH => 4, WRITE_WIDTH => 1)
         port map(
             clk => clk, reset => '0', en => '0',
                 
             writeAllow => sendingToIntRF,
             writeInput => dataToIntRF,
 
             readAllowVec => (others => '1'), -- TEMP!
             
             selectRead(0 to 2) => regsSelI0,--(others => (others => '0')),
             selectRead(3 to 5) => (others => (others => '0')),
             selectRead(6 to 8) => regsSelM0,
             selectRead(9 to 11) => regsSelS0,
             
             readValues(0 to 2) => regValsI0,--open,
             readValues(3 to 5) => regValsB,
             readValues(6 to 8) => regValsM0,                        
             readValues(9 to 11) => regValsS0            
         );
         
         INT_READY_TABLE: entity work.RegisterReadyTable(Behavioral)
         generic map(
             WRITE_WIDTH => 1
         )
         port map(
             clk => clk, reset => '0', en => '0', 
             
             sendingToReserve => frontLastSending,
             stageDataToReserve => frontDataLastLiving,
                 
             newPhysDests => newIntDests,    -- FOR MAPPING
             stageDataReserved => renamedDataLivingRe, --stageDataOutRename,
                 
             -- TODO: change to ins slot based
             writingMask(0) => sendingToIntRF,
             writingData(0) => dataToIntRF(0).ins,
             readyRegFlagsNext => readyRegFlagsNext -- FOR IQs
         );


            sendingToFloatWriteQueue <= sendingM0_E2f or sendingF0_E2; -- TEMP, TODO!
            dataToFloatWriteQueue <= slotM0_E2f when sendingM0_E2f = '1' else slotF0_E2;
            
            FLOAT_WRITE_QUEUE: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingToFloatWriteQueue,
                nextAccepting => '1',
                
                stageDataIn => dataToFloatWriteQueue,
                acceptingOut => open,
                sendingOut => sendingToFloatRF,
                stageDataOut => dataToFloatRF,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );


		 FLOAT_REG_FILE: entity work.RegFile(Behavioral)
         generic map(IS_FP => true, WIDTH => 4, WRITE_WIDTH => 1)
         port map(
             clk => clk, reset => '0', en => '0',
                 
             writeAllow => sendingToFloatRF,
             writeInput => dataToFloatRF,
 
             readAllowVec => (others => '1'), -- TEMP!
             
             selectRead(0 to 2) => regsSelF0,
             selectRead(3 to 5) => (others => (others => '0')),
             selectRead(6 to 8) => (others => (others => '0')),--regsSelM0,
             selectRead(9 to 11) => regsSelFS0,
             
             readValues(0 to 2) => regValsF0,--open,
             readValues(3 to 5) => regValsFloatB,
             readValues(6 to 8) => regValsFloatC,                       
             readValues(9 to 11) => regValsFS0            
         );
         
         FLOAT_READY_TABLE: entity work.RegisterReadyTable(Behavioral)
         generic map(
             IS_FP => true, WRITE_WIDTH => 1
         )
         port map(
             clk => clk, reset => '0', en => '0', 
             
             sendingToReserve => frontLastSending,
             stageDataToReserve => frontDataLastLiving,
                 
             newPhysDests => newFloatDests,    -- FOR MAPPING
             stageDataReserved => renamedDataLivingFloatPre, --stageDataOutRename,
                 
             writingMask(0) => sendingToFloatRF,  
             writingData(0) => dataToFloatRF(0).ins,
             readyRegFlagsNext => readyFloatFlagsNext -- FOR IQs
         );
               
         READY_REG_FLAGS: process(clk)
         begin
            if rising_edge(clk) then
                readyRegFlags <= readyRegFlagsNext;
                readyFloatFlags <= readyFloatFlagsNext;
            end if;
         end process;
         
         sysRegSending <= sysRegRead;


	   VIEW: if VIEW_ON generate
            use work.Viewing.all;
           
            signal cycleCount: Mword := (others => '0');
            
            signal issueTextI0, issueTextM0, issueTextSVI, issueTextSVF, issueTextF0: InsStringArray(0 to 0);
            signal slotTextRegReadF0: InsStringArray(0 to 0);            
            signal slotTextI0_E0, slotTextI0_E1, slotTextI0_E2, slotTextM0_E0, slotTextM0_E1i, slotTextM0_E2i, slotTextM0_E1f, slotTextM0_E2f: InsStringArray(0 to 0);
            
            signal iqInputI0, iqInputI1, iqInputM0, iqInputSVI, iqInputSVF, iqInputF0: GenericStageView;
            signal execOutputsText1, execOutputsText2: InsStringArray(0 to 3);
 
            type SubpipeMonitor is record
                empty: std_logic;
                lastSent: integer;
                lastComp: integer;
            end record;
            
            constant DEFAULT_SUBPIPE_MONITOR: SubpipeMonitor := ('0', -1, -1);
            
            function updateSubpipeMonitor(sm: SubpipeMonitor; empty: std_logic; cycleCtr: Mword; sent, comp: std_logic) return SubpipeMonitor is
                variable res: SubpipeMonitor := sm;
            begin
                res.empty := empty;
                if sent = '1' then
                    res.lastSent := 0;
                elsif res.lastSent >= 0 then
                    res.lastSent := res.lastSent + 1;
                end if;
                if comp = '1' then
                    res.lastComp := 0;
                elsif res.lastComp >= 0 then
                    res.lastComp := res.lastComp + 1;
                end if;
                return res;
            end function;
            
            signal slotM0_E1iText, slotM0_E2iText: InsString := (others=> ' ');
            
            signal monitorI0, monitorM0, monitorSVI, monitorSVF, monitorF0: SubpipeMonitor := DEFAULT_SUBPIPE_MONITOR; 
         begin
            cycleCount <= cycleCounter;
            
            execOutputsText1 <= getInsStringArray(execOutputs1);
            execOutputsText2 <= getInsStringArray(execOutputs2);
            
            
            iqInputI0 <= getInsStringArray(schedDataI0);
            iqInputM0 <= getInsStringArray(schedDataM0);
            iqInputSVI <= getInsStringArray(schedDataStoreValue);
            iqInputSVF <= getInsStringArray(schedDataStoreValueFloat);
            iqInputF0 <= getInsStringArray(schedDataF0);
         
            issueTextI0 <= getInsStringArray(SchedulerEntrySlotArray'(0 => slotIssueI0));
            issueTextM0 <= getInsStringArray(SchedulerEntrySlotArray'(0 => slotIssueM0));
            issueTextSVI <= getInsStringArray(SchedulerEntrySlotArray'(0 => dataToRegReadStoreValue));
            issueTextSVF <= getInsStringArray(SchedulerEntrySlotArray'(0 => dataToRegReadFloatStoreValue));
            issueTextF0 <= getInsStringArray(SchedulerEntrySlotArray'(0 => slotIssueF0));

            
            slotTextRegReadF0 <= getInsStringArray(SchedulerEntrySlotArray'(0 => slotRegReadF0));

            slotTextI0_E0 <= getInsStringArray(slotI0_E0);
            slotTextI0_E1 <= getInsStringArray(slotI0_E1);
            slotTextI0_E2 <= getInsStringArray(slotI0_E2);

            slotTextM0_E0 <= getInsStringArray(slotM0_E0);
            slotTextM0_E1i <= getInsStringArray(slotM0_E1i);
            slotTextM0_E1f <= getInsStringArray(slotM0_E1f);
            slotTextM0_E2i <= getInsStringArray(slotM0_E2i);
            slotTextM0_E2f <= getInsStringArray(slotM0_E2f);


            slotM0_E1iText <= getInsString(slotM0_E1i(0), transfer);
            slotM0_E2iText <= getInsString(slotM0_E2i(0), transfer);

            process (clk)
            begin
                if rising_edge(clk) then
--                    if cmpGtU(cycleCounter, 30) = '1' then --and cmpLtU(cycleCounter, 100) = '1' then
--                    end if;
                    
                    -- Issue & complete monitoring
                    -- sendingSel* - from IQ;  sendingIssue* - form Issue stage
                    monitorI0 <= updateSubpipeMonitor(monitorI0, emptyI0, cycleCounter, sendingSelI0, sendingI0_D0);
                    monitorM0 <= updateSubpipeMonitor(monitorM0, emptyM0, cycleCounter, sendingSelM0, sendingM0_D0i or sendingM0_D0f);
                    monitorSVI <= updateSubpipeMonitor(monitorSVI, emptySVI, cycleCounter, sendingToIssueStoreValue, '0');
                    monitorSVF <= updateSubpipeMonitor(monitorSVF, emptySVF, cycleCounter, sendingToIssueFloatStoreValue, '0');
                    monitorF0 <= updateSubpipeMonitor(monitorF0, emptyF0, cycleCounter, sendingSelF0, sendingF0_D0); 
                    
                end if;
            end process;                                     
            -- TODO: add remaining stages of Exec area
         end generate;
        
    end block; -- TEMP_EXEC

    renamedDataToBQ <= setFullMask(renamedDataLivingBuff, getBranchMask(renamedDataLivingBuff));
    renamedDataToSQ <= setFullMask(renamedDataLivingMemBuff, getStoreMask(renamedDataLivingMemBuff));
    renamedDataToLQ <= setFullMask(renamedDataLivingMemBuff, getLoadMask(renamedDataLivingMemBuff));

    BRANCH_QUEUE: entity work.BranchQueue
	generic map(
		QUEUE_SIZE => BQ_SIZE
	)
	port map(
		clk => clk,
		reset => '0',
		en => '0',

		acceptingOut => open,
		almostFull => open,
		
		acceptingBr => bqAccepting,
		
		prevSending => renamedSendingBuff,
	    prevSendingBr => bpSending,
	    
	    prevSendingRe => renameSendingBr,
	    
	       bqPtrOut => bqPointer,
	    
		dataIn => renamedDataToBQ,
		dataInBr => bpData,

        --- interface with Int Exec
		storeValueInput => bqUpdate,
		compareAddressInput => bqCompare,

		selectedDataOutput => bqSelected,
        ----

		committing => robSending, -- When ROB is sending so is BQ if it has corresponding branches
		robData => dataOutROB,
		groupCtrInc => commitGroupCtrInc,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignal,
		execCausing => execCausing,
		
		nextAccepting => commitAccepting,		
		sendingSQOut => bqSending,
		dataOutV => bqData,
		
		committedDataOut => bqTargetData
	);

    STORE_QUEUE: entity work.StoreQueue(Behavioral)
	generic map(
		QUEUE_SIZE => SQ_SIZE
	)
	port map(
		clk => clk,
		reset => '0',
		en => '0',

		acceptingOut => acceptingSQ,
		almostFull => almostFullSQ,
				
	       prevSendingRe => frontLastSending,
		prevSending => renamedSendingBuff,
		
		   dataInRe => --frontDataLastLiving,
		                  TMP_frontDataSpMasked,
		dataIn => renamedDataToSQ, -- !!!!!
            
            renamedPtr => sqPointer,
            
        -- interface with Exec
		storeValueInput => sqValueInput, 
		compareAddressInput => sqAddressInput,
        compareTagInput => preAguTag,
            compareIndexInput => preIndexSQ,
                            
		selectedDataOutput => sqSelectedOutput,
        ------------

		committing => robSending,
		robData => dataOutROB,
		groupCtrInc => commitGroupCtrInc,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignal,
		execCausing => execCausing,
		
		nextAccepting => commitAccepting,		
		sendingSQOut => open,
		dataOutV => open,

        committedEmpty => sbEmpty,
        committedSending => sbSending,
        committedDataOut => dataFromSB
	);


    LOAD_QUEUE: entity work.StoreQueue(Behavioral)
	generic map(
		QUEUE_SIZE => LQ_SIZE,
		IS_LOAD_QUEUE => true
	)
	port map(
		clk => clk,
		reset => '0',
		en => '0',

		acceptingOut => acceptingLQ,
		almostFull => almostFullLQ,

	       prevSendingRe => frontLastSending,				
		prevSending => renamedSendingBuff,
		
		   dataInRe => --frontDataLastLiving,
		                  	TMP_frontDataSpMasked,
		dataIn => renamedDataToLQ, -- !!!!!

            renamedPtr => lqPointer,

        -- interface with Exec
		storeValueInput => DEFAULT_INSTRUCTION_SLOT, 
		compareAddressInput => lqAddressInput,
        compareTagInput => preAguTag,
            compareIndexInput => preIndexLQ,        
             
		selectedDataOutput => lqSelectedOutput,
        ----------------

		committing => robSending,
		robData => dataOutROB,
		groupCtrInc => commitGroupCtrInc,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignal,
		execCausing => execCausing,
		
		nextAccepting => commitAccepting,		
		sendingSQOut => open,
		dataOutV => open,

        committedEmpty => open,--sbEmpty,
        committedSending => open,--sbSending,
        committedDataOut => open --dataFromSB
	);

-----------------------------------------
----- Mem signals -----------------------
	MEMORY_INTERFACE: block
		signal sysStoreAddressW: Mword := (others => '0');
	begin
		doutadr <= dataFromSB(0).ins.target;
		dwrite <= sbSending and dataFromSB(0).full and isStoreMemOp(dataFromSB(0).ins);
		dout <= dataFromSB(0).ins.result;
	end block;
	
end Behavioral;
