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
use work.ForwardingNetwork.all;


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
    signal pcDataSig, frontCausing, execCausing, preExecCausing, execCausingDelayed, lateCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;

    signal pcSending, frontAccepting, bpAccepting, bpSending, renameAccepting, frontLastSending,
           frontEventSignal, bqAccepting, acceptingSQ, almostFullSQ, acceptingLQ, almostFullLQ,
           canSendFront, canSendRename,
           execEventSignal, execEventSignalDelayed, lateEventSignal, lateEventSetPC,
            robSending, robAccepting, renamedSending, commitAccepting,
            lsbrAccepting, lsbrAcceptingMore,
            issueQueuesAccepting, issueQueuesAcceptingMore, renameSendingBr, stopRename,
            queuesAccepting, queuesAcceptingMore, iqAcceptingI0, iqAcceptingM0, iqAcceptingF0, iqAcceptingS0, iqAcceptingSF0,
            robAcceptingMore, iqAcceptingMoreI0, iqAcceptingMoreM0, iqAcceptingMoreF0, iqAcceptingMoreS0, iqAcceptingMoreSF0,  iqAcceptingI0_NS, iqAcceptingMoreI0_NS,
            sbSending, sbEmpty, sysRegRead, sysRegSending, intSignal, committedSending: std_logic := '0';

    signal frontDataLastLiving, TMP_frontDataSpMasked,
            renamedDataLivingIntOut, renamedDataLivingMem, renamedDataLivingReMem, renamedDataLivingRe, renamedDataLivingMerged,
            dataOutROB, renamedDataToBQ, renamedDataToSQ, renamedDataToLQ, bqData, bpData, committedOut: 
                InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

        signal renamedArgsInt, renamedArgsFloat, renamedArgsMerged: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);

    signal bqCompare, bqCompareEarly, bqSelected, bqUpdate, sqValueInput, preAddressInput, sqSelectedOutput, memAddressInput, lqSelectedOutput,
           specialAction, specialOutROB, lastEffectiveOut, bqTargetData: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    
    signal bqPointer, bqPointerSeq, lqPointer, sqPointer, preIndexSQ, preIndexLQ: SmallNumber := (others => '0');
           
    signal commitGroupCtr: InsTag := (others => '0');
    signal newIntDests, newFloatDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal intType: std_logic_vector(0 to 1) := (others => '0');
    signal sysRegReadValue: Mword := (others => '0');
    signal sysRegReadSel: slv5 := (others => '0');

    signal execOutputs1, execOutputs2: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);    
    signal dataFromSB: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal cycleCounter: Word := (others => '0');
    
    signal events, eventsOnlyLate: EventState := ('0', '0', DEFAULT_INSTRUCTION_STATE, DEFAULT_INSTRUCTION_STATE);
    
    signal ch0, ch1, ch2, ch3, ch4: std_logic := '0';
begin

    MONITOR: process (clk)
    begin
        if rising_edge(clk) then
            cycleCounter <= addInt(cycleCounter, 1);
        end if;
    end process;

    intSignal <= int0 or int1;
    intType <= (int0, int1);

    events <= (lateEventSignal, execEventSignal, execCausing, preExecCausing);
    eventsOnlyLate <= (lateEvent => lateEventSignal, execEvent => '0', execCausing => DEFAULT_INS_STATE, preExecCausing => DEFAULT_INS_STATE);

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
        renameAccepting => renameAccepting,
        frontLastSendingIn => frontLastSending,
        frontDataLastLiving => frontDataLastLiving,
        
        TMP_spMaskedDataOut => TMP_frontDataSpMasked,
        
        nextAccepting => canSendRename,

        renamedDataLiving => renamedDataLivingIntOut,
        
        renamedArgsInt => renamedArgsInt,
        renamedArgsFloat => renamedArgsFloat,

        renamedSending => renamedSending,

        renamingBr => renameSendingBr,

        bqPointer => bqPointer,
        sqPointer => sqPointer,
        lqPointer => lqPointer,
            bqPointerSeq => bqPointerSeq,

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

    renamedDataLivingRe <= renamedDataLivingIntOut;

    canSendFront <= renameAccepting and not stopRename;
    canSendRename <= not stopRename;  --  Could also be : queuesAccepting;

    renamedDataToBQ <= setFullMask(renamedDataLivingRe, getBranchMask1(renamedDataLivingRe));
    renamedDataToSQ <= setFullMask(renamedDataLivingReMem, getStoreMask1(renamedDataLivingRe));
    renamedDataToLQ <= setFullMask(renamedDataLivingReMem, getLoadMask1(renamedDataLivingRe));
    

    renamedDataLivingMerged <= replaceDests(renamedDataLivingRe, renamedArgsMerged);

    lsbrAccepting <= robAccepting and acceptingSQ and acceptingLQ;
    lsbrAcceptingMore <= robAcceptingMore and not almostFullSQ and not almostFullLQ;
    
    issueQueuesAccepting <= iqAcceptingI0 and iqAcceptingM0 and iqAcceptingS0 and iqAcceptingF0 and iqAcceptingSF0;
    issueQueuesAcceptingMore <= iqAcceptingMoreI0 and iqAcceptingMoreM0 and iqAcceptingMoreS0 and iqAcceptingMoreF0 and iqAcceptingMoreSF0;
    
    queuesAccepting <= lsbrAccepting and issueQueuesAccepting;
    queuesAcceptingMore <= lsbrAcceptingMore and issueQueuesAcceptingMore;

    renamedDataLivingReMem <= TMP_recodeMem(renamedDataLivingRe);
   
    renamedArgsMerged <= mergeRenameInfoFP(renamedArgsInt, renamedArgsFloat);

	REORDER_BUFFER: entity work.ReorderBuffer(Behavioral)
	port map(
		clk => clk, reset => '0', en => '0',
		
		lateEventSignal => lateEventSignal,

		execEndSigs1 => execOutputs1,
		execEndSigs2 => execOutputs2,
		
		inputSpecial => specialAction,		
		inputData => renamedDataLivingMerged,
		prevSending => renamedSending,
		
		acceptingOut => robAccepting,
		acceptingMore => robAcceptingMore,
		
		nextAccepting => commitAccepting,
		sendingOut => robSending, 
		outputData => dataOutROB,
		outputSpecial => specialOutROB		
	);     


    TEMP_EXEC: block
       use work.LogicExec.all;
        
       -- Selection from IQ and state after Issue stage
       signal      slotSelI0, slotIssueI0, slotRegReadI0,      slotIssueI0_out, slotRegReadI0_out,
                    slotSelI1, slotIssueI1, slotRegReadI1,
                    slotSelM0, slotIssueM0, slotRegReadM0,      slotIssueM0_out, slotRegReadM0_out,
                    slotSelF0, slotIssueF0, slotRegReadF0,      slotIssueF0_out, slotRegReadF0_out,
               
                    slotSel4, slotIssue4, slotSel5, slotIssue5, slotSel6, slotIssue6,

                    slotSelIntSV, slotIssueIntSV, slotRegReadIntSV,         slotIssueIntSV_out, slotRegReadIntSV_out,
                    slotSelFloatSV, slotIssueFloatSV, slotRegReadFloatSV,   slotIssueFloatSV_out, slotRegReadFloatSV_out,
                    
                    dataToExecStoreValue
                        : SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

       -- Exec and later delay stages
       signal      slotI0_E0, slotI0_E1, slotI0_E2, slotI0_D0, slotI0_D1,
                    slotI1_E0, slotI1_E1, slotI1_E2, slotI1_D0, slotI1_D1,
                    slotM0_E0,
                    slotM0_E0i, slotM0_E1i, slotM0_E2i, slotM0_D0i, slotM0_D1i, -- Here logical stages get split into Int and FP
                    slotM0_E0f, slotM0_E1f, slotM0_E2f, slotM0_D0f, slotM0_D1f,

                    slotF0_E0, slotF0_E1, slotF0_E2, slotF0_D0, slotF0_D1,
                    slotDummy: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);                     

       signal NEW_ARR_DUMMY, newArrShared: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
       signal dataToIntWriteQueue, dataToFloatWriteQueue, dataToIntRF, dataToFloatRF: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
       signal resultToIntWQ, resultToFloatWQ, resultToIntRF, resultToFloatRF: ExecResult := DEFAULT_EXEC_RESULT;

       signal regsSelI0,           regsSelM0, regsSelS0, regsSelFloatA, regsSelFloatC, regsSelFS0, regsSelF0: PhysNameArray(0 to 2) := (others => (others => '0'));
       signal regValsI0, regValsB, regValsM0, regValsS0, regValsE, regValsFloatA, regValsFloatB, regValsFloatC, regValsFS0, regValsF0: MwordArray(0 to 2) := (others => (others => '0'));
       signal readyRegFlagsInt, readyRegFlagsInt_C, readyRegFlagsFloat, readyRegFlagsInt_T, readyRegFlagsFloat_T, readyRegFlagsIntNext, readyRegFlagsIntNext_C, readyRegFlagsSV, readyRegFlagsFloatNext, readyRegFlagsFloatSV: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
        
       signal newIntSources, newFloatSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
       
       -- Issue control 
       signal issuedStoreDataInt, issuedStoreDataFP, allowIssueStoreDataInt, allowIssueStoreDataFP, allowIssueStageStoreDataFP,
              memSubpipeSent, fp0subpipeSelected, lockIssueI0, allowIssueI0, lockIssueM0, allowIssueM0, lockIssueF0, allowIssueF0, memLoadReady, intWriteConflict,
              storeValueCollision1, storeValueCollision2, cancelledSVI1,
              sendingToStoreWrite, sendingToStoreWriteInt, sendingToStoreWriteFloat: std_logic := '0';

       signal sendingFromDLQ, sendingToAgu, sendingToRegReadI0, sendingToRegReadM0: std_logic := '0';       
       signal dataFromDLQ: InstructionState := DEFAULT_INSTRUCTION_STATE;
       
       signal outSigsI0, outSigsM0, outSigsSVI, outSigsSVF, outSigsF0: IssueQueueSignals := (others => '0');
       
       signal subpipeI0_Issue, subpipeI0_RegRead, subpipeI0_E0,                                                        subpipeI0_D0,
              subpipeM0_Issue, subpipeM0_RegRead, subpipeM0_E0,
                                                subpipeM0_E0i,  subpipeM0_E1i, subpipeM0_E2i, subpipeM0_E3i,         subpipeM0_D0i, subpipeM0_D1i,
                                                subpipeM0_E0f,  subpipeM0_E1f, subpipeM0_E2f, subpipeM0_E3f,         subpipeM0_D0f, subpipeM0_D1f,                                                              
              subpipeF0_Issue, subpipeF0_RegRead, subpipeF0_E0, subpipeF0_E1,subpipeF0_E2, subpipeF0_D0,
              subpipe_DUMMY
                : ExecResult := DEFAULT_EXEC_RESULT;
                
        signal unfoldedAluOp: work.LogicExec.AluControl := work.LogicExec.DEFAULT_ALU_CONTROL;     
        signal aluMask, memMask, fpMask, intStoreMask, fpStoreMask, branchMask, sqMask, lqMask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        signal fmaInt: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);
        signal fni, fniFloat, fniEmpty: ForwardingInfo := DEFAULT_FORWARDING_INFO;
        
                signal ch_a, ch_m, ch_si, ch_sf, ch_f: std_logic := '0';        
    begin
        newIntSources <= TMP_getPhysicalArgsNew(renamedArgsInt);
        newFloatSources <= TMP_getPhysicalArgsNew(renamedArgsFloat);
    
        aluMask <= getAluMask1(renamedDataLivingRe);
        memMask <= getMemMask1(renamedDataLivingRe);
        fpMask <= getFpMask1(renamedDataLivingRe);
        intStoreMask <= getIntStoreMask1((renamedDataLivingRe));
        fpStoreMask <= getFloatStoreMask1((renamedDataLivingRe));
        sqMask <= getStoreMask1((renamedDataLivingRe));
        lqMask <= getLoadMask1((renamedDataLivingRe));               
        branchMask <= getBranchMask1((renamedDataLivingRe));
        
        SUBPIPE_ALU: block
           signal dataToAlu, dataToBranch: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);           
           signal dataFromBranch: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
           signal branchData: InstructionState := DEFAULT_INSTRUCTION_STATE;           
           signal schedInfoA, schedInfoUpdatedA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => work.LogicIssue.DEFAULT_SCHEDULER_INFO);
        begin
            fmaInt <= work.LogicIssue.findForwardingMatchesArray(schedInfoA, fni);        
            schedInfoA <= work.LogicIssue.getIssueInfoArray(TMP_removeArg2(TMP_recodeALU(renamedDataLivingRe)), aluMask, true, removeArg2(renamedArgsInt));
            schedInfoUpdatedA <= work.LogicIssue.updateSchedulerArray(schedInfoA, fni, fmaInt, true, false, false, FORWARDING_MODES_INT_D, FORWARDING_MODES_INT_D);
              
            IQUEUE_I0: entity work.IssueQueue(Behavioral)
            generic map(
                IQ_SIZE => IQ_SIZE_I0,
                FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
                FORWARDING1(0 to 2) => FORWARDING_MODES_INT(0 to 2),
                FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2)
                    ,NONSHIFT => true
            )
            port map(
                clk => clk, reset => '0', en => '0',
        
                acceptingOut => iqAcceptingI0,
                acceptingMore => iqAcceptingMoreI0,
                
                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedA,
                fni => fni,
                readyRegFlags => readyRegFlagsInt,
                nextAccepting => allowIssueI0,
                events => events,
                schedulerOut => slotSelI0,
                outputSignals => outSigsI0
            );

            TMP_ISSUE_I0: block
                use work.LogicIssue.all;
                signal inputDataWithArgsI, inputDataWithArgsR, argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
                signal fullI, fullR: std_logic := '0';
            begin
                inputDataWithArgsI <= getDispatchArgValues_Is(slotSelI0.state, outSigsI0.sending);
                -- Reg
                slotIssueI0.full <= fullI;
                slotIssueI0.state <= updateDispatchArgs_Is(argStateI, fullI);
                -- pseudo interface
                sendingToRegReadI0 <= slotIssueI0.state.full and not outSigsI0.cancelled;
                inputDataWithArgsR <= getDispatchArgValues_RR(slotIssueI0.state, sendingToRegReadI0, fni, true, false);
                -- Reg
                slotRegReadI0.full <= fullR;
                slotRegReadI0.state <= updateDispatchArgs_RR(argStateR, fullR, fni.values0, regValsI0, false);

                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                            fullI <= outSigsI0.sending;
                        end if;

                        if events.lateEvent = '1' then
                            argStateI.full <= '0';
                            fullI <= '0';
                        end if;

                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                            fullR <= sendingToRegReadI0;
                            
                            unfoldedAluOp <= work.LogicExec.getAluControl(slotIssueI0.state.operation.arith);
                        end if;

                        if events.lateEvent = '1' then
                            argStateR.full <= '0';
                            fullR <= '0';
                        end if;

                    end if;
                end process;

                subpipeI0_Issue <= makeExecResult(slotIssueI0.state);
                subpipeI0_RegRead <= makeExecResult(slotRegReadI0.state);
            end block;


            -- NOTE: it seems that only elements of .state used in Exec are: { args: MwordArray; immediate: std_logic }
            dataToAlu(0) <= (slotRegReadI0.state.full and not outSigsI0.killSel2,
                                   executeAlu(--work.LogicIssue.TMP_restoreState(slotRegReadI0.full, slotRegReadI0.state).ins,
                                              work.LogicIssue.TMP_getIns(slotRegReadI0.state),
                                              slotRegReadI0.state, bqSelected.ins, branchData, unfoldedAluOp));
          
            STAGE_I0_E0: entity work.GenericStage2(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                input => dataToAlu(0),
                output => slotI0_E0(0),
                events => eventsOnlyLate
            );

            subpipeI0_E0 <= makeExecResult(slotI0_E0(0), slotI0_E0(0).full);

            branchData <= basicBranch(slotRegReadI0.state.full and not outSigsI0.killSel2 and slotRegReadI0.state.branchIns,
                                        --work.LogicIssue.TMP_restoreState(slotRegReadI0.full, slotRegReadI0.state).ins,
                                        work.LogicIssue.TMP_getIns(slotRegReadI0.state),
                                        slotRegReadI0.state, bqSelected.ins, unfoldedAluOp);                  
            
            dataToBranch(0) <= (slotRegReadI0.state.full and not outSigsI0.killSel2
                                                    and slotRegReadI0.state.branchIns, branchData);            
            bqCompare <= (dataToBranch(0).full, --work.LogicIssue.TMP_restoreState(slotRegReadI0.full, slotRegReadI0.state).ins);
                                                work.LogicIssue.TMP_getIns(slotRegReadI0.state));
            bqCompareEarly <= (sendingToRegReadI0 and slotIssueI0.state.branchIns, --work.LogicIssue.TMP_restoreState(slotIssueI0.full, slotIssueI0.state).ins);
                                                                                   work.LogicIssue.TMP_getIns(slotIssueI0.state));

            
            STAGE_I0_E0_BRANCH: entity work.GenericStage2(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                input => dataToBranch(0),
                output => dataFromBranch,
                events => eventsOnlyLate                  
            );
            
            execEventSignal <= dataFromBranch.ins.controlInfo.newEvent and dataFromBranch.full; -- TODO: remove 'full' here because is factored into 'newEvent'?
            execCausing <= clearDbCausing(dataFromBranch.ins);
            preExecCausing <= clearDbCausing(dataToBranch(0).ins);
            bqUpdate <= dataFromBranch; -- TODO: possibly redundant cause execCausing can carry the data
            
            DELAYED_EXEC_EVENT: process (clk)
            begin
                if rising_edge(clk) then
                    execEventSignalDelayed <= execEventSignal;
                    execCausingDelayed <= execCausing;
                end if;
            end process;
        end block;
         
            
        SUBPIPE_MEM: block
           signal dataToAgu, dataToAguInt, dataToAguFloat, dataOutMem0,
                         dataInMem0, dataInMemInt0, dataInMemFloat0, dataInMem1, dataInMemInt1, dataInMemFloat1: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);

           signal schedInfoA, schedInfoUpdatedA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1); 
           signal memLoadValue: Mword := (others => '0');                                                            
        begin
           schedInfoA <= work.LogicIssue.getIssueInfoArray(TMP_removeArg2(renamedDataLivingReMem), memMask, true, removeArg2(renamedArgsMerged));         
           schedInfoUpdatedA <= work.LogicIssue.updateSchedulerArray(schedInfoA, fni, fmaInt, true, false,  true, FORWARDING_MODES_INT_D, FORWARDING_MODES_NONE);
                        
		   IQUEUE_MEM: entity work.IssueQueue(Behavioral)--UnitIQ
           generic map(
               IQ_SIZE => IQ_SIZE_M0,
               ALT_INPUT => false,
               DONT_MATCH1 => true,
               FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
               FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2)
           )
           port map(
               clk => clk, reset => '0', en => '0',
       
               acceptingOut => iqAcceptingM0,
               acceptingMore => iqAcceptingMoreM0,
               prevSendingOK => renamedSending,
               newArr => schedInfoUpdatedA,
               --     newArr_Alt => newArrShared,
               fni => fni,
               readyRegFlags => readyRegFlagsInt,
               nextAccepting => allowIssueM0,
               events => events,
               schedulerOut => slotSelM0,
               outputSignals => outSigsM0
           );

            TMP_ISSUE_M0: block
                use work.LogicIssue.all;
                signal inputDataWithArgsI, inputDataWithArgsR, argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
                signal fullI, fullR: std_logic := '0';
            begin

                inputDataWithArgsI <= getDispatchArgValues1(TMP_prepareDispatchSlot(slotSelM0.state, outSigsM0.sending));

                slotIssueM0.full <= fullI;-- and nextAccepting;
                slotIssueM0.state <= updateDispatchArgs1(argStateI);

                subpipeM0_Issue <= makeExecResult(work.LogicIssue.TMP_restoreState(slotIssueM0.full, slotIssueM0.state), slotIssueM0.full);

                inputDataWithArgsR <= getDispatchArgValues2(TMP_prepareDispatchSlot(slotIssueM0.state, sendingToRegReadM0), fni, sendingToRegReadM0, true, false);
                sendingToRegReadM0 <= slotIssueM0.full and not outSigsM0.cancelled;

                slotRegReadM0.full <= fullR;-- and nextAccepting;
                slotRegReadM0.state <= updateDispatchArgs2(argStateR, fni.values0, regValsM0, false);
                
                subpipeM0_RegRead <= makeExecResult(work.LogicIssue.TMP_restoreState(slotRegReadM0.full, slotRegReadM0.state), slotRegReadM0.full);

                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                            fullI <= outSigsM0.sending;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullI <= '0';
                        end if;

                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                            fullR <= sendingToRegReadM0;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullR <= '0';
                        end if;
                                
                    end if;
                end process;

            end block;
                    

           preIndexSQ <= slotRegReadM0.state.sqPointer;
           preIndexLQ <= slotRegReadM0.state.lqPointer;
           preAddressInput <= (slotRegReadM0.full, work.LogicIssue.TMP_restoreState(slotRegReadM0.full, slotRegReadM0.state).ins);

           sendingFromDLQ <= '0';          -- TEMP!
           dataFromDLQ <= DEFAULT_INSTRUCTION_STATE; -- TEMP!

           sendingToAgu <= (slotRegReadM0.full and not outSigsM0.killSel2) or sendingFromDLQ;
	       dataToAgu(0) <= (sendingToAgu, calcEffectiveAddress(work.LogicIssue.TMP_restoreState(slotRegReadM0.full, slotRegReadM0.state).ins, slotRegReadM0.state, sendingFromDLQ, dataFromDLQ));
                            
           STAGE_AGU: entity work.GenericStage2(Behavioral)
           generic map(
               COMPARE_TAG => --'1'
                                '0'
           )
           port map(
               clk => clk, reset => reset, en => en,
               input => dataToAgu(0),
               output => slotM0_E0(0),
               events => events
           );
           
           subpipeM0_E0 <= makeExecResult(slotM0_E0(0), slotM0_E0(0).full);

           dataToAguInt <= clearFloatDest(dataToAgu);
           dataToAguFloat <= clearIntDest(dataToAgu);
                
           STAGE_AGU_INT: entity work.GenericStage2(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
               input => dataToAguInt(0),
               output => slotM0_E0i(0),
               events => events
           );
           
           subpipeM0_E0i <= makeExecResult(slotM0_E0i(0), slotM0_E0i(0).full);
           
           STAGE_AGU_FLOAT: entity work.GenericStage2(Behavioral)
           generic map(
               COMPARE_TAG => --'1'
                                '0'
           )
           port map(
               clk => clk, reset => reset, en => en,
               input => dataToAguFloat(0),
               output => slotM0_E0f(0),
               events => events
           );
           
           subpipeM0_E0f <= makeExecResult(slotM0_E0f(0), slotM0_E0f(0).full);

	       dataInMem0(0) <= (slotM0_E0(0).full, slotM0_E0(0).ins);
           memAddressInput <= dataInMem0(0);

           dataInMemInt0 <= clearFloatDest(dataInMem0);
           dataInMemFloat0 <= clearIntDest(dataInMem0);

           -- TLB lookup, Dcache access
	       STAGE_MEM0: entity work.GenericStage2(Behavioral)
           generic map(
               COMPARE_TAG => --'1'
                                '0'
           )
           port map(
               clk => clk, reset => reset, en => en,
               input => dataInMemInt0(0),
               output => slotM0_E1i(0),
               events => events     
           );

           subpipeM0_E1i <= makeExecResult(slotM0_E1i(0), slotM0_E1i(0).full);

	       STAGE_MEM0_FLOAT: entity work.GenericStage2(Behavioral)
           generic map(
               COMPARE_TAG => --'1'
                                '0'
           )
           port map(
               clk => clk, reset => reset, en => en,
               input => dataInMemFloat0(0),
               output => slotM0_E1f(0),               
               events => events              
           );

           subpipeM0_E1f <= makeExecResult(slotM0_E1f(0), slotM0_E1i(0).full);
                      
           dataOutMem0(0) <= mergePhysDests(slotM0_E1i(0), slotM0_E1f(0)); -- [dest := Int.dest | Float.dest];

           dataInMem1(0).full <= slotM0_E1i(0).full;
           dataInMem1(0).ins <= getLSResultData(dataOutMem0(0).ins,
                                                  '1', (others => '0'),
                                                  memLoadReady, memLoadValue,
                                                  sysRegSending, sysRegReadValue,
                                                  sqSelectedOutput, lqSelectedOutput);

           dataInMemInt1 <= clearFloatDest(dataInMem1); -- with zeroed dest when load is FP
           dataInMemFloat1 <= clearIntDest(dataInMem1); -- with zeroed dest when load is Int??
                                                  	       
           -- Source selection and verification
	       STAGE_MEM1: entity work.GenericStage2(Behavioral)
           generic map(
               COMPARE_TAG => --'1'
                                '0'
           )
           port map(
               clk => clk, reset => reset, en => en,
               input => dataInMemInt1(0),
               output => slotM0_E2i(0),
               events => events             
           );

           subpipeM0_E2i <= makeExecResult(slotM0_E2i(0), slotM0_E2i(0).full);
           
           -- Branching into FP cluster
           STAGE_MEM1_FLOAT: entity work.GenericStage2(Behavioral)
           generic map(
               COMPARE_TAG => --'1'
                                '0'
           )
           port map(
               clk => clk, reset => reset, en => en,
               input => dataInMemFloat1(0),
               output => slotM0_E2f(0),
               events => events              
           );

           subpipeM0_E2f <= makeExecResult(slotM0_E2f(0), slotM0_E2f(0).full);

           -- TEMP mem interface    
		   dread <= slotM0_E0(0).full;
           dadr <= slotM0_E0(0).ins.result;
           sysRegReadSel <= slotM0_E0(0).ins.result(4 downto 0);
           sysRegRead <= slotM0_E0(0).full and isLoadSysOp(slotM0_E0(0).ins);
           
           memLoadReady <= dvalid;              
           memLoadValue <= din;      
        end block;

        ------------------------
        readyRegFlagsSV <= (readyRegFlagsInt(2), '0', '0', readyRegFlagsInt(5), '0', '0', readyRegFlagsInt(8), '0', '0', readyRegFlagsInt(11), '0', '0');

        SUBPIPES_STORE_VALUE: block
            signal fmaIntSV, fmaFloatSV: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);
            signal sendingToRegReadI, sendingToRegReadF: std_logic := '0';
            signal schedInfoIntA, schedInfoUpdatedIntA, schedInfoFloatA, schedInfoUpdatedFloatA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1);
        begin
            -- CHECK: does it need to use 'sentCancelled' signal from IQs?
            schedInfoIntA <= work.LogicIssue.getIssueInfoArray(prepareForStoreValueIQ(renamedDataLivingReMem), intStoreMask, false, useStoreArg2(renamedArgsInt));
            schedInfoUpdatedIntA <= work.LogicIssue.updateSchedulerArray(schedInfoIntA, fni, fmaIntSV, true, false, true, FORWARDING_MODES_SV_INT_D, FORWARDING_MODES_SV_INT_D);

            schedInfoFloatA <= work.LogicIssue.getIssueInfoArray(prepareForStoreValueFloatIQ(renamedDataLivingReMem), fpStoreMask, false, useStoreArg2(renamedArgsFloat));
            schedInfoUpdatedFloatA <= work.LogicIssue.updateSchedulerArray(schedInfoFloatA, fni, fmaFloatSV, true, false, true, FORWARDING_MODES_SV_FLOAT_D, FORWARDING_MODES_SV_FLOAT_D);

            fmaIntSV <= work.LogicIssue.findForwardingMatchesArray(schedInfoIntA, fni);
            fmaFloatSV <= work.LogicIssue.findForwardingMatchesArray(schedInfoFloatA, fniFloat);
        
            IQUEUE_SV: entity work.IssueQueue(Behavioral)
            generic map(
                IQ_SIZE => IQ_SIZE_INT_SV,
                FORWARDING_D(0 to 2) => FORWARDING_MODES_SV_INT_D(0 to 2)
            )
            port map(
                clk => clk, reset => '0', en => '0',
        
                acceptingOut => iqAcceptingS0,
                acceptingMore => iqAcceptingMoreS0,
                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedIntA,
                --    newArr_Alt => NEW_ARR_DUMMY,
                fni => fni,     
                readyRegFlags => readyRegFlagsSV,
                nextAccepting => allowIssueStoreDataInt,
                events => events,
                schedulerOut => slotSelIntSV,
                outputSignals => outSigsSVI
            );


            TMP_ISSUE_SVI: block
                use work.LogicIssue.all;
                signal inputDataWithArgsI, inputDataWithArgsR, argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
                signal fullI, fullR: std_logic := '0';
            begin

                inputDataWithArgsI <= getDispatchArgValues1(TMP_prepareDispatchSlot(slotSelIntSV.state, outSigsSVI.sending));

                slotIssueIntSV.full <= fullI;-- and nextAccepting;
                slotIssueIntSV.state <= updateDispatchArgs1(argStateI);

                cancelledSVI1 <= outSigsSVI.cancelled or (storeValueCollision2 and outSigsSVI.killSel2); -- If stalled, it stayed here but kill sig moved to next stage


                inputDataWithArgsR <= getDispatchArgValues2(TMP_prepareDispatchSlot(slotIssueIntSV.state, sendingToRegReadI), fni, sendingToRegReadI, false, true);
                sendingToRegReadI <= slotIssueIntSV.full and not cancelledSVI1;

                slotRegReadIntSV.full <= fullR;-- and nextAccepting;
                slotRegReadIntSV.state <= updateDispatchArgs2(argStateR, fni.values0, regValsS0, true);


                process (clk)
                begin
                    if rising_edge(clk) then
                        if allowIssueStoreDataInt = '1' then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                            fullI <= outSigsSVI.sending;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullI <= '0';
                        end if;

                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                            fullR <= sendingToRegReadI;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullR <= '0';
                        end if;
                                
                    end if;
                end process;

            end block;

            sendingToStoreWriteInt <= slotRegReadIntSV.full and not outSigsSVI.killSel2;
   
            ------------------------------------
            readyRegFlagsFloatSV <= (readyRegFlagsFloat(2), '0', '0', readyRegFlagsFloat(5), '0', '0', readyRegFlagsFloat(8), '0', '0', readyRegFlagsFloat(11), '0', '0');
                      
            IQUEUE_FLOAT_SV: entity work.IssueQueue(Behavioral)
            generic map(
                IQ_SIZE => IQ_SIZE_FLOAT_SV, -- CAREFUL: not IS_FP because doesn't have destination
                FORWARDING_D(0 to 2) => FORWARDING_MODES_SV_FLOAT_D(0 to 2)
            )
            port map(
                clk => clk, reset => '0', en => '0',
       
                acceptingOut => iqAcceptingSF0,
                acceptingMore => iqAcceptingMoreSF0,
                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedFloatA,
                --    newArr_Alt => NEW_ARR_DUMMY,                
                fni => fniFloat,      
                readyRegFlags => readyRegFlagsFloatSV,
                nextAccepting => allowIssueStoreDataFP,
                events => events,
                schedulerOut => slotSelFloatSV,              
                outputSignals => outSigsSVF
            );       

            TMP_ISSUE_SVF: block
                use work.LogicIssue.all;
                signal inputDataWithArgsI, inputDataWithArgsR, argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
                signal fullI, fullR: std_logic := '0';
            begin
                inputDataWithArgsI <= getDispatchArgValues1(TMP_prepareDispatchSlot(slotSelFloatSV.state, outSigsSVF.sending));

                slotIssueFloatSV.full <= fullI;-- and nextAccepting;
                slotIssueFloatSV.state <= updateDispatchArgs1(argStateI);

                inputDataWithArgsR <= getDispatchArgValues2(TMP_prepareDispatchSlot(slotIssueFloatSV.state, sendingToRegReadF), fni, sendingToRegReadF, false, true);
                sendingToRegReadF <= slotIssueFloatSV.full and not outSigsSVF.cancelled;

                slotRegReadFloatSV.full <= fullR;-- and nextAccepting;
                slotRegReadFloatSV.state <= updateDispatchArgs2(argStateR, fni.values0, regValsFS0, true);

                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                            fullI <= outSigsSVF.sending;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullI <= '0';
                        end if;

                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                            fullR <= sendingToRegReadF;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullR <= '0';
                        end if;

                    end if;
                end process;

            end block;

            sendingToStoreWriteFloat <= slotRegReadFloatSV.full and not outSigsSVF.killSel2;
           
            dataToExecStoreValue <= getStoreDataOp(work.LogicIssue.TMP_restoreState(slotRegReadFloatSV.full, slotRegReadFloatSV.state)) when slotRegReadFloatSV.full = '1'
                            else    getStoreDataOp(work.LogicIssue.TMP_restoreState(slotRegReadIntSV.full, slotRegReadIntSV.state));
                
            sendingToStoreWrite <= sendingToStoreWriteInt or sendingToStoreWriteFloat;
        end block;

        
        SUBPIPE_FP0: block
            signal dataToFpu0: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
            signal fmaF0: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);          
            signal schedInfoA, schedInfoUpdatedA: work.LogicIssue.SchedulerInfoArray(0 to PIPE_WIDTH-1);
            signal sendingToRegReadF0: std_logic := '0';
        begin
            fmaF0 <= work.LogicIssue.findForwardingMatchesArray(schedInfoA, fniFloat);

            schedInfoA <= work.LogicIssue.getIssueInfoArray(TMP_recodeFP(renamedDataLivingRe), fpMask, false, renamedArgsFloat);
            schedInfoUpdatedA <= work.LogicIssue.updateSchedulerArray(schedInfoA, fniFloat, fmaF0, true, false, false, FORWARDING_MODES_FLOAT_D, FORWARDING_MODES_FLOAT_D);

            IQUEUE_F0: entity work.IssueQueue(Behavioral)
            generic map(
                IQ_SIZE => IQ_SIZE_F0, IS_FP => true,
                FORWARDING(0 to 2) => FORWARDING_MODES_FLOAT(0 to 2),
                FORWARDING1(0 to 2) => FORWARDING_MODES_FLOAT(0 to 2),
                FORWARDING_D(0 to 2) => FORWARDING_MODES_FLOAT_D(0 to 2)
            )
            port map(
                clk => clk, reset => '0', en => '0',
        
                acceptingOut => iqAcceptingF0,
                acceptingMore => iqAcceptingMoreF0,
                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedA,
                --    newArr_Alt => NEW_ARR_DUMMY,                
                fni => fniFloat,
                readyRegFlags => readyRegFlagsFloat,
                nextAccepting => allowIssueF0,
                events => events,
                schedulerOut => slotSelF0,              
                outputSignals => outSigsF0
            );
           

            TMP_ISSUE_F0: block
                use work.LogicIssue.all;
                signal inputDataWithArgsI, inputDataWithArgsR, argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
                signal fullI, fullR: std_logic := '0';
            begin
                inputDataWithArgsI <= getDispatchArgValues1(TMP_prepareDispatchSlot(slotSelF0.state, outSigsF0.sending));

                slotIssueF0.full <= fullI;-- and nextAccepting;
                slotIssueF0.state <= updateDispatchArgs1(argStateI);

                subpipeF0_Issue <= makeExecResult(work.LogicIssue.TMP_restoreState(slotIssueF0.full, slotIssueF0.state), slotIssueF0.full);


                inputDataWithArgsR <= getDispatchArgValues2(TMP_prepareDispatchSlot(slotIssueF0.state, sendingToRegReadF0), fni, sendingToRegReadF0, false, false);
                sendingToRegReadF0 <= slotIssueF0.full and not outSigsF0.cancelled;

                slotRegReadF0.full <= fullR;-- and nextAccepting;
                slotRegReadF0.state <= updateDispatchArgs2(argStateR, fni.values0, regValsF0, false);
                    
                subpipeF0_RegRead <= makeExecResult(work.LogicIssue.TMP_restoreState(slotRegReadF0.full, slotRegReadF0.state), slotRegReadF0.full);

                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                            fullI <= outSigsF0.sending;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullI <= '0';
                        end if;

                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                            fullR <= sendingToRegReadF0;
                        end if;
                        
                        if events.lateEvent = '1' then
                            fullR <= '0';
                        end if;
                                
                    end if;
                end process;

            end block;


          
            dataToFpu0(0) <= (slotRegReadF0.full and not outSigsF0.killSel2, executeFpu(work.LogicIssue.TMP_restoreState(slotRegReadF0.full, slotRegReadF0.state).ins, slotRegReadF0.state));

            STAGE_F0_E0: entity work.GenericStage2(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                input => dataToFpu0(0),
                output => slotF0_E0(0),
                events => eventsOnlyLate
            );     

            subpipeF0_E0 <= makeExecResult(slotF0_E0(0), slotF0_E0(0).full);

            STAGE_F0_E1: entity work.GenericStage2(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                input => slotF0_E0(0),
                output => slotF0_E1(0),  
                events => eventsOnlyLate
            );
            
            subpipeF0_E1 <= makeExecResult(slotF0_E1(0), slotF0_E1(0).full);

            STAGE_F0_E2: entity work.GenericStage2(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                input => slotF0_E1(0),
                output => slotF0_E2(0),
                events => eventsOnlyLate
            );
            
            subpipeF0_E2 <= makeExecResult(slotF0_E2(0), slotF0_E2(0).full);
  
         end block;
         
         sqValueInput <= -- CAREFUL: This implies that integer StoreData op value is lost when Int and FP are issued simultaneously. This must be prevented by scheduler!
                        (sendingToStoreWrite, setInstructionResult(dataToExecStoreValue.ins, dataToExecStoreValue.state.args(0)));
         
         -- StoreData issue control:
         -- When Int and FP store data issue at the same time, the port conflict is resolved thus:
         -- Both IQs are blocked for the next cycle, so combined issue rate is never higher that 1 per cycle
         -- Int op is stalled for 1 cycle at IssueStage - no problems appear with scheduling because this subpipe has no wakeup observers and reads ags only form RF 
         process (clk)
         begin
            if rising_edge(clk) then
                issuedStoreDataInt <= outSigsSVI.sending;
                issuedStoreDataFP <= outSigsSVF.sending;
                
                    storeValueCollision2 <= storeValueCollision1;
            end if;
         end process;

         storeValueCollision1 <= (issuedStoreDataInt and issuedStoreDataFP);

         allowIssueStoreDataInt <= not storeValueCollision1;
         allowIssueStoreDataFP <= not storeValueCollision1;

         -------------------------------------------
         TMP_EXEC_D0: process (clk)
         begin
            if rising_edge(clk) then
                 subpipeI0_D0 <= subpipeI0_E0;

                 subpipeM0_D0i <= subpipeM0_E2i;
                 subpipeM0_D0f <= subpipeM0_E2f;
                 subpipeM0_D1f <= subpipeM0_D0f;

                 subpipeF0_D0 <= subpipeF0_E2;
             end if;
         end process;

         intWriteConflict <= subpipeI0_E0.full and subpipeM0_E2i.full;
            
         SCHED_BLOCK: process(clk)
         begin
             if rising_edge(clk) then
                 assert intWriteConflict = '0' report "Int write queue conflict!" severity error;

                 memSubpipeSent <= sendingToRegReadM0;
                 fp0subpipeSelected <= outSigsF0.sending;
             end if;
         end process;

         lockIssueI0 <= memSubpipeSent;
         allowIssueI0 <= not lockIssueI0;

         -- TODO: issue locking for F0 subpipe - avoid WB collisions with FP load!
         lockIssueM0 <= fp0subpipeSelected;
         allowIssueM0 <= not lockIssueM0;

         lockIssueF0 <= '0';
         allowIssueF0 <= not lockIssueF0;

         execOutputs1(0) <= (slotI0_E0(0).full, slotI0_E0(0).ins);
         execOutputs1(2) <= mergePhysDests(slotM0_E2i(0), slotM0_E2f(0)); --  [dest := Int.dest | Float.dest];             
         execOutputs1(3) <= (slotF0_E2(0).full, slotF0_E2(0).ins);
            
         -- TODO: include mem hit in 'full' flag! Should merge some info from Float path??
         execOutputs2(2) <= (dataToExecStoreValue.full, dataToExecStoreValue.ins);

         fni <= buildForwardingNetwork(   DEFAULT_EXEC_RESULT, subpipeI0_Issue,     subpipeI0_RegRead,   subpipeI0_E0,        subpipeI0_D0,
                                          DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT,
                                          subpipeM0_RegRead,   subpipeM0_E0i,       subpipeM0_E1i,       subpipeM0_E2i,       subpipeM0_D0i
                                        );
    
         fniFloat <= buildForwardingNetworkFP( subpipeF0_RegRead,   subpipeF0_E0,        subpipeF0_E1,        subpipeF0_E2,        subpipeF0_D0,
                                               DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT,
                                               subpipeM0_E0f,       subpipeM0_E1f,       subpipeM0_E2f,       subpipeM0_D0f,       subpipeM0_D1f                                        
                                            );

         regsSelI0 <= work.LogicRenaming.getPhysicalArgs(slotIssueI0);
         regsSelM0 <= work.LogicRenaming.getPhysicalArgs(slotIssueM0);        
         -- TEMP!
         regsSelS0 <= work.LogicRenaming.getPhysicalArgs(slotIssueIntSV);              
         regsSelFS0 <= work.LogicRenaming.getPhysicalArgs(slotIssueFloatSV);
         
         regsSelF0 <= work.LogicRenaming.getPhysicalArgs(slotIssueF0);


         resultToIntWQ <= subpipeM0_E2i when subpipeM0_E2i.full = '1' else subpipeI0_E0;
            
         TMP_WQ: process (clk)
         begin   
            if rising_edge(clk) then
                resultToIntRF <= resultToIntWQ;
                resultToFloatRF <= resultToFloatWQ;
            end if;
         end process;
            
		 INT_REG_FILE: entity work.RegFile(Behavioral)
         generic map(WIDTH => 4, WRITE_WIDTH => 1)
         port map(
             clk => clk, reset => '0', en => '0',
                 
             writeAllow => '1',--dataToIntRF(0).full,
             writeInput => dataToIntRF,
                writeInput_T(0) => resultToIntRF,
                
             readAllowVec => (others => '1'), -- TEMP!
             
             selectRead(0 to 2) => regsSelI0,
             selectRead(3 to 5) => (others => (others => '0')),
             selectRead(6 to 8) => regsSelM0,
             selectRead(9 to 11) => regsSelS0,
             
             readValues(0 to 2) => regValsI0,
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
             newPhysDests => newIntDests,
             newPhysSources => newIntSources,
             writingMask(0) => dataToIntRF(0).full,
             writingData(0) => dataToIntRF(0).ins,
                writingData_T(0) => resultToIntRF,
             readyRegFlagsNext => readyRegFlagsIntNext
         );

         resultToFloatWQ <= subpipeM0_E2f when subpipeM0_E2f.full = '1' else subpipeF0_E2;

		 FLOAT_REG_FILE: entity work.RegFile(Behavioral)
         generic map(IS_FP => true, WIDTH => 4, WRITE_WIDTH => 1)
         port map(
             clk => clk, reset => '0', en => '0',
                 
             writeAllow => '1',--dataToFloatRF(0).full,
             writeInput => dataToFloatRF,
                writeInput_T(0) => resultToFloatRF,
 
             readAllowVec => (others => '1'),
             
             selectRead(0 to 2) => regsSelF0,
             selectRead(3 to 5) => (others => (others => '0')),
             selectRead(6 to 8) => (others => (others => '0')),
             selectRead(9 to 11) => regsSelFS0,
             
             readValues(0 to 2) => regValsF0,
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
             newPhysDests => newFloatDests,
             newPhysSources => newFloatSources,    
             writingMask(0) => dataToFloatRF(0).full,  
             writingData(0) => dataToFloatRF(0).ins,
                writingData_T(0) => resultToFloatRF,
             readyRegFlagsNext => readyRegFlagsFloatNext
         );
     
         SRC_LATE_OVERRIDE: if true or TMP_PARAM_LATE_SRC_DEP_OVERRIDE generate
              readyRegFlagsInt_T <= updateArgStates(renamedArgsInt, renamedArgsFloat, readyRegFlagsIntNext);
               --         readyRegFlagsInt_C <= updateArgStates(renamedDataLivingRe_C, renamedArgsInt, renamedArgsFloat, readyRegFlagsIntNext_C);
              readyRegFlagsFloat_T <= updateArgStatesFloat(renamedArgsInt, renamedArgsFloat, readyRegFlagsFloatNext);
         end generate;
         
              readyRegFlagsInt <= readyRegFlagsIntNext    ;-- and not groupDependencyFlags;
              readyRegFlagsFloat <= readyRegFlagsFloatNext;-- and not groupDependencyFlags;
         
       sysRegSending <= sysRegRead;
     
    end block; -- TEMP_EXEC


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
		
		prevSending => renamedSending,
	    prevSendingBr => bpSending,
	    
	    prevSendingRe => renameSendingBr,
	    
	    renamedPtr => bqPointerSeq,
	       
	    bqPtrOut => bqPointer,
	    
	    dataInRe => TMP_frontDataSpMasked,
		dataIn => renamedDataToBQ,
		dataInBr => bpData,

		storeValueInput => bqUpdate,
		compareAddressInput => bqCompare,
        compareAddressQuickInput => bqCompareEarly,

		selectedDataOutput => bqSelected,

		committing => robSending, -- When ROB is sending so is BQ if it has corresponding branches
		robData => dataOutROB,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalDelayed,
		execCausing => execCausingDelayed,
		nextAccepting => commitAccepting,		
		sendingSQOut => open,
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
		prevSending => renamedSending,
		
		dataInRe => TMP_frontDataSpMasked,
		dataIn => renamedDataToSQ,
            
        renamedPtr => sqPointer,
            
		storeValueInput => sqValueInput, 
		compareAddressInput => memAddressInput,
        compareIndexInput => preIndexSQ,
        preCompareAddressInput => preAddressInput,
            
		selectedDataOutput => sqSelectedOutput,

		committing => robSending,
		robData => dataOutROB,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalDelayed,
        execCausing => execCausingDelayed,
		
		nextAccepting => commitAccepting,

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
		prevSending => renamedSending,
		
		dataInRe => TMP_frontDataSpMasked,
		dataIn => renamedDataToLQ,

        renamedPtr => lqPointer,

		storeValueInput => DEFAULT_INSTRUCTION_SLOT, 
		compareAddressInput => memAddressInput,
        compareIndexInput => preIndexLQ,        
        preCompareAddressInput => preAddressInput,
             
		selectedDataOutput => lqSelectedOutput,

		committing => robSending,
		robData => dataOutROB,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalDelayed,
        execCausing => execCausingDelayed,
		
		nextAccepting => commitAccepting,
		
        committedEmpty => open,
        committedSending => open,
        committedDataOut => open
	);

	MEMORY_INTERFACE: block
		signal sysStoreAddressW: Mword := (others => '0');
	begin
		doutadr <= dataFromSB(0).ins.target;
		dwrite <= sbSending and dataFromSB(0).full and isStoreMemOp(dataFromSB(0).ins);
		dout <= dataFromSB(0).ins.result;
	end block;
	
end Behavioral;
