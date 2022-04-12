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
            robAcceptingMore, iqAcceptingMoreI0, iqAcceptingMoreM0, iqAcceptingMoreF0, iqAcceptingMoreS0, iqAcceptingMoreSF0,
            sbSending, sbEmpty, sysRegRead, sysRegSending, intSignal
            : std_logic := '0';

    signal frontDataLastLiving, bpData, TMP_frontDataSpMasked,
          renamedDataLivingReMem, renamedDataLivingRe, renamedDataLivingMerged, renamedDataToBQ, renamedDataToSQ, renamedDataToLQ,
            dataOutROB: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal dataFromSB: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal bqSelected, preAddressInput, sqSelectedOutput, memAddressInput, lqSelectedOutput, specialAction: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;

    signal renamedArgsInt, renamedArgsFloat, renamedArgsMerged: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);

    signal bqPointer, bqPointerSeq, lqPointer, sqPointer, preIndexSQ, preIndexLQ: SmallNumber := (others => '0');

    signal commitGroupCtr: InsTag := (others => '0');
    signal newIntDests, newFloatDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal intType: std_logic_vector(0 to 1) := (others => '0');
    signal sysRegReadValue: Mword := (others => '0');
    signal sysRegReadSel: slv5 := (others => '0');

    signal execOutMain, execOutSec: ExecResultArray(0 to 3) := (others => DEFAULT_EXEC_RESULT);

    signal cycleCounter: Word := (others => '0');
    
    signal events: EventState := ('0', '0', DEFAULT_INSTRUCTION_STATE, DEFAULT_INSTRUCTION_STATE);

    signal preAddressOp, specialOutROB_N: SpecificOp := DEFAULT_SPECIFIC_OP;

    signal branchCtrl, memoryCtrl: InstructionControlInfo := DEFAULT_CONTROL_INFO;

           signal bqUpdate, bqCompareEarly, sqValueResult,      
                    frontEvent_N, execEvent_N, execResultDelayed, lateEvent_N, bqTargetData_N, bqTargetData_N2, dataFromSB_N,
                    execCausingDelayedSQ, execCausingDelayedLQ: ExecResult := DEFAULT_EXEC_RESULT;
    
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

        execEvent_N <= (execEventSignal, InsTag'(others => '0'), execCausing.tags.bqPointerSeq, execCausing.target);
        dataFromSB_N <= (dataFromSB(0).full and isStoreSysOp(dataFromSB(0).ins), InsTag'(others => '0'),
                                    zeroExtend(dataFromSB(0).ins.target(4 downto 0), SMALL_NUMBER_SIZE), dataFromSB(0).ins.result);

	UNIT_SEQUENCER: entity work.UnitSequencer(Behavioral)
	generic map(DEBUG_FILE_PREFIX => DEBUG_FILE_PREFIX)
    port map (
        clk => clk, reset => reset, en => '0',
        
        -- sys reg interface
        sysRegReadSel => sysRegReadSel,
        sysRegReadValue => sysRegReadValue,

        -- to front pipe
        pcDataLiving => pcDataSig,
        pcSending => pcSending,
        
        intAllowOut => intallow,
        intAckOut => intack,
        intRejOut => open,--
        -- Events in
        intSignal => intSignal,
        intType => intType,
        execEventSignal => execEventSignal,        
        frontEventSignal => frontEventSignal,        
             frontEvent_N => frontEvent_N,
             execEvent_N => execEvent_N,
                     
        -- Events out
        lateEventOut => lateEventSignal,
        lateEventSetPC => lateEventSetPC,
             lateEvent_N => lateEvent_N,

        -- Interface from ROB
        commitAccepting => commitAccepting,
        sendingFromROB => robSending,    
        robDataLiving => dataOutROB,
            robSpecial_N => specialOutROB_N,
        ---
            bqTargetData_N => bqTargetData_N,
            
        sbSending => sbSending,
            dataFromSB_N => dataFromSB_N,
        sbEmpty => sbEmpty,

        commitGroupCtrOut => commitGroupCtr,

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
            frontCausing_N => frontEvent_N,

            execCausing_N => execEvent_N,
            lateCausing_N => lateEvent_N,
        
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal,
        lateEventSetPC => lateEventSetPC
    );    
            lateCausing.target <= lateEvent_N.value;
            
    REGISTER_MANAGER: entity work.UnitRegManager(Behavioral)
    port map(
        clk => clk,
        renameAccepting => renameAccepting,
        frontLastSendingIn => frontLastSending,
        frontDataLastLiving => frontDataLastLiving,
        
        TMP_spMaskedDataOut => TMP_frontDataSpMasked,
        
        nextAccepting => canSendRename,

        renamedDataLiving => renamedDataLivingRe,
        
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

		  execSigsMain => execOutMain,
		  execSigsSec => execOutSec,

		  branchControl => branchCtrl,
		  memoryControl => memoryCtrl,

		  specialOp => specialAction.ins.specificOperation,
			
		inputData => renamedDataLivingMerged,
		prevSending => renamedSending,
		
		acceptingOut => robAccepting,
		acceptingMore => robAcceptingMore,
		
		nextAccepting => commitAccepting,
		sendingOut => robSending, 
		outputData => dataOutROB,
		  outputSpecial_N => specialOutROB_N		
	);     


    TEMP_EXEC: block
       use work.LogicExec.all;
        
       -- Selection from IQ and state after Issue stage
       signal      slotSelI0, slotIssueI0, slotRegReadI0,
                    slotSelI1, slotIssueI1, slotRegReadI1,
                    slotSelM0, slotIssueM0, slotRegReadM0,
                    slotSelF0, slotIssueF0, slotRegReadF0,
               
                    slotSel4, slotIssue4, slotSel5, slotIssue5, slotSel6, slotIssue6,

                    slotSelIntSV, slotIssueIntSV, slotRegReadIntSV,
                    slotSelFloatSV, slotIssueFloatSV, slotRegReadFloatSV
                    
                        : SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

       -- Exec and later delay stages
       signal      slotI0_E0, slotI0_E1, slotI0_E2, slotI0_D0, slotI0_D1,
                    slotI1_E0, slotI1_E1, slotI1_E2, slotI1_D0, slotI1_D1,
                    slotM0_E0,
                    slotM0_E0i, slotM0_E1i, slotM0_E2i, slotM0_D0i, slotM0_D1i, -- Here logical stages get split into Int and FP
                    slotM0_E0f, slotM0_E1f, slotM0_E2f, slotM0_D0f, slotM0_D1f,

                    slotF0_E0, slotF0_E1, slotF0_E2, slotF0_D0, slotF0_D1,
                    slotDummy: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);                     

       signal resultToIntWQ, resultToFloatWQ, resultToIntRF, resultToFloatRF: ExecResult := DEFAULT_EXEC_RESULT;

       signal regsSelI0,           regsSelM0, regsSelS0, regsSelFloatA, regsSelFloatC, regsSelFS0, regsSelF0: PhysNameArray(0 to 2) := (others => (others => '0'));
       signal regValsI0, regValsB, regValsM0, regValsS0, regValsE, regValsFloatA, regValsFloatB, regValsFloatC, regValsFS0, regValsF0: MwordArray(0 to 2) := (others => (others => '0'));
       signal readyRegFlagsInt, readyRegFlagsInt_C, readyRegFlagsFloat, readyRegFlagsInt_T, readyRegFlagsFloat_T,
                readyRegFlagsIntNext, readyRegFlagsIntNext_C, readyRegFlagsSV, readyRegFlagsFloatNext, readyRegFlagsFloatSV: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
        
       signal newIntSources, newFloatSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
       
       -- Issue control 
       signal issuedStoreDataInt, issuedStoreDataFP, allowIssueStoreDataInt, allowIssueStoreDataFP, allowIssueStageStoreDataFP,
              memSubpipeSent, fp0subpipeSelected, lockIssueI0, allowIssueI0, lockIssueM0, allowIssueM0, lockIssueF0, allowIssueF0, memLoadReady, intWriteConflict,
              storeValueCollision1, storeValueCollision2, cancelledSVI1,
              sendingToStoreWrite, sendingToStoreWriteInt, sendingToStoreWriteFloat: std_logic := '0';

       signal sendingFromDLQ, sendingBranchRR, sendingToAgu, sendingToRegReadI0, sendingToRegReadM0: std_logic := '0';  -- MOVE to subpipes     
       signal dataFromDLQ: InstructionState := DEFAULT_INSTRUCTION_STATE;

           signal dataFromBranch: InstructionSlot := DEFAULT_INSTRUCTION_SLOT; -- CLEANUP
        signal stateExecStoreValue: SchedulerState := DEFAULT_SCHED_STATE;
       
       signal outSigsI0, outSigsM0, outSigsSVI, outSigsSVF, outSigsF0: IssueQueueSignals := (others => '0');
       
       signal subpipeI0_Issue, subpipeI0_RegRead, subpipeI0_E0,                                          subpipeI0_D0,
                                   subpipeI0_RegRead_u,
                                   subpipeI0_RegRead_b, subpipeI0_E0_b,

              subpipeM0_Issue, subpipeM0_RegRead, subpipeM0_E0,    subpipeM0_E1,    subpipeM0_E2,
                                subpipeM0_RR, subpipeM0_RR_u,
                                   subpipeM0_RRi, subpipeM0_E0i, subpipeM0_E1i,   subpipeM0_E2i,   subpipeM0_D0i, subpipeM0_D1i,
                                   subpipeM0_RRf, subpipeM0_E0f, subpipeM0_E1f,   subpipeM0_E2f,   subpipeM0_D0f, subpipeM0_D1f,
                                                                            subpipeM0_E1_u,
                                                                            subpipeM0_E1i_u,
                                                                            subpipeM0_E1f_u,
                                                                                      
              subpipeF0_Issue, subpipeF0_RegRead, subpipeF0_E0,    subpipeF0_E1,      subpipeF0_E2,      subpipeF0_D0,
                                           subpipeF0_RRu,
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
           signal dataToAlu, dataToBranch: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
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
            begin
                inputDataWithArgsI <= getDispatchArgValues_Is(slotSelI0.state, outSigsI0.sending);
                -- Reg
                slotIssueI0.state <= updateDispatchArgs_Is(argStateI);
                -- pseudo interface
                sendingToRegReadI0 <= slotIssueI0.state.full and not outSigsI0.cancelled;
                inputDataWithArgsR <= getDispatchArgValues_RR(slotIssueI0.state, sendingToRegReadI0, fni, true, false);
                -- Reg
                slotRegReadI0.state <= updateDispatchArgs_RR(argStateR, fni.values0, regValsI0, false);

                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                        end if;

                        if events.lateEvent = '1' then
                            argStateI.full <= '0';
                        end if;

                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                            
                            unfoldedAluOp <= work.LogicExec.getAluControl(slotIssueI0.state.operation.arith);
                        end if;

                        if events.lateEvent = '1' then
                            argStateR.full <= '0';
                        end if;

                    end if;
                end process;

                subpipeI0_Issue <= makeExecResult(slotIssueI0.state);
                subpipeI0_RegRead <= makeExecResult(slotRegReadI0.state);
            end block;


            -- NOTE: it seems that only elements of .state used in Exec are: { args: MwordArray; immediate: std_logic }
            dataToAlu <= (slotRegReadI0.state.full and not outSigsI0.killSel2, executeAlu(slotRegReadI0.state, bqSelected.ins, branchData, unfoldedAluOp));
          
                    subpipeI0_RegRead_u.full <= dataToAlu.full;
                    subpipeI0_RegRead_u.tag <= subpipeI0_RegRead.tag;
                    subpipeI0_RegRead_u.dest <= subpipeI0_RegRead.dest;
                    subpipeI0_RegRead_u.value <= dataToAlu.ins.result;

                    subpipeI0_RegRead_b.full <= dataToBranch.full;
                    subpipeI0_RegRead_b.tag <= subpipeI0_RegRead.tag;
                    --subpipeI0_RegRead_b.dest <= subpipeI0_RegRead.dest;
                    subpipeI0_RegRead_b.value <= dataToBranch.ins.result;

            branchData <= basicBranch(slotRegReadI0.state.full and not outSigsI0.killSel2 and not lateEventSignal and slotRegReadI0.state.branchIns,
                                            slotRegReadI0.state, bqSelected.ins, unfoldedAluOp);   
            dataToBranch <= (slotRegReadI0.state.full and not outSigsI0.killSel2 and not lateEventSignal and slotRegReadI0.state.branchIns, branchData);  

                process (clk)
                begin
                    if rising_edge(clk) then
                        subpipeI0_E0 <= subpipeI0_RegRead_u;
                        subpipeI0_E0_b <= subpipeI0_RegRead_b;
                            dataFromBranch <= dataToBranch;
                    end if;
                end process;

            sendingBranchRR <= (sendingToRegReadI0 and slotIssueI0.state.branchIns);

                bqCompareEarly.full <= sendingBranchRR;
                bqCompareEarly.tag <= slotIssueI0.state.renameIndex;
                bqCompareEarly.dest <= slotIssueI0.state.bqPointer;

            
            execEventSignal <= dataFromBranch.ins.controlInfo.newEvent and dataFromBranch.full; -- TODO: remove 'full' here because is factored into 'newEvent'?
            execCausing <= clearDbCausing(dataFromBranch.ins);
            preExecCausing <= clearDbCausing(dataToBranch.ins);
            
                    bqUpdate.full <= dataFromBranch.full;
                    bqUpdate.tag <= dataFromBranch.ins.tags.renameIndex;
                    bqUpdate.value <= dataFromBranch.ins.target; 
            
            DELAYED_EXEC_EVENT: process (clk)
            begin
                if rising_edge(clk) then
                    execEventSignalDelayed <= execEventSignal;
                    execCausingDelayed <= execCausing;
                    execResultDelayed <= execEvent_N;
                end if;
            end process;
        end block;
         
            
        SUBPIPE_MEM: block
           signal dataToAgu, dataToAguInt, dataToAguFloat, dataInMem1: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
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
            begin
                inputDataWithArgsI <= getDispatchArgValues_Is(slotSelM0.state, outSigsM0.sending);
                -- Reg
                slotIssueM0.state <= updateDispatchArgs_Is(argStateI);
                -- pseudo interface
                sendingToRegReadM0 <= slotIssueM0.state.full and not outSigsM0.cancelled;
                inputDataWithArgsR <= getDispatchArgValues_RR(slotIssueM0.state, sendingToRegReadM0, fni, true, false);
                -- Reg
                slotRegReadM0.state <= updateDispatchArgs_RR(argStateR, fni.values0, regValsM0, false);

                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateI.full <= '0';
                        end if;

                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateR.full <= '0';
                        end if;
                                
                    end if;
                end process;

                subpipeM0_Issue <= makeExecResult(slotIssueM0.state);
                subpipeM0_RegRead <= makeExecResult(slotRegReadM0.state);
                
            end block;

           preIndexSQ <= slotRegReadM0.state.sqPointer;
           preIndexLQ <= slotRegReadM0.state.lqPointer;
                preAddressOp <= slotRegReadM0.state.operation;

           sendingFromDLQ <= '0';          -- TEMP!
           dataFromDLQ <= DEFAULT_INSTRUCTION_STATE; -- TEMP!

                    subpipeM0_RR.full <= (slotRegReadM0.state.full and not outSigsM0.killSel2) and not lateEventSignal;
                    subpipeM0_RR.tag <= slotRegReadM0.state.renameIndex;
                    subpipeM0_RR.dest <= slotRegReadM0.state.argSpec.dest;

                    subpipeM0_RRi.full <= (slotRegReadM0.state.full and not outSigsM0.killSel2) and slotRegReadM0.state.argSpec.intDestSel   and not lateEventSignal;
                    subpipeM0_RRi.tag <= slotRegReadM0.state.renameIndex;
                    subpipeM0_RRi.dest <= slotRegReadM0.state.argSpec.dest when slotRegReadM0.state.argSpec.intDestSel = '1' else (others => '0');
                    subpipeM0_RRi.value <= subpipeM0_RR_u.value;

                    subpipeM0_RRf.full <= (slotRegReadM0.state.full and not outSigsM0.killSel2) and slotRegReadM0.state.argSpec.floatDestSel  and not lateEventSignal;
                    subpipeM0_RRf.tag <= slotRegReadM0.state.renameIndex;
                    subpipeM0_RRf.dest <= slotRegReadM0.state.argSpec.dest when slotRegReadM0.state.argSpec.floatDestSel = '1' else (others => '0');
                    subpipeM0_RRf.value <= subpipeM0_RR_u.value;


                subpipeM0_RR_u.full <= sendingToAgu;
                subpipeM0_RR_u.tag <= subpipeM0_RR.tag;
                subpipeM0_RR_u.dest <= subpipeM0_RR.dest;
                subpipeM0_RR_u.value <= calcEffectiveAddress(slotRegReadM0.state, sendingFromDLQ, dataFromDLQ).result;

                    process (clk)
                    begin
                        if rising_edge(clk) then
                            subpipeM0_E0 <= subpipeM0_RR_u;
                            subpipeM0_E0i <= subpipeM0_RRi;
                            subpipeM0_E0f <= subpipeM0_RRf;

                            subpipeM0_E1 <= subpipeM0_E0;
                            subpipeM0_E1i <= subpipeM0_E0i;
                            subpipeM0_E1f <= subpipeM0_E0f;

                            -- Here we integrate mem read result

                            subpipeM0_E2 <= subpipeM0_E1_u;
                            subpipeM0_E2i <= subpipeM0_E1i_u;
                            subpipeM0_E2f <= subpipeM0_E1f_u;
                            
                            
                            slotM0_E0 <= dataToAgu;
                            slotM0_E1i <= slotM0_E0;                       
                            slotM0_E2i <= dataInMem1;

                        end if;
                    end process;

           sendingToAgu <= ((slotRegReadM0.state.full and not outSigsM0.killSel2) or sendingFromDLQ) and not lateEventSignal;
	       dataToAgu(0) <= (sendingToAgu, calcEffectiveAddress(slotRegReadM0.state, sendingFromDLQ, dataFromDLQ));

               memAddressInput <= slotM0_E0(0);

           dataInMem1(0).full <= slotM0_E1i(0).full;
           dataInMem1(0).ins <= getLSResultData(slotM0_E1i(0).ins,
                                                  '1', (others => '0'),
                                                  memLoadReady, memLoadValue,
                                                  sysRegSending, sysRegReadValue,
                                                  sqSelectedOutput, lqSelectedOutput);

                subpipeM0_E1_u.full <= subpipeM0_E1.full;
                subpipeM0_E1_u.tag <= subpipeM0_E1.tag;
                subpipeM0_E1_u.dest <= subpipeM0_E1.dest;
                subpipeM0_E1_u.value <= dataInMem1(0).ins.result;
                
                subpipeM0_E1i_u.full <= subpipeM0_E1i.full;
                subpipeM0_E1i_u.tag <= subpipeM0_E1i.tag;
                subpipeM0_E1i_u.dest <= subpipeM0_E1i.dest;
                subpipeM0_E1i_u.value <= dataInMem1(0).ins.result;

                subpipeM0_E1f_u.full <= subpipeM0_E1f.full;
                subpipeM0_E1f_u.tag <= subpipeM0_E1f.tag;
                subpipeM0_E1f_u.dest <= subpipeM0_E1f.dest;
                subpipeM0_E1f_u.value <= dataInMem1(0).ins.result;           

           -- TEMP mem interface    
		   dread <= subpipeM0_E0.full;
           dadr <= subpipeM0_E0.value;
           sysRegReadSel <= subpipeM0_E0.value(4 downto 0);
           sysRegRead <= subpipeM0_E0.full;
           
           memLoadReady <= dvalid;              
           memLoadValue <= din;      
        end block;

        ------------------------
        readyRegFlagsSV <= (readyRegFlagsInt(2), '0', '0', readyRegFlagsInt(5), '0', '0', readyRegFlagsInt(8), '0', '0', readyRegFlagsInt(11), '0', '0');

        SUBPIPES_STORE_VALUE: block
            signal fmaIntSV, fmaFloatSV: ForwardingMatchesArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_FORWARDING_MATCHES);
            signal sendingToRegReadI, sendingToRegReadF, sendingToRegReadIntSV, sendingToRegReadFloatSV: std_logic := '0';
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
            begin

                cancelledSVI1 <= outSigsSVI.cancelled or (storeValueCollision2 and outSigsSVI.killSel2); -- If stalled, it stayed here but kill sig moved to next stage

                inputDataWithArgsI <= getDispatchArgValues_Is(slotSelIntSV.state, outSigsSVI.sending);
                -- Reg
                slotIssueIntSV.state <= updateDispatchArgs_Is(argStateI);
                -- pseudo interface
                sendingToRegReadIntSV <= slotIssueIntSV.state.full and not cancelledSVI1;
          
                inputDataWithArgsR <= getDispatchArgValues_RR(slotIssueIntSV.state, sendingToRegReadIntSV, fni, false, true);
                -- Reg
                slotRegReadIntSV.state <= updateDispatchArgs_RR(argStateR, fni.values0, regValsS0, true);
    
                process (clk)
                begin
                    if rising_edge(clk) then
                        if allowIssueStoreDataInt = '1' then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateI.full <= '0';
                        end if;
    
                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateR.full <= '0';
                        end if;
                                
                    end if;
                end process;

            end block;

            sendingToStoreWriteInt <= slotRegReadIntSV.state.full and not outSigsSVI.killSel2;
   
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
            begin
            
                inputDataWithArgsI <= getDispatchArgValues_Is(slotSelFloatSV.state, outSigsSVF.sending);
                -- Reg
                slotIssueFloatSV.state <= updateDispatchArgs_Is(argStateI);
                -- pseudo interface
                sendingToRegReadFloatSV <= slotIssueFloatSV.state.full and not outSigsSVF.cancelled;
                inputDataWithArgsR <= getDispatchArgValues_RR(slotIssueFloatSV.state, sendingToRegReadFloatSV, fniFloat, false, true);
                -- Reg
                slotRegReadFloatSV.state <= updateDispatchArgs_RR(argStateR, fniFloat.values0, regValsFS0, true);
    
                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateI.full <= '0';
                        end if;
    
                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateR.full <= '0';
                        end if;
                                
                    end if;
                end process;

            end block;

            sendingToStoreWriteFloat <= slotRegReadFloatSV.state.full and not outSigsSVF.killSel2;
              
            stateExecStoreValue <= slotRegReadFloatSV.state when slotRegReadFloatSV.state.full = '1'
                                  else slotRegReadIntSV.state;
            sendingToStoreWrite <= sendingToStoreWriteInt or sendingToStoreWriteFloat;            
        end block;

        
        SUBPIPE_FP0: block
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
            begin    
                inputDataWithArgsI <= getDispatchArgValues_Is(slotSelF0.state, outSigsF0.sending);
                -- Reg
                slotIssueF0.state <= updateDispatchArgs_Is(argStateI);
                -- pseudo interface
                sendingToRegReadF0 <= slotIssueF0.state.full and not outSigsF0.cancelled;
                inputDataWithArgsR <= getDispatchArgValues_RR(slotIssueF0.state, sendingToRegReadF0, fniFloat, false, false);
                -- Reg
                slotRegReadF0.state <= updateDispatchArgs_RR(argStateR, fniFloat.values0, regValsF0, false);
    
                process (clk)
                begin
                    if rising_edge(clk) then
                        if true then -- nextAccepting
                            argStateI <= inputDataWithArgsI;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateI.full <= '0';
                        end if;
    
                        if true then -- nextAccepting
                            argStateR <= inputDataWithArgsR;
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateR.full <= '0';
                        end if;
                                
                    end if;
                end process;
    
                subpipeF0_Issue <= makeExecResult(slotIssueF0.state);
                subpipeF0_RegRead <= makeExecResult(slotRegReadF0.state);

            end block;

            subpipeF0_RRu.full <= slotRegReadF0.state.full and not outSigsF0.killSel2;
            subpipeF0_RRu.tag <= slotRegReadF0.state.renameIndex;
            subpipeF0_RRu.dest <= slotRegReadF0.state.argSpec.dest;
            subpipeF0_RRu.value <= executeFpu(slotRegReadF0.state).result;

            process (clk)
            begin
                if rising_edge(clk) then
                    subpipeF0_E0 <= subpipeF0_RRu;
                    subpipeF0_E1 <= subpipeF0_E0;
                    subpipeF0_E2 <= subpipeF0_E1;
                end if;
            end process;
            
         end block;

                sqValueResult.full <= sendingToStoreWrite;
                sqValueResult.tag <= stateExecStoreValue.renameIndex;
                sqValueResult.dest <= stateExecStoreValue.sqPointer;
                sqValueResult.value <= stateExecStoreValue.args(0);
         
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

                branchCtrl <= dataFromBranch.ins.controlInfo;
                memoryCtrl <= slotM0_E2i(0).ins.controlInfo;
   
            execOutMain(0) <= subpipeI0_E0;
            execOutMain(2) <= subpipeM0_E2;
            execOutMain(3) <= subpipeF0_E2;
            
            execOutSec(2) <= sqValueResult;
            
         fni <= buildForwardingNetwork(   DEFAULT_EXEC_RESULT, subpipeI0_Issue,     subpipeI0_RegRead,   subpipeI0_E0,        subpipeI0_D0,
                                          DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT,
                                          subpipeM0_RegRead,   subpipeM0_E0i,     subpipeM0_E1i,     subpipeM0_E2i,     subpipeM0_D0i
                                          
                                        );
    
         fniFloat <= buildForwardingNetworkFP( subpipeF0_RegRead,   subpipeF0_E0,        subpipeF0_E1,        subpipeF0_E2,        subpipeF0_D0,
                                               DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT,
                                               subpipeM0_E0f,     subpipeM0_E1f,     subpipeM0_E2f,     subpipeM0_D0f,       subpipeM0_D1f                                        
                                            );

         regsSelI0 <= work.LogicRenaming.getPhysicalArgs(slotIssueI0.state);
         regsSelM0 <= work.LogicRenaming.getPhysicalArgs(slotIssueM0.state);
         -- TEMP!
         regsSelS0 <= work.LogicRenaming.getPhysicalArgs(slotIssueIntSV.state);
         regsSelFS0 <= work.LogicRenaming.getPhysicalArgs(slotIssueFloatSV.state);
         
         regsSelF0 <= work.LogicRenaming.getPhysicalArgs(slotIssueF0.state);


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
                writingData_T(0) => resultToIntRF,
             readyRegFlagsNext => readyRegFlagsIntNext
         );

         resultToFloatWQ <= subpipeM0_E2f when subpipeM0_E2f.full = '1' else subpipeF0_E2;

		 FLOAT_REG_FILE: entity work.RegFile(Behavioral)
         generic map(IS_FP => true, WIDTH => 4, WRITE_WIDTH => 1)
         port map(
             clk => clk, reset => '0', en => '0',

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

		    storeValueInput_N => bqUpdate,
            compareAddressQuickInput_N => bqCompareEarly,
        compareQuickPtr => bqCompareEarly.dest,

		selectedDataOutput => bqSelected,

		committing => robSending, -- When ROB is sending so is BQ if it has corresponding branches
		robData => dataOutROB,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalDelayed,
		  execCausing_N => DEFAULT_EXEC_RESULT,
		nextAccepting => commitAccepting,		
		sendingSQOut => open,
		
		  committedDataOut_N => bqTargetData_N
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
            
		  storeValuePtr => sqValueResult.dest,
		  storeValueResult => sqValueResult,
		
		compareAddressInput => memAddressInput,
        compareIndexInput => preIndexSQ,
            preCompareOp => preAddressOp,
            
		selectedDataOutput => sqSelectedOutput,

		committing => robSending,
		robData => dataOutROB,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalDelayed,
		  execCausing_N => execCausingDelayedSQ,
		
		nextAccepting => commitAccepting,

        committedEmpty => sbEmpty,
        committedSending => sbSending,
        committedDataOut => dataFromSB
	);
        execCausingDelayedSQ.dest <= execCausingDelayed.tags.sqPointer;


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

		    storeValuePtr => (others => '0'),
            storeValueResult => DEFAULT_EXEC_RESULT,
		
		compareAddressInput => memAddressInput,
        compareIndexInput => preIndexLQ,        
             preCompareOp => preAddressOp,
             
		selectedDataOutput => lqSelectedOutput,

		committing => robSending,
		robData => dataOutROB,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalDelayed,
		  execCausing_N => execCausingDelayedLQ,
		
		nextAccepting => commitAccepting,
		
        committedEmpty => open,
        committedSending => open,
        committedDataOut => open
	);

        execCausingDelayedLQ.dest <= execCausingDelayed.tags.lqPointer;

	MEMORY_INTERFACE: block
		signal sysStoreAddressW: Mword := (others => '0');
	begin
		doutadr <= dataFromSB(0).ins.target;
		dwrite <= sbSending and dataFromSB(0).full and isStoreMemOp(dataFromSB(0).ins);
		dout <= dataFromSB(0).ins.result;
	end block;
	
end Behavioral;
