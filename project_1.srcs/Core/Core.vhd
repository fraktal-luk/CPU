----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;
use work.ForwardingNetwork.all;

use work.DebugUtils.all;


entity Core is
    port ( clk : in  STD_LOGIC;
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

    signal frontAccepting, bpSending, renameAllow, frontGroupSend, frontSendAllow, canSendRename, robSending, renameSendingBr, renamedSending, commitAccepting, bqAccepting,
           allocAcceptAlu, allocAcceptMul, allocAcceptMem, allocAcceptSVI, allocAcceptSVF, allocAcceptF0, allocAcceptSQ, allocAcceptLQ, allocAcceptROB, acceptingMQ, almostFullMQ,
           mqReady, mqIssueSending, mqRegReadSending, sbEmpty, intSignal, memFail
           : std_logic := '0';

    signal renamedDataLivingRe, renamedDataLivingMerged, renamedDataToBQ: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal aluMaskRe, mulMaskRe, memMaskRe, branchMaskRe, loadMaskRe, storeMaskRe, intStoreMaskRe, floatStoreMaskRe, fpMaskRe,
           branchMaskOO, loadMaskOO, storeMaskOO, systemStoreMaskOO, systemLoadMaskOO, zerosMask, commitEffectiveMaskSQ, commitEffectiveMaskLQ, branchCommitMask
           : std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');

    signal frontGroupOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);

    signal renamedArgsInt, renamedArgsFloat, renamedArgsMerged, renamedArgsIntROB, renamedArgsFloatROB: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);

    signal bqPointer, bqPointerSeq, lqPointer, sqPointer: SmallNumber := (others => '0');

    signal renameGroupCtrNext, commitGroupCtr, commitGroupCtrNext: InsTag := (others => '0');
    signal newIntDests, newFloatDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal intType: std_logic_vector(0 to 1) := (others => '0');

    signal execOutMain, execOutSec: ExecResultArray(0 to 3) := (others => DEFAULT_EXEC_RESULT);
    signal renamedCtrl, branchCtrl, memoryCtrlE2, ctrlOutROB: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal bqCompareEarly, bqUpdate, sqValueResultRR, sqValueResultE0, sqValueResultE1, sqValueResultE2, memAddressInput, memAddressInputEarly, frontEvent, execEvent, lateEvent,
           bqTargetData, resOutSQ, dataFromSB, missedMemResultE1, missedMemResultE2, mqReexecResIssue, mqReexecResRR, memoryRead, sysRegReadIn, sysRegReadOut, defaultExecRes
           : ExecResult := DEFAULT_EXEC_RESULT;

    signal pcData, dataToBranch, bqSelected, mqReexecCtrlIssue, mqReexecCtrlRR,
           memCtrlRR, memCtrlE0, missedMemCtrlE1, missedMemCtrlE2, ctOutLQ, ctOutSQ, ctOutSB: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal bpData: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
    signal robOut: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);

    signal ch0, ch1, ch2, ch3, ch4: std_logic := '0';

    signal events: EventState := DEFAULT_EVENT_STATE;
    signal dbState: DbCoreState := DEFAULT_DB_STATE;

    signal TMP_aluTags, TMP_mulTags, TMP_memTags, TMP_sviTags, TMP_svfTags, TMP_fpTags,
            TMP_aluTagsPre,  TMP_mulTagsPre, TMP_memTagsPre, TMP_sviTagsPre, TMP_svfTagsPre, TMP_fpTagsPre,

            TMP_aluTagsT, TMP_mulTagsT, TMP_memTagsT, TMP_sviTagsT, TMP_svfTagsT, TMP_fpTagsT,
            TMP_aluTagsPreT, TMP_mulTagsPreT, TMP_memTagsPreT, TMP_sviTagsPreT, TMP_svfTagsPreT, TMP_fpTagsPreT
            : SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));

    signal TMP_renamedDests: SmallNumberArray(0 to RENAME_W-1) := (others => (others => '0'));
    signal TMP_renamedSources: SmallNumberArray(0 to 3*RENAME_W-1) := (others => (others => '0'));
    signal memIssueFullIQ, memIssueFullMQ, lockIssueI0_NoMemFail, dividerSending: std_logic := '0';
begin
            ch0 <= '0';

    intSignal <= int0 or int1;
    intType <= (int0, int1);

    dread <= memoryRead.full;
    dadr <= memoryRead.value;

    dataFromSB <= (DEFAULT_DEBUG_INFO, ctOutSB.controlInfo.c_full and isStoreSysOp(ctOutSB.op), '0', InsTag'(others => '0'), zeroExtend(ctOutSB.target(4 downto 0), SMALL_NUMBER_SIZE), ctOutSB.nip);

    sysRegReadIn.full <= memoryRead.full;
    sysRegReadIn.value <= zeroExtend(memoryRead.value(4 downto 0), MWORD_SIZE);

	UNIT_SEQUENCER: entity work.UnitSequencer(Behavioral)
    port map (
        clk => clk, reset => reset, en => '0',

        -- sys reg interface
        sysRegReadIn => sysRegReadIn,
        sysRegReadOut => sysRegReadOut,

        -- to front pipe
        pcDataOut => pcData,

        intAllowOut => intallow,
        intAckOut => intack,
        intRejOut => open,
        -- Events in
        intSignal => intSignal,
        intType => intType,
        frontEvent => frontEvent,
        execEvent => execEvent,

        -- Events out
        lateEvent => lateEvent,

        -- Interface from ROB
        commitAccepting => commitAccepting,
        robData => robOut,
        robCtrl => ctrlOutROB,
        ---
        bqTargetData => bqTargetData,

        sbSending => ctOutSB.full,
        dataFromSB => dataFromSB,
        sbEmpty => sbEmpty,

        commitGroupCtrOut => commitGroupCtr,
        commitGroupCtrNextOut => commitGroupCtrNext,

        doneSig => oaux(0),
        failSig => oaux(1)
    );

    iadr <= pcData.ip;
    iadrvalid <= pcData.controlInfo.c_full;


	UNIT_FRONT: entity work.UnitFront(Behavioral)
    port map(
        clk => clk, reset => '0', en => '0',
        events => events,

        iin => iin,
       
        pcDataIn => pcData,
        frontAccepting => frontAccepting,

        bqAccepting => bqAccepting,
        bpSending => bpSending,
        bpData => bpData,

        renameAccepting => frontSendAllow,

        dataOut => frontGroupOut,
        lastSending => frontGroupSend,

        frontCausing => frontEvent,

        dbState => dbState
    );    

    REGISTER_MANAGER: entity work.UnitRegManager(Behavioral)
    port map(
        clk => clk,
        events => events,

        renameAccepting => renameAllow,
        frontSendingIn => frontGroupSend,
        frontData => frontGroupOut,
        frontCtrl => DEFAULT_CONTROL_PACKET,

        aluMaskRe => aluMaskRe,
        mulMaskRe => mulMaskRe,
        memMaskRe => memMaskRe,
        branchMaskRe => branchMaskRe,
        loadMaskRe => loadMaskRe,
        storeMaskRe => storeMaskRe,
        intStoreMaskRe => intStoreMaskRe,
        floatstoreMaskRe => floatStoreMaskRe,
        fpMaskRe => fpMaskRe,

        nextAccepting => canSendRename,

        renamedDataLiving => renamedDataLivingRe,

        renamedArgsInt => renamedArgsInt,
        renamedArgsFloat => renamedArgsFloat,

        renamedSending => renamedSending,
        renamedCtrl => renamedCtrl,

        bqPointer => bqPointer,
        sqPointer => sqPointer,
        lqPointer => lqPointer,
        bqPointerSeq => bqPointerSeq,

        sendingFromROB => robSending,
        robData => robOut,
        commitArgInfoI => renamedArgsIntROB,
        commitArgInfoF => renamedArgsFloatROB,

        newPhysDestsOut => newIntDests,
        newFloatDestsOut => newFloatDests,

        renameGroupCtrNextOut => renameGroupCtrNext,

        dbState => dbState
    );

    renameSendingBr <= frontGroupSend and frontGroupOut(0).firstBr;

    frontSendAllow <=   renameAllow 
                    and allocAcceptAlu and allocAcceptMul and allocAcceptMem
                    and allocAcceptSVI and allocAcceptSVF and allocAcceptF0
                    and allocAcceptSQ and allocAcceptLQ and allocAcceptROB;
    canSendRename <= '1'; 

    renamedArgsMerged <= mergeRenameInfoFP(renamedArgsInt, renamedArgsFloat);
    renamedDataLivingMerged <= replaceDests(renamedDataLivingRe, renamedArgsMerged);

	REORDER_BUFFER: entity work.ReorderBuffer(Behavioral)
	port map(
		clk => clk, reset => '0', en => '0',

        events => events,

		execSigsMain => execOutMain,
		execSigsSec => execOutSec,

		branchControl => branchCtrl,
		memoryControl => memoryCtrlE2,

        inputCtrl => renamedCtrl,
		inputData => renamedDataLivingMerged,
		prevSending => renamedSending,
		prevSendingRe => frontGroupSend,

		acceptAlloc => allocAcceptROB,

		nextAccepting => commitAccepting,

		sendingOut => robSending, 
        robOut => robOut,
        outputCtrl => ctrlOutROB,
        
        outputArgInfoI => renamedArgsIntROB,
        outputArgInfoF => renamedArgsFloatROB,

		dbState => dbState	
	);     

        TMP_aluTagsT <= iqInds2tags(TMP_aluTags);
        TMP_mulTagsT <= iqInds2tags(TMP_mulTags);
        TMP_memTagsT <= iqInds2tags(TMP_memTags);
        TMP_sviTagsT <= iqInds2tags(TMP_sviTags);
        TMP_svfTagsT <= iqInds2tags(TMP_svfTags);
        TMP_fpTagsT <= iqInds2tags(TMP_fpTags);

        TMP_aluTagsPreT <= iqInds2tags(TMP_aluTagsPre);
        TMP_mulTagsPreT <= iqInds2tags(TMP_mulTagsPre);
        TMP_memTagsPreT <= iqInds2tags(TMP_memTagsPre);
        TMP_sviTagsPreT <= iqInds2tags(TMP_sviTagsPre);
        TMP_svfTagsPreT <= iqInds2tags(TMP_svfTagsPre);
        TMP_fpTagsPreT <= iqInds2tags(TMP_fpTagsPre);


        ALLOC_MUL_STUB: if not ENABLE_MUL_DIV generate
            allocAcceptMul <= '1';
        end generate;

        ALLOC_FP_STUB: if not ENABLE_FP generate
            allocAcceptSVF <= '1';
            allocAcceptF0 <= '1';
        end generate;


        RENAMER: entity work.Renamer
        port map(
            clk => clk, evt => events,

            prevSending => frontGroupSend,

            frontData => frontGroupOut,

            maskAlu => aluMaskRe,
            maskMul => mulMaskRe,
            maskMem => memMaskRe,

            TMP_tagsAlu => TMP_aluTagsPreT,
            TMP_tagsMul => TMP_mulTagsPreT,
            TMP_tagsMem => TMP_memTagsPreT,

            commitArgInfoI => renamedArgsIntROB,

            TMP_destsOut => TMP_renamedDests,
            TMP_sourcesOut => TMP_renamedSources,

            commitGroupCtr => commitGroupCtr,
            commitGroupCtrNext => commitGroupCtrNext,
            renameGroupCtrNext => renameGroupCtrNext,

            renameSending => renamedSending, -- CAREFUL, it's an input
            robSending => robSending, -- CAREFUL, it's an input

            dummy => open
        );


    TEMP_EXEC: block
       use work.LogicExec.all;
        
       -- TODO: These 2 are in large scope because issue signal is needed for conflict resolution. Fix it  
       signal outSigsSVI: IssueQueueSignals := (others => '0');
       signal outSigsSVF: IssueQueueSignals := (others => '0');

       -- Selection from IQ and state after Issue stage
       signal slotIssueI0, slotRegReadI0,
              slotIssueI1, slotRegReadI1,
              slotIssueM0, slotRegReadM0,
              slotIssueF0, slotRegReadF0,

                slotIssueI0_TF, slotIssueI1_TF, slotIssueM0_TF, slotIssueSVI_TF, slotIssueSVF_TF, slotIssueF0_TF,
                slotIssueI0_TS, slotIssueI1_TS, slotIssueM0_TS, slotIssueSVI_TS, slotIssueSVF_TS, slotIssueF0_TS,
                    slotIssueI0_U, slotIssueI1_U, slotIssueM0_U, slotIssueSVI_U, slotIssueSVF_U, slotIssueF0_U,

              slotSel4, slotIssue4, slotSel5, slotIssue5, slotSel6, slotIssue6,

              slotSelIntSV, slotIssueIntSV,
              slotRegReadIntSV, slotRegReadIntSV_Delay,
              slotSelFloatSV, slotIssueFloatSV, slotRegReadFloatSV
                        : SchedulerState := DEFAULT_SCHED_STATE;

       signal newIntSources, newFloatSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

       -- Issue control 
       signal memSubpipeSent, mulSubpipeSent, mulSubpipeAtE0, fp0subpipeSelected, mulSubpipeSelected,
              lockIssueSVI, lockIssueSVF, allowIssueStoreDataInt, allowIssueStoreDataFP, lockIssueI0, allowIssueI0,
              lockIssueI1, allowIssueI1, lockIssueM0, allowIssueM0, lockIssueF0, allowIssueF0,
              intWriteConflict, storeValueCollision1, storeValueCollision2, storeValueCollision3, memDepFail, prevMemDepFail: std_logic := '0';

       signal subpipeI0_Issue, subpipeI0_RegRead, subpipeI0_E0,                                    subpipeI0_D0,
              subpipeI1_Issue, subpipeI1_RegRead, subpipeI1_E0,  subpipeI1_E1,    subpipeI1_E2,    subpipeI1_D0,  subpipeI1_D1,
              subpipeM0_Issue, subpipeM0_RegRead, subpipeM0_E0,  subpipeM0_E1,    subpipeM0_E2,
                                   subpipeM0_RRi, subpipeM0_E0i, subpipeM0_E1i,   subpipeM0_E2i,   subpipeM0_D0i,-- subpipeM0_D1i,
                                   subpipeM0_RRf, subpipeM0_E0f, subpipeM0_E1f,   subpipeM0_E2f,   subpipeM0_D0f, subpipeM0_D1f,
                                                            
                                                            subpipeM0_E1_u, subpipeM0_E1i_u, subpipeM0_E1f_u,

              subpipeF0_Issue, subpipeF0_RegRead, subpipeF0_E0,    subpipeF0_E1,      subpipeF0_E2,      subpipeF0_D0,
                                           subpipeF0_RRu,
              subpipe_DUMMY: ExecResult := DEFAULT_EXEC_RESULT;

        signal unfoldedAluOp, unfoldedAluOp_T: work.LogicExec.AluControl := work.LogicExec.DEFAULT_ALU_CONTROL;

        signal bypassInt, bypassFloat, bypassIntSV, bypassFloatSV: BypassState := DEFAULT_BYPASS_STATE;
        signal valuesInt0, valuesInt1, valuesFloat0, valuesFloat1: MwordArray(0 to 2) := (others => (others => '0'));
        signal issueTagI0: SmallNumber := sn(0);

        signal regValsI0, regValsI1, regValsM0, regValsS0, regValsE, regValsFloatA, regValsFloatB, regValsFloatC, regValsFS0, regValsF0: MwordArray(0 to 2) := (others => (others => '0'));
        signal readyRegFlagsInt_Early, readyRegFlagsInt_Early_Mem, readyRegFlagsInt_Early_Mem2, readyRegFlagsInt_C, readyRegFlagsFloat_Early,-- readyRegFlagsInt_T, readyRegFlagsFloat_T,
               readyRegFlagsIntNext_Early, readyRegFlagsIntNext_C, readyRegFlagsSV, readyRegFlagsSV2, readyRegFlagsFloatNext_Early, readyRegFlagsFloatSV, readyRegFlagsFloatSV2
              : std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');

            signal ch_a, ch_m, ch_si, ch_sf, ch_f: std_logic := '0';              
    begin
        newIntSources <= TMP_getPhysicalArgsNew(renamedArgsInt);
        newFloatSources <= TMP_getPhysicalArgsNew(renamedArgsFloat);

        SUBPIPE_ALU: block
            use work.LogicIssue.all;
            use work.LogicArgRead.all;

            signal outSigsI0: IssueQueueSignals := (others => '0');

            signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
            signal dataToAlu: ExecResult := DEFAULT_EXEC_RESULT;           

            constant CFG_ALU: SchedulerUpdateConfig := (true, false, false, FORWARDING_MODES_INT_D, false);
            constant CFG_ALU_WAIT: SchedulerUpdateConfig := (false, false, false, FORWARDING_MODES_INT_D, false); -- UNUSED
            constant CFG_ALU_SEL: SchedulerUpdateConfig :=  (false, false, false, FORWARDING_MODES_INT, false);   -- UNUSED
        begin
            wups <= work.LogicIssue.getInitWakeups(schedInfoA, bypassInt, CFG_ALU);

            schedInfoA <= getIssueInfoArray(renamedDataLivingRe, true, renamedArgsInt, TMP_renamedDests, TMP_renamedSources, I0);
            schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, readyRegFlagsInt_Early, memFail, CFG_ALU);

            IQUEUE_I0: entity work.IssueQueue(Behavioral)
            generic map(
                NAME => "I0",
                IQ_SIZE => IQ_SIZE_I0,
                FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
                FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2),
                    WAKEUP_SPEC => WAKEUP_SPEC_I0
            )
            port map(
                clk => clk, reset => '0', en => '0', events => events,

                inReady => frontGroupSend,
                inMask => aluMaskRe,

                accept => allocAcceptAlu,
                TMP_outTags => TMP_aluTags,
                TMP_outTagsPre => TMP_aluTagsPre,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedU,
                TMP_newTags => TMP_aluTags,
                bypass => bypassInt,
                nextAccepting => allowIssueI0,
                    unlockDiv => '0',
                    schedulerOut_Fast => slotIssueI0_TF,
                    schedulerOut_Slow => slotIssueI0_TS,
                outputSignals => outSigsI0, 
                dbState => dbState
            );
                    slotIssueI0 <= slotIssueI0_TF;
                    slotIssueI0_U <= TMP_mergeStatic(slotIssueI0, slotIssueI0_TS);

            TMP_ISSUE_I0: block
                signal argStateRegI0: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                slotRegReadI0 <= updateRegReadStage(argStateRegI0, outSigsI0, events, valuesInt0, regValsI0, false);

                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateRegI0 <= getRegReadStage_N(slotIssueI0_U, events, valuesInt0, valuesInt1, true, false);
                        unfoldedAluOp <= work.LogicExec.getAluControl(slotIssueI0_U.st.operation.arith);
                    end if;
                end process;

                subpipeI0_Issue <= makeExecResult(slotIssueI0);
                subpipeI0_RegRead <= makeExecResult(slotRegReadI0);

                issueTagI0 <= slotIssueI0.destTag;

                bqCompareEarly.full <= slotRegReadI0.full and slotRegReadI0.st.branchIns;
                bqCompareEarly.tag <= slotRegReadI0.st.tags.renameIndex;
                bqCompareEarly.dest <= slotRegReadI0.st.tags.bqPointer;
            end block;

                unfoldedAluOp_T <= work.LogicExec.getAluControl(slotRegReadI0.st.operation.arith);

            dataToAlu <= executeAlu(slotRegReadI0.full, slotRegReadI0, bqSelected.nip,-- dataToBranch.controlInfo
                                    unfoldedAluOp);
            process (clk)
            begin
                if rising_edge(clk) then
                    subpipeI0_E0 <= dataToAlu;
                end if;
            end process;

            dataToBranch <= basicBranch(slotRegReadI0.full and slotRegReadI0.st.branchIns, slotRegReadI0, bqSelected, unfoldedAluOp, lateEvent);

            JUMPS: block
                signal branchResultE0: ControlPacket := DEFAULT_CONTROL_PACKET;
            begin
                process (clk)
                    use work.LogicLogging.all;
                begin
                    if rising_edge(clk) then
                        if dataToBranch.controlInfo.c_full = '1' then
                            DB_reportBranchEvent(dataToBranch);
                        end if;

                        branchResultE0 <= dataToBranch;
                    end if;
                end process;
    
                execEvent <= (DEFAULT_DEBUG_INFO, branchResultE0.controlInfo.newEvent, '0', branchResultE0.tags.renameIndex, branchResultE0.tags.bqPointerSeq, branchResultE0.target);

                branchCtrl <= branchResultE0;

                bqUpdate.full <= branchResultE0.controlInfo.c_full;
                bqUpdate.tag <= branchResultE0.tags.renameIndex;
                bqUpdate.value <= branchResultE0.target;

                events <= (dataToBranch.tags, branchResultE0.tags, execEvent, lateEvent, memFail);
            end block;
        end block;


        MUL_BLOCK: if ENABLE_MUL_DIV generate
            SUBPIPE_MUL: block
               use work.LogicIssue.all;
               use work.LogicArgRead.all;

               signal outSigsI1: IssueQueueSignals := (others => '0');

               signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
               signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));

               signal divUnlock, killFollowerNextI1: std_logic := '0';
               constant CFG_MUL: SchedulerUpdateConfig := (true, false, false, FORWARDING_MODES_INT_D, false);
            begin
                wups <= getInitWakeups(schedInfoA, bypassInt, CFG_MUL);

                schedInfoA <= getIssueInfoArray(renamedDataLivingRe, true, renamedArgsInt, TMP_renamedDests, TMP_renamedSources, I1);
                schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, readyRegFlagsInt_Early, memFail, CFG_MUL);

                IQUEUE_I1: entity work.IssueQueue(Behavioral)
                generic map(
                    NAME => "I1",
                    IQ_SIZE => IQ_SIZE_I0,
                    FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
                    FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2)
                )
                port map(
                    clk => clk, reset => '0', en => '0',

                    inReady => frontGroupSend,
                    inMask => mulMaskRe,
                    
                    TMP_outTags => TMP_mulTags,
                    TMP_outTagsPre => TMP_mulTagsPre,
                
                    accept => allocAcceptMul,
            
                    prevSendingOK => renamedSending,
                    newArr => schedInfoUpdatedU,
                    TMP_newTags => TMP_mulTags,
                    bypass => bypassInt,
                    nextAccepting => allowIssueI1,
                                        unlockDiv => divUnlock,
                    events => events,
                        schedulerOut_Fast => slotIssueI1_TF,
                        schedulerOut_Slow => slotIssueI1_TS,
                    outputSignals => outSigsI1,
                    dbState => dbState
                );
                    slotIssueI1 <= slotIssueI1_TF;
                    slotIssueI1_U <= TMP_mergeStatic(slotIssueI1, slotIssueI1_TS);

                TMP_ISSUE_I1: block
                    signal argStateRegI1: SchedulerState := DEFAULT_SCHEDULER_STATE;
                begin
                    slotRegReadI1 <= updateRegReadStage(argStateRegI1, outSigsI1, events, valuesInt0, regValsI1, false);

                    process (clk)
                    begin
                        if rising_edge(clk) then
                            argStateRegI1 <= getRegReadStage_N(slotIssueI1_U, events, valuesInt0, valuesInt1, true, false);
                        end if;
                    end process;

                    subpipeI1_Issue <= makeExecResult(slotIssueI1);
                    subpipeI1_RegRead <= makeExecResult(slotRegReadI1);
                end block;
                
                killFollowerNextI1 <= killFollower(outSigsI1.trialPrev1, events);

                MUL_DIV: entity work.MultiplierDivider
                port map (
                    clk => clk,

                    prevSending => slotRegReadI1.full,
                    preInput => slotIssueI1,
                    input => slotRegReadI1,

                    allowIssueI1 => allowIssueI1,
                    killFollowerNext => killFollowerNextI1,

                    events => events,
                    
                    lockIssueI1Out => lockIssueI1,
                    divUnlockOut => divUnlock,
                    
                    sending => dividerSending,
                    outStage0 => subpipeI1_E0,
                    outStage1 => subpipeI1_E1,
                    output => subpipeI1_E2
                );
    
            end block;
        end generate;

            TMP_REORDER_MEM: block
                function reorder(flags: std_logic_vector) return std_logic_vector is
                    variable res: std_logic_vector(0 to 3*PIPE_WIDTH-1) := flags;
                begin
                    for i in 0 to PIPE_WIDTH-1 loop
                        res(3*i + 1) := flags(3*i + 1 + QQQ);
                        res(3*i + 2) := flags(3*i + 2 - QQQ);
                    end loop;
                    
                    return res;
                end function;
                
            begin
                readyRegFlagsInt_Early_Mem <= reorder(readyRegFlagsInt_Early);
            end block;
            
        SUBPIPE_MEM: block
            use work.LogicIssue.all;
            use work.LogicArgRead.all;

            signal outSigsM0: IssueQueueSignals := (others => '0');

            signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));

            constant CFG_MEM: SchedulerUpdateConfig := (true, false, false, FORWARDING_MODES_INT_D, false);

            signal controlToM0_E0, ctrlE0, ctrlE1, ctrlE1u, ctrlE2: ControlPacket := DEFAULT_CONTROL_PACKET;
            signal slotRegReadM0iq, slotRegReadM0_Merged,  slotIssueM0mq, slotRegReadM0mq: SchedulerState := DEFAULT_SCHED_STATE;
            signal resultToM0_E0, resultToM0_E0i, resultToM0_E0f: ExecResult := DEFAULT_EXEC_RESULT;
        begin
            wups <= work.LogicIssue.getInitWakeups(schedInfoA, bypassInt, CFG_MEM);

            schedInfoA <= getIssueInfoArray(renamedDataLivingRe, true, renamedArgsMerged, TMP_renamedDests, TMP_renamedSources, M0);         
            schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, readyRegFlagsInt_Early_Mem, memFail, CFG_MEM);

            IQUEUE_MEM: entity work.IssueQueue(Behavioral)
            generic map(
               NAME => "M0",
               IQ_SIZE => IQ_SIZE_M0,
               FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
               FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2)
            )
            port map(
                clk => clk, reset => '0', en => '0',

                inReady => frontGroupSend,
                inMask => memMaskRe,

                TMP_outTags => TMP_memTags,
                TMP_outTagsPre => TMP_memTagsPre,

                accept => allocAcceptMem,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedU,
                TMP_newTags => TMP_memTags,
                bypass => bypassInt,
                nextAccepting => allowIssueM0,
                                    unlockDiv => '0',
                events => events,
                    schedulerOut_Fast => slotIssueM0_TF,
                    schedulerOut_Slow => slotIssueM0_TS,
                outputSignals => outSigsM0,            
                dbState => dbState
            );

                slotIssueM0 <= slotIssueM0_TF;
                slotIssueM0_U <= TMP_mergeStatic(slotIssueM0, slotIssueM0_TS);

            mqIssueSending <= mqReexecCtrlIssue.controlInfo.c_full;
            slotIssueM0mq <= TMP_slotIssueM0mq(mqReexecCtrlIssue, mqReexecResIssue, mqIssueSending);

            TMP_ISSUE_M0: block
                signal argStateRegM0, argStateR_Merged: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                slotRegReadM0iq <= updateRegReadStage(argStateRegM0, outSigsM0, events, valuesInt0, regValsM0, false, true);
                slotRegReadM0_Merged <= updateRegReadStage(argStateR_Merged, outSigsM0, events, valuesInt0, regValsM0, false, true);

                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateRegM0 <= getRegReadStage_N(slotIssueM0_U, events, valuesInt0, valuesInt1, true, false, true);
                        argStateR_Merged <= getRegReadStage_Merge(slotIssueM0_U, slotIssueM0.full, slotIssueM0mq, events, valuesInt0, valuesInt1, true, false, true);
                    end if;
                end process;

                subpipeM0_Issue <= makeExecResult(slotIssueM0);
                subpipeM0_RegRead <= makeExecResult(slotRegReadM0);               
            end block;

            ---------------------------------------------
            -- RR --
            mqRegReadSending <= mqReexecCtrlRR.controlInfo.c_full;
            slotRegReadM0mq <= TMP_slotRegReadM0mq(mqReexecCtrlRR, mqReexecResRR, mqRegReadSending);

            -- Merge IQ with MQ
            slotRegReadM0 <= slotRegReadM0_Merged;
            ----------------------------
            -- Single packet of information for E0
            resultToM0_E0 <= calcEffectiveAddress(slotRegReadM0.full, slotRegReadM0, mqRegReadSending);
            resultToM0_E0i <= updateMemDest(resultToM0_E0, slotRegReadM0.intDestSel);
            resultToM0_E0f <= updateMemDest(resultToM0_E0, slotRegReadM0.floatDestSel);

            controlToM0_E0.controlInfo.c_full <= slotRegReadM0.full;
            controlToM0_E0.op <= slotRegReadM0.st.operation;
            controlToM0_E0.tags <= slotRegReadM0.st.tags;
            controlToM0_E0.dbInfo <= slotRegReadM0.st.dbInfo;
            --------------------------------------

            memCtrlRR <= controlToM0_E0; -- Interface LSQ
            memAddressInputEarly <= resultToM0_E0;

            ------------------------------------------------
            -- E0 -- 
            memAddressInput <= subpipeM0_E0;  -- Interface LSQ

            --------------------------------------------------------
            -- E1 --
            
            MEM_RESULTS: block
                signal memLoadReady, memoryMissed: std_logic := '0';
                signal memLoadValue, memResult: Mword := (others => '0');
            begin
                memLoadReady <= dvalid; -- In
                memLoadValue <= din;    -- In
            
                memResult <= getLSResultData_result(  ctrlE1.op,
                                                      memLoadReady, memLoadValue,
                                                      sysRegReadOut.full, sysRegReadOut.value,
                                                      ctOutSQ, ctOutLQ).value;
                ctrlE1u.tags <= ctrlE1.tags;
                ctrlE1u.op <= ctrlE1.op;
                ctrlE1u.controlInfo <= getLSResultData(ctrlE1.op,
                                                       subpipeM0_E1.value,
                                                       '1', memLoadReady, sysRegReadOut.full,
                                                       ctOutSQ, ctOutLQ);

                memoryMissed <= ctrlE1u.controlInfo.dataMiss or ctrlE1u.controlInfo.sqMiss;
    
                subpipeM0_E1_u <= setMemFail(subpipeM0_E1, memoryMissed and bool2std(ENABLE_MQ), memResult);     
                subpipeM0_E1i_u <= setMemFail(subpipeM0_E1i, memoryMissed and bool2std(ENABLE_MQ), memResult);
                subpipeM0_E1f_u <= setMemFail(subpipeM0_E1f, memoryMissed and bool2std(ENABLE_MQ), memResult);
    
                memFail <= subpipeM0_E1_u.failed;

                missedMemResultE1 <= TMP_missedMemResult(subpipeM0_E1, memoryMissed, memResult);    -- for MQ             
                missedMemCtrlE1 <= TMP_missedMemCtrl(subpipeM0_E1, subpipeM0_E1f, ctrlE1, ctrlE1u, resOutSQ); -- MQ
            end block;

            --------------------------------------------
            memIssueFullIQ <= slotIssueM0.maybeFull;
            memIssueFullMQ <= mqReexecCtrlIssue.controlInfo.c_full;

            process (clk)
            begin
                if rising_edge(clk) then
                    ctrlE0 <= controlToM0_E0;
                    subpipeM0_E0 <= resultToM0_E0;  -- mem out interface
                    subpipeM0_E0i <= resultToM0_E0i; -- common: tag, value; different: full, dest
                    subpipeM0_E0f <= resultToM0_E0f; -- common: tag, value; different: full, dest

                    ctrlE1 <= ctrlE0;
                    subpipeM0_E1 <= subpipeM0_E0;
                    subpipeM0_E1i <= subpipeM0_E0i;
                    subpipeM0_E1f <= subpipeM0_E0f;

                    -- Here we integrate mem read result
                    ctrlE2 <= ctrlE1u;
                    subpipeM0_E2 <= subpipeM0_E1_u;         -- injection of mem miss to 'full'
                    subpipeM0_E2i <= subpipeM0_E1i_u;
                    subpipeM0_E2f <= subpipeM0_E1f_u;             
                end if;
            end process;

            memCtrlE0 <= ctrlE0; -- Interface
            memoryRead <= subpipeM0_E0; -- Out

            memoryCtrlE2 <= ctrlE2; -- for ROB

        end block;

        ------------------------

        SUBPIPES_STORE_VALUE: block
            use work.LogicIssue.all;
            use work.LogicArgRead.all;

            signal schedInfoIntA, schedInfoUpdatedIntU, schedInfoFloatA, schedInfoUpdatedFloatU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);

            constant CFG_SVI: SchedulerUpdateConfig := (true, false, true, FORWARDING_MODES_SV_INT_D, false);
            constant CFG_SVF: SchedulerUpdateConfig := (true, true, true, FORWARDING_MODES_SV_FLOAT_D, false);

            signal wupsInt, wupsFloat: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
            
            signal stateExecStoreValue: SchedulerState := DEFAULT_SCHED_STATE;
            
                function reorder(flags: std_logic_vector) return std_logic_vector is
                    variable res: std_logic_vector(0 to 3*PIPE_WIDTH-1) := flags;
                begin
                    for i in 0 to PIPE_WIDTH-1 loop
                        res(3*i) := flags(3*i + 2 - QQQ);
                        res(3*i + 1) := '0';
                        res(3*i + 2) := '0';
                    end loop;
                    
                    return res;
                end function;
                
            function convertExecStoreValue(sx: SchedulerState) return ExecResult is
                variable res: ExecResult := DEFAULT_EXEC_RESULT;
            begin
                res.full := sx.full;
                res.tag := sx.st.tags.renameIndex;
                res.dest := sx.st.tags.sqPointer;
                res.value := sx.argValues(0);
                return res;
            end function;

        begin
            wupsInt <= getInitWakeups(schedInfoIntA, bypassIntSV, CFG_SVI);
            wupsFloat <= getInitWakeups(schedInfoFloatA, bypassFloatSV, CFG_SVF);

            schedInfoIntA <= getIssueInfoArray(renamedDataLivingRe, false, renamedArgsInt, TMP_renamedDests, TMP_renamedSources, SVI);
            schedInfoUpdatedIntU <= updateOnDispatch(schedInfoIntA, wupsInt, readyRegFlagsSV, memFail, CFG_SVI);

            schedInfoFloatA <= getIssueInfoArray(renamedDataLivingRe, false, renamedArgsFloat, TMP_renamedDests, TMP_renamedSources, SVF);
            schedInfoUpdatedFloatU <= updateOnDispatch(schedInfoFloatA, wupsFloat, readyRegFlagsFloatSV, memFail, CFG_SVF);

            readyRegFlagsSV <= reorder(readyRegFlagsInt_Early);
            
            IQUEUE_SV: entity work.IssueQueue(Behavioral)
            generic map(
                NAME => "SVI",
                IQ_SIZE => IQ_SIZE_INT_SV,
                FORWARDING_D(0 to 2) => FORWARDING_MODES_SV_INT_D(0 to 2),
                IGNORE_MEM_FAIL => true,
                    WAKEUP_SPEC => WAKEUP_SPEC_SVI
            )
            port map(
                clk => clk, reset => '0', en => '0',
          
                inReady => frontGroupSend,
                inMask => intStoreMaskRe,

                TMP_outTags => TMP_sviTags,

                accept => allocAcceptSVI,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedIntU,
                TMP_newTags => TMP_sviTags,
                bypass => bypassIntSV,
                nextAccepting => allowIssueStoreDataInt,
                                    unlockDiv => '0',
                events => events,
                    schedulerOut_Fast => slotIssueSVI_TF,
                    schedulerOut_Slow => slotIssueSVI_TS,
                outputSignals => outSigsSVI,
                dbState => dbState
            );
                slotIssueIntSV <= slotIssueSVI_TF;
                slotIssueSVI_U <= TMP_mergeStatic(slotIssueIntSV, slotIssueSVI_TS);


            TMP_ISSUE_SVI: block
                signal argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                slotRegReadIntSV <= updateRegReadStage(argStateR, outSigsSVI, events, valuesInt0, regValsS0, true);

                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateR <= getRegReadStage_N(slotIssueSVI_U, events, valuesInt0, valuesInt1, false, true);              
                        slotRegReadIntSV_Delay <= slotRegReadIntSV;
                    end if;
                end process;

            end block;

            readyRegFlagsFloatSV <= reorder(readyRegFlagsFloat_Early);

            FP_STORE_IQ: if ENABLE_FP generate
                IQUEUE_FLOAT_SV: entity work.IssueQueue(Behavioral)
                generic map(
                    NAME => "SVF",
                    IQ_SIZE => IQ_SIZE_FLOAT_SV, -- CAREFUL: not IS_FP because doesn't have destination
                    FORWARDING_D(0 to 2) => FORWARDING_MODES_SV_FLOAT_D(0 to 2),
                    IGNORE_MEM_FAIL => true
                )
                port map(
                    clk => clk, reset => '0', en => '0',

                    inReady => frontGroupSend,
                    inMask => floatStoreMaskRe,

                    TMP_outTags => TMP_svfTags,

                    accept => allocAcceptSVF,

                    prevSendingOK => renamedSending,
                    newArr => schedInfoUpdatedFloatU,
                    TMP_newTags => TMP_svfTags,
                    bypass => bypassFloatSV,
                    nextAccepting => allowIssueStoreDataFP,
                                        unlockDiv => '0',
                    events => events,
                        schedulerOut_Fast => slotIssueSVF_TF,
                        schedulerOut_Slow => slotIssueSVF_TS,           
                    outputSignals => outSigsSVF,
                    dbState => dbState
                );
            end generate;

                slotIssueFloatSV <= slotIssueSVF_TF;
                slotIssueSVF_U <= TMP_mergeStatic(slotIssueFloatSV, slotIssueSVF_TS);

            TMP_ISSUE_SVF: block
                signal argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                slotRegReadFloatSV <= updateRegReadStage(argStateR, outSigsSVF, events, valuesFloat0, regValsFS0, true);

                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateR <= getRegReadStage_N(slotIssueSVF_U, events, valuesFloat0, valuesFloat1, false, true);
                    end if;
                end process;
            end block;

            stateExecStoreValue <= slotRegReadIntSV_Delay when storeValueCollision3 = '1'
                              else slotRegReadFloatSV when slotRegReadFloatSV.full = '1'
                              else slotRegReadIntSV;

            sqValueResultRR <= convertExecStoreValue(stateExecStoreValue);

        end block;


        SUBPIPE_FP0: if ENABLE_FP generate
            use work.LogicIssue.all;
            use work.LogicArgRead.all;

            signal outSigsF0: IssueQueueSignals := (others => '0');
            signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
            constant CFG_FP0: SchedulerUpdateConfig := (true, true, false, FORWARDING_MODES_FLOAT_D, false);
        begin
            wups <= getInitWakeups(schedInfoA, bypassFloat, CFG_FP0);

            schedInfoA <= getIssueInfoArray(renamedDataLivingRe, false, renamedArgsFloat, TMP_renamedDests, TMP_renamedSources, work.LogicIssue.F0);
            schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, readyRegFlagsFloat_Early, memFail, CFG_FP0);

            IQUEUE_F0: entity work.IssueQueue(Behavioral)
            generic map(
                NAME => "F0",
                IQ_SIZE => IQ_SIZE_F0,
                FORWARDING(0 to 2) => FORWARDING_MODES_FLOAT(0 to 2),
                FORWARDING_D(0 to 2) => FORWARDING_MODES_FLOAT_D(0 to 2)
            )
            port map(
                clk => clk, reset => '0', en => '0',
                                
                inReady => frontGroupSend,
                inMask => fpMaskRe,

                TMP_outTags => TMP_fpTags,
                TMP_outTagsPre => TMP_fpTagsPre,
            
                accept => allocAcceptF0,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedU,
                TMP_newTags => TMP_fpTags,
                bypass => bypassFloat,
                nextAccepting => allowIssueF0,
                                    unlockDiv => '0',
                events => events,
                    schedulerOut_Fast => slotIssueF0_TF,
                    schedulerOut_Slow => slotIssueF0_TS,
                outputSignals => outSigsF0,
                dbState => dbState
            );
                slotIssueF0 <= slotIssueF0_TF;
                slotIssueF0_U <= TMP_mergeStatic(slotIssueF0, slotIssueF0_TS);

            TMP_ISSUE_F0: block
                signal argStateRegF0: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin    
                slotRegReadF0 <= updateRegReadStage(argStateRegF0, outSigsF0, events, valuesFloat0, regValsF0, false);

                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateRegF0 <= getRegReadStage_N(slotIssueF0_U, events, valuesFloat0, valuesFloat1, false, false); 
                    end if;
                end process;

                subpipeF0_Issue <= makeExecResult(slotIssueF0);
                subpipeF0_RegRead <= makeExecResult(slotRegReadF0);
            end block;

            subpipeF0_RRu <= TMP_fp(slotRegReadF0.full, slotRegReadF0);

            process (clk)
            begin
                if rising_edge(clk) then
                    subpipeF0_E0 <= subpipeF0_RRu;
                    subpipeF0_E1 <= subpipeF0_E0;
                    subpipeF0_E2 <= subpipeF0_E1;
                end if;
            end process;
            
         end generate;


         TMP_EXEC_D0: process (clk)
         begin
            if rising_edge(clk) then
                 subpipeI0_D0 <= subpipeI0_E0;

                 subpipeI1_D0 <= subpipeI1_E2;
                 subpipeI1_D1 <= subpipeI1_D0;
                 
                 subpipeM0_D0i <= subpipeM0_E2i;
                 subpipeM0_D0f <= subpipeM0_E2f;
                 subpipeM0_D1f <= subpipeM0_D0f;
                 
                 subpipeF0_D0 <= subpipeF0_E2;
             end if;
         end process;

         -- StoreData issue control:
         -- When Int and FP store data issue at the same time, the port conflict is resolved thus:
         -- Both IQs are blocked for the next cycle, so combined issue rate is never higher that 1 per cycle
         -- Int op is stalled for 1 cycle at IssueStage - no problems appear with scheduling because this subpipe has no wakeup observers and reads args only form RF 
         process (clk)
         begin
            if rising_edge(clk) then
                storeValueCollision1 <= outSigsSVI.sending and outSigsSVF.sending;
                storeValueCollision2 <= storeValueCollision1;
                storeValueCollision3 <= storeValueCollision2;

                lockIssueI0_NoMemFail <= memIssueFullIQ or memIssueFullMQ or mulSubpipeSent or dividerSending;
            end if;
         end process;

        lockIssueSVI <= storeValueCollision1 or memFail;
        lockIssueSVF <= storeValueCollision1 or memFail;

        memSubpipeSent <= slotRegReadM0.maybeFull;
        mulSubpipeSent <= slotRegReadI1.maybeFull;
        mulSubpipeAtE0   <= subpipeI1_E0.full;

        mulSubpipeSelected <= slotIssueI1.maybeFull;
        fp0subpipeSelected <= slotIssueF0.maybeFull;

        lockIssueI0 <= lockIssueI0_NoMemFail or memFail;

        -- Issue locking:
        --     if F0 issued, to avoid WB collisions with FP load
        --     if MQ intends to reexecute
        lockIssueM0 <= fp0subpipeSelected or mqReady or memFail or almostFullMQ or mulSubpipeAtE0; --CAREFUL: this if mul sends result to write queue after D0, 1 cycle later than Mem pipe
        lockIssueF0 <= '0' or memFail;

        allowIssueI0 <= not lockIssueI0;
        allowIssueI1 <= not lockIssueI1;         
        allowIssueM0 <= not lockIssueM0;     
        allowIssueStoreDataInt <= not lockIssueSVI;
        allowIssueStoreDataFP <= not lockIssueSVF;
        allowIssueF0 <= not lockIssueF0;


        execOutMain(0) <= subpipeI0_E0;
        execOutMain(1) <= subpipeI1_E2;
        execOutMain(2) <= subpipeM0_E2;
        execOutMain(3) <= subpipeF0_E2;

        execOutSec(2) <= sqValueResultRR;


        bypassInt <= makeBypassInt((subpipeI0_Issue, subpipeI1_E1, subpipeM0_RegRead),
                                   (subpipeI0_RegRead, subpipeI1_E2, subpipeM0_E0i) ,
                                   (DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT, subpipeM0_E1i),
                                    issueTagI0, memFail);
        bypassIntSV <= makeBypassIntSV((subpipeI0_E0, subpipeI1_D0, subpipeM0_E2i),
                                       (subpipeI0_D0, subpipeI1_D1, subpipeM0_D0i) ,
                                       (others => DEFAULT_EXEC_RESULT),
                                        sn(0), memFail);
         bypassFloat <= makeBypassFloat((subpipeF0_RegRead, DEFAULT_EXEC_RESULT, subpipeM0_E2f),
                                        (subpipeF0_E0, DEFAULT_EXEC_RESULT, subpipeM0_D0f) ,
                                        (subpipeF0_E1, DEFAULT_EXEC_RESULT, subpipeM0_D1f),
                                        sn(0), memFail);
        bypassFloatSV <= makeBypassFloatSV((subpipeF0_E2, DEFAULT_EXEC_RESULT, subpipeM0_D0f),
                                           (subpipeF0_D0, DEFAULT_EXEC_RESULT, subpipeM0_D1f) ,
                                           (others => DEFAULT_EXEC_RESULT),
                                            sn(0), memFail);
            
        valuesInt0 <= getExecValues((subpipeI0_E0, subpipeI1_D0, subpipeM0_E2i));
        valuesInt1 <= getExecValues((subpipeI0_D0, subpipeI1_D1, subpipeM0_D0i));

        valuesFloat0 <= getExecValues((subpipeF0_E2, DEFAULT_EXEC_RESULT, subpipeM0_D0f));
        valuesFloat1 <= getExecValues((subpipeF0_D0, DEFAULT_EXEC_RESULT, subpipeM0_D1f));


        REGISTER_FILES: block
            signal regsSelI0, regsSelI1, regsSelM0, regsSelS0, regsSelFloatA, regsSelFloatC, regsSelFS0, regsSelF0: PhysNameArray(0 to 2) := (others => (others => '0'));
            signal resultToIntRF, resultToIntRF_Early, resultToIntRF_EarlyEffective, resultToFloatRF, resultToFloatRF_Early: ExecResult := DEFAULT_EXEC_RESULT;
        begin
            regsSelI0 <= work.LogicRenaming.getPhysicalArgs(slotIssueI0);
            regsSelI1 <= work.LogicRenaming.getPhysicalArgs(slotIssueI1);
            regsSelM0 <= work.LogicRenaming.getPhysicalArgs(slotIssueM0);
            -- TEMP!
            regsSelS0 <= work.LogicRenaming.getPhysicalArgs(slotIssueIntSV);
            regsSelFS0 <= work.LogicRenaming.getPhysicalArgs(slotIssueFloatSV);
    
            regsSelF0 <= work.LogicRenaming.getPhysicalArgs(slotIssueF0);
    
            intWriteConflict <= (subpipeM0_E2i.full and subpipeI0_E0.full) or (subpipeM0_E2i.full and subpipeI1_D0.full) or (subpipeI1_D0.full and subpipeI0_E0.full);
    
            TMP_WQ: process (clk)
            begin
               if rising_edge(clk) then
                   assert intWriteConflict = '0' report "Int write queue conflict!" severity error;
                
                   resultToIntRF <= selectOrdered((subpipeM0_E2i, subpipeI1_D0, subpipeI0_E0));
                   resultToIntRF_Early <= selectOrdered((subpipeM0_E0i, subpipeI1_E1, setMemFail(subpipeI0_Issue, memFail, (others => '0'))));
                   resultToFloatRF <= selectOrdered((subpipeM0_E2f, subpipeF0_E2));
                   resultToFloatRF_Early <= selectOrdered((subpipeM0_E0f, subpipeF0_E0));
               end if;
            end process;
    
            resultToIntRF_EarlyEffective <= setMemFail(resultToIntRF_Early, memFail, resultToIntRF_Early.value);
    
            INT_REG_FILE: entity work.RegFile(Behavioral)
            generic map(WIDTH => 4, WRITE_WIDTH => 1)
            port map(
                clk => clk, reset => '0', en => '0',
    
                writeInput(0) => resultToIntRF,                
                readAllowVec => (others => '1'), -- TEMP!
     
                selectRead(0 to 2) => regsSelI0,
                selectRead(3 to 5) => regsSelI1,
                selectRead(6 to 8) => regsSelM0,
                selectRead(9 to 11) => regsSelS0,
        
                readValues(0 to 2) => regValsI0,
                readValues(3 to 5) => regValsI1,
                readValues(6 to 8) => regValsM0,
                  --  readValues(6) => regValsM0(1),
                    --readValues(7 to 8) => open,
                readValues(9 to 11) => regValsS0            
            );
    
            INT_READY_TABLE_EARLY: entity work.RegisterReadyTable(Behavioral)
            generic map(
                WRITE_WIDTH => 1
            )
            port map(
                clk => clk, reset => '0', en => '0',
    
                sendingToReserve => frontGroupSend,
                newPhysDests => newIntDests,
                newPhysSources => newIntSources,
                writingData_T(0) => resultToIntRF_EarlyEffective,
                readyRegFlagsNext => readyRegFlagsIntNext_Early
            );
            
            FP_REGISTERS: if ENABLE_FP generate
                FLOAT_REG_FILE: entity work.RegFile(Behavioral)
                generic map(IS_FP => true, WIDTH => 4, WRITE_WIDTH => 1)
                port map(
                    clk => clk, reset => '0', en => '0',
        
                    writeInput(0) => resultToFloatRF, 
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
    
                FLOAT_READY_TABLE_EARLY: entity work.RegisterReadyTable(Behavioral)
                generic map(
                    IS_FP => true, WRITE_WIDTH => 1
                )
                port map(
                    clk => clk, reset => '0', en => '0', 
                     
                    sendingToReserve => frontGroupSend,                 
                    newPhysDests => newFloatDests,
                    newPhysSources => newFloatSources,
                    writingData_T(0) => resultToFloatRF_Early,
                    readyRegFlagsNext => readyRegFlagsFloatNext_Early
                );
            end generate;

        end block;

        SRC_LATE_OVERRIDE: if true generate
        --     readyRegFlagsInt_T <= updateArgStates(renamedArgsInt, renamedArgsFloat, readyRegFlagsIntNext_Early);
        --     readyRegFlagsFloat_T <= updateArgStatesFloat(renamedArgsInt, renamedArgsFloat, readyRegFlagsFloatNext_Early);
        end generate;

        RR_FLAGS_TEMP: for i in 0 to PIPE_WIDTH-1 generate
            readyRegFlagsInt_Early(3*i to 3*i + 2 - QQQ) <= readyRegFlagsIntNext_Early(3*i to 3*i + 2 - QQQ);
            readyRegFlagsFloat_Early(3*i to 3*i + 2 - QQQ) <= readyRegFlagsFloatNext_Early(3*i to 3*i + 2 - QQQ);
        end generate;

    end block; -- TEMP_EXEC


    QUEUE_MASKS: block
    begin
        renamedDataToBQ <= setFullMask(renamedDataLivingRe, getBranchMask1(renamedDataLivingRe));

        branchMaskOO <= getBranchMask1(renamedDataLivingRe);
        loadMaskOO <= getLoadMask1(renamedDataLivingRe);
        storeMaskOO <= getStoreMask1(renamedDataLivingRe);

        systemStoreMaskOO <= getStoreSysMask(renamedDataLivingRe);
        systemLoadMaskOO <= getLoadSysMask(renamedDataLivingRe);

        commitEffectiveMaskSQ <= work.LogicQueues.getCommittedEffectiveMask(robOut, false);
        commitEffectiveMaskLQ <= work.LogicQueues.getCommittedEffectiveMask(robOut, true);
        branchCommitMask <= work.LogicQueues.getCommittedMaskBr(robOut);
    end block;

    BRANCH_QUEUE: entity work.BranchQueue
	generic map(
		QUEUE_SIZE => BQ_SIZE
	)
	port map(
		clk => clk,
		events => events,

		reset => '0',
		en => '0',

		acceptingOut => open,

		acceptingBr => bqAccepting,

		prevSending => renamedSending,
	    prevSendingBr => bpSending,

	    prevSendingRe => renameSendingBr,
        renamedCtrl => renamedCtrl,

	    renamedPtr => bqPointerSeq,

	    bqPtrOut => bqPointer,

	    branchMaskRe => branchMaskRe,
		dataIn => renamedDataToBQ,  -- Uses only .tags + .firstBr?
        dataInBr => bpData,

		storeValueInput => bqUpdate,
        compareAddressQuickInput => bqCompareEarly,

        selectedDataOutput => bqSelected,

		committing => robSending, -- When ROB is sending so is BQ if it has corresponding branches
        commitBr => robOut(0).controlInfo.firstBr,
        commitMask => branchCommitMask,

		nextAccepting => commitAccepting,		

		committedDataOut => bqTargetData,

		dbState => dbState
	);

    STORE_QUEUE: entity work.StoreQueue(Behavioral)
	generic map(
		QUEUE_SIZE => SQ_SIZE
	)
	port map(
		clk => clk,
		events => events,

		reset => '0',
		en => '0',

		acceptAlloc => allocAcceptSQ,

	    prevSendingRe => frontGroupSend,
		prevSending => renamedSending,

        renameMask => storeMaskRe,
        inputMask => storeMaskOO,
        systemMask => systemStoreMaskOO,

        renamedPtr => sqPointer,

        storeValueResult => sqValueResultRR,

        compareAddressEarlyInput => memAddressInputEarly,
        compareAddressEarlyInput_Ctrl => memCtrlRR,

        compareAddressInput => memAddressInput,
        compareAddressCtrl => memCtrlE0,

        selectedDataOutput => ctOutSQ,
        selectedDataResult => resOutSQ,

		committing => robSending,
        commitEffectiveMask => commitEffectiveMaskSQ,

		nextAccepting => commitAccepting,

        committedEmpty => sbEmpty,
        committedDataOut => ctOutSB,

        dbState => dbState
	);


    LOAD_QUEUE: entity work.StoreQueue(Behavioral)
	generic map(
		QUEUE_SIZE => LQ_SIZE,
		IS_LOAD_QUEUE => true
	)
	port map(
		clk => clk,
		events => events,

		reset => '0',
		en => '0',

		acceptAlloc => allocAcceptLQ,

	    prevSendingRe => frontGroupSend,				
		prevSending => renamedSending,

		renameMask => loadMaskRe,
        inputMask => loadMaskOO,
        systemMask => systemLoadMaskOO,

        renamedPtr => lqPointer,

        storeValueResult => DEFAULT_EXEC_RESULT,

		compareAddressEarlyInput => memAddressInputEarly,
        compareAddressEarlyInput_Ctrl => memCtrlRR,

		compareAddressInput => memAddressInput,
        compareAddressCtrl => memCtrlE0,

        selectedDataOutput => ctOutLQ,

		committing => robSending,
        commitEffectiveMask => commitEffectiveMaskLQ,

		nextAccepting => commitAccepting,

        committedEmpty => open,
        committedDataOut => open,

        dbState => dbState
	);


    process (clk)
    begin
        if rising_edge(clk) then
            -- delayed Store Data op
            sqValueResultE0 <= sqValueResultRR;
            sqValueResultE1 <= sqValueResultE0;
            sqValueResultE2 <= sqValueResultE1;

            -- MQ inputs
            missedMemResultE2 <= missedMemResultE1;
            missedMemCtrlE2 <= missedMemCtrlE1;

            -- MQ outputs
            mqReexecCtrlRR <= mqReexecCtrlIssue;
            mqReexecResRR <= mqReexecResIssue;
        end if;
    end process;

    MQ_BLOCK: if ENABLE_MQ generate 
        LOAD_MISS_QUEUE: entity work.MissQueue(DefaultMQ)
        generic map(
            QUEUE_SIZE => 8
            )
        port map(
            clk => clk,
            events => events,

            reset => '0',
            en => '0',

            acceptingOut => acceptingMQ,
            almostFull => almostFullMQ,
            acceptAlloc => open,
    
            prevSendingRe => '0',
            prevSending => '0',

            renameMask => zerosMask,
            inputMask => zerosMask,
            systemMask => zerosMask,
            renamedPtr => open,

            storeValueResult => sqValueResultE2,

            compareAddressEarlyInput => defaultExecRes,--DEFAULT_EXEC_RESULT,
            compareAddressEarlyInput_Ctrl => memCtrlRR, -- only 'tag' and 'full'

            compareAddressInput => missedMemResultE2,
            compareAddressCtrl => missedMemCtrlE2,

            selectedDataOutput => mqReexecCtrlIssue,
            selectedDataResult => mqReexecResIssue,

            committing => '0',
            commitMask => zerosMask,
            commitEffectiveMask => zerosMask,

            nextAccepting => '0',

            committedEmpty => open,
            committedSending => mqReady,

            dbState => dbState        
        );
    end generate;

    MQ_ACCEPT: if not ENABLE_MQ generate
        acceptingMQ <= '1';
        almostFullMQ <= '0';
    end generate;

	MEMORY_INTERFACE: block
	begin
		doutadr <= ctOutSB.target;
		dwrite <= ctOutSB.full and isStoreMemOp(ctOutSB.op);
		dout <= ctOutSB.nip;
	end block;


	-- pragma synthesis off
	DEBUG_HANDLING: if DB_ENABLE generate
	    use std.textio.all;
	    use work.LogicLogging.all;

        signal cycleCount, watchdogCount, watchdogCountNext: natural := 0;
        signal stallDetected, stallDetectedPrev, stallAction: std_logic := '0';
        file eventLog: text open write_mode is "event_log.txt";
    begin
        watchdogCountNext <= watchdogCount + 1 when not std2bool(robSending or renamedSending) else 0;

        MONITOR: process (clk)
        begin
            if rising_edge(clk) then
                cycleCount <= cycleCount + 1;
                watchdogCount <= watchdogCountNext;
                stallDetected <= bool2std(watchdogCount = 57);

                if DB_LOG_EVENTS then
                    logEvent(eventLog, lateEvent.full, execEvent.full, frontEvent.full, stallDetected, cycleCount);
                 end if;
            end if;
        end process;

        dbState.dbSignal <= stallDetected;

	end generate;
	-- pragma synthesis on

end Behavioral;
