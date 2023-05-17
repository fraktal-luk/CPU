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

use work.DebugUtils.all;


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

    signal frontAccepting, bpSending, renameAllow, frontGroupSend, frontSendAllow, canSendRename, robSending,
           renameSendingBr, renamedSending, commitAccepting, frontEventSignal, bqAccepting, execEventSignalE0, execEventSignalE1, lateEventSignal, lateEventSetPC,
           allocAcceptAlu, allocAcceptMul, allocAcceptMem, allocAcceptSVI, allocAcceptSVF, allocAcceptF0, allocAcceptSQ, allocAcceptLQ, allocAcceptROB, acceptingMQ, almostFullMQ,
           mqReady, mqRegReadSending, sbSending, sbEmpty, sysRegRead, sysRegSending, intSignal
           : std_logic := '0';

    signal renamedDataLivingRe, renamedDataLivingMerged, renamedDataToBQ: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal aluMaskRe, mulMaskRe, memMaskRe, branchMaskRe, loadMaskRe, storeMaskRe, intStoreMaskRe, floatStoreMaskRe, fpMaskRe,
           branchMaskOO, loadMaskOO, storeMaskOO, systemStoreMaskOO, systemLoadMaskOO,
           commitMaskSQ, commitEffectiveMaskSQ, commitMaskLQ, commitEffectiveMaskLQ, branchCommitMask, branchCommitEffectiveMask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');

    signal frontGroupOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);

    signal renamedArgsInt, renamedArgsFloat, renamedArgsMerged, renamedArgsIntROB, renamedArgsFloatROB: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);

    signal bqPointer, bqPointerSeq, lqPointer, sqPointer: SmallNumber := (others => '0');

    signal renameGroupCtrNext, commitGroupCtr, commitGroupCtrNext: InsTag := (others => '0');
    signal newIntDests, newFloatDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal intType: std_logic_vector(0 to 1) := (others => '0');
    signal sysRegReadValue: Mword := (others => '0');
    signal sysRegReadSel: slv5 := (others => '0');

    signal execOutMain, execOutSec: ExecResultArray(0 to 3) := (others => DEFAULT_EXEC_RESULT);
    signal events: EventState := DEFAULT_EVENT_STATE;
    signal specialOp, specialOutROB: SpecificOp := DEFAULT_SPECIFIC_OP;
    signal branchCtrl, memoryCtrlE2: InstructionControlInfo := DEFAULT_CONTROL_INFO;

    signal bqCompareEarly, bqUpdate, sqValueResultRR, sqValueResultE0, sqValueResultE1, sqValueResultE2,
           memAddressInput, memAddressInputEarly, frontEvent, execEvent, lateEvent, execCausingDelayedSQ, execCausingDelayedLQ,
           bqTargetData, resOutSQ, dataFromSB, missedMemResultE1, missedMemResultE2, mqReexecResIssue, mqReexecResRR, memoryRead
           : ExecResult := DEFAULT_EXEC_RESULT;

    signal pcData, dataToBranch, bqSelected, branchResultE0, branchResultE1, mqReexecCtrlIssue, mqReexecCtrlRR,
           memCtrlRR, memCtrlE0, missedMemCtrlE1, missedMemCtrlE2, ctOutLQ, ctOutSQ, ctOutSB: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal bpData: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
    signal robOut: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);

    signal ch0, ch1, ch2, ch3, ch4: std_logic := '0';

    signal dbState: DbCoreState := DEFAULT_DB_STATE;

        signal TMP_aluTags, TMP_mulTags, TMP_memTags, TMP_sviTags, TMP_svfTags, TMP_fpTags,
                TMP_aluTagsPre,  TMP_mulTagsPre, TMP_memTagsPre, TMP_sviTagsPre, TMP_svfTagsPre, TMP_fpTagsPre,
                
                TMP_aluTagsT, TMP_mulTagsT, TMP_memTagsT, TMP_sviTagsT, TMP_svfTagsT, TMP_fpTagsT,
                TMP_aluTagsPreT, TMP_mulTagsPreT, TMP_memTagsPreT, TMP_sviTagsPreT, TMP_svfTagsPreT, TMP_fpTagsPreT
                : SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));
                
        signal TMP_renamedDests: SmallNumberArray(0 to RENAME_W-1) := (others => (others => '0'));
        signal TMP_renamedSources: SmallNumberArray(0 to 3*RENAME_W-1) := (others => (others => '0'));
begin

    intSignal <= int0 or int1;
    intType <= (int0, int1);


    dread <= memoryRead.full;
    dadr <= memoryRead.value;
    sysRegReadSel <= memoryRead.value(4 downto 0);
    sysRegRead <= memoryRead.full;


    events <= (lateEventSignal, execEventSignalE0, dataToBranch.tags, execEvent, lateEvent);

    execEvent <= (DEFAULT_DEBUG_INFO, execEventSignalE0, '0', branchResultE0.tags.renameIndex, branchResultE0.tags.bqPointerSeq, branchResultE0.target);
    dataFromSB <= (DEFAULT_DEBUG_INFO, ctOutSB.controlInfo.full and isStoreSysOp(ctOutSB.op), '0', InsTag'(others => '0'),
                     zeroExtend(ctOutSB.target(4 downto 0), SMALL_NUMBER_SIZE), ctOutSB.nip);

	UNIT_SEQUENCER: entity work.UnitSequencer(Behavioral)
	generic map(DEBUG_FILE_PREFIX => DEBUG_FILE_PREFIX)
    port map (
        clk => clk, reset => reset, en => '0',
        
        -- sys reg interface
        sysRegReadSel => sysRegReadSel,
        sysRegReadValue => sysRegReadValue,

        -- to front pipe
        pcDataOut => pcData,

        intAllowOut => intallow,
        intAckOut => intack,
        intRejOut => open,
        -- Events in
        intSignal => intSignal,
        intType => intType,
        execEventSignal => execEventSignalE0,        
        frontEventSignal => frontEventSignal,        
        frontEvent => frontEvent,
        execEvent => execEvent,

        -- Events out
        lateEventOut => lateEventSignal,
        lateEventSetPC => lateEventSetPC,
        lateEvent => lateEvent,

        -- Interface from ROB
        commitAccepting => commitAccepting,
        sendingFromROB => robSending,    
        robData => robOut,
        robSpecial => specialOutROB,
        ---
        bqTargetData => bqTargetData,

        sbSending => sbSending,
        dataFromSB => dataFromSB,
        sbEmpty => sbEmpty,

        commitGroupCtrOut => commitGroupCtr,
        commitGroupCtrNextOut => commitGroupCtrNext,

        doneSig => oaux(0),
        failSig => oaux(1)
    );

    iadr <= pcData.ip;
    iadrvalid <= pcData.controlInfo.full;



	UNIT_FRONT: entity work.UnitFront(Behavioral)
    port map(
        clk => clk, reset => '0', en => '0',
        
        iin => iin,
                    
        pcDataIn => pcData,
        frontAccepting => frontAccepting,
    
        bqAccepting => bqAccepting,
        bpSending => bpSending,
        bpData => bpData,

        renameAccepting => frontSendAllow,
        dataOut => frontGroupOut,
        lastSending => frontGroupSend,

        frontEventSignal => frontEventSignal,
        frontCausing => frontEvent,

        execCausing => execEvent,
        lateCausing => lateEvent,

        execEventSignal => execEventSignalE0,
        lateEventSignal => lateEventSignal,
        lateEventSetPC => lateEventSetPC,

        dbState => dbState
    );    

    REGISTER_MANAGER: entity work.UnitRegManager(Behavioral)
    port map(
        clk => clk,
        renameAccepting => renameAllow,
        frontSendingIn => frontGroupSend,
        frontData => frontGroupOut,

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

        renamingBr => renameSendingBr,

        bqPointer => bqPointer,
        sqPointer => sqPointer,
        lqPointer => lqPointer,
        bqPointerSeq => bqPointerSeq,

        commitArgInfoI => renamedArgsIntROB,
        commitArgInfoF => renamedArgsFloatROB,
        sendingFromROB => robSending,

        newPhysDestsOut => newIntDests,
        newFloatDestsOut => newFloatDests,

        specialOut => specialOp,

        renameGroupCtrNextOut => renameGroupCtrNext,

        execCausing => branchResultE0,

        execEventSignal => execEventSignalE0,
        lateEventSignal => lateEventSignal,

        dbState => dbState
    );

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

		lateEventSignal => lateEventSignal,

		execSigsMain => execOutMain,
		execSigsSec => execOutSec,

		branchControl => branchCtrl,
		memoryControl => memoryCtrlE2,

		specialOp => specialOp,

		inputData => renamedDataLivingMerged,
		prevSending => renamedSending,
		prevSendingRe => frontGroupSend,

		acceptAlloc => allocAcceptROB,

		nextAccepting => commitAccepting,

		sendingOut => robSending, 
        robOut => robOut,
        outputArgInfoI => renamedArgsIntROB,
        outputArgInfoF => renamedArgsFloatROB,

		outputSpecial => specialOutROB,

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
       signal slotSelI0, slotIssueI0, slotRegReadI0,
              slotSelI1, slotIssueI1, slotRegReadI1,
              slotSelM0, slotIssueM0, slotRegReadM0,
              slotSelF0, slotIssueF0, slotRegReadF0,

              slotSel4, slotIssue4, slotSel5, slotIssue5, slotSel6, slotIssue6,

              slotSelIntSV, slotIssueIntSV, slotRegReadIntSV,
              slotSelFloatSV, slotIssueFloatSV, slotRegReadFloatSV
                        : SchedulerState := DEFAULT_SCHED_STATE;

       signal regValsI0, regValsI1, regValsM0, regValsS0, regValsE, regValsFloatA, regValsFloatB, regValsFloatC, regValsFS0, regValsF0: MwordArray(0 to 2) := (others => (others => '0'));
       signal readyRegFlagsInt_Early, readyRegFlagsInt_Early_Mem, readyRegFlagsInt_Early_Mem2, readyRegFlagsInt_C, readyRegFlagsFloat_Early, readyRegFlagsInt_T, readyRegFlagsFloat_T,
              readyRegFlagsIntNext_Early, readyRegFlagsIntNext_C, readyRegFlagsSV, readyRegFlagsSV2, readyRegFlagsFloatNext_Early, readyRegFlagsFloatSV, readyRegFlagsFloatSV2
              : std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');

       signal newIntSources, newFloatSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

       -- Issue control 
       signal issuedStoreDataInt, issuedStoreDataFP, allowIssueStoreDataInt, lockIssueSVI, lockIssueSVF, allowIssueStoreDataFP, allowIssueStageStoreDataFP,
              memSubpipeSent, mulSubpipeSent, mulSubpipeAtE0, fp0subpipeSelected, mulSubpipeSelected,
              lockIssueI0, allowIssueI0, lockIssueI1, lockIssueI1_Alt, allowIssueI1, lockIssueM0, allowIssueM0, lockIssueF0, allowIssueF0, intWriteConflict,
              storeValueCollision1, storeValueCollision2, cancelledSVI1,
              memFail, memDepFail, prevMemDepFail: std_logic := '0';  

       signal subpipeI0_Issue, subpipeI0_RegRead, subpipeI0_E0,                                    subpipeI0_D0,
              subpipeI1_Issue, subpipeI1_RegRead, subpipeI1_E0,  subpipeI1_E1,    subpipeI1_E2,    subpipeI1_D0,  subpipeI1_D1,
              
                                                                                  subpipeI1_E2_Alt,
              
              subpipeM0_Issue, subpipeM0_RegRead, subpipeM0_E0,  subpipeM0_E1,    subpipeM0_E2,
                                   subpipeM0_RRi, subpipeM0_E0i, subpipeM0_E1i,   subpipeM0_E2i,   subpipeM0_D0i,-- subpipeM0_D1i,
                                   subpipeM0_RRf, subpipeM0_E0f, subpipeM0_E1f,   subpipeM0_E2f,   subpipeM0_D0f, subpipeM0_D1f,
                                                            
                                                            subpipeM0_E1_u, subpipeM0_E1i_u, subpipeM0_E1f_u,

              subpipeF0_Issue, subpipeF0_RegRead, subpipeF0_E0,    subpipeF0_E1,      subpipeF0_E2,      subpipeF0_D0,
                                           subpipeF0_RRu,
              subpipe_DUMMY
                : ExecResult := DEFAULT_EXEC_RESULT;

        signal unfoldedAluOp: work.LogicExec.AluControl := work.LogicExec.DEFAULT_ALU_CONTROL;     
        signal aluMask, mulMask, memMask, fpMask, intStoreMask, fpStoreMask, branchMask, sqMask, lqMask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');

        signal bypassInt, bypassFloat, bypassIntSV, bypassFloatSV: BypassState := DEFAULT_BYPASS_STATE;

        signal valuesInt0, valuesInt1, valuesFloat0, valuesFloat1: MwordArray(0 to 2) := (others => (others => '0'));

        signal issueTagI0: SmallNumber := sn(0);


        function TMP_clearFull(ss: SchedulerState; evts: EventState) return SchedulerState is
            variable res: SchedulerState := ss;
        begin
            if evts.lateEvent = '1' then
                res.full := '0';
            end if;
            
            return res;
        end function;

            signal ch_a, ch_m, ch_si, ch_sf, ch_f: std_logic := '0';              
    begin
        newIntSources <= TMP_getPhysicalArgsNew(renamedArgsInt);
        newFloatSources <= TMP_getPhysicalArgsNew(renamedArgsFloat);

        aluMask <= getAluMask1(renamedDataLivingRe);
        mulMask <= getMulMask1(renamedDataLivingRe);
        memMask <= getMemMask1(renamedDataLivingRe);
        fpMask <= getFpMask1(renamedDataLivingRe);
        intStoreMask <= getIntStoreMask1((renamedDataLivingRe));
        fpStoreMask <= getFloatStoreMask1((renamedDataLivingRe));
        sqMask <= getStoreMask1((renamedDataLivingRe));
        lqMask <= getLoadMask1((renamedDataLivingRe));
        branchMask <= getBranchMask1((renamedDataLivingRe));


        SUBPIPE_ALU: block
            use work.LogicIssue.all;

            signal outSigsI0: IssueQueueSignals := (others => '0');

            signal schedInfoA, schedInfoUpdatedA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));

            constant CFG_ALU: SchedulerUpdateConfig := (true, false, false, FORWARDING_MODES_INT_D, false);
            constant CFG_ALU_WAIT: SchedulerUpdateConfig := (false, false, false, FORWARDING_MODES_INT_D, false);
            constant CFG_ALU_SEL: SchedulerUpdateConfig :=  (false, false, false, FORWARDING_MODES_INT, false);

            signal dataToAlu: ExecResult := DEFAULT_EXEC_RESULT;           
        begin
            wups <= work.LogicIssue.getInitWakeups(schedInfoA, bypassInt, CFG_ALU);

            schedInfoA <= getIssueInfoArray(TMP_recodeALU(renamedDataLivingRe), aluMask, true, removeArg2(renamedArgsInt), TMP_renamedDests, TMP_renamedSources);
            schedInfoUpdatedA <= updateSchedulerArray_N(schedInfoA, wups, memFail, CFG_ALU);
            schedInfoUpdatedU <= prepareNewArr( schedInfoUpdatedA, readyRegFlagsInt_Early );

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
                schedulerOut => slotSelI0,
                outputSignals => outSigsI0, 
                dbState => dbState
            );

            TMP_ISSUE_I0: block
                use work.LogicIssue.all;
                signal argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                slotIssueI0 <= updateDispatchArgs_Is_N(argStateI, outSigsI0);
                slotRegReadI0 <= updateDispatchArgs_RR(argStateR, valuesInt0, regValsI0, false); -- TODO: include outSigs (.killFollower)? 

                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateI <= TMP_clearFull(getDispatchArgValues_Is(slotSelI0, outSigsI0.sending), events);
                        argStateR <= TMP_clearFull(getDispatchArgValues_RR(slotIssueI0, slotIssueI0.full, valuesInt0, valuesInt1, true, false), events);
                        unfoldedAluOp <= work.LogicExec.getAluControl(slotIssueI0.st.operation.arith);
                    end if;
                end process;

                subpipeI0_Issue <= makeExecResult(slotIssueI0);
                subpipeI0_RegRead <= makeExecResult(slotRegReadI0);

                issueTagI0 <= slotIssueI0.destTag;
                
                
                bqCompareEarly.full <= slotIssueI0.full and slotIssueI0.st.branchIns;
                bqCompareEarly.tag <= slotIssueI0.st.tags.renameIndex;
                bqCompareEarly.dest <= slotIssueI0.st.tags.bqPointer;
            end block;
                                         --   killFollower => (sentTrial2_T and events.execEvent) or events.lateEvent
            dataToAlu <= executeAlu(slotRegReadI0.full and not killFollower(outSigsI0.trialPrev2, events), slotRegReadI0, bqSelected.nip, dataToBranch.controlInfo, unfoldedAluOp);
            process (clk)
            begin
                if rising_edge(clk) then
                    subpipeI0_E0 <= dataToAlu;
                end if;
            end process;

            dataToBranch <= basicBranch(slotRegReadI0.full and not killFollower(outSigsI0.trialPrev2, events) and not lateEventSignal and slotRegReadI0.st.branchIns,
                                        slotRegReadI0,
                                        bqSelected.tags, bqSelected.controlInfo, bqSelected.target, bqSelected.nip,                            
                                        unfoldedAluOp);
            process (clk)
                use work.LogicLogging.all;
            begin
                if rising_edge(clk) then
                    branchResultE0 <= dataToBranch;

                    if dataToBranch.controlInfo.full = '1' then
                        DB_reportBranchEvent(dataToBranch);
                    end if;

                    execEventSignalE1 <= execEventSignalE0 and not lateEventSignal; -- Don't allow subsequent event from cancelled branch
                    branchResultE1 <= branchResultE0;
                end if;
            end process;

            execEventSignalE0 <= branchResultE0.controlInfo.full and branchResultE0.controlInfo.newEvent;

            bqUpdate.full <= branchResultE0.controlInfo.full;
            bqUpdate.tag <= branchResultE0.tags.renameIndex;
            bqUpdate.value <= branchResultE0.target;

            execCausingDelayedSQ.dest <= branchResultE1.tags.sqPointer;
            execCausingDelayedLQ.dest <= branchResultE1.tags.lqPointer;
        end block;


            MUL_BLOCK: if true generate
                SUBPIPE_MUL: block
                   use work.LogicIssue.all;

                   signal outSigsI1: IssueQueueSignals := (others => '0');

                   signal schedInfoA, schedInfoUpdatedA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
                   signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));

                   constant CFG_MUL: SchedulerUpdateConfig := (true, false, false, FORWARDING_MODES_INT_D, false);

                   signal divUnlock, divUnlock_Alt, sendingToMultiplier, killFollowerNextI1: std_logic := '0';
                begin
                    wups <= getInitWakeups(schedInfoA, bypassInt, CFG_MUL);

                    schedInfoA <= getIssueInfoArray(TMP_recodeMul(renamedDataLivingRe), mulMask, true, removeArg2(renamedArgsInt), TMP_renamedDests, TMP_renamedSources);
                    schedInfoUpdatedA <= updateSchedulerArray_N(schedInfoA, wups, memFail, CFG_MUL);
                    schedInfoUpdatedU <= prepareNewArr( schedInfoUpdatedA, readyRegFlagsInt_Early );

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
                        schedulerOut => slotSelI1,
                        outputSignals => outSigsI1,
                        dbState => dbState
                    );

                    TMP_ISSUE_I1: block
                        use work.LogicIssue.all;
                        signal argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
                    begin
                        slotIssueI1 <= updateDispatchArgs_Is_N(argStateI, outSigsI1);
                        slotRegReadI1 <= updateDispatchArgs_RR(argStateR, valuesInt0, regValsI1, false);

                        process (clk)
                        begin
                            if rising_edge(clk) then
                                argStateI <= TMP_clearFull(getDispatchArgValues_Is(slotSelI1, outSigsI1.sending), events);
                                argStateR <= TMP_clearFull(getDispatchArgValues_RR(slotIssueI1, slotIssueI1.full, valuesInt0, valuesInt1, true, false), events);
                            end if;
                        end process;

                        subpipeI1_Issue <= makeExecResult(slotIssueI1);
                        subpipeI1_RegRead <= makeExecResult(slotRegReadI1);
                    end block;

                    sendingToMultiplier <= slotRegReadI1.full and not --outSigsI1.killFollower;
                                                                      killFollower(outSigsI1.trialPrev2, events);
                    
                    subpipeI1_E2 <= subpipeI1_E2_Alt;
                    lockIssueI1 <= lockIssueI1_Alt;

                    divUnlock <= divUnlock_Alt;

                        killFollowerNextI1 <= killFollower(outSigsI1.trialPrev1, events);

                    MUL_DIV: entity work.MultiplierDivider
                    port map (
                        clk => clk,

                        prevSending => sendingToMultiplier,
                        preInput => slotIssueI1,
                        input => slotRegReadI1,

                        allowIssueI1 => allowIssueI1,
                        killFollowerNext => killFollowerNextI1,--outSigsI1.killFollowerNext,

                        events => events,
                        
                        lockIssueI1_Alt => lockIssueI1_Alt,

                        sending => open,
                        divUnlock_Alt => divUnlock_Alt,
                        
                        outStage0 => subpipeI1_E0,
                        outStage1 => subpipeI1_E1,
                        output => subpipeI1_E2_Alt
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
--                readyRegFlagsInt_Early_Mem2 <= --readyRegFlagsInt_Early;
--                                              (readyRegFlagsInt_Early(0), readyRegFlagsInt_Early(1 + QQQ), readyRegFlagsInt_Early(2 - QQQ),
--                                               readyRegFlagsInt_Early(3), readyRegFlagsInt_Early(4 + QQQ), readyRegFlagsInt_Early(5 - QQQ),
--                                               readyRegFlagsInt_Early(6), readyRegFlagsInt_Early(7 + QQQ), readyRegFlagsInt_Early(8 - QQQ),
--                                               readyRegFlagsInt_Early(9), readyRegFlagsInt_Early(10 + QQQ), readyRegFlagsInt_Early(11 - QQQ)
--                                                );
                readyRegFlagsInt_Early_Mem <= reorder(readyRegFlagsInt_Early);
                ch0 <= bool2std(readyRegFlagsInt_Early_Mem = readyRegFlagsInt_Early_Mem2);
            end block;
            
        SUBPIPE_MEM: block
            use work.LogicIssue.all;

            signal outSigsM0: IssueQueueSignals := (others => '0');

            signal schedInfoA, schedInfoUpdatedA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));

            constant CFG_MEM: SchedulerUpdateConfig := (true, false, false, FORWARDING_MODES_INT_D, false);

            signal controlToM0_E0, ctrlE0, ctrlE1, ctrlE1u, ctrlE2: ControlPacket := DEFAULT_CONTROL_PACKET;
            signal slotRegReadM0iq, slotRegReadM0mq: SchedulerState := DEFAULT_SCHED_STATE;
            signal resultToM0_E0, resultToM0_E0i, resultToM0_E0f: ExecResult := DEFAULT_EXEC_RESULT;
        begin
            wups <= work.LogicIssue.getInitWakeups(schedInfoA, bypassInt, CFG_MEM);

            schedInfoA <= getIssueInfoArray(renamedDataLivingRe, memMask, true, removeArg2(swapArgs12(renamedArgsMerged)), TMP_renamedDests, TMP_renamedSources);         
            schedInfoUpdatedA <= updateSchedulerArray_N(schedInfoA, wups, memFail, CFG_MEM);
            schedInfoUpdatedU <= prepareNewArr( schedInfoUpdatedA, readyRegFlagsInt_Early_Mem );

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
                schedulerOut => slotSelM0,
                outputSignals => outSigsM0,            
                dbState => dbState
            );

            TMP_ISSUE_M0: block
                use work.LogicIssue.all;
                signal argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                slotIssueM0 <= updateDispatchArgs_Is_N(argStateI, outSigsM0);
                slotRegReadM0iq <= updateDispatchArgs_RR(argStateR, valuesInt0, regValsM0, false, true);

                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateI <= TMP_clearFull(getDispatchArgValues_Is(slotSelM0, outSigsM0.sending), events);
                        argStateR <= TMP_clearFull(getDispatchArgValues_RR(slotIssueM0, slotIssueM0.full, valuesInt0, valuesInt1, true, false, true), events);
                    end if;
                end process;

                subpipeM0_Issue <= makeExecResult(slotIssueM0);
                subpipeM0_RegRead <= makeExecResult(slotRegReadM0);               
            end block;


            ---------------------------------------------
            -- RR --
            mqRegReadSending <= mqReexecCtrlRR.controlInfo.full;
            slotRegReadM0mq <= TMP_slotRegReadM0mq(mqReexecCtrlRR, mqReexecResRR, mqRegReadSending);

            -- Merge IQ with MQ
            slotRegReadM0 <= mergeMemOp(slotRegReadM0iq, slotRegReadM0mq, mqRegReadSending);

            ----------------------------
            -- Single packet of information for E0
            resultToM0_E0 <= calcEffectiveAddress(slotRegReadM0.full and not killFollower(outSigsM0.trialPrev2, events) and not lateEventSignal, slotRegReadM0, mqRegReadSending);
            resultToM0_E0i <= updateMemDest(resultToM0_E0, slotRegReadM0.argSpec.intDestSel);
            resultToM0_E0f <= updateMemDest(resultToM0_E0, slotRegReadM0.argSpec.floatDestSel);

            controlToM0_E0.controlInfo.full <= slotRegReadM0.full;
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
                signal memoryCtrlPre: InstructionControlInfo := DEFAULT_CONTROL_INFO;                
            begin
                memLoadReady <= dvalid; -- In
                memLoadValue <= din;    -- In
            
                memResult <= getLSResultData_result(  ctrlE1.op,
                                                      memLoadReady, memLoadValue,
                                                      sysRegSending, sysRegReadValue,
                                                      ctOutSQ, ctOutLQ).value;
    
                memoryCtrlPre <= getLSResultData(   ctrlE1.op,
                                                    subpipeM0_E1.value,
                                                    '1', memLoadReady, sysRegSending,
                                                    ctOutSQ, ctOutLQ);
                ctrlE1u.tags <= ctrlE1.tags;
                ctrlE1u.op <= ctrlE1.op;
                ctrlE1u.controlInfo <= memoryCtrlPre;

                memoryMissed <= ctrlE1u.controlInfo.dataMiss or ctrlE1u.controlInfo.sqMiss;
                                --not memLoadReady;
    
                subpipeM0_E1_u <= setMemFail(subpipeM0_E1, memoryMissed and bool2std(ENABLE_MQ), memResult);     
                subpipeM0_E1i_u <= setMemFail(subpipeM0_E1i, memoryMissed and bool2std(ENABLE_MQ), memResult);
                subpipeM0_E1f_u <= setMemFail(subpipeM0_E1f, memoryMissed and bool2std(ENABLE_MQ), memResult);
    
                memFail <= subpipeM0_E1_u.failed;


                missedMemResultE1 <= TMP_missedMemResult(subpipeM0_E1, memoryMissed, memResult);    -- for MQ             
                missedMemCtrlE1 <= TMP_missedMemCtrl(subpipeM0_E1, subpipeM0_E1f, ctrlE1, ctrlE1u, resOutSQ); -- MQ
            end block;

            --------------------------------------------

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


            memoryCtrlE2 <= ctrlE2.controlInfo;  -- for ROB

            memCtrlE0 <= ctrlE0; -- Interface

            memoryRead <= subpipeM0_E0; -- Out
        end block;

        ------------------------

        SUBPIPES_STORE_VALUE: block
            use work.LogicIssue.all;
       
            signal sendingToRegReadI, sendingToRegReadF, sendingToRegReadIntSV-- sendingToRegReadFloatSV
            : std_logic := '0';
            signal schedInfoIntA, schedInfoUpdatedIntA, schedInfoUpdatedIntU, schedInfoFloatA, schedInfoUpdatedFloatA, schedInfoUpdatedFloatU: SchedulerInfoArray(0 to PIPE_WIDTH-1)
                        := (others => DEFAULT_SCHEDULER_INFO);

            constant CFG_SVI: SchedulerUpdateConfig := (true, false, true, FORWARDING_MODES_SV_INT_D, false);
            constant CFG_SVF: SchedulerUpdateConfig := (true, true, true, FORWARDING_MODES_SV_FLOAT_D, false);

            signal wupsInt, wupsFloat: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
            
            signal stateExecStoreValue: SchedulerState := DEFAULT_SCHED_STATE;
            
            signal sendingToStoreWrite, sendingToStoreWriteInt, sendingToStoreWriteFloat: std_logic := '0';
            
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
        begin
            wupsInt <= getInitWakeups(schedInfoIntA, bypassIntSV, CFG_SVI);
            wupsFloat <= getInitWakeups(schedInfoFloatA, bypassFloatSV, CFG_SVF);

            schedInfoIntA <= getIssueInfoArray(renamedDataLivingRe, intStoreMask, false, useStoreArg2(swapArgs12(renamedArgsInt)), TMP_renamedDests, TMP_renamedSources);
            schedInfoUpdatedIntA <= updateSchedulerArray_N(schedInfoIntA, wupsInt, memFail, CFG_SVI);
            schedInfoFloatA <= getIssueInfoArray(renamedDataLivingRe, fpStoreMask, false, useStoreArg2(swapArgs12(renamedArgsFloat)), TMP_renamedDests, TMP_renamedSources);
            schedInfoUpdatedFloatA <= updateSchedulerArray_N(schedInfoFloatA, wupsFloat, memFail, CFG_SVF);

            schedInfoUpdatedIntU <= prepareNewArr( schedInfoUpdatedIntA, readyRegFlagsSV );
            schedInfoUpdatedFloatU <= prepareNewArr( schedInfoUpdatedFloatA, readyRegFlagsFloatSV );


--            readyRegFlagsSV2 <= (readyRegFlagsInt_Early(2 - QQQ), '0', '0',
--                                readyRegFlagsInt_Early(5 - QQQ), '0', '0',
--                                readyRegFlagsInt_Early(8 - QQQ), '0', '0',
--                                readyRegFlagsInt_Early(11 - QQQ), '0', '0');
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
                schedulerOut => slotSelIntSV,
                outputSignals => outSigsSVI,
                dbState => dbState
            );


            TMP_ISSUE_SVI: block
                use work.LogicIssue.all;
                signal argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin

                -- Reg
                slotIssueIntSV <= updateDispatchArgs_Is(argStateI);
                -- pseudo interface
                
                       cancelledSVI1 <= outSigsSVI.cancelled or (storeValueCollision2 and killFollower(outSigsSVI.trialPrev2, events)); -- If stalled, it stayed here but kill sig moved to next stage
                sendingToRegReadIntSV <= slotIssueIntSV.full and not (cancelledSVI1 or killFollower(outSigsSVI.trialPrev1, events));--outSigsSVI.killFollowerNext);
                -- Reg
                slotRegReadIntSV <= updateDispatchArgs_RR(argStateR, valuesInt0, regValsS0, true);
    
                process (clk)
                begin
                    if rising_edge(clk) then
                        if allowIssueStoreDataInt = '1' then -- nextAccepting
                            argStateI <= getDispatchArgValues_Is(slotSelIntSV, outSigsSVI.sending);
                        end if;
                        
                        if events.lateEvent = '1' then
                            argStateI.full <= '0';
                        end if;
    
                        if true then -- nextAccepting
                            argStateR <= TMP_clearFull(getDispatchArgValues_RR(slotIssueIntSV, sendingToRegReadIntSV, valuesInt0, valuesInt1, false, true), events);
                        end if;
 
                    end if;
                end process;

            end block;

            sendingToStoreWriteInt <= slotRegReadIntSV.full and not killFollower(outSigsSVI.trialPrev2, events);--outSigsSVI.killFollower;

            ------------------------------------
--            readyRegFlagsFloatSV2 <= (readyRegFlagsFloat_Early(2 - QQQ), '0', '0',
--                                     readyRegFlagsFloat_Early(5 - QQQ), '0', '0',
--                                     readyRegFlagsFloat_Early(8 - QQQ), '0', '0',
--                                     readyRegFlagsFloat_Early(11 - QQQ), '0', '0');
                            readyRegFlagsFloatSV <= reorder(readyRegFlagsFloat_Early);
            
                ch1 <= bool2std(readyRegFlagsSV2 = readyRegFlagsSV);
                ch2 <= bool2std(readyRegFlagsFloatSV2 = readyRegFlagsFloatSV);
            
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
                    schedulerOut => slotSelFloatSV,              
                    outputSignals => outSigsSVF,
                    dbState => dbState
                );       
            end generate;

            TMP_ISSUE_SVF: block
                use work.LogicIssue.all;
                signal argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                slotIssueFloatSV <= updateDispatchArgs_Is_N(argStateI, outSigsSVF);
                slotRegReadFloatSV <= updateDispatchArgs_RR(argStateR, valuesFloat0, regValsFS0, true);
    
                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateI <= TMP_clearFull(getDispatchArgValues_Is(slotSelFloatSV, outSigsSVF.sending), events);
                        argStateR <= TMP_clearFull(getDispatchArgValues_RR(slotIssueFloatSV, slotIssueFloatSV.full, valuesFloat0, valuesFloat1, false, true), events);
                    end if;
                end process;
            end block;

            sendingToStoreWriteFloat <= slotRegReadFloatSV.full and not killFollower(outSigsSVF.trialPrev2, events);--outSigsSVF.killFollower;

            stateExecStoreValue <= slotRegReadFloatSV when slotRegReadFloatSV.full = '1' else slotRegReadIntSV;
            sendingToStoreWrite <= sendingToStoreWriteInt or sendingToStoreWriteFloat;

            sqValueResultRR.full <= sendingToStoreWrite;
            sqValueResultRR.tag <= stateExecStoreValue.st.tags.renameIndex;
            sqValueResultRR.dest <= stateExecStoreValue.st.tags.sqPointer;
            sqValueResultRR.value <= stateExecStoreValue.args(0);
        end block;


        SUBPIPE_FP0: if ENABLE_FP generate
            use work.LogicIssue.all;
            
            signal outSigsF0: IssueQueueSignals := (others => '0');

            signal schedInfoA, schedInfoUpdatedA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1);

            constant CFG_FP0: SchedulerUpdateConfig := (true, true, false, FORWARDING_MODES_FLOAT_D, false);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
        begin
            wups <= getInitWakeups(schedInfoA, bypassFloat, CFG_FP0);

            schedInfoA <= getIssueInfoArray(TMP_recodeFP(renamedDataLivingRe), fpMask, false, renamedArgsFloat, TMP_renamedDests, TMP_renamedSources);
            schedInfoUpdatedA <= updateSchedulerArray_N(schedInfoA, wups, memFail, CFG_FP0);
            schedInfoUpdatedU <= prepareNewArr( schedInfoUpdatedA, readyRegFlagsFloat_Early );

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
                schedulerOut => slotSelF0,
                outputSignals => outSigsF0,
                dbState => dbState
            );
           

            TMP_ISSUE_F0: block
                use work.LogicIssue.all;

                signal argStateI, argStateR: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin    
                slotIssueF0 <= updateDispatchArgs_Is_N(argStateI, outSigsF0);
                slotRegReadF0 <= updateDispatchArgs_RR(argStateR, valuesFloat0, regValsF0, false);
    
                process (clk)
                begin
                    if rising_edge(clk) then
                        argStateI <= TMP_clearFull(getDispatchArgValues_Is(slotSelF0, outSigsF0.sending), events);
                        argStateR <= TMP_clearFull(getDispatchArgValues_RR(slotIssueF0, slotIssueF0.full, valuesFloat0, valuesFloat1, false, false), events); 
                    end if;
                end process;
    
                subpipeF0_Issue <= makeExecResult(slotIssueF0);
                subpipeF0_RegRead <= makeExecResult(slotRegReadF0);
            end block;

            subpipeF0_RRu.full <= slotRegReadF0.full and not killFollower(outSigsF0.trialPrev2, events);--outSigsF0.killFollower;
            subpipeF0_RRu.tag <= slotRegReadF0.st.tags.renameIndex;
            subpipeF0_RRu.dest <= slotRegReadF0.argSpec.dest;
            subpipeF0_RRu.value <= executeFpu(slotRegReadF0);

            process (clk)
            begin
                if rising_edge(clk) then
                    subpipeF0_E0 <= subpipeF0_RRu;
                    subpipeF0_E1 <= subpipeF0_E0;
                    subpipeF0_E2 <= subpipeF0_E1;
                end if;
            end process;
            
         end generate;
         
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

         lockIssueSVI <= storeValueCollision1 or memFail;
         lockIssueSVF <= storeValueCollision1 or memFail;

         allowIssueStoreDataInt <= not lockIssueSVI;
         allowIssueStoreDataFP <= not lockIssueSVF;

         -------------------------------------------
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

        memSubpipeSent <= slotRegReadM0.full;
        mulSubpipeSent <= slotRegReadI1.full;
        mulSubpipeAtE0   <= subpipeI1_E0.full;

        mulSubpipeSelected <= slotIssueI1.full;
        fp0subpipeSelected <= slotIssueF0.full;

        lockIssueI0 <= memSubpipeSent or memFail or mulSubpipeAtE0;

        -- Issue locking:
        --     if F0 issued, to avoid WB collisions with FP load
        --     if MQ intends to reexecute
        lockIssueM0 <= fp0subpipeSelected or mqReady or memFail  or almostFullMQ or mulSubpipeAtE0; --CAREFUL: this if mul sends result to write queue after D0, 1 cycle later than Mem pipe
                       --     memFail;
        lockIssueF0 <= '0' or memFail;

        allowIssueI0 <= not lockIssueI0;
        allowIssueI1 <= not lockIssueI1;         
        allowIssueM0 <= not lockIssueM0;     
        allowIssueF0 <= not lockIssueF0;

        branchCtrl <= branchResultE0.controlInfo;

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
            
            FNI_VALUES: block
                signal fni, fniFloat: ForwardingInfo := DEFAULT_FORWARDING_INFO;
            begin
                fni <= buildForwardingNetwork(subpipeI0_E0,        subpipeI0_D0,
                                              subpipeI1_D0,        subpipeI1_D1,
                                              subpipeM0_E2i,       subpipeM0_D0i,
                                              memFail, memDepFail
                                             );

                fniFloat <= buildForwardingNetworkFP(subpipeF0_E2,        subpipeF0_D0,
                                                     DEFAULT_EXEC_RESULT, DEFAULT_EXEC_RESULT,
                                                     subpipeM0_D0f,       subpipeM0_D1f,
                                                     memFail, memDepFail
                                                    );

                valuesInt0 <= fni.values0;
                valuesInt1 <= fni.values1;
                valuesFloat0 <= fniFloat.values0;
                valuesFloat1 <= fniFloat.values1;
            end block;

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
             readyRegFlagsInt_T <= updateArgStates(renamedArgsInt, renamedArgsFloat, readyRegFlagsIntNext_Early);
              --         readyRegFlagsInt_C <= updateArgStates(renamedDataLivingRe_C, renamedArgsInt, renamedArgsFloat, readyRegFlagsIntNext_C);
             readyRegFlagsFloat_T <= updateArgStatesFloat(renamedArgsInt, renamedArgsFloat, readyRegFlagsFloatNext_Early);
        end generate;

        RR_FLAGS_TEMP: for i in 0 to PIPE_WIDTH-1 generate
            readyRegFlagsInt_Early(3*i to 3*i + 2 - QQQ) <= readyRegFlagsIntNext_Early(3*i to 3*i + 2 - QQQ);
            
            readyRegFlagsFloat_Early(3*i to 3*i + 2 - QQQ) <= readyRegFlagsFloatNext_Early(3*i to 3*i + 2 - QQQ);
        end generate;

--            readyRegFlagsInt_Early(0 to 2 - QQQ) <= readyRegFlagsIntNext_Early(0 to 2 - QQQ);
--            readyRegFlagsInt_Early(3 to 5 - QQQ) <= readyRegFlagsIntNext_Early(3 to 5 - QQQ);
--            readyRegFlagsInt_Early(6 to 8 - QQQ) <= readyRegFlagsIntNext_Early(6 to 8 - QQQ);
--            readyRegFlagsInt_Early(9 to 11 - QQQ) <= readyRegFlagsIntNext_Early(9 to 11 - QQQ);
            
--            readyRegFlagsFloat_Early(0 to 2 - QQQ) <= readyRegFlagsFloatNext_Early(0 to 2 - QQQ);
--            readyRegFlagsFloat_Early(3 to 5 - QQQ) <= readyRegFlagsFloatNext_Early(3 to 5 - QQQ);
--            readyRegFlagsFloat_Early(6 to 8 - QQQ) <= readyRegFlagsFloatNext_Early(6 to 8 - QQQ);
--            readyRegFlagsFloat_Early(9 to 11 - QQQ) <= readyRegFlagsFloatNext_Early(9 to 11 - QQQ);

        sysRegSending <= sysRegRead;

    end block; -- TEMP_EXEC


    QUEUE_MASKS: block
    begin
        renamedDataToBQ <= setFullMask(renamedDataLivingRe, getBranchMask1(renamedDataLivingRe));

        branchMaskOO <= getBranchMask1(renamedDataLivingRe);
        loadMaskOO <= getLoadMask1(renamedDataLivingRe);
        storeMaskOO <= getStoreMask1(renamedDataLivingRe);
        
        systemStoreMaskOO <= getStoreSysMask(renamedDataLivingRe);
        systemLoadMaskOO <= getLoadSysMask(renamedDataLivingRe);
     
        commitMaskSQ <= work.LogicQueues.getCommittedMask(robOut, false);
        commitMaskLQ <= work.LogicQueues.getCommittedMask(robOut, true);
        commitEffectiveMaskSQ <= work.LogicQueues.getCommittedEffectiveMask(robOut, false);
        commitEffectiveMaskLQ <= work.LogicQueues.getCommittedEffectiveMask(robOut, true);
    
        branchCommitMask <= work.LogicQueues.getCommittedMaskBr(robOut);
        branchCommitEffectiveMask <= work.LogicQueues.getCommittedEffectiveMaskBr(robOut);
    end block;

    BRANCH_QUEUE: entity work.BranchQueue
	generic map(
		QUEUE_SIZE => BQ_SIZE
	)
	port map(
		clk => clk,
		reset => '0',
		en => '0',

		acceptingOut => open,
		
		acceptingBr => bqAccepting,
		
		prevSending => renamedSending,
	    prevSendingBr => bpSending,
	    
	    prevSendingRe => renameSendingBr,
	    
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
        commitEffectiveMask => branchCommitEffectiveMask,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalE1,
		execCausing => DEFAULT_EXEC_RESULT,
		nextAccepting => commitAccepting,		
		sendingSQOut => open,
		
		committedDataOut => bqTargetData,
		
		dbState => dbState
	);

    STORE_QUEUE: entity work.StoreQueue(Behavioral)
	generic map(
		QUEUE_SIZE => SQ_SIZE
	)
	port map(
		clk => clk,
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
        commitMask => commitMaskSQ,
        commitEffectiveMask => commitEffectiveMaskSQ,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalE1,
		execCausing => execCausingDelayedSQ,
		
		nextAccepting => commitAccepting,

        committedEmpty => sbEmpty,
        committedSending => sbSending,
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
        commitMask => commitMaskLQ,
        commitEffectiveMask => commitEffectiveMaskLQ,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignalE1,
		execCausing => execCausingDelayedLQ,
		
		nextAccepting => commitAccepting,
		
        committedEmpty => open,
        committedSending => open,
        
        dbState => dbState
	);


    process (clk)
    begin
        if rising_edge(clk) then
            -- delayed Store Data op
            sqValueResultE2 <= sqValueResultE1;
            sqValueResultE1 <= sqValueResultE0;
            sqValueResultE0 <= sqValueResultRR;
        
            -- MQ inputs
            missedMemResultE2 <= missedMemResultE1;
            missedMemCtrlE2 <= missedMemCtrlE1;
        
            -- MQ outputs
            mqReexecCtrlRR <= mqReexecCtrlIssue;
            mqReexecResRR <= mqReexecResIssue;
        end if;
    end process;

    MQ_BLOCK: if ENABLE_MQ generate 
        LOAD_MISS_QUEUE: entity work.StoreQueue(MissQueue)
        generic map(
            QUEUE_SIZE => 8
            )
        port map(
            clk => clk,
            reset => '0',
            en => '0',
    
            acceptingOut => acceptingMQ,
            almostFull => almostFullMQ,
            acceptAlloc => open,
    
            prevSendingRe => '0',                
            prevSending => '0',
    
            renameMask => (others => '0'),
            inputMask => (others => '0'),
            systemMask => (others => '0'),
    
            renamedPtr => open,
    
            storeValueResult => sqValueResultE2,
    
            compareAddressEarlyInput => DEFAULT_EXEC_RESULT,
            compareAddressEarlyInput_Ctrl => memCtrlRR, -- only 'tag' and 'full'
    
            compareAddressInput => missedMemResultE2,
            compareAddressCtrl => missedMemCtrlE2,
            
            selectedDataOutput => mqReexecCtrlIssue,
            selectedDataResult => mqReexecResIssue,
    
            committing => '0',
            commitMask => (others => '0'),
            commitEffectiveMask => (others => '0'),
    
            lateEventSignal => lateEventSignal,
            execEventSignal => execEventSignalE1,
            execCausing => execCausingDelayedLQ,
    
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
		signal sysStoreAddressW: Mword := (others => '0');
	begin
		doutadr <= ctOutSB.target;
		dwrite <= sbSending and ctOutSB.controlInfo.full and isStoreMemOp(ctOutSB.op);
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
                    logEvent(eventLog, lateEventSignal, execEventSignalE0, frontEventSignal, stallDetected, cycleCount);
                 end if;
            end if;
        end process;
        
        dbState.dbSignal <= stallDetected;
        
	end generate;
	-- pragma synthesis on

end Behavioral;
