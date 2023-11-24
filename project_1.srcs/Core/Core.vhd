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

    signal frontAccepting,
            bqAccepting, renameAllow, frontSendAllow, frontGroupSend, frontSendingBr,
            allocAcceptAlu, allocAcceptMul, allocAcceptMem, allocAcceptSVI, allocAcceptSVF, allocAcceptF0, allocAcceptSQ, allocAcceptLQ, allocAcceptROB, acceptingMQ, almostFullMQ,
            canSendRename, renamedSending, commitAccepting, robSending,
           mqReady, sbEmpty
           : std_logic := '0';

    signal zerosMask,  aluMaskRe, mulMaskRe, memMaskRe, branchMaskRe, loadMaskRe, storeMaskRe, intStoreMaskRe, floatStoreMaskRe, fpMaskRe,
                        loadMaskOO, storeMaskOO, systemStoreMaskOO, systemLoadMaskOO, 
                        commitEffectiveMaskSQ, commitEffectiveMaskLQ, branchCommitMask
           : std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');

    signal bpData: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);

    signal frontGroupOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);

    signal bqPointer, bqPointerSeq, lqPointer, sqPointer: SmallNumber := (others => '0');

    signal renamedData, renamedDataMerged: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal renamedArgsInt, renamedArgsFloat, renamedArgsMerged, renamedArgsIntROB, renamedArgsFloatROB: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);

    signal TMP_renamedDests: SmallNumberArray(0 to RENAME_W-1) := (others => (others => '0'));
    signal TMP_renamedSources: SmallNumberArray(0 to 3*RENAME_W-1) := (others => (others => '0'));

    signal TMP_aluTags, TMP_mulTags, TMP_memTags, TMP_sviTags, TMP_svfTags, TMP_fpTags,
            TMP_aluTagsPre,  TMP_mulTagsPre, TMP_memTagsPre, TMP_sviTagsPre, TMP_svfTagsPre, TMP_fpTagsPre
            : SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));

    signal renameGroupCtrNext, commitGroupCtr, commitGroupCtrNext: InsTag := (others => '0');

    signal execOutMain, execOutSec: ExecResultArray(0 to 3) := (others => DEFAULT_EXEC_RESULT);

    signal pcData, bpCtrl, frontCtrl, renamedCtrl, ctrlOutROB,
           bqSelected, branchCtrl,       
           mqReexecCtrlIssue, mqReexecCtrlRR,   
           memCtrlRR, memCtrlE0, memoryCtrlE2, missedMemCtrlE1, missedMemCtrlE2,
           ctOutLQ, ctOutSQ, ctOutSB: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal frontEvent, execEvent, lateEvent,
           bqCompareEarly, bqUpdate,
           mqReexecResIssue, mqReexecResRR,
           memoryRead, sysRegReadIn, sysRegReadOut,
           memAddressInputEarly, memAddressInput,
           sqValueResultRR, sqValueResultE0, sqValueResultE1, sqValueResultE2,
           resOutSQ,
           missedMemResultE1, missedMemResultE2,
           bqTargetData,
           defaultExecRes
           : ExecResult := DEFAULT_EXEC_RESULT;

    signal robOut: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);

    signal dispMasks_Actual, dispMasks_N, renamedMasks_Actual, renamedMasks_N, commitMasks_Actual, commitMasks_N: DispatchMasks := DEFAULT_DISPATCH_MASKS;


    signal events, eventsPrev, events_T, events_I: EventState := DEFAULT_EVENT_STATE;
    signal dbState: DbCoreState := DEFAULT_DB_STATE;


    signal lockIssueI0_NoMemFail, dividerSending: std_logic := '0';   

    signal missedMemE0_EP, missedMemE1_EP, missedMemE2_EP: ExecPacket := DEFAULT_EXEC_PACKET;
    signal EP_MQ_Issue, EP_M0_RegRead_copy, EP_M0_E0_copy, EP_M0_E1_copy, EP_M0_E2_copy: ExecPacket := DEFAULT_EXEC_PACKET;

    signal EP_A_Main, EP_A_Sec: ExecPacketArray(0 to 3) := (others => DEFAULT_EXEC_PACKET);

    signal ch0, ch1, ch2, ch3, ch4: std_logic := '0';


    function reorderMemRRF(flags: std_logic_vector) return std_logic_vector is
        variable res: std_logic_vector(0 to 3*PIPE_WIDTH-1) := flags;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            res(3*i + 1) := flags(3*i + 1 + QQQ);
            res(3*i + 2) := flags(3*i + 2 - QQQ);
        end loop;
        
        return res;
    end function;

    function reorderSV(flags: std_logic_vector) return std_logic_vector is
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

    -- TODO: move closer to mem code
    dread <= memoryRead.full;
    dadr <= memoryRead.value;

    SEQUENCING: block
        signal intSignal: std_logic := '0';
        signal intType: std_logic_vector(0 to 1) := (others => '0');
        signal dataFromSB: ExecResult := DEFAULT_EXEC_RESULT;
    begin
        sysRegReadIn.full <= memoryRead.full;
        sysRegReadIn.value <= zeroExtend(memoryRead.value(4 downto 0), MWORD_SIZE);
    
        intSignal <= int0 or int1;
        intType <= (int0, int1);
    
        dataFromSB <= (DEFAULT_DEBUG_INFO, ctOutSB.full and isStoreSysOp(ctOutSB.op), '0', DEFAULT_POISON, InsTag'(others => '0'), zeroExtend(ctOutSB.target(4 downto 0), SMALL_NUMBER_SIZE), ctOutSB.nip);

        SEQUENCER: entity work.UnitSequencer(Behavioral)
        port map (
            clk => clk, reset => reset, en => '0',

            -- Interface with ROB
            commitAccepting => commitAccepting,
            robData => robOut,
            robCtrl => ctrlOutROB,
            ---
            bqTargetData => bqTargetData,

            dataFromSB => dataFromSB,
            sbEmpty => sbEmpty,

            -- sys reg interface
            sysRegReadIn => sysRegReadIn,
            sysRegReadOut => sysRegReadOut,

            -- to front pipe
            pcDataOut => pcData,

            -- Events
            intAllowOut => intallow,
            intAckOut => intack,
            intRejOut => open,
            
            intSignal => intSignal,
            intType => intType,
            
            frontEvent => frontEvent,
            execEvent => execEvent,

            -- Events out
            lateEvent => lateEvent,
            

            commitGroupCtrOut => commitGroupCtr,
            commitGroupCtrNextOut => commitGroupCtrNext,

            doneSig => oaux(0),
            failSig => oaux(1)
        );

    end block;

    iadr <= pcData.ip;
    iadrvalid <= pcData.controlInfo.c_full;


	UNIT_FRONT: entity work.UnitFront(Behavioral)
    port map(
        clk => clk, reset => '0', en => '0',
        events => events,

        frontAccepting => frontAccepting,
        pcDataIn => pcData,
        iin => iin,

        bqAccepting => bqAccepting,
        bpData => bpData,
        bpCtrl => bpCtrl, -- TODO: control packet

        renameAccepting => frontSendAllow,

        dataOut => frontGroupOut,
        ctrlOut => frontCtrl, -- TODO: control packet

        -- Event out
        frontCausing => frontEvent,

        dbState => dbState
    );

    frontGroupSend <= frontCtrl.full;

    frontSendAllow <=   renameAllow 
                    and allocAcceptAlu and allocAcceptMul and allocAcceptMem
                    and allocAcceptSVI and allocAcceptSVF and allocAcceptF0
                    and allocAcceptSQ and allocAcceptLQ and allocAcceptROB;

    REGISTER_MANAGER: entity work.UnitRegManager(Behavioral)
    port map(
        clk => clk,
        events => events, events_T => events_T,

        renameAccepting => renameAllow,
        frontData => frontGroupOut,
        frontCtrl => frontCtrl,

        bqPointer => bqPointer,
        sqPointer => sqPointer,
        lqPointer => lqPointer,
        bqPointerSeq => bqPointerSeq,

        nextAccepting => canSendRename,

        renamedDataLiving => renamedData,
        renamedCtrl => renamedCtrl,

        renamedArgsInt => renamedArgsInt,
        renamedArgsFloat => renamedArgsFloat,

        robData => robOut,  -- TODO: send only commitMasks
        robCtrl => ctrlOutROB,

        commitArgInfoI => renamedArgsIntROB,
        commitArgInfoF => renamedArgsFloatROB,

        renameGroupCtrNextOut => renameGroupCtrNext,

        dbState => dbState
    );

        renamedSending <= renamedCtrl.full;

            dispMasks_Actual <= (
                alu => aluMaskRe,
                mul => mulMaskRe,
                mem => memMaskRe,
                branch => branchMaskRe,
                load => loadMaskRe,
                store => storeMaskRe,
                intStore => intStoreMaskRe,
                floatStore => floatStoreMaskRe,
                fp => fpMaskRe
            );

            dispMasks_N <= getDispatchMasks(frontGroupOut);

                aluMaskRe <= dispMasks_N.alu;
                mulMaskRe <= dispMasks_N.mul;
                memMaskRe <= dispMasks_N.mem;
                branchMaskRe <= dispMasks_N.branch;
                loadMaskRe <= dispMasks_N.load;
                storeMaskRe <= dispMasks_N.store;
                intStoreMaskRe <= dispMasks_N.intStore;
                floatStoreMaskRe <= dispMasks_N.floatStore;
                fpMaskRe <= dispMasks_N.fp;

    canSendRename <= '1';

    renamedArgsMerged <= mergeRenameInfoFP(renamedArgsInt, renamedArgsFloat);
    renamedDataMerged <= replaceDests(renamedData, renamedArgsMerged);


    DEV_NEW_RENAMER: block
        signal 
            TMP_aluTagsT, TMP_mulTagsT, TMP_memTagsT, TMP_sviTagsT, TMP_svfTagsT, TMP_fpTagsT,
            TMP_aluTagsPreT, TMP_mulTagsPreT, TMP_memTagsPreT, TMP_sviTagsPreT, TMP_svfTagsPreT, TMP_fpTagsPreT
            : SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));
    begin
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
    
            renameSending => renamedSending, -- CAREFUL, it's an input
    
            TMP_destsOut => TMP_renamedDests,
            TMP_sourcesOut => TMP_renamedSources,
    
            renameGroupCtrNext => renameGroupCtrNext,
    
            commitGroupCtr => commitGroupCtr,
            commitGroupCtrNext => commitGroupCtrNext,
    
            robSending => robSending,
            commitArgInfoI => renamedArgsIntROB,
    
            dummy => open -- TODO: remove
        );
    end block;

	REORDER_BUFFER: entity work.ReorderBuffer(Behavioral)
	port map(
		clk => clk, reset => '0', en => '0',
        events => events,

		acceptAlloc => allocAcceptROB,
		prevSendingRe => frontGroupSend,

		prevSending => renamedSending,
        inputCtrl => renamedCtrl,
		inputData => renamedDataMerged,

		execSigsMain => execOutMain,
		execSigsSec => execOutSec,

        execMain => EP_A_Main,
        execSec => EP_A_Sec,

		branchControl => branchCtrl,
		memoryControl => memoryCtrlE2,

		nextAccepting => commitAccepting,

		sendingOut => open,--robSending, 
        robOut => robOut,
        outputCtrl => ctrlOutROB,
        
        outputArgInfoI => renamedArgsIntROB,
        outputArgInfoF => renamedArgsFloatROB,

		dbState => dbState	
	);     
    
    robSending <= ctrlOutROB.full;

    ALLOC_MUL_STUB: if not ENABLE_MUL_DIV generate
        allocAcceptMul <= '1';
    end generate;

    ALLOC_FP_STUB: if not ENABLE_FP generate
        allocAcceptSVF <= '1';
        allocAcceptF0 <= '1';
    end generate;


    TEMP_EXEC: block
       use work.LogicIssue.all;
       use work.LogicArgRead.all;
       use work.LogicExec.all;

       -- Selection from IQ and state after Issue stage
       signal slotIssueI0,-- slotRegReadI0,
              slotIssueI1, slotRegReadI1,
              slotIssueM0, slotRegReadM0, slotRegReadM0i, slotRegReadM0f,
              slotIssueF0, slotRegReadF0,

              slotIssueI0_TF, slotIssueI1_TF, slotIssueM0_TF, slotIssueSVI_TF, slotIssueSVF_TF, slotIssueF0_TF,
              slotIssueI0_TS, slotIssueI1_TS, slotIssueM0_TS, slotIssueSVI_TS, slotIssueSVF_TS, slotIssueF0_TS,

              slotSel4, slotIssue4, slotSel5, slotIssue5, slotSel6, slotIssue6,

              slotSelIntSV, slotIssueIntSV,
              slotRegReadIntSV, slotRegReadIntSV_Delay,
              slotSelFloatSV, slotIssueFloatSV, slotRegReadFloatSV
                        : SchedulerState := DEFAULT_SCHED_STATE;

       signal stateExecStoreValue: SchedulerState := DEFAULT_SCHED_STATE;

       -- Issue control 
       signal lockIssueSVI, lockIssueSVF, allowIssueStoreDataInt, allowIssueStoreDataFP, lockIssueI0, allowIssueI0,
              lockIssueI1, allowIssueI1, lockIssueM0, allowIssueM0, lockIssueF0, allowIssueF0,
              issuedIntSV, issuedFloatSV,
              intWriteConflict, storeValueCollisionIssue, storeValueCollisionRR, storeValueCollisionE0, memDepFail, prevMemDepFail: std_logic := '0';

        signal subpipeI0_Issue, subpipeI0_RegRead, subpipeI0_E0,                                    subpipeI0_D0,
                --subpipeI1_Issue, subpipeI1_RegRead, 
                                                    subpipeI1_E0,  subpipeI1_E1,    subpipeI1_E2,    subpipeI1_D0,  subpipeI1_D1,
                                 subpipeM0_RegRead, subpipeM0_E0,  subpipeM0_E1,    subpipeM0_E2,
                                 subpipeM0_RegReadInt, subpipeM0_E0i, subpipeM0_E1i,   subpipeM0_E2i,   subpipeM0_D0i,
                                 subpipeM0_RegReadFloat, subpipeM0_E0f, subpipeM0_E1f,   subpipeM0_E2f,   subpipeM0_D0f, subpipeM0_D1f,

              subpipeF0_RegRead, -- bypass
              subpipeF0_E0,    -- ready reg, bypass?
              subpipeF0_E1,    -- bypass?
              subpipeF0_E2,    -- bypass, values, complete   
              subpipeF0_D0,    -- bypass, values
              subpipe_DUMMY: ExecResult := DEFAULT_EXEC_RESULT;

       signal
               EP_I0_Issue, EP_I0_RegRead, EP_I0_E0, EP_I0_D0, 
               EP_I1_Issue, EP_I1_RegRead, EP_I1_E0, EP_I1_E1, EP_I1_E2, EP_I1_D0, EP_I1_D1,
               EP_M0_Issue, EP_M0_RegRead, EP_M0_E0, EP_M0_E1, EP_M0_E2, EP_M0_D0, EP_M0_D1,
               EP_SVI_Issue, EP_SVI_RegRead, EP_SVI_E0, EP_SVI_D0,
               EP_SVF_Issue, EP_SVF_RegRead, EP_SVF_E0, EP_SVF_D0,
               EP_F0_Issue, EP_F0_RegRead, EP_F0_E0, EP_F0_E1, EP_F0_E2, EP_F0_D0
            : ExecPacket := DEFAULT_EXEC_PACKET;

        signal unfoldedAluOp, unfoldedAluOp_T: work.LogicExec.AluControl := work.LogicExec.DEFAULT_ALU_CONTROL;

        signal bypassInt, bypassFloat, bypassIntSV, bypassFloatSV: BypassState := DEFAULT_BYPASS_STATE;
        signal valuesInt0, valuesInt1, valuesFloat0, valuesFloat1: MwordArray(0 to 2) := (others => (others => '0'));
        signal issueTagI0: SmallNumber := sn(0);

        signal regValsI0, regValsI1, regValsM0, regValsS0, regValsE, regValsFloatA, regValsFloatB, regValsFloatC, regValsFS0, regValsF0: MwordArray(0 to 2) := (others => (others => '0'));
        signal readyRegFlagsInt_Early, readyRegFlagsInt_Early_Mem, readyRegFlagsInt_Early_Mem2, readyRegFlagsInt_C, readyRegFlagsFloat_Early,
               readyRegFlagsIntNext_Early, readyRegFlagsIntNext_C, readyRegFlagsSV, readyRegFlagsSV2, readyRegFlagsFloatNext_Early, readyRegFlagsFloatSV, readyRegFlagsFloatSV2
              : std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');

        signal argValuesIntSV, argValuesIntDelaySV, argValuesFloatSV, argValuesSV: MwordArray(0 to 2) := (others => (others => '0')); 

        signal memFail, memFailSig: std_logic := '0';
            signal ch_a, ch_m, ch_si, ch_sf, ch_f: std_logic := '0';          
    begin
        memFail <= events.memFail;

        SUBPIPE_ALU: block
            signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));

            signal slotRegReadI0_static, slotE0_static, slotD0_static: SchedulerState := DEFAULT_SCHEDULER_STATE;
            signal liveRR: std_logic := '0';

            signal aluResult: Mword := (others => '0');
            signal argValuesInitial, argValuesUpdated: MwordArray(0 to 2) := (others => (others => '0'));
        begin
            schedInfoA <= getIssueInfoArray(renamedData, true, renamedArgsInt, readyRegFlagsInt_Early, TMP_renamedDests, TMP_renamedSources, I0);

            wups <= work.LogicIssue.getInitWakeups(schedInfoA, bypassInt, CFG_ALU);
            schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, memFail, CFG_ALU);

            IQUEUE_I0: entity work.IssueQueue(Behavioral)
            generic map(
                NAME => "I0",
                IQ_SIZE => IQ_SIZE_I0,
                FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
                FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2),
                    WAKEUP_SPEC => WAKEUP_SPEC_I0
            )
            port map(
                clk => clk, reset => '0', en => '0',
                events => events_I,
                events_T => events,--_T,

                accept => allocAcceptAlu,

                inReady => frontGroupSend,
                inMask => aluMaskRe,

                TMP_outTags => TMP_aluTags,
                TMP_outTagsPre => TMP_aluTagsPre,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedU,
                
                bypass => bypassInt,
                unlockDiv => '0',

                nextAccepting => allowIssueI0,
                schedulerOut_Fast => slotIssueI0_TF,
                schedulerOut_Slow => slotIssueI0_TS,
                    outEP => EP_I0_Issue,

                dbState => dbState
            );
 
            slotIssueI0 <= TMP_mergeStatic(slotIssueI0_TF, slotIssueI0_TS);

            subpipeI0_Issue <= makeExecResult(slotIssueI0);
            issueTagI0 <= slotIssueI0.destTag;

            process (clk)
            begin
                if rising_edge(clk) then
                    slotRegReadI0_static <= advanceControlRR(slotIssueI0, slotIssueI0.full, events);
                    argValuesInitial <= getArgValuesRR(slotIssueI0, valuesInt0, valuesInt1, true, false);

                    unfoldedAluOp <= work.LogicExec.getAluControl(slotIssueI0.st.operation.arith);

                    slotE0_static <= advanceControlRR(slotRegReadI0_static, slotRegReadI0_static.full, events);
                    slotD0_static <= advanceControlRR(slotE0_static, slotE0_static.full, events);

                    EP_I0_RegRead <= updateEP(EP_I0_Issue, events);
                    EP_I0_E0 <= updateEP(EP_I0_RegRead, events);
                    EP_I0_D0 <= updateEP(EP_I0_E0, events);

                end if;
            end process;

            argValuesUpdated <= updateArgsRR(slotRegReadI0_static, argValuesInitial, valuesInt0, regValsI0, false);   
            
            subpipeI0_RegRead <= makeExecResult(slotRegReadI0_static);

            liveRR <= updateControlRR(slotRegReadI0_static, (others => '0'), events).full;

            bqCompareEarly <= getBranchCompareEarly(slotRegReadI0_static, liveRR);

                unfoldedAluOp_T <= work.LogicExec.getAluControl(slotRegReadI0_static.st.operation.arith);

            process (clk)
            begin
                if rising_edge(clk) then
                    aluResult <= executeAlu(liveRR, slotRegReadI0_static, argValuesUpdated, bqSelected.nip, unfoldedAluOp).value;
                end if;
            end process;

                subpipeI0_E0.dbInfo <= slotE0_static.st.dbInfo;
                subpipeI0_E0.full <= slotE0_static.full;
                subpipeI0_E0.failed <= '0';
                subpipeI0_E0.poison <= slotE0_static.poison;
                subpipeI0_E0.tag <= slotE0_static.st.tags.renameIndex;
                subpipeI0_E0.dest <= slotE0_static.dest;
                subpipeI0_E0.value <= aluResult;


            JUMPS: block
                signal dataToBranch, branchResultE0, branchResultE1: ControlPacket := DEFAULT_CONTROL_PACKET;
                signal suppressNext1, suppressNext2, lateEventPre: std_logic := '0';
                signal branch0BeforeRR, branch1BeforeRR: std_logic := '0';
            begin

                dataToBranch <= basicBranch(liveRR and slotRegReadI0_static.st.branchIns and not suppressNext1 and not suppressNext2,
                                   -- .st,
                                    slotRegReadI0_static, argValuesUpdated, bqSelected, unfoldedAluOp, events.lateCausing);

                process (clk)
                    use work.LogicLogging.all;
                begin
                    if rising_edge(clk) then
                        if dataToBranch.full = '1' then
                            DB_reportBranchEvent(dataToBranch);
                        end if;

                        branchResultE0 <= dataToBranch;
                        branchResultE1 <= branchResultE0;

                        lateEventPre <= events.lateCausing.full;
                        eventsPrev <= events;
                    end if;
                end process;

                branch0BeforeRR <= compareTagBefore(branchResultE0.tags.renameIndex, slotRegReadI0_static.st.tags.renameIndex);
                branch1BeforeRR <= compareTagBefore(branchResultE1.tags.renameIndex, slotRegReadI0_static.st.tags.renameIndex);
                suppressNext1 <= (branch0BeforeRR and branchResultE0.controlInfo.newEvent) or events.lateCausing.full;
                suppressNext2 <= (branch1BeforeRR and branchResultE1.controlInfo.newEvent) or lateEventPre;

                execEvent <= (DEFAULT_DEBUG_INFO, branchResultE0.controlInfo.newEvent, '0', DEFAULT_POISON, branchResultE0.tags.renameIndex, branchResultE0.tags.bqPointerSeq, branchResultE0.target);

                branchCtrl <= branchResultE0;

                bqUpdate.full <= branchResultE0.controlInfo.c_full;
                bqUpdate.tag <= branchResultE0.tags.renameIndex;
                bqUpdate.value <= branchResultE0.target;

                events <= (dataToBranch.tags, branchResultE0.tags, execEvent, lateEvent, memFailSig);

                events_T <= (eventsPrev.preExecTags, eventsPrev.execTags, eventsPrev.execCausing, eventsPrev.lateCausing, memFailSig);
                events_I <= events;
            end block;
        end block;

        MUL_BLOCK: if ENABLE_MUL_DIV generate
           signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
           signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));

           signal argValuesInitial, argValuesUpdated: MwordArray(0 to 2) := (others => (others => '0'));
           signal stageE0, stageE1, stageE2, stageD0, stageD1: SchedulerState := DEFAULT_SCHEDULER_STATE;

           signal divUnlock: std_logic := '0';
        begin
            schedInfoA <= getIssueInfoArray(renamedData, true, renamedArgsInt, readyRegFlagsInt_Early, TMP_renamedDests, TMP_renamedSources, I1);

            wups <= getInitWakeups(schedInfoA, bypassInt, CFG_MUL);
            schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, memFail, CFG_MUL);

            IQUEUE_I1: entity work.IssueQueue(Behavioral)
            generic map(
                NAME => "I1",
                IQ_SIZE => IQ_SIZE_I0,
                FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
                FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2)
            )
            port map(
                clk => clk, reset => '0', en => '0',
                events => events_I,
                events_T => events,--_T,

                accept => allocAcceptMul,

                inReady => frontGroupSend,
                inMask => mulMaskRe,
                
                TMP_outTags => TMP_mulTags,
                TMP_outTagsPre => TMP_mulTagsPre,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedU,
                
                bypass => bypassInt,
                unlockDiv => divUnlock,

                nextAccepting => allowIssueI1,

                schedulerOut_Fast => slotIssueI1_TF,
                schedulerOut_Slow => slotIssueI1_TS,
                    outEP => EP_I1_Issue,

                dbState => dbState
            );

            slotIssueI1 <= TMP_mergeStatic(slotIssueI1_TF, slotIssueI1_TS);

            process (clk)
            begin
                if rising_edge(clk) then                            
                    slotRegReadI1 <= advanceControlRR(slotIssueI1, slotIssueI1.full, events);
                    argValuesInitial <= getArgValuesRR(slotIssueI1, valuesInt0, valuesInt1, false, false, false);

                    EP_I1_RegRead <= updateEP(EP_I1_Issue, events);

                    EP_I1_D0 <= updateEP(EP_I1_E2, events);
                    EP_I1_D1 <= updateEP(EP_I1_D0, events);

--                            stageE0 <= advanceControlRR(slotRegReadI1, slotRegReadI1.full, events);
--                            stageE1 <= advanceControlRR(stageE0, stageE0.full, events);
--                            stageE2 <= advanceControlRR(stageE1, stageE1.full, events);
--                            stageD0 <= advanceControlRR(stageE2, stageE2.full, events);
--                            stageD1 <= advanceControlRR(stageD0, stageD0.full, events);
                end if;
            end process;

            argValuesUpdated <= updateArgsRR(slotRegReadI1, argValuesInitial, valuesInt0, regValsI1, false, false);

            MUL_DIV: entity work.MultiplierDivider
            port map (
                clk => clk,

                prevSending => slotRegReadI1.full,
                preInput => slotIssueI1,
                input => slotRegReadI1,
                    inputArgs => argValuesUpdated,
                    inputEP => EP_I1_RegRead,

                allowIssueI1 => allowIssueI1,
                killFollowerNext => '0',

                events => events,

                lockIssueI1Out => lockIssueI1,
                divUnlockOut => divUnlock,

                sending => dividerSending,
                    outE0 => EP_I1_E0,
                    outE1 => EP_I1_E1,
                    outE2 => EP_I1_E2,
                outStage0 => subpipeI1_E0,
                outStage1 => subpipeI1_E1,
                output => subpipeI1_E2
            );

        end generate;


        readyRegFlagsInt_Early_Mem <= reorderMemRRF(readyRegFlagsInt_Early);

        SUBPIPE_MEM: block
            signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));

            signal argValuesInitial, argValuesUpdated: MwordArray(0 to 2) := (others => (others => '0'));

            signal stageE0, stageE1, stageE2, stageD0, stageD1: SchedulerState := DEFAULT_SCHEDULER_STATE;
            signal destE0i, destE0f, destE1i, destE1f, destE2i, destE2f, destD0i, destD0f, destD1i, destD1f: PhysName := (others => '0');
            signal resE0i, resE0f, resE1i, resE1f, resE2i, resE2f, resD0i, resD0f, resD1i, resD1f: ExecResult := DEFAULT_EXEC_RESULT;

            signal memResultE0, memResultE1, memResultE2, memResultD0, memResultD1: Mword := (others => '0');

            signal controlToM0_E0, ctrlE0, ctrlE1, ctrlE1u, ctrlE2: ControlPacket := DEFAULT_CONTROL_PACKET;
            signal slotIssueM0mq, slotIssueMerged: SchedulerState := DEFAULT_SCHED_STATE;
            signal resultToM0_E0, resultToM0_E0i, resultToM0_E0f: ExecResult := DEFAULT_EXEC_RESULT;

            signal EP_M0_IssueMQ: ExecPacket := DEFAULT_EXEC_PACKET;

            signal hitInt, hitFloat, failInt, failFloat: std_logic := '0';
            signal loadValueE2: Mword := (others => '0');
        begin
            schedInfoA <= getIssueInfoArray(renamedData, true, renamedArgsMerged, readyRegFlagsInt_Early_Mem, TMP_renamedDests, TMP_renamedSources, M0);         

            wups <= work.LogicIssue.getInitWakeups(schedInfoA, bypassInt, CFG_MEM);
            schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, memFail, CFG_MEM);

            IQUEUE_MEM: entity work.IssueQueue(Behavioral)
            generic map(
               NAME => "M0",
               IQ_SIZE => IQ_SIZE_M0,
               FORWARDING(0 to 2) => FORWARDING_MODES_INT(0 to 2),
               FORWARDING_D(0 to 2) => FORWARDING_MODES_INT_D(0 to 2)
            )
            port map(
                clk => clk, reset => '0', en => '0',
                events => events_I,
                events_T => events,--_T,

                accept => allocAcceptMem,

                inReady => frontGroupSend,
                inMask => memMaskRe,

                TMP_outTags => TMP_memTags,
                TMP_outTagsPre => TMP_memTagsPre,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedU,

                bypass => bypassInt,
                unlockDiv => '0',

                nextAccepting => allowIssueM0,

                schedulerOut_Fast => slotIssueM0_TF,
                schedulerOut_Slow => slotIssueM0_TS,
                    outEP => EP_M0_Issue,

                dbState => dbState
            );

            slotIssueM0 <= TMP_mergeStatic(slotIssueM0_TF, slotIssueM0_TS);

            slotIssueM0mq <= TMP_slotIssueM0mq(mqReexecCtrlIssue, mqReexecResIssue, mqReexecCtrlIssue.controlInfo.c_full);

            slotIssueMerged <= slotIssueM0mq when slotIssueM0mq.full = '1'
                        else   slotIssueM0;

            EP_M0_IssueMQ <= updateEP_Async( makeEP(slotIssueM0mq), events_T);

            process (clk)
                function TMP_clearDestForInt(ss: SchedulerState) return SchedulerState is
                    variable res: SchedulerState := ss;
                begin
                    if res.intDestSel /= '1' then
                        res.dest := (others => '0');
                    end if;
                    return res;
                end function;
                
                function TMP_clearDestForFloat(ss: SchedulerState) return SchedulerState is
                    variable res: SchedulerState := ss;
                begin
                    if res.floatDestSel /= '1' then
                        res.dest := (others => '0');
                    end if;
                    return res;
                end function;
            begin
                if rising_edge(clk) then
                    slotRegReadM0 <= advanceControlRR(slotIssueMerged, slotIssueMerged.full, events);
                        slotRegReadM0i <= TMP_clearDestForInt(advanceControlRR(slotIssueMerged, slotIssueMerged.full, events));
                        slotRegReadM0f <= TMP_clearDestForFloat(advanceControlRR(slotIssueMerged, slotIssueMerged.full, events));


                    if slotIssueM0mq.full = '1' then
                        argValuesInitial <= (mqReexecCtrlIssue.target, (others => '0'), (others => '0'));
                    else
                        argValuesInitial <= getArgValuesRR(slotIssueMerged, valuesInt0, valuesInt1, true, false, true);
                    end if;

                    EP_M0_RegRead <= mergeEP(updateEP(EP_M0_Issue, events),
                                             updateEP(EP_M0_IssueMQ, events)
                                            );

                    EP_M0_E0 <= updateEP(EP_M0_RegRead, events);
                    EP_M0_E1 <= updateEP(EP_M0_E0, events);
                    EP_M0_E2 <= applyFail(updateEP(EP_M0_E1, events), memFailSig);
                    EP_M0_D0 <= updateEP(EP_M0_E2, events);
                    EP_M0_D1 <= updateEP(EP_M0_D0, events);

                    stageE0 <= advanceControlRR(slotRegReadM0, slotRegReadM0.full, events);
                    stageE1 <= advanceControlRR(stageE0, stageE0.full, events); 
                    stageE2 <= advanceControlRR(stageE1, stageE1.full, events); 
                    stageD0 <= advanceControlRR(stageE2, stageE2.full, events); 
                    stageD1 <= advanceControlRR(stageD0, stageD0.full, events); 

                    if slotRegReadM0.full = '1' then
                        if slotRegReadM0.intDestSel = '1' then
                            destE0i <= resultToM0_E0.dest;
                        else
                            destE0i <= (others => '0');               
                        end if;

                        if slotRegReadM0.floatDestSel = '1' then
                            destE0f <= resultToM0_E0.dest;
                        else
                            destE0f <= (others => '0');               
                        end if;
                    else
                        destE0i <= (others => '0');
                        destE0f <= (others => '0');           
                    end if;

                    if stageE0.full = '1' then
                        destE1i <= destE0i;
                        destE1f <= destE0f;
                    else
                        destE1i <= (others => '0');
                        destE1f <= (others => '0');
                    end if;

                    if stageE1.full = '1' then
                        destE2i <= destE1i;
                        destE2f <= destE1f;
                    else
                        destE2i <= (others => '0');
                        destE2f <= (others => '0');
                    end if;

                    if stageE2.full = '1' then
                        destD0i <= destE2i;
                        destD0f <= destE2f;
                    else
                        destD0i <= (others => '0');
                        destD0f <= (others => '0');
                    end if;

                   if stageD0.full = '1' then
                        destD1i <= destD0i;
                        destD1f <= destD0f;
                    else
                        destD1i <= (others => '0');
                        destD1f <= (others => '0');
                    end if;

                end if;
            end process;

            EP_M0_RegRead_copy <= EP_M0_RegRead;
            EP_M0_E0_copy <= EP_M0_E0;
            EP_M0_E1_copy <= EP_M0_E1;
            EP_M0_E2_copy <= EP_M0_E2;

            argValuesUpdated <= updateArgsRR(slotRegReadM0, argValuesInitial, valuesInt0, regValsM0, false, true);

            subpipeM0_RegRead <= makeExecResult(slotRegReadM0);               
                subpipeM0_RegReadInt <= makeExecResult(slotRegReadM0i);               
                subpipeM0_RegReadFloat <= makeExecResult(slotRegReadM0f);               

            ---------------------------------------------
            -- RR --
            -- Single packet of information for E0
            resultToM0_E0 <= calcEffectiveAddress(slotRegReadM0.full, slotRegReadM0, argValuesUpdated, mqReexecCtrlRR.controlInfo.c_full);
            resultToM0_E0i <= updateMemDest(resultToM0_E0, slotRegReadM0.intDestSel);
            resultToM0_E0f <= updateMemDest(resultToM0_E0, slotRegReadM0.floatDestSel);

            controlToM0_E0.full <= slotRegReadM0.full;
            controlToM0_E0.controlInfo.c_full <= slotRegReadM0.full;
            controlToM0_E0.op <= slotRegReadM0.st.operation;
            controlToM0_E0.tags <= slotRegReadM0.st.tags;
            controlToM0_E0.dbInfo <= slotRegReadM0.st.dbInfo;
            --------------------------------------

            memAddressInputEarly <= resultToM0_E0;
            memCtrlRR <= controlToM0_E0; -- Interface LSQ

            ------------------------------------------------
            -- E0 -- 
            memAddressInput <= subpipeM0_E0;  -- Interface LSQ

            --------------------------------------------------------
            -- E1 --

            MEM_RESULTS: block
                signal memLoadReady, memoryMissed: std_logic := '0';
                signal memLoadValue: Mword := (others => '0');
            begin
                memLoadReady <= dvalid; -- In
                memLoadValue <= din;    -- In

                memResultE1 <= getLSResultData_result(  ctrlE1.op,
                                                      memLoadReady, memLoadValue,
                                                      sysRegReadOut.full, sysRegReadOut.value,
                                                      ctOutSQ, ctOutLQ).value;

                ctrlE1u.full <= ctrlE1.full;
                ctrlE1u.tags <= ctrlE1.tags;
                ctrlE1u.op <= ctrlE1.op;
                ctrlE1u.controlInfo <= getLSResultData(ctrlE1.op,
                                                       '1', memLoadReady, sysRegReadOut.full,
                                                       ctOutSQ, ctOutLQ);

                memoryMissed <= ctrlE1u.controlInfo.dataMiss or ctrlE1u.controlInfo.sqMiss;

                memFailSig <= (memoryMissed and subpipeM0_E1.full);
                missedMemE1_EP <= TMP_missedMemResultEP(EP_M0_E1, memoryMissed, memResultE1);
                missedMemResultE1 <= TMP_missedMemResult(subpipeM0_E1, memoryMissed, memResultE1);    -- for MQ             
                missedMemCtrlE1 <= TMP_missedMemCtrl(subpipeM0_E1, subpipeM0_E1f, ctrlE1, ctrlE1u, resOutSQ); -- MQ

                process (clk)
                begin
                    if rising_edge(clk) then              
                        hitInt <= EP_M0_E1.full and stageE1.intDestSel and not memoryMissed;
                        failInt <= EP_M0_E1.full and stageE1.intDestSel and memoryMissed;

                        hitFloat <= EP_M0_E1.full and stageE1.floatDestSel and not memoryMissed;
                        failFloat <= EP_M0_E1.full and stageE1.floatDestSel and memoryMissed;

                        loadValueE2 <= memResultE1;
                    end if;
                end process;
            end block;


            process (clk)
                function TMP_ZZZ(er: ExecResult; memMissed: std_logic) return ExecResult is
                    variable res: ExecResult := setMemFail(er, memMissed and bool2std(ENABLE_MQ), er.value);
                begin
                    res.dest := (others => '0');
                    res.value := (others => '0');
                    return res;
                end function;
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
                    subpipeM0_E2 <= TMP_ZZZ( subpipeM0_E1, memFailSig );         -- injection of mem miss to 'full'

                    memResultE2 <= memResultE1;
                    memResultD0 <= memResultE2;
                    memResultD1 <= memResultD0;
                end if;
            end process;

                        subpipeM0_E2i <= resE2i;
                        subpipeM0_E2f <= resE2f;

            memCtrlE0 <= ctrlE0; -- Interface
            memoryRead <= subpipeM0_E0; -- Out
    
            memoryCtrlE2 <= ctrlE2; -- for ROB

                -- E0
                resE0i.dest <= destE0i;
                resE0f.dest <= destE0f;

                resE0i.value <= memResultE0;
                resE0f.value <= memResultE0;

                -- E1
                resE1i <= makeMemResult(stageE1, stageE1.full and stageE1.intDestSel, '0', destE1i, (others => '0'));
                resE1f <= makeMemResult(stageE1, stageE1.full and stageE1.floatDestSel, '0', destE1f, (others => '0'));

                -- E2
                resE2i <= makeMemResult(stageE2, hitInt,   failInt,   destE2i, loadValueE2);                
                resE2f <= makeMemResult(stageE2, hitFloat, failFloat, destE2f, loadValueE2);
                
                -- D0
                resD0i.dest <= destD0i;
                resD0f.dest <= destD0f;
           
                resD0i.value <= memResultD0;
                resD0f.value <= memResultD0;

                -- D1
                resD1i.dest <= destD1i;
                resD1f.dest <= destD1f;
                
                resD1i.value <= memResultD1;
                resD1f.value <= memResultD1;         

        end block;

        ------------------------

        SUBPIPES_STORE_VALUE: block
            signal schedInfoIntA, schedInfoUpdatedIntU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wupsInt: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
        begin
            wupsInt <= getInitWakeups(schedInfoIntA, bypassIntSV, CFG_SVI);
            schedInfoIntA <= getIssueInfoArray(renamedData, false, renamedArgsInt, readyRegFlagsSV, TMP_renamedDests, TMP_renamedSources, SVI);
            schedInfoUpdatedIntU <= updateOnDispatch(schedInfoIntA, wupsInt, memFail, CFG_SVI);

            readyRegFlagsSV <= reorderSV(readyRegFlagsInt_Early);
 
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
                events => events_I,
                events_T => events,--_T,

                accept => allocAcceptSVI,

                inReady => frontGroupSend,
                inMask => intStoreMaskRe,

                TMP_outTags => TMP_sviTags,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedIntU,

                bypass => bypassIntSV,
                unlockDiv => '0',

                nextAccepting => allowIssueStoreDataInt,

                schedulerOut_Fast => slotIssueSVI_TF,
                schedulerOut_Slow => slotIssueSVI_TS,
                    outEP => EP_SVI_Issue,

                dbState => dbState
            );

            slotIssueIntSV <= TMP_mergeStatic(slotIssueSVI_TF, slotIssueSVI_TS);
            issuedIntSV <= slotIssueIntSV.maybeFull;

            TMP_ISSUE_SVI: block
                signal slotRegReadDelay_static: SchedulerState := DEFAULT_SCHEDULER_STATE;
            begin
                process (clk)
                begin
                    if rising_edge(clk) then                        
                        slotRegReadIntSV <= advanceControlRR(slotIssueIntSV, slotIssueIntSV.full, events);

                        slotRegReadDelay_static <= advanceControlRR(slotRegReadIntSV, slotRegReadIntSV.full, events);
                        argValuesIntDelaySV <= argValuesIntSV;                        
                    end if;
                end process;

                argValuesIntSV <= regValsS0;
                slotRegReadIntSV_Delay <= slotRegReadDelay_static;
            end block;
        end block;


        STORE_VALUE_FLOAT: if ENABLE_FP generate
            signal schedInfoFloatA, schedInfoUpdatedFloatU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wupsFloat: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
        begin
            readyRegFlagsFloatSV <= reorderSV(readyRegFlagsFloat_Early);

            wupsFloat <= getInitWakeups(schedInfoFloatA, bypassFloatSV, CFG_SVF);
            schedInfoFloatA <= getIssueInfoArray(renamedData, false, renamedArgsFloat, readyRegFlagsFloatSV, TMP_renamedDests, TMP_renamedSources, SVF);
            schedInfoUpdatedFloatU <= updateOnDispatch(schedInfoFloatA, wupsFloat, memFail, CFG_SVF);

            IQUEUE_FLOAT_SV: entity work.IssueQueue(Behavioral)
            generic map(
                NAME => "SVF",
                IQ_SIZE => IQ_SIZE_FLOAT_SV, -- CAREFUL: not IS_FP because doesn't have destination
                FORWARDING_D(0 to 2) => FORWARDING_MODES_SV_FLOAT_D(0 to 2),
                IGNORE_MEM_FAIL => true
            )
            port map(
                clk => clk, reset => '0', en => '0',
                events => events_I,
                events_T => events,--_T,

                accept => allocAcceptSVF,

                inReady => frontGroupSend,
                inMask => floatStoreMaskRe,

                TMP_outTags => TMP_svfTags,

                prevSendingOK => renamedSending,
                newArr => schedInfoUpdatedFloatU,

                bypass => bypassFloatSV,
                unlockDiv => '0',

                nextAccepting => allowIssueStoreDataFP,

                schedulerOut_Fast => slotIssueSVF_TF,
                schedulerOut_Slow => slotIssueSVF_TS,           
                    outEP => EP_SVF_Issue,

                dbState => dbState
            );

            slotIssueFloatSV <= TMP_mergeStatic(slotIssueSVF_TF, slotIssueSVF_TS);
            issuedFloatSV <= slotIssueFloatSV.maybeFull;

            process (clk)
            begin
                if rising_edge(clk) then                        
                    slotRegReadFloatSV <= advanceControlRR(slotIssueFloatSV, slotIssueFloatSV.full, events);
                end if;
            end process;

            argValuesFloatSV <= regValsFS0;
        end generate;

        stateExecStoreValue <= slotRegReadIntSV_Delay when storeValueCollisionE0 = '1'
                          else slotRegReadFloatSV when slotRegReadFloatSV.full = '1'
                          else slotRegReadIntSV;

        argValuesSV <= argValuesIntDelaySV when storeValueCollisionE0 = '1'
                  else argValuesFloatSV when slotRegReadFloatSV.full = '1'
                  else argValuesIntSV;

        sqValueResultRR <= convertExecStoreValue(stateExecStoreValue, argValuesSV);


        SUBPIPE_FP0: if ENABLE_FP generate
            signal schedInfoA, schedInfoUpdatedU: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);
            signal wups: WakeupStructArray2D(0 to PIPE_WIDTH-1, 0 to 1) := (others => (others => work.LogicIssue.DEFAULT_WAKEUP_STRUCT));
            
            signal stageE0, stageE1, stageE2, stageD0, stageD1: SchedulerState := DEFAULT_SCHEDULER_STATE;
            signal argValuesInitial, argValuesUpdated: MwordArray(0 to 2) := (others => (others => '0'));
            
            signal subpipeF0_RRu: ExecResult := DEFAULT_EXEC_RESULT;
        begin
            wups <= getInitWakeups(schedInfoA, bypassFloat, CFG_FP0);

            schedInfoA <= getIssueInfoArray(renamedData, false, renamedArgsFloat, readyRegFlagsFloat_Early, TMP_renamedDests, TMP_renamedSources, work.LogicIssue.F0);
            schedInfoUpdatedU <= updateOnDispatch(schedInfoA, wups, memFail, CFG_FP0);
            
            IQUEUE_F0: entity work.IssueQueue(Behavioral)
            generic map(
               NAME => "F0",
               IQ_SIZE => IQ_SIZE_F0,
               FORWARDING(0 to 2) => FORWARDING_MODES_FLOAT(0 to 2),
               FORWARDING_D(0 to 2) => FORWARDING_MODES_FLOAT_D(0 to 2)
            )
            port map(
               clk => clk, reset => '0', en => '0',
               events => events_I,
               events_T => events,--_T,

               accept => allocAcceptF0,

               inReady => frontGroupSend,
               inMask => fpMaskRe,

               TMP_outTags => TMP_fpTags,
               TMP_outTagsPre => TMP_fpTagsPre,

               prevSendingOK => renamedSending,
               newArr => schedInfoUpdatedU,
               bypass => bypassFloat,
               unlockDiv => '0',
               nextAccepting => allowIssueF0,

               schedulerOut_Fast => slotIssueF0_TF,
               schedulerOut_Slow => slotIssueF0_TS,
                    outEP => EP_F0_Issue,

               dbState => dbState
            );
            
            slotIssueF0 <= TMP_mergeStatic(slotIssueF0_TF, slotIssueF0_TS);

            process (clk)
            begin
               if rising_edge(clk) then                       
                   slotRegReadF0 <= advanceControlRR(slotIssueF0, slotIssueF0.full, events);
                   argValuesInitial <= getArgValuesRR(slotIssueF0, valuesFloat0, valuesFloat1, false, false);
                   
                   stageE0 <= advanceControlRR(slotRegReadF0, slotRegReadF0.full, events);
                   stageE1 <= advanceControlRR(stageE0, stageE0.full, events);
                   stageE2 <= advanceControlRR(stageE1, stageE1.full, events);
                   stageD0 <= advanceControlRR(stageE2, stageE2.full, events);
                   stageD1 <= advanceControlRR(stageD0, stageD0.full, events);

                   EP_F0_RegRead <= updateEP(EP_F0_Issue, events);
                   EP_F0_E0 <= updateEP(EP_F0_RegRead, events);
                   EP_F0_E1 <= updateEP(EP_F0_E0, events);
                   EP_F0_E2 <= updateEP(EP_F0_E1, events);
                   EP_F0_D0 <= updateEP(EP_F0_E2, events);
                   --EP_F0_D1 <= updateEP(EP_F0_D0, events);
               end if;
            end process;

            argValuesUpdated <= updateArgsRR(slotRegReadF0, argValuesInitial, valuesFloat0, regValsF0, false, false);

            subpipeF0_RegRead <= makeExecResult(slotRegReadF0);

            subpipeF0_RRu <= TMP_fp(slotRegReadF0.full, slotRegReadF0, argValuesUpdated);

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
            function TMP_clearDestOnFail(er: ExecResult) return ExecResult is
                variable res: ExecResult := er;
            begin
                if (not res.full or res.failed) = '1' then
                    res.dest := (others => '0');
                end if;
                return res;
            end function; 
        begin
            if rising_edge(clk) then
                subpipeI0_D0 <= TMP_clearDestOnFail(subpipeI0_E0);
                
                subpipeI1_D0 <= TMP_clearDestOnFail(subpipeI1_E2);
                subpipeI1_D1 <= TMP_clearDestOnFail(subpipeI1_D0);
                
                subpipeM0_D0i <= TMP_clearDestOnFail(subpipeM0_E2i);
                subpipeM0_D0f <= TMP_clearDestOnFail(subpipeM0_E2f);
                subpipeM0_D1f <= TMP_clearDestOnFail(subpipeM0_D0f);
                
                subpipeF0_D0 <= TMP_clearDestOnFail(subpipeF0_E2);
            end if;
        end process;

        -- StoreData issue control:
        -- When Int and FP store data issue at the same time, the port conflict is resolved thus:
        -- Both IQs are blocked for the next cycle, so combined issue rate is never higher that 1 per cycle
        -- Int op is stalled for 1 cycle at IssueStage - no problems appear with scheduling because this subpipe has no wakeup observers and reads args only form RF
        process (clk)
        begin
           if rising_edge(clk) then
               storeValueCollisionRR <= issuedIntSV and issuedFloatSV;
               storeValueCollisionE0 <= storeValueCollisionRR;

               lockIssueI0_NoMemFail <= slotIssueM0.maybeFull or mqReexecCtrlIssue.controlInfo.c_full or slotRegReadI1.maybeFull or dividerSending;
           end if;
        end process;

        storeValueCollisionIssue <= issuedIntSV and issuedFloatSV;

        lockIssueSVI <= storeValueCollisionIssue or memFail;
        lockIssueSVF <= storeValueCollisionIssue or memFail;

        lockIssueI0 <= lockIssueI0_NoMemFail or memFail;

        -- Issue locking:
        --     if F0 issued, to avoid WB collisions with FP load
        --     if MQ intends to reexecute
        lockIssueM0 <= slotIssueF0.maybeFull or mqReady or memFail or almostFullMQ or subpipeI1_E0.full; --CAREFUL: this if mul sends result to write queue after D0, 1 cycle later than Mem pipe
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

        EP_A_Main <= (EP_I0_E0, EP_I1_E2, EP_M0_E2, EP_F0_E2);
        EP_A_Sec <=  (DEFAULT_EXEC_PACKET, DEFAULT_EXEC_PACKET, DEFAULT_EXEC_PACKET, DEFAULT_EXEC_PACKET);


        bypassInt <= makeBypassInt((subpipeI0_Issue, subpipeI1_E1, subpipeM0_RegReadInt),
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
            signal newIntSources, newFloatSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
            signal newIntDests, newFloatDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
        begin
            newIntSources <= TMP_getPhysicalArgsNew(renamedArgsInt);
            newFloatSources <= TMP_getPhysicalArgsNew(renamedArgsFloat);

            newIntDests <= TMP_getPhysicalDestsNew(renamedArgsInt);
            newFloatDests <= TMP_getPhysicalDestsNew(renamedArgsFloat);

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

                sendingToReserve => renamedSending,
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

                    sendingToReserve => renamedSending,                 
                    newPhysDests => newFloatDests,
                    newPhysSources => newFloatSources,
                    writingData_T(0) => resultToFloatRF_Early,
                    readyRegFlagsNext => readyRegFlagsFloatNext_Early
                );
            end generate;
        end block;

        RR_FLAGS_TEMP: for i in 0 to PIPE_WIDTH-1 generate
            readyRegFlagsInt_Early(3*i to 3*i + 2 - QQQ) <= readyRegFlagsIntNext_Early(3*i to 3*i + 2 - QQQ);
            readyRegFlagsFloat_Early(3*i to 3*i + 2 - QQQ) <= readyRegFlagsFloatNext_Early(3*i to 3*i + 2 - QQQ);
        end generate;

    end block; -- TEMP_EXEC


    QUEUE_MASKS: block
    begin
        -- Renamed
        loadMaskOO <= getLoadMask1(renamedData);
        storeMaskOO <= getStoreMask1(renamedData);

        systemStoreMaskOO <= getStoreSysMask(renamedData);
        systemLoadMaskOO <= getLoadSysMask(renamedData);

           renamedMasks_Actual <= (
                alu => zerosMask,
                mul => zerosMask,
                mem => zerosMask,
                branch => zerosMask,
                load => loadMaskOO,
                store => storeMaskOO,
                intStore => zerosMask,
                floatStore => zerosMask,
                fp => zerosMask
            );

        -- Committing
        commitEffectiveMaskSQ <= work.LogicQueues.getCommittedEffectiveMask(robOut, false);
        commitEffectiveMaskLQ <= work.LogicQueues.getCommittedEffectiveMask(robOut, true);
        branchCommitMask <= work.LogicQueues.getCommittedMaskBr(robOut);

            commitMasks_Actual <= (
                alu => zerosMask,
                mul => zerosMask,
                mem => zerosMask,
                branch => branchCommitMask,
                load => commitEffectiveMaskLQ,
                store => commitEffectiveMaskLQ,
                intStore => zerosMask,
                floatStore => zerosMask,
                fp => zerosMask
            );
    end block;

    frontSendingBr <= frontCtrl.full and frontCtrl.controlInfo.firstBr;

    BRANCH_QUEUE: entity work.BranchQueue
	generic map(
		QUEUE_SIZE => BQ_SIZE
	)
	port map(
		clk => clk, reset => '0', en => '0',
		events => events,

		acceptingBr => bqAccepting,
        dataInBr => bpData,
        ctrlInBr => bpCtrl,

		acceptingOut => open,
        
        renamedPtr => bqPointerSeq,
	    bqPtrOut => bqPointer,

	    frontSending => frontSendingBr,
	    branchMaskFront => branchMaskRe,

		renamedSending => renamedSending,
        renamedCtrl => renamedCtrl,
		renamedDataIn => renamedData,

        compareAddressQuickInput => bqCompareEarly,
        selectedDataOutput => bqSelected,

		storeValueInput => bqUpdate,


		committing => robSending, -- When ROB is sending so is BQ if it has corresponding branches
        commitBr => ctrlOutROB.controlInfo.firstBr,
        commitMask => branchCommitMask,

		nextAccepting => commitAccepting, -- UNUSED
		committedDataOut => bqTargetData,

		dbState => dbState
	);

    STORE_QUEUE: entity work.StoreQueue(Behavioral)
	generic map(
		QUEUE_SIZE => SQ_SIZE
	)
	port map(
		clk => clk, reset => '0', en => '0',
		events => --events,
		              events_T,

		acceptAlloc => allocAcceptSQ,

        renamedPtr => sqPointer,

	    prevSendingRe => frontGroupSend,
	    renameMask => storeMaskRe,

		prevSending => renamedSending,

        inputMask => storeMaskOO,
        systemMask => systemStoreMaskOO,


        compareAddressEarlyInput => memAddressInputEarly,
        compareAddressEarlyInput_Ctrl => memCtrlRR,
            compareAddressEarlyEP => EP_M0_RegRead_copy,

        compareAddressInput => memAddressInput,
        compareAddressCtrl => memCtrlE0,
            compareAddressEP => EP_M0_E0_copy,

        selectedDataOutput => ctOutSQ,
        selectedDataResult => resOutSQ,

        storeValueResult => sqValueResultRR,
            storeValueEP => DEFAULT_EXEC_PACKET,

		committing => robSending,
        commitEffectiveMask => commitEffectiveMaskSQ,

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
		clk => clk, reset => '0', en => '0',
		events => --events,
		              events_T,

		acceptAlloc => allocAcceptLQ,

        renamedPtr => lqPointer,

	    prevSendingRe => frontGroupSend,
		renameMask => loadMaskRe,

		prevSending => renamedSending,

        inputMask => loadMaskOO,
        systemMask => systemLoadMaskOO,

		compareAddressEarlyInput => memAddressInputEarly,
        compareAddressEarlyInput_Ctrl => memCtrlRR,
            compareAddressEarlyEP => EP_M0_RegRead_copy,

		compareAddressInput => memAddressInput,
        compareAddressCtrl => memCtrlE0,
            compareAddressEP => EP_M0_E0_copy,

        selectedDataOutput => ctOutLQ,

        storeValueResult => DEFAULT_EXEC_RESULT,
            storeValueEP => DEFAULT_EXEC_PACKET,

		committing => robSending,
        commitEffectiveMask => commitEffectiveMaskLQ,

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
                missedMemE2_EP <= updateEP(missedMemE1_EP, events);
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
            clk => clk, reset => '0', en => '0',
            events => events,
                      --  events_T,

            acceptAlloc => open,

            renamedPtr => open,

            prevSendingRe => '0',
            renameMask => zerosMask,

            acceptingOut => acceptingMQ,
            almostFull => almostFullMQ,
    
            prevSending => '0',

            inputMask => zerosMask,
            systemMask => zerosMask,

            compareAddressEarlyInput => memAddressInputEarly,--DEFAULT_EXEC_RESULT,
            compareAddressEarlyInput_Ctrl => memCtrlRR, -- only 'tag' and 'full'
                earlyInput => DEFAULT_EXEC_PACKET,
                
            compareAddressInput => missedMemResultE2,
            compareAddressCtrl => missedMemCtrlE2,
                lateInput => EP_M0_E2_copy,

            selectedDataOutput => mqReexecCtrlIssue,
            selectedDataResult => mqReexecResIssue,
                selectedEP => EP_MQ_Issue,

            storeValueResult => sqValueResultE2,

            nextAccepting => '0',

            committing => '0',
            commitMask => zerosMask,
            commitEffectiveMask => zerosMask,

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
                    logEvent(eventLog, events.lateCausing.full, events.execCausing.full, frontEvent.full, stallDetected, cycleCount);
                 end if;
            end if;
        end process;

        dbState.dbSignal <= stallDetected;

	end generate;
	-- pragma synthesis on

end Behavioral;
