----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 22:32:16
-- Design Name: 
-- Module Name: Core - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
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
                frontEventSignal, bqAccepting, bqSending, acceptingSQ, almostFullSQ, acceptingLQ, almostFullLQ: std_logic := '0';
    signal bpData: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal frontDataLastLiving, renamedDataLiving, renamedDataLivingFloat, renamedDataMerged, dataOutROB, renamedDataToBQ, renamedDataToSQ, renamedDataToLQ, bqData: 
                InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal bqCompare, bqSelected, bqUpdate, sqValueInput, sqAddressInput, sqSelectedOutput, lqAddressInput, lqSelectedOutput: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    
    signal execOutputs1, execOutputs2: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);    

    signal execEventSignal, lateEventSignal, lateEventSetPC, sendingBranchIns: std_logic := '0';
    signal robSending, robAccepting, renamedSending, commitAccepting, 
                iqAccepting, iqAcceptingA, iqAcceptingC, iqAcceptingE,
                robAcceptingMore, iqAcceptingMoreA, iqAcceptingMoreC, iqAcceptingMoreE: std_logic := '0';
    signal commitGroupCtr, commitCtr, commitGroupCtrInc: InsTag := (others => '0');
    signal newIntDests, newFloatDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal intSignal: std_logic := '0';
    signal intType: std_logic_vector(0 to 1) := (others => '0');
    signal sysRegReadValue: Mword := (others => '0');
    signal sysRegReadSel: slv5 := (others => '0');
    
    signal sbSending, sbEmpty, sysRegRead, sysRegSending: std_logic := '0';
    signal dataFromSB: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);
    
    function mergeDests(dataInt: InstructionSlotArray; dataFloat: InstructionSlotArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := dataInt;
    begin
        for i in res'range loop
            if dataFloat(i).ins.physicalArgSpec.floatDestSel = '1' then
                --res(i).ins.physicalArgSpec.dest := dataFloat(i).ins.physicalArgSpec.dest;            
                res(i).ins.physicalArgSpec.dest := dataFloat(i).ins.physicalArgSpec.dest;
                res(i).ins.physicalArgSpec.floatDestSel := dataFloat(i).ins.physicalArgSpec.floatDestSel;
            end if;
        end loop;
        return res;
    end function; 
begin

    intSignal <= int0 or int1;
    intType <= (int0, int1);

	UNIT_SEQUENCER: entity work.UnitSequencer(Behavioral)
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
        ---
        dataFromBQV => bqData,

        sbSending => sbSending,
        dataFromSB => dataFromSB(0).ins,
        sbEmpty => sbEmpty,

        commitGroupCtrOut => commitGroupCtr,
        commitCtrOut => commitCtr,
        commitGroupCtrIncOut => commitGroupCtrInc,
        
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
    
        bpAccepting => bqAccepting,--bpAccepting,
        bpSending => bpSending,
        bpData => bpData,
    
        renameAccepting => iqAccepting,            
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
        
        renamedDataLiving => renamedDataLiving,
        renamedDataLivingFloat => renamedDataLivingFloat,        
        renamedSending => renamedSending,

        robDataLiving => dataOutROB,
        sendingFromROB => robSending,
        
        newPhysDestsOut => newIntDests,
        newFloatDestsOut => newFloatDests,
            
        commitGroupCtr => commitGroupCtr,
        commitCtr => commitCtr,
		
		execCausing => execCausing,
        lateCausing => lateCausing,
        
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal
    );

    -- CAREFUL, TODO: this must block renaming of a group if there will be no place for it in IQs the next cycle;
    --                Because stalling at Rename is illegal, sending to Rename has to depend on free slots 
    --                after accounting for current group at Rename that will use some resources!  
    iqAccepting <= 
    (not isNonzero(extractFullMask(renamedDataLiving))
        and robAccepting and iqAcceptingA and acceptingSQ and acceptingLQ and renameAccepting)
    or (robAcceptingMore and iqAcceptingMoreA and not almostFullSQ and not almostFullLQ and renameAccepting);
    
    
    renamedDataMerged <= mergeDests(renamedDataLiving, renamedDataLivingFloat);
    

	REORDER_BUFFER: entity work.ReorderBuffer(Behavioral)
	port map(
		clk => clk, reset => '0', en => '0',
		
		lateEventSignal => lateEventSignal,
		
		commitGroupCtr => commitGroupCtr,

		execEndSigs1 => execOutputs1,
		execEndSigs2 => execOutputs2,
		
		inputData => renamedDataMerged,
		prevSending => renamedSending,
		acceptingOut => robAccepting,
		acceptingMore => robAcceptingMore,
		
		nextAccepting => commitAccepting,
		sendingOut => robSending, 
		outputData => dataOutROB		
	);


    TEMP_EXEC: block
        use work.LogicExec.all;
        -- Selection from IQ and state after Issue stage
        signal slotSelI0, slotIssueI0,
               slotSelI1, slotIssueI1,
               slotSelM0, slotIssueM0,
                slotSel3, slotIssue3,
                slotSel4, slotIssue4, slotSel5, slotIssue5, slotSel6, slotIssue6: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
        -- Exec stages
        signal slotI0_E0, slotI0_E1, slotI0_E2,
               slotI1_E0, slotI1_E1, slotI1_E2,
               slotM0_E0, slotM0_E1i, slotM0_E2i, -- Here logical stages get split into Int and FP
                          slotM0_E1f, slotM0_E2f,
               slot3_E0, slot3_E1,slot3_E2, 
                slot4_E0, slot4_E1, slot4_E2, slot5_E0, slot5_E1, slot5_E2: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
        -- Delay stages - after Exec
        signal slotI0_D0, slotI0_D1,
               slotI1_D0, slotI1_D1,
               slotM0_D0i, slotM0_D1i, -- Continued split of Int and FP
               slotM0_D0f, slotM0_D1f,               
                slot3_D0, slot3_D1, slot4_D0, slot4_D1, slot5_D0, slot5_D1:
                            InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
        
        signal sendingSelI0, sendingIssueI0,
               sendingSelI1, sendingIssueI1,
               sendingSelM0, sendingIssueM0: std_logic := '0';
        signal sendingI0_E0, sendingI0_E1, sendingI0_E2,
               sendingI1_E0, sendingI1_E1, sendingI1_E2,
               sendingM0_E0, sendingM0_E1, sendingM0_E2i,
                                           sendingM0_E2f:
                std_logic := '0';
        signal sendingI0_D0, sendingI0_D1,
               sendingI1_D0, sendingI1_D1,
               sendingM0_D0i, sendingM0_D1i,         
               sendingM0_D0f, sendingM0_D1f: std_logic := '0';                         

           signal schedDataAlu, dataToAluIQ: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
           
           signal dataToAlu, dataToBranch: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
           signal dataToIssueAlu, dataToExecAlu: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;

           signal sendingToIssueAlu, sendingAlu, sendingBranch: std_logic := '0';


           signal schedDataMem, schedDataMemInt, schedDataMemFloat, dataToMemIQ: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
           signal dataToAgu, dataOutMem, dataOutMemDelay: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT); 
           --signal  slotIssueM0: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;


           signal schedDataStoreValue, dataToStoreValueIQ,
                  schedDataStoreValueFloat, dataToStoreValueFloatIQ: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);        
           signal dataToIssueStoreValue, dataToRegReadStoreValue, dataToExecIntStoreValue,
                  dataToIssueFloatStoreValue, dataToRegReadFloatStoreValue, dataToExecFloatStoreValue,
                                                                                    dataToExecStoreValue
                  : SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
                   
        signal  dataInMem0, dataOutMem0, 
                dataInMemInt0, dataOutMemInt0,
                dataInMemFloat0, dataOutMemFloat0,
                
                dataOutMemInt1, dataOutMemFloat1,
                dataInMem1, dataInMemInt1, dataInMemFloat1, dataToStoreValue,
                
                dataOutMemFloatDelay, dataOutMemFloatDelay2,
                dataToIntRF, dataToIntWriteQueue, dataToFloatWriteQueue, dataToFloatRF: InstructionSlotArray(0 to 0)
                                            := (others => DEFAULT_INSTRUCTION_SLOT);
        signal dataFromBranch, lsData: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
        

        signal sendingToAgu, sendingMem0, sendingMem1, sendingToIntRF, sendingFromDLQ,
                sendingToIssueStoreValue, sendingToRegReadStoreValue, sendingStoreValue, sendingToIssueFloatStoreValue: std_logic := '0';
        signal branchData, dataFromDLQ: InstructionState := DEFAULT_INSTRUCTION_STATE;
        signal regsSelA, regsSelC, regsSelD, regsSelFloatA, regsSelFloatC, regsSelFloatD: PhysNameArray(0 to 2) := (others => (others => '0'));
        signal regValsA, regValsB, regValsC, regValsD, regValsE, regValsFloatA, regValsFloatB, regValsFloatC, regValsFloatD: MwordArray(0 to 2) := (others => (others => '0'));
        signal readyRegFlags, readyRegFlagsNext, readyRegFlagsSV, readyFloatFlags, readyFloatFlagsNext, readyRegFlagsFloatSV: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
        
        signal memMask, memMaskInt, memMaskFloat: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        
        signal fni, fniFloat, fniEmpty: ForwardingInfo := DEFAULT_FORWARDING_INFO;

	    signal addressingData: InstructionState := DEFAULT_INSTRUCTION_STATE;
        signal sendingAddressing, memSubpipeSent, lockIssueA, allowIssueA, sendingToIntWriteQueue, memLoadReady: std_logic := '0';
        signal memLoadValue: Mword := (others => '0');
        
        signal sendingIntLoad, sendingFloatLoad, sendingMemFloat1,
                sendingToFloatWriteQueue, sendingToFloatRF, sendingOutAluDelay, sendingOutMemDelay,
                    sendingOutMemFloatDelay, sendingOutMemFloatDelay2: std_logic := '0';
         
        signal issuedStoreDataInt, issuedStoreDataFP, allowIssueStoreDataInt, allowIssueStoreDataFP, allowIssueStageStoreDataFP: std_logic := '0';
        signal intStoreMask, floatStoreMask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
     
    begin
        
        SUBPIPE_ALU: block            
        begin
            schedDataAlu <= getSchedData(extractData(renamedDataLiving), getAluMask(renamedDataLiving));
            dataToAluIQ <= work.LogicIssue.updateSchedulerArray(schedDataAlu, readyRegFlags xor readyRegFlags, fni, ENQUEUE_FN_MAP, true);
            
            IQUEUE_I0: entity work.IssueQueue(Behavioral)--UnitIQ
            generic map(
                IQ_SIZE => 8 --IQ_SIZES(4)
            )
            port map(
                clk => clk, reset => '0', en => '0',
        
                acceptingOut => iqAcceptingA,--iqAcceptingArr(4),
                acceptingMore => iqAcceptingMoreA,
                prevSendingOK => renamedSending,
                newArr => dataToAluIQ,--,schArrays(4),
                fni => fni,
                waitingFM => WAITING_FN_MAP,
                selectionFM => SELECTION_FN_MAP,
                readyRegFlags => readyRegFlags,
                nextAccepting => allowIssueA,--issueAcceptingArr(4),
                execCausing => execCausing,
                lateEventSignal => lateEventSignal,
                execEventSignal => execEventSignal,
                anyReady => open,--iqReadyArr(4),
                schedulerOut => slotSelI0,--dataToIssueAlu,
                sending => sendingSelI0 --sendingToIssueAlu
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
                regValues => regValsA --(others => (others => '0'))     
            );
          
            dataToAlu(0) <= (slotIssueI0.full, executeAlu(slotIssueI0.ins, slotIssueI0.state, bqSelected.ins));
          
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
          
            branchData <= basicBranch(setInstructionTarget(slotIssueI0.ins, 
                                                                      slotIssueI0.ins.constantArgs.imm),
                                             slotIssueI0.state, bqSelected.ins, bqSelected.full);                    
            
            dataToBranch(0) <= (slotIssueI0.full and isBranch(slotIssueI0.ins), branchData);
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
            execCausing <= dataFromBranch.ins;
            bqUpdate <= dataFromBranch;
  
        end block;
         
            
        SUBPIPE_MEM: block
      
        begin
        
           memMaskInt <=  getStoreMask(renamedDataLiving) or getLoadMask(renamedDataLiving);
           memMaskFloat <=  getStoreMask(renamedDataLivingFloat) or getLoadMask(renamedDataLivingFloat);        
           memMask <= memMaskInt or memMaskFloat;
            
           schedDataMem <= getSchedData(removeArg2(extractData(renamedDataLiving)), memMaskInt);

           dataToMemIQ <= work.LogicIssue.updateSchedulerArray(schedDataMem, readyRegFlags xor readyRegFlags, fni, ENQUEUE_FN_MAP, true);        

                    
		   IQUEUE_MEM: entity work.IssueQueue(Behavioral)--UnitIQ
           generic map(
               IQ_SIZE => 8 --IQ_SIZES(4)
           )
           port map(
               clk => clk, reset => '0', en => '0',
       
               acceptingOut => iqAcceptingC,--iqAcceptingArr(4),
               acceptingMore => iqAcceptingMoreC,
               prevSendingOK => renamedSending,
               newArr => dataToMemIQ,--,schArrays(4),
               fni => fni,
               waitingFM => WAITING_FN_MAP,
               selectionFM => SELECTION_FN_MAP,
               readyRegFlags => readyRegFlags,
               nextAccepting => '1',--issueAcceptingArr(4),
               execCausing => execCausing,
               lateEventSignal => lateEventSignal,
               execEventSignal => execEventSignal,
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
               regValues => regValsC --(others => (others => '0'))     
           );
                      
            sendingToAgu <= slotIssueM0.full or sendingFromDLQ;
            
                    sendingFromDLQ <= '0';          -- TEMP!
                    dataFromDLQ <= DEFAULT_INSTRUCTION_STATE; -- TEMP!

	       dataToAgu(0) <= (slotIssueM0.full or sendingFromDLQ,
                                       calcEffectiveAddress(slotIssueM0.ins, slotIssueM0.state, sendingFromDLQ, dataFromDLQ));
       
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

	       lsData <= (sendingM0_E0, setDataCompleted(setAddressCompleted(slotM0_E0(0).ins, '0'), '0'));
           dataInMem0(0) <= lsData;
           sqAddressInput <= lsData; -- TEMP!!
           lqAddressInput <= lsData;

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
                      
	       sendingAddressing <= sendingM0_E1; -- After translation
	       addressingData	<= dataOutMem0(0).ins;

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
		   dread <= '1';
           dadr <= slotM0_E0(0).ins.result;
           sysRegReadSel <= slotM0_E0(0).ins.result(4 downto 0);
           sysRegRead <= sendingM0_E0 and bool2std(slotM0_E0(0).ins.operation = (System, sysMfc));
           
           memLoadReady <= dvalid;              
           memLoadValue <= din;      
        end block;   

        ------------------------
        readyRegFlagsSV <= (readyRegFlags(2), '0', '0', readyRegFlags(5), '0', '0', readyRegFlags(8), '0', '0', readyRegFlags(11), '0', '0');

        SUBPIPES_STORE_VALUE: block
        begin
            
            intStoreMask <= getStoreMask(renamedDataLiving) and not floatStoreMask;                                        
            schedDataStoreValue <= getSchedData(prepareForStoreValueIQ(extractData(renamedDataLiving)), intStoreMask);
            dataToStoreValueIQ <= work.LogicIssue.updateSchedulerArray(schedDataStoreValue, readyRegFlags xor readyRegFlags, fni, ENQUEUE_FN_MAP_SV, true);
            
            floatStoreMask <= getStoreMask(renamedDataLivingFloat);
            schedDataStoreValueFloat <= getSchedData(prepareForStoreValueFloatIQ(extractData(renamedDataLiving), extractData(renamedDataLivingFloat)), floatStoreMask);       
            dataToStoreValueFloatIQ <= work.LogicIssue.updateSchedulerArray(schedDataStoreValueFloat, readyFloatFlags xor readyFloatFlags, fniFloat, ENQUEUE_FN_MAP_FLOAT_SV, true);
                    
            IQUEUE_SV: entity work.IssueQueue(Behavioral)--UnitIQ
            generic map(
                IQ_SIZE => 8 --IQ_SIZES(4)
            )
            port map(
                clk => clk, reset => '0', en => '0',
        
                acceptingOut => iqAcceptingE,--iqAcceptingArr(4),
                acceptingMore => iqAcceptingMoreE,
                prevSendingOK => renamedSending,
                newArr => dataToStoreValueIQ,--,schArrays(4),
                fni => fni,
                waitingFM => WAITING_FN_MAP_SV,
                selectionFM => DEFAULT_FORWARDING_MAP,      
                readyRegFlags => readyRegFlagsSV,
                nextAccepting => allowIssueStoreDataInt,--issueAcceptingArr(4),
                execCausing => execCausing,
                lateEventSignal => lateEventSignal,
                execEventSignal => execEventSignal,
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
    
            REG_READ_STAGE_SV: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => dataToRegReadStoreValue.full,
                nextAccepting => '1',
        
                input => dataToRegReadStoreValue,
                
                acceptingOut => open,
                output => dataToExecIntStoreValue,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniEmpty,
                regValues => regValsD     
            );
    
    
            ------------------------------------
            readyRegFlagsFloatSV <= (readyFloatFlags(2), '0', '0', readyFloatFlags(5), '0', '0', readyFloatFlags(8), '0', '0', readyFloatFlags(11), '0', '0');
    
            IQUEUE_FLOAT_SV: entity work.IssueQueue(Behavioral)--UnitIQ
            generic map(
                IQ_SIZE => 8 --IQ_SIZES(4)
            )
            port map(
                clk => clk, reset => '0', en => '0',
        
                acceptingOut => open,--iqAcceptingE,--iqAcceptingArr(4),
                acceptingMore => open,--iqAcceptingMoreE,
                prevSendingOK => renamedSending,
                newArr => dataToStoreValueFloatIQ,--,schArrays(4),
                fni => fniFloat,
                waitingFM => WAITING_FN_MAP_FLOAT_SV,
                selectionFM => DEFAULT_FORWARDING_MAP,      
                readyRegFlags => readyRegFlagsFloatSV,
                nextAccepting => allowIssueStoreDataFP,--issueAcceptingArr(4),
                execCausing => execCausing,
                lateEventSignal => lateEventSignal,
                execEventSignal => execEventSignal,
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
                nextAccepting => --'1',--
                                  allowIssueStageStoreDataFP,
        
                input => dataToIssueFloatStoreValue,
                
                acceptingOut => open,
                output => dataToRegReadFloatStoreValue,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniEmpty,
                regValues => (others => (others => '0'))   
            );        
    
            REG_READ_STAGE_FLOAT_SV: entity work.IssueStage
            generic map(USE_IMM => false, REGS_ONLY => true)
            port map(
                clk => clk,
                reset => '0',
                en => '0',
        
                prevSending => dataToRegReadFloatStoreValue.full,
                nextAccepting => '1',
        
                input => dataToRegReadFloatStoreValue,
                
                acceptingOut => open,
                output => dataToExecFloatStoreValue,
                
                execEventSignal => execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => execCausing,
                fni => fniEmpty,
                regValues => regValsFloatD     
            );
    
            dataToExecStoreValue <= dataToExecFloatStoreValue when dataToExecFloatStoreValue.full = '1'
                            else    dataToExecIntStoreValue;
        end block;
        
           
         sqValueInput <= -- CAREFUL, TODO: This implies that integer StoreData op value is lost when Int and FP are issued simultaneously. This must be prevented by scheduler!
                    (dataToExecFloatStoreValue.full, setInstructionResult(dataToExecFloatStoreValue.ins, dataToExecFloatStoreValue.state.argValues.arg0)) when dataToExecFloatStoreValue.full = '1' 
            else    (dataToExecIntStoreValue.full, setInstructionResult(dataToExecIntStoreValue.ins, dataToExecIntStoreValue.state.argValues.arg0)); -- TEMP!!
         
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

                  
            -- TEMP:
            SCHED_BLOCK: process(clk)
            begin
                if rising_edge(clk) then
                    assert (sendingI0_E0 and sendingM0_E2i) = '0' report "Int write queue conflict!" severity error;
                    memSubpipeSent <= sendingToAgu;
                end if;
            end process;
            
            lockIssueA <= memSubpipeSent;
            allowIssueA <= not lockIssueA;
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
            
            
            
         -- TODO: add FP outputs!
         execOutputs1(0) <= (sendingI0_E0, slotI0_E0(0).ins);
         execOutputs1(2) <= mergePhysDests(slotM0_E2i(0), slotM0_E2f(0)); --  [dest := Int.dest | Float.dest];
             
                            -- TODO: include mem hit in 'full' flag! Should merge some info from Float path??
                            -- TODO: merge Int and Float stage into one (with dest + destAlt) to avoid unnecessary muxing

         execOutputs2(0) <= (sendingBranch, dataFromBranch.ins);
            execOutputs2(2) <= (dataToExecStoreValue.full, dataToExecStoreValue.ins); -- TODO: mux with FP op

         
         regsSelA <= work.LogicRenaming.getPhysicalArgs((0 => ('1', slotSelI0.ins)));
         regsSelC <= work.LogicRenaming.getPhysicalArgs((0 => ('1', slotSelM0.ins)));        
            -- TEMP!
            regsSelD <= work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToRegReadStoreValue.ins)));
          
          -- Forwarding network
		  fni.nextTagsM1 <= (0 => slotIssueI0.ins.physicalArgSpec.dest, 2 => dataOutMem0(0).ins.physicalArgSpec.dest, others => (others => '0'));        
		  fni.nextTagsM2 <= (2 => slotM0_E0(0).ins.physicalArgSpec.dest, others => (others => '0'));
          fni.tags0 <= (execOutputs1(0).ins.physicalArgSpec.dest, -- ALU
                             execOutputs1(1).ins.physicalArgSpec.dest, slotM0_E2i(0).ins.physicalArgSpec.dest);
          fni.tags1 <= (0 => slotI0_D0(0).ins.physicalArgSpec.dest, 2 => slotM0_D0i(0).ins.physicalArgSpec.dest, others => (others => '0'));
          fni.values0 <= (execOutputs1(0).ins.result, -- ALU
                             execOutputs1(1).ins.result, execOutputs1(2).ins.result);
          fni.values1 <= (0 => slotI0_D0(0).ins.result, 2 => slotM0_D0i(0).ins.result, others => (others => '0'));                 
                
                    
                    
                regsSelFloatD <= work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToRegReadFloatStoreValue.ins)));

                -- NOTE: FP load path is 1 cycle longer, so different stages are involved here from those in Int datapath
                fniFloat.nextTagsM1 <= (2 => slotM0_E2f(0).ins.physicalArgSpec.dest, others => (others => '0')); -- TODO: check!
                fniFloat.nextTagsM2 <= (2 => slotM0_E1f(0).ins.physicalArgSpec.dest, others => (others => '0')); -- TODO: check!                
                fniFloat.tags0 <= (2 => slotM0_D0f(0).ins.physicalArgSpec.dest, others => (others => '0')); -- TODO: check!
                fniFloat.tags1 <= (2 => slotM0_D1f(0).ins.physicalArgSpec.dest, others => (others => '0')); -- TODO: check!
                fniFloat.values0 <= (2 => slotM0_D0f(0).ins.result, others => (others => '0')); -- TODO: check!
                fniFloat.values1 <= (2 => slotM0_D1f(0).ins.result, others => (others => '0')); -- TODO: check!
                    

		 INT_REG_FILE: entity work.RegFile(Behavioral)
         generic map(WIDTH => 4, WRITE_WIDTH => 1)
         port map(
             clk => clk, reset => '0', en => '0',
                 
             writeAllow => sendingToIntRF,
             writeInput => dataToIntRF,
 
             readAllowVec => (others => '1'), -- TEMP!
             
             selectRead(0 to 2) => regsSelA,--(others => (others => '0')),
             selectRead(3 to 5) => (others => (others => '0')),
             selectRead(6 to 8) => regsSelC,
             selectRead(9 to 11) => regsSelD,
             
             readValues(0 to 2) => regValsA,--open,
             readValues(3 to 5) => regValsB,
             readValues(6 to 8) => regValsC,                        
             readValues(9 to 11) => regValsD            
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
             stageDataReserved => renamedDataLiving, --stageDataOutRename,
                 
             -- TODO: change to ins slot based
             writingMask(0) => sendingToIntRF,
             writingData(0) => dataToIntRF(0).ins,
             readyRegFlagsNext => readyRegFlagsNext -- FOR IQs
         );


            sendingToFloatWriteQueue <= sendingM0_E2f; -- TEMP, TODO!
            dataToFloatWriteQueue <= slotM0_E2f;
            
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
             
             selectRead(0 to 2) => (others => (others => '0')),
             selectRead(3 to 5) => (others => (others => '0')),
             selectRead(6 to 8) => (others => (others => '0')),--regsSelC,
             selectRead(9 to 11) => regsSelFloatD,
             
             readValues(0 to 2) => regValsFloatA,--open,
             readValues(3 to 5) => regValsFloatB,
             readValues(6 to 8) => regValsFloatC,                       
             readValues(9 to 11) => regValsFloatD            
         );
         
         FLOAT_READY_TABLE: entity work.RegisterReadyTable(Behavioral)
         generic map(
             WRITE_WIDTH => 1
         )
         port map(
             clk => clk, reset => '0', en => '0', 
             
             sendingToReserve => frontLastSending,
             stageDataToReserve => frontDataLastLiving,
                 
             newPhysDests => newFloatDests,    -- FOR MAPPING
             stageDataReserved => renamedDataLivingFloat, --stageDataOutRename,
                 
             -- TODO: use FP results
             writingMask(0) => sendingToFloatRF,  
             writingData(0) => dataToFloatRF(0).ins,
             readyRegFlagsNext => readyFloatFlagsNext -- FOR IQs
         );

                  
         process(clk)
         begin
            if rising_edge(clk) then
                readyRegFlags <= readyRegFlagsNext;
                readyFloatFlags <= readyFloatFlagsNext;
            end if;
         end process;
         
         sysRegSending <= sysRegRead;
    end block;

    renamedDataToBQ <= setFullMask(renamedDataLiving, getBranchMask(renamedDataLiving));
    renamedDataToSQ <= setFullMask(renamedDataLiving, getStoreMask(renamedDataLiving));
    renamedDataToLQ <= setFullMask(renamedDataLiving, getLoadMask(renamedDataLiving));

    BRANCH_QUEUE: entity work.BranchQueue
	generic map(
		QUEUE_SIZE => 8
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
		dataIn => renamedDataToBQ,
		dataInBr => bpData,

        --- interface with Int Exec
		storeValueInput => bqUpdate,
		compareAddressInput => bqCompare,

		selectedDataOutput => bqSelected,
        ----

		committing => robSending, -- When ROB is sending so is BQ if it has corresponding branches
		groupCtrInc => commitGroupCtrInc,

		lateEventSignal => lateEventSignal,
		execEventSignal => execEventSignal,
		execCausing => execCausing,
		
		nextAccepting => commitAccepting,		
		sendingSQOut => bqSending,
		dataOutV => bqData
	);

    STORE_QUEUE: entity work.StoreQueue(Behavioral)
	generic map(
		QUEUE_SIZE => 8
	)
	port map(
		clk => clk,
		reset => '0',
		en => '0',

		acceptingOut => acceptingSQ,
			almostFull => almostFullSQ,
				
		prevSending => renamedSending,
		dataIn => renamedDataToSQ, -- !!!!!

        -- interface with Exec
		storeValueInput => sqValueInput, 
		compareAddressInput => sqAddressInput,
                            
		selectedDataOutput => sqSelectedOutput,
        ------------

		committing => robSending,
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
		QUEUE_SIZE => 8,
		IS_LOAD_QUEUE => true
	)
	port map(
		clk => clk,
		reset => '0',
		en => '0',

		acceptingOut => acceptingLQ,
			almostFull => almostFullLQ,
				
		prevSending => renamedSending,
		dataIn => renamedDataToLQ, -- !!!!!

        -- interface with Exec
		storeValueInput => DEFAULT_INSTRUCTION_SLOT, 
		compareAddressInput => lqAddressInput,
                            
		selectedDataOutput => lqSelectedOutput,
        ----------------

		committing => robSending,
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
		--memStoreAddress <= dataFromSB(0).ins.target;
		--memStoreValue <= dataFromSB(0).ins.result;
		--memStoreAllow <= sbSending and dataFromSB(0).ins.operation = (Memory, store);
				
		--sysStoreAllow <= sbSending and isSysRegWrite(dataFromSB);

		--sysStoreAddressW <= getStoredArg1(dataFromSB);
		--sysStoreAddress <= sysStoreAddressW(4 downto 0);
		--sysStoreValue <= getStoredArg2(dataFromSB);			

		--dadr <= dataFromSB(0).ins.target;
		doutadr <= dataFromSB(0).ins.target;
		--dread <= memLoadAllow;
		dwrite <= sbSending and bool2std(dataFromSB(0).ins.operation = (Memory, store));
		dout <= dataFromSB(0).ins.result;

	end block;
	
end Behavioral;
