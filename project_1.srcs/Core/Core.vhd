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
    signal frontDataLastLiving, renamedDataLiving, dataOutROB, renamedDataToBQ, renamedDataToSQ, renamedDataToLQ, bqData: 
                InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal bqCompare, bqSelected, bqUpdate, sqValueInput, sqAddressInput, sqSelectedOutput, lqAddressInput, lqSelectedOutput: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    
    signal execOutputs1, execOutputs2: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);    

    signal execEventSignal, lateEventSignal, lateEventSetPC, sendingBranchIns: std_logic := '0';
    signal robSending, robAccepting, renamedSending, commitAccepting, 
                iqAccepting, iqAcceptingA, iqAcceptingC, iqAcceptingE,
                robAcceptingMore, iqAcceptingMoreA, iqAcceptingMoreC, iqAcceptingMoreE: std_logic := '0';
    signal commitGroupCtr, commitCtr, commitGroupCtrInc: InsTag := (others => '0');
    signal newPhysDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal intSignal: std_logic := '0';
    signal intType: std_logic_vector(0 to 1) := (others => '0');
    signal sysStoreValue, sysRegReadValue: Mword := (others => '0');
    signal sysStoreAddress, sysRegReadSel: slv5 := (others => '0');
    
    signal sbSending, sbEmpty, sysWrite, sysRegRead, sysRegSending: std_logic := '0';
    signal dataFromSB: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);
begin

    intSignal <= int0 or int1;
    intType <= (int0, int1);

	UNIT_SEQUENCER: entity work.UnitSequencer(Behavioral)
    port map (
        clk => clk, reset => reset, en => '0',
        
        -- sys reg interface
        sysRegReadSel => sysRegReadSel,
        sysRegReadValue => sysRegReadValue,    
        sysStoreAllow => sysWrite,
        sysStoreAddress => sysStoreAddress,
        sysStoreValue => sysStoreValue,

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

        sbSending => sysWrite,
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
        renamedSending => renamedSending,

        robDataLiving => dataOutROB,
        sendingFromROB => robSending,
        
        newPhysDestsOut => newPhysDests,
            
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
               

	REORDER_BUFFER: entity work.ReorderBuffer(Behavioral)
	port map(
		clk => clk, reset => '0', en => '0',
		
		lateEventSignal => lateEventSignal,
		
		commitGroupCtr => commitGroupCtr,

		execEndSigs1 => execOutputs1,
		execEndSigs2 => execOutputs2,
		
		inputData => renamedDataLiving,
		prevSending => renamedSending,
		acceptingOut => robAccepting,
		acceptingMore => robAcceptingMore,
		
		nextAccepting => commitAccepting,
		sendingOut => robSending, 
		outputData => dataOutROB		
	);


    TEMP_EXEC: block
        use work.LogicExec.all;
    
        signal schedDataAlu, schedDataMem, schedDataStoreValue, dataToIQ, dataToAluIQ, dataToMemIQ, dataToStoreValueIQ: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1)
                            := (others => DEFAULT_SCH_ENTRY_SLOT);
        signal dataToAlu, dataToBranch, dataToAgu, dataOutAlu, dataOutAgu, dataOutAluDelay, dataOutMem, dataInMem0, dataOutMem0, dataOutMem1, 
                dataInMem1, dataToStoreValue,
                dataOutMemDelay, dataToIntRF, dataToIntWriteQueue: InstructionSlotArray(0 to 0)
                                            := (others => DEFAULT_INSTRUCTION_SLOT);
        signal dataFromBranch, lsData: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
        signal dataToIssueAlu, dataToExecAlu, dataToIssueMem, dataToExecMem, dataToIssueStoreValue, dataToRegReadStoreValue, dataToExecStoreValue: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
        signal sendingToIssueAlu, sendingAlu, sendingAgu, sendingToIssueMem, sendingMem, sendingMem0, sendingMem1, sendingToIntRF, sendingBranch, sendingFromDLQ, sendingToAgu,
                sendingToIssueStoreValue, sendingToRegReadStoreValue, sendingStoreValue: std_logic := '0';
        signal branchData, dataFromDLQ: InstructionState := DEFAULT_INSTRUCTION_STATE;
        signal regsSelA, regsSelC, regsSelD: PhysNameArray(0 to 2) := (others => (others => '0'));
        signal regValsA, regValsB, regValsC, regValsD, regValsE: MwordArray(0 to 2) := (others => (others => '0'));
        signal readyRegFlags, readyRegFlagsNext, readyRegFlagsSV: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
        
        signal memMask: std_logic_vector(0 to PIPE_WIDTH-1):= (others => '0');
        
        signal fni, fniEmpty: ForwardingInfo := DEFAULT_FORWARDING_INFO;

	    signal addressingData: InstructionState := DEFAULT_INSTRUCTION_STATE;
        signal sendingAddressing, memSubpipeSent, lockIssueA, allowIssueA, sendingToIntWriteQueue, memLoadReady: std_logic := '0';
        signal memLoadValue: Mword := (others => '0');

    begin
        schedDataAlu <= getSchedData(extractData(renamedDataLiving), getAluMask(renamedDataLiving));
        memMask <=  getStoreMask(renamedDataLiving) or getLoadMask(renamedDataLiving);
        schedDataMem <= getSchedData(removeArg2(extractData(renamedDataLiving)), memMask);
                                        --  prepareForStoreValueIQ - moves arg2 to arg0, removes arg2
        schedDataStoreValue <= getSchedData(prepareForStoreValueIQ(extractData(renamedDataLiving)), getStoreMask(renamedDataLiving));
    
        dataToAluIQ <= work.LogicIssue.updateSchedulerArray(schedDataAlu, readyRegFlags xor readyRegFlags, fni, ENQUEUE_FN_MAP, true);
        dataToMemIQ <= work.LogicIssue.updateSchedulerArray(schedDataMem, readyRegFlags xor readyRegFlags, fni, ENQUEUE_FN_MAP, true);        
        dataToStoreValueIQ <= work.LogicIssue.updateSchedulerArray(schedDataStoreValue, readyRegFlags xor readyRegFlags, fni, ENQUEUE_FN_MAP_SV, true);
    
		IQUEUE_ALU: entity work.IssueQueue(Behavioral)--UnitIQ
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
            schedulerOut => dataToIssueAlu,
            sending => sendingToIssueAlu
        );
 
        ISSUE_STAGE_ALU: entity work.IssueStage
 	    generic map(USE_IMM => true)
        port map(
            clk => clk,
            reset => '0',
            en => '0',
    
            prevSending => sendingToIssueAlu,
            nextAccepting => '1',
    
            input => dataToIssueAlu,
            
            acceptingOut => open,
            output => dataToExecAlu,
            
            execEventSignal => execEventSignal,
            lateEventSignal => lateEventSignal,
            execCausing => execCausing,
            fni => fni,
            regValues => regValsA --(others => (others => '0'))     
        );
      
        dataToAlu(0) <= (dataToExecAlu.full, executeAlu(dataToExecAlu.ins, dataToExecAlu.state, bqSelected.ins));

      
            SUBPIPE_A: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '0'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => dataToAlu(0).full,
                nextAccepting => '1',
                
                stageDataIn => dataToAlu,
                acceptingOut => open,
                sendingOut => sendingAlu,
                stageDataOut => dataOutAlu,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => lateEventSignal,
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );      
      
		branchData <= basicBranch(setInstructionTarget(dataToExecAlu.ins, 
		                                                          dataToExecAlu.ins.constantArgs.imm),
                                         dataToExecAlu.state, bqSelected.ins, bqSelected.full);                    
            
            dataToBranch(0) <= (dataToExecAlu.full and isBranch(dataToExecAlu.ins), branchData);
            sendingBranchIns <= dataToBranch(0).full;
            
            bqCompare <= (sendingBranchIns, dataToExecAlu.ins);
            
            SUBPIPE_D: entity work.GenericStage(Behavioral)
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
            
           --------------------------------------------------------------------------------------- 
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
               schedulerOut => dataToIssueMem,
               sending => sendingToIssueMem
           );
    
           ISSUE_STAGE_MEM: entity work.IssueStage
            generic map(USE_IMM => true)
           port map(
               clk => clk,
               reset => '0',
               en => '0',
       
               prevSending => sendingToIssueMem,
               nextAccepting => '1',
       
               input => dataToIssueMem,
               
               acceptingOut => open,
               output => dataToExecMem,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing,
               
               fni => fni,
               regValues => regValsC --(others => (others => '0'))     
           );
                      
            sendingToAgu <= dataToExecMem.full or sendingFromDLQ;
            
                    sendingFromDLQ <= '0';          -- TEMP!
                    dataFromDLQ <= DEFAULT_INSTRUCTION_STATE; -- TEMP!

	       dataToAgu(0) <= (dataToExecMem.full or sendingFromDLQ,
                                       calcEffectiveAddress(dataToExecMem.ins, dataToExecMem.state, sendingFromDLQ, dataFromDLQ));
       
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
               sendingOut => sendingAgu,
               stageDataOut => dataOutAgu,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing
           );

	       lsData <= (sendingAgu, setDataCompleted(setAddressCompleted(dataOutAgu(0).ins, '0'), '0'));
           dataInMem0(0) <= lsData;
           sqAddressInput <= lsData; -- TEMP!!
           lqAddressInput <= lsData;

           -- TLB lookup, Dcache access
	       STAGE_MEM0: entity work.GenericStage(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
               
               prevSending => sendingAgu,
               nextAccepting => '1',
               
               stageDataIn => dataInMem0,
               acceptingOut => open,
               sendingOut => sendingMem0,
               stageDataOut => dataOutMem0,--dataAfterMemA,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing                
           );
           
	       sendingAddressing <= sendingMem0; -- After translation
	       addressingData	<= dataOutMem0(0).ins;

           dataInMem1(0).full <= sendingMem0;
           dataInMem1(0).ins <= getLSResultData(dataOutMem0(0).ins,
                                                  '1', (others => '0'),
                                                  memLoadReady, memLoadValue,
                                                  sysRegSending, sysRegReadValue, 
                                                  sqSelectedOutput.full, sqSelectedOutput.ins,
                                                  lqSelectedOutput);	       
           -- Source selection and verification
	       STAGE_MEM1: entity work.GenericStage(Behavioral)
           generic map(
               COMPARE_TAG => '1'
           )
           port map(
               clk => clk, reset => reset, en => en,
               
               prevSending => sendingMem0,
               nextAccepting => '1',
               
               stageDataIn => dataInMem1,
               acceptingOut => open,
               sendingOut => sendingMem1,
               stageDataOut => dataOutMem1,--dataAfterMemA,
               
               execEventSignal => execEventSignal,
               lateEventSignal => lateEventSignal,
               execCausing => execCausing                
           );
           
           -- TEMP mem interface    
		   dread <= '1';
           dadr <= dataOutAgu(0).ins.result;
           sysRegReadSel <= dataOutAgu(0).ins.result(4 downto 0);
           sysRegRead <= sendingAgu and bool2std(dataOutAgu(0).ins.operation = (System, sysMfc));
           
           memLoadReady <= dvalid;              
           memLoadValue <= din;      
           

        ------------------------
        readyRegFlagsSV <= (readyRegFlags(2), '0', '0', readyRegFlags(5), '0', '0', readyRegFlags(8), '0', '0', readyRegFlags(11), '0', '0');
        
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
            nextAccepting => '1',--issueAcceptingArr(4),
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
            output => dataToExecStoreValue,
            
            execEventSignal => execEventSignal,
            lateEventSignal => lateEventSignal,
            execCausing => execCausing,
            fni => fniEmpty,
            regValues => regValsD     
        );
           
         sqValueInput <= (dataToExecStoreValue.full, setInstructionResult(dataToExecStoreValue.ins, dataToExecStoreValue.state.argValues.arg0)); -- TEMP!!
            
           --------------------------------------------------------------------------------------- 
            
            SUBPIPE_A_DELAY: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingAlu,
                nextAccepting => '1',
                
                stageDataIn => dataOutAlu,
                acceptingOut => open,
                sendingOut => open,
                stageDataOut => dataOutAluDelay,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );             

            SUBPIPE_C_DELAY: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingMem1,
                nextAccepting => '1',
                
                stageDataIn => dataOutMem1,
                acceptingOut => open,
                sendingOut => open,
                stageDataOut => dataOutMemDelay,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            );         
                  
            -- TEMP:
            SCHED_BLOCK: process(clk)
            begin
                if rising_edge(clk) then
                    assert (sendingAlu and sendingMem1) = '0' report "Int write queue conflict!" severity error;
                    memSubpipeSent <= sendingToAgu;
                end if;
            end process;
            
            lockIssueA <= memSubpipeSent;
            allowIssueA <= not lockIssueA;
            -----
            
            sendingToIntWriteQueue <= sendingAlu or sendingMem1;
            dataToIntWriteQueue <= dataOutMem1 when sendingMem1 = '1' else dataOutAlu;
            
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
            
  
         execOutputs1(0) <= (sendingAlu, dataOutAlu(0).ins);
             execOutputs1(2) <= (sendingMem1, dataOutMem1(0).ins); -- TODO: include mem hit in 'full' flag!

         execOutputs2(0) <= (sendingBranch, dataFromBranch.ins);
            execOutputs2(2) <= (dataToExecStoreValue.full, dataToExecStoreValue.ins);

         
         regsSelA <= work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToIssueAlu.ins)));
         regsSelC <= work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToIssueMem.ins)));        
            -- TEMP!
            regsSelD <= work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToRegReadStoreValue.ins)));
          
          -- Forwarding network
		  fni.nextResultTags <= (0 => dataToExecAlu.ins.physicalArgSpec.dest, 2 => dataOutMem0(0).ins.physicalArgSpec.dest, others => (others => '0'));        
		  fni.nextTagsM2 <= (2 => dataOutAgu(0).ins.physicalArgSpec.dest, others => (others => '0'));
          fni.tags0 <= (execOutputs1(0).ins.physicalArgSpec.dest, -- ALU
                             execOutputs1(1).ins.physicalArgSpec.dest, execOutputs1(2).ins.physicalArgSpec.dest);
          fni.tags1 <= (0 => dataOutAluDelay(0).ins.physicalArgSpec.dest, 2 => dataOutMemDelay(0).ins.physicalArgSpec.dest, others => (others => '0'));
          fni.values0 <= (execOutputs1(0).ins.result, -- ALU
                             execOutputs1(1).ins.result, execOutputs1(2).ins.result);
          fni.values1 <= (0 => dataOutAluDelay(0).ins.result, 2 => dataOutMemDelay(0).ins.result, others => (others => '0'));                 
                
                    
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
                 
             newPhysDests => newPhysDests,    -- FOR MAPPING
             stageDataReserved => renamedDataLiving, --stageDataOutRename,
                 
             -- TODO: change to ins slot based
             writingMask(0) => sendingToIntRF,
             writingData(0) => dataToIntRF(0).ins,
             readyRegFlagsNext => readyRegFlagsNext -- FOR IQs
         );
         
         process(clk)
         begin
            if rising_edge(clk) then
                readyRegFlags <= readyRegFlagsNext;
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

		storeValueInput => bqUpdate,
		compareAddressInput => bqCompare,

		selectedDataOutput => bqSelected,

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

		storeValueInput => sqValueInput, 
		compareAddressInput => sqAddressInput,
                            
		selectedDataOutput => sqSelectedOutput,

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

		storeValueInput => DEFAULT_INSTRUCTION_SLOT, 
		compareAddressInput => lqAddressInput,
                            
		selectedDataOutput => lqSelectedOutput,

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


    sysWrite <= sbSending and bool2std(dataFromSB(0).ins.operation = (System, sysMtc));
    sysStoreAddress <= dataFromSB(0).ins.target(4 downto 0);
    sysStoreValue <= dataFromSB(0).ins.result;

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
