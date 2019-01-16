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
                frontEventSignal, bqAccepting, bqSending: std_logic := '0';
    signal bpData: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal frontDataLastLiving, renamedDataLiving, dataOutROB, renamedDataToBQ, bqData: 
                InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal bqCompare, bqSelected, bqUpdate: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
    
    signal execOutputs1, execOutputs2: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);    

    signal execEventSignal, lateEventSignal, lateEventSetPC, sendingBranchIns: std_logic := '0';
    signal robSending, robAccepting, renamedSending, commitAccepting, 
                iqAccepting, iqAcceptingA,
                robAcceptingMore, iqAcceptingMoreA: std_logic := '0';
    --    signal iadrReg: Mword := X"ffffffb0";
    signal commitGroupCtr, commitCtr, commitGroupCtrInc: InsTag := (others => '0');
    signal newPhysDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal intSignal: std_logic := '0';
    signal intType: std_logic_vector(0 to 1) := (others => '0');
begin

    intSignal <= int0 or int1;
    intType <= (int0, int1);

	UNIT_SEQUENCER: entity work.UnitSequencer(Behavioral)
    port map (
        clk => clk, reset => reset, en => '0',
        
        -- sys reg interface
        sysRegReadSel => (others => '0'),--sysRegReadSel,
        sysRegReadValue => open,--sysRegReadValue,    
        sysStoreAllow => '0',--sysStoreAllow,
        sysStoreAddress => (others => '0'),--sysStoreAddress,
        sysStoreValue => (others => '0'),--sysStoreValue,

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
        --start => int1,        
        execEventSignal => execEventSignal,
        execCausing => execCausing,
        
        frontEventSignal => frontEventSignal,
        frontCausing => frontCausing,
        
        -- Events out
        execOrIntEventSignalOut => open,--execOrIntEventSignal,
        execOrIntCausingOut => open,--execOrIntCausing,
        lateEventOut => lateEventSignal,
        lateEventSetPC => lateEventSetPC,
        lateCausing => lateCausing,
        -- Data from front pipe interface        
 --       renameAccepting => open,--renameAccepting, -- to frontend
        frontLastSending => frontLastSending,
        frontDataLastLiving => frontDataLastLiving,

--        -- Interface from register mapping
--        newPhysDestsIn => (others => (others => '0')),--newPhysDests,
--        newPhysDestPointerIn => (others => '0'),--newPhysDestPointer,
--        newPhysSourcesIn => (others => (others => '0')),--newPhysSources,

        -- Interface with IQ
--        iqAccepts => iqAccepting,
--        renamedDataLiving => open,--renamedDataLiving, -- !!!
--        renamedSending => open,--renamedSending,
        
        -- Interface from ROB
        commitAccepting => commitAccepting,
        sendingFromROB => robSending,    
        robDataLiving => dataOutROB,

        ---
        dataFromBQV => bqData,

        sbSending => '0',--sbSending,
        dataFromSB => DEFAULT_INSTRUCTION_STATE,--dataFromSB,
        sbEmpty => '1',--sbEmpty,

        -- Interface from committed stage
        committedSending => open,--committedSending,
        committedDataOut => open,--committedDataOut,
        renameLockEndOut => open,--renameLockEnd,
                
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
        and robAccepting and iqAcceptingA and renameAccepting)
    or (robAcceptingMore and iqAcceptingMoreA and renameAccepting);
               

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
        signal schedData, dataToIQ: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1)
                            := (others => DEFAULT_SCH_ENTRY_SLOT);
        signal dataToAlu, dataToBranch, dataOutAlu, dataOutAluDelay, dataToIntRF: InstructionSlotArray(0 to 0)
                                            := (others => DEFAULT_INSTRUCTION_SLOT);
        signal dataFromBranch: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
        signal dataToIssue, dataToExec: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
        signal sendingToIssue, sendingAlu, sendingToIntRF, sendingBranch: std_logic := '0';
        signal branchData: InstructionState := DEFAULT_INSTRUCTION_STATE;
        signal regsSelA: PhysNameArray(0 to 2) := (others => (others => '0'));
        signal regValsA, regValsB, regValsC, regValsD: MwordArray(0 to 2) := (others => (others => '0'));
        signal readyRegFlags, readyRegFlagsNext: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
        
        signal fni: ForwardingInfo := DEFAULT_FORWARDING_INFO;
    begin
        schedData <= getSchedData(extractData(renamedDataLiving), getAluMask(renamedDataLiving));
    
        dataToIQ <= work.LogicIssue.updateForWaitingArrayNewFNI(schedData, readyRegFlags xor readyRegFlags, fni);
    
		IQUEUE: entity work.IssueQueue(Behavioral)--UnitIQ
        generic map(
            IQ_SIZE => 8 --IQ_SIZES(4)
        )
        port map(
            clk => clk, reset => '0', en => '0',
    
            --acceptingVec => open,--iqAcceptingVecArr(i),
            acceptingOut => iqAcceptingA,--iqAcceptingArr(4),
            acceptingMore => iqAcceptingMoreA,
            prevSendingOK => renamedSending,
            --newData => dataToQueuesArr(i),
                newArr => dataToIQ,--,schArrays(4),
            fni => fni,
            readyRegFlags => readyRegFlags,
            nextAccepting => '1',--issueAcceptingArr(4),
            execCausing => execCausing,
            lateEventSignal => lateEventSignal,
            execEventSignal => execEventSignal,
            anyReady => open,--iqReadyArr(4),
            schedulerOut => dataToIssue,
            sending => sendingToIssue
        );
 
        ISSUE_STAGE: entity work.IssueStage
 	    generic map(USE_IMM => true)
        port map(
            clk => clk,
            reset => '0',
            en => '0',
    
            prevSending => sendingToIssue,
            nextAccepting => '1',
    
            input => dataToIssue,
            
            acceptingOut => open,
            output => dataToExec,
            
            execEventSignal => execEventSignal,
            lateEventSignal => lateEventSignal,
            execCausing => execCausing,
            
                fni => fni,
            
            --resultTags: in PhysNameArray(0 to N_RES_TAGS-1);
            --resultVals: in MwordArray(0 to N_RES_TAGS-1);
            regValues => regValsA --(others => (others => '0'))     
        );
      
        dataToAlu(0) <= (dataToExec.full, 
                       work.LogicExec.executeAlu(dataToExec.ins, dataToExec.state, bqSelected.ins));

      
            SUBPIPE_A: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
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
      
		branchData <=  
		        work.LogicExec.basicBranch(setInstructionTarget(dataToExec.ins, 
		                                                          dataToExec.ins.constantArgs.imm),
                                         dataToExec.state, bqSelected.ins, bqSelected.full);                    
            
            dataToBranch(0) <= (dataToExec.full and 
                                          work.LogicExec.isBranch(dataToExec.ins), branchData);
            sendingBranchIns <= dataToBranch(0).full;
            
            --dataD0 <= outputDataD2(0).ins;
                bqCompare <= (sendingBranchIns, dataToExec.ins);
            
            SUBPIPE_D: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
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
            
            -- TEMP: just for subpipe A
            INT_WRITE_QUEUE: entity work.GenericStage(Behavioral)
            generic map(
                COMPARE_TAG => '1'
            )
            port map(
                clk => clk, reset => '0', en => '0',
                
                prevSending => sendingAlu,
                nextAccepting => '1',
                
                stageDataIn => dataOutAlu,
                acceptingOut => open,
                sendingOut => sendingToIntRF,
                stageDataOut => dataToIntRF,
                
                execEventSignal => '0',--execEventSignal,
                lateEventSignal => '0',
                execCausing => DEFAULT_INSTRUCTION_STATE--execCausing
            ); 
            
  
         execOutputs1(0) <= (sendingAlu, dataOutAlu(0).ins);
         execOutputs2(0) <= (sendingBranch, dataFromBranch.ins);
         
         
         regsSelA <= --getPhysicalSources(iqOutputArr(0).ins);
                    work.LogicRenaming.getPhysicalArgs((0 => ('1', dataToIssue.ins)));
           
          -- Forwarding network
		  fni.nextResultTags <= (0 => dataToExec.ins.physicalArgSpec.dest, others => (others => '0'));        
		  fni.nextTagsM2 <= (others => (others => '0'));
          fni.tags0 <= (execOutputs1(0).ins.physicalArgSpec.dest, -- ALU
                             execOutputs1(1).ins.physicalArgSpec.dest, execOutputs1(2).ins.physicalArgSpec.dest);
          fni.tags1 <= (0 => dataOutAluDelay(0).ins.physicalArgSpec.dest, others => (others => '0'));
          fni.values0 <= (execOutputs1(0).ins.result, -- ALU
                             execOutputs1(1).ins.result, execOutputs1(2).ins.result);
          fni.values1 <= (0 => dataOutAluDelay(0).ins.result, others => (others => '0'));                 
                    
                    
		 INT_REG_FILE: entity work.RegFile(Behavioral)
         generic map(WIDTH => 4, WRITE_WIDTH => 1)
         port map(
             clk => clk, reset => '0', en => '0',
                 
             writeAllow => sendingToIntRF,
             writeInput => dataToIntRF,
 
             readAllowVec => (others => '1'), -- TEMP!
             
             selectRead(0 to 2) => regsSelA,--(others => (others => '0')),
             selectRead(3 to 5) => (others => (others => '0')),
             selectRead(6 to 8) => (others => (others => '0')),
             selectRead(9 to 11) => (others => (others => '0')),
             
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
    end block;

    renamedDataToBQ <= setFullMask(renamedDataLiving, getBranchMask(renamedDataLiving));



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

		--storeAddressInput: in InstructionSlot;
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
		dataOutV => bqData,
		
			committedOutput => open,
			committedEmpty => open
	);

end Behavioral;
