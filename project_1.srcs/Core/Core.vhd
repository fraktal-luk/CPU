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
                frontEventSignal: std_logic := '0';
    signal bpData: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal frontDataLastLiving, renamedDataLiving, dataOutROB, renamedDataToBQ: 
                InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    
    signal execOutputs1, execOutputs2: InstructionSlotArray(0 to 3) := (others => DEFAULT_INSTRUCTION_SLOT);    

    signal execEventSignal, lateEventSignal, lateEventSetPC: std_logic := '0';
    signal robSending, robAccepting, renamedSending, commitAccepting, iqAccepting: std_logic := '0';
    --    signal iadrReg: Mword := X"ffffffb0";
    signal commitGroupCtr, commitCtr: InsTag := (others => '0');
begin

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
        intSignal => int0,
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
        dataFromBQV => (others => DEFAULT_INSTRUCTION_SLOT),--dataOutBQV,

        sbSending => '0',--sbSending,
        dataFromSB => DEFAULT_INSTRUCTION_STATE,--dataFromSB,
        sbEmpty => '1',--sbEmpty,

        -- Interface from committed stage
        committedSending => open,--committedSending,
        committedDataOut => open,--committedDataOut,
        renameLockEndOut => open,--renameLockEnd,
                
        commitGroupCtrOut => commitGroupCtr,
        commitCtrOut => commitCtr,
        commitGroupCtrIncOut => open --commitGroupCtrIncSig
    );
        
        iqAccepting <= robAccepting and '1'; -- TEMP 
        
    iadr <= pcDataSig.ip;
    iadrvalid <= pcSending;
       
	UNIT_FRONT: entity work.UnitFront(Behavioral)
    port map(
        clk => clk, reset => '0', en => '0',
        
        iin => iin,
                    
        pcDataLiving => pcDataSig,
        pcSending => pcSending,    
        frontAccepting => frontAccepting,
    
        bpAccepting => '1',--bpAccepting,
        bpSending => bpSending,
        bpData => bpData,
    
        renameAccepting => renameAccepting,            
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
            
        commitGroupCtr => commitGroupCtr,
        commitCtr => commitCtr,
		
		execCausing => execCausing,
        lateCausing => lateCausing,
        
        execEventSignal => execEventSignal,
        lateEventSignal => lateEventSignal
    );

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
		
		nextAccepting => commitAccepting,
		sendingOut => robSending, 
		outputData => dataOutROB		
	);


    TEMP_EXEC: block
        signal dataToIQ: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
    begin
        dataToIQ <= getSchedData(extractData(dataOutROB), extractFullMask(dataOutROB));
    
		IQUEUE: entity work.IssueQueue(Behavioral)--UnitIQ
        generic map(
            IQ_SIZE => 8 --IQ_SIZES(4)
        )
        port map(
            clk => clk, reset => '0', en => '0',
    
            --acceptingVec => open,--iqAcceptingVecArr(i),
            acceptingOut => open,--iqAcceptingArr(4),
            prevSendingOK => robSending,
            --newData => dataToQueuesArr(i),
                newArr => dataToIQ,--,schArrays(4),
            fni => DEFAULT_FORWARDING_INFO,--fni,
            readyRegFlags => (others => '0'),--readyRegFlags,
            nextAccepting => '1',--issueAcceptingArr(4),
            execCausing => execCausing,
            lateEventSignal => lateEventSignal,
            execEventSignal => execEventSignal,
            anyReady => open,--iqReadyArr(4),
            schedulerOut => open,--iqOutputArr(4),
            sending => open --iqSending(4)
        );
 
        ISSUE_STAGE: entity work.IssueStage
 	    generic map(USE_IMM => true)
        port map(
            clk => '0',
            reset => '0',
            en => '0',
    
            prevSending => '0',
            nextAccepting => '0',
    
            input => DEFAULT_SCH_ENTRY_SLOT,
            
            acceptingOut => open,
            output => open,
            
            execEventSignal => execEventSignal,
            lateEventSignal => lateEventSignal,
            execCausing => execCausing,
            
                fni => DEFAULT_FORWARDING_INFO,
            
            --resultTags: in PhysNameArray(0 to N_RES_TAGS-1);
            --resultVals: in MwordArray(0 to N_RES_TAGS-1);
            regValues => (others => (others => '0'))     
        );
      
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
		
		acceptingBr => open,
		
		prevSending => renamedSending,
			prevSendingBr => bpSending,
		dataIn => renamedDataToBQ,
			dataInBr => bpData,

		--storeAddressInput: in InstructionSlot;
		--storeValueInput: in InstructionSlot;
		--compareAddressInput: in InstructionSlot;

		selectedDataOutput => open,

		committing => '0',
		groupCtrInc => (others => '0'),

		lateEventSignal => '0',
		execEventSignal => '0',
		execCausing => execCausing,
		
		nextAccepting => '0',		
		sendingSQOut => open,
		dataOutV => open,
		
			committedOutput => open,
			committedEmpty => open
	);

end Behavioral;
