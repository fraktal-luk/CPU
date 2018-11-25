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


architecture Empty of Core is
    signal pcDataSig, frontCausing, execCausing, lateCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;
    signal pcSending, frontAccepting, bpAccepting, bpSending, renameAccepting, frontLastSending,
                frontEventSignal: std_logic := '0';
    signal bpData: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal frontDataLastLiving: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal execEventSignal, lateEventSignal, lateEventSetPC: std_logic := '0';
        signal iadrReg: Mword := X"ffffffb0";
begin
   --         pcSending <= '1';
            
--            process (clk)
--            begin
--                if rising_edge(clk) then
--                    iadrReg <= addWord(iadrReg, X"00000010");
--                end if;
--            end process;
            --iadr <= iadrReg;
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
        renameAccepting => renameAccepting, -- to frontend
        frontLastSending => frontLastSending,
        frontDataLastLiving => frontDataLastLiving,

        -- Interface from register mapping
        newPhysDestsIn => (others => (others => '0')),--newPhysDests,
        newPhysDestPointerIn => (others => '0'),--newPhysDestPointer,
        newPhysSourcesIn => (others => (others => '0')),--newPhysSources,

        -- Interface with IQ
        iqAccepts => '1',--iqAccepts,
        renamedDataLiving => open,--renamedDataLiving, -- !!!
        renamedSending => open,--renamedSending,
        
        -- Interface from ROB
        commitAccepting => open,--commitAccepting,
        sendingFromROB => '0',--robSending,    
        robDataLiving => (others => DEFAULT_INSTRUCTION_SLOT),--dataOutROB,

        ---
        dataFromBQV => (others => DEFAULT_INSTRUCTION_SLOT),--dataOutBQV,

        sbSending => '0',--sbSending,
        dataFromSB => DEFAULT_INSTRUCTION_STATE,--dataFromSB,
        sbEmpty => '1',--sbEmpty,

        -- Interface from committed stage
        committedSending => open,--committedSending,
        committedDataOut => open,--committedDataOut,
        renameLockEndOut => open,--renameLockEnd,
                
        commitGroupCtrOut => open,--commitGroupCtrSig,
        commitGroupCtrIncOut => open --commitGroupCtrIncSig
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
    
        bpAccepting => '1',--bpAccepting,
        bpSending => bpSending,
        bpData => bpData,
    
        renameAccepting => '1',--renameAccepting,            
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

end Empty;
