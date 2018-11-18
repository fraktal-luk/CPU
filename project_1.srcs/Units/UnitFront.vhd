----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    18:06:59 04/24/2016 
-- Design Name: 
-- Module Name:    UnitFront - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;

use work.CoreConfig.all;

use work.PipelineGeneral.all;

use work.TmpLogicFront.all;


entity UnitFront is
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
										
		iin: in WordArray(0 to FETCH_WIDTH-1);
		--ivalid: in std_logic;		

		-- Interface PC to front
		pcDataLiving: in InstructionState;
		pcSending: in std_logic;
		frontAccepting: out std_logic;

		bpAccepting: in std_logic;
		bpSending: out std_logic;
		bpData: out InstructionSlotArray(0 to FETCH_WIDTH-1);

		-- Interface front to renaming
		renameAccepting: in std_logic;		
		dataLastLiving: out InstructionSlotArray(0 to PIPE_WIDTH-1); 
		lastSending: out std_logic;
		-------
		
		frontEventSignal: out std_logic;
		frontCausing: out InstructionState;
		
		execCausing: in InstructionState;
		lateCausing: in InstructionState;
		
		execEventSignal: in std_logic;
		lateEventSignal: in std_logic;
		lateEventSetPC: in std_logic		
	);
end UnitFront;


architecture Behavioral of UnitFront is
	signal resetSig, enSig: std_logic := '0';							

    signal stageDataInFetch1: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal stageDataInFetch0: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
    
    signal stageDataInEarlyBranch: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);        

        signal dummyBP0, dummyBP1: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0'); -- Resuts from BP 
	signal sendingOutFetch0, sendingOutHbuffer, sendingOut0: std_logic := '0';	
	signal acceptingOutFetch0, acceptingOutHbuffer, bufferAccepting, acceptingOut0: std_logic := '0';
	
	signal fetchedLine0, fetchedLine1: WordArray(0 to FETCH_WIDTH-1) := (others => (others => '0')); 
	
	signal ivalid1, ivalidFinal: std_logic := '0';
	signal sendingOutFetch1, sendingOutFetchFinal: std_logic := '0';	
	signal acceptingOutFetch1: std_logic := '0';	

	signal fetchBlock, fetchBlock1: HwordArray(0 to FETCH_BLOCK_SIZE-1) := (others => (others => '0'));

	signal fetchBlockFinal, fetchBlockBP: HwordArray(0 to FETCH_BLOCK_SIZE-1) := (others => (others => '0'));
	
	signal acceptingForFetchFirst, earlyBranchAccepting, earlyBranchSending, earlyBranchMultiSending
						: std_logic := '0'; -- earlyBranchAccepting always true?
	
	signal hbufferDataIn, stallCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;

	signal frontBranchEvent, killAll, killAllOrFront, frontKill, stallEventSig: std_logic := '0';

	signal sendingToEarlyBranch, fetchStall: std_logic := '0'; 
	signal pcSendingDelayed0, pcSendingDelayed1, pcSendingDelayedFinal: std_logic := '0';
	
	signal frontCausingSig, earlyBranchIn, earlyBranchDataOut: InstructionState := DEFAULT_INSTRUCTION_STATE;
	signal predictedAddress: Mword := (others => '0');

	signal stageDataOutFetch0, stageDataoutFetch1, earlyBranchDataOutA:
				InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);

	signal earlyBranchMultiDataInA, earlyBranchMultiDataOutA, stageDataDecodeInA, stageDataDecodeOutA:
								InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	signal branchMask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');	
begin	 
	resetSig <= '0';
	enSig <= '0';
			
	killAll <= execEventSignal or lateEventSignal;
    killAllOrFront <= killAll or frontKill;
			

    stageDataInFetch0(0) <= (pcSending, pcDataLiving);

	SUBUNIT_FETCH_0: entity work.GenericStage(Behavioral)
	generic map(
		WIDTH => 1 --PIPE_WIDTH
	)
	port map(
		clk => clk, reset => '0', en => '0',
				
		prevSending => pcSending,	
		nextAccepting => acceptingOutFetch1,
		stageDataIn => stageDataInFetch0,
		
		acceptingOut => acceptingOutFetch0,
		sendingOut => sendingOutFetch0,
		stageDataOut => stageDataOutFetch0,
		
		execEventSignal => killAllOrFront,
		lateEventSignal => killAll,
		execCausing => DEFAULT_INSTRUCTION_STATE
	);
	
    process(clk)
    begin
        if rising_edge(clk) then
            fetchedLine0 <= iin;
        end if;
    end process;
	
	SUBUNIT_FETCH_1: entity work.GenericStage(Behavioral)
	generic map(
		WIDTH => 1 --PIPE_WIDTH
	)
	port map(
		clk => clk, reset => resetSig, en => enSig,
				
		prevSending => sendingOutFetch0,	
		nextAccepting => '1',--earlyBranchAccepting,
		acceptingOut => acceptingOutFetch1,
		
		stageDataIn => stageDataOutFetch0,
		sendingOut => sendingOutFetch1,
		stageDataOut => stageDataOutFetch1,
		
		execEventSignal => killAllOrFront,
		lateEventSignal => killAll,
		execCausing => DEFAULT_INSTRUCTION_STATE
	);	

    process(clk)
    begin
        if rising_edge(clk) then
            fetchedLine1 <= fetchedLine0;
        end if;
    end process;
				
	earlyBranchMultiDataInA <= getFrontEventMulti(predictedAddress, stageDataOutFetch1(0).ins, fetchedLine1);

    --	     report insText(decodeFromWord(fetchLine(i)));

	SUBUNIT_EARLY_BRANCH_MULTI: entity work.GenericStage(Behavioral)
	generic map(
		WIDTH => PIPE_WIDTH
	)
	port map(
		clk => clk, reset => resetSig, en => enSig,
				
		prevSending => sendingOutFetch1,	
		nextAccepting => '1',
		stageDataIn => earlyBranchMultiDataInA,
		
		acceptingOut => open,
		sendingOut => earlyBranchMultiSending,
		stageDataOut => earlyBranchMultiDataOutA,
		
		execEventSignal => killAll, -- CAREFUL: not killing on stall, because is sent to void
		lateEventSignal => killAll,
		execCausing => DEFAULT_INSTRUCTION_STATE
	);

	earlyBranchIn <=
		setInstructionIP(findEarlyTakenJump(stageDataOutFetch1(0).ins, earlyBranchMultiDataInA), predictedAddress);
			
	stageDataInEarlyBranch(0) <= (sendingOutFetch1, earlyBranchIn);
										
	SUBUNIT_EARLY_BRANCH: entity work.GenericStage(Behavioral)
	port map(
		clk => clk, reset => resetSig, en => enSig,
				
		prevSending => sendingToEarlyBranch,	
		nextAccepting => '1',
		stageDataIn => stageDataInEarlyBranch,
			
		acceptingOut => earlyBranchAccepting,
		sendingOut => earlyBranchSending,
		stageDataOut => earlyBranchDataOutA,
		
		execEventSignal => killAll, -- CAREFUL: not killing on stall, because is sent to void
		lateEventSignal => killAll,
		execCausing => DEFAULT_INSTRUCTION_STATE
	);

	earlyBranchDataOut <= earlyBranchDataOutA(0).ins;
	frontBranchEvent <= earlyBranchDataOut.controlInfo.newEvent;

	--bpSending is handled elsewhere
	branchMask <= getBranchMask(earlyBranchMultiDataInA);
	bpData <= prepareForBQ(earlyBranchMultiDataInA, branchMask);


	stallEventSig <= fetchStall;
	stallCausing <= setInstructionTarget(earlyBranchDataOut, earlyBranchDataOut.ip);
	frontKill <= frontBranchEvent or fetchStall;
	fetchStall <= earlyBranchSending and (not bufferAccepting or not bpAccepting);

	SAVE_PRED_TARGET: process(clk)
	begin
		if rising_edge(clk) then
			if lateEventSetPC = '1' then				
				predictedAddress <= lateCausing.target;
			elsif execEventSignal = '1' then
				predictedAddress <= execCausing.target;
			elsif (stallEventSig or frontBranchEvent) = '1' then -- CAREFUL: must equal frontEventSignal
				predictedAddress <= frontCausingSig.target; -- ?
			elsif sendingToEarlyBranch = '1' then
				predictedAddress <= earlyBranchIn.target; -- ??
			end if;
		end if;
	end process;
	
	-- Hword buffer
	hbufferDataIn <= earlyBranchDataOut;
	
--	SUBUNIT_HBUFFER: entity work.SubunitHbuffer(Implem)
--	port map(
--		clk => clk, reset => resetSig, en => enSig,
		
--		prevSending => earlyBranchSending and not fetchStall,
--		nextAccepting => acceptingOut0,
--		stageDataIn => hbufferDataIn,
--		stageDataInMulti => makeSDM(earlyBranchMultiDataOutA),
--		fetchBlock => fetchBlockBP,
		
--		acceptingOut => acceptingOutHbuffer,
--		sendingOut => sendingOutHbuffer,
--		stageDataOut => stageDataOutHbuffer,
		
--		execEventSignal => killAll,--killVector(3),
--		execCausing => DEFAULT_INSTRUCTION_STATE		
--	);

-- Decode stage
	--newDecoded <= decodeMulti(stageDataOutHbuffer);
	--stageDataDecodeNew <= --fillTargetsAndLinks(newDecoded);
	--							 newDecoded;
	
	--stageDataDecodeInA <= makeSlotArray(stageDataDecodeNew.data, stageDataDecodeNew.fullMask);
	
	SUBUNIT_DECODE: entity work.GenericStage(Behavioral)
		generic map(
			WIDTH => PIPE_WIDTH
		)
		port map(
		clk => clk, reset => resetSig, en => enSig,
		
		prevSending => sendingOutHbuffer,	
		nextAccepting => renameAccepting,
		stageDataIn => stageDataDecodeInA,
		
		acceptingOut => acceptingOut0,
		sendingOut => sendingOut0,
		stageDataOut => stageDataDecodeOutA,

		execEventSignal => killAll,
		lateEventSignal => killAll,
		execCausing => DEFAULT_INSTRUCTION_STATE
	);	
	
	-- from later stages
	--dataLastLiving <= clearTempControlInfoMulti(makeSDM(stageDataDecodeOutA));
	lastSending <= sendingOut0;
	
	frontAccepting <= acceptingOutFetch0;
	
	frontEventSignal <= stallEventSig or frontBranchEvent;
	frontCausingSig <= stallCausing when stallEventSig = '1' else earlyBranchDataOut;
	frontCausing <= frontCausingSig;
	
	bpSending <= earlyBranchSending and not fetchStall;
end Behavioral;
