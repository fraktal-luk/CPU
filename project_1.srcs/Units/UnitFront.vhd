
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

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

        signal dummyBP0, dummyBP1: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0'); -- Resuts from BP 
	signal sendingOutFetch0, sendingOutFetch1, sendingOutBuffer: std_logic := '0';	
	signal acceptingOutFetch0, acceptingOutFetch1, bufferAccepting: std_logic := '0';
	signal earlyBranchSending, earlyBranchMultiSending: std_logic := '0';
	
	signal fetchedLine0, fetchedLine1: WordArray(0 to FETCH_WIDTH-1) := (others => (others => '0')); 
	
	signal stallCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;

	signal frontBranchEvent, killAll, killAllOrFront, frontKill, stallEventSig: std_logic := '0';
	signal sendingToEarlyBranch, sendingToBuffer, fetchStall: std_logic := '0'; 
	signal pcSendingDelayed0, pcSendingDelayed1, pcSendingDelayedFinal: std_logic := '0';
	
	signal frontCausingSig, earlyBranchIn, earlyBranchDataOut: InstructionState := DEFAULT_INSTRUCTION_STATE;
	signal predictedAddress: Mword := (others => '0');

	signal stageDataOutFetch0, stageDataOutFetch1, stageDataInEarlyBranch, earlyBranchDataOutA:
				InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);

	signal earlyBranchMultiDataInA, earlyBranchMultiDataOutA:
								InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	signal branchMask: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
begin
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

	fetchStall <= sendingOutFetch1 and (not bufferAccepting or not bpAccepting);

	earlyBranchIn <=
		getEarlyEvent(stageDataOutFetch1(0).ins, earlyBranchMultiDataInA, predictedAddress, fetchStall);
			
    sendingToEarlyBranch <= sendingOutFetch1;
	stageDataInEarlyBranch(0) <= (sendingOutFetch1, earlyBranchIn);
										
	SUBUNIT_EARLY_BRANCH: entity work.GenericStage(Behavioral)
	port map(
		clk => clk, reset => resetSig, en => enSig,
				
		prevSending => sendingToEarlyBranch,	
		nextAccepting => '1',
		stageDataIn => stageDataInEarlyBranch,
			
		acceptingOut => open,--earlyBranchAccepting,
		sendingOut => earlyBranchSending,
		stageDataOut => earlyBranchDataOutA,
		
		execEventSignal => killAll, -- CAREFUL: not killing on stall, because is sent to void
		lateEventSignal => killAll,
		execCausing => DEFAULT_INSTRUCTION_STATE
	);

	earlyBranchDataOut <= earlyBranchDataOutA(0).ins;

	branchMask <= getBranchMask(earlyBranchMultiDataInA);
	bpData <= prepareForBQ(earlyBranchMultiDataInA, branchMask);

	frontKill <= frontBranchEvent;-- or fetchStall;

	frontBranchEvent <= earlyBranchDataOut.controlInfo.newEvent;
	frontEventSignal <= frontBranchEvent;
	frontCausingSig <= earlyBranchDataOut;
	
	SAVE_PRED_TARGET: process(clk)
	begin
		if rising_edge(clk) then
			if lateEventSetPC = '1' then				
				predictedAddress <= lateCausing.target;
			elsif execEventSignal = '1' then
				predictedAddress <= execCausing.target;
			elsif frontBranchEvent = '1' then -- CAREFUL: must equal frontEventSignal
				predictedAddress <= frontCausingSig.target;
			elsif sendingToEarlyBranch = '1' then -- Normal flow - target from line predictor
				predictedAddress <= earlyBranchIn.target;
			end if;
		end if;
	end process;
	
	sendingToBuffer <= sendingOutFetch1 and bufferAccepting;
	
	SUBUNIT_IBUFFER: entity work.InstructionBuffer(Implem)
	port map(
		clk => clk, reset => resetSig, en => enSig,
		
		prevSending => sendingToBuffer,
		nextAccepting => renameAccepting,
		stageDataIn => earlyBranchMultiDataInA,
		
		acceptingOut => bufferAccepting,
		sendingOut => sendingOutBuffer,
		stageDataOut => dataLastLiving,
		
		execEventSignal => killAll,--killVector(3),
		execCausing => DEFAULT_INSTRUCTION_STATE		
	);

	lastSending <= sendingOutBuffer;
	
	frontAccepting <= '1';-- acceptingOutFetch0;

	frontCausing <= frontCausingSig;
	
	bpSending <= sendingOutFetch1 and not fetchStall;
end Behavioral;
