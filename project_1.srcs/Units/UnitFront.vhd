
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicFront.all;


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

    signal dummyBP0, dummyBP1: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0'); -- Results from BP 
	signal sendingOutFetch0, sendingOutFetch1, sendingOutBuffer: std_logic := '0';	
	signal acceptingOutFetch0, acceptingOutFetch1, bufferAccepting: std_logic := '0';
	signal earlyBranchSending, earlyBranchMultiSending, sendingToBranchTransfer: std_logic := '0';
	
	signal fetchedLine0, fetchedLine1: WordArray(0 to FETCH_WIDTH-1) := (others => (others => '0')); 
	signal frontBranchEvent, killAll, killAllOrFront: std_logic := '0';
	signal sendingToEarlyBranch, sendingToBQ, sendingToBuffer, fetchStall,  full0, full1, fullBr, fullBt, sf0, sf1, sfBr, sfBu, sfBt,  ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0'; 
	
	signal frontCausingSig, earlyBranchIn: InstructionState := DEFAULT_INSTRUCTION_STATE;
	signal predictedAddress: Mword := (others => '0');

	signal stageDataOutFetch0, stageDataOutFetch1, stageDataInEarlyBranch, earlyBranchDataOutA: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);

    --                              earlyBranchMultiDataOutA UNUSED!
	signal earlyBranchMultiDataInA, earlyBranchMultiDataOutA, ibufDataOut: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	
	signal dataToIbuffer, dataToBranchTransfer, dataBranchTransferOut: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	signal fetchCounter, fetchCounterNext: Word := (others => '0');
begin
	killAll <= execEventSignal or lateEventSignal;
    killAllOrFront <= killAll or frontBranchEvent;

    fetchCounterNext <= addInt(fetchCounter, PIPE_WIDTH) when pcSending = '1' else fetchCounter;

    stageDataInFetch0(0).full <= pcSending;
    stageDataInFetch0(0).ins.ip <= pcDataLiving.ip;
    stageDataInFetch0(0).ins.target <= pcDataLiving.target;
    stageDataInFetch0(0).ins.tags.fetchCtr <= fetchCounter when not CLEAR_DEBUG_INFO else (others => '0');

--	SUBUNIT_FETCH_0: entity work.GenericStage(Behavioral)
--	generic map(
--		WIDTH => 1 --PIPE_WIDTH
--	)
--	port map(
--		clk => clk, reset => '0', en => '0',
				
--		prevSending => pcSending,	
--		nextAccepting => acceptingOutFetch1,
--		stageDataIn => stageDataInFetch0,
		
--		acceptingOut => acceptingOutFetch0,
--		--sendingOut => sendingOutFetch0,
--		--stageDataOut => stageDataOutFetch0,
		
--		execEventSignal => killAllOrFront,
--		lateEventSignal => killAll,
--		execCausing => DEFAULT_INSTRUCTION_STATE
--	);

	
	fetchedLine0 <= iin;
	
--	SUBUNIT_FETCH_1: entity work.GenericStage(Behavioral)
--	generic map(
--		WIDTH => 1 --PIPE_WIDTH
--	)
--	port map(
--		clk => clk, reset => resetSig, en => enSig,
				
--		prevSending => sendingOutFetch0,	
--		nextAccepting => '1',--earlyBranchAccepting,
--		acceptingOut => acceptingOutFetch1,
		
--		stageDataIn => stageDataOutFetch0,
--		--sendingOut => sendingOutFetch1,
--		--stageDataOut => stageDataOutFetch1,
		
--		execEventSignal => killAllOrFront,
--		lateEventSignal => killAll,
--		execCausing => DEFAULT_INSTRUCTION_STATE
--	);	

    process(clk)
    begin
        if rising_edge(clk) then
            fetchedLine1 <= fetchedLine0;
            fetchCounter <= fetchCounterNext;
            
            
                full0 <= bool2std(pcSending = '1');
                if killAllOrFront = '1' then full0 <= '0'; end if;

                full1 <= sendingOutFetch0;
                if killAllOrFront = '1' then full1 <= '0'; end if;
                
                fullBr <= sendingOutFetch1;
                if killAll = '1' then fullBr <= '0'; end if;
                
                
                fullBt <= bool2std(sendingToBranchTransfer = '1');
                if killAll = '1' then fullBt <= '0'; end if;

                earlyBranchDataOutA <= stageDataInEarlyBranch;
                dataBranchTransferOut <= dataToBranchTransfer;

                stageDataOutFetch0 <= stageDataInFetch0;
                stageDataOutFetch1 <= stageDataOutFetch0;

        end if;
    end process;
    
    --sf0
    sendingOutFetch0
    <= full0 and not killAllOrFront;
    --sf1
    sendingOutFetch1
    <= full1 and not killAllOrFront;
    --sfBr
    earlyBranchSending
    <= fullBr and not killAll;
    --sfBt
    sendingToBQ
    <= fullBt and not killAll;
    
    ch0 <= sendingOutFetch0 xnor sf0;
    ch1 <= sendingOutFetch1 xnor sf1;
    ch2 <= earlyBranchSending xnor sfBr;
    ch3 <= sendingToBQ xnor sfBt;
    
--   sendingOutFetch0 <= pcSending;
--     fullF0 <= pcSending;
--     sendingOutFetch0 <= fullF0 and not eventSignal;
--     if eventSignal = '1' then fullF0 <= '0'; end if;

--   stageDataOutFetch0 <= stageDataInFetch0;

--    sendingOutFetch1 <= sendingOutFetch0;
--      fullF1 <= sendingOutFetch0;
--      sendingOutFetch1 <= fullF1 and not eventSignal;
--     if eventSignal = '1' then fullF1 <= '0'; end if;

--    stageDataOutFetch1 <= stageDataOutFetch0;
    
    
    
	earlyBranchMultiDataInA <= getFrontEventMulti(predictedAddress, stageDataOutFetch1(0).ins, fetchedLine1);


	fetchStall <= sendingOutFetch1 and (not bufferAccepting or not bpAccepting);

	earlyBranchIn <= getEarlyEvent(stageDataOutFetch1(0).ins, earlyBranchMultiDataInA, predictedAddress, fetchStall);
			
    sendingToEarlyBranch <= sendingOutFetch1;
	stageDataInEarlyBranch(0) <= (sendingOutFetch1, earlyBranchIn);
										
--	SUBUNIT_EARLY_BRANCH: entity work.GenericStage(Behavioral)
--	port map(
--		clk => clk, reset => resetSig, en => enSig,
				
--		prevSending => sendingToEarlyBranch,	
--		nextAccepting => '1',
--		stageDataIn => stageDataInEarlyBranch,
			
--		acceptingOut => open,--earlyBranchAccepting,
--		--sendingOut => earlyBranchSending,
--		--stageDataOut => earlyBranchDataOutA,
		
--		execEventSignal => killAll, -- CAREFUL: not killing on stall, because is sent to void
--		lateEventSignal => killAll,
--		execCausing => DEFAULT_INSTRUCTION_STATE
--	);

	frontBranchEvent <= earlyBranchDataOutA(0).ins.controlInfo.newEvent and earlyBranchSending;
	frontEventSignal <= frontBranchEvent;
	frontCausingSig <= clearDbCausing(earlyBranchDataOutA(0).ins);
	
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
	
	sendingToBuffer <= sendingOutFetch1 and not fetchStall;
	   
	   dataToIbuffer <= adjustStage(earlyBranchMultiDataInA);
	   
	SUBUNIT_IBUFFER: entity work.InstructionBuffer(Implem)
	port map(
		clk => clk, reset => resetSig, en => enSig,
		
		prevSending => sendingToBuffer,
		nextAccepting => renameAccepting,
		stageDataIn => dataToIbuffer,
		
		acceptingOut => bufferAccepting,
		sendingOut => sendingOutBuffer,
		stageDataOut => ibufDataOut,
		
		execEventSignal => killAll,
		execCausing => DEFAULT_INSTRUCTION_STATE		
	);

	lastSending <= sendingOutBuffer;
	dataLastLiving <= ibufDataOut;
	frontAccepting <= '1';

	frontCausing <= frontCausingSig;
   
	sendingToBranchTransfer <= sendingOutFetch1 and not fetchStall;

	dataToBranchTransfer <= prepareForBQ(--earlyBranchMultiDataInA);
	                                      stageDataOutFetch1(0), dataToIbuffer);

    bpData <= dataBranchTransferOut;
              --  dataToBranchTransfer;
    bpSending <= sendingToBQ;
                -- sendingToBranchTransfer;


--    SUBUNIT_BRANCH_TRANSFER: entity work.GenericStage(Behavioral)
--	generic map(
--		WIDTH => PIPE_WIDTH
--	)
--	port map(
--		clk => clk, reset => resetSig, en => enSig,
				
--		prevSending => sendingToBranchTransfer,	
--		nextAccepting => '1',
--		stageDataIn => dataToBranchTransfer,
		
--		acceptingOut => open,
--		--sendingOut => sendingToBQ,
--		--stageDataOut => dataBranchTransferOut,
		
--		execEventSignal => killAll,
--		lateEventSignal => killAll,
--		execCausing => DEFAULT_INSTRUCTION_STATE
--	);

--	VIEW: if VIEW_ON generate
--	   --use work.Viewing.all;
--        signal insBufInput, stagePreBuffer, branchTransferData, stageOut: GenericStageView;
--        signal stageFetch0, stageFetch1: FetchStageView;
        
--        function expandToSlotArray(ia: InstructionSlotArray; wa: WordArray) return InstructionSlotArray is
--            variable res: InstructionSlotArray(wa'range) := (others => DEFAULT_INS_SLOT);
--            variable adrHi, adrLo: Mword := ia(0).ins.ip; 
--        begin
--            adrHi(ALIGN_BITS-1 downto 0) := (others => '0');
--            adrLo(MWORD_SIZE-1 downto ALIGN_BITS) := (others => '0');    
        
--            for i in 0 to wa'length-1 loop
--                res(i).ins.bits := wa(i);
--                res(i).ins.ip := i2slv(slv2u(adrHi) + 4*i, 32);
--                if ia(0).full = '1' and slv2u(adrLo) <= 4*i then
--                    res(i).full := '1';
--                end if;
--            end loop;
            
--            return res;
--        end function;  
--    begin
--        insBufInput <= getInsStringArray(earlyBranchMultiDataInA);

--        stageFetch0 <= getInsStringArray(expandToSlotArray(stageDataOutFetch0, fetchedLine0));
--        stageFetch1 <= getInsStringArray(expandToSlotArray(stageDataOutFetch1, fetchedLine1));
--        stagePreBuffer <= getInsStringArray(earlyBranchMultiDataOutA);
        
--        branchTransferData <= getInsStringArray(dataBranchTransferOut);
        
--        stageOut <= getInsStringArray(ibufDataOut);
--    end generate;

end Behavioral;
