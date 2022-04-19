
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
		pcDataIn: in ControlPacket;
		  
		frontAccepting: out std_logic;

		bpAccepting: in std_logic;
		bpSending: out std_logic;
		--bpData: out InstructionSlotArray(0 to FETCH_WIDTH-1);
            bpData_N: out ControlPacketArray(0 to FETCH_WIDTH-1);

		-- Interface front to renaming
		renameAccepting: in std_logic;		
		--dataLastLiving: out InstructionSlotArray(0 to PIPE_WIDTH-1);
		  dataOut_N: out BufferEntryArray; 
		lastSending: out std_logic;
		-------
		
		frontEventSignal: out std_logic;
		frontCausing_N: out ExecResult;

		execCausing_N: in ExecResult;
		lateCausing_N: in ExecResult;
		
		execEventSignal: in std_logic;
		lateEventSignal: in std_logic;
		lateEventSetPC: in std_logic		
	);
end UnitFront;


architecture Behavioral of UnitFront is
	signal resetSig, enSig: std_logic := '0';							

    signal cpFetch0, cpFetch1: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal stageDataInFetch1, dataToIbuffer, dataToBranchTransfer, dataBranchTransferOut: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal stageDataInFetch0, stageDataOutFetch0, stageDataOutFetch1: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal dummyBP0, dummyBP1: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0'); -- Results from BP 
	signal sendingOutFetch0, sendingOutFetch1, sendingOutBuffer, bufferAccepting, earlyBranchSending, sendingToBranchTransfer: std_logic := '0';
	signal pcEn, frontBranchEvent, killAll, killAllOrFront: std_logic := '0';
	
	signal fetchedLine0, fetchedLine1: WordArray(0 to FETCH_WIDTH-1) := (others => (others => '0')); 
	signal sendingToEarlyBranch, sendingToBQ, sendingToBuffer, fetchStall,  full0, full1, fullBr, fullBt,
	                                                                                                      ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';	
	signal frontCausingSig, earlyBranchIn, earlyBranchOut: InstructionState := DEFAULT_INSTRUCTION_STATE;
	signal predictedAddress: Mword := (others => '0');

    signal ibufDataOut_N: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);

	signal earlyBranchMultiDataInA, ibufDataOut: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

    signal bqDataSig, bqDataSigPre: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
	
	signal fetchCounter, fetchCounterNext: Word := (others => '0');
begin
	killAll <= execEventSignal or lateEventSignal;
    killAllOrFront <= killAll or frontBranchEvent;

    fetchCounterNext <= addInt(fetchCounter, PIPE_WIDTH) when pcEn = '1' else fetchCounter;

	fetchedLine0 <= iin;

    pcEn <= pcDataIn.controlInfo.full;

    process(clk)
    begin
        if rising_edge(clk) then
            fetchCounter <= fetchCounterNext;

            -- fetchedLine0: assigned async
            fetchedLine1 <= fetchedLine0;

            full0 <= bool2std(pcEn = '1') and not killAllOrFront;
            full1 <= sendingOutFetch0 and not killAllOrFront;          -- F1
            fullBr <= sendingOutFetch1 and not killAll;                -- F2
            fullBt <= bool2std(sendingToBranchTransfer = '1') and not killAll; -- F2

            earlyBranchOut <= earlyBranchIn; -- only .controlInfo, .target ?
                    
            dataBranchTransferOut <= dataToBranchTransfer; -- F2
                bqDataSig <= bqDataSigPre;
            -- Ibuf is on F2 too

            stageDataOutFetch0 <= stageDataInFetch0;
            stageDataOutFetch1 <= stageDataOutFetch0;  -- .ip, .target

            -- Stage F0
            
            -- Stage F1
            
            -- Stage Ibuf/BrEval
            
            -- 

        end if;
    end process;

    stageDataInFetch0(0).ins.ip <= pcDataIn.ip;
    stageDataInFetch0(0).ins.target <= pcDataIn.target;

    stageDataInFetch0(0).ins.tags.fetchCtr <= fetchCounter when not CLEAR_DEBUG_INFO else (others => '0');

    
    sendingOutFetch0 <= full0 and not killAllOrFront;
    sendingOutFetch1 <= full1 and not killAllOrFront;
    earlyBranchSending <= fullBr and not killAll;
    sendingToBQ <= fullBt and not killAll;
    
    -- src for Ibuf and BT
	earlyBranchMultiDataInA <= getFrontEventMulti(predictedAddress, stageDataOutFetch1(0).ins.ip, stageDataOutFetch1(0).ins.target, fetchedLine1);

    sendingToEarlyBranch <= sendingOutFetch1;

    earlyBranchIn <= getEarlyEvent(earlyBranchMultiDataInA, stageDataOutFetch1(0).ins.target, predictedAddress, fetchStall);

	SAVE_PRED_TARGET: process(clk)
	begin
		if rising_edge(clk) then
			if lateEventSetPC = '1' then				
				predictedAddress <= lateCausing_N.value;
			elsif execEventSignal = '1' then
				predictedAddress <= execCausing_N.value;
			elsif frontBranchEvent = '1' then -- CAREFUL: must equal frontEventSignal
				predictedAddress <= frontCausingSig.target;
			elsif sendingToEarlyBranch = '1' then -- Normal flow - target from line predictor
				predictedAddress <= earlyBranchIn.target;
			end if;
		end if;
	end process;
	
	sendingToBuffer <= sendingOutFetch1 and not fetchStall; 
	dataToIbuffer <= adjustStage(earlyBranchMultiDataInA);

	sendingToBranchTransfer <= sendingOutFetch1 and not fetchStall;
	dataToBranchTransfer <= prepareForBQ(stageDataOutFetch1(0).ins.ip, dataToIbuffer);
    	bqDataSigPre <= prepareForBQ_N(stageDataOutFetch1(0).ins.ip, dataToIbuffer);

	   
	SUBUNIT_IBUFFER: entity work.InstructionBuffer(Implem)
	port map(
		clk => clk, reset => resetSig, en => enSig,
		
		prevSending => sendingToBuffer,
		nextAccepting => renameAccepting,
		stageDataIn => dataToIbuffer,
		
		acceptingOut => bufferAccepting,
		sendingOut => sendingOutBuffer,
		--stageDataOut => ibufDataOut,
		  stageDataOut_N => ibufDataOut_N,
		
		execEventSignal => killAll
		--execCausing => DEFAULT_INSTRUCTION_STATE		
	);


	fetchStall <= sendingOutFetch1 and (not bufferAccepting or not bpAccepting);

	frontBranchEvent <= earlyBranchOut.controlInfo.newEvent and earlyBranchSending;
    frontCausingSig.target <= earlyBranchOut.target;

    -- Outputs 

    -- Pipeline (F2) (may be delayed any number of cycles)
	lastSending <= sendingOutBuffer;
	--dataLastLiving <= ibufDataOut;
    	dataOut_N <= ibufDataOut_N;
    
    -- Pipeline F2    
    --bpData <= dataBranchTransferOut;
        bpData_N <= bqDataSig;
    bpSending <= sendingToBQ;

    -- Events
	frontEventSignal <= frontBranchEvent;	
    frontCausing_N.full <= frontBranchEvent;
    frontCausing_N.value <= earlyBranchOut.target;
        
    -- 	Pipeline backwards
	frontAccepting <= '1';

end Behavioral;
