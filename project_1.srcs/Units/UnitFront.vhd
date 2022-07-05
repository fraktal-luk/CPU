
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
        bpData: out ControlPacketArray(0 to FETCH_WIDTH-1);

		-- Interface front to renaming
		renameAccepting: in std_logic;		
		dataOut: out BufferEntryArray; 
		lastSending: out std_logic;
		-------
		
		frontEventSignal: out std_logic;
		frontCausing: out ExecResult;

		execCausing: in ExecResult;
		lateCausing: in ExecResult;
		
		execEventSignal: in std_logic;
		lateEventSignal: in std_logic;
		lateEventSetPC: in std_logic		
	);
end UnitFront;


architecture Behavioral of UnitFront is
	signal resetSig, enSig: std_logic := '0';							

    signal dummyBP0, dummyBP1: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0'); -- Results from BP
	
	signal fetchedLine0, fetchedLine1: WordArray(0 to FETCH_WIDTH-1) := (others => (others => '0'));
	
	signal sendingOutFetch0, sendingOutFetch1, sendingOutBuffer, bufferAccepting, earlyBranchSending, sendingToBranchTransfer,
           pcEn, frontBranchEvent, killAll, killAllOrFront, sendingToEarlyBranch, sendingToBQ, sendingToBuffer, fetchStall, full0, full1, fullBr, fullBt,
	                                                                                                      ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';	

    signal cpFetch0, cpFetch1, earlyBranchIn, earlyBranchOut, stageDataInFetch0, stageDataOutFetch0, stageDataOutFetch1: ControlPacket := DEFAULT_CONTROL_PACKET;
    signal bqDataSig, bqDataSigPre: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);

	signal predictedAddress, frontTarget: Mword := (others => '0');
	signal fetchCounter, fetchCounterNext, decodeCounter, decodeCounterNext: Word := (others => '0');

	signal dataToIbuffer, ibufDataOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
begin
	killAll <= execEventSignal or lateEventSignal;
    killAllOrFront <= killAll or frontBranchEvent;

    fetchCounterNext <= addInt(fetchCounter, PIPE_WIDTH) when pcEn = '1' else fetchCounter;
    decodeCounterNext <= addInt(decodeCounter, countOnes(extractFullMask(dataToIbuffer))) when sendingToBuffer = '1' else decodeCounter;

	fetchedLine0 <= iin;

    pcEn <= pcDataIn.controlInfo.full;

    process(clk)
    begin
        if rising_edge(clk) then
            fetchCounter <= fetchCounterNext;
            decodeCounter <= decodeCounterNext;
            
            -- fetchedLine0: assigned async
            fetchedLine1 <= fetchedLine0;

            full0 <= bool2std(pcEn = '1') and not killAllOrFront;
            full1 <= sendingOutFetch0 and not killAllOrFront;          -- F1
            fullBr <= sendingOutFetch1 and not killAll;                -- F2
            fullBt <= bool2std(sendingToBranchTransfer = '1') and not killAll; -- F2

            earlyBranchOut <= earlyBranchIn;
                
            bqDataSig <= bqDataSigPre;

            stageDataOutFetch0 <= stageDataInFetch0;
            stageDataOutFetch1 <= stageDataOutFetch0;

            -- Stage F0
            
            -- Stage F1
            
            -- Stage Ibuf/BrEval
            
            -- 

        end if;
    end process;

    stageDataInFetch0.ip <= pcDataIn.ip;
    stageDataInFetch0.target <= pcDataIn.target;
--        stageDataInFetch0.dbInfo <= pcDataIn.dbInfo;

    stageDataInFetch0.tags.fetchCtr <= fetchCounter when not CLEAR_DEBUG_INFO else (others => '0');  
    
    sendingOutFetch0 <= full0 and not killAllOrFront;
    sendingOutFetch1 <= full1 and not killAllOrFront;
    earlyBranchSending <= fullBr and not killAll;
    sendingToBQ <= fullBt and not killAll;

    sendingToEarlyBranch <= sendingOutFetch1;

	SAVE_PRED_TARGET: process(clk)
	begin
		if rising_edge(clk) then
			if lateEventSetPC = '1' then				
				predictedAddress <= lateCausing.value;
			elsif execEventSignal = '1' then
				predictedAddress <= execCausing.value;
			elsif frontBranchEvent = '1' then -- CAREFUL: must equal frontEventSignal
				predictedAddress <= frontTarget;
			elsif sendingToEarlyBranch = '1' then -- Normal flow - target from line predictor
				predictedAddress <= earlyBranchIn.target;
			end if;
		end if;
	end process;
	
	sendingToBuffer <= sendingOutFetch1 and not fetchStall;
	
	LEGACY: block
		signal earlyBranchIn_OLD: InstructionState := DEFAULT_INSTRUCTION_STATE;
        signal dataToIbuffer_OLD: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        signal earlyBranchMultiDataInA: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);	   
	begin
        -- src for Ibuf and BT
        earlyBranchMultiDataInA <= getFrontEventMulti(predictedAddress, stageDataOutFetch1.ip, stageDataOutFetch1.target, stageDataOutFetch1, fetchedLine1);
    
        earlyBranchIn_OLD <= getEarlyEvent(earlyBranchMultiDataInA, stageDataOutFetch1.target, predictedAddress, fetchStall);
            earlyBranchIn.controlInfo <= earlyBranchIn_OLD.controlInfo;
            earlyBranchIn.target <= earlyBranchIn_OLD.target;

        dataToIbuffer_OLD <= adjustStage(earlyBranchMultiDataInA);
            dataToIbuffer <= assignSeqNum(getEntryArray(dataToIbuffer_OLD), decodeCounter);
            bqDataSigPre <= assignSeqNum(prepareForBQ(stageDataOutFetch1.ip, dataToIbuffer_OLD), decodeCounter);
    end block;

	sendingToBranchTransfer <= sendingOutFetch1 and not fetchStall;

	SUBUNIT_IBUFFER: entity work.InstructionBuffer(Implem)
	port map(
		clk => clk, reset => resetSig, en => enSig,
		
		prevSending => sendingToBuffer,
		nextAccepting => renameAccepting,
		stageDataIn => dataToIbuffer,
		acceptingOut => bufferAccepting,
		sendingOut => sendingOutBuffer,
		stageDataOut => ibufDataOut,
		
		execEventSignal => killAll
	);


	fetchStall <= sendingOutFetch1 and (not bufferAccepting or not bpAccepting);

	frontBranchEvent <= earlyBranchOut.controlInfo.newEvent and earlyBranchSending;
    frontTarget <= earlyBranchOut.target;

    -- Outputs 

    -- Pipeline (F2) (may be delayed any number of cycles)
	lastSending <= sendingOutBuffer;
    dataOut <= ibufDataOut;
    
    -- Pipeline F2    
    bpData <= bqDataSig;
    bpSending <= sendingToBQ;

    -- Events
	frontEventSignal <= frontBranchEvent;	
    frontCausing.full <= frontBranchEvent;
    frontCausing.value <= earlyBranchOut.target;
        
    -- 	Pipeline backwards
	frontAccepting <= '1';

end Behavioral;
