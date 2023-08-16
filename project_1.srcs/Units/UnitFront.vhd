
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
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

		bqAccepting: in std_logic;
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
		lateEventSetPC: in std_logic;
		
		dbState: in DbCoreState		
	);
end UnitFront;


architecture Behavioral of UnitFront is
	signal resetSig, enSig: std_logic := '0';							

    signal dummyBP0, dummyBP1: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0'); -- Results from BP

	signal fetchedLine0, fetchedLine1, fetchedLineShifted0, fetchedLineShifted1, fetchedLineShifted1_Alt: WordArray(0 to FETCH_WIDTH-1) := (others => (others => '0'));

	signal full0, full1, sendingOutFetch0, sendingOutFetch1, bufferAccepting, queuesAccepting, fullBr, fullBt, earlyBranchSending, sendingToBuffer,
           pcEn, fetchStall, frontBranchEvent, killAll, killAllOrFront, sendingOutBuffer, sendingToBQ,
                                                                             ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';
    signal stageDataInFetch0, stageDataOutFetch0, stageDataOutFetch1, stallCt, normalCt, earlyBranchIn, earlyBranchOut: ControlPacket := DEFAULT_CONTROL_PACKET;

	signal predictedAddress, predictedAddressNext: Mword := (others => '0');

    signal toBQ, bqDataSig, bqDataSigPre: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
	signal decodedEA, dataToIbuffer, ibufDataOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);

    signal groupShift, groupShift_Early: SmallNumber := sn(0);
    signal nW: natural := 0;

    signal decodeCounter: Word := (others => '0'); -- DB

begin
	killAll <= execEventSignal or lateEventSignal;
    killAllOrFront <= killAll or frontBranchEvent;

	fetchedLine0 <= iin;
    pcEn <= pcDataIn.controlInfo.full;

    process(clk)
    begin
        if rising_edge(clk) then

            -- Stage F0
            stageDataOutFetch0 <= stageDataInFetch0;
            -- fetchedLine0: assigned async
            full0 <= bool2std(pcEn = '1') and not killAllOrFront;

            -- Stage F1
            stageDataOutFetch1 <= stageDataOutFetch0;
            fetchedLine1 <= fetchedLine0;
                fetchedLineShifted1_Alt <= fetchedLineShifted0;
            full1 <= sendingOutFetch0 and not killAllOrFront;          -- F1

            -- Stage Ibuf/BrEval
			predictedAddress <= predictedAddressNext;

            earlyBranchOut <= earlyBranchIn;
            fullBr <= sendingOutFetch1 and not killAll;                -- F2
            fullBt <= bool2std(sendingToBuffer = '1') and not killAll; -- F2
            bqDataSig <= bqDataSigPre; -- F2


            if sendingToBuffer = '1' then
                DB_trackSeqNum(dataToIbuffer);
                decodeCounter <= addInt(decodeCounter, countOnes(extractFullMask(dataToIbuffer)));
            end if;

        end if;
    end process;

    stageDataInFetch0.ip <= pcDataIn.ip;
    stageDataInFetch0.target <= pcDataIn.target;
    stageDataInFetch0.dbInfo <= pcDataIn.dbInfo;

    sendingOutFetch0 <= full0 and not killAllOrFront;
    sendingOutFetch1 <= full1 and not killAllOrFront;

    earlyBranchSending <= fullBr and not killAll;
    sendingToBQ <= fullBt and not killAll;

    queuesAccepting <= bufferAccepting and bqAccepting;
	fetchStall <= sendingOutFetch1 and not queuesAccepting;
	sendingToBuffer <= sendingOutFetch1 and queuesAccepting;


    stallCt <= getStallEvent(predictedAddress);
    normalCt <= getNormalEvent(stageDataOutFetch1.target,-- getFrontEvent(predictedAddress, stageDataOutFetch1.target, fetchedLine1),
                                                          predictedAddress, fetchedLine1);

    earlyBranchIn <=      stallCt when --fetchStall = '1'
                                        queuesAccepting /= '1'
                    else  normalCt
                   --                 when sendingToBuffer = '1'
                   -- else  DEFAULT_CONTROL_PACKET
                        ;


    predictedAddressNext <= lateCausing.value     when lateEventSetPC = '1'
                       else execCausing.value     when execEventSignal = '1'
                       else earlyBranchOut.target when frontBranchEvent = '1'
                       else normalCt.target       when sendingToBuffer = '1'
                       else predictedAddress;

    groupShift_Early(LOG2_PIPE_WIDTH-1 downto 0) <= normalCt.target(LOG2_PIPE_WIDTH+1 downto 2) when sendingToBuffer = '1'
                                            else    predictedAddress(LOG2_PIPE_WIDTH+1 downto 2);
    fetchedLineShifted0 <= shiftLine(fetchedLine0, groupShift_Early);


    groupShift(LOG2_PIPE_WIDTH-1 downto 0) <= predictedAddress(LOG2_PIPE_WIDTH+1 downto 2);
    nW <= slv2u(normalCt.tags.bqPointer) + 1 - slv2u(groupShift);

    fetchedLineShifted1 <= --shiftLine(fetchedLine1, groupShift);
                           fetchedLineShifted1_Alt;


    decodedEA <= decodeGroup(fetchedLineShifted1, nW, predictedAddress, stageDataOutFetch1);
    dataToIbuffer <= assignSeqNum(decodedEA, decodeCounter); -- TODO: DB (decodeCounter incremented per instruction)

    toBQ <= getControlA(fetchedLineShifted1, nW, predictedAddress, groupHasBranch(dataToIbuffer));
    bqDataSigPre <= assignSeqNum(toBQ, decodeCounter); -- TODO: DB


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

	frontBranchEvent <= earlyBranchOut.controlInfo.newEvent and earlyBranchSending;

    -- Outputs 

    -- Pipeline (F2) (may be delayed any number of cycles)
	lastSending <= sendingOutBuffer;
    dataOut <= ibufDataOut;
    
    -- Pipeline F2    
    bpData <= bqDataSig;
    bpSending <= sendingToBQ;

    -- Events
	frontEventSignal <= frontBranchEvent;	
    frontCausing.full <= frontBranchEvent; -- redundant with frontBranchEvent
    frontCausing.value <= earlyBranchOut.target;--frontTarget;

    -- 	Pipeline backwards
	frontAccepting <= '1';

end Behavioral;
