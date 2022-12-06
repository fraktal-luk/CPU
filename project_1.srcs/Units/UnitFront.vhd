
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
		lateEventSetPC: in std_logic;
		
		  dbState: in DbCoreState		
	);
end UnitFront;


architecture Behavioral of UnitFront is
	signal resetSig, enSig: std_logic := '0';							

    signal dummyBP0, dummyBP1: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0'); -- Results from BP
	
	signal fetchedLine0, fetchedLine1, fetchedLine1_Sh: WordArray(0 to FETCH_WIDTH-1) := (others => (others => '0'));
	
	signal sendingOutFetch0, sendingOutFetch1, sendingOutBuffer, bufferAccepting, earlyBranchSending, sendingToBranchTransfer,
           pcEn, frontBranchEvent, killAll, killAllOrFront, sendingToEarlyBranch, sendingToBQ, sendingToBuffer, fetchStall, full0, full1, fullBr, fullBt,
	                                                                                                      ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';
    signal cpFetch0, cpFetch1, earlyBranchIn, earlyBranchIn_N, earlyBranchOut, stageDataInFetch0, stageDataOutFetch0, stageDataOutFetch1: ControlPacket := DEFAULT_CONTROL_PACKET;
    signal bqDataSig, bqDataSigPre: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);

	signal predictedAddress, frontTarget: Mword := (others => '0');
	signal decodeCounter, decodeCounterNext: Word := (others => '0');

	signal dataToIbuffer, ibufDataOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
begin
	killAll <= execEventSignal or lateEventSignal;
    killAllOrFront <= killAll or frontBranchEvent;

    decodeCounterNext <= addInt(decodeCounter, countOnes(extractFullMask(dataToIbuffer))) when sendingToBuffer = '1' else decodeCounter;

	fetchedLine0 <= iin;

    pcEn <= pcDataIn.controlInfo.full;

    process(clk)
    begin
        if rising_edge(clk) then
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
        stageDataInFetch0.dbInfo <= pcDataIn.dbInfo;

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
	    signal partMask, partMask_N --, decodedFullMask
	    : std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        signal decodedGroup, dataToIbuffer_IS: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        signal decodedGroup_N, dataToIbuffer_IS_N: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        signal toBQ, data_C, data_CA: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
        signal       data_C_N, data_CA_N: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
        signal frontControl, frontControl_T: ControlPacket := DEFAULT_CONTROL_PACKET;
        
            signal groupIP: Mword := (others => '0');
        
        signal groupShift: SmallNumber := sn(0);	   
        signal lastIndex: SmallNumber := sn(3);
        
        signal hasBranch: std_logic := '0';
        
            signal cv0, cv1: std_logic_vector( 0 to FETCH_WIDTH-1) := (others => '0');
	begin
	       groupIP <= --stageDataOutFetch1.ip;
	                   predictedAddress;
	
	    partMask <= partialMask(predictedAddress);

        groupShift(1 downto 0) <= predictedAddress(3 downto 2);
        lastIndex <= frontControl.tags.bqPointer;

            fetchedLine1_Sh <= fetchedLine1                                                               when groupShift(1 downto 0) = "00"
                        else   fetchedLine1(1 to 3) & fetchedLine1(3)                                     when groupShift(1 downto 0) = "01"
                        else   fetchedLine1(2 to 3) & fetchedLine1(3) & fetchedLine1(3)                   when groupShift(1 downto 0) = "10"
                        else   fetchedLine1(3 to 3) & fetchedLine1(3) & fetchedLine1(3) & fetchedLine1(3) when groupShift(1 downto 0) = "11";

            partMask_N <= partMask                      when groupShift(1 downto 0) = "00"
                        else   partMask(1 to 3) & "0"   when groupShift(1 downto 0) = "01"
                        else   partMask(2 to 3) & "00"  when groupShift(1 downto 0) = "10"
                        else   partMask(3 to 3) & "000" when groupShift(1 downto 0) = "11";

        frontControl <= getFrontEvent(predictedAddress,
                                        stageDataOutFetch1.target, fetchedLine1, partMask); -- Here stageDataOutFetch1.ip is aligned inside the function
            frontControl_T <= getFrontEvent(predictedAddress,
                                        stageDataOutFetch1.target, fetchedLine1, partMask); -- Here stageDataOutFetch1.ip is aligned inside the function
        earlyBranchIn <= getEarlyEvent(frontControl, stageDataOutFetch1.target, predictedAddress, fetchStall, sendingOutFetch1);


--        decodedGroup <= decodeGroup(stageDataOutFetch1, fetchedLine1, groupIP, partMask, true);
--        dataToIbuffer_IS <= adjustStage(decodedGroup, slv2u(groupShift), slv2u(lastIndex) + 1 - slv2u(groupShift));

                decodedGroup_N <= decodeGroup(stageDataOutFetch1, fetchedLine1_Sh, groupIP, partMask_N, false);
                dataToIbuffer_IS_N <= adjustStage(decodedGroup_N, 0                , slv2u(lastIndex) + 1 - slv2u(groupShift));
                                              
                ch0 <= bool2std(dataToIbuffer_IS_N = dataToIbuffer_IS);
                                                    
                CV_0: for i in 0 to FETCH_WIDTH-1 generate
                    cv0(i) <= bool2std(dataToIbuffer_IS_N(i) = dataToIbuffer_IS(i));
                    cv1(i) <= bool2std(data_CA_N(i) = data_CA(i));
                end generate;
                                                    
        --    ch0 <= groupHasBranch(dataToIbuffer_IS);
        hasBranch <= groupHasBranch(dataToIbuffer);
             
       --      ch1 <= hasBranch xnor ch0;
             
        dataToIbuffer <= assignSeqNum(getEntryArray(dataToIbuffer_IS_N), decodeCounter);


--        data_C <= getControlA(groupIP, fetchedLine1, partMask, true);--, decodedGroup(0).ins.controlInfo.firstBr);
--        data_CA <= adjustStage(data_C, slv2u(groupShift), slv2u(lastIndex) + 1 - slv2u(groupShift));

                data_C_N <= getControlA(groupIP, fetchedLine1_Sh, partMask_N, false);--, decodedGroup(0).ins.controlInfo.firstBr);
                data_CA_N <= adjustStage(data_C_N, 0            , slv2u(lastIndex) + 1 - slv2u(groupShift));
        
                    ch1 <= bool2std(data_CA_N = data_CA);
        
        
                        ch2 <= bool2std(predictedAddress = stageDataOutFetch1.ip);
                        ch3 <= sendingOutFetch1 and not ch2;
                            ch4 <= bool2std(frontControl_T = frontControl);

        toBQ <= prepareForBQ(groupIP, data_CA_N, hasBranch);
        bqDataSigPre <= assignSeqNum(toBQ, decodeCounter);
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
