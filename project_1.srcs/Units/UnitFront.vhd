
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
	
	-- TODO: move to DB only
	signal decodeCounter, decodeCounterNext: Word := (others => '0');

	signal decodedEA, dataToIbuffer, ibufDataOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
	   
	   
	   procedure DB_trackSeqNum(arr: BufferEntryArray) is
       begin
           -- pragma synthesis off
           if DB_OP_TRACKING then
               for i in arr'range loop
                   if arr(i).dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                       report "";
                       report "DEBUG: Tracked seqNum assigned: " & integer'image(slv2u(DB_TRACKED_SEQ_NUM));
                       report "";
                   end if;
               end loop;
           end if;
           -- pragma synthesis on
       end procedure;
	   
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
            if sendingToBuffer = '1' then
                DB_trackSeqNum(dataToIbuffer);   
            end if;

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
	    signal partMask, partMask_N: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        signal toBQ: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
        signal frontControl: ControlPacket := DEFAULT_CONTROL_PACKET;
                
        signal groupShift: SmallNumber := sn(0);	   
        signal lastIndex: SmallNumber := sn(3);
        
        signal hasBranch: std_logic := '0';        
	begin
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

        frontControl <= getFrontEvent(predictedAddress, stageDataOutFetch1.target, fetchedLine1, partMask);
        earlyBranchIn <= getEarlyEvent(frontControl, stageDataOutFetch1.target, predictedAddress, fetchStall, sendingOutFetch1);

        decodedEA <= decodeGroup(fetchedLine1_Sh, slv2u(lastIndex) + 1 - slv2u(groupShift), predictedAddress, stageDataOutFetch1);

        dataToIbuffer <= assignSeqNum(decodedEA, decodeCounter, stageDataOutFetch1); -- TODO: DB (decodeCounter incremented per instruction)

        hasBranch <= groupHasBranch(dataToIbuffer);

        toBQ <= getControlA(predictedAddress, fetchedLine1_Sh, slv2u(lastIndex) + 1 - slv2u(groupShift), hasBranch);

        bqDataSigPre <= assignSeqNum(toBQ, decodeCounter); -- TODO: DB
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
