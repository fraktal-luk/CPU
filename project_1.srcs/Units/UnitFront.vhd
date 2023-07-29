
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

	signal full0, full1, sendingOutFetch0, sendingOutFetch1, bufferAccepting, fullBr, fullBt, earlyBranchSending, sendingToBuffer,
           pcEn, fetchStall, frontBranchEvent, killAll, killAllOrFront, sendingOutBuffer, sendingToBQ,
	                                                                                                    ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7: std_logic := '0';
    signal stageDataInFetch0, stageDataOutFetch0, stageDataOutFetch1, stallCt, normalCt, earlyBranchIn, earlyBranchOut: ControlPacket := DEFAULT_CONTROL_PACKET;

	signal predictedAddress, predictedAddressNext: Mword := (others => '0');

    signal toBQ, bqDataSig, bqDataSigPre: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
	signal decodedEA, dataToIbuffer, ibufDataOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);

    signal groupShift, groupShift_Early: SmallNumber := sn(0);
    signal nW: natural := 0;

    signal decodeCounter: Word := (others => '0'); -- DB

    procedure DB_trackSeqNum(arr: BufferEntryArray) is
    begin
       -- pragma synthesis off
       if DB_OP_TRACKING then
           for i in arr'range loop
               if arr(i).dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                   report "";
                   report "DEBUG: Tracked seqNum assigned: " & work.CpuText.slv2hex(DB_TRACKED_SEQ_NUM);
    
                   report "";
               end if;
           end loop;
       end if;
       -- pragma synthesis on
    end procedure;

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

            bqDataSig <= bqDataSigPre;

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


    PREDICTED_ADDRESS: block
        function getPA(late, exec, branch, normal, same: Mword; selLate, selExec, selBranch, selNormal: std_logic) return Mword is
        begin
            if selLate = '1' then
                return late;
            elsif selExec = '1' then
                return exec;
            elsif selBranch = '1' then
                return branch;
            elsif selNormal = '1' then
                return normal;
            else
                return same;
            end if;
        end function;

        function getChoice(late, exec, branch, normal, same: Mword; selLate, selExec, selBranch, selNormal: std_logic) return SmallNumber is
        begin
            if selLate = '1' then
                return sn(0);
            elsif selExec = '1' then
                return sn(1);
            elsif selBranch = '1' then
                return sn(2);
            elsif selNormal = '1' then
                return sn(3);
            else
                return sn(4);
            end if;
        end function;

        --signal k0,k1,k2,k3,k4,k5,e0,e1,e2,e3,e4,e5: std_logic := '0';
        --signal s0, s1, s2: std_logic := '0';
        --signal choice: SmallNumber := sn(-1);
        
        --signal normalTarget, nt0, nt1, nt2: Mword := (others => '0');
        
        --constant a10: Mword := X"00000010";
        --constant a20: Mword := X"00000020";
        
        --signal frontE: ControlPacket := DEFAULT_CONTROL_PACKET;
    begin
--        choice <= getChoice(lateCausing.value, execCausing.value, earlyBranchOut.target, normalCt.target, predictedAddress,
--                            lateEventSetPC, execEventSignal, frontBranchEvent, sendingToBuffer);
        
--        normalTarget <= --normalCt.target;
--                        frontE.target when (frontE.controlInfo.full = '1' and frontE.controlInfo.frontBranch = '1')
--                  else stageDataOutFetch1.target; 
--            nt0 <= normalTarget;
--            nt1 <= nt0;
--            nt2 <= nt1;
        
        predictedAddressNext <= lateCausing.value     when lateEventSetPC = '1'
                           else execCausing.value     when execEventSignal = '1'
                           else earlyBranchOut.target when frontBranchEvent = '1'
                           --else normalTarget          when sendingToBuffer = '1'
                           else normalCt.target       when sendingToBuffer = '1'
                           else predictedAddress;
        
--        predictedAddressNext <= getPA(lateCausing.value, execCausing.value, earlyBranchOut.target, normalCt.target, predictedAddress,
--                                      lateEventSetPC, execEventSignal, frontBranchEvent, sendingToBuffer);

--            e0 <= bool2std(normalCt.target = stageDataOutFetch1.target);
--            e2 <= bool2std(normalTarget = normalCt.target);
--            e3 <= bool2std(normalTarget = stageDataOutFetch1.target);
            
--            frontE <= getFrontEvent(predictedAddress, stageDataOutFetch1.target, fetchedLine1);
    end block;

	fetchStall <= sendingOutFetch1 and (not bufferAccepting or not bqAccepting);
	sendingToBuffer <= sendingOutFetch1 and not fetchStall;

    groupShift_Early(LOG2_PIPE_WIDTH-1 downto 0) <= normalCt.target(LOG2_PIPE_WIDTH+1 downto 2) when sendingToBuffer = '1'
                                            else    predictedAddress(LOG2_PIPE_WIDTH+1 downto 2);
    fetchedLineShifted0 <= shiftLine(fetchedLine0, groupShift_Early);


    stallCt <= getStallEvent(predictedAddress);
    normalCt <= getNormalEvent(stageDataOutFetch1.target, getFrontEvent(predictedAddress, stageDataOutFetch1.target, fetchedLine1));

    earlyBranchIn <=              stallCt when fetchStall = '1'
                            else  normalCt when sendingToBuffer = '1'
                            else  DEFAULT_CONTROL_PACKET;

    groupShift(LOG2_PIPE_WIDTH-1 downto 0) <= predictedAddress(LOG2_PIPE_WIDTH+1 downto 2);
    nW <= slv2u(normalCt.tags.bqPointer) + 1 - slv2u(groupShift);

    fetchedLineShifted1 <= --shiftLine(fetchedLine1, groupShift);
                           fetchedLineShifted1_Alt;


    decodedEA <= decodeGroup(fetchedLineShifted1, nW, predictedAddress, stageDataOutFetch1);
    dataToIbuffer <= assignSeqNum(decodedEA, decodeCounter, stageDataOutFetch1); -- TODO: DB (decodeCounter incremented per instruction)

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
    frontCausing.full <= frontBranchEvent;
    frontCausing.value <= earlyBranchOut.target;--frontTarget;

    -- 	Pipeline backwards
	frontAccepting <= '1';

end Behavioral;
