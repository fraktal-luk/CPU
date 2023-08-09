

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicQueues.all;


entity MissQueue is
	generic(
		QUEUE_SIZE: integer := 8;
		IS_LOAD_QUEUE: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		acceptingOut: out std_logic; -- UNUSED
		almostFull: out std_logic;   -- UNUSED
		acceptAlloc: out std_logic;
		
   	    prevSendingRe: in std_logic;
		prevSending: in std_logic;

        renameMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        inputMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        systemMask: in std_logic_vector(0 to PIPE_WIDTH-1);

        renamedPtr: out SmallNumber;

		compareAddressEarlyInput: in ExecResult;
        compareAddressEarlyInput_Ctrl: in ControlPacket;

		compareAddressInput: in ExecResult;
		compareAddressCtrl: in ControlPacket;

		storeValueResult: in ExecResult;

        selectedDataOutput: out ControlPacket;
        selectedDataResult: out ExecResult;

		committing: in std_logic;
        commitMask: in std_logic_vector(0 to PIPE_WIDTH-1);
        commitEffectiveMask: in std_logic_vector(0 to PIPE_WIDTH-1);

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
    	execCausing: in ExecResult;

		nextAccepting: in std_logic;		

		committedEmpty: out std_logic;
		committedSending: out std_logic;
		committedDataOut: out ControlPacket;
		
		dbState: in DbCoreState
	);
end MissQueue;

architecture DefaultMQ of MissQueue is

    type MQ_Entry is record
        full: std_logic;
        ready: std_logic;
        active: std_logic;
        
        tag: InsTag;
        lqPointer: SmallNumber;
        sqPointer: SmallNumber;
        
        dest: PhysName;
        adr: Mword;
        sqTag: SmallNumber;
        op: SpecificOp;
        
        fp: std_logic;
        tlbMiss: std_logic;
        dataMiss: std_logic;
        sqMiss: std_logic;
        
        TMP_cnt: SmallNumber;
        
        dbInfo: InstructionDebugInfo;
    end record;
    
    constant DEFAULT_MQ_ENTRY: MQ_Entry := (
        full => '0',
        ready => '0',
        active => '0',
        tag => (others => '0'),
        lqPointer => (others => '0'),
        sqPointer => (others => '0'),
        dest => (others => '0'),
        adr => (others => '0'),
        sqTag => (others => '0'),
        op => DEFAULT_SPECIFIC_OP,
        fp => '0',
        tlbMiss => '0',
        dataMiss => '0',
        sqMiss => '0',
        
        TMP_cnt => (others => '0'),
        
        dbInfo => DEFAULT_DEBUG_INFO);
    
    type MQ_EntryArray is array(integer range <>) of MQ_Entry;
    
    signal TMP_prevSending, canSend, sending, sending1, sending2, sending3, isFull, isAlmostFull, accepting: std_logic := '0';

    signal outEntrySig: MQ_Entry := DEFAULT_MQ_ENTRY;

    signal queueContent: MQ_EntryArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_MQ_ENTRY);    
    signal addresses, tags, renameTags: MwordArray(0 to QUEUE_SIZE-1) := (others => (others => '0'));
    
    signal fullMask, killMask, selectMask, inputFullMask, readyMask, outputFullMask3: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0'); 
    signal writePtr, selPtr0, selPtr1, selPtr2, selPtr3, nFull, nFullNext, nIn, nInRe, nOut, nCommitted, nCommittedEffective, recoveryCounter: SmallNumber := (others => '0');

    signal selValid0, selValid1, selValid2, selValid3: std_logic := '0';

    signal adrInWord, adrOutWord, tagInWord, tagOutWord, renameTagOutWord: Mword := (others => '0');
    
    function TMP_getNewIndex(fullMask: std_logic_vector) return natural is
        variable res: natural := 0;
    begin
        for i in 0 to fullMask'length-1 loop
            if fullMask(i) /= '1' then
                return i;
            end if;
        end loop;
        
        return res;
    end function;
    
    function getKillMask(content: MQ_EntryArray; execEvent, lateEvent: std_logic; tag: InsTag) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        if lateEvent = '1' then
            res := (others => '1');
            return res;
        elsif execEvent /= '1' then
            return res;
        end if;

        for i in 0 to res'length-1 loop
            if compareTagBefore(tag, content(i).tag) = '1' then
                res(i) := '1';
            end if;
        end loop;
        
        return res;
    end function;

    function getInputMask(content: MQ_EntryArray; index: natural; prevSending: std_logic) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        if prevSending = '1' then
            res(index) := '1';
        end if;
        
        return res;
    end function;

    function getSelectMask(content: MQ_EntryArray) return std_logic_vector is
        variable res: std_logic_vector(0 to content'length-1) := (others => '0');
    begin
        for i in 0 to content'length - 1 loop
            if content(i).full = '1' and content(i).active = '1' and content(i).ready = '1' then
                res(i) := '1';
            end if;
        end loop;

        res := getFirstOne(res);
        return res;
    end function;

    function maskFromIndex(index: SmallNumber; constant len: natural) return std_logic_vector is
        variable res: std_logic_vector(0 to len-1) := (others => '0');
        constant indexI: natural := slv2u(index);
    begin
        res(indexI) := '1';
        return res;
    end function;

    function TMP_firstOnePtr(v: std_logic_vector) return SmallNumber is
        variable res: SmallNumber := (others => '0');
    begin
        for i in 0 to v'length-1 loop
            if v(i) = '1' then
                return i2slv(i, SMALL_NUMBER_SIZE);
            end if;
        end loop;
        return res;
    end function;

    signal prevSendingEarly: std_logic := '0';

    signal ch0, ch1, ch2, ch3: std_logic := '0'; 
begin

        prevSendingEarly <= --compareAddressEarlyInput.full;
                            compareAddressEarlyInput_Ctrl.controlInfo.full;

    canSend <= '1';

    TMP_prevSending <= compareAddressInput.full;

    writePtr <= i2slv(TMP_getNewIndex(fullMask), SMALL_NUMBER_SIZE);
    killMask <= getKillMask(queueContent, execEventSignal, lateEventSignal, execCausing.tag);
    inputFullMask <= maskFromIndex(writePtr, QUEUE_SIZE) when TMP_prevSending = '1' else (others => '0');

    -- completion and subsequent removal from queue is triggered by:
    --      L1 fill in case of Cache miss ops
    --      SQ value fill in case of missed SQ forwarding 

    selectMask <= getSelectMask(queueContent);

    sending <= canSend and isNonzero(selectMask);

    selPtr0 <= TMP_firstOnePtr(selectMask);
    selValid0 <= sending;

    outputFullMask3 <= maskFromIndex(selPtr3, QUEUE_SIZE) when selValid3 = '1' else (others => '0');


    READY_MASL: for i in 0 to QUEUE_SIZE-1 generate
        fullMask(i) <= queueContent(i).full;
        readyMask(i) <= queueContent(i).ready;
    end generate;

    nFull <= i2slv(countOnes(fullMask), SMALL_NUMBER_SIZE); 

    process (clk)

    begin
        if rising_edge(clk) then

            selPtr1 <= selPtr0;
            selPtr2 <= selPtr1;
            selPtr3 <= selPtr2;
        
            selValid1 <= selValid0;
            selValid2 <= selValid1;
            selValid3 <= selValid2;
            
            sending1 <= sending;
            sending2 <= sending1;
            sending3 <= sending2;

            for i in 0 to QUEUE_SIZE-1 loop

                -- Wakeup for dependents on SQ:
                -- CAREFUL: compare SQ pointers ignoring the high bit of StoreData sqPointer (used for ordering, not indexing the content!)    
                if queueContent(i).active = '1' and queueContent(i).sqMiss = '1' and storeValueResult.full = '1' and queueContent(i).sqTag(2 downto 0) = storeValueResult.dest(2 downto 0) then
                    queueContent(i).ready <= '1';
                end if;

                if (canSend and selectMask(i)) = '1' then
                    queueContent(i).active <= '0';
                end if;

                -- CAREFUL: can't be reordered after "Update late part" because 'full' signals would be incorrect
                queueContent(i).full <= (fullMask(i) and not killMask(i) and not outputFullMask3(i)) or inputFullMask(i);
                queueContent(i).TMP_cnt <= addIntTrunc(queueContent(i).TMP_cnt, 1, 3);

                -- Update late part of slot or free it if no miss
                if queueContent(i).full = '1' and queueContent(i).active /= '1' and queueContent(i).ready /= '1' and queueContent(i).TMP_cnt = X"02" then
                    -- if TMP_prevSending => confirm full, fill other info from compareAddressInput, compareAddressCtrl
                    -- if not TMP_prevSending => clear
                    if TMP_prevSending = '1' then
                        queueContent(i).active <= '1';
                        queueContent(i).adr <= compareAddressCtrl.ip;
                        queueContent(i).sqTag <= compareAddressCtrl.target(SMALL_NUMBER_SIZE-1 downto 0);

                        queueContent(i).dest <= compareAddressInput.dest;

                        queueContent(i).lqPointer <= compareAddressCtrl.tags.lqPointer;
                        queueContent(i).sqPointer <= compareAddressCtrl.tags.sqPointer;
                        
                        queueContent(i).op <= compareAddressCtrl.op;
        
                        queueContent(i).fp <= compareAddressCtrl.classInfo.useFP;
                        queueContent(i).tlbMiss <= compareAddressCtrl.controlInfo.tlbMiss;
                        queueContent(i).dataMiss <= compareAddressCtrl.controlInfo.dataMiss;
                        queueContent(i).sqMiss <= compareAddressCtrl.controlInfo.sqMiss;

                        addresses(i) <= compareAddressCtrl.ip;
                        tags(i) <= tagInWord; -- CAREFUL: tag is duplicated (this used for output, other accessible for comparisons when kill signal). 
                                              --          Impact on efficiencynot known (redundant memory but don't need output mux for FF data) 
                        renameTags(i)(TAG_SIZE-1 downto 0) <= compareAddressCtrl.tag;
                        
                        -- pragma synthesis off
                        if DB_OP_TRACKING and queueContent(i).dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                            report "";
                            report "DEBUG: put to MQ: " & --natural'image(slv2u(queueContent(i).dbInfo.seqNum));
                                                          work.CpuText.slv2hex(DB_TRACKED_SEQ_NUM);
                            report "";
                        end if;
                        -- pragma synthesis on
                        
                    else
                        queueContent(i).full <= '0';    
                        queueContent(i).active <= '0';
                        
                        queueContent(i).dbInfo <= DEFAULT_DEBUG_INFO;
                    end if;

                end if;
                
                if killMask(i) = '1' then
                    queueContent(i).full <= '0';
                    queueContent(i).active <= '0';
                    queueContent(i).ready <= '0';
                    
                    queueContent(i).dbInfo <= DEFAULT_DEBUG_INFO;

                    -- pragma synthesis off
                    if DB_OP_TRACKING and queueContent(i).dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                        report "";
                        report "DEBUG: flushed from MQ: " & --natural'image(slv2u(queueContent(i).dbInfo.seqNum));
                                                            work.CpuText.slv2hex(DB_TRACKED_SEQ_NUM);
                        report "";
                    end if;
                    -- pragma synthesis on
                end if;
            end loop;

            if prevSendingEarly = '1' then
                queueContent(p2i(writePtr, QUEUE_SIZE)).full <= '1';
                queueContent(p2i(writePtr, QUEUE_SIZE)).ready <= '0';
                queueContent(p2i(writePtr, QUEUE_SIZE)).active <= '0';--'1';              
                queueContent(p2i(writePtr, QUEUE_SIZE)).tag <= --compareAddressEarlyInput.tag;
                                                               compareAddressEarlyInput_Ctrl.tags.renameIndex;
                queueContent(p2i(writePtr, QUEUE_SIZE)).TMP_cnt <= (others => '0');
                
                queueContent(p2i(writePtr, QUEUE_SIZE)).dbInfo <= compareAddressEarlyInput_Ctrl.dbInfo;
            end if;



            if sending3 = '1' then
                -- pragma synthesis off
                if DB_OP_TRACKING and queueContent(p2i(selPtr3, QUEUE_SIZE)).dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                    report "";
                    report "DEBUG: sent from MQ: " & --natural'image(slv2u(queueContent(p2i(selPtr3, QUEUE_SIZE)).dbInfo.seqNum));
                                                     work.CpuText.slv2hex(DB_TRACKED_SEQ_NUM);
                    report "";
                end if;
                -- pragma synthesis on

                queueContent(p2i(selPtr3, QUEUE_SIZE)) <= DEFAULT_MQ_ENTRY;
            end if;

            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
            recoveryCounter(7 downto 2) <= (others => '0');
            
            isFull <= cmpGtU(nFull, QUEUE_SIZE-3);
            isAlmostFull <= cmpGtU(nFull, QUEUE_SIZE-5);
        end if;

        --isFull <= cmpGtU(nFull, QUEUE_SIZE-2);
        --isAlmostFull <= cmpGtU(nFull, QUEUE_SIZE-5);
    end process;
    
    accepting <= not isFull;
    
    tagInWord <= compareAddressCtrl.tags.lqPointer & compareAddressCtrl.tags.sqPointer & compareAddressInput.dest & X"00";
    
    outEntrySig <= queueContent(p2i(selPtr2, QUEUE_SIZE));
    adrOutWord <= addresses(p2i(selPtr2, QUEUE_SIZE));
    tagOutWord <= tags(p2i(selPtr2, QUEUE_SIZE));
    renameTagOutWord <= renameTags(p2i(selPtr2, QUEUE_SIZE));

    selectedDataOutput.controlInfo.full <= sending2 and outEntrySig.full and not lateEventSignal;
    selectedDataOutput.tag <= renameTagOutWord(TAG_SIZE-1 downto 0);
    selectedDataResult.dest <= tagOutWord(15 downto 8);
    
    selectedDataOutput.target <= adrOutWord;
    selectedDataOutput.op <= outEntrySig.op;
    selectedDataOutput.classInfo.useFP <= outEntrySig.fp;
    selectedDataOutput.controlInfo.tlbMiss <= outEntrySig.tlbMiss;
    selectedDataOutput.controlInfo.dataMiss <= outEntrySig.dataMiss;
    selectedDataOutput.controlInfo.sqMiss <= outEntrySig.sqMiss;
    selectedDataOutput.tags.renameIndex <= outEntrySig.tag;
    selectedDataOutput.tags.lqPointer <= tagOutWord(31 downto 24);   
    selectedDataOutput.tags.sqPointer <= tagOutWord(23 downto 16);

    selectedDataOutput.dbInfo <= outEntrySig.dbInfo;
    selectedDataResult.dbInfo <= outEntrySig.dbInfo;


    committedSending <= sending1; -- Indication to block normal mem issue
    
        almostFull <= isAlmostFull;
        acceptingOut <= accepting;
end DefaultMQ;

