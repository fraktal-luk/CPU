
architecture MissQueue of StoreQueue is

    constant MQ_SIZE: natural := 8;

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
        
        TMP_cnt => (others => '0'));
    
    type MQ_EntryArray is array(integer range <>) of MQ_Entry;
    
    signal TMP_prevSending, canSend, sending, sending1, sending2, sending3, isFull, isAlmostFull, accepting: std_logic := '0';

    signal outEntrySig: MQ_Entry := DEFAULT_MQ_ENTRY;

    signal queueContent: MQ_EntryArray(0 to MQ_SIZE-1) := (others => DEFAULT_MQ_ENTRY);
        signal queueContent_N: MQ_EntryArray(0 to MQ_SIZE-1) := (others => DEFAULT_MQ_ENTRY);
    
    signal addresses, tags, renameTags: MwordArray(0 to MQ_SIZE-1) := (others => (others => '0'));
    
    signal fullMask, killMask, selectMask, inputFullMask, readyMask, outputFullMask3: std_logic_vector(0 to MQ_SIZE-1) := (others => '0'); 
    signal fullMask_N, killMask_N, -- selectMask,
            inputFullMask_N,
                readyMask_N --, outputFullMask3
                    : std_logic_vector(0 to MQ_SIZE-1) := (others => '0'); 

    signal writePtr, selPtr0, selPtr1, selPtr2, selPtr3,
	       nFull, nFullNext, nIn, nInRe, nOut, nCommitted, nCommittedEffective, recoveryCounter: SmallNumber := (others => '0');

        signal writePtr_N, selPtr0_N, selPtr1_N, selPtr2_N, selPtr3_N,
               nFull_N, nFullNext_N, nIn_N, nInRe_N, nOut_N, nCommitted_N, nCommittedEffective_N: SmallNumber := (others => '0');

    signal           selValid0, selValid1, selValid2, selValid3: std_logic := '0';

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

        prevSendingEarly <= compareAddressEarlyInput.full;

    canSend <= '1';

    TMP_prevSending <= compareAddressInput.full;

    writePtr <= i2slv(TMP_getNewIndex(fullMask), SMALL_NUMBER_SIZE);
    killMask <= getKillMask(queueContent, execEventSignal, lateEventSignal, execCausing.tag);
    inputFullMask <= maskFromIndex(writePtr, MQ_SIZE) when TMP_prevSending = '1' else (others => '0');

        writePtr_N <= i2slv(TMP_getNewIndex(fullMask_N), SMALL_NUMBER_SIZE);
        killMask_N <= getKillMask(queueContent_N, execEventSignal, lateEventSignal, execCausing.tag);
        inputFullMask_N <= maskFromIndex(writePtr_N, MQ_SIZE) when prevSendingEarly = '1' else (others => '0');


    -- completion and subsequent removal from queue is triggered by:
    --      L1 fill in case of Cache miss ops
    --      SQ value fill in case of missed SQ forwarding 

    selectMask <= getSelectMask(queueContent);

    sending <= canSend and isNonzero(selectMask);

    selPtr0 <= TMP_firstOnePtr(selectMask);
    selValid0 <= sending;

    outputFullMask3 <= maskFromIndex(selPtr3, MQ_SIZE) when selValid3 = '1' else (others => '0');

        -- Wakeup for dependents on SQ:
        -- CAREFUL: compare SQ pointers ignoring the high bit of StoreData sqPointer (used for ordering, not indexing the content!)


    READY_MASL: for i in 0 to MQ_SIZE-1 generate
        fullMask(i) <= queueContent(i).full;
        readyMask(i) <= queueContent(i).ready;
        
            fullMask_N(i) <= queueContent_N(i).full;
            readyMask_N(i) <= queueContent_N(i).ready;
    end generate;

       nFull <= i2slv(countOnes(fullMask), SMALL_NUMBER_SIZE); 
           nFull_N <= i2slv(countOnes(fullMask_N), SMALL_NUMBER_SIZE); 

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

            for i in 0 to MQ_SIZE-1 loop
                queueContent(i).TMP_cnt <= addInt(queueContent(i).TMP_cnt, 1);
                    queueContent_N(i).TMP_cnt <= addIntTrunc(queueContent_N(i).TMP_cnt, 1, 2);
                
                if queueContent(i).sqMiss = '1' and queueContent(i).sqTag(2 downto 0) = storeValueResult.dest(2 downto 0) then
                    queueContent(i).ready <= '1';
                end if;
            end loop;

            for i in 0 to MQ_SIZE-1 loop                    
                queueContent(i).full <= (fullMask(i) and not killMask(i) and not outputFullMask3(i)) or inputFullMask(i);
                        queueContent_N(i).full <= (fullMask_N(i) and not killMask_N(i) and not outputFullMask3(i)) or inputFullMask_N(i);

                if (canSend and selectMask(i)) = '1' then
                    queueContent(i).active <= '0';
                end if; 
            end loop;

            if TMP_prevSending = '1' then
                queueContent(p2i(writePtr, MQ_SIZE)).full <= '1';
                queueContent(p2i(writePtr, MQ_SIZE)).ready <= '0';
                queueContent(p2i(writePtr, MQ_SIZE)).active <= '1';                
                queueContent(p2i(writePtr, MQ_SIZE)).tag <= compareAddressInput.tag;

                queueContent(p2i(writePtr, MQ_SIZE)).dest <= compareAddressInput.dest;
                queueContent(p2i(writePtr, MQ_SIZE)).adr <= compareAddressCtrl.ip;
                queueContent(p2i(writePtr, MQ_SIZE)).sqTag <= compareAddressCtrl.target(SMALL_NUMBER_SIZE-1 downto 0);
                
                queueContent(p2i(writePtr, MQ_SIZE)).lqPointer <= compareAddressCtrl.tags.lqPointer;
                queueContent(p2i(writePtr, MQ_SIZE)).sqPointer <= compareAddressCtrl.tags.sqPointer;
                
                queueContent(p2i(writePtr, MQ_SIZE)).op <= compareAddressCtrl.op;

                queueContent(p2i(writePtr, MQ_SIZE)).fp <= compareAddressCtrl.classInfo.useFP;
                queueContent(p2i(writePtr, MQ_SIZE)).tlbMiss <= compareAddressCtrl.controlInfo.tlbMiss;
                queueContent(p2i(writePtr, MQ_SIZE)).dataMiss <= compareAddressCtrl.controlInfo.dataMiss;
                queueContent(p2i(writePtr, MQ_SIZE)).sqMiss <= compareAddressCtrl.controlInfo.sqMiss;
                
                queueContent(p2i(writePtr, MQ_SIZE)).TMP_cnt <= (others => '0');

                addresses(p2i(writePtr, MQ_SIZE)) <= compareAddressCtrl.ip;
                tags(p2i(writePtr, MQ_SIZE)) <= tagInWord;
                renameTags(p2i(writePtr, MQ_SIZE))(TAG_SIZE-1 downto 0) <= compareAddressCtrl.tag;

            end if;

                    if prevSendingEarly = '1' then
                        queueContent_N(p2i(writePtr_N, MQ_SIZE)).full <= '1';
                        queueContent_N(p2i(writePtr_N, MQ_SIZE)).ready <= '0';
                        queueContent_N(p2i(writePtr_N, MQ_SIZE)).active <= '1';              
                        queueContent_N(p2i(writePtr_N, MQ_SIZE)).tag <= compareAddressEarlyInput.tag;
        
                        
                        queueContent_N(p2i(writePtr_N, MQ_SIZE)).TMP_cnt <= (others => '0');
                    end if;

                    for i in 0 to MQ_SIZE-1 loop
                        if queueContent_N(i).full = '1' and queueContent_N(i).active /= '1' and queueContent_N(i).TMP_cnt = X"01" then
                            -- if TMP_prevSending => confirm full, fill other info from compareAddressInput, compareAddressCtrl
                            -- if not TMP_prevSending => clear
                            if TMP_prevSending = '1' then
                                queueContent_N(i).active <= '1';
                                queueContent_N(i).adr <= compareAddressCtrl.ip;
                                queueContent_N(i).sqTag <= compareAddressCtrl.target(SMALL_NUMBER_SIZE-1 downto 0);

                                queueContent_N(i).dest <= compareAddressInput.dest;

                                queueContent_N(i).lqPointer <= compareAddressCtrl.tags.lqPointer;
                                queueContent_N(i).sqPointer <= compareAddressCtrl.tags.sqPointer;
                                
                                queueContent_N(i).op <= compareAddressCtrl.op;
                
                                queueContent_N(i).fp <= compareAddressCtrl.classInfo.useFP;
                                queueContent_N(i).tlbMiss <= compareAddressCtrl.controlInfo.tlbMiss;
                                queueContent_N(i).dataMiss <= compareAddressCtrl.controlInfo.dataMiss;
                                queueContent_N(i).sqMiss <= compareAddressCtrl.controlInfo.sqMiss;
                            else
                                queueContent_N(i).full <= '0';    
                                queueContent_N(i).active <= '0';    
                            end if;
                        end if;
                    end loop;


            if sending3 = '1' then
                queueContent(p2i(selPtr3, MQ_SIZE)) <= DEFAULT_MQ_ENTRY;
            end if;


            --nFull <= nFullNext;
            --    nFull_N <= nFullNext_N;

            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
            recoveryCounter(7 downto 2) <= (others => '0');        
        end if;
        
        isFull <= cmpGtU(nFullNext, MQ_SIZE-2);
        isAlmostFull <= cmpGtU(nFullNext, MQ_SIZE-5);
    end process;
    
        accepting <= not isFull;
    
    tagInWord <= compareAddressCtrl.tags.lqPointer & compareAddressCtrl.tags.sqPointer & compareAddressInput.dest & X"00";
    
    outEntrySig <= queueContent(p2i(selPtr2, MQ_SIZE));
    adrOutWord <= addresses(p2i(selPtr2, MQ_SIZE));
    tagOutWord <= tags(p2i(selPtr2, MQ_SIZE));
    renameTagOutWord <= renameTags(p2i(selPtr2, MQ_SIZE));

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

    committedSending <= sending1; -- Indication to block normal mem issue
    
        almostFull <= isAlmostFull;
        acceptingOut <= accepting;
end MissQueue;

