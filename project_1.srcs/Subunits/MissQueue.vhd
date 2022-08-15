
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
    
    signal TMP_prevSending, canSend, sending, sending1, sending2, sending3: std_logic := '0';

    signal outEntrySig: MQ_Entry := DEFAULT_MQ_ENTRY;

    signal queueContent: MQ_EntryArray(0 to MQ_SIZE-1) := (others => DEFAULT_MQ_ENTRY);
    
    signal addresses, tags, renameTags: MwordArray(0 to MQ_SIZE-1) := (others => (others => '0'));
    
    signal fullMask, fullMaskNew, killMask, selectMask, selectMask1, selectMask2, selectMask3, inputFullMask,
            outputFullMask, outputFullMask1, outputFullMask2, outputFullMask3, outputFullMask3_N, readyMask: std_logic_vector(0 to MQ_SIZE-1) := (others => '0'); 
    
    signal firstOnePosI_O: integer := -1;
    signal queueIndexNew_O, queueIndexNew_K, firstOnePos_O, firstOnePos1_O, firstOnePos2_O, firstOnePos3_O: natural := 0;

    signal nFull: SmallNumber := (others => '0');
    signal writePtr, selPtr0, selPtr1, selPtr2, selPtr3: SmallNumber := (others => '0');
    signal           selValid0, selValid1, selValid2, selValid3: std_logic := '0';

    signal adrInWord, adrOutWord, tagInWord, tagOutWord, renameTagOutWord: Mword := (others => '0');
    
    function TMP_getNextIndex(index: natural; size: natural) return natural is
        variable res: natural := index;
    begin
    
        if res = size - 1 then
            res := 0;
        else
            res := res + 1;
        end if;
        
        return res;
    end function;
    
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

    signal ch0, ch1, ch2, ch3: std_logic := '0'; 
begin
    
        ch0 <= bool2std(outputFullMask3_N = outputFullMask3);
    
        queueIndexNew_K <= p2i(writePtr, MQ_SIZE);-- queueIndexNew_O;

    canSend <= '1';

    TMP_prevSending <= compareAddressInput.full;

    queueIndexNew_O <= TMP_getNewIndex(fullMask);
        writePtr <= i2slv(queueIndexNew_O, SMALL_NUMBER_SIZE);
        
        selValid0 <= sending;

    killMask <= getKillMask(queueContent, execEventSignal, lateEventSignal, execCausing.tag);

    inputFullMask <= getInputMask(queueContent, queueIndexNew_O, TMP_prevSending);

    -- completion and subsequent removal from queue is triggered by:
    --      L1 fill in case of Cache miss ops
    --      SQ value fill in case of missed SQ forwarding 


    selectMask <= getSelectMask(queueContent);

    outputFullMask <= selectMask when canSend = '1' else (others => '0'); 

        outputFullMask3_N <= maskFromIndex(selPtr3, MQ_SIZE) when selValid3 = '1' else (others => '0');

    firstOnePosI_O <= getFirstOnePosition(selectMask);
    
    firstOnePos_O <= 0 when firstOnePosI_O = -1 else firstOnePosI_O;

        selPtr0 <= i2slv(firstOnePos_O, SMALL_NUMBER_SIZE);

    READY_MASL: for i in 0 to MQ_SIZE-1 generate
        fullMask(i) <= queueContent(i).full;
        readyMask(i) <= queueContent(i).ready;
    end generate;

    process (clk)

    begin
        if rising_edge(clk) then			

            firstOnePos1_O <= firstOnePos_O;
            firstOnePos2_O <= firstOnePos1_O;
            firstOnePos3_O <= firstOnePos2_O;
            
                selPtr1 <= selPtr0;
                selPtr2 <= selPtr1;
                selPtr3 <= selPtr2;
            
                selValid1 <= selValid0;
                selValid2 <= selValid1;
                selValid3 <= selValid2;
            
            sending1 <= sending;
            sending2 <= sending1;
            sending3 <= sending2;

            selectMask1 <= selectMask;
            selectMask2 <= selectMask1;
            selectMask3 <= selectMask2;

            outputFullMask1 <= outputFullMask;
            outputFullMask2 <= outputFullMask1;
            outputFullMask3 <= outputFullMask2;
                            
            for i in 0 to MQ_SIZE-1 loop
                queueContent(i).TMP_cnt <= addInt(queueContent(i).TMP_cnt, 1);
                -- dummy setting to ready after some varying time
                if slv2u(queueContent(i).TMP_cnt) >= 20 + slv2u(queueContent(i).tag(7 downto 3)) then
                    queueContent(i).ready <= '1';
                end if; 
            end loop;

            for i in 0 to MQ_SIZE-1 loop                    
                queueContent(i).full <= (fullMask(i) and not killMask(i) and not outputFullMask3(i)) or inputFullMask(i);
                if (canSend and selectMask(i)) = '1' then
                    queueContent(i).active <= '0';
                end if; 
            end loop;

            if TMP_prevSending = '1' then
                queueContent(queueIndexNew_K).full <= '1';
                queueContent(queueIndexNew_K).ready <= '0';
                queueContent(queueIndexNew_K).active <= '1';                
                queueContent(queueIndexNew_K).tag <= compareAddressInput.tag;

                queueContent(queueIndexNew_K).dest <= compareAddressInput.dest;
                queueContent(queueIndexNew_K).adr <= compareAddressCtrl.ip;
                queueContent(queueIndexNew_K).sqTag <= (others => 'U');
                
                queueContent(queueIndexNew_K).lqPointer <= compareAddressCtrl.tags.lqPointer;
                queueContent(queueIndexNew_K).sqPointer <= compareAddressCtrl.tags.sqPointer;
                
                queueContent(queueIndexNew_K).op <= compareAddressCtrl.op;

                queueContent(queueIndexNew_K).fp <= compareAddressCtrl.classInfo.useFP;
                queueContent(queueIndexNew_K).tlbMiss <= compareAddressCtrl.controlInfo.tlbMiss;
                queueContent(queueIndexNew_K).dataMiss <= compareAddressCtrl.controlInfo.dataMiss;
                queueContent(queueIndexNew_K).sqMiss <= compareAddressCtrl.controlInfo.sqMiss;
                
                queueContent(queueIndexNew_K).TMP_cnt <= (others => '0');

                addresses(queueIndexNew_K) <= compareAddressCtrl.ip;
                tags(queueIndexNew_K) <= tagInWord;
                renameTags(queueIndexNew_K)(TAG_SIZE-1 downto 0) <= compareAddressCtrl.tag;
            end if;

            if sending3 = '1' then
                queueContent(firstOnePos3_O) <= DEFAULT_MQ_ENTRY;
            end if;
            
        end if;
    end process;    
    
    tagInWord <= compareAddressCtrl.tags.lqPointer & compareAddressCtrl.tags.sqPointer & compareAddressInput.dest & X"00";
    
    sending <= canSend and isNonzero(selectMask);
    
    outEntrySig <= queueContent(firstOnePos2_O);
    adrOutWord <= addresses(firstOnePos2_O);
    tagOutWord <= tags(firstOnePos2_O);
    renameTagOutWord <= renameTags(firstOnePos2_O);

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
end MissQueue;

