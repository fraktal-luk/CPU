
architecture MissQueue of StoreQueue is

    constant MQ_SIZE: natural := 8;


    type MQ_Entry is record
        full: std_logic;
        ready: std_logic;
        tag: InsTag;
        
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
        tag => (others => '0'),
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
    signal fullMask, fullMaskNew, killMask, selectMask, selectMask1, selectMask2, selectMask3, inputFullMask,
            outputFullMask, outputFullMask1, outputFullMask2, outputFullMask3, readyMask: std_logic_vector(0 to MQ_SIZE-1) := (others => '0'); 
    
        signal firstOnePosI: integer := -1;
        signal queueIndex, queueIndexNext, queueIndexNew, firstOnePos, firstOnePos1, firstOnePos2, firstOnePos3: natural := 0;
    
    
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
            if content(i).full = '1' and content(i).ready = '1' then
                res(i) := '1';
            end if;
        end loop;
        
        res := getFirstOne(res);
        return res;
    end function;

begin
    
    canSend <= '1';

        TMP_prevSending <= compareAddressInput.full;

        queueIndexNew <= TMP_getNewIndex(fullMask);

            killMask <= getKillMask(queueContent, execEventSignal, lateEventSignal, execCausing.tag);

            inputFullMask <= getInputMask(queueContent, queueIndexNew, TMP_prevSending);

            -- completion and subsequent removal from queue is triggered by:
            --      L1 fill in case of Cache miss ops
            --      SQ value fill in case of missed SQ forwarding 


            selectMask <= getSelectMask(queueContent);

            outputFullMask <= selectMask when canSend = '1' else (others => '0'); 

            firstOnePosI <= getFirstOnePosition(selectMask);
            
            firstOnePos <= 0 when firstOnePosI = -1 else firstOnePos;

        READY_MASL: for i in 0 to MQ_SIZE-1 generate
            fullMask(i) <= queueContent(i).full;
            readyMask(i) <= queueContent(i).ready;
        end generate;

    process (clk)
        
    begin
        if rising_edge(clk) then			

                firstOnePos1 <= firstOnePos;
                firstOnePos2 <= firstOnePos1;
                firstOnePos3 <= firstOnePos2;
                
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

              --  selMask <= getFirstOne(readyMask);

                for i in 0 to MQ_SIZE-1 loop                    
                    queueContent(i).full <= (fullMask(i) and not killMask(i) and not outputFullMask3(i)) or inputFullMask(i);

                end loop;

            if TMP_prevSending = '1' then
                queueContent(queueIndexNew).full <= '1';
                queueContent(queueIndexNew).tag <= compareAddressInput.tag;
                queueContent(queueIndexNew).ready <= '0';
                
                queueContent(queueIndexNew).dest <= compareAddressInput.dest;
                queueContent(queueIndexNew).adr <= compareAddressCtrl.ip;
                queueContent(queueIndexNew).sqTag <= (others => 'U');
                
                queueContent(queueIndexNew).fp <= compareAddressCtrl.classInfo.useFP;
                queueContent(queueIndexNew).tlbMiss <= compareAddressCtrl.controlInfo.tlbMiss;
                queueContent(queueIndexNew).dataMiss <= compareAddressCtrl.controlInfo.dataMiss;
                queueContent(queueIndexNew).sqMiss <= compareAddressCtrl.controlInfo.sqMiss;
                
                queueContent(queueIndexNew).TMP_cnt <= (others => '0');

            end if;

                        
            if sending3 = '1' then
                queueContent(firstOnePos3) <= DEFAULT_MQ_ENTRY;
            end if;
            
        end if;
    end process;    
    
    sending <= canSend and isNonzero(selectMask);
    
    outEntrySig <= queueContent(firstOnePos3);

        selectedDataOutput.controlInfo.full <= sending3 and outEntrySig.full and not lateEventSignal;
        selectedDataOutput.tag <= outEntrySig.tag;
        --selectedDataOutput.value <= outEntrySig.target;
        selectedDataResult.dest <= outEntrySig.dest;
        
        selectedDataOutput.target <= outEntrySig.adr;
        selectedDataOutput.classInfo.useFP <= outEntrySig.fp;
        selectedDataOutput.controlInfo.tlbMiss <= outEntrySig.tlbMiss;
        selectedDataOutput.controlInfo.dataMiss <= outEntrySig.dataMiss;
        selectedDataOutput.controlInfo.sqMiss <= outEntrySig.sqMiss;
        

        committedSending <= sending1; -- Indication to block normal mem issue
end MissQueue;

