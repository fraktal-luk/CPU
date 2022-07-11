
architecture MissQueue of StoreQueue is
--	signal content, contentNext, contentUpdated:
--                InstructionSlotArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INSTRUCTION_SLOT);    

--	signal fullMask, fullMaskNext, livingMask, killMask, sendingMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

--				function lmQueueNext(content: InstructionStateArray;
--									  livingMask: std_logic_vector;
--									  newContent: InstructionState;
--									  sendingVec: std_logic_vector; -- shows which one sending
--									  --receiving: std_logic;
--									  inputMask: std_logic_vector;
--									  fillData: InstructionSlot
--									  ) return InstructionStateArray is
--					constant LEN: integer := content'length;
--					variable res: InstructionStateArray(0 to LEN-1) := content;
--				begin
--					for i in 0 to LEN-1 loop
----						if (filling and newFilled(i) and livingMask(i)) = '1' then
----							res(i).controlInfo.completed2 := '1';
----						end if;
						
					
--						if --receiving = '1' and 
--						    inputMask(i) = '1' then
--							res(i) := newContent;
--							--res(i).argValues := DEFAULT_ARG_VALUES;	
--							res(i).result := (others => '0');
--							res(i).constantArgs := DEFAULT_CONSTANT_ARGS;
--							res(i).virtualArgSpec := DEFAULT_ARG_SPEC;
--							res(i).physicalArgSpec := DEFAULT_ARG_SPEC;
--						elsif sendingVec(i) = '1' then
--							null; 
--						end if;
--					end loop;
					
--					return res;
--				end function;
				
				
--                function findFirstFree(mask: std_logic_vector) return std_logic_vector is
--                    variable res: std_logic_vector(0 to mask'length-1) := (others => '0');
--                begin
--                    for i in 0 to mask'length-1 loop
--                        if mask(i) = '0' then
--                            res(i) := '1';
--                            exit;
--                        end if;
--                    end loop;
                    
--                    return res;
--                end function;

    constant MQ_SIZE: natural := 8;


    type MQ_Entry is record
        full: std_logic;
        ready: std_logic;
        tag: InsTag;
        
            TMP_cnt: SmallNumber;
    end record;
    
    constant DEFAULT_MQ_ENTRY: MQ_Entry := (full => '0', ready => '0', tag => (others => '0'), TMP_cnt => (others => '0'));
    
    type MQ_EntryArray is array(integer range <>) of MQ_Entry;
    
    signal TMP_prevSending, canSend: std_logic := '0';

    signal outEntrySig: MQ_Entry := DEFAULT_MQ_ENTRY;

    signal queueContent: MQ_EntryArray(0 to MQ_SIZE-1) := (others => DEFAULT_MQ_ENTRY);
    signal fullMask, fullMaskNew, killMask, selectMask, inputFullMask, outputFullMask, readyMask: std_logic_vector(0 to MQ_SIZE-1) := (others => '0'); 
    
        signal firstOnePosI: integer := -1;
        signal queueIndex, queueIndexNext, queueIndexNew, firstOnePos: natural := 0;
    
    
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
--	livingMask <= fullMask and not killMask;
--    fullMaskNext <= (livingMask and not sendingMask) or inputMask;

----    inputMask <= findFirstFree(fullMask) when dataIn(0).full = '1' else (others => '0');

        TMP_prevSending <= compareAddressInput.full;

        --queueIndexNext <= TMP_getNextIndex(queueIndex, MQ_SIZE) when TMP_prevSending = '1' else queueIndex;

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
            --content <= contentNext;
            --fullMask <= fullMaskNext;
                for i in 0 to MQ_SIZE-1 loop
                    queueContent(i).TMP_cnt <= addInt(queueContent(i).TMP_cnt, 1);
                    
                        -- dummy setting to ready after some varying time
                        if slv2u(queueContent(i).TMP_cnt) >= 20 + slv2u(queueContent(i).tag(7 downto 3)) then
                            queueContent(i).ready <= '1';
                        end if; 
                        
                end loop;

              --  selMask <= getFirstOne(readyMask);

                for i in 0 to MQ_SIZE-1 loop                    
                    queueContent(i).full <= (fullMask(i) and not killMask(i) and not outputFullMask(i)) or inputFullMask(i);

                end loop;

            if TMP_prevSending = '1' then
                queueContent(queueIndexNew).full <= '1';
                queueContent(queueIndexNew).tag <= compareAddressInput.tag;
                queueContent(queueIndexNew).ready <= '0';
                queueContent(queueIndexNew).TMP_cnt <= (others => '0');

            end if;

                        
            if canSend = '1' and isNonzero(selectMask) = '1' then
                queueContent(firstOnePos) <= DEFAULT_MQ_ENTRY;
            end if;
            
        end if;
    end process;    
    
    outEntrySig <= queueContent(firstOnePos);

end MissQueue;

