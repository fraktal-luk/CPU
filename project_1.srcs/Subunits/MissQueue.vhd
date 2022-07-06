
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
begin
    
--	livingMask <= fullMask and not killMask;
--    fullMaskNext <= (livingMask and not sendingMask) or inputMask;    

----    inputMask <= findFirstFree(fullMask) when dataIn(0).full = '1' else (others => '0');
    
--    process (clk)
--    begin
--        if rising_edge(clk) then			
--            content <= contentNext;
--            fullMask <= fullMaskNext;
--        end if;
--    end process;    
    
end MissQueue;

