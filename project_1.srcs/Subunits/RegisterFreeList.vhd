
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;

use work.PipelineGeneral.all;
use work.LogicRenaming.all;


entity RegisterFreeList is
    generic(
        IS_FP: boolean := false
    );
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		rewind: in std_logic;
		causingPointer: in SmallNumber;
		
		sendingToReserve: in std_logic;
		takeAllow: in std_logic;
			auxTakeAllow: in std_logic;
		stageDataToReserve: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		
		newPhysDests: out PhysNameArray(0 to PIPE_WIDTH-1);
		newPhysDestPointer: out SmallNumber;

		sendingToRelease: in std_logic;
		stageDataToRelease: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		
		physStableDelayed: in PhysNameArray(0 to PIPE_WIDTH-1)
	);	
end RegisterFreeList;



architecture Behavioral of RegisterFreeList is
	constant WIDTH: natural := PIPE_WIDTH;

		signal freeListTakeAllow: std_logic := '0';
		signal freeListTakeSel: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
		-- Don't remove, it is used by newPhysDestPointer!
		signal freeListTakeNumTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
		signal freeListPutAllow: std_logic := '0';
		signal freeListPutSel: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
		signal freeListRewind: std_logic := '0';
		signal freeListWriteTag: SmallNumber := (others => '0');
		
			signal stableUpdateSelDelayed: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
			signal physCommitFreedDelayed, physCommitDestsDelayed: 
							PhysNameArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
		signal newPhysDestsSync: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
		signal newPhysDestsAsync, newPhysDestsAsync_T: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
		
		      signal ch0: std_logic := '0';
		      signal recoveryCounter: integer := 0;
		
function initList return PhysNameArray is
    variable res: PhysNameArray(0 to FREE_LIST_SIZE-1) := (others => (others=> '0'));
begin
    for i in 0 to N_PHYS - 32 - 1 loop
        res(i) := i2slv(32 + i, PhysName'length);
        if IS_FP then
            res(i) := i2slv(32 + i + 1, PhysName'length);
            if i = N_PHYS - 32 - 1 then
               res(i) := (others => '0'); -- CAREFUL: no reg 0 for FP, so 1 less on the list!
            end if;
        end if;
    end loop;
    return res;
end function;

function initList32 return WordArray is
    variable res: WordArray(0 to FREE_LIST_SIZE/4 - 1) := (others => (others=> '0'));
begin
    for i in 0 to (N_PHYS - 32)/4 - 1 loop
        res(i)(7 downto 0) := i2slv(32 + 4*i + 0, PhysName'length);
        res(i)(15 downto 8) := i2slv(32 + 4*i + 1, PhysName'length);
        res(i)(23 downto 16) := i2slv(32 + 4*i + 2, PhysName'length);
        res(i)(31 downto 24) := i2slv(32 + 4*i + 3, PhysName'length);       
        
        if IS_FP then
            res(i)(7 downto 0) := i2slv(32 + 4*i + 1, PhysName'length);
            res(i)(15 downto 8) := i2slv(32 + 4*i + 2, PhysName'length);
            res(i)(23 downto 16) := i2slv(32 + 4*i + 3, PhysName'length);
            res(i)(31 downto 24) := i2slv(32 + 4*i + 4, PhysName'length);            
            
            if i = N_PHYS - 32 - 1 then
               res(i) := (others => '0'); -- CAREFUL: no reg 0 for FP, so 1 less on the list!
            end if;
        end if;
    end loop;
    return res;
end function;


-- TEMP: to reduce num regs by 1 in case of FP
function FP_1 return integer is
begin
    if IS_FP then return 1;
    else return 0;
    end if;
end function;

						
begin
            ch0 <= bool2std(newPhysDestsAsync_T /= newPhysDestsAsync);


		FREED_DELAYED_SELECTION: for i in 0 to PIPE_WIDTH-1 generate
			physCommitFreedDelayed(i) <= physStableDelayed(i) when stableUpdateSelDelayed(i) = '1'
										else physCommitDestsDelayed(i);
		end generate;

		physCommitDestsDelayed <= getPhysicalDests(stageDataToRelease);
		
		-- CAREFUL: excluding overridden dests here means that we don't bypass phys names when getting
		--				physStableDelayed! >> Related code in top module
		stableUpdateSelDelayed <= -- NOTE: putting *previous stable* register if: full, has dest, not excpetion.
                                    freeListPutSel
					and not getExceptionMask(stageDataToRelease)
					and not findOverriddenDests(stageDataToRelease, IS_FP); -- CAREFUL: and must not be overridden!
										  -- NOTE: if those conditions are not satisfied, putting the allocated reg

		-- CAREFUL! Because there's a delay of 1 cycle to read FreeList, we need to do reading
		--				before actual instrucion goes to Rename, and pointer shows to new registers for next
		--				instruction, not those that are visible on output. So after every rewinding
		--				we must send a signal to read and advance the pointer.
		--				Rewinding has 2 specific moemnts: the event signal, and renameLockRelease,
		--				so on the former the rewinded pointer is written, and on the latter incremented and read.
		--				We also need to do that before the first instruction is executed (that's why resetSig here).
		freeListTakeAllow <= takeAllow; -- CMP: => ... or auxTakeAllow;
							-- or auxTakeAllow; -- CAREFUL: for additional step in rewinding for complex implems
		
		freeListTakeSel <= whichTakeReg(stageDataToReserve, IS_FP); -- CAREFUL: must agree with Sequencer signals
		freeListPutAllow <= sendingToRelease;
		-- Releasing a register every time (but not always prev stable!)
		freeListPutSel <= whichPutReg(stageDataToRelease, IS_FP);-- CAREFUL: this chooses which ops put anyth. at all
		freeListRewind <= rewind;
	
		freeListWriteTag <= causingPointer;--causingInstruction.tags.intPointer;

		
		newPhysDests <= newPhysDestsAsync; -- CMP: Async => Sync
		newPhysDestPointer <= freeListTakeNumTags(0); -- BL_OUT	

		IMPL_3: block
            --signal listContent: PhysNameArray(0 to FREE_LIST_SIZE-1) := initList;
            signal listContent32: WordArray(0 to FREE_LIST_SIZE/4 - 1) := initList32;
            
            signal listPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
            signal listPtrPut: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
            
            signal listFront, listBack: PhysNameArray(0 to 7) := (others => (others => '0'));
            signal memData: Word := (others => '0');            
            signal physPtrTake, effectivePhysPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
            signal physPtrPut: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
      
            signal numFront, numBack, numToTake: --integer range -7 to 8 := 0;
                --signal numFrontS, numBackS, numToTakeS: 
                                                SmallNumber := (others => '0');
            signal memRead, needTake: std_logic := '0';
        begin
            
            freeListTakeNumTags(0) <= i2slv((slv2u(listPtrTake)) mod FREE_LIST_SIZE, SMALL_NUMBER_SIZE);

            READ_DESTS: for i in 0 to WIDTH-1 generate
                newPhysDestsAsync(i) <= listFront(i);
            end generate;
            
            effectivePhysPtrTake <= i2slv(slv2u(physPtrTake) + 4, SMALL_NUMBER_SIZE) when (needTake and memRead) = '1'
                               else physPtrTake;
            needTake <= --bool2std(numFront - numToTake <= 4);
                        cmpLessSignedSN(numFront, addSN(numToTake, i2slv(4, SMALL_NUMBER_SIZE)));
            numToTake <= --countOnes(freeListTakeSel) when freeListTakeAllow = '1' else 0;
                        i2slv(countOnes(freeListTakeSel), SMALL_NUMBER_SIZE) when freeListTakeAllow = '1' else (others => '0');

            SYNCHRONOUS: process(clk)
                variable indPut, indTake: --integer := 0;
                                            SmallNumber := (others => '0');
                variable nTaken, nPut, numFrontVar, numBackVar: --integer := 0;
                                                                SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
                variable indPutV, indTakeV, physPtrTakeVar, physPtrPutVar, tmpTag2: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
                
                variable listFrontExt, listBackExt: PhysNameArray(0 to 11) := (others => (others => '0'));
                variable listFrontExtM4: PhysNameArray(0 to 15) := (others => (others => '0'));                
            begin
                if rising_edge(clk) then
                        indTake := --slv2u(listPtrTake);
                                    listPtrTake;
                        indPut := --slv2u(listPtrPut);                            
                                     listPtrPut;
                                        
                        nTaken := --countOnes(freeListTakeSel);
                                    i2slv(countOnes(freeListTakeSel), SMALL_NUMBER_SIZE);
                        nPut := --countOnes(freeListPutSel);
                                    i2slv(countOnes(freeListPutSel), SMALL_NUMBER_SIZE);
                     
                        numFrontVar := numFront;
                                       -- slv2s(numFrontS);
                        listFrontExt(0 to 7) := listFront;
                        
                        if freeListRewind = '1' then
                            listPtrTake <= freeListWriteTag; -- Indexing TMP                            
                            physPtrTake(SMALL_NUMBER_SIZE-1 downto 2) <= freeListWriteTag(SMALL_NUMBER_SIZE-1 downto 2);                         
                                    tmpTag2 := "000000" & freeListWriteTag(1 downto 0);
                            numFrontVar := ---slv2u(freeListWriteTag(1 downto 0));
                                            subSN(i2slv(0, SMALL_NUMBER_SIZE), tmpTag2);
                            memRead <= '0';
                        else
                            physPtrTake <= effectivePhysPtrTake;
                        end if;
                        
                        if freeListTakeAllow = '1' and freeListRewind = '0' then

                            indTake := --(indTake + nTaken) mod FREE_LIST_SIZE; -- CMP: nTaken => WIDTH
                                        addSN(indTake, nTaken); -- TODO: mask for list size!
                                        
                            indTakeV := --i2slv(indTake, listPtrTake'length);
                                        indTake;
                            
                            numFrontVar := --numFrontVar - nTaken;
                                            subSN(numFrontVar, nTaken);                         
                            
                            -- CAREFUL: expression is
                            --listFrontExt(0 to 7) := listFrontExt(0 + nTaken to 7 + nTaken);
                            
                                case nTaken is
                                    --when 1 =>
                                      when X"01" =>
                                        listFrontExt(0 to 7) := listFrontExt(0 + 1 to 7 + 1);
                                    --when 2 =>
                                      when X"02" =>
                                        listFrontExt(0 to 7) := listFrontExt(0 + 2 to 7 + 2);
                                    --when 3 =>
                                      when X"03" =>
                                        listFrontExt(0 to 7) := listFrontExt(0 + 3 to 7 + 3);
                                    --when 4 =>
                                      when X"04" =>
                                        listFrontExt(0 to 7) := listFrontExt(0 + 4 to 7 + 4);
                                    when others =>
                                         
                                end case;
                                                   
                            listPtrTake <= indTakeV;                            
                        end if;
                        
                        if freeListRewind = '0' then
                             if --numFrontVar <= 4 then
                                cmpGreaterSignedSN(numFrontVar, i2slv(4, SMALL_NUMBER_SIZE)) = '0' then
                                listFrontExtM4(4 to 15) := listFrontExt;                                                           
                                
                                case numFrontVar is
                                    --when -2 =>
                                        when i2slv(-2, SMALL_NUMBER_SIZE) =>
                                        listFrontExtM4(-2 + 4) := memData(7 downto 0);
                                        listFrontExtM4(-1 + 4) := memData(15 downto 8);
                                        listFrontExtM4(0 + 4) := memData(23 downto 16);
                                        listFrontExtM4(1 + 4) := memData(31 downto 24);                                    
                                    --when -1 =>
                                        when i2slv(-1, SMALL_NUMBER_SIZE) =>
                                        listFrontExtM4(-1 + 4) := memData(7 downto 0);
                                        listFrontExtM4(0 + 4) := memData(15 downto 8);
                                        listFrontExtM4(1 + 4) := memData(23 downto 16);
                                        listFrontExtM4(2 + 4) := memData(31 downto 24);                                    
                                    --when 0 =>
                                        when i2slv(0, SMALL_NUMBER_SIZE) =>
                                        listFrontExtM4(0 + 4) := memData(7 downto 0);
                                        listFrontExtM4(1 + 4) := memData(15 downto 8);
                                        listFrontExtM4(2 + 4) := memData(23 downto 16);
                                        listFrontExtM4(3 + 4) := memData(31 downto 24);                                    
                                    --when 1 =>
                                        when i2slv(1, SMALL_NUMBER_SIZE) =>
                                        listFrontExtM4(1 + 4) := memData(7 downto 0);
                                        listFrontExtM4(2 + 4) := memData(15 downto 8);
                                        listFrontExtM4(3 + 4) := memData(23 downto 16);
                                        listFrontExtM4(4 + 4) := memData(31 downto 24);                           
                                    --when 2 =>
                                        when i2slv(2, SMALL_NUMBER_SIZE) =>
                                        listFrontExtM4(2 + 4) := memData(7 downto 0);
                                        listFrontExtM4(3 + 4) := memData(15 downto 8);
                                        listFrontExtM4(4 + 4) := memData(23 downto 16);
                                        listFrontExtM4(5 + 4) := memData(31 downto 24);
                                    --when 3 =>
                                        when i2slv(3, SMALL_NUMBER_SIZE) =>
                                        listFrontExtM4(3 + 4) := memData(7 downto 0);
                                        listFrontExtM4(4 + 4) := memData(15 downto 8);
                                        listFrontExtM4(5 + 4) := memData(23 downto 16);
                                        listFrontExtM4(6 + 4) := memData(31 downto 24);                                   
                                    --when 4 =>
                                        when i2slv(4, SMALL_NUMBER_SIZE) =>
                                        listFrontExtM4(4 + 4) := memData(7 downto 0);
                                        listFrontExtM4(5 + 4) := memData(15 downto 8);
                                        listFrontExtM4(6 + 4) := memData(23 downto 16);
                                        listFrontExtM4(7 + 4) := memData(31 downto 24);
                                    when others => -- -3
                                        listFrontExtM4(-3 + 4) := memData(7 downto 0);
                                        listFrontExtM4(-2 + 4) := memData(15 downto 8);
                                        listFrontExtM4(-1 + 4) := memData(23 downto 16);
                                        listFrontExtM4(0 + 4) := memData(31 downto 24);
                                    end case;
                                                                    
                                listFrontExt := listFrontExtM4(4 to 15);
                                    
                                if memRead = '1' then                            
                                    numFrontVar := --numFrontVar + 4;
                                                    addSN(numFrontVar, i2slv(4, SMALL_NUMBER_SIZE));
                                end if;
                                --if numFrontVar <= 4 then -- if another word needed, get next address
                                --physPtrTake <= i2slv(slv2u(physPtrTake) + 4, SMALL_NUMBER_SIZE);
                                --end if;
                             end if;
                             
                             listFront <= listFrontExt(0 to 7);
                               
                             memRead <= '1';
                        end if;
                        
                        memData <= listContent32(slv2u(effectivePhysPtrTake)/4);                        
                        numFront <= numFrontVar;
                        --    numFrontS <= i2slv(numFrontVar, SMALL_NUMBER_SIZE);
                        
                        listBackExt(0 to 7) := listBack;
                        numBackVar := numBack;
                                      --  slv2s(numBackS);
                        if --numBackVar >= 4 then
                            cmpLessSignedSN(numBackVar, i2slv(4, SMALL_NUMBER_SIZE)) = '0' then
                            listContent32((slv2u(physPtrPut)/4)) <= listBackExt(3) & listBackExt(2) & listBackExt(1) & listBackExt(0); 

                            listBackExt(0 to 7) := listBackExt(4 to 11);
                            numBackVar := --numBackVar - 4;
                                            subSN(numBackVar, i2slv(4, SMALL_NUMBER_SIZE));
                            
                            physPtrPut <= i2slv(slv2u(physPtrPut) + 4, SMALL_NUMBER_SIZE);                          
                        end if;                        
                        
                        if freeListPutAllow = '1' then
                            for i in 0 to WIDTH-1 loop
                                -- for each element of input vec
                                if freeListPutSel(i) = '1' then
                                    --listContent(indPut) <= physCommitFreedDelayed(i);
                                    indPut := --(indPut + 1) mod FREE_LIST_SIZE;
                                                addSN(indPut, i2slv(1, SMALL_NUMBER_SIZE)); -- TODO: mask for list size!
                                    
                                        assert isNonzero(physCommitFreedDelayed(i)) = '1' report "Putting 0 to free list!";
                                end if;    
                            end loop;
                            listPtrPut <= --i2slv(indPut, listPtrPut'length);
                                            indPut;
                            
                            -- CAREFUL: expression is
                            --listBackExt(numBackVar to numBackVar + 3) := physCommitFreedDelayed;
--                            listBackExt(numBackVar + 0) := physCommitFreedDelayed(0);
--                            listBackExt(numBackVar + 1) := physCommitFreedDelayed(1);
--                            listBackExt(numBackVar + 2) := physCommitFreedDelayed(2);
--                            listBackExt(numBackVar + 3) := physCommitFreedDelayed(3);
                            
                                    listBackExt(slv2u(numBackVar) + 0) := physCommitFreedDelayed(0);
                                    listBackExt(slv2u(numBackVar) + 1) := physCommitFreedDelayed(1);
                                    listBackExt(slv2u(numBackVar) + 2) := physCommitFreedDelayed(2);
                                    listBackExt(slv2u(numBackVar) + 3) := physCommitFreedDelayed(3);
                            
                            numBackVar := --numBackVar + nPut;
                                          addSN(numBackVar, nPut);                 
                        end if;                        
                        
                        listBack <= listBackExt(0 to 7);
                        numBack <= numBackVar;
                        --    numBackS <= i2slv(numBackVar, SMALL_NUMBER_SIZE);
                        
                        
                        -- CHECK: 3 cycles to restore?
                        if freeListRewind = '1' then
                            recoveryCounter <= 3;
                        elsif recoveryCounter > 0 then
                            recoveryCounter <= recoveryCounter - 1;
                        end if;
                end if;
            end process;            
            
            assert physPtrTake /= physPtrPut report "Error: free list can overflow!" severity failure;
            
        end block;
        		
end Behavioral;