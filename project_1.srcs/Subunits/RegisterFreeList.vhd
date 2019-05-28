
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
		signal newPhysDestsAsync: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
		
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

-- TEMP: to reduce num regs by 1 in case of FP
function FP_1 return integer is
begin
    if IS_FP then return 1;
    else return 0;
    end if;
end function;

						
begin

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
		
		IMPL: block
			signal listContent: PhysNameArray(0 to FREE_LIST_SIZE-1) := initList;
			signal listPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
			signal listPtrPut: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
		begin
			
			freeListTakeNumTags(0) <= i2slv((slv2u(listPtrTake)) mod FREE_LIST_SIZE, SMALL_NUMBER_SIZE);

			READ_DESTS: for i in 0 to WIDTH-1 generate
				newPhysDestsAsync(i) <= listContent((slv2u(listPtrTake) + i) mod FREE_LIST_SIZE);
			end generate;


			SYNCHRONOUS: process(clk)
				variable indPut, indTake: integer := 0;
				variable nTaken, nPut: integer := 0;
			begin
				if rising_edge(clk) then
						indTake := slv2u(listPtrTake); 
						indPut := slv2u(listPtrPut);							
										
						nTaken := countOnes(freeListTakeSel);
						nPut := countOnes(freeListPutSel);

						-- pragma synthesis off
							-- Check if list has enough free entries!
						--	checkFreeList(indTake, indPut, nTaken, nPut);
						--	logFreeList(indTake, indPut, nTaken, nPut,
						--					listContent, freeListTakeSel,
						--					physCommitFreedDelayed, freeListPutSel,
						--					freeListTakeAllow, freeListPutAllow,
						--					freeListRewind, freeListWriteTag);
						-- pragma synthesis on
							
						if freeListRewind = '1' then
							listPtrTake <= freeListWriteTag; -- Indexing TMP
						end if;
						
						if freeListTakeAllow = '1' and freeListRewind = '0' then
							for i in 0 to WIDTH-1 loop
								newPhysDestsSync(i) <= listContent((slv2u(listPtrTake) + i) mod FREE_LIST_SIZE);
							end loop;
							indTake := (indTake + nTaken) mod FREE_LIST_SIZE; -- CMP: nTaken => WIDTH
							listPtrTake <= i2slv(indTake, listPtrTake'length);
						end if;
						
						if freeListPutAllow = '1' then
							for i in 0 to WIDTH-1 loop
								-- for each element of input vec
								if freeListPutSel(i) = '1' then
									listContent(indPut) <= physCommitFreedDelayed(i);
									indPut := (indPut + 1) mod FREE_LIST_SIZE;
										assert isNonzero(physCommitFreedDelayed(i)) = '1' report "Putting 0 to free list!";
								end if;	
							end loop;
							listPtrPut <= i2slv(indPut, listPtrPut'length);	
						end if;						
						
				end if;
			end process;			
		
		end block;
		
		newPhysDests <= newPhysDestsAsync; -- CMP: Async => Sync
		newPhysDestPointer <= freeListTakeNumTags(0); -- BL_OUT	
		
		
		IMPL_2: block
            signal listContent: PhysNameArray(0 to FREE_LIST_SIZE-1) := initList;
            signal listPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
            signal listPtrPut: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
            
            signal listFront, listBack: PhysNameArray(0 to 7) := (others => (others => '0'));
            signal physPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
            signal physPtrPut: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
      
            signal numFront, numBack: integer := 0;
        begin
            
            --freeListTakeNumTags(0) <= i2slv((slv2u(listPtrTake)) mod FREE_LIST_SIZE, SMALL_NUMBER_SIZE);


            -- 
            --     0 1 2 3
            --  [ (x x x x) (x x x x) ]
            --
            --
            --

            READ_DESTS: for i in 0 to WIDTH-1 generate
                --newPhysDestsAsync(i) <= listContent((slv2u(listPtrTake) + i) mod FREE_LIST_SIZE);
            end generate;


            SYNCHRONOUS: process(clk)
                variable indPut, indTake: integer := 0;
                variable nTaken, nPut, numFrontVar, numBackVar: integer := 0;
                variable indPutV, indTakeV, physPtrTakeVar, physPtrPutVar: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
                
                variable listFrontExt, listBackExt: PhysNameArray(0 to 11) := (others => (others => '0'));
                variable listFrontExtM4: PhysNameArray(0 to 15) := (others => (others => '0'));                
            begin
                if rising_edge(clk) then
                        indTake := slv2u(listPtrTake); 
                        indPut := slv2u(listPtrPut);                            
                                        
                        nTaken := countOnes(freeListTakeSel);
                        nPut := countOnes(freeListPutSel);
                     
                        numFrontVar := numFront;
                        listFrontExt(0 to 7) := listFront;
                        
                        if freeListRewind = '1' then
                            listPtrTake <= freeListWriteTag; -- Indexing TMP                            
                            physPtrTake(SMALL_NUMBER_SIZE-1 downto 2) <= freeListWriteTag(SMALL_NUMBER_SIZE-1 downto 2);                         
                            numFrontVar := -slv2u(freeListWriteTag(1 downto 0));
                        end if;
                        
                        if freeListTakeAllow = '1' and freeListRewind = '0' then
                            for i in 0 to WIDTH-1 loop
                                --newPhysDestsSync(i) <= listContent((slv2u(listPtrTake) + i) mod FREE_LIST_SIZE);
                            end loop;
                            indTake := (indTake + nTaken) mod FREE_LIST_SIZE; -- CMP: nTaken => WIDTH
                            indTakeV := i2slv(indTake, listPtrTake'length);
                            
                            numFrontVar := numFrontVar - nTaken;                          
                            listFrontExt(0 to 7) := listFrontExt(0 + nTaken to 7 + nTaken);                          
                            listPtrTake <= indTakeV;                            
                        end if;
                        
                        if freeListRewind = '0' then
                             if numFrontVar <= 4 then
                                
                                listFrontExtM4(4 to 15) := listFrontExt;                                
                                listFrontExtM4(numFrontVar + 4 to numFrontVar + 3 + 4) := listContent(slv2u(physPtrTake) to slv2u(physPtrTake) + 3);
                                listFrontExt := listFrontExtM4(4 to 15);                                   
                                numFrontVar := numFrontVar + 4;
                                physPtrTake <= i2slv(slv2u(physPtrTake) + 4, SMALL_NUMBER_SIZE);
                             end if;
                             
                             listFront <= listFrontExt(0 to 7);
                        end if;
                        
                        numFront <= numFrontVar;
                        
                        
                        listBackExt(0 to 7) := listBack;
                        numBackVar := numBack;
                        if freeListPutAllow = '1' then
                            for i in 0 to WIDTH-1 loop
                                -- for each element of input vec
                                if freeListPutSel(i) = '1' then
                                    --listContent(indPut) <= physCommitFreedDelayed(i);
                                    indPut := (indPut + 1) mod FREE_LIST_SIZE;
                                        assert isNonzero(physCommitFreedDelayed(i)) = '1' report "Putting 0 to free list!";
                                end if;    
                            end loop;
                            listPtrPut <= i2slv(indPut, listPtrPut'length);
                            
                            listBackExt(numBackVar to numBackVar + 3) := physCommitFreedDelayed;
                            numBackVar := numBackVar + nPut;
                            if numBackVar >= 4 then
                                for j in 0 to 3 loop
                                    listContent((slv2u(physPtrPut) + j) mod FREE_LIST_SIZE) <= listBackExt(j);
                                end loop;
                            
                                listBackExt(0 to 7) := listBackExt(4 to 11);
                                numBackVar := numBackVar - 4;
                                
                                physPtrPut <= i2slv(slv2u(physPtrPut) + 4, SMALL_NUMBER_SIZE);                          
                            end if;                                                           
                        end if;                        
                        
                        listBack <= listBackExt(0 to 7);
                        numBack <= numBackVar;
                        
                end if;
            end process;            
        
        end block;		
		
end Behavioral;