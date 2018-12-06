
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;

use work.PipelineGeneral.all;



entity RegisterFreeList is
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
    end loop;
    return res;
end function;
						
begin

		FREED_DELAYED_SELECTION: for i in 0 to PIPE_WIDTH-1 generate
			physCommitFreedDelayed(i) <= physStableDelayed(i) when stableUpdateSelDelayed(i) = '1'
										else physCommitDestsDelayed(i);
		end generate;

		--physCommitDestsDelayed <= getPhysicalDests(stageDataToRelease);
		
		-- CAREFUL: excluding overridden dests here means that we don't bypass phys names when getting
		--				physStableDelayed! >> Related code in top module
--		stableUpdateSelDelayed <= -- NOTE: putting *previous stable* register if: full, has dest, not excpetion.
--							  getPhysicalDestMask(stageDataToRelease) 
--					and	  stageDataToRelease.fullMask  
--					and not getExceptionMask(stageDataToRelease)
--					and not findOverriddenDests(stageDataToRelease); -- CAREFUL: and must not be overridden!
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
		
		--freeListTakeSel <= findWhichTakeReg(stageDataToReserve); -- CAREFUL: must agree with Sequencer signals
		freeListPutAllow <= sendingToRelease;
		-- Releasing a register every time (but not always prev stable!)
		--freeListPutSel <= findWhichPutReg(stageDataToRelease);-- CAREFUL: this chooses which ops put anyth. at all
		freeListRewind <= rewind;
		
		
		freeListWriteTag <= causingPointer;--causingInstruction.tags.intPointer;
		
		IMPL: block
			signal listContent: PhysNameArray(0 to FREE_LIST_SIZE-1) := initList;
			signal listPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
			signal listPtrPut: SmallNumber := i2slv(N_PHYS - 32, SMALL_NUMBER_SIZE);
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
							checkFreeList(indTake, indPut, nTaken, nPut);
							logFreeList(indTake, indPut, nTaken, nPut,
											listContent, freeListTakeSel,
											physCommitFreedDelayed, freeListPutSel,
											freeListTakeAllow, freeListPutAllow,
											freeListRewind, freeListWriteTag);
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
end Behavioral;