
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
		execEventSignal: in std_logic;
		lateEventSignal: in std_logic;
		causingPointer: in SmallNumber; -- CAREFUL: this is just for Exec events, late events are handled differently
		
		sendingToReserve: in std_logic;
		takeAllow: in std_logic;
		reserveInfoA: in RenameInfoArray(0 to PIPE_WIDTH-1);

		newPhysDests: out PhysNameArray(0 to PIPE_WIDTH-1);
		newPhysDestPointer: out SmallNumber;

		sendingToRelease: in std_logic;
		releaseInfoA: in RenameInfoArray(0 to PIPE_WIDTH-1);

		physStableDelayed: in PhysNameArray(0 to PIPE_WIDTH-1)
	);
end RegisterFreeList;


architecture Behavioral of RegisterFreeList is
	constant FP_1: natural := getFp1(IS_FP);

    signal freeListTakeSel, freeListPutSel, stableUpdateSelDelayed, stableTakeUpdate, vsels, psels, overridden: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');    
    signal freeListTakeAllow, freeListPutAllow, freeListRewind: std_logic := '0';
    signal newListPointer: SmallNumber := (others => '0');
    signal physCommitFreedDelayed, physCommitDestsDelayed, newPhysDestsSync, newPhysDestsAsync: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	  
    signal recoveryCounter: SmallNumber := (others => '0');
    signal ch0, ch1, ch2, ch3, ch4,ch5, ch6: std_logic := '0';  	
begin

    vsels <= getVirtualFloatDestSels(releaseInfoA) when IS_FP else getVirtualIntDestSels(releaseInfoA);
    psels <= getPsels(releaseInfoA);

    overridden <= findOverriddenDests(releaseInfoA, IS_FP);

    physCommitFreedDelayed <= selAndCompactPhysDests(physStableDelayed, physCommitDestsDelayed, stableUpdateSelDelayed, freeListPutSel);
    physCommitDestsDelayed <= getPhysicalDests(releaseInfoA);
    
    stableUpdateSelDelayed <= psels and not overridden;  -- CAREFUL: excluding overridden dests
    stableTakeUpdate <= vsels; -- Those which commit any register that won't ever be rewound
    
    -- CAREFUL! Because there's a delay of 1 cycle to read FreeList, we need to do reading
    --				before actual instruction goes to Rename, and pointer shows to new registers for next
    --				instruction, not those that are visible on output. So after every rewinding
    --				we must send a signal to read and advance the pointer.
    --				Rewinding has 2 specific moemnts: the event signal, and renameLockRelease,
    --				so on the former the rewinded pointer is written, and on the latter incremented and read.
    --				We also need to do that before the first instruction is executed (that's why resetSig here).
    freeListTakeAllow <= takeAllow;
    freeListPutAllow <= sendingToRelease;
    freeListRewind <= rewind;

    freeListTakeSel <= whichTakeReg(reserveInfoA, IS_FP); -- CAREFUL: must agree with Sequencer signals

    freeListPutSel <= vsels; -- Releasing a register every time any dest exists (but not always prev stable!)

    newPhysDests <= newPhysDestsAsync;
    newPhysDestPointer <= newListPointer;

    IMPL: block
        signal listContent: WordArray(0 to FREE_LIST_SIZE/4 - 1) := initFreeList32(IS_FP);

        signal listPtrTake, listPtrTakeStable, listPtrTakeStableNext, causingTag: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
        signal listPtrPut, listPtrPutNext: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);

        signal memOutput, memInput: Word := (others => '0');

        signal listFront, listBack, listFrontNext, listBackNext: PhysNameArray(0 to 7) := (others => (others => '0'));
        signal physPtrTake, physPtrTakeNext, effectivePhysPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
        signal physPtrPut, physPtrPutNext: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);

        signal numFront, numBack, numBackReduced, numFrontNext, numBackNext, numToTake: SmallNumber := (others => '0');
        signal memRead, needTake, canWriteBack: std_logic := '0';

        signal outputRegs: PhysNameArray(0 to 3) := (others => (others => '0'));
    begin
        causingTag <= listPtrTakeStable when lateEventSignal = '1' else causingPointer;

        numToTake <= i2slv(countOnes(freeListTakeSel), SMALL_NUMBER_SIZE) when freeListTakeAllow = '1' else (others => '0');
        needTake <= cmpLeS(numFront, addInt(numToTake, 4));

        canWriteBack <= cmpGeS(numBack, 4);                
        memInput <= listBack(3) & listBack(2) & listBack(1) & listBack(0);
        outputRegs <= splitWord(memOutput);

        listPtrTake <= sub(physPtrTake, numFront);

        effectivePhysPtrTake <= addInt(physPtrTake, 4) when (needTake and memRead) = '1' else physPtrTake;
        physPtrTakeNext <= causingTag(SMALL_NUMBER_SIZE-1 downto 2) & physPtrTake(1 downto 0) when freeListRewind = '1' else effectivePhysPtrTake;
        listPtrTakeStableNext <= addInt(listPtrTakeStable, countOnes(stableTakeUpdate)) when freeListPutAllow = '1' else listPtrTakeStable;
        listPtrPutNext <= addInt(listPtrPut, countOnes(freeListPutSel)) when freeListPutAllow = '1' else listPtrPut;
        physPtrPutNext <= addInt(physPtrPut, 4) when canWriteBack = '1' else physPtrPut;

        numFrontNext <= getNumFrontNext(numFront, causingTag, freeListRewind, freeListTakeAllow, memRead, freeListTakeSel);
                
        numBackNext <= addIntTrunc(numBackReduced, countOnes(freeListPutSel), 3) when freeListPutAllow = '1' else numBackReduced;
        numBackReduced <= numBack and "00000011";
            
        listFrontNext <= moveFrontList(listFront, numFront, numToTake, outputRegs);
        listBackNext <= moveBackList(listBack, canWriteBack, freeListPutAllow, numBackReduced, physCommitFreedDelayed);
  
        SYNCHRONOUS: process(clk)
        begin
            if rising_edge(clk) then
                physPtrTake <= physPtrTakeNext;
                listPtrTakeStable <= listPtrTakeStableNext;

                listPtrPut <= listPtrPutNext;
                physPtrPut <= physPtrPutNext;

                numFront <= numFrontNext;
                numBack <= numBackNext;
                
                listFront <= listFrontNext;
                listBack <= listBackNext;

                memRead <= not freeListRewind;
                memOutput <= listContent(slv2u(effectivePhysPtrTake(7 downto 2)));

                if canWriteBack = '1' then
                    listContent(slv2u(physPtrPut(7 downto 2))) <= memInput;
                end if;                        
    
                -- CHECK: 3 cycles to restore?
                if freeListRewind = '1' then
                    recoveryCounter <= i2slv(3, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addIntTrunc(recoveryCounter, -1, 2);
                end if;                
            end if;
        end process;
                    
        newListPointer <= listPtrTake;                                  
        newPhysDestsAsync(0 to PIPE_WIDTH-1) <= listFront(0 to PIPE_WIDTH-1);
              
-------------
-------------        
        VIEW: if VIEW_ON generate            
            signal vFree, vUsed: std_logic_vector(0 to N_PHYS-1) := (others => '0');
            signal newTaken, newPut: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
            
            function TMP_w2b(wa: WordArray) return ByteArray is
                constant LEN: natural := wa'length;
                variable res: ByteArray(0 to 4*LEN-1);
            begin
                for i in 0 to LEN-1 loop 
                    res(4*i to 4*i + 3) := (wa(i)(31 downto 24), wa(i)(23 downto 16), wa(i)(15 downto 8), wa(i)(7 downto 0));    
                end loop;
                return res;
            end function;
            
            function getFreeVec(listContent32: WordArray; physStart, physEnd: SmallNumber;
                                listFront: PhysNameArray; numFront: SmallNumber; listBack: PhysNameArray; numBack: SmallNumber)
            return std_logic_vector is
                variable res: std_logic_vector(0 to N_PHYS-1) := (others => '0');
                constant wa: WordArray(0 to 2*listContent32'length-1) := listContent32 & listContent32;
                constant tmpByteArray: ByteArray(0 to 2*FREE_LIST_SIZE-1) := TMP_w2b(wa); -- 2x for easy range extraction
                variable pStart, pEnd: natural;
            begin            
                pStart := slv2u(physStart);
                pEnd := slv2u(physEnd);
                
                if pStart > pEnd then
                    pEnd := pEnd + FREE_LIST_SIZE;                    
                end if;
                
                for i in pStart to pEnd-1 loop
                    res(slv2u(tmpByteArray(i))) := '1';
                end loop;
                
                for i in 0 to slv2s(numFront)-1 loop -- CAREFUL: numFront can be negative
                    res(slv2u(listFront(i))) := '1';
                end loop;
                
                for i in 0 to slv2s(numBack)-1 loop -- CAREFUL: numBack can be negative
                    res(slv2u(listBack(i))) := '1';                    
                end loop;                            
                return res;
            end function;
            
            signal numFree: natural := 0;
        begin
            
            process(clk)
            begin
                if rising_edge(clk) then      
                    -- Watch what is taken from the list, how it is rewinded, and what is put at the end
                    
                    -- There can be no 0 in the list!
                    for i in 0 to slv2s(numFront)-1 loop
                        assert std2bool(isNonzero(listFront(i))) report "Has 0 in free list!" severity failure;                    
                    end loop;
                    for i in 0 to slv2s(numBack)-1 loop
                        assert std2bool(isNonzero(listBack(i))) report "Put 0 to free list!" severity failure;
                    end loop;
                                        
                    vFree <= getFreeVec(listContent, physPtrTake, physPtrPut, listFront, numFront, listBack, numBack);
                    
                    assert physPtrTake /= physPtrPut report "Error: free list can overflow!" severity failure;
                    
                    -- Num of free regs may be overstated after rewind when physical pointer gets decremented more than the virtual one!
                    -- Beyond the time of recovery we can't tolerate less than 32 occupied regs
                    assert (numFree <= N_PHYS-32 or std2bool(isNonzero(recoveryCounter))) report "Impossible number of free regs" severity failure; 
                    -- NOTE: For FP no phys reg 0 so 1 less free but it cancels out on both sides ((TODO?) unless -1 added to numFree)
                end if;
            end process;
            
            numFree <= countOnes(vFree); -- CAREFUL (TODO?): for FP physical reg 0 is not used so number actually -1
        end generate;
        
    end block;

end Behavioral;
