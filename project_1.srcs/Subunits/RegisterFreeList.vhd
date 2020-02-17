
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
    signal freeListTakeSel, freeListPutSel, stableUpdateSelDelayed: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    -- Don't remove, it is used by newPhysDestPointer!
    signal freeListTakeNumTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal freeListTakeAllow, freeListPutAllow, freeListRewind: std_logic := '0';
    signal freeListWriteTag: SmallNumber := (others => '0');
    
    signal physCommitFreedDelayed, physCommitDestsDelayed, newPhysDestsSync, newPhysDestsAsync: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal recoveryCounter: SmallNumber := (others => '0');
            
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
        if IS_FP then
            return 1;
        else
            return 0;
        end if;
    end function;
    
    function initList32 return WordArray is
        variable res: WordArray(0 to FREE_LIST_SIZE/4 - 1) := (others => (others=> '0'));
    begin
        for i in 0 to (N_PHYS - 32)/4 - 1 loop
            res(i)(7 downto 0) := i2slv(32 + 4*i + 0 + FP_1, PhysName'length);
            res(i)(15 downto 8) := i2slv(32 + 4*i + 1 + FP_1, PhysName'length);
            res(i)(23 downto 16) := i2slv(32 + 4*i + 2 + FP_1, PhysName'length);
            res(i)(31 downto 24) := i2slv(32 + 4*i + 3 + FP_1, PhysName'length);
        end loop;
        
        -- For FP, there's no register 0, mapper starts with 1:32 rather than 0:31, so one less is in this list
        if IS_FP then
            res((N_PHYS - 32)/4 - 1)(31 downto 24) := (others => '0');
        end if;
        return res;
    end function;

    function compactFreedRegs(names: PhysNameArray; mask: std_logic_vector) return PhysNameArray is
        variable res: PhysNameArray(0 to PIPE_WIDTH-1) := names;
        variable j: integer := 0;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            if mask(i) = '1' then
                res(j) := names(i);
                j := j + 1;
            end if;
        end loop;      
        return res;    
    end function;
	
	function selAndCompactPhysDests(physStableDelayed, physCommitDestsDelayed: PhysNameArray; stableUpdateSelDelayed, freeListPutSel: std_logic_vector)
	return PhysNameArray is
	   variable selected: PhysNameArray(0 to PIPE_WIDTH-1) := physStableDelayed;
	   variable res: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));	   
	begin
	   for i in 0 to PIPE_WIDTH-1 loop
	       if stableUpdateSelDelayed(i) = '0' then
	           selected(i) := physCommitDestsDelayed(i);
	       end if;
	   end loop;	   
	   res := compactFreedRegs(selected, freeListPutSel);
	   return res;
	end function;
		
begin
    physCommitFreedDelayed <= selAndCompactPhysDests(physStableDelayed, physCommitDestsDelayed, stableUpdateSelDelayed, freeListPutSel);

    physCommitDestsDelayed <= getPhysicalDests(stageDataToRelease);
    
    -- CAREFUL: excluding overridden dests here means that we don't bypass phys names when getting
    --				physStableDelayed! >> Related code in top module
    stableUpdateSelDelayed <=  freeListPutSel -- NOTE: putting *previous stable* register if: full, has dest, not exception.
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
    freeListTakeAllow <= takeAllow;
    
    freeListTakeSel <= whichTakeReg(stageDataToReserve, IS_FP); -- CAREFUL: must agree with Sequencer signals
    freeListPutAllow <= sendingToRelease;
    -- Releasing a register every time (but not always prev stable!)
    freeListPutSel <= whichPutReg(stageDataToRelease, IS_FP);-- CAREFUL: this chooses which ops put anyth. at all
    freeListRewind <= rewind;
    
    freeListWriteTag <= causingPointer;
    
    newPhysDests <= newPhysDestsAsync;
    newPhysDestPointer <= freeListTakeNumTags(0);
    
    IMPL: block
        signal listContent32: WordArray(0 to FREE_LIST_SIZE/4 - 1) := initList32;
        
        signal listPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
        signal listPtrPut: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
        
        signal listFront, listBack: PhysNameArray(0 to 7) := (others => (others => '0'));
        signal memData: Word := (others => '0');            
        signal physPtrTake, effectivePhysPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
        signal physPtrPut: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
    
        signal numFront, numBack, numToTake: SmallNumber := (others => '0');
        signal memRead, needTake: std_logic := '0';
        
        signal listFrontNext: PhysNameArray(0 to 7) := (others => (others => '0'));
        signal memTemp: PhysNameArray(0 to 3) := (others => (others => '0'));
        signal doTake: std_logic := '0';

        signal listFrontNextShift, listFrontNextMemNoTake, listFrontNextMemTake: PhysNameArray(0 to 7) := (others => (others => '0'));
        signal overNF, overNFmNT: std_logic_vector(0 to 7) := (others => '0');

        function nextShift(list: PhysNameArray; memData: PhysNameArray; allow: std_logic; nF, nT: SmallNumber) return PhysNameArray is
            variable res: PhysNameArray(0 to 7) := (others => (others => '0'));
            variable ind: SmallNumber := (others => '0');
            variable indM: SmallNumber := (others => '0');--std_logic_vector(1 downto 0) := (others => '0');           
            variable listExt: PhysNameArray(0 to 11) := list & PhysNameArray'(X"00", X"00", X"00", X"00");
        begin
            for i in 0 to 6 loop
                ind := addIntTrunc(nT, -1, 2);
                indM := addInt(ind, i + 1);
                res(i) := listExt(--i + (slv2u(nT)-1 mod 4) + 1);
                                    slv2u(indM(3 downto 0))); -- 4 bits for 12-element list
            end loop;
            
            return res;
        end function;

        function nextMemNoTake(list: PhysNameArray; memData: PhysNameArray; allow: std_logic; nF, nT: SmallNumber) return PhysNameArray is
            variable res: PhysNameArray(0 to 7) := (others => (others => '0'));
            variable ind: SmallNumber := (others => '0');
            variable notNF: SmallNumber := not nF; -- -nF == not nF + 1; not nF == -nF - 1                    
        begin
            for i in 0 to 7 loop
                ind := addIntTrunc(notNF, 1 + i, 2);
                res(i) := memData(--(i - slv2u(nF)) mod 4);
                                    slv2u(ind(1 downto 0)));
            end loop;
            
            return res;
        end function;
        
        function nextMemTake(list: PhysNameArray; memData: PhysNameArray; allow: std_logic; nF, nT: SmallNumber) return PhysNameArray is
            variable res: PhysNameArray(0 to 7) := (others => (others => '0'));
            variable ind: SmallNumber := (others => '0');
        begin
            for i in 0 to 7 loop
                ind := addIntTrunc(subSN(nT, nF), i, 2);
                res(i) := memData(--(i - slv2u(nF) + slv2u(nT)) mod 4);
                                    slv2u(ind(1 downto 0)));
            end loop;
            
            return res;
        end function;            
        
        function getOverNF(nF, nT: SmallNumber) return std_logic_vector is
            variable res: std_logic_vector(0 to 7) := (others => '0');
        begin
            for i in 0 to 7 loop
                res(i) := cmpLeS(nF, i);
            end loop;
            
            return res;
        end function;
    
        function getOverNFmNT(nF, nT: SmallNumber) return std_logic_vector is
            variable res: std_logic_vector(0 to 7) := (others => '0');
        begin
            for i in 0 to 7 loop
                res(i) := cmpLeS(subSN(nF, nT), i);
            end loop;
            
            return res;
        end function;
                    
    begin
        doTake <= freeListTakeAllow and isNonzero(freeListTakeSel);
        GEN_MEM_TEMP: for i in 0 to 3 generate
            memTemp(i) <= memData(8*i + 7 downto 8*i);
        end generate;
        
        GEN_VEC: for i in 0 to 7 generate
            overNF <= getOverNF(numFront, numToTake);
            overNFmNT <= getOverNFmNT(numFront, numToTake);
                
            listFrontNextShift <=       nextShift      (listFront, memTemp, '1', numFront, numToTake);
            listFrontNextMemNoTake <=   nextMemNoTake  (listFront, memTemp, '1', numFront, numToTake);
            listFrontNextMemTake <=     nextMemTake    (listFront, memTemp, '1', numFront, numToTake);
                        
            listFrontNext(i) <= listFrontNextMemTake(i)     when (    doTake and     overNFmNT(i)) = '1'
                           else listFrontNextShift(i)       when (    doTake and not overNFmNT(i)) = '1'
                           else listFrontNextMemNoTake(i)   when (not doTake and     overNF(i)) = '1'
                           else listFront(i);                          
        end generate;

        freeListTakeNumTags(0) <= i2slv((slv2u(listPtrTake)) mod FREE_LIST_SIZE, SMALL_NUMBER_SIZE);
        newPhysDestsAsync(0 to PIPE_WIDTH-1) <= listFront(0 to PIPE_WIDTH-1);
      
        effectivePhysPtrTake <= addInt(physPtrTake, 4) when (needTake and memRead) = '1' else physPtrTake;
        needTake <= cmpLeS(numFront, addInt(numToTake, 4));
        numToTake <= i2slv(countOnes(freeListTakeSel), SMALL_NUMBER_SIZE) when freeListTakeAllow = '1' else (others => '0');

        SYNCHRONOUS: process(clk)
            variable indPut, indTake: SmallNumber := (others => '0');
            variable nTaken, nPut, numFrontVar, numBackVar: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
            variable physPtrTakeVar, physPtrPutVar, tmpTag2: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
            variable listBackExt: PhysNameArray(0 to 11) := (others => (others => '0'));
        begin
            if rising_edge(clk) then
                listFront <= listFrontNext;
            
                indPut := listPtrPut;                                
                nTaken := i2slv(countOnes(freeListTakeSel), SMALL_NUMBER_SIZE);
                nPut := i2slv(countOnes(freeListPutSel), SMALL_NUMBER_SIZE);
                numFrontVar := numFront;
                
                if freeListRewind = '1' then
                    listPtrTake <= freeListWriteTag; -- Indexing TMP                            
                    physPtrTake(SMALL_NUMBER_SIZE-1 downto 2) <= freeListWriteTag(SMALL_NUMBER_SIZE-1 downto 2);                         
                    tmpTag2(1 downto 0) := freeListWriteTag(1 downto 0);
                    numFrontVar := subSN(i2slv(0, SMALL_NUMBER_SIZE), tmpTag2); -- TODO: find simpler notation for uminus
                    memRead <= '0';
                else
                    physPtrTake <= effectivePhysPtrTake;
                end if;
                
                if freeListTakeAllow = '1' and freeListRewind = '0' then
                    numFrontVar := subSN(numFrontVar, nTaken);
                    listPtrTake <= addSN(listPtrTake, nTaken);                            
                end if;
                
                if freeListRewind = '0' then
                     if cmpLeS(numFrontVar, 4) = '1' then
                        if memRead = '1' then                            
                            numFrontVar := addInt(numFrontVar, 4); 
                        end if;
                     end if;               
                     memRead <= '1';
                end if;
                
                memData <= listContent32(slv2u(effectivePhysPtrTake)/4);                        
                numFront <= numFrontVar;
                
                listBackExt(0 to 7) := listBack;
                numBackVar := numBack;
    
                if cmpGeS(numBack, 4) = '1' then
                    listContent32((slv2u(physPtrPut)/4)) <= listBackExt(3) & listBackExt(2) & listBackExt(1) & listBackExt(0); 
    
                    listBackExt(0 to 7) := listBackExt(4 to 11);
                    numBackVar := addInt(numBackVar, -4);               
                    physPtrPut <= addInt(physPtrPut, 4);         
                end if;                        
                
                if freeListPutAllow = '1' then
                    for i in 0 to PIPE_WIDTH-1 loop
                        -- for each element of input vec
                        if freeListPutSel(i) = '1' then
                            indPut := addInt(indPut, 1);
                            assert isNonzero(physCommitFreedDelayed(i)) = '1' report "Putting 0 to free list!" severity failure;
                        end if;    
                    end loop;
                    listPtrPut <= indPut;

                    for i in 0 to PIPE_WIDTH-1 loop
                        listBackExt(slv2u(numBackVar) + i) := physCommitFreedDelayed(i);
                    end loop;
                    
                    numBackVar := addSN(numBackVar, nPut);                 
                end if;                        
                
                listBack <= listBackExt(0 to 7);
                numBack <= numBackVar;
                                    
                -- CHECK: 3 cycles to restore?
                if freeListRewind = '1' then
                    recoveryCounter <= i2slv(3, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addInt(recoveryCounter, -1);
                end if;
            end if;
        end process;            
        
        assert physPtrTake /= physPtrPut report "Error: free list can overflow!" severity failure;
        
    end block;

end Behavioral;