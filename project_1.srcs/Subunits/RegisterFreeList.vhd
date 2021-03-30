
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
    signal freeListTakeSel, freeListPutSel, stableUpdateSelDelayed, stableTakeUpdate, overridden: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    -- Don't remove, it is used by newPhysDestPointer!
    signal freeListTakeNumTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal freeListTakeAllow, freeListPutAllow, freeListRewind: std_logic := '0';
    
    signal physCommitFreedDelayed, physCommitDestsDelayed, newPhysDestsSync, newPhysDestsAsync: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal recoveryCounter: SmallNumber := (others => '0');


	function getFp1(constant IS_FP: boolean) return natural is
	begin
	   if IS_FP then
	       return 1;
	   else
	       return 0;
	   end if;
	end function;
    
    function initFreeList32(constant IS_FP: boolean) return WordArray is
        variable fp1: natural := 0;
        variable res: WordArray(0 to FREE_LIST_SIZE/4 - 1) := (others => (others=> '0'));
    begin
        if IS_FP then
            fp1 := 1;
        end if;
        for i in 0 to (N_PHYS - 32)/4 - 1 loop
            res(i)(7 downto 0) := i2slv(32 + 4*i + 0 + fp1, PhysName'length);
            res(i)(15 downto 8) := i2slv(32 + 4*i + 1 + fp1, PhysName'length);
            res(i)(23 downto 16) := i2slv(32 + 4*i + 2 + fp1, PhysName'length);
            res(i)(31 downto 24) := i2slv(32 + 4*i + 3 + fp1, PhysName'length);
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
		
		
		
		signal ch0, ch1, ch2, ch3, ch4, ch5: std_logic := '0';

	
	constant FP_1: natural := getFp1(IS_FP);
	
    signal vsels, psels: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	
begin
    vsels <= getVirtualFloatDestSels(stageDataToRelease) when IS_FP else getVirtualIntDestSels(stageDataToRelease);
    psels <= getPhysicalFloatDestSels(stageDataToRelease) when IS_FP else getPhysicalIntDestSels(stageDataToRelease);

    overridden <= findOverriddenDests(stageDataToRelease, IS_FP);

    physCommitFreedDelayed <= selAndCompactPhysDests(physStableDelayed, physCommitDestsDelayed, stableUpdateSelDelayed, freeListPutSel);
    physCommitDestsDelayed <= getPhysicalDests(stageDataToRelease);
    
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
    
    freeListTakeSel <= whichTakeReg(stageDataToReserve, IS_FP); -- CAREFUL: must agree with Sequencer signals
    -- Releasing a register every time any dest exists (but not always prev stable!)
    freeListPutSel <= vsels;
    
    newPhysDests <= newPhysDestsAsync;
    newPhysDestPointer <= freeListTakeNumTags(0);
    
    IMPL: block
        signal listContent32: WordArray(0 to FREE_LIST_SIZE/4 - 1) := initFreeList32(IS_FP);
        
        signal listPtrTake, listPtrTakeNext, listPtrTake_N, listPtrTakeStable, listPtrTakeStable_N, listPtrTakeStableNext, causingTag: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
        signal listPtrPut, listPtrPutNext, listPtrPut_N: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
        
        signal memWord, memInput: Word := (others => '0');
        
        signal listFront, listBack, listFront_N, listBack_N, 
                        listFrontNext, listBackNext, listFrontNext_N, listBackNext_N
                     --, listFrontNextShift, listFrontNextMemNoTake, listFrontNextMemTake
                     : PhysNameArray(0 to 7) := (others => (others => '0'));
        signal memData: Word := (others => '0');
        signal physPtrTake, physPtrTakeNext, physPtrTake_N, effectivePhysPtrTake: SmallNumber := i2slv(0, SMALL_NUMBER_SIZE);
        signal physPtrPut, physPtrPutNext, physPtrPut_N: SmallNumber := i2slv(N_PHYS - 32 - FP_1, SMALL_NUMBER_SIZE);
    
        signal numFront, numBack, numBackReduced, numFront_N, numFrontNext, numBack_N, numBackNext,  numToTake: SmallNumber := (others => '0');
        signal memRead, needTake, doTake, canWriteBack: std_logic := '0';
        
        signal memTemp, memTemp_N: PhysNameArray(0 to 3) := (others => (others => '0'));

        --signal overNF, overNFmNT: std_logic_vector(0 to 7) := (others => '0');

        function nextShift(list: PhysNameArray; memData: PhysNameArray; allow: std_logic; nF, nT: SmallNumber) return PhysNameArray is
            variable res: PhysNameArray(0 to 7) := (others => (others => '0'));
            variable ind: SmallNumber := (others => '0');
            variable indM: SmallNumber := (others => '0');--std_logic_vector(1 downto 0) := (others => '0');           
            variable listExt: PhysNameArray(0 to 11) := list & PhysNameArray'(X"00", X"00", X"00", X"00");
        begin
            for i in 0 to 6 loop
                ind := addIntTrunc(nT, -1, 2);
                indM := addInt(ind, i + 1);
                res(i) := listExt(slv2u(indM(3 downto 0))); -- 4 bits for 12-element list
            end loop;          
            return res;
        end function;

        function nextMemNoTake(list: PhysNameArray; memData: PhysNameArray; allow: std_logic; nF, nT: SmallNumber) return PhysNameArray is
            variable res: PhysNameArray(0 to 7) := (others => (others => '0'));
            variable ind: SmallNumber := (others => '0');
            variable notNF: SmallNumber := not nF;               
        begin
            for i in 0 to 7 loop
                ind := addIntTrunc(notNF, 1 + i, 2);
                res(i) := memData(slv2u(ind(1 downto 0)));
            end loop;         
            return res;
        end function;
        
        function nextMemTake(list: PhysNameArray; memData: PhysNameArray; allow: std_logic; nF, nT: SmallNumber) return PhysNameArray is
            variable res: PhysNameArray(0 to 7) := (others => (others => '0'));
            variable ind: SmallNumber := (others => '0');
        begin
            for i in 0 to 7 loop
                ind := addIntTrunc(subSN(nT, nF), i, 2);
                res(i) := memData(slv2u(ind(1 downto 0)));
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
        
        
            function getNumFrontNext(numFront, causingTag: SmallNumber; freeListRewind, freeListTakeAllow, memRead: std_logic; freeListTakeSel: std_logic_vector)
            return SmallNumber is
                variable res: SmallNumber := numFront;
                variable nTaken, tmpTag2: SmallNumber := (others => '0');
            begin

                nTaken := i2slv(countOnes(freeListTakeSel), SMALL_NUMBER_SIZE);
                res := numFront;
                
                if freeListRewind = '1' then                     
                    tmpTag2(1 downto 0) := causingTag(1 downto 0);
                    res := subSN(i2slv(0, SMALL_NUMBER_SIZE), tmpTag2); -- TODO: find simpler notation for uminus
                end if;
                
                if freeListTakeAllow = '1' and freeListRewind = '0' then
                    res := subSN(res, nTaken);
                end if;
                
                if freeListRewind = '0' then
                     if cmpLeS(res, 4) = '1' and memRead = '1' then
                         res := addInt(res, 4); 
                     end if;               
                end if;

                res(7 downto 5) := (others => res(4));
                                
                return res;
            end function;

        
        function moveFrontList(list: PhysNameArray; numFront, numToTake: SmallNumber; doTake: std_logic; input: PhysNameArray) return PhysNameArray is
            variable res: PhysNameArray(0 to 7) := list;          
            variable overNF, overNFmNT: std_logic_vector(0 to 7) := (others => '0');
            variable listFrontNextShift, listFrontNextMemNoTake, listFrontNextMemTake: PhysNameArray(0 to 7) := (others => (others => '0'));
        begin
            overNF := getOverNF(numFront, numToTake);
            overNFmNT := getOverNFmNT(numFront, numToTake);
            
            
            for i in 0 to 7 loop
                listFrontNextShift :=       nextShift      (list, input, '1', numFront, numToTake);
                listFrontNextMemNoTake :=   nextMemNoTake  (list, input, '1', numFront, numToTake);
                listFrontNextMemTake :=     nextMemTake    (list, input, '1', numFront, numToTake);
                
                if   (    doTake and     overNFmNT(i)) = '1' then
                    res(i) := listFrontNextMemTake(i);
                elsif (    doTake and not overNFmNT(i)) = '1' then
                    res(i) := listFrontNextShift(i);
                elsif (not doTake and     overNF(i)) = '1' then
                    res(i) := listFrontNextMemNoTake(i);
                else
                    res(i) := list(i);                    
                end if;        

            end loop;
            return res;
        end function;


        function moveBackList(list: PhysNameArray; canWriteBack, putAllow: std_logic; numReduced: SmallNumber; input: PhysNameArray) return PhysNameArray is
            variable listExt: PhysNameArray(0 to 11) := (others => (others => '0'));
            variable res: PhysNameArray(0 to 7) := (others => (others => '0'));
        begin
            listExt(0 to 7) := list;
            
            if canWriteBack = '1' then
                listExt(0 to 7) := listExt(4 to 11);
            end if;
            
            if putAllow = '1' then
                for i in 0 to 3 loop
                    listExt(slv2u(numReduced) + i) := input(i);
                end loop;
            end if;
            
            res(0 to 6) := listExt(0 to 6);
            return res;
        end function;
 
            function splitWord(w: Word) return PhysNameArray is
                variable res: PhysNameArray(0 to 3) := (others => (others => '0'));
            begin                   
                res(0) := w(7 downto 0);
                res(1) := w(15 downto 8);
                res(2) := w(23 downto 16);
                res(3) := w(31 downto 24);
                return res;
            end function; 
                    
    begin
            memTemp <= splitWord(memWord);
    
    
        doTake <= freeListTakeAllow and isNonzero(freeListTakeSel);


        freeListTakeNumTags(0) <= i2slv((slv2u(listPtrTake)) mod FREE_LIST_SIZE, SMALL_NUMBER_SIZE);
        newPhysDestsAsync(0 to PIPE_WIDTH-1) <= listFront(0 to PIPE_WIDTH-1);
      
        needTake <= cmpLeS(numFront, addInt(numToTake, 4));
        numToTake <= i2slv(countOnes(freeListTakeSel), SMALL_NUMBER_SIZE) when freeListTakeAllow = '1' else (others => '0');

        causingTag <= listPtrTakeStable when lateEventSignal = '1' else causingPointer;


        effectivePhysPtrTake <= addInt(physPtrTake, 4) when (needTake and memRead) = '1' else physPtrTake;
        
            listPtrTakeNext <= causingTag when freeListRewind = '1'
                        else   addInt(listPtrTake, countOnes(freeListTakeSel)) when freeListTakeAllow = '1'
                        else   listPtrTake;
            
            physPtrTakeNext <= causingTag(SMALL_NUMBER_SIZE-1 downto 2) & physPtrTake(1 downto 0) when freeListRewind = '1'
                        else   effectivePhysPtrTake;

            listPtrTakeStableNext <= addInt(listPtrTakeStable, countOnes(stableTakeUpdate)) when freeListPutAllow = '1'
                             else    listPtrTakeStable;

            listPtrPutNext <= addInt(listPtrPut, countOnes(freeListPutSel)) when freeListPutAllow = '1'
                        else  listPtrPut;

            physPtrPutNext <= addInt(physPtrPut, 4) when canWriteBack = '1'
                        else  physPtrPut;

            numFrontNext <= getNumFrontNext(numFront, causingTag, freeListRewind, freeListTakeAllow, memRead, freeListTakeSel);
                    
            numBackNext <= addIntTrunc(numBackReduced, countOnes(freeListPutSel), 3) when freeListPutAllow = '1'
                      else numBackReduced;            
            numBackReduced <= numBack and "00000011";
                
            listFrontNext <= moveFrontList(listFront, numFront, numToTake, doTake, memTemp);
            listBackNext <= moveBackList(listBack, canWriteBack, freeListPutAllow, numBackReduced, physCommitFreedDelayed);

            
            canWriteBack <= cmpGeS(numBack, 4);


--                    listPtrTake <= listPtrTake;
--                    physPtrTake <= physPtrTake;
--                    listPtrTakeStable <= listPtrTakeStable;

--                    listPtrPut <= listPtrPut;
--                    physPtrPut <= physPtrPut;
                    
--                    numFront <= numFront;
--                    numBack <= numBack;
                    
--                    listFront <= listFront;
--                    listBack <= listBack;

--             memTemp <= memTemp; 

                   
                   memInput <= listBack(3) & listBack(2) & listBack(1) & listBack(0);
                    
        SYNCHRONOUS: process(clk)
        
            function readRegGroup(arr: WordArray; adr: SmallNumber) return PhysNameArray is
                variable res: PhysNameArray(0 to 3) := (others => (others => '0'));
                variable w: Word := (others => '0');
            begin
                w := arr(slv2u(adr(7 downto 2)));
                
                res(0) := w(7 downto 0);
                res(1) := w(15 downto 8);
                res(2) := w(23 downto 16);
                res(3) := w(31 downto 24);
                return res;
            end function;


            function readRegGroup_W(arr: WordArray; adr: SmallNumber) return Word is
                variable res: PhysNameArray(0 to 3) := (others => (others => '0'));
                variable w: Word := (others => '0');
            begin
                w := arr(slv2u(adr(7 downto 2)));
                
--                res(0) := w(7 downto 0);
--                res(1) := w(15 downto 8);
--                res(2) := w(23 downto 16);
--                res(3) := w(31 downto 24);
                return w;
            end function;

            procedure writeRegGroup(signal arr: inout WordArray; adr: in SmallNumber; input: in PhysNameArray) is
                variable w: Word := (others => '0');
            begin
                w := input(3) & input(2) & input(1) & input(0);
                
                arr(slv2u(adr(7 downto 2))) <= w;
            end procedure;

            procedure writeRegGroup_W(signal arr: inout WordArray; adr: in SmallNumber; w: in Word) is
            begin                
                arr(slv2u(adr(7 downto 2))) <= w;
            end procedure;
                    
        begin
            if rising_edge(clk) then
                    listPtrTake <= listPtrTakeNext;
                    physPtrTake <= physPtrTakeNext;
                    listPtrTakeStable <= listPtrTakeStableNext;

                    listPtrPut <= listPtrPutNext;
                    physPtrPut <= physPtrPutNext;

                    
                    numFront <= numFrontNext;
                    numBack <= numBackNext;
                    
                    listFront <= listFrontNext;
                    listBack <= listBackNext;
                    
                ----------------------------------------------------------------------
                -- Front side
                memRead <= not freeListRewind;
                --memTemp <= readRegGroup(listContent32, effectivePhysPtrTake);
                    memWord <= --readRegGroup_W(listContent32, effectivePhysPtrTake);
                               listContent32(slv2u(effectivePhysPtrTake(7 downto 2)));
                
                ----------------------------------------------------------------------                
                -- Back side
                if canWriteBack = '1' then
                    --writeRegGroup_W(listContent32, physPtrPut, memInput);
                        listContent32(slv2u(physPtrPut(7 downto 2))) <= memInput;
                end if;                        
                
                
                -- CHECK: 3 cycles to restore?
                if freeListRewind = '1' then
                    recoveryCounter <= i2slv(3, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addIntTrunc(recoveryCounter, -1, 2);
                end if;                
            end if;
        end process;            

-------------
-------------        
        VIEW: if VIEW_ON generate
            use work.Viewing.all;
            
            signal vFree, vUsed: std_logic_vector(0 to N_PHYS-1) := (others => '0');
            signal newTaken, newPut: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
            
            -- CAREFUL, TODO: this assumes some endianness, maybe needed universal
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
                                        
                    vFree <= getFreeVec(listContent32, physPtrTake, physPtrPut, listFront, numFront, listBack, numBack);
                    
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