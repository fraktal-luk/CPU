----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.Arith.all;

use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package LogicRenaming is


type RegMaskArray is array(natural range <>) of std_logic_vector(0 to 31);

function getPhysicalArgs(sch: SchedulerState) return PhysNameArray;

function getVirtualArgs(ria: RenameInfoArray) return RegNameArray;    
function getVirtualDests(ria: RenameInfoArray) return RegNameArray;
function getPhysicalArgs(ria: RenameInfoArray) return PhysNameArray;
function getPhysicalDests(ria: RenameInfoArray) return PhysNameArray;

function getVirtualArgs(ia: BufferEntryArray) return RegNameArray;

function whichTakeReg(ria: RenameInfoArray; fp: boolean) return std_logic_vector;
function findOverriddenDests(ria: RenameInfoArray; fp: boolean) return std_logic_vector;

function getPhysicalIntDestSels(ria: RenameInfoArray) return std_logic_vector;
function getPhysicalFloatDestSels(ria: RenameInfoArray) return std_logic_vector;
function getVirtualIntDestSels(ria: RenameInfoArray) return std_logic_vector;
function getVirtualFloatDestSels(ria: RenameInfoArray) return std_logic_vector;

function getPsels(ria: RenameInfoArray) return std_logic_vector;

function initMap(constant IS_FP: boolean) return PhysNameArray;    

function getSelectedA(adr: RegNameArray; mask: std_logic_vector; constant IS_FP: boolean) return RegMaskArray;

function getNextMap(content, stable: PhysNameArray; inputArr: PhysNameArray; reserve: std_logic_vector; sm: RegMaskArray; sending, rew: std_logic)
return PhysNameArray;

function getNumFrontNext(numFront, causingTag: SmallNumber; freeListRewind, freeListTakeAllow, memRead: std_logic; freeListTakeSel: std_logic_vector)
return SmallNumber;
function moveFrontList(list: PhysNameArray; numFront, numToTake, numToTake_N: SmallNumber; input: PhysNameArray; allowTake: std_logic) return PhysNameArray;
function moveBackList(list: PhysNameArray; canWriteBack, putAllow: std_logic; numReduced: SmallNumber; input: PhysNameArray) return PhysNameArray;

function splitWord(w: Word) return PhysNameArray;

function getFp1(constant IS_FP: boolean) return natural;
function initFreeList(constant IS_FP: boolean) return PhysNameArray;
function initFreeList32(constant IS_FP: boolean) return WordArray;
function selAndCompactPhysDests(physStableDelayed, physCommitDestsDelayed: PhysNameArray; stableUpdateSelDelayed, freeListPutSel: std_logic_vector)
return PhysNameArray;

function assignDests(ia: BufferEntryArray; newDests: PhysNameArray; constant IS_FP: boolean) return PhysNameArray;
function assignDests(ria: RenameInfoArray; newDests: PhysNameArray; constant IS_FP: boolean) return PhysNameArray;

function DB_addTag(dbi: InstructionDebugInfo; tag: InsTag) return InstructionDebugInfo;
function DB_addRenameCounter(dbi: InstructionDebugInfo; ctr: Word) return InstructionDebugInfo;
procedure DB_trackSeqNum(renamed: InstructionSlotArray);

end package;



package body LogicRenaming is

function assignDests(ia: BufferEntryArray; newDests: PhysNameArray; constant IS_FP: boolean) return PhysNameArray is
    variable res: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    variable reserveSelSig, takeVecInt, takeVecFloat, stores, loads: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
    variable nToTake: integer := 0;
    variable newGprTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));    
    variable newNumberTags: InsTagArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
    variable found: boolean := false;
    variable va: InstructionArgSpec := DEFAULT_ARG_SPEC;
begin
    -- Assign dest registers
    for i in 0 to PIPE_WIDTH-1 loop
        va := ia(i).argSpec;
        if va.intDestSel = '1' and not IS_FP then
            res(i) := newDests(countOnes(takeVecInt)); -- how many used before
        elsif va.floatDestSel = '1' and IS_FP then
            res(i) := newDests(countOnes(takeVecFloat)); -- how many used before
        end if;
        takeVecInt(i) := va.intDestSel;   
        takeVecFloat(i) := va.floatDestSel;   
    end loop;
    return res;
end function;

function assignDests(ria: RenameInfoArray; newDests: PhysNameArray; constant IS_FP: boolean) return PhysNameArray is
    variable res: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    variable reserveSelSig, takeVecInt, takeVecFloat, stores, loads: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
    variable nToTake: integer := 0;
    variable newGprTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));    
    variable newNumberTags: InsTagArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
    variable found: boolean := false;
begin
    -- Assign dest registers
    for i in 0 to PIPE_WIDTH-1 loop
        if ria(i).destSel = '1' and not IS_FP then
            res(i) := newDests(countOnes(takeVecInt)); -- how many used before
        elsif ria(i).destSelFP = '1' and IS_FP then
            res(i) := newDests(countOnes(takeVecFloat)); -- how many used before
        end if;
        takeVecInt(i) := ria(i).destSel;   
        takeVecFloat(i) := ria(i).destSelFP;   
    end loop;
    return res;       
end function;

function initMap(constant IS_FP: boolean) return PhysNameArray is
    variable res: PhysNameArray(0 to 31) := (others => (others=> '0'));
begin
    for i in 0 to 31 loop
        res(i) := i2slv(i, PhysName'length);
        if IS_FP then
         res(i) := i2slv(i + 1, PhysName'length); -- CAREFUL: No reg 0 for FP
        end if;			
    end loop;
    return res;
end function;

function getSelMask(adr: RegName; en: std_logic) return std_logic_vector is
    variable res: std_logic_vector(0 to 31) := (others => '0');
begin
    for i in 0 to 31 loop -- selected if reg number agrees and enabled flag set
        res(i) := bool2std(adr(4 downto 0) & en = i2slv(i, 5) & '1');
    end loop;
    return res;
end function;

function extendMask(v: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to 3) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := v(i);
    end loop;
    return res;
end function;

function extendRegList(pa: PhysNameArray) return PhysNameArray is
    variable res: PhysNameArray(0 to 3) := (others => (others => '0'));
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := pa(i);
    end loop;
    return res;
end function;

function getSelection(pa: PhysNameArray; st: PhysName; reserve: std_logic_vector; sa: std_logic_vector; rew: std_logic) return PhysName is
    variable res: PhysName;
    variable sel: std_logic_vector(1 downto 0) := (others => '0');        
    constant resVec: PhysNameArray(0 to 3) := extendRegList(pa);
    constant reserveE: std_logic_vector(0 to 3) := extendMask(reserve);
    constant r0: std_logic := reserveE(0);
    constant r1: std_logic := reserveE(1);
    constant r2: std_logic := reserveE(2);
    constant r3: std_logic := reserveE(3);
    constant saE: std_logic_vector(0 to 3) := extendMask(sa);
    constant s0: std_logic := saE(0);
    constant s1: std_logic := saE(1);
    constant s2: std_logic := saE(2);
    constant s3: std_logic := saE(3);
begin   
    if (s3 and r2 and r1 and r0) = '1' then
        sel := "11";
    elsif ((s3 and ((r1 and r0) or (r2 and r0) or (r2 and r1))) or (s2 and r1 and r0)) = '1' then
        sel := "10";
    elsif ((s3 and (r2 or r1 or r0)) or (s2 and (r1 or r0)) or (s1 and r0)) = '1' then
        sel := "01";
    else
        sel := "00";
    end if;
    ----------
    if rew = '1' then
        res := st;
    else
        res := resVec(slv2u(sel));                    
    end if;
    return res;
end function;


function getSelectedA(adr: RegNameArray; mask: std_logic_vector; constant IS_FP: boolean) return RegMaskArray is
    variable res: RegMaskArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
begin
    for i in adr'range loop 
        res(i) := getSelMask(adr(i), mask(i));
        if not IS_FP then
            res(i)(0) := '0'; -- reg 0 not used for Int
        end if;
    end loop;
    return res;
end function;


function getNextMap(content, stable: PhysNameArray; inputArr: PhysNameArray; reserve: std_logic_vector; sm: RegMaskArray; sending, rew: std_logic)
return PhysNameArray is
    variable res: PhysNameArray(0 to 31) := content;--(others => (others => '0'));
    variable enMask: std_logic_vector(0 to 31) := (others => '0');
    variable sa: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to 31 loop
        for k in 0 to PIPE_WIDTH-1 loop
        --sa := (sm(0)(i), sm(1)(i), sm(2)(i), sm(3)(i));
            sa(k) := sm(k)(i);
        end loop;

        enMask(i) :=  (sending and isNonzero(sa))--  (sa(0)(i) or sa(1)(i) or sa(2)(i) or sa(3)(i))) 
                    or rew;
        
        if enMask(i) = '1' then
            res(i) := getSelection(inputArr, stable(i), reserve, sa, rew);
        end if;                                   
    end loop;
    return res;
end function;


function getVirtualArgs(ria: RenameInfoArray) return RegNameArray is
    variable res: RegNameArray(0 to 3*ria'length-1) := (others => (others => '0'));
begin
    for i in ria'range loop
--        res(3*i+0) := ria(i).virtualSources(0)(4 downto 0);
--        res(3*i+1) := ria(i).virtualSources(1)(4 downto 0);
--        res(3*i+2) := ria(i).virtualSources(2)(4 downto 0);
        
            res(3*i+0) := ria(i).argStates(0).virtual(4 downto 0);
            res(3*i+1) := ria(i).argStates(1).virtual(4 downto 0);
            res(3*i+2) := ria(i).argStates(2).virtual(4 downto 0);
    end loop;
    return res;
end function;

function getVirtualArgs(ia: BufferEntryArray) return RegNameArray is
    variable res: RegNameArray(0 to 3*ia'length-1) := (others => (others => '0'));
begin
    for i in ia'range loop
        res(3*i+0) := ia(i).argSpec.args(0)(4 downto 0);
        res(3*i+1) := ia(i).argSpec.args(1)(4 downto 0);
        res(3*i+2) := ia(i).argSpec.args(2)(4 downto 0);
    end loop;
    return res;
end function;

function getVirtualDests(ria: RenameInfoArray) return RegNameArray is
    variable res: RegNameArray(0 to ria'length-1) := (others=>(others=>'0'));
begin
    for i in ria'range loop
        res(i) := ria(i).virtualDest(4 downto 0);
    end loop;
    return res;
end function;

function getPhysicalArgs(ria: RenameInfoArray) return PhysNameArray is
    variable res: PhysNameArray(0 to 3*ria'length-1) := (others=>(others=>'0'));
begin
    for i in ria'range loop
--        res(3*i+0) := ria(i).physicalSources(0);
--        res(3*i+1) := ria(i).physicalSources(1);
--        res(3*i+2) := ria(i).physicalSources(2);

            res(3*i+0) := ria(i).argStates(0).physical;
            res(3*i+1) := ria(i).argStates(1).physical;
            res(3*i+2) := ria(i).argStates(2).physical;
    end loop;
    return res;
end function;


function getPhysicalArgs(sch: SchedulerState) return PhysNameArray is
    variable res: PhysNameArray(0 to 2) := (others=>(others=>'0'));
begin
--        res(0) := sch.argSpec.args(0);
--        res(1) := sch.argSpec.args(1);
--        res(2) := sch.argSpec.args(2);
        
                res(0) := sch.args(0);
                res(1) := sch.args(1);
                res(2) := sch.args(2);
    return res;
end function;


function getPhysicalDests(ria: RenameInfoArray) return PhysNameArray is
    variable res: PhysNameArray(0 to ria'length-1) := (others=>(others=>'0'));
begin
    for i in ria'range loop
        res(i) := ria(i).physicalDest;
    end loop;
    return res;
end function;


function getPhysicalIntDestSels(ria: RenameInfoArray) return std_logic_vector is
    variable res: std_logic_vector(0 to ria'length-1) := (others => '0');
begin
    for i in ria'range loop
        res(i) := ria(i).destSel;
    end loop;
    return res;
end function;

function getPhysicalFloatDestSels(ria: RenameInfoArray) return std_logic_vector is
    variable res: std_logic_vector(0 to ria'length-1) := (others => '0');
begin
    for i in ria'range loop
        res(i) := ria(i).destSelFP;
    end loop;
    return res;
end function;

function getPsels(ria: RenameInfoArray) return std_logic_vector is
    variable res: std_logic_vector(0 to ria'length-1) := (others => '0');
begin
    for i in ria'range loop
        res(i) := ria(i).psel;
    end loop;
    return res;
end function;


function getVirtualIntDestSels(ria: RenameInfoArray) return std_logic_vector is
    variable res: std_logic_vector(0 to ria'length-1) := (others => '0');
begin
    for i in ria'range loop
        res(i) := ria(i).destSel;
    end loop;
    return res;
end function;

function getVirtualFloatDestSels(ria: RenameInfoArray) return std_logic_vector is
    variable res: std_logic_vector(0 to ria'length-1) := (others => '0');
begin
    for i in ria'range loop
        res(i) := ria(i).destSelFP;
    end loop;
    return res;
end function;


function whichTakeReg(ria: RenameInfoArray; fp: boolean) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := ((ria(i).destSel and not bool2std(fp)) or (ria(i).destSelFP and bool2std(fp)));
    end loop;
    return res;
end function;


function findOverriddenDests(ria: RenameInfoArray; fp: boolean) return std_logic_vector is
	variable res: std_logic_vector(ria'range) := (others => '0');
begin
	for i in ria'range loop
		for j in ria'range loop
			if 		j > i
			    and ((ria(j).destSel = '1' and not fp) or (ria(j).destSelFP = '1' and fp)) -- Overrides only if really uses a destination!
				and ria(i).virtualDest(4 downto 0) = ria(j).virtualDest(4 downto 0)
			then				
				res(i) := '1';
			end if;
		end loop;
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
        res := subSN(sn(0), tmpTag2);
    end if;
    
    if freeListTakeAllow = '1' and freeListRewind = '0' then -- NOTE: this is exclusive with previous 'if'; so logic could be simplified?
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

function nextShift(list: PhysNameArray; nT: SmallNumber) return PhysNameArray is
    variable res: PhysNameArray(0 to 7) := (others => (others => '0'));          
    variable listExt: PhysNameArray(0 to 11) := list & PhysNameArray'(X"00", X"00", X"00", X"00");
begin
    for i in 0 to 7 loop
        res(i) := listExt(i + slv2u(nT(2 downto 0))); -- 4 bits for 12-element list
    end loop;          
    return res;
end function;


function moveListImpl(list: PhysNameArray; diff: SmallNumber; input: PhysNameArray) return PhysNameArray is
    variable res: PhysNameArray(0 to 7) := list;          
    --variable listShifted: PhysNameArray(0 to 7) := (others => (others => '0'));
    --constant diff: SmallNumber := sub(numToTake, numFront);
    variable ind: SmallNumber := (others => '0');
begin
    --listShifted := nextShift(list, numToTake);
    for i in 0 to 7 loop
        ind := addInt(diff, i);            
        if slv2s(ind) >= 0 then
            res(i) := input(slv2u(ind(1 downto 0)));
        else
            res(i) := list(i);                    
        end if;
    end loop;
    return res;
end function;


function moveFrontList(list: PhysNameArray; numFront, numToTake, numToTake_N: SmallNumber; input: PhysNameArray; allowTake: std_logic) return PhysNameArray is
    variable res: PhysNameArray(0 to 7) := list;          
    variable listShifted: PhysNameArray(0 to 7) := (others => (others => '0'));
    constant diff: SmallNumber := sub(numToTake_N, numFront);
    constant diffZ: SmallNumber := sub(sn(0), numFront);
    variable ind: SmallNumber := (others => '0');
begin
    listShifted := nextShift(list, numToTake);
--    for i in 0 to 7 loop
--        ind := addInt(diff, i);            
--        if slv2s(ind) >= 0 then
--            res(i) := input(slv2u(ind(1 downto 0)));
--        else
--            res(i) := listShifted(i);                    
--        end if;
--    end loop;
    if allowTake = '1' then
        res := moveListImpl(listShifted, diff, input);
    else
        res := moveListImpl(listShifted, diffZ, input);
    end if;
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
        for i in 0 to PIPE_WIDTH-1 loop
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



function getFp1(constant IS_FP: boolean) return natural is
begin
   if IS_FP then
       return 1;
   else
       return 0;
   end if;
end function;

function initFreeList(constant IS_FP: boolean) return PhysNameArray is
    variable fp1: natural := 0;
    variable res: PhysNameArray(0 to FREE_LIST_SIZE - 1) := (others => (others=> '0'));
begin
    if IS_FP then
        fp1 := 1;
    end if;
    for i in 0 to (N_PHYS - 32) - 1 loop
        res(i) := i2slv(i + 32 + fp1, PhysName'length);
    end loop;

--    -- For FP, there's no register 0, mapper starts with 1:32 rather than 0:31, so one less is in this list
--    if IS_FP then
--        res((N_PHYS - 32)/4 - 1)(31 downto 24) := (others => '0');
--    end if;
    return res;
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


    function DB_addTag(dbi: InstructionDebugInfo; tag: InsTag) return InstructionDebugInfo is
        variable res: InstructionDebugInfo := dbi;
    begin
        -- pragma synthesis off
        res.tag := tag;
        -- pragma synthesis on
        return res;
    end function;

    function DB_addRenameCounter(dbi: InstructionDebugInfo; ctr: Word) return InstructionDebugInfo is
        variable res: InstructionDebugInfo := dbi;
    begin
        -- pragma synthesis off
        res.rename := ctr;
        -- pragma synthesis on
        return res;
    end function;

    procedure DB_trackSeqNum(renamed: InstructionSlotArray) is
    begin
        -- pragma synthesis off
        if DB_OP_TRACKING then
            for i in renamed'range loop
                if renamed(i).ins.dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                    report "";
                    report "DEBUG: Tracked seqNum renamed: " & work.CpuText.slv2hex(DB_TRACKED_SEQ_NUM);

                    report "";

                    return;
                end if;
            end loop;
        end if;
        -- pragma synthesis on
    end procedure;

end package body;
