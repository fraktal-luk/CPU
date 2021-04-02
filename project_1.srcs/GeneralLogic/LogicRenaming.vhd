----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.Arith.all;

use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package LogicRenaming is


type RegMaskArray is array(natural range <>) of std_logic_vector(0 to 31);


function getVirtualArgs(insVec: InstructionSlotArray) return RegNameArray;    
function getVirtualDests(insVec: InstructionSlotArray) return RegNameArray;
function getPhysicalArgs(insVec: InstructionSlotArray) return PhysNameArray;
function getPhysicalDests(insVec: InstructionSlotArray) return PhysNameArray;

function whichTakeReg(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;
function findOverriddenDests(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;

function getPhysicalIntDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getPhysicalFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getVirtualIntDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getVirtualFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector;


function initMap(constant IS_FP: boolean) return PhysNameArray;    

function getSelectedA(adr: RegNameArray; mask: std_logic_vector; constant IS_FP: boolean) return RegMaskArray;

function getNextMap(content, stable: PhysNameArray; inputArr: PhysNameArray; reserve: std_logic_vector; sm: RegMaskArray; sending, rew: std_logic)
return PhysNameArray;

function getNumFrontNext(numFront, causingTag: SmallNumber; freeListRewind, freeListTakeAllow, memRead: std_logic; freeListTakeSel: std_logic_vector)
return SmallNumber;
function moveFrontList(list: PhysNameArray; numFront, numToTake: SmallNumber; input: PhysNameArray) return PhysNameArray;
function moveBackList(list: PhysNameArray; canWriteBack, putAllow: std_logic; numReduced: SmallNumber; input: PhysNameArray) return PhysNameArray;

function splitWord(w: Word) return PhysNameArray;

function getFp1(constant IS_FP: boolean) return natural;
function initFreeList32(constant IS_FP: boolean) return WordArray;
function compactFreedRegs(names: PhysNameArray; mask: std_logic_vector) return PhysNameArray;
function selAndCompactPhysDests(physStableDelayed, physCommitDestsDelayed: PhysNameArray; stableUpdateSelDelayed, freeListPutSel: std_logic_vector)
return PhysNameArray;

end package;



package body LogicRenaming is

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

function getSelection(pa: PhysNameArray; st: PhysName; reserve: std_logic_vector; sa: std_logic_vector; rew: std_logic) return PhysName is
    variable res: PhysName;
    variable sel: std_logic_vector(1 downto 0) := (others => '0');        
    constant resVec: PhysNameArray(0 to 3) := pa;
    constant r0: std_logic := reserve(0);
    constant r1: std_logic := reserve(1);
    constant r2: std_logic := reserve(2);
    constant r3: std_logic := reserve(3);
    constant s0: std_logic := sa(0);  
    constant s1: std_logic := sa(1);  
    constant s2: std_logic := sa(2);  
    constant s3: std_logic := sa(3);  
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
        if IS_FP then
            res(i)(0) := '0'; -- reg 0 not used for Int
        end if;
    end loop;
    return res;
end function;


function getNextMap(content, stable: PhysNameArray; inputArr: PhysNameArray; reserve: std_logic_vector; sm: RegMaskArray; sending, rew: std_logic)
return PhysNameArray is
    variable res: PhysNameArray(0 to 31) := content;--(others => (others => '0'));
    variable enMask: std_logic_vector(0 to 31) := (others => '0');
    variable sa: std_logic_vector(0 to PIPE_WIDTH-1);
begin
    for i in 0 to 31 loop
        enMask(i) :=  (sending and (sm(0)(i) or sm(1)(i) or sm(2)(i) or sm(3)(i))) 
                    or rew;
        
        sa := (sm(0)(i), sm(1)(i), sm(2)(i), sm(3)(i));
        
        if enMask(i) = '1' then
            res(i) := getSelection(inputArr, stable(i), reserve, sa, rew);
        end if;                                   
    end loop;
    return res;
end function;



function getVirtualArgs(insVec: InstructionSlotArray) return RegNameArray is
    variable res: RegNameArray(0 to 3*insVec'length-1) := (others => (others => '0'));
begin
    for i in insVec'range loop
        res(3*i+0) := insVec(i).ins.virtualArgSpec.args(0)(4 downto 0);
        res(3*i+1) := insVec(i).ins.virtualArgSpec.args(1)(4 downto 0);
        res(3*i+2) := insVec(i).ins.virtualArgSpec.args(2)(4 downto 0);
    end loop;
    return res;
end function;


function getVirtualDests(insVec: InstructionSlotArray) return RegNameArray is
    variable res: RegNameArray(0 to insVec'length-1) := (others=>(others=>'0'));
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.virtualArgSpec.dest(4 downto 0);
    end loop;
    return res;
end function;

function getPhysicalArgs(insVec: InstructionSlotArray) return PhysNameArray is
    variable res: PhysNameArray(0 to 3*insVec'length-1) := (others=>(others=>'0'));
begin
    for i in insVec'range loop
        res(3*i+0) := insVec(i).ins.physicalArgSpec.args(0);
        res(3*i+1) := insVec(i).ins.physicalArgSpec.args(1);
        res(3*i+2) := insVec(i).ins.physicalArgSpec.args(2);
    end loop;
    return res;
end function;


function getPhysicalDests(insVec: InstructionSlotArray) return PhysNameArray is
    variable res: PhysNameArray(0 to insVec'length-1) := (others=>(others=>'0'));
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.physicalArgSpec.dest;
    end loop;
    return res;
end function;


function getPhysicalIntDestSels(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to insVec'length-1) := (others => '0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.physicalArgSpec.intDestSel;
    end loop;
    return res;
end function;

function getPhysicalFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to insVec'length-1) := (others => '0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.physicalArgSpec.floatDestSel;
    end loop;
    return res;
end function;


function getVirtualIntDestSels(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to insVec'length-1) := (others => '0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.virtualArgSpec.intDestSel;
    end loop;
    return res;
end function;

function getVirtualFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to insVec'length-1) := (others => '0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.virtualArgSpec.floatDestSel;
    end loop;
    return res;
end function;


function whichTakeReg(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := ((insVec(i).ins.virtualArgSpec.intDestSel and not bool2std(fp)) or (insVec(i).ins.virtualArgSpec.floatDestSel and bool2std(fp)));
    end loop;
    return res;
end function;

function findOverriddenDests(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others => '0');
begin
	for i in insVec'range loop
		for j in insVec'range loop
			if 		j > i
			    and ((insVec(j).ins.virtualArgSpec.intDestSel = '1' and not fp) or (insVec(j).ins.virtualArgSpec.floatDestSel = '1' and fp)) -- Overrides only if really uses a destination!
				and insVec(i).ins.virtualArgSpec.dest(4 downto 0) = insVec(j).ins.virtualArgSpec.dest(4 downto 0)
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

function nextShift(list: PhysNameArray; nT: SmallNumber) return PhysNameArray is
    variable res: PhysNameArray(0 to 7) := (others => (others => '0'));          
    variable listExt: PhysNameArray(0 to 11) := list & PhysNameArray'(X"00", X"00", X"00", X"00");
begin
    for i in 0 to 7 loop
        res(i) := listExt(i + slv2u(nT(2 downto 0))); -- 4 bits for 12-element list
    end loop;          
    return res;
end function;
        
function moveFrontList(list: PhysNameArray; numFront, numToTake: SmallNumber; input: PhysNameArray) return PhysNameArray is
    variable res: PhysNameArray(0 to 7) := list;          
    variable listShifted: PhysNameArray(0 to 7) := (others => (others => '0'));
    constant diff: SmallNumber := sub(numToTake, numFront);
    variable ind: SmallNumber := (others => '0');
begin                
    listShifted := nextShift(list, numToTake);
    for i in 0 to 7 loop
        ind := addInt(diff, i);            
        if slv2s(ind) >= 0 then
            res(i) := input(slv2u(ind(1 downto 0)));
        else 
            res(i) := listShifted(i);                    
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

end package body;

