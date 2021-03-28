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
function whichTakeReg_T(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;
function whichPutReg(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector; -- UNUSED

function findOverriddenDests(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;


function getPhysicalIntDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getPhysicalFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getVirtualIntDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getVirtualFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector;


function initMap(constant IS_FP: boolean) return PhysNameArray;    
function selectPhysDests(newDests: PhysNameArray; taking: std_logic_vector) return PhysNameArray;   

function getSelectedA(adr: RegNameArray; mask: std_logic_vector; constant IS_FP: boolean) return RegMaskArray;

function getNextMap(content, stable: PhysNameArray; inputArr: PhysNameArray; reserve: std_logic_vector; sm: RegMaskArray; sending, rew: std_logic)
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

function selectPhysDests(newDests: PhysNameArray; taking: std_logic_vector) return PhysNameArray is
    variable res: PhysNameArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
    variable num: natural := 0;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        num := countOnes(taking(0 to i-1));
        res(i) := newDests(num);
    end loop;
    
        if TMP_PARAM_DEST_MOVE then
            res := newDests;
        end if;
    
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

function whichTakeReg_T(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := ((insVec(i).ins.virtualArgSpec.intDestSel and not bool2std(fp)) or (insVec(i).ins.virtualArgSpec.floatDestSel and bool2std(fp)))
          ;--  and insVec(i).full;        
    end loop;
    return res;
end function;

function whichPutReg(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := ((insVec(i).ins.virtualArgSpec.intDestSel and not bool2std(fp)) or (insVec(i).ins.virtualArgSpec.floatDestSel and bool2std(fp)))
             and (insVec(i).full);
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



end package body;

