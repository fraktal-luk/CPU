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

function getVirtualArgs(insVec: InstructionSlotArray) return RegNameArray;    
function getVirtualDests(insVec: InstructionSlotArray) return RegNameArray;
function getPhysicalArgs(insVec: InstructionSlotArray) return PhysNameArray;
function getPhysicalDests(insVec: InstructionSlotArray) return PhysNameArray;

function whichTakeReg(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;
function whichTakeReg_T(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;
function whichPutReg(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;

function findOverriddenDests(insVec: InstructionSlotArray; fp: boolean) return std_logic_vector;


function getPhysicalIntDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getPhysicalFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getVirtualIntDestSels(insVec: InstructionSlotArray) return std_logic_vector;
function getVirtualFloatDestSels(insVec: InstructionSlotArray) return std_logic_vector;

end package;


package body LogicRenaming is


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
        res(i) := ((insVec(i).ins.virtualArgSpec.intDestSel and not bool2std(fp)) or (insVec(i).ins.virtualArgSpec.floatDestSel and bool2std(fp)))
          ;-- and insVec(i).full;        
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

