----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 10.12.2018 21:49:16
-- Design Name: 
-- Module Name: LogicRenaming - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
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

function whichTakeReg(insVec: InstructionSlotArray) return std_logic_vector;
function whichPutReg(insVec: InstructionSlotArray) return std_logic_vector;

function findOverriddenDests(insVec: InstructionSlotArray) return std_logic_vector;

end package;


package body LogicRenaming is

	
    function getVirtualArgs(insVec: InstructionSlotArray) return RegNameArray is
        variable res: RegNameArray(0 to 3*insVec'length-1) := (others=>(others=>'0'));
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


function whichTakeReg(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.virtualArgSpec.intDestSel and insVec(i).full;        
    end loop;
    return res;
end function;

function whichPutReg(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.virtualArgSpec.intDestSel and 
            (insVec(i).full );-- or insVec(i).ins.controlInfo.squashed and FREE_LIST_COARSE_REWIND);     
    end loop;
    return res;
end function;


function findOverriddenDests(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others=>'0');
	variable em: std_logic_vector(insVec'range) := (others => '0');
begin
	em := getExceptionMask(insVec);
	for i in insVec'range loop
		for j in insVec'range loop
			if 		j > i and insVec(j).full = '1' and em(j) = '0' -- CAREFUL: if exception, doesn't write
				and insVec(i).ins.virtualArgSpec.dest(4 downto 0) = insVec(j).ins.virtualArgSpec.dest(4 downto 0)
			then				
				res(i) := '1';
			end if;
		end loop;
	end loop;			
	return res;
end function;

end package body;

