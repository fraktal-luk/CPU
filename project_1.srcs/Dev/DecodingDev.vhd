----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09.11.2018 22:30:26
-- Design Name: 
-- Module Name: DecodingDev - Behavioral
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
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.ArchDefs.all;

use work.InstructionState.all;


package DecodingDev is

function insText(ins: InstructionState) return string;


end package;


package body DecodingDev is

function reg2txt(reg: std_logic_vector) return string is
    variable res: string(1 to 2);
    constant letters: string(1 to 16) := "0123456789abcdef";
begin
    if reg(4) = '1' then
        res(1) := '1';
    else
        res(1) := '0';
    end if;
    
    res(2) := letters(slv2u(reg) + 1);
    
    return res;
end function;

function insText(ins: InstructionState) return string is
    variable dest, src0, src1, src2: string(1 to 3) := (others => '*');
begin
    

     dest(2 to 3) := reg2txt(ins.virtualArgSpec.dest);
     src0(2 to 3) := reg2txt((ins.virtualArgSpec.args(0)));
     src1(2 to 3) := reg2txt((ins.virtualArgSpec.args(1)));
     src2(2 to 3) := reg2txt((ins.virtualArgSpec.args(2)));
    
    if ins.virtualArgSpec.intDestSel = '1' then
        dest(1) := 'r';
    end if;
    if ins.virtualArgSpec.intArgSel(0) = '1' then
        src0(1) := 'r';
    end if;
    if ins.virtualArgSpec.intArgSel(1) = '1' then
        src1(1) := 'r';
    end if;
    
    return ExecFunc'image(ins.operation.func) & "  " &
     dest & ", " &
     src0 & ", " &
     src1 & ", #" &
     integer'image(slv2s(ins.constantArgs.imm));    
    
    --return "";
end function;

end package body;