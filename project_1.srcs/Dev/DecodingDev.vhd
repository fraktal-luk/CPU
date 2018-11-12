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

-- DUMMY
		type OpFieldStruct is record
    opcode:            ProcOpcode;
    opcont:             ProcOpcont;    
    unit:        ExecUnit;
    func:        ExecFunc;
    --quintetSel:        QuintetSelect;
    --quintetValues: QuintetValArray;
    hasLeftImm:     std_logic;
    leftImm:         word;
    hasImm:             std_logic;
    imm:                 word;
    target:             word;
end record;

function getOpFields(w: word) return OpFieldStruct;

	procedure ofsInfo(ofs: in OpFieldStruct;
                        op: out BinomialOp;
                        ci: out InstructionClassInfo;
                        ca: out InstructionConstantArgs;
                        va: out InstructionVirtualArgs;
                        vda: out InstructionVirtualDestArgs);

-----------------

function insText(ins: InstructionState) return string;


end package;


package body DecodingDev is

        -- DUMMY
		function getOpFields(w: word) return OpFieldStruct is
			--variable num: integer;
			--variable parts: InsFieldTableNewW;
			variable ofs: OpFieldStruct;
			--variable opcd: slv6; --ProcOpcode;
			--variable opct: slv6; --ProcOpcont;
			--variable match: InsDefNewW;			
		begin


			return ofs;
		end function;

	procedure ofsInfo(ofs: in OpFieldStruct;
							op: out BinomialOp;
							ci: out InstructionClassInfo;
							ca: out InstructionConstantArgs;
							va: out InstructionVirtualArgs;
							vda: out InstructionVirtualDestArgs)
	is		
		variable num: integer;
	begin		

	end procedure;

--------------------------





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

function w2hex(w: Word) return string is
    constant letters: string(1 to 16) := "0123456789abcdef";
    variable res: string(1 to 8) := (others => '0');
begin
    for i in 7 downto 0 loop
        res(8-i) := letters(slv2u(w(4*i+3 downto 4*i)) + 1);
    end loop;
    return res;
end function;

function strExt(str: string; n: positive) return string is 
    variable res: string(1 to n) := (others => ' ');
begin
    for i in 1 to str'length loop
        if i > n then
            exit;
        end if;
        res(i) := str(i);
    end loop;
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
    
    -- Length 35
    return strExt(ExecFunc'image(ins.operation.func), 9) & "  " &
     dest & ", " &
     src0 & ", " &
     src1 & ", #" &
     w2hex(ins.constantArgs.imm);    
    
    --return "";
end function;

end package body;