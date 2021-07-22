
------------------------------------------------------------------------------------------
-- Computations as visible to the programmer
------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;


package Arith is

function bitLogicalShiftImm(a: Mword; b: Word) return Mword;
function bitLogicalShift(a, b: Mword) return Mword;

function bitArithmeticShiftImm(a: Mword; b: Word) return Mword;
function bitArithmeticShift(a, b: Mword) return Mword;


function rotate64(a: Dword; b: slv6) return Dword;
function clearRight64(a: Dword; b: slv6) return Dword;
function fillLeft64(a: Dword; b: slv6; val: std_logic) return Dword;

function addExtNew(a, b: Word; carryIn: std_logic) return Word;

end Arith;


package body Arith is

-- Shift by imm
function bitLogicalShiftImm(a: Mword; b: Word) return Mword is
    variable res: Mword := (others => '0');
    constant sh: integer := slv2s(b);
    variable leftIn, rightIn, leftOut, rightOut: natural := 0;
begin
    
    if sh < 1-MWORD_SIZE  or sh > MWORD_SIZE-1 then
        res := (others => '0');
        return res;
    elsif sh < 0 then -- dir right?
        leftIn := MWORD_SIZE-1;
        rightIn := -sh;
        
        leftOut := MWORD_SIZE-1 + sh; 
        rightOut := 0;
    else -- dir left?
        leftIn := MWORD_SIZE-1 - sh;
        rightIn := 0;
        
        leftOut := MWORD_SIZE-1;
        rightOut := sh;
    end if;
    
    res(leftOut downto rightOut) := a(leftIn downto rightIn);    
    return res;
end function;

-- Shift by reg
-- TMP: just copy of *Imm version
function bitLogicalShift(a, b: Mword) return Mword is
    variable res: Mword := (others => '0');
    constant sh: integer := slv2s(b);
    variable leftIn, rightIn, leftOut, rightOut: natural := 0;
begin
    
    if sh < 1-MWORD_SIZE  or sh > MWORD_SIZE-1 then
        res := (others => '0');
        return res;
    elsif sh < 0 then -- dir right?
        leftIn := MWORD_SIZE-1;
        rightIn := -sh;
        
        leftOut := MWORD_SIZE-1 + sh; 
        rightOut := 0;
    else -- dir left?
        leftIn := MWORD_SIZE-1 - sh;
        rightIn := 0;
        
        leftOut := MWORD_SIZE-1;
        rightOut := sh;
    end if;
    
    res(leftOut downto rightOut) := a(leftIn downto rightIn);    
    return res;
end function;


-- Shift by imm
function bitArithmeticShiftImm(a: Mword; b: Word) return Mword is
    variable res: Mword := (others => b(31));
    constant sh: integer := slv2s(b);
    variable leftIn, rightIn, leftOut, rightOut: natural := 0;
begin
    
    if sh < 1-MWORD_SIZE  or sh > MWORD_SIZE-1 then
        res := (others => '0');
        return res;
    elsif sh < 0 then -- dir right?
        leftIn := MWORD_SIZE-1;
        rightIn := -sh;
        
        leftOut := MWORD_SIZE-1 + sh; 
        rightOut := 0;
    else -- dir left?
        leftIn := MWORD_SIZE-1 - sh;
        rightIn := 0;
        
        leftOut := MWORD_SIZE-1;
        rightOut := sh;
    end if;
    
    res(leftOut downto rightOut) := a(leftIn downto rightIn);    
    return res;
end function;


-- Shift by reg
-- TMP: just copy of *Imm version
function bitArithmeticShift(a, b: Mword) return Mword is
    variable res: Mword := (others => b(MWORD_SIZE-1));
    constant sh: integer := slv2s(b);
    variable leftIn, rightIn, leftOut, rightOut: natural := 0;
begin
    
    if sh < 1-MWORD_SIZE  or sh > MWORD_SIZE-1 then
        res := (others => '0');
        return res;
    elsif sh < 0 then -- dir right?
        leftIn := MWORD_SIZE-1;
        rightIn := -sh;
        
        leftOut := MWORD_SIZE-1 + sh; 
        rightOut := 0;
    else -- dir left?
        leftIn := MWORD_SIZE-1 - sh;
        rightIn := 0;
        
        leftOut := MWORD_SIZE-1;
        rightOut := sh;
    end if;
    
    res(leftOut downto rightOut) := a(leftIn downto rightIn);    
    return res;
end function;


function rotate64(a: Dword; b: slv6) return Dword is
	variable res, res16, res4: Dword := (others => '0');
begin
	-- Rotate each 16
	case b(5 downto 4) is
		when "00" =>
			res16 := a;
		when "01" =>
			res16 := a(47 downto 0) & a(63 downto 48);
		when "10" =>
			res16 := a(31 downto 0) & a(63 downto 32);			
		when others =>
			res16 := a(15 downto 0) & a(63 downto 16);
	end case;

	-- Rotate each 4
	case b(3 downto 2) is
		when "00" =>
			res4 := res16;
		when "01" =>
			res4 := res16(59 downto 0) & res16(63 downto 60);
		when "10" =>
			res4 := res16(55 downto 0) & res16(63 downto 56);			
		when others =>
			res4 := res16(51 downto 0) & res16(63 downto 52);
	end case;

	-- Rotate each 1;
	case b(1 downto 0) is
		when "00" =>
			res := res4;
		when "01" =>
			res := res4(62 downto 0) & res4(63 downto 63);
		when "10" =>
			res := res4(61 downto 0) & res4(63 downto 62);			
		when others =>
			res := res4(60 downto 0) & res4(63 downto 61);
	end case;
	
	return res;
end function;


function clearRight64(a: Dword; b: slv6) return Dword is
	variable res: Dword := a;
begin
	for i in 63 downto 0 loop
		if i < slv2u(b) then
			res(i) := '0';
		end if;
	end loop;
	return res;
end function;

function fillLeft64(a: Dword; b: slv6; val: std_logic) return Dword is
	variable res: Dword := a;
begin
	for i in 63 downto 0 loop
		if i + slv2u(b) > 63 then
			res(i) := val;
		end if;
	end loop;
	return res;
end function;



-- Accepts carry input and returns 1-bit longer result for carry output 
function addExtNew(a, b: Word; carryIn: std_logic) return Word is
	variable res: Word := (others => '0');
	variable ra, rb, rc: std_logic_vector(a'length+1 downto 0) := (others => '0');	
begin
    ra(a'length downto 1) := a;
    rb(a'length downto 1) := b;

    ra(0) := carryIn;
    rb(0) := carryIn;

	rc := add(ra, rb);
	res := rc(a'length+1 downto 1);
	return res;
end function;

end Arith;
