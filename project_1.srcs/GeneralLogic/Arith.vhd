
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



end Arith;
