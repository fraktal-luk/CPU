
------------------------------------------------------------------------------------------
-- Computations implemented specifically for the microarchitecture 
------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;


package Arith is

-- TODO: should be moved somewhere else, but depends on Mword definition.
--			Because Mword is device specific, these functions must be redone as abstractions of addWord...
function addMwordBasic(a, b: Mword) return Mword;
function subMwordBasic(a, b: Mword) return Mword;

function addMwordExt(a, b: Mword) return std_logic_vector;
function subMwordExt(a, b: Mword) return std_logic_vector;

function addMwordFaster(a, b: Mword) return Mword;

function addMwordFasterExt(a, b: Mword; carryIn: std_logic) return std_logic_vector;

end Arith;



package body Arith is


function addMwordBasic(a, b: Mword) return Mword is
	variable res: Mword := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
	for i in 0 to MWORD_SIZE-1 loop
		rdigit := a(i) xor b(i) xor carry;
		carry := (a(i) and b(i)) or (a(i) and carry) or (b(i) and carry);
		res(i) := rdigit;
	end loop;
	
	   res := add(a, b);
	return res;
end function;

function subMwordBasic(a, b: Mword) return Mword is
	variable res: Mword := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
	carry := '1';
	for i in 0 to MWORD_SIZE-1 loop
		rdigit := a(i) xor (not b(i)) xor carry;
		carry := (a(i) and not b(i)) or (a(i) and carry) or ((not b(i)) and carry);
		res(i) := rdigit;
	end loop;
	
	   res := sub(a,b);
	return res;
end function;

function addMwordExt(a, b: Mword) return std_logic_vector is
	variable res, ra, rb: std_logic_vector(MWORD_SIZE downto 0) := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
    ra := signExtend(a, MWORD_SIZE+1);
    rb := signExtend(b, MWORD_SIZE+1);

	for i in 0 to MWORD_SIZE-1 loop
		rdigit := a(i) xor b(i) xor carry;
		carry := (a(i) and b(i)) or (a(i) and carry) or (b(i) and carry);
		res(i) := rdigit;
	end loop;
	res(MWORD_SIZE) := carry;
	
	   res := add(ra, rb);
	
	return res;
end function;

function subMwordExt(a, b: Mword) return std_logic_vector is
	variable res, ra, rb: std_logic_vector(MWORD_SIZE downto 0) := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
    ra := signExtend(a, MWORD_SIZE+1);
    rb := signExtend(b, MWORD_SIZE+1);

	carry := '1';
	for i in 0 to MWORD_SIZE-1 loop
		rdigit := a(i) xor (not b(i)) xor carry;
		carry := (a(i) and not b(i)) or (a(i) and carry) or ((not b(i)) and carry);
		res(i) := rdigit;
	end loop;
	res(MWORD_SIZE) := carry;
	
	   res := sub(a, b);
	return res;
end function;


	-- CAREFUL: assuming SMALL_NUMBER_SIZE = 8
	function addExt8(a, b: SmallNumber; cIn: std_logic) return std_logic_vector is
		variable res: std_logic_vector(8 downto 0) := (others => '0');
		variable rdigit, carry: std_logic := '0';
	begin
		carry := cIn;
		for i in 0 to 8-1 loop
			rdigit := a(i) xor b(i) xor carry;
			carry := (a(i) and b(i)) or (a(i) and carry) or (b(i) and carry);
			res(i) := rdigit;
		end loop;
		res(8) := carry;	
		return res;
	end function;


function addMwordFasterExt(a, b: Mword; carryIn: std_logic) return std_logic_vector is
	variable res: std_logic_vector(32 downto 0) := (others => '0'); -- CAREFUL, TODO: 32b only!
	variable rdigit, carry: std_logic := '0';
	variable partial0N, partial1N, partial2N, partial3N, partial0C, partial1C, partial2C, partial3C:
		std_logic_vector(8 downto 0) := (others => '0');
	variable c7, c15, c23, c31: std_logic := '0';
	   variable ra, rb, rc: std_logic_vector(a'length+1 downto 0) := (others => '0');	
begin
        ra(a'length downto 1) := a;
        rb(a'length downto 1) := b;

        ra(0) := carryIn;
        rb(0) := carryIn;
        
	-- Carry select, for 32b

	partial0N := addExt8(a(7 downto 0), b(7 downto 0), carryIn);
	partial0C := addExt8(a(7 downto 0), b(7 downto 0), '1');
	partial1N := addExt8(a(15 downto 8), b(15 downto 8), '0');
	partial1C := addExt8(a(15 downto 8), b(15 downto 8), '1');
	partial2N := addExt8(a(23 downto 16), b(23 downto 16), '0');
	partial2C := addExt8(a(23 downto 16), b(23 downto 16), '1');
	partial3N := addExt8(a(31 downto 24), b(31 downto 24), '0');
	partial3C := addExt8(a(31 downto 24), b(31 downto 24), '1');

	-- Carry chain, selection
	c7 := partial0N(8);
	c15 := partial1N(8) or (partial1C(8) and c7);
	c23 := partial2N(8) or (partial2C(8) and c15);
	c31 := partial3N(8) or (partial3C(8) and c23);
	
	if c23 = '1' then
		res(31 downto 24) := partial3C(7 downto 0);
	else
		res(31 downto 24) := partial3N(7 downto 0);		
	end if;

	if c15 = '1' then
		res(23 downto 16) := partial2C(7 downto 0);
	else
		res(23 downto 16) := partial2N(7 downto 0);		
	end if;
	
	if c7 = '1' then
		res(15 downto 8) := partial1C(7 downto 0);
	else
		res(15 downto 8) := partial1N(7 downto 0);		
	end if;	

	--if c23 = '1' then
		res(7 downto 0) := partial0N(7 downto 0);
	--else
	--	res(31 downto 24) := partial3N(7 downto 0);		
	--end if;
	res(32) := c31;
	
	   rc := add(ra, rb);
	   res := rc(a'length+1 downto 1);
	return res;
end function;


function addMwordFaster(a, b: Mword) return Mword is
	variable res: Mword := (others => '0');
	variable rdigit, carry: std_logic := '0';
	variable partial0N, partial1N, partial2N, partial3N, partial0C, partial1C, partial2C, partial3C:
		std_logic_vector(8 downto 0) := (others => '0');
	variable c7, c15, c23, c31: std_logic := '0';	
begin
	-- Carry select, for 32b

	partial0N := addExt8(a(7 downto 0), b(7 downto 0), '0');
	partial0C := addExt8(a(7 downto 0), b(7 downto 0), '1');
	partial1N := addExt8(a(15 downto 8), b(15 downto 8), '0');
	partial1C := addExt8(a(15 downto 8), b(15 downto 8), '1');
	partial2N := addExt8(a(23 downto 16), b(23 downto 16), '0');
	partial2C := addExt8(a(23 downto 16), b(23 downto 16), '1');
	partial3N := addExt8(a(31 downto 24), b(31 downto 24), '0');
	partial3C := addExt8(a(31 downto 24), b(31 downto 24), '1');

	-- Carry chain, selection
	c7 := partial0N(8);
	c15 := partial1N(8) or (partial1C(8) and c7);
	c23 := partial2N(8) or (partial2C(8) and c15);
	c31 := partial3N(8) or (partial3C(8) and c23);
	
	if c23 = '1' then
		res(31 downto 24) := partial3C(7 downto 0);
	else
		res(31 downto 24) := partial3N(7 downto 0);		
	end if;

	if c15 = '1' then
		res(23 downto 16) := partial2C(7 downto 0);
	else
		res(23 downto 16) := partial2N(7 downto 0);		
	end if;
	
	if c7 = '1' then
		res(15 downto 8) := partial1C(7 downto 0);
	else
		res(15 downto 8) := partial1N(7 downto 0);		
	end if;	

	--if c23 = '1' then
		res(7 downto 0) := partial0N(7 downto 0);
	--else
	--	res(31 downto 24) := partial3N(7 downto 0);		
	--end if;
	       res := add(a, b);
	return res;
end function;

end Arith;
