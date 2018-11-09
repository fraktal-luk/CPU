----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 22:45:51
-- Design Name: 
-- Module Name: Helpers - Behavioral
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


package Helpers is

function std2bool(arg: std_logic) return boolean;
function bool2std(b: boolean) return std_logic;

	function addWordE(a, b: Word) return std_logic_vector;
	function addWord(a, b: Word) return word;
	function addDword(a, b: Word) return dword;


-- Get '1' bits for remaining slots, pushed to left
function pushToLeft(fullSlots, freedSlots: std_logic_vector) return std_logic_vector;

function countOnes(vec: std_logic_vector) return natural;
function setToOnes(vec: std_logic_vector; n: natural) return std_logic_vector;

-- Leaves first continuous seq of ones, clears the rest
function firstGroupOfOnes(vec: std_logic_vector) return std_logic_vector;

-- Sets all bits starting from the position of first '1'
function setFromFirstOne(vec: std_logic_vector) return std_logic_vector;
-- Count leading zeroes of course
function clz(vec: std_logic_vector) return natural;
-- Shift left so that all leading zeroes are out
function alignLeft(vec: std_logic_vector) return std_logic_vector;

function findByNumber(numVec: IntArray; seeking: integer) return std_logic_vector;

-- Finding first position of qualified ones
-- Those 2 functions must work together: if returned slv at returned index is '0', then OFC it means "nothing" 
function getFirstOne(readySlots: std_logic_vector) return std_logic_vector;
function getFirstOnePosition(readySlots: std_logic_vector) return integer;

function isNonzero(vec: std_logic_vector) return std_logic;

function invertVec(vec: std_logic_vector) return std_logic_vector;

end Helpers;


package body Helpers is

function std2bool(arg: std_logic) return boolean is
begin
	return (arg = '1');
end function;

function bool2std(b: boolean) return std_logic is
begin
	if b then
		return '1';
	else
		return '0';
	end if;
end function;

		function addWordE(a, b: word) return std_logic_vector is
			variable res: std_logic_vector(32 downto 0) := (others => '0');
			variable al, ah, bl, bh, cl, ch: integer := 0;
			variable ta, tb: hword := (others => '0'); 
		begin
			ta := a(31 downto 16);
			ah := slv2u(ta);
			al := slv2u(a(15 downto 0));
			tb := b(31 downto 16);
			bh := slv2u(tb);
			bl := slv2u(b(15 downto 0));
			
			cl := bl + al;
			ch := ah + bh;
			
			if cl >= 2**16 then
				ch := ch + 1;
			end if;
			
			res(31 downto 16) := i2slv(ch, 16);
			res(15 downto 0) := i2slv(cl, 16);

			if ch >= 2**16 then
				res(32) := '1';
			end if;
			
			return res;
		end function;


		function addWord(a, b: word) return word is
			variable res: word := (others => '0');
			variable tmp: std_logic_vector(32 downto 0) := (others => '0');
		begin
			tmp := addWordE(a, b);
			res := tmp(31 downto 0); 
			return res;
		end function;

		function addDword(a, b: dword) return dword is
			variable res: dword := (others => '0');
			variable tmp: std_logic_vector(32 downto 0) := (others => '0');
			variable aw, bw, cw: word := (others => '0');
		begin
			tmp := addWordE(a(31 downto 0), b(31 downto 0));
			res(32 downto 0) := tmp; -- Includes carry form lower word! 
			cw := res(63 downto 32);
			aw := a(63 downto 32);
			bw := b(63 downto 32);
			tmp := addWordE(aw, bw);
			res(63 downto 32) := addWord(tmp(31 downto 0), cw);
			return res;
		end function;


function pushToLeft(fullSlots, freedSlots: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(fullSlots'range) := (others => '0');
	variable remaining: std_logic_vector(fullSlots'range) := fullSlots and not freedSlots;
	variable k: integer := 0;
begin
	for i in remaining'range loop	
		if remaining(i) = '1' then
			res(k) := '1';
			k := k+1;
		end if;
	end loop;
	return res;
end function;

function countOnes(vec: std_logic_vector) return natural is
	variable sum: natural := 0;
begin
	for i in vec'range loop
		if vec(i) = '1' then
			sum := sum + 1;
		end if;
	end loop;
	return sum;
end function;


function setToOnes(vec: std_logic_vector; n: natural) return std_logic_vector is
	variable res: std_logic_vector(vec'range) := (others=>'0');
	variable b: natural := n;
begin
	if n > vec'length then
		return res;
	end if;
	
	for i in vec'range loop -- 0 to n-1 loop
		if i >= n then
			exit;
		end if;
		res(i) := '1';
	end loop;	
		
	return res;
end function;



function firstGroupOfOnes(vec: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(vec'range) := (others=>'0');
	variable beforeOnes: boolean := true;
	variable afterOnes: boolean := false;	
begin
		for i in vec'range loop
			if beforeOnes then
				-- wait until first '1' appears
				if vec(i) = '1' then
								-- CAREFUL! This detail (wantSend instead of conj, just in this expression)
								--				makes the diff from WRONG version 
					beforeOnes := false;
				end if;
			end if;
			-- If we're already inside seq of '1's, go on until they end	
			if not beforeOnes then
				if vec(i) = '0' then
					afterOnes := true;
				end if;
			end if;
			
			-- If it's after the 1st seq of ones, we put zeros; otherwise copy 'conj'
			if afterOnes then
				res(i) := '0';
			else	
				res(i) := vec(i);
			end if;
		end loop;	
	
	return res;
end function;



function setFromFirstOne(vec: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(vec'range) := (others=>'0');
	variable beforeOnes: boolean := true;
	--variable afterOnes: boolean := false;	
begin
		for i in vec'range loop
			if beforeOnes then
				-- wait until first '1' appears
				if vec(i) = '1' then
								-- CAREFUL! This detail (wantSend instead of conj, just in this expression)
								--				makes the diff from WRONG version 
					beforeOnes := false;
				end if;
			end if;
			-- If we're already inside seq of '1's, go on until they end	
			if not beforeOnes then
				res(i) := '1';
			end if;
		end loop;	
	
	return res;
end function;


function clz(vec: std_logic_vector) return natural is
	variable res: natural := 0;
begin
	for i in vec'range loop
		if vec(i) = '0' then
			res := res+1;
		else
			exit;
		end if;
	end loop;
	return res;
end function;

function alignLeft(vec: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(vec'range) := (others=>'0');
	variable nlz: natural := clz(vec);
begin 
--			report integer'image(nlz);
	for i in vec'left to vec'right - nlz loop
		res(i) := vec(i + nlz);
	end loop;
	return res;
end function;


function findByNumber(numVec: IntArray; seeking: integer) return std_logic_vector is
	variable res: std_logic_vector(numVec'range) := (others=>'0');
begin
	for i in numVec'range loop
		if numVec(i) = seeking then
			res(i) := '1';
		end if;
	end loop;
	return res;
end function;


function getFirstOne(readySlots: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(readySlots'range) := (others=>'0');
begin
	for i in readySlots'range loop
		if readySlots(i) = '1' then
			res(i) := '1';
			exit;
		end if;
	end loop;
	return res;
end function;

function getFirstOnePosition(readySlots: std_logic_vector) return integer is
	variable res: integer := -1;
begin
	for i in readySlots'range loop
		if readySlots(i) = '1' then
			res := i;
			exit;
		end if;
	end loop;
	return res;
end function;


function isNonzero(vec: std_logic_vector) return std_logic is
begin
	for i in vec'range loop
		if vec(i) = '1' then
			return '1';
		end if;
	end loop;
	return '0';
end function;


function invertVec(vec: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(vec'range) := (others => '0');
	constant LEN: integer := vec'length;
	constant LLIMIT: integer := vec'left;
	constant RLIMIT: integer := res'right; 
begin
	for i in 0 to LEN-1 loop
		res(RLIMIT - i) := vec(LLIMIT + i);
	end loop;
	return res;
end function;


end Helpers;
