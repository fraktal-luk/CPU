
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_arith.all;


package BasicTypes is

-- Typical data sizes
subtype  Byte is std_logic_vector( 7 downto 0);
subtype Hword is std_logic_vector(15 downto 0);
subtype  Word is std_logic_vector(31 downto 0);
subtype Dword is std_logic_vector(63 downto 0);
subtype Qword is std_logic_vector(127 downto 0);

-- Special dataSizes
subtype slv5  is std_logic_vector(4 downto 0);
subtype slv6  is std_logic_vector(5 downto 0);
subtype slv10 is std_logic_vector(9 downto 0);
subtype slv21 is std_logic_vector(20 downto 0);
subtype slv26 is std_logic_vector(25 downto 0);	
 

type ByteArray is array(integer range <>) of byte;
type HwordArray is array(integer range <>) of hword;
type WordArray is array(integer range <>) of word;
type DwordArray is array(integer range <>) of dword;

type IntArray is array (integer range <>) of integer;

function slv2s(v: std_logic_vector) return integer;
function slv2u(v: std_logic_vector) return natural;
function i2slv(val: integer; n: natural) return std_logic_vector;


subtype SmallNumber is byte;
type SmallNumberArray is array(integer range <>) of SmallNumber;
constant SMALL_NUMBER_SIZE: natural := SmallNumber'length;

function cmpGreaterThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpGreaterThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpLessThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpLessThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpEqualToSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;

function add(a: std_logic_vector; b: std_logic_vector) return std_logic_vector;
function sub(a: std_logic_vector; b: std_logic_vector) return std_logic_vector;

-- Accepts carry input and returns 1-bit longer result for carry output 
function addExt(a: std_logic_vector; b: std_logic_vector; carryIn: std_logic) return std_logic_vector;
function subExt(a: std_logic_vector; b: std_logic_vector; carryIn: std_logic) return std_logic_vector;


-- Some arithmetic for SmallNumber
alias addSN is add[std_logic_vector, std_logic_vector return std_logic_vector];
alias subSN is sub[std_logic_vector, std_logic_vector return std_logic_vector];


procedure CHECK_BE(v: std_logic_vector);
function zeroExtend(a: std_logic_vector; n: natural) return std_logic_vector;
function signExtend(a: std_logic_vector; n: natural) return std_logic_vector;

function std2bool(arg: std_logic) return boolean;
function bool2std(b: boolean) return std_logic;

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

-- Finding first position of qualified ones
-- Those 2 functions must work together: if returned slv at returned index is '0', then OFC it means "nothing" 
function getFirstOne(readySlots: std_logic_vector) return std_logic_vector;
function getFirstOnePosition(readySlots: std_logic_vector) return integer;
function getLastOnePosition(readySlots: std_logic_vector) return integer;

function isNonzero(vec: std_logic_vector) return std_logic;

function invertVec(vec: std_logic_vector) return std_logic_vector;

function addInt(v: std_logic_vector; n: integer) return std_logic_vector;
function addIntTrunc(v: std_logic_vector; n: integer; len: natural) return std_logic_vector;


function cmpLtU(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpLtS(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpLeU(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpLeS(a: std_logic_vector; b: std_logic_vector) return std_logic;

-- Less [or eq]
function cmpLtU(a: std_logic_vector; b: natural) return std_logic;
function cmpLtS(a: std_logic_vector; b: integer) return std_logic;
function cmpLeU(a: std_logic_vector; b: natural) return std_logic;
function cmpLeS(a: std_logic_vector; b: integer) return std_logic;


-- Greater [or eq]
function cmpGtU(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpGtS(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpGeU(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpGeS(a: std_logic_vector; b: std_logic_vector) return std_logic;

-- With int
function cmpGtU(a: std_logic_vector; b: natural) return std_logic;
function cmpGtS(a: std_logic_vector; b: integer) return std_logic;
function cmpGeU(a: std_logic_vector; b: natural) return std_logic;
function cmpGeS(a: std_logic_vector; b: integer) return std_logic;

function cmpEqU(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpEqS(a: std_logic_vector; b: std_logic_vector) return std_logic;
function cmpNeU(a: std_logic_vector; b: std_logic_vector) return std_logic;    
function cmpNeS(a: std_logic_vector; b: std_logic_vector) return std_logic;

function cmpEqU(a: std_logic_vector; b: natural) return std_logic;
function cmpEqS(a: std_logic_vector; b: integer) return std_logic;
function cmpNeU(a: std_logic_vector; b: natural) return std_logic;  
function cmpNeS(a: std_logic_vector; b: integer) return std_logic;

function addTruncZ(a: std_logic_vector; b: std_logic_vector; n: natural) return std_logic_vector;
function subTruncZ(a: std_logic_vector; b: std_logic_vector; n: natural) return std_logic_vector;

end package;


package body BasicTypes is

function slv2s(v: std_logic_vector) return integer is
    variable accum: integer := 0;
    variable bitVal: natural;
begin
        if v(v'high) = '1' then
            accum := -1;
        else
            accum := 0;
        end if;
    
    for i in v'high-1 downto v'low loop
        if v(i) = '1' then
            bitVal := 1;
        else
            bitVal := 0;
        end if;
        
        accum := 2*accum + bitVal;
    end loop;
    
    return accum;
end function;

function slv2u(v: std_logic_vector) return natural is
    variable accum: natural := 0;
    variable bitVal: natural;
begin
    for i in v'high downto v'low loop
        if v(i) = '1' then
            bitVal := 1;
        else
            bitVal := 0;
        end if;
        
        accum := 2*accum + bitVal;
    end loop;
    
    return accum;
end function;	

----------------------------------------------------------------------
--  Below: copied from numeric_std!
----------------------------------------------------------------
function TO_UNSIGNED(ARG,SIZE: NATURAL) return std_logic_vector is
  variable RESULT: std_logic_vector (SIZE-1 downto 0) ;
  variable i_val:natural := ARG;
  begin
--  if (SIZE < 1) then return NAU; end if;
  for i in 0 to RESULT'left loop
    if (i_val MOD 2) = 0 then
       RESULT(i) := '0';
    else RESULT(i) := '1' ;
      end if;
    i_val := i_val/2 ;
    end loop;
  if not(i_val=0) then
--    assert NO_WARNING 
--    report "numeric_std.TO_UNSIGNED : vector truncated"
--		severity WARNING ;
    end if;
  return RESULT ;
  end TO_UNSIGNED;

     -- Id: D.4
function TO_SIGNED(ARG: INTEGER; SIZE: NATURAL) return std_logic_vector is
  variable RESULT: std_logic_vector (SIZE-1 downto 0) ;
  variable b_val : STD_LOGIC:= '0' ;
  variable i_val : INTEGER:= ARG ;
  begin
--  if (SIZE < 1) then return NAS; end if;
  if (ARG<0) then
    b_val := '1' ;
    i_val := -(ARG+1) ;
    end if ;
  for i in 0 to RESULT'left loop
    if (i_val MOD 2) = 0 then
      RESULT(i) := b_val;
    else 
      RESULT(i) := not b_val ;
      end if;
    i_val := i_val/2 ;
    end loop;
  if ((i_val/=0) or (b_val/=RESULT(RESULT'left))) then
--    assert NO_WARNING 
--    report "numeric_std.TO_SIGNED : vector truncated"
--    severity WARNING ;
    end if;
  return RESULT;
  end TO_SIGNED;
------------------------------------

	function i2slv(val: integer; n: natural) return std_logic_vector is
		variable num: integer := val;
		variable v: std_logic_vector(n-1 downto 0);
		variable res: std_logic_vector(n-1 downto 0);
		variable bitVal: natural;
		--variable newBit: std_logic;
	begin
			--	report "i 2 vec";
									--	report " ";
		return to_signed(val, n);
	end function;


function cmpGreaterThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := --cmpGreaterUnsignedSN(arr(i), num);
		          cmpGtU(arr(i), num);
	end loop;
	return res;
end function;

function cmpGreaterThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := --cmpGreaterSignedSN(arr(i), num);
		          cmpGtS(arr(i), num);
	end loop;
	return res;
end function;

function cmpLessThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := --cmpLessUnsignedSN(arr(i), num);
		          cmpLtU(arr(i), num);
	end loop;
	return res;
end function;

function cmpLessThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := --cmpLessSignedSN(arr(i), num);
		          cmpLtS(arr(i), num);
	end loop;
	return res;
end function;

function cmpEqualToSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	constant LEN: integer := arr'length;
	variable res: std_logic_vector(0 to LEN-1) := (others => '0');
	variable sn: SmallNumber := (others => '0');
begin
	for i in 0 to LEN-1 loop
		res(i) := cmpEqU(arr(i), num);
	end loop;
	return res;
end function;



procedure CHECK_BE(v: std_logic_vector) is
begin
   assert not v'ascending report "Trying to use little endian slv as number!" severity error;
end procedure;



function zeroExtend(a: std_logic_vector; n: natural) return std_logic_vector is
    constant LEN: natural := a'length;
    variable res: std_logic_vector(n-1 downto 0) := (others => '0');
begin
    CHECK_BE(a);

    if n < LEN then
        res := a(n-1 downto 0);
    else
        res(LEN-1 downto 0) := a;
    end if;
    
    return res;
end function; 


function signExtend(a: std_logic_vector; n: natural) return std_logic_vector is
    constant LEN: natural := a'length;
    variable res: std_logic_vector(n-1 downto 0) := (others => '0');
begin
    CHECK_BE(a);

    if n < LEN then
        res := a(n-1 downto 0);
    else
        res(LEN-1 downto 0) := a;
        res(n-1 downto LEN) := (others => a(LEN-1));
    end if;
    
    return res;
end function; 


function add(a: std_logic_vector; b: std_logic_vector) return std_logic_vector is
    constant LEN: natural := a'length;
    variable bRes, res: std_logic_vector(LEN-1 downto 0) := (others => '0');
    variable ua, ub, ur: unsigned(LEN-1 downto 0) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    bRes := zeroExtend(b, LEN);
    ua := unsigned(a);
    ub := unsigned(b);
    ur := ua + ub;
    res := std_logic_vector(ur);
    return res;
end function;

function sub(a: std_logic_vector; b: std_logic_vector) return std_logic_vector is
    constant LEN: natural := a'length;
    variable bRes, res: std_logic_vector(LEN-1 downto 0) := (others => '0');
    variable ua, ub, ur: unsigned(LEN-1 downto 0) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
        
    bRes := zeroExtend(b, LEN);
    ua := unsigned(a);
    ub := unsigned(b);
    ur := ua - ub;
    res := std_logic_vector(ur);
    return res;
end function;

-- Accepts carry input and returns 1-bit longer result for carry output 
function addExt(a: std_logic_vector; b: std_logic_vector; carryIn: std_logic) return std_logic_vector is
	variable res: std_logic_vector(a'length downto 0) := (others => '0');
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


-- Accepts carry input and returns 1-bit longer result for carry output 
function subExt(a: std_logic_vector; b: std_logic_vector; carryIn: std_logic) return std_logic_vector is
	variable res: std_logic_vector(a'length downto 0) := (others => '0');
	variable ra, rb, rc: std_logic_vector(a'length+1 downto 0) := (others => '0');	
begin
    ra(a'length downto 1) := a;
    rb(a'length downto 1) := b;

    ra(0) := carryIn;
    rb(0) := carryIn;

	rc := sub(ra, rb);
	res := rc(a'length+1 downto 1);
	return res;
end function;



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
	for i in vec'left to vec'right - nlz loop
		res(i) := vec(i + nlz);
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

function getLastOnePosition(readySlots: std_logic_vector) return integer is
	variable res: integer := -1;
begin
	for i in readySlots'reverse_range loop
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





function addTruncZ(a: std_logic_vector; b: std_logic_vector; n: natural) return std_logic_vector is
    variable res0, res: std_logic_vector(a'range) := (others => '0');
    variable bRes: std_logic_vector(a'range) := (others => '0');       
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    bRes := zeroExtend(b, a'length);
    
    res0 := add(a, b);

    res(n-1 downto 0) := res0(n-1 downto 0);

    return res;
end function;

function subTruncZ(a: std_logic_vector; b: std_logic_vector; n: natural) return std_logic_vector is
    variable res0, res: std_logic_vector(a'range) := (others => '0');
    variable bRes: std_logic_vector(a'range) := (others => '0');       
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    bRes := zeroExtend(b, a'length);
    
    res0 := sub(a, b);

    res(n-1 downto 0) := res0(n-1 downto 0);

    return res;
end function;



function addInt(v: std_logic_vector; n: integer) return std_logic_vector is
    variable res: std_logic_vector(v'range) := (others => '0');
    variable vInt: integer := slv2u(v); -- Signed or not, addition bit results are the same
    variable bRes: std_logic_vector(v'range) := (others => '0'); 
begin
    CHECK_BE(v);
    bRes := i2slv(n, v'length);
    
    res := add(v, bRes);--i2slv(vInt + n, v'length);
    return res;
end function;

function addIntTrunc(v: std_logic_vector; n: integer; len: natural) return std_logic_vector is
    variable res0, res: std_logic_vector(v'range) := (others => '0');
    variable bRes: std_logic_vector(v'range) := (others => '0'); 
begin
    CHECK_BE(v);
    bRes := i2slv(n, v'length);
    res0 := add(v, bRes);--i2slv(vInt + n, v'length);
    res(len-1 downto 0) := res0(len-1 downto 0);
    return res;
end function;



-- This is the internal implementation
function baseCompareLessU(a: std_logic_vector; b: std_logic_vector; orEq: boolean) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := b;
begin
    
	for i in LEN-1 downto 0 loop
        if a(i) = '1' and bRes(i) = '0' then
            return '0';
        elsif a(i) = '0' and bRes(i) = '1' then
            return '1';
        end if;
    end loop;
    
    return bool2std(orEq);
end function;


-- Less [or eq]
function cmpLtU(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    bRes := zeroExtend(b, LEN);
    return baseCompareLessU(a, bRes, false);
end function;

function cmpLtS(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    aRes := a;
    bRes := signExtend(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(aRes, bRes, false);
end function;

function cmpLeU(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    bRes := zeroExtend(b, LEN);
    return baseCompareLessU(a, bRes, true);
end function;

function cmpLeS(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    aRes := a;
    bRes := signExtend(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(aRes, bRes, true);
end function;

--- With int

-- Less [or eq]
function cmpLtU(a: std_logic_vector; b: natural) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    bRes := i2slv(b, LEN);
    return baseCompareLessU(a, bRes, false);
end function;

function cmpLtS(a: std_logic_vector; b: integer) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    aRes := a;
    bRes := i2slv(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(aRes, bRes, false);
end function;

function cmpLeU(a: std_logic_vector; b: natural) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    bRes := i2slv(b, LEN);
    return baseCompareLessU(a, bRes, true);
end function;

function cmpLeS(a: std_logic_vector; b: integer) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    aRes := a;
    bRes := i2slv(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(aRes, bRes, true);
end function;




-- Greater [or eq]
function cmpGtU(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    bRes := zeroExtend(b, LEN);
    return baseCompareLessU(bRes, a, false);
end function;

function cmpGtS(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    aRes := a;
    bRes := signExtend(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(bRes, aRes, false);
end function;

function cmpGeU(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    bRes := zeroExtend(b, LEN);
    return baseCompareLessU(bRes, a, true);
end function;

function cmpGeS(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);
    
    aRes := a;
    bRes := signExtend(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(bRes, aRes, true);
end function;

-- With int
function cmpGtU(a: std_logic_vector; b: natural) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    bRes := i2slv(b, LEN);
    return baseCompareLessU(bRes, a, false);
end function;

function cmpGtS(a: std_logic_vector; b: integer) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    aRes := a;
    bRes := i2slv(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(bRes, aRes, false);
end function;

function cmpGeU(a: std_logic_vector; b: natural) return std_logic is
    constant LEN: natural := a'length;
    variable bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    bRes := i2slv(b, LEN);
    return baseCompareLessU(bRes, a, true);
end function;

function cmpGeS(a: std_logic_vector; b: integer) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);
    
    aRes := a;
    bRes := i2slv(b, LEN);
    aRes(LEN-1) := not aRes(LEN-1); -- Invert sign bit to allow using unsigned comparison
    bRes(LEN-1) := not bRes(LEN-1);
    return baseCompareLessU(bRes, aRes, true);
end function;



-- Equal/not equal
function cmpEqU(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);

    bRes := zeroExtend(b, LEN);
    
    return bool2std(a = bRes);
end function;    
    
function cmpEqS(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);

    bRes := signExtend(b, LEN);
    
    return bool2std(a = bRes);
end function;

function cmpNeU(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);

    bRes := zeroExtend(b, LEN);
    
    return bool2std(a /= bRes);
end function;    
    
function cmpNeS(a: std_logic_vector; b: std_logic_vector) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    CHECK_BE(b);

    bRes := signExtend(b, LEN);
    
    return bool2std(a /= bRes);
end function;

-- With integers
function cmpEqU(a: std_logic_vector; b: natural) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);

    bRes := i2slv(b, LEN);
    
    return bool2std(a = bRes);
end function;    
    
function cmpEqS(a: std_logic_vector; b: integer) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);

    bRes := i2slv(b, LEN);
    
    return bool2std(a = bRes);
end function;

function cmpNeU(a: std_logic_vector; b: natural) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);

    bRes := i2slv(b, LEN);
    
    return bool2std(a /= bRes);
end function;    
    
function cmpNeS(a: std_logic_vector; b: integer) return std_logic is
    constant LEN: natural := a'length;
    variable aRes, bRes: std_logic_vector(a'range) := (others => '0');
begin
    CHECK_BE(a);
    --CHECK_BE(b);

    bRes := i2slv(b, LEN);
    
    return bool2std(a /= bRes);
end function;




end package body;
