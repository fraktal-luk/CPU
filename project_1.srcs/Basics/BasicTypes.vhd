
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


-- Some arithmetic for SmallNumber
function addSN(a, b: SmallNumber) return SmallNumber;
function subSN(a, b: SmallNumber) return SmallNumber;

function uminSN(a, b: SmallNumber) return SmallNumber;
function sminSN(a, b: SmallNumber) return SmallNumber;

function cmpLessSignedSN(a: SmallNumber; b: SmallNumber) return std_logic;
function cmpGreaterSignedSN(a: SmallNumber; b: SmallNumber) return std_logic;
function cmpLessUnsignedSN(a: SmallNumber; b: SmallNumber) return std_logic;
function cmpGreaterUnsignedSN(a: SmallNumber; b: SmallNumber) return std_logic;


function cmpGreaterThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpGreaterThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpLessThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpLessThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;
function cmpEqualToSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector;

function add(a: std_logic_vector; b: std_logic_vector) return std_logic_vector;
function sub(a: std_logic_vector; b: std_logic_vector) return std_logic_vector;

-- Accepts carry input and returns 1-bit longer result for carry output 
function addExt(a: std_logic_vector; b: std_logic_vector; carryIn: std_logic) return std_logic_vector;


procedure CHECK_BE(v: std_logic_vector);
function zeroExtend(a: std_logic_vector; n: natural) return std_logic_vector;
function signExtend(a: std_logic_vector; n: natural) return std_logic_vector;


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


--------------------

function addSN(a, b: SmallNumber) return SmallNumber is
	variable res: SmallNumber := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
	for i in 0 to SMALL_NUMBER_SIZE-1 loop
		rdigit := a(i) xor b(i) xor carry;
		carry := (a(i) and b(i)) or (a(i) and carry) or (b(i) and carry);
		res(i) := rdigit;
	end loop;
	       res := add(a, b);
	
	return res;
end function;

function subSN(a, b: SmallNumber) return SmallNumber is
	variable res: SmallNumber := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
	carry := '1';
	for i in 0 to SMALL_NUMBER_SIZE-1 loop
		rdigit := a(i) xor (not b(i)) xor carry;
		carry := (a(i) and not b(i)) or (a(i) and carry) or ((not b(i)) and carry);
		res(i) := rdigit;
	end loop;
	
	       res := sub(a, b);
	return res;
end function;


function uminSN(a, b: SmallNumber) return SmallNumber is
	variable res: SmallNumber := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
	res := a;
	for i in SMALL_NUMBER_SIZE-1 downto 0 loop
		if a(i) = '0' and b(i) = '1' then
			res := a;
			exit;
		elsif a(i) = '1' and b(i) = '0' then 
			res := b;
			exit;
		else
			null;
		end if;
	end loop;
	return res;
end function;

function sminSN(a, b: SmallNumber) return SmallNumber is
	variable res: SmallNumber := (others => '0');
	variable rdigit, carry: std_logic := '0';
begin
	res := a;
	if a(SMALL_NUMBER_SIZE-1) = '1' and b(SMALL_NUMBER_SIZE-1) = '0' then
		res := a;
		return res;
	elsif a(SMALL_NUMBER_SIZE-1) = '0' and b(SMALL_NUMBER_SIZE-1) = '1' then
		res := b;
		return res;
	else
		null;
	end if;
	
	for i in SMALL_NUMBER_SIZE-2 downto 0 loop
		if a(i) = '0' and b(i) = '1' then
			res := a;
			exit;
		elsif a(i) = '1' and b(i) = '0' then 
			res := b;
			exit;
		else
			null;
		end if;
	end loop;
	return res;
end function;


function cmpLessSignedSN(a: SmallNumber; b: SmallNumber) return std_logic is
begin
		if a(SMALL_NUMBER_SIZE-1) = '1' and b(SMALL_NUMBER_SIZE-1) = '0' then
			return '1';
		elsif a(SMALL_NUMBER_SIZE-1) = '0' and b(SMALL_NUMBER_SIZE-1) = '1' then
			return '0';
		end if;		

	for i in SMALL_NUMBER_SIZE-2 downto 0 loop
		if a(i) = '0' and b(i) = '1' then
			return '1';
		elsif a(i) = '1' and b(i) = '0' then
			return '0';
		end if;
	end loop;
	return '0';
end function;

function cmpGreaterSignedSN(a: SmallNumber; b: SmallNumber) return std_logic is
begin
		if a(SMALL_NUMBER_SIZE-1) = '0' and b(SMALL_NUMBER_SIZE-1) = '1' then
			return '1';
		elsif a(SMALL_NUMBER_SIZE-1) = '1' and b(SMALL_NUMBER_SIZE-1) = '0' then
			return '0';
		end if;		

	for i in SMALL_NUMBER_SIZE-2 downto 0 loop
		if a(i) = '1' and b(i) = '0' then
			return '1';
		elsif a(i) = '0' and b(i) = '1' then
			return '0';
		end if;
	end loop;
	return '0';
end function;

function cmpLessUnsignedSN(a: SmallNumber; b: SmallNumber) return std_logic is
begin
	for i in SMALL_NUMBER_SIZE-1 downto 0 loop
		if a(i) = '1' and b(i) = '0' then
			return '0';
		elsif a(i) = '0' and b(i) = '1' then
			return '1';
		end if;
	end loop;
	return '0';
end function;

function cmpGreaterUnsignedSN(a: SmallNumber; b: SmallNumber) return std_logic is
begin
	for i in SMALL_NUMBER_SIZE-1 downto 0 loop
		if a(i) = '1' and b(i) = '0' then
			return '1';
		elsif a(i) = '0' and b(i) = '1' then
			return '0';
		end if;
	end loop;
	return '0';
end function;


function cmpGreaterThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := cmpGreaterUnsignedSN(arr(i), num);
	end loop;
	return res;
end function;

function cmpGreaterThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := cmpGreaterSignedSN(arr(i), num);
	end loop;
	return res;
end function;

function cmpLessThanUnsignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := cmpLessUnsignedSN(arr(i), num);
	end loop;
	return res;
end function;

function cmpLessThanSignedSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	variable res: std_logic_vector(0 to arr'length-1) := (others => '0');
begin
	for i in 0 to res'length-1 loop
		res(i) := cmpLessSignedSN(arr(i), num);
	end loop;
	return res;
end function;

function cmpEqualToSNA(arr: SmallNumberArray; num: SmallNumber) return std_logic_vector is
	constant LEN: integer := arr'length;
	variable res: std_logic_vector(0 to LEN-1) := (others => '0');
	variable sn: SmallNumber := (others => '0');
begin
	for i in 0 to LEN-1 loop
		if num = arr(i) then
			res(i) := '1';
		else
			res(i) := '0';
		end if;
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
        res(n-1 downto 0) := a;
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
        res(n-1 downto 0) := a;
        res(LEN-1 downto n) := (others => a(n-1));
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


end package body;
