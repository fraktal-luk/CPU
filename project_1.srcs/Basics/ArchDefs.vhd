
library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;

package ArchDefs is

constant r0: natural := 0;			constant r1: natural := 1;
constant r2: natural := 2;			constant r3: natural := 3;
constant r4: natural := 4;			constant r5: natural := 5;
constant r6: natural := 6;			constant r7: natural := 7;		
constant r8: natural := 8;			constant r9: natural := 9;
constant r10: natural := 10;		constant r11: natural := 11;
constant r12: natural := 12;		constant r13: natural := 13;
constant r14: natural := 14;		constant r15: natural := 15;
constant r16: natural := 16;		constant r17: natural := 17;
constant r18: natural := 18;		constant r19: natural := 19;
constant r20: natural := 20;		constant r21: natural := 21;
constant r22: natural := 22;		constant r23: natural := 23;
constant r24: natural := 24;		constant r25: natural := 25;
constant r26: natural := 26;		constant r27: natural := 27;
constant r28: natural := 28;		constant r29: natural := 29;
constant r30: natural := 30;		constant r31: natural := 31;

constant f0: natural := 0;			constant f1: natural := 1;
constant f2: natural := 2;			constant f3: natural := 3;
constant f4: natural := 4;			constant f5: natural := 5;
constant f6: natural := 6;			constant f7: natural := 7;		
constant f8: natural := 8;			constant f9: natural := 9;
constant f10: natural := 10;		constant f11: natural := 11;
constant f12: natural := 12;		constant f13: natural := 13;
constant f14: natural := 14;		constant f15: natural := 15;
constant f16: natural := 16;		constant f17: natural := 17;
constant f18: natural := 18;		constant f19: natural := 19;
constant f20: natural := 20;		constant f21: natural := 21;
constant f22: natural := 22;		constant f23: natural := 23;
constant f24: natural := 24;		constant f25: natural := 25;
constant f26: natural := 26;		constant f27: natural := 27;
constant f28: natural := 28;		constant f29: natural := 29;
constant f30: natural := 30;		constant f31: natural := 31;


-- This definiton is architectural: independent of implementation details
subtype Mword is Word; -- machine word: registers and pointers
type MwordArray is array(integer range<>) of Mword;
constant MWORD_SIZE: natural := Mword'length;

subtype RegFile is MwordArray(0 to 31);
subtype RegName is slv5;	

type RegNameArray is array (natural range <>) of RegName;
type QuintetArray is array (natural range <>) of slv5;


type ProcMnemonic is ( -- one word instruction names, distinguishing different arg combinations
    --set, mov, clr, nop, -- pseudoinstructions
    undef,


    and_i, and_r,
    or_i, or_r,
    xor_i, xor_r,
    
    add_i,
    add_h,
    add_r,
    sub_r,
    
    shl_i, shl_r, -- direction defined by shift value, not opcode 
    sha_i, sha_r, --   
    rot_i, rot_r,
    
    mult, 
        mulh_s, mulh_u,
    div_s, div_u,
    
    mov_f, or_f,    -- Float operations
    
    ldi_i, ldi_r, -- int
    sti_i, sti_r,
    
    ldf_i, ldf_r, -- float
    stf_i, stf_r, 
    
    lds, -- load sys
    
    sts, -- store sys
    
    jz_i, jz_r, jnz_i, jnz_r,
    ja, jl, -- jump always, jump link
    
    sys, -- system operation
    
			sys_retE,
            sys_retI,
            sys_halt,
            sys_sync,
            sys_replay,
            sys_error,
            sys_call,
            sys_send 
    
);

-- 2^6 possible values
type ProcOpcode is (
							andI,
							orI,
							
							addI,
							subI,
							
							jz,
							jnz,
							
							j,
							jl,
							
							ld,
							st,
							
							ldf,
							stf,
							
							ext0, -- E format: opcont is present and defines exact operation  
							ext1,
							ext2,
							fop, -- Float operations
							
							undef
							);


subtype ProcStandaloneOpcode is ProcOpcode range andI to jl; -- Update when needed!

-- 2^6 numeric values, but each 6b number can stand for different opconts, depending on Opcode!							
type ProcOpcont is ( -- ALU functions
							none,
							
							-- ext0:
							andR,
							orR,
							
							shlC,
							--shrlC,
							shaC,
							
							addR,
							subR,
							
							muls,
							mulu,
							
							divs,
							divu,
							
							-- ext1:
							--mem
							store,
							load,
								
								storeFP,
								loadFP,
							--
							jzR, -- jumping with adr in register
							jnzR,	
							
							-- ext2: 
							-- system jumps
							retE,
							retI,
							halt,
							sync,
							replay,
							error,
							call,
							send,
							
							mfc,
							mtc,
							
							fmov,
							forr,
							
							undef
							
							);

--type ConditionType is (unknown, zero, nonzero, always); --??

type ExceptionType is (none, unknown, 
							restrictedInstruction, undefinedInstruction,
							dataCacheMiss,		illegalAccess,
							sysCall,
							tlbMiss,
							integerOverflow,	integerDivisionBy0
						);

constant EXC_BASE:   Mword := X"00000100";
constant CALL_BASE:  Mword := X"00000180";
constant RESET_BASE: Mword := X"00000200";
constant INT_BASE:   Mword := X"00000280";

function hasOpcont(op: ProcOpcode) return boolean; 

function firstOpcont(op: ProcOpcode) return ProcOpcont;

-- To obtain bit encoding, for Opcode just get position of literal
-- For Opcont, get posiiton relative to the first of its Opcode: 
--				(ext1, load) -> (ProcOpcode'pos(ext1), ProcOpcont'pos(load) - ProcOpcont'pos(ext1'firstOpcode) )

function opcode2int(opc: ProcOpcode) return integer;
function opcont2int(opcd: ProcOpcode; opct: ProcOpcont) return integer;

function int2opcode(opcodeVec: integer) return ProcOpcode;
function int2opcont(opcodeVec, opcontVec: integer) return ProcOpcont;

function opcode2slv(opc: ProcOpcode) return slv6;
function opcont2slv(opcd: ProcOpcode; opct: ProcOpcont) return slv6;

function slv2opcode(opcodeVec: slv6) return ProcOpcode;
function slv2opcont(opcodeVec, opcontVec: slv6) return ProcOpcont;

-- Encoding
function ins6L(opcode: ProcOpcode; offset: integer) return Word;
function ins65J(opcode: ProcOpcode; ra, offset: integer) return Word;
function ins655H(opcode: ProcOpcode; ra, rb, offset: integer) return Word;
function ins6556X(opcode: ProcOpcode; ra, rb: integer; opcont: ProcOpcont; offset: integer) return Word;
function ins655655(opcode: ProcOpcode; ra, rb: integer; opcont: ProcOpcont; rc, rd: integer) return Word;


-- Encoding (with undefined offset)
function ins6L(opcode: ProcOpcode) return Word;
function ins65J(opcode: ProcOpcode; ra: integer) return Word;
function ins655H(opcode: ProcOpcode; ra, rb: integer) return Word;
function ins6556X(opcode: ProcOpcode; ra, rb: integer; opcont: ProcOpcont) return Word;


end ArchDefs;



package body ArchDefs is

function hasOpcont(op: ProcOpcode) return boolean is
begin
	case op is 
		when ext0 | ext1 | ext2 | fop =>
			return true;
		when others => 
			return false;
	end case;
end function;

function firstOpcont(op: ProcOpcode) return ProcOpcont is
begin
	assert hasOpcont(op) report "no opcont" severity error;
	case op is
		when ext0 =>
			return andR;
		when ext1 =>
			return jzR;
		when ext2 =>
			return retE;
		when fop =>
			return fmov;
		when others =>
			return none; -- none corresponds ofc to those that have no opcont
	end case;
end function;


function opcode2int(opc: ProcOpcode) return integer is
begin
	return (ProcOpcode'pos(opc));
end function;

function opcont2int(opcd: ProcOpcode; opct: ProcOpcont) return integer is
	variable i0, i1: integer;
	variable res: integer;
begin
	i0 := ProcOpcont'pos(opct);
	i1 := ProcOpcont'pos(firstOpcont(opcd));
	res := (i0-i1);
	return res;
end function;

-- Convert vectors to opcde + opcont 
function int2opcode(opcodeVec: integer) return ProcOpcode is
	variable i0: integer;
begin
	i0 := (opcodeVec) mod 64;
	return ProcOpcode'val(i0);
end function;

function int2opcont(opcodeVec, opcontVec: integer) return ProcOpcont is
	variable i0, i1: integer;
begin
	i0 := (opcodeVec) mod 64;
	i1 := (opcontVec) mod 64;
	return ProcOpcont'val(i1 + ProcOpcont'pos(firstOpcont(ProcOpcode'val(i0))));
end function;

 
-- Convert opcde + opcont to vectors
function opcode2slv(opc: ProcOpcode) return slv6 is
begin
	return i2slv(ProcOpcode'pos(opc), 6);
end function;

function opcont2slv(opcd: ProcOpcode; opct: ProcOpcont) return slv6 is
	variable i0, i1: integer;
	variable res: slv6;
begin
	i0 := ProcOpcont'pos(opct);
	i1 := ProcOpcont'pos(firstOpcont(opcd));
	res := i2slv(i0-i1, 6);
	return res;
end function;

-- Convert vectors to opcde + opcont 
function slv2opcode(opcodeVec: slv6) return ProcOpcode is
begin
	if slv2u(opcodeVec) > ProcOpcode'pos(ProcOpcode'right) then
		return undef;
	else
		return ProcOpcode'val(slv2u(opcodeVec));
	end if;
end function;

function slv2opcont(opcodeVec, opcontVec: slv6) return ProcOpcont is
begin
	if hasOpcont(slv2opcode(opcodeVec)) then
		return ProcOpcont'val(slv2u(opcontVec) + ProcOpcont'pos(firstOpcont(ProcOpcode'val(slv2u(opcodeVec)))));
	else
		return none;
	end if;	
end function;


function ins6L(opcode: ProcOpcode; offset: integer) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 0) := i2slv(offset, 26);
	return res;
end function;

function ins65J(opcode: ProcOpcode; ra, offset: integer) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 21) := i2slv(ra, 5);
    res(20 downto 0) := i2slv(offset, 21);
	return res;
end function;

function ins655H(opcode: ProcOpcode; ra, rb, offset: integer) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 21) := i2slv(ra, 5);
    res(20 downto 16) := i2slv(rb, 5);    
    res(15 downto 0) := i2slv(offset, 16);
	return res;
end function;

function ins6556X(opcode: ProcOpcode; ra, rb: integer; opcont: ProcOpcont; offset: integer) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 21) := i2slv(ra, 5);
    res(20 downto 16) := i2slv(rb, 5);
    res(15 downto 10) := opcont2slv(opcode, opcont);  
    res(9 downto 0) := i2slv(offset, 10);
	return res;
end function;

function ins655655(opcode: ProcOpcode; ra, rb: integer; opcont: ProcOpcont; rc, rd: integer) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 21) := i2slv(ra, 5);
    res(20 downto 16) := i2slv(rb, 5);
    res(15 downto 10) := opcont2slv(opcode, opcont);  
    res(9 downto 5) := i2slv(rc, 5);
    res(4 downto 0) := i2slv(rd, 5);
    return res;
end function;	



-- Variants with undefined offset - only for formats with offset
function ins6L(opcode: ProcOpcode) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 0) := (others => 'U');
	return res;
end function;

function ins65J(opcode: ProcOpcode; ra: integer) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 21) := i2slv(ra, 5);
    res(20 downto 0) := (others => 'U');
	return res;
end function;

function ins655H(opcode: ProcOpcode; ra, rb: integer) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 21) := i2slv(ra, 5);
    res(20 downto 16) := i2slv(rb, 5);    
    res(15 downto 0) := (others => 'U');
	return res;
end function;

function ins6556X(opcode: ProcOpcode; ra, rb: integer; opcont: ProcOpcont) return Word is
    variable res: Word := (others => '0');
begin
    res(31 downto 26) := opcode2slv(opcode);
    res(25 downto 21) := i2slv(ra, 5);
    res(20 downto 16) := i2slv(rb, 5);
    res(15 downto 10) := opcont2slv(opcode, opcont);  
    res(9 downto 0) := (others => 'U');
	return res;
end function;

end ArchDefs;
