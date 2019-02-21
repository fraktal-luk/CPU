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
use work.Helpers.all;
use work.ArchDefs.all;

use work.InstructionState.all;


package DecodingDev is

-- dest: qa, none
-- src0: qb, qa, none
-- src1: qc, imm, none
-- src2: qd, qa, none
-- The above needs just 2 bits for quintet selection: (src0a, src2a)
-- Another bit will be for imm selection. Let it be always signed
-- Also must be selected the domain of args: I/F/none
-- (I|F|0)^4 -> or intSelect & fpSelect
-- > If src0 is qa, it will always be Int (branch cond)

--type ImmFormat is (imm26, imm21, imm16, imm10);

type InstructionFormat is record
    src0a, src1i, src2a: std_logic; -- src0 from qa? src1 from imm? src2 from qa?
    intDestSel: std_logic;
    intSrcSel: std_logic_vector(0 to 2);
    fpDestSel: std_logic;
    fpSrcSel: std_logic_vector(0 to 2);
    imm16: std_logic; -- Sizes 26, 21 are only for jumps, so not needed in back end (kept as full jump targets)
end record;

type InstructionParts is record
    opcode, opcont: slv6;
    qa, qb, qc, qd: SmallNumber;
    imm: Word;
end record;

constant DEFAULT_INS_PARTS: InstructionParts := (
  opcode => (others => '0'),
  opcont => (others => '0'),
  qa => (others => '0'),
  qb => (others => '0'),
  qc => (others => '0'),
  qd => (others => '0'),
  imm => (others => '0')  
);

function parseInsWord(w: Word) return InstructionParts;

--                                          0a  imm  2a  idest  isrc fdest  fsrc  imm16
constant FMT_DEFAULT: InstructionFormat := ('0', '0', '0', '0', "000",  '0', "000",  '0');
constant FMT_INT2   : InstructionFormat := ('0', '0', '0', '1', "110",  '0', "000",  '0');
constant FMT_INT3   : InstructionFormat := ('0', '0', '0', '1', "111",  '0', "000",  '0');
constant FMT_IMM    : InstructionFormat := ('0', '1', '0', '1', "110",  '0', "000",  '1');
constant FMT_SHIFT  : InstructionFormat := ('0', '1', '0', '1', "110",  '0', "000",  '0');
constant FMT_FP2    : InstructionFormat := ('0', '0', '0', '0', "000",  '1', "110",  '0');
constant FMT_FP3    : InstructionFormat := ('0', '0', '0', '0', "000",  '1', "111",  '0');
constant FMT_FLOAD  : InstructionFormat := ('0', '1', '0', '0', "110",  '1', "000",  '1');
constant FMT_ISTORE : InstructionFormat := ('0', '1', '1', '0', "111",  '0', "000",  '1');
constant FMT_FSTORE : InstructionFormat := ('0', '1', '1', '0', "110",  '0', "001",  '1');
constant FMT_JL     : InstructionFormat := ('0', '1', '0', '1', "000",  '0', "000",  '0'); -- Jump link
constant FMT_JC     : InstructionFormat := ('1', '1', '0', '0', "100",  '0', "000",  '0'); -- Jump cond
constant FMT_SSTORE : InstructionFormat := ('0', '1', '1', '0', "011",  '0', "000",  '0'); -- mtc

constant FMT_JA     : InstructionFormat := ('0', '1', '0', '0', "000",  '0', "000",  '0'); -- Jump long
constant FMT_ILOAD  : InstructionFormat := FMT_IMM;
constant FMT_JR     : InstructionFormat := FMT_INT2;  -- Jump reg
constant FMT_SLOAD  : InstructionFormat := FMT_SHIFT; -- mfc

type InsDef is record
    opcd: ProcOpcode;
    opct: ProcOpcont;
    unit: ExecUnit;
    func: ExecFunc;
    fmt:  InstructionFormat; 
end record;

type InsDefArray is array (natural range <>) of InsDef;

		constant DECODE_TABLE: InsDefArray(0 to 40) := (
				0 => (andI, none, Alu, logicAnd, FMT_IMM),
				1 => (orI,  none, Alu, logicOr,  FMT_IMM),
				2 => (addI, none, Alu, arithAdd, FMT_IMM),
				3 => (subI, none, Alu, arithSub, FMT_IMM),
				
				4 => (ld, none,	Memory,load,	FMT_IMM),
				5 => (st, none,Memory,store,	FMT_ISTORE),

				6 => (j, 	none, Jump, jump, FMT_JA),
				7 => (jl, 	none, Jump, jump,	FMT_JL),
				8 => (jz, 	none, Jump, jumpZ, FMT_JC),
				9 => (jnz, 	none, Jump, jumpNZ, FMT_JC),
				
				10=> (ext0, muls, Mac, mulS, FMT_INT2),
				11=> (ext0, mulu, Mac, mulU, FMT_INT2),

				12 => (ext0, shlC,  Alu,  logicShl,	FMT_SHIFT),
				--13 => (ext0, shrlC, Alu,  logicShrl,fmtShiftImm),
				14 => (ext0, shaC, Alu,  arithSha, FMT_SHIFT),

				15=> (ext2, mfc,	System, sysMFC, FMT_SLOAD),
				16=> (ext2, mtc, 	System, sysMTC, FMT_SSTORE),		
							
				17=> (ext0, addR, Alu, arithAdd, FMT_INT2),
				18=> (ext0, subR, Alu, arithSub, FMT_INT2),
				19=> (ext0, andR, Alu, logicAnd, FMT_INT2),
				20=> (ext0, orR,  Alu, logicOr,  FMT_INT2),
					
				21=> (ext1, jzR,  Jump, jumpZ, FMT_JR),
				22=> (ext1, jnzR, Jump, jumpNZ, FMT_JR),

			    23 => (ldf, none,	Memory,load,	FMT_FLOAD),
				24 => (stf, none,	Memory,store,	FMT_FSTORE),
				
				25 => (ext2, halt, System, sysHalt, FMT_DEFAULT),
				26 => (ext2, retI, System, sysRetI, FMT_DEFAULT),
				27 => (ext2, retE, System, sysRetE, FMT_DEFAULT),
				28 => (ext2, sync,	System, sysSync,	 FMT_DEFAULT),
				29 => (ext2, replay, System, sysReplay, FMT_DEFAULT),
				30 => (ext2, error,  System, sysError, FMT_DEFAULT),
				31 => (ext2, call,  System, sysCall, FMT_DEFAULT),
				32 => (ext2, send,  System, sysSend, FMT_DEFAULT),
				
				others => (ext2, undef, System, sysUndef, FMT_DEFAULT)
				);


function decodeFromWord(w: word) return InstructionState;

function insText(ins: InstructionState) return string;


end package;


package body DecodingDev is

function parseInsWord(w: Word) return InstructionParts is
    variable res: InstructionParts;
begin
    res.qa := (others => '0');
    res.qb := (others => '0');
    res.qc := (others => '0');
    res.qd := (others => '0');

    res.opcode := w(31 downto 26);
    res.opcont := w(15 downto 10);
    res.qa(4 downto 0) := w(25 downto 21);
    res.qb(4 downto 0) := w(20 downto 16);
    res.qc(4 downto 0) := w(9 downto 5);
    res.qd(4 downto 0) := w(4 downto 0);
    return res;
end function;

function decodeFromWord(w: word) return InstructionState is
    variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
    variable parts: InstructionParts;
    variable fmt: InstructionFormat := FMT_DEFAULT;
    variable opcode: ProcOpcode;
    variable opcont: ProcOpcont;
    variable haveOpcont: boolean;
    variable operation: BinomialOp := (General, unknown);
begin
    parts := parseInsWord(w);
    opcode := slv2opcode(parts.opcode);
    if hasOpcont(opcode) then
        haveOpcont := true;
        opcont := slv2opcont(parts.opcode, parts.opcont);
    else
        haveOpcont := false;
        opcont := none;
    end if;

    -- Find in table
    operation := (System, sysUndef);
    fmt := FMT_DEFAULT;
    for i in DECODE_TABLE'range loop
        if opcode = DECODE_TABLE(i).opcd and
           (not haveOpcont or opcont = DECODE_TABLE(i).opct)
        then
           operation := (DECODE_TABLE(i).unit, DECODE_TABLE(i).func);
           fmt := DECODE_TABLE(i).fmt;
           exit;
        end if;
    end loop;    
    
    -- Convert to InstructionState
    res.operation := operation;
    
    -- assign register definitions
    res.virtualArgSpec.dest := parts.qa;
    if fmt.intDestSel = '1' and isNonzero(res.virtualArgSpec.dest) = '1' then
        res.virtualArgSpec.intDestSel := '1';
    elsif fmt.fpDestSel = '1' then
        res.virtualArgSpec.floatDestSel := '1';
    else -- When none selected, set 0
        --res.virtualArgSpec.dest := (others => '0');
    end if;
    
    if fmt.src0a = '1' then
        res.virtualArgSpec.args(0) := parts.qa;
    else
        res.virtualArgSpec.args(0) := parts.qb;
    end if;
    if fmt.intSrcSel(0) = '1' then
        res.virtualArgSpec.intArgSel(0) := '1';
    elsif fmt.fpSrcSel(0) = '1' then
        res.virtualArgSpec.floatArgSel(0) := '1';
    else -- When none selected, set 0
        --res.virtualArgSpec.dest := (others => '0');
    end if;


    if fmt.src1i = '1' then
        res.virtualArgSpec.args(1) := (others => '0');
    else
        res.virtualArgSpec.args(1) := parts.qc;
    end if;
    if fmt.intSrcSel(1) = '1' then
        res.virtualArgSpec.intArgSel(1) := '1';
    elsif fmt.fpSrcSel(1) = '1' then
        res.virtualArgSpec.floatArgSel(1) := '1';
    else -- When none selected, set 0
        --res.virtualArgSpec.dest := (others => '0');
    end if;

    if fmt.src1i = '1' then -- When immediate, suppres register source 1??
        res.virtualArgSpec.intArgSel(1) := '0';
        res.virtualArgSpec.floatArgSel(1) := '0';
    end if;


    if fmt.src2a = '1' then
        res.virtualArgSpec.args(2) := parts.qa;
    else
        res.virtualArgSpec.args(2) := parts.qd;
    end if;
    if fmt.intSrcSel(2) = '1' then
        res.virtualArgSpec.intArgSel(2) := '1';
    elsif fmt.fpSrcSel(2) = '1' then
        res.virtualArgSpec.floatArgSel(2) := '1';
    else -- When none selected, set 0
        --res.virtualArgSpec.dest := (others => '0');
    end if;
    
    -- process immediate
    res.constantArgs.immSel := fmt.src1i; 
    res.constantArgs.imm := w;
    if fmt.imm16 = '1' then -- Put imm in proper form 
        res.constantArgs.imm(31 downto 16) := (others => w(15));
    else
        res.constantArgs.imm(31 downto 10) := (others => w(9));
    end if;
    
    return res;
end function;




function reg2txt(reg: std_logic_vector) return string is
    variable res: string(1 to 2);
    constant letters: string(1 to 16) := "0123456789abcdef";
begin
    if reg(4) = '1' then
        res(1) := '1';
    else
        res(1) := '0';
    end if;
    
    res(2) := letters(slv2u(reg(3 downto 0)) + 1);
    
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