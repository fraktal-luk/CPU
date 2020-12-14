----------------------------------------------------------------------------------

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
constant FMT_FP1    : InstructionFormat := ('0', '0', '0', '0', "000",  '1', "100",  '0');
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
    fmt:  InstructionFormat;
    sop:  SpecificOp;
end record;

type InsDefArray is array (natural range <>) of InsDef;

constant DECODE_TABLE: InsDefArray(0 to 40) := (
    0 => (andI, none, FMT_IMM, sop(ALU, opAnd)),
    1 => (orI,  none, FMT_IMM, sop(ALU, ArithOp'(opOr))),
    2 => (addI, none, FMT_IMM, sop(ALU, opAdd)),
    3 => (subI, none, FMT_IMM, sop(ALU, opSub)),
    
    4 => (ld, none,	  FMT_IMM,  sop(Mem, opLoad)),
    5 => (st, none,   FMT_ISTORE, sop(Mem, opStore)),

    6 => (j, 	none, FMT_JA,     sop(ALU, opJ)),
    7 => (jl, 	none, FMT_JL,   sop(ALU, opJ)),
    8 => (jz, 	none, FMT_JC,    sop(ALU, opJz)),
    9 => (jnz, 	none, FMT_JC,   sop(ALU, opJnz)),
    
    10=> (ext0, muls, FMT_INT2,    sop(ALU, opMul)),
    11=> (ext0, mulu, FMT_INT2,    sop(ALU, opMul)),

    12 => (ext0, shlC,FMT_SHIFT, sop(ALU, opShl)),
    --13 => (ext0, shrlC, Alu,  logicShrl,fmtShiftImm),
    14 => (ext0, shaC,FMT_SHIFT, sop(ALU, opSha)),

    15=> (ext2, mfc,  FMT_SLOAD, sop(Mem, opLoadSys)),
    16=> (ext2, mtc,  FMT_SSTORE, sop(Mem, opStoreSys)),		
                
    17=> (ext0, addR, FMT_INT2, sop(ALU, opAdd)),
    18=> (ext0, subR, FMT_INT2, sop(ALU, opSub)),
    19=> (ext0, andR, FMT_INT2, sop(ALU, opAnd)),
    20=> (ext0, orR,  FMT_INT2, sop(ALU, ArithOp'(opOr))),
        
    21=> (ext1, jzR,  FMT_JR,      sop(ALU, opJz)),
    22=> (ext1, jnzR, FMT_JR,     sop(ALU, opJnz)),

    23 => (ldf, none, FMT_FLOAD,    sop(Mem, opLoad)),
    24 => (stf, none, FMT_FSTORE,   sop(Mem, opStore)),
    
    25 => (ext2, halt,  FMT_DEFAULT, sop(None, opHalt)),
    26 => (ext2, retI,  FMT_DEFAULT, sop(None, opRetI)),
    27 => (ext2, retE,  FMT_DEFAULT, sop(None, opRetE)),
    28 => (ext2, sync,  FMT_DEFAULT, sop(None, opSync)),
    29 => (ext2, replay,FMT_DEFAULT, sop(None, opReplay)),
    30 => (ext2, error, FMT_DEFAULT, sop(None, opError)),
    31 => (ext2, call,  FMT_DEFAULT, sop(None, opCall)),
    32 => (ext2, send,  FMT_DEFAULT, sop(None, opSend)),
    
    33 => (fop,  fmov,  FMT_FP1,     sop(FP, opMove)),
    34 => (fop,  forr,  FMT_FP2,     sop(FP, FpOp'(opOr))),
    
    others => (ext2, undef, FMT_DEFAULT, sop(None, opUndef))
);


function decodeFromWord(w: word) return InstructionState;





subtype LogicTable is std_logic_vector(0 to 63);

constant LTABLE_immSel_none: LogicTable := (0 to 11 => '1', others => '0'); 
constant LTABLE_immSel_ext0: LogicTable := (2 to 3 => '1', others => '0'); 
constant LTABLE_immSel_ext2: LogicTable := (8 to 9 => '1', others => '0'); 

constant LTABLE_branchIns_none: LogicTable := (4 to 7 => '1', others => '0'); 
constant LTABLE_branchIns_ext1: LogicTable := (0 to 1 => '1', others => '0'); 

constant LTABLE_mainCluster_none: LogicTable := (0 to 13 => '1', 15 => '1', others => '0'); -- ext2 is the exception 
--constant LTABLE_mainCluster_ext0: LogicTable := (others => '1'); 
constant LTABLE_mainCluster_ext2: LogicTable := (8 to 9 => '1', others => '0');

constant LTABLE_secCluster_none: LogicTable := (9 => '1', 11 => '1', others => '0'); 
--constant LTABLE_secCluster_ext0: LogicTable := (others => '0'); 
constant LTABLE_secCluster_ext2: LogicTable := (9 => '1', others => '0'); 

function logicFunction(table: LogicTable; v: slv6) return std_logic;


function decodeImmSel(w: Word) return std_logic;
function decodeBranchIns(w: Word) return std_logic;
function decodeMainCluster(w: Word) return std_logic;
function decodeSecCluster(w: Word) return std_logic;
function decodeFpRename(w: Word) return std_logic;


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
    variable specificOperation: SpecificOp := DEFAULT_SPECIFIC_OP;
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
    specificOperation := DEFAULT_SPECIFIC_OP;
    fmt := FMT_DEFAULT;
    for i in DECODE_TABLE'range loop
        if opcode = DECODE_TABLE(i).opcd and
           (not haveOpcont or opcont = DECODE_TABLE(i).opct)
        then
           specificOperation := DECODE_TABLE(i).sop;
           fmt := DECODE_TABLE(i).fmt;
           exit;
        end if;
    end loop;    
    
    res.specificOperation := specificOperation;
    
    res.classInfo.fpRename := fmt.fpDestSel or isNonzero(fmt.fpSrcSel);
    
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




function logicFunction(table: LogicTable; v: slv6) return std_logic is
begin
    return table(slv2u(v));
end function;


-- Decoding control bits
--  immSel
--  ins.classInfo.mainCluster 
--  ins.classInfo.secCluster 
--  ins.classInfo.fpRename
--  ins.classInfo.branchIns;
-- Secondary, depending on BP (to be defined in LogicFront because impl. specific):
--  frontBranch
--  confirmedBranch
function decodeImmSel(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    
    -- if opcode in [opcodes with imm]
    -- or opcode is opcA and opcont in [imm(opcA)] 
    -- ...
    -- or opcode is opcZ and opcont in [imm(opcZ)]
    --  -> '1'
    -- else -> '0';
    
    res :=                                              logicFunction(LTABLE_immSel_none, opcode) 
            or (bool2std(opcode = opcode2slv(ext0)) and logicFunction(LTABLE_immSel_ext0, opcont))
            or (bool2std(opcode = opcode2slv(ext2)) and logicFunction(LTABLE_immSel_ext2, opcont));
    
    return res;
end function; 




-- ......



function decodeBranchIns(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res :=                                              logicFunction(LTABLE_branchIns_none, opcode) 
            or (bool2std(opcode = opcode2slv(ext1)) and logicFunction(LTABLE_branchIns_ext1, opcont));
    return res;
end function;


function decodeMainCluster(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res :=                                              logicFunction(LTABLE_mainCluster_none, opcode) 
            or (bool2std(opcode = opcode2slv(ext2)) and logicFunction(LTABLE_mainCluster_ext2, opcont));
    return res;
end function;

function decodeSecCluster(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res :=                                              logicFunction(LTABLE_secCluster_none, opcode) 
            or (bool2std(opcode = opcode2slv(ext2)) and logicFunction(LTABLE_secCluster_ext2, opcont));
    return res;
end function;



function decodeFpRename(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res := bool2std(opcode = opcode2slv(ldf)) or bool2std(opcode = opcode2slv(stf)) or bool2std(opcode = opcode2slv(fop));
    
    return res;
end function;


end package body;