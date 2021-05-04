----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.InstructionSet.all;

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

function decodeFromWordNew(w: word) return InstructionState;



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

function decodeSrc2a(w: Word) return std_logic;
function decodeSrc0a(w: Word) return std_logic;

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
    
            fmt.src0a := decodeSrc0a(w);
            fmt.src2a := decodeSrc2a(w);
            fmt.src1i := decodeImmSel(w);
    
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





--constant FMT_DEFAULT: InstructionFormat := ('0', '0', '0', '0', "000",  '0', "000",  '0');
--constant FMT_INT2   : InstructionFormat := ('0', '0', '0', '1', "110",  '0', "000",  '0');
--constant FMT_INT3   : InstructionFormat := ('0', '0', '0', '1', "111",  '0', "000",  '0');
--constant FMT_IMM    : InstructionFormat := ('0', '1', '0', '1', "110",  '0', "000",  '1');
--constant FMT_SHIFT  : InstructionFormat := ('0', '1', '0', '1', "110",  '0', "000",  '0');
--constant FMT_FP1    : InstructionFormat := ('0', '0', '0', '0', "000",  '1', "100",  '0');
--constant FMT_FP2    : InstructionFormat := ('0', '0', '0', '0', "000",  '1', "110",  '0');
--constant FMT_FP3    : InstructionFormat := ('0', '0', '0', '0', "000",  '1', "111",  '0');
--constant FMT_FLOAD  : InstructionFormat := ('0', '1', '0', '0', "110",  '1', "000",  '1');
--constant FMT_ISTORE : InstructionFormat := ('0', '1', '1', '0', "111",  '0', "000",  '1');
--constant FMT_FSTORE : InstructionFormat := ('0', '1', '1', '0', "110",  '0', "001",  '1');
--constant FMT_JL     : InstructionFormat := ('0', '1', '0', '1', "000",  '0', "000",  '0'); -- Jump link
--constant FMT_JC     : InstructionFormat := ('1', '1', '0', '0', "100",  '0', "000",  '0'); -- Jump cond
--constant FMT_SSTORE : InstructionFormat := ('0', '1', '1', '0', "011",  '0', "000",  '0'); -- mtc

--constant FMT_JA     : InstructionFormat := ('0', '1', '0', '0', "000",  '0', "000",  '0'); -- Jump long
--constant FMT_ILOAD  : InstructionFormat := FMT_IMM;
--constant FMT_JR     : InstructionFormat := FMT_INT2;  -- Jump reg
--constant FMT_SLOAD  : InstructionFormat := FMT_SHIFT; -- mfc

--constant MainTable: OpcodeTable0 := (
--    0 => (intAlu, none, none, undef), -- TableIntAlu
--    1 => (floatOp, none, none, undef), -- TableFloatOp     
--    2 => (intMem, none, none, undef), -- TableIntMem
--    3 => (floatMem, none, none, undef), -- TableFloatMem
--    4 => (sysMem, none, none, undef),  -- TableSysMem
    
--    7 => (sysControl, none, none, undef),
--    8 => (jumpLong, jump, jumpLong, ja),
--    9 => (jumpLink, jump, jumpLink, jl),
--    10 => (jumpZ, jump, jumpCond, jz_i),
--    11 => (jumpNZ, jump, jumpCond, jnz_i),
    
--    16 => (addI, intAdd, intImm16, add_i),
--    17 => (addH, intAdd, intImm16, add_h),
    
--    20 => (intLoadW16, intLoadW, intImm16, ldi_i),
--    21 => (intStoreW16, intStoreW, intStore16, sti_i),
--    22 => (floatLoadW16, floatLoadW, floatLoad16, ldf_i),
--    23 => (floatStoreW16, floatStoreW, floatStore16, stf_i),
    
--    others => (none, none, none, undef)
--);


function decodeOperation(op0, op1: slv6; op2: slv5) return SpecificOp is
    variable specificOperation: SpecificOp := DEFAULT_SPECIFIC_OP;
    
    variable isUndef: boolean := false;
    variable isAluOp, isFpOp, isMemOp, isSysOp: boolean := false;
    variable aluOp: ArithOp; 
    variable floatOp: FpOp; 
    variable memoryOp: MemOp; 
    variable systemOp: SysOp; 
begin

    isAluOp :=        op0 = "000000"
                or  op0 = "001000"     
                or  op0 = "001001"
                or  op0 = "001010"
                or  op0 = "001011"
                or  op0 = "010000"
                or  op0 = "010001";
                
     isFpOp :=        op0 = "000001";
    
     isMemOp :=      op0 = "000010"
               or  op0 = "000011"     
               or  op0 = "000100"     
               or  op0 = "010100"
               or  op0 = "010101"
               or  op0 = "010110"
               or  op0 = "010111";    
    
     isSysOp :=     op0 = "000111";
     
     isUndef := not (isAluOp or isMemOp or isSysOp);

    -- 

    if op0 = "010000" then
        aluOp := opAdd;
    elsif op0 = "010001" then
        aluOp := opAdd;
    elsif op0 = "001000" then
        aluOp := opJ;
    elsif op0 = "001001" then
        aluOp := opJl;        
    elsif op0 = "001010" then
        aluOp := opJz;        
    elsif op0 = "001011" then
        aluOp := opJnz;        
    
    elsif op0 = "000000" then
        
        if op1 = "000000" then
            if op2 = "00000" then
                aluOp := opAnd;
            elsif op2 = "00001" then
                aluOp := opOr;
            else 
                isUndef := true;
            end if;
        elsif op1 = "000001" then
            if op2 = "00000" then
                aluOp := opAdd;
            elsif op2 = "00001" then
                aluOp := opSub;
            else 
                isUndef := true;
            end if;
        elsif op1 = "000010" then
            if op2 = "00000" then
                aluOp := opJz;
            elsif op2 = "00001" then
                aluOp := opJnz;
            else 
                isUndef := true;
            end if;
         else
            isUndef := true;
        end if;
        
    end if;
    
    -- Mem
    if op0 = "010100" or op0 = "010110" then
        memoryOp := opLoad;    
    elsif op0 = "010101" or op0 = "010111" then
        memoryOp := opStore;
    elsif op0 = "000010" then 
        isUndef := true;
    elsif op0 = "000011" then 
        isUndef := true;
    elsif op0 = "000100" then 
        if op1 = "000000" then
            memoryOp := opLoadSys;
        elsif op1 = "100000" then
            memoryOp := opStoreSys;
        else
            isUndef := true; 
        end if;
    end if;
    
    -- Fp
    if op0 = "000001" then
        if op1 = "000000" and op2 = "00000" then
            floatOp := opMove;
        else
            isUndef := true;
        end if;
    end if;
    
    -- Sys
    if op0 = "000111" then
        if op1 = "000001" then
            systemOp := opError;        
        elsif op1 = "000010" then
            systemOp := opCall;        
        elsif op1 = "000011" then
            systemOp := opSync;        
        elsif op1 = "000100" then
            systemOp := opReplay;        
        elsif op1 = "000101" then
            systemOp := opHalt;        
        elsif op1 = "000110" then
            systemOp := opSend;        
        elsif op1 = "000111" then
            systemOp := opRetE;        
        elsif op1 = "001000" then
            systemOp := opRetI;
        else
            isUndef := true;      
        end if;
    end if;
    
    
    if isUndef then
        specificOperation := sop(None, opUndef);
    elsif isAluOp then
        specificOperation := sop(ALU, aluOp);
    elsif isFpOp then
        specificOperation := sop(FP, floatOp);
    elsif isMemOp then
        specificOperation := sop(Mem, memoryOp);
    else
        specificOperation := sop(None, systemOp);
    end if;
    
    return specificOperation;
end function;


function decodeFromWordNew(w: word) return InstructionState is
    variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
    variable fmt: InstructionFormat := FMT_DEFAULT;
    variable specificOperation: SpecificOp := DEFAULT_SPECIFIC_OP;
    
    constant op0: slv6 := w(31 downto 26);
    constant op1: slv6 := w(15 downto 10);
    constant op2: slv5 := w(4 downto 0);
    
    constant qa: slv5 := w(25 downto 21);
    constant qb: slv5 := w(20 downto 16);
    constant qc: slv5 := w(9 downto 5);
    constant qd: slv5 := w(4 downto 0);
    
    --constant imm16: Hword := w(15 downto 0);
    --constant imm10: Hword :=
    
    variable    hasOp1, hasOp2,
                isBranch, hasImm26, hasImm21, hasImm16, hasImm10, hasFpDest, hasIntDest, hasNoIntDest,
                src2a, src0a,
                intSrc0, intSrc1, intSrc2,
                fpSrc0, fpSrc1, fpSrc2: boolean := false;                
begin
    isBranch :=     op0 = "001000"
                 or op0 = "001001"
                 or op0 = "001010"
                 or op0 = "001011"
                 or (op0 = "000000" and (op1 = "000010"));
    
    hasImm26 :=     op0 = "001000";
    
    hasImm21 :=     op0 = "001001"
                 or op0 = "001010"
                 or op0 = "001011";
    
    hasImm16 :=     op0(4) = '1';
    
    -- TODO: add shift instructions 
    hasImm10 :=     op0 = "000010"
                 or op0 = "000011"
                 or op0 = "000100";
    
    hasFpDest :=    op0 = "010110"
                or  op0 = "000001";
    
    hasNoIntDest :=   op0 = "000001"
                  or  op0 = "000011"
                  or  op0 = "000111"
                  or  op0 = "001000"
                  or  op0 = "001010"
                  or  op0 = "001011"
                  or  op0 = "010101"
                  or  op0 = "010110"
                  or  op0 = "010111"
                  or  (op0 = "000100" and op1 = "100000");

    hasIntDest := not hasNoIntDest;
    
    src2a :=           op0 = "010101"
                   or  op0 = "010111"
                   or  (op0 = "000100" and op1 = "100000");
    
    src0a :=          op0 = "001010"
                  or  op0 = "001011";
    
    intSrc0 :=          op0 = "000000"
                    or  op0 = "000010"     
                    or  op0 = "000011"
                    or  op0 = "001010"
                    or  op0 = "001011"

                    or  op0(4) = '1';
    
    intSrc1 :=             op0 = "000000";
    
    --fpSrc1  :=             op0 = "000001"
    
    intSrc2 :=          op0 = "010101"
                    or  op0 = "010111"
                    or  (op0 = "000100" and op1 = "100000");      
    
    fpSrc2 :=        op0 = "010111";
     
     
    res.specificOperation := decodeOperation(op0, op1, op2);
    
    res.classInfo.fpRename := bool2std(hasFpDest or fpSrc0 or fpSrc1 or fpSrc2);
    
    -- assign register definitions
    res.virtualArgSpec.dest := "000" & qa;
    if hasIntDest and isNonzero(res.virtualArgSpec.dest) = '1' then
        res.virtualArgSpec.intDestSel := '1';
    elsif hasFpDest then
        res.virtualArgSpec.floatDestSel := '1';
    else -- When none selected, set 0
        --res.virtualArgSpec.dest := (others => '0');
    end if;
    
    if src0a then
       res.virtualArgSpec.args(0) := "000" & qa;
    else
       res.virtualArgSpec.args(0) := "000" & qb;
    end if;

    res.virtualArgSpec.intArgSel := (bool2std(intSrc0), bool2std(intSrc1), bool2std(intSrc2));
    res.virtualArgSpec.floatArgSel := (bool2std(fpSrc0), bool2std(fpSrc1), bool2std(fpSrc2));

    if hasImm16 or hasImm10 then
        res.virtualArgSpec.args(1) := (others => '0');
    else
        res.virtualArgSpec.args(1) := "000" & qc;
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

    if src2a then
       res.virtualArgSpec.args(2) := "000" & qa;
    else
       res.virtualArgSpec.args(2) := "000" & qd;
    end if;
    
    -- process immediate
    res.constantArgs.immSel := bool2std(hasImm16 or hasImm10 or hasImm26 or hasImm21);
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

function decodeSrc2a(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res := bool2std(opcode = opcode2slv(st))
        or bool2std(opcode = opcode2slv(stf))
        or bool2std((opcode = opcode2slv(ext2)) and (opcont = opcont2slv(ext2, mtc)));
    
    return res;
end function;

function decodeSrc0a(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res := bool2std(opcode = opcode2slv(jz))
        or bool2std(opcode = opcode2slv(jnz));
    
    return res;
end function;


end package body;