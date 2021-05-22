

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.ArchDefs.all;
use work.CpuText.all;

package InstructionSet is

-- Main table - level 0
-- 
--  000000: ext0
--  
--          addi
--          addh
-- 
-- 


type Opcode0 is (none, jumpLong, jumpLink, jumpZ, jumpNZ, intAlu, floatOp, intMem, floatMem, 
                    addI, addH, 
                    
                    intLoadW16, intStoreW16,
                    floatLoadW16, floatStoreW16,
                    
                    sysMem, sysControl);

type Opcode1 is (none,
                 
                 -- intAlu
                 intLogic, intArith, jumpReg,
                 
                 -- floatOp
                 floatMove,
                 
                 -- intMem
                 intLoadW, intStoreW,
                 
                 -- floatMem
                 floatLoadW, floatStoreW,
                 
                 -- sysMem
                 sysLoad, sysStore,

                 
                 -- sysControl                 
                 sysUndef,
                 sysError,
                 sysCall,
                 sysRetE,
                 sysRetI,
                 sysHalt,
                 sysSync,
                 sysReplay,
                 sysSend                 
                 );


type Opcode2 is (none, 

                intAnd, intOr, intXor,
                
                intAdd, intSub, 
                
                
                floatMove,
                  
                    jumpRegZ, jumpRegNZ
                    
                
                );

type Format is (none, 
                    noRegs,
                        jumpLong, jumpLink, jumpCond,
                        intImm16, intImm10,
                        intRR, 
                        floatRR,
                    intStore16, intStore10, floatLoad10, floatLoad16, floatStore10, floatStore16,
                    sysLoad, sysStore);

--  
--  no regs
--  ja, jl, j(c)
--  int3r, int2r, int1r
--  fp3r, fp2r, fp1r,
--  int imm16, int imm10
--  int load16, int load10 -> equal to immN
--  int store16, int store10
--  fp load16, fp load10
--  fp store16, fp store10
--  sys load10
--  sys store10
--  int to fp
--  fp to int

type NEW_Format is (
    None,
    
    NoRegs,
    
    JumpLong, JumpLink, JumpCond,
    
    IntImm16, IntImm10,
    IntStore16, IntStore10,
    
    FloatLoad16, FloatLoad10,
    FloatStore16, FloatStore10,
    
    SysLoad, SysStore,
    
    Int3R, Int2R, Int1R,
    Float3R, Float2R, Float1R,
    
    FloatToInt, IntToFloat
);

-- Format description has 3 parts:
-- asm form, decoding, type spec
-- 
-- "d012",  "a,bcd",   "i,iii"   -- example
type NEW_FormatDescription is record
    asmForm: string(1 to 4);
    decoding: string(1 to 5);
    typeSpec: string(1 to 5);
end record;

type NEW_FormatDescriptionTable is array(NEW_Format range <>) of NEW_FormatDescription;

constant FormatDescriptions: NEW_FormatDescriptionTable(None to IntToFloat) := (
    None =>         ("    ", "0,000", "0,000"),

    NoRegs =>       ("    ", "0,000", "0,000"),

    JumpLong =>     ("1   ", "0,0L0", "i,ic0"),
    JumpLink =>     ("d1  ", "a,0J0", "i,ic0"),
    JumpCond =>     ("01  ", "0,aJ0", "i,ic0"),
    
    IntImm16 =>     ("d01 ", "a,bH0", "i,ic0"),
    IntImm10 =>     ("d01 ", "a,bX0", "i,ic0"),
    
    IntStore16 =>   ("201 ", "0,bHa", "i,ici"),
    IntStore10 =>   ("201 ", "0,bXa", "i,ici"),
    
    FloatLoad16 =>  ("d01 ", "a,bH0", "f,ic0"),
    FloatLoad10 =>  ("d01 ", "a,bX0", "f,ic0"),
    
    FloatStore16 => ("201 ", "0,bHa", "i,icf"),
    FloatStore10 => ("201 ", "0,bXa", "i,icf"),

    SysLoad =>      ("d1  ", "a,0X0", "i,ic0"),
    
    SysStore =>     ("21  ", "0,0Xa", "0,ici"),

    Int3R =>        ("d012", "a,bcd", "i,iii"),
    Int2R =>        ("d01 ", "a,bc0", "i,ii0"),
    Int1R =>        ("d0  ", "a,b00", "i,i00"),

    Float3R =>      ("d012", "a,bcd", "f,fff"),
    Float2R =>      ("d01 ", "a,bc0", "f,ff0"),
    Float1R =>      ("d0  ", "a,b00", "f,f00"),
    
    FloatToInt =>   ("d0  ", "a,b00", "i,f00"),
    
    IntToFloat =>   ("d0  ", "a,b00", "f,i00"),    


    others =>       ("    ", "0,000", "0,000")
);


type FormatAssignments is array(ProcMnemonic range <>) of NEW_Format;

constant FormatList: FormatAssignments(undef to sys_send) :=
(
    undef => None,

    and_i => None,
    and_r => Int2R,
    or_i => None,
    or_r => Int2R,
    xor_i => None,
    xor_r => Int2R,
    
    add_i => IntImm16,
    add_h => IntImm16,
    add_r => Int2R,
    sub_r => Int2R,
    
    shl_i => IntImm10,
    shl_r => Int2R, -- direction defined by shift value, not opcode 
    sha_i => IntImm10,
    sha_r => Int2R, --   
    
    mul => Int2R,
    mulh_s => Int2R,
    mulh_u => Int2R,
    div_u => Int2R,
    div_s => Int2R,
    
    mov_f => Float1R,
    or_f => Float2R,    -- Float operations
    
    ldi_i => IntImm16,
    ldi_r => None, -- int
    
    sti_i => IntStore16,
    sti_r => None,
    
    ldf_i => FloatLoad16,
    ldf_r => None, -- float
    
    stf_i => FloatStore16,
    stf_r => None, 
    
    lds => sysLoad, -- load sys
    sts => SysStore, -- store sys
    
    jz_i => JumpCond,
    jz_r => Int2R,
    jnz_i => JumpCond,
    jnz_r => Int2R,
    ja => JumpLong,
    jl => JumpLink,
    
    sys => NoRegs, -- system operation

        sys_retE => NoRegs,
        sys_retI => NoRegs,
        sys_halt => NoRegs,
        sys_sync => NoRegs,
        sys_replay => NoRegs,
        sys_error => NoRegs,
        sys_call => NoRegs,
        sys_send => NoRegs,

    others => None
);





type Operation is (none,
                    
                    undef,
                    call,
                    sync,
                    retE,
                    retI,
                    replay,
                    halt,
                    send,
                    
                    
                    jump,
                    
                    intAnd, intOr, intXor,
                    
                    
                    intAdd, intSub,
                    
                    floatMove,
                    
                    
                    intLoadW, intLoadD,
                    
                    intStoreW, intStoreD,
                    
                    floatLoadW, floatStoreW,
                    
                    sysLoad, sysStore
                    
                    );

type OpcodeDef0 is record
    --num: slv6;
    name: Opcode0;
    op: Operation;
    fmt: Format;
    mnem: ProcMnemonic;
end record;

type OpcodeTable0 is array(0 to 63) of OpcodeDef0;

type OpcodeDef1 is record
    --num: slv6;
    name: Opcode1;
    op: Operation;
    fmt: Format;
    mnem: ProcMnemonic;    
end record;

type OpcodeTable1 is array(0 to 63) of OpcodeDef1;

type OpcodeDef2 is record
    --num: slv5;
    name: Opcode2;
    op: Operation;
    mnem: ProcMnemonic;    
end record;

type OpcodeTable2 is array(0 to 31) of OpcodeDef2;


type TableArray0 is array(Opcode0) of OpcodeTable1; 
type TableArray1 is array(Opcode1) of OpcodeTable2; 


type Reference1 is record
    name: Opcode1;
    table: OpcodeTable2;
end record;


type InstructionDefinition is record
        mnem: ProcMnemonic;
    opc0: Opcode0;
    opc1: Opcode1;
    opc2: Opcode2;
    op: Operation;    
    fmt: Format;
    i: integer;
    j: integer;
    k: integer;  
end record;


type GeneralTable is array (ProcMnemonic'left to ProcMnemonic'right) of InstructionDefinition;

function getDef(i, j, k: integer) return InstructionDefinition;

function buildGeneralTable return GeneralTable;

constant TheTable: GeneralTable --(ProcMnemonic'left to ProcMnemonic'right)
                                := buildGeneralTable;


constant EmptyTable1: OpcodeTable1 := (others => (none, none, none, undef));
constant EmptyTable2: OpcodeTable2 := (others => (none, none, undef));
    

    constant OP1_INT_ARITH_BRANCH: Dword := (2 => '1', others => '0');

    constant OP1_FLOAT_ARITH_SRC1: Dword := (others => '0');
    constant OP1_FLOAT_ARITH_SRC2: Dword := (others => '0');

    constant OP1_INT_MEM_LOAD: Dword := (others => '0');
    constant OP1_INT_MEM_STORE: Dword := (others => '0');
    constant OP1_FLOAT_MEM_LOAD: Dword := (others => '0');
    constant OP1_FLOAT_MEM_STORE: Dword := (others => '0');
    constant OP1_SYS_MEM_LOAD: Dword := (0 => '1', others => '0');
    constant OP1_SYS_MEM_STORE: Dword := (32 => '1', others => '0');


    constant OP0_INT_MEM: Dword := (2 => '1', others => '0');
    constant OP0_FLOAT_MEM: Dword := (3 => '1', others => '0');
    constant OP0_SYS_MEM: Dword := (4 => '1', others => '0');

    constant OP0_INT_ARITH: Dword := (0 => '1', others => '0');
    constant OP0_FLOAT_ARITH: Dword := (1 => '1', others => '0');

    constant OP0_JUMP_LONG: Dword := (8 => '1', others => '0');
    constant OP0_JUMP_LINK: Dword := (9 => '1', others => '0');
    constant OP0_JUMP_COND: Dword := (10 to 11 => '1', others => '0');

    constant OP0_JUMP: Dword := OP0_JUMP_LONG or OP0_JUMP_LINK or OP0_JUMP_COND;

    constant OP0_INT_IMM: Dword := (16 to 17 => '1', others => '0');

    constant OP0_IMM10: Dword := (2 to 4 => '1', others => '0');
    constant OP0_IMM16: Dword := (16 to 17 => '1', 20 to 23 => '1', others => '0');
    constant OP0_IMM21: Dword := (9 to 11 => '1', others => '0');
    constant OP0_IMM26: Dword := (8 => '1', others => '0');

    constant OP0_INT_LOAD: Dword := (20 => '1', others => '0');
    constant OP0_INT_STORE: Dword := (21 => '1', others => '0');
    constant OP0_FLOAT_LOAD: Dword := (22 => '1', others => '0');
    constant OP0_FLOAT_STORE: Dword := (23 => '1', others => '0');

    constant OP0_INT_DEST: Dword := OP0_INT_ARITH or OP0_JUMP_LINK or OP0_INT_IMM or OP0_INT_LOAD;
    constant OP0_FLOAT_DEST: Dword := OP0_FLOAT_ARITH or OP0_FLOAT_LOAD;

    constant OP0_LOAD: Dword := OP0_INT_LOAD or OP0_FLOAT_LOAD;
    constant OP0_STORE: Dword := OP0_INT_STORE or OP0_FLOAT_STORE;

    constant OP0_2A: Dword := OP0_STORE;
    constant OP0_0A: Dword := OP0_JUMP_COND;

    
    constant OP0_INT_SRC0: Dword := OP0_INT_ARITH or OP0_INT_MEM or OP0_FLOAT_MEM or OP0_JUMP_COND or OP0_IMM16;
    constant OP0_INT_SRC1: Dword := OP0_INT_ARITH;
    constant OP0_INT_SRC2: Dword := OP0_INT_STORE;

    constant OP0_FLOAT_SRC0: Dword := OP0_FLOAT_ARITH; --// or FP to Int
    
    constant OP0_FLOAT_SRC2: Dword := OP0_FLOAT_STORE;

    -- fp src1 -> (op0 fpArith) and (op1 fpArith src1)  
    -- fp src2 -> (op0 fpArith) and (op1 fpArith src1) or (op0 fp src2)
    
    -- int src2 -> OP0 or (intMem and op1 int store) or (sysMem and op1 sys store)

constant MainTable: OpcodeTable0 := (
    0 => (intAlu, none, none, undef), -- TableIntAlu
    1 => (floatOp, none, none, undef), -- TableFloatOp 
    
    2 => (intMem, none, none, undef), -- TableIntMem
    3 => (floatMem, none, none, undef), -- TableFloatMem
    
    4 => (sysMem, none, none, undef),  -- TableSysMem
    
    7 => (sysControl, none, none, undef),
    
    8 => (jumpLong, jump, jumpLong, ja),
    9 => (jumpLink, jump, jumpLink, jl),
    10 => (jumpZ, jump, jumpCond, jz_i),
    11 => (jumpNZ, jump, jumpCond, jnz_i),
    
    
    16 => (addI, intAdd, intImm16, add_i),
    17 => (addH, intAdd, intImm16, add_h),
    
    20 => (intLoadW16, intLoadW, intImm16, ldi_i),
    21 => (intStoreW16, intStoreW, intStore16, sti_i),
    22 => (floatLoadW16, floatLoadW, floatLoad16, ldf_i),
    23 => (floatStoreW16, floatStoreW, floatStore16, stf_i),
    
    others => (none, none, none, undef)
);


constant TableIntAlu: OpcodeTable1 := (
    0 => (intLogic, none, intRR, undef),
    1 => (intArith, none, intRR, undef),
    2 => (jumpReg, none, intRR, undef),
    
    others => (none, none, none, undef)
);

constant TableFloatOp: OpcodeTable1 := (
    0 => (floatMove, none, floatRR, undef),
    
    others => (none, none, none, undef)
);


constant TableIntMem: OpcodeTable1 := (
    --0 => (intLoadW, intLoadW, intImm10, ldi_i),

    --32 => (intStoreW, intStoreW, intStore10, sti_i),
    
    others => (none, none, none, undef)
);


constant TableFloatMem: OpcodeTable1 := (
    --0 => (floatLoadW, floatLoadW, floatLoad10, ldf_i),

    --32 => (floatStoreW, floatStoreW, floatStore10, stf_i),
    
    others => (none, none, none, undef)
);


constant TableSysMem: OpcodeTable1 := (
    0 => (sysLoad, sysLoad, sysLoad, lds),

    32 => (sysStore, sysStore, sysStore, sts),
    
    others => (none, none, none, undef)
);

constant TableSysControl: OpcodeTable1 := (
    0 => (sysUndef, undef, noRegs, undef),
    1 => (sysError, undef, noRegs, sys_error),
    2 => (sysCall,  call, noRegs, sys_call),
    3 => (sysSync,  sync, noRegs, sys_sync),
    4 => (sysReplay,  replay, noRegs, sys_replay),
    5 => (sysHalt,  halt, noRegs, sys_halt),
    6 => (sysSend,  send, noRegs, sys_send),
    7 => (sysRetE,  retE, noRegs, sys_retE),
    8 => (sysRetI,  retI, noRegs, sys_retI),
    
    others => (none, none, none, undef)
);



------------------

constant TableIntLogic: OpcodeTable2 := (
    0 => (intAnd, intAnd, and_r),
    1 => (intOr, intOr, or_r),
    others => (none, none, undef)
);

constant TableIntArith: OpcodeTable2 := (
    0 => (intAdd, intAdd, add_r),
    1 => (intSub, intSub, sub_r),
    others => (none, none, undef)
);


constant TableJumpReg: OpcodeTable2 := (
    0 => (jumpRegZ, jump, jz_r),
    1 => (jumpRegNZ, jump, jnz_r),
    others => (none, none, undef)
);


constant TableFloatMove: OpcodeTable2 := (
    0 => (floatMove, floatMove, mov_f),
    others => (none, none, undef)
);



constant Tables1: TableArray0 := (
    intAlu => TableIntAlu,
    floatOp => TableFloatOp,
    intMem => TableIntMem,
    floatMem => TableFloatMem,
    sysMem => TableSysMem,
    sysControl => TableSysControl,
    
    others => EmptyTable1
);

constant Tables2: TableArray1 := (
    intLogic => TableIntLogic,
    intArith => TableIntArith,
    jumpReg => TableJumpReg,
    floatMove => TableFloatMove,
    
    others => EmptyTable2
);


-- Level 1
-- ext0 - ALU
-- 
-- 000000: logical op
-- ...
-- 
-- 
-- 
-- 
-- 



-- Level 2
-- ext0:logical
-- 
-- 00000: and
-- 
-- 
-- 
-- 
-- 

function findEncoding(mnem: ProcMnemonic) return InstructionDefinition;

function TMP_processStrings(cmd, a0, a1, a2, a3: string) return Word;


        -- 
        function decodeMnem2(w: Word) return ProcMnemonic;

    function TMP_disasm(w: Word) return string;

end InstructionSet;



package body InstructionSet is

--function findMnemonic(mnem: ProcMnemonic) return boolean is
--    variable op0, op1, op2: integer := -1;
--    variable fmt: Format := none;
--    variable tab1: OpcodeTable1;
--begin
--    -- Search in main table
--    for i in 0 to 63 loop
--        if MainTable(i).mnem = mnem and MainTable(i).op /= none then
--            op0 := i;
--            fmt := MainTable(i).fmt;
            
--            return true;
--        end if;
--    end loop;
    
--    -- Search in 1 level tables
--    for t in Tables1'range loop
        
--    end loop;
    
--    return false;
--end function;



function getDef(i, j, k: integer) return InstructionDefinition is
    variable res: InstructionDefinition;
    variable op0: Opcode0 := none;
    variable op1: Opcode1 := none;
    variable op2: Opcode2 := none;
    variable op: Operation;
    variable fmt: Format;
    variable mnem: ProcMnemonic;
begin
--    report "Row: ";
--        report integer'image(i);
--        report integer'image(j);
--        report integer'image(k);

    if i /= -1 then
        op0 := MainTable(i).name;
        fmt := MainTable(i).fmt;
        op := MainTable(i).op;
        mnem := MainTable(i).mnem;
        
        --if j /= -1 then
        if op = none then
            op1 := Tables1(op0)(j).name;
            fmt := Tables1(op0)(j).fmt;
            op := Tables1(op0)(j).op;
            mnem := Tables1(op0)(j).mnem;
            
            --if k /= -1 then
            if op = none then
                op2 := Tables2(op1)(k).name;
                op := Tables2(op1)(k).op;
                mnem := Tables2(op1)(k).mnem;
            end if;
        end if;
    end if;
  
    return (mnem, op0, op1, op2, op, fmt, i, j ,k);
end function;



function makeRow(i, j, k: integer) return InstructionDefinition is
    variable res: InstructionDefinition;
    variable op0: Opcode0 := none;
    variable op1: Opcode1 := none;
    variable op2: Opcode2 := none;
    variable op: Operation;
    variable fmt: Format;
begin
--    report "Row: ";
--        report integer'image(i);
--        report integer'image(j);
--        report integer'image(k);

    if i /= -1 then
        op0 := MainTable(i).name;
        fmt := MainTable(i).fmt;
        op := MainTable(i).op;
        
        if j /= -1 then
            op1 := Tables1(op0)(j).name;
            fmt := Tables1(op0)(j).fmt;
            op := Tables1(op0)(j).op;
            
            if k /= -1 then
                op2 := Tables2(op1)(k).name;
                op := Tables2(op1)(k).op; 
            end if;
        end if;
    end if;
  
    return (undef, op0, op1, op2, op, fmt, i, j ,k);
end function;

function buildGeneralTable return GeneralTable is
    variable op0, op1, op2: integer := -1;
    variable fmt: Format := none;
    variable tab1: OpcodeTable1;
    variable tab2: OpcodeTable2;
    variable res: GeneralTable;
    variable found: boolean := false;
begin
    --        report "Build table";
        
    -- Search in main table
    for i in 0 to 63 loop
        --found := false;
        if MainTable(i).op /= none then
            -- Add this to the table
            res(MainTable(i).mnem) := makeRow(i, -1, -1);
            res(MainTable(i).mnem).mnem := MainTable(i).mnem;
        else
            --found := false;
            -- Enter level 1 table 
            tab1 := Tables1(MainTable(i).name);
                  
            for j in 0 to 63 loop
                if tab1(j).op /= none then
                    -- Add to table
                    res(tab1(j).mnem) := makeRow(i, j, -1);
                    res(tab1(j).mnem).mnem := tab1(j).mnem;
                    --exit;
                else
                    -- Enter level 2 table
                    tab2 := Tables2(tab1(j).name);
                
                    for k in 0 to 31 loop
                        if tab2(k).op /= none then
                            -- Add to table
                            res(tab2(k).mnem) := makeRow(i, j, k);
                            res(tab2(k).mnem).mnem := tab2(k).mnem;
                            --found := true;
                            --exit;
                        end if;
                    end loop;
                    
--                    if found then
--                        exit;
--                    end if;
                end if;
            end loop;
        end if;
    end loop;
    
    return res;
end function;

function findEncoding(mnem: ProcMnemonic) return InstructionDefinition is
    constant Table: GeneralTable := buildGeneralTable;
begin
    return Table(mnem);    
end function;




type ArgArray is array(0 to 3) of string(1 to 10);

function TMP_orderArgs(a0, a1, a2, a3: string; desc: NEW_FormatDescription) return ArgArray is
    variable res: ArgArray := (others => (others => ' '));
begin
    case desc.asmForm(1) is
        when 'd' =>
            res(0) := padLeft(a0, 10);
        when '0' =>
            res(1) := padLeft(a0, 10);
        when '1' =>
            res(2) := padLeft(a0, 10);
        when '2' =>
            res(3) := padLeft(a0, 10);       
        when others =>
    end case;

    case desc.asmForm(2) is
        when 'd' =>
            res(0) := padLeft(a1, 10);
        when '0' =>
            res(1) := padLeft(a1, 10);
        when '1' =>
            res(2) := padLeft(a1, 10);
        when '2' =>
            res(3) := padLeft(a1, 10);       
        when others =>
    end case;
    
    case desc.asmForm(3) is
        when 'd' =>
            res(0) := padLeft(a2, 10);
        when '0' =>
            res(1) := padLeft(a2, 10);
        when '1' =>
            res(2) := padLeft(a2, 10);
        when '2' =>
            res(3) := padLeft(a2, 10);       
        when others =>
    end case;
    
    case desc.asmForm(4) is
        when 'd' =>
            res(0) := padLeft(a3, 10);
        when '0' =>
            res(1) := padLeft(a3, 10);
        when '1' =>
            res(2) := padLeft(a3, 10);
        when '2' =>
            res(3) := padLeft(a3, 10);       
        when others =>
    end case;

    return res;
end function;

-- check if their form is according to description 
function TMP_checkArgs(argList: ArgArray; desc: NEW_FormatDescription) return boolean is
begin
    if desc.typeSpec(1) = 'i' then
        if argList(0)(1) /= 'r' then return false; end if;
    elsif desc.typeSpec(1) = 'f' then
        if argList(0)(1) /= 'f' then return false; end if;
    else
        if argList(0)(1) /= ' ' then return false; end if;
    end if;

    for i in 1 to 3 loop
        if desc.typeSpec(i + 1) = 'i' then
            if argList(i)(1) /= 'r' then return false; end if;
        elsif desc.typeSpec(i + 1) = 'f' then
            if argList(i)(1) /= 'f' then return false; end if;
        elsif desc.typeSpec(i + 1) = 'c' then
            -- TODO: check for correct constant?
            --if argList(i)(1) /= 'f' then return false; end if;            
        else
            if argList(i)(1) /= ' ' then return false; end if;
        end if;        
    end loop;
    
    return true;
end function;


function TMP_getArgs(argList: ArgArray) return IntArray is

    -- TMP, remove
    function parseArg(s: string) return integer is
        variable x: integer := -1;
    begin
        if s(1) = '$' or s(1) = '@' then
        
        elsif s(1) = 'r' and s(2) >= '0' and s(2) <= '9' then
            -- register
            x := TMP_str2int(s(2 to s'length));
        elsif s(1) = 'f' and s(2) >= '0' and s(2) <= '9' then
            -- register (FP)
            x := TMP_str2int(s(2 to s'length ));            
        elsif s(1) = '-' then
            x := -TMP_str2int(s(2 to s'length));                  
        elsif not isAlphanum(s(1)) then
            x := -1;
        elsif s(1) >= '0' and s(1) <= '9' then
            -- Hope it's a number 
            x := TMP_str2int(s);
        else
            x := 0;
        end if;    
        return x;
    end function;

    variable res: IntArray(0 to 3) := (others => 0);
begin
    for i in 0 to 3 loop
        res(i) := parseArg(argList(i));
    end loop;
    return res;
end function;

-- Insert args into word
function TMP_fillArgs(args: IntArray; desc: NEW_FormatDescription; undefOffset: boolean) return Word is
    variable res: Word := (others => '0');
begin
    case desc.decoding(1) is
        when 'a' =>
            res(25 downto 21) := i2slv(args(0), 5);
        when 'b' =>
            res(20 downto 16) := i2slv(args(0), 5);
        when 'c' =>
            res(9 downto 5) := i2slv(args(0), 5);
        when 'd' =>
            res(4 downto 0) := i2slv(args(0), 5);                                                
        when others =>
    end case;

    case desc.decoding(3) is -- Skip the comma!
        when 'a' =>
            res(25 downto 21) := i2slv(args(1), 5);
        when 'b' =>
            res(20 downto 16) := i2slv(args(1), 5);
        when 'c' =>
            res(9 downto 5) := i2slv(args(1), 5);
        when 'd' =>
            res(4 downto 0) := i2slv(args(1), 5);
        when 'X' =>
            res(9 downto 0) := i2slv(args(1), 10);
        when 'H' =>
            res(15 downto 0) := i2slv(args(1), 16);
        when 'J' =>
            res(20 downto 0) := i2slv(args(1), 21);
        when 'L' =>
            res(25 downto 0) := i2slv(args(1), 26);                                                            
        when others =>
    end case;
    
    case desc.decoding(4) is
        when 'a' =>
            res(25 downto 21) := i2slv(args(2), 5);
        when 'b' =>
            res(20 downto 16) := i2slv(args(2), 5);
        when 'c' =>
            res(9 downto 5) := i2slv(args(2), 5);
        when 'd' =>
            res(4 downto 0) := i2slv(args(2), 5);
        when 'X' =>     
            res(9 downto 0) := i2slv(args(2), 10);
            if undefOffset then
                res(9 downto 0) := (others => 'U');
            end if;
        when 'H' =>
            res(15 downto 0) := i2slv(args(2), 16);
            if undefOffset then
                res(15 downto 0) := (others => 'U');
            end if;
        when 'J' =>
            res(20 downto 0) := i2slv(args(2), 21);
            if undefOffset then
                res(20 downto 0) := (others => 'U');
            end if;
        when 'L' =>
            res(25 downto 0) := i2slv(args(2), 26);
            if undefOffset then
                res(25 downto 0) := (others => 'U');
            end if;                                                            
        when others =>
    end case;
    
    case desc.decoding(5) is
        when 'a' =>
            res(25 downto 21) := i2slv(args(3), 5);
        when 'b' =>
            res(20 downto 16) := i2slv(args(3), 5);
        when 'c' =>
            res(9 downto 5) := i2slv(args(3), 5);
        when 'd' =>
            res(4 downto 0) := i2slv(args(3), 5);
        when 'X' =>
            res(9 downto 0) := i2slv(args(3), 10);
        when 'H' =>
            res(15 downto 0) := i2slv(args(3), 16);
        when 'J' =>
            res(20 downto 0) := i2slv(args(3), 21);
        when 'L' =>
            res(25 downto 0) := i2slv(args(3), 26);                                                            
        when others =>
    end case;   

    return res;
end function;

function TMP_fillOp(w: Word; insDef: InstructionDefinition) return Word is
    variable res: Word := w;
begin
    res(31 downto 26) := i2slv(insDef.i, 6);
    
    if insDef.j = -1 then return res; end if;
    
    res(15 downto 10) := i2slv(insDef.j, 6);

    if insDef.k = -1 then return res; end if;

    res(4 downto 0) := i2slv(insDef.k, 5);
    
    return res;
end function;



function TMP_processStrings(cmd, a0, a1, a2, a3: string) return Word is
    variable mnem: ProcMnemonic;
    
    variable fmt: NEW_Format;
    variable desc: NEW_FormatDescription;
    variable args: IntArray(0 to 3) := (others => 0);
    variable w: Word;
    variable argList: ArgArray;
    variable undefOffset: boolean := false;
begin
    mnem := undef;
    for m in ProcMnemonic loop
        if matches(cmd, ProcMnemonic'image(m)) then
            mnem := m;
        end if;
    end loop;
    
    fmt := FormatList(mnem);
    desc := FormatDescriptions(fmt);
    
    argList := TMP_orderArgs(a0, a1, a2, a3, desc);
    
    if not TMP_checkArgs(argList, desc) then
        report "Wrong args!";     
    end if;
    
    args := TMP_getArgs(argList);
    
        -- TMP
        if argList(2)(1) = '$' then
            undefOffset := true;
        end if;
    
    w := TMP_fillArgs(args, desc, undefOffset);
    
    -- Fill op spec
    w := TMP_fillOp(w, findEncoding(mnem));
    
    return w;
end function;


function decodeMnem2(w: Word) return ProcMnemonic is
    variable mnem: ProcMnemonic := undef;
    variable i, j, k: integer;
    variable insDef: InstructionDefinition;
begin
    i := slv2u(w(31 downto 26));
    j := slv2u(w(15 downto 10));
    k := slv2u(w(4 downto 0));

    insDef := getDef(i, j, k);
    mnem := insDef.mnem;
    
    return mnem;
end function;

function TMP_disasm(w: Word) return string is
    constant mnem: ProcMnemonic := decodeMnem2(w);
    constant fmt: NEW_Format := FormatList(mnem);
    constant fmtDesc: NEW_FormatDescription := FormatDescriptions(fmt);
    
    constant qa: slv5 := w(25 downto 21);
    constant qb: slv5 := w(20 downto 16);
    constant qc: slv5 := w(9 downto 5);
    constant qd: slv5 := w(4 downto 0);
    variable d, s0, s1, s2, immValue: integer := 0;
    variable sources: IntArray(0 to 2) := (others => 0);
    variable lastArg: boolean := false;
    
    variable res: string(1 to 30) := (others => ' ');
    variable rd, r0, r1, r2: string(1 to 3);
    variable argStrings: ArgArray := (others => (others => ' '));
    variable destString, tmpString: string(1 to 10) := (others => ' ');
    
    variable ptr: integer := 0;
    
    -- TMP, remove
    function reg2str(n: natural; fp: boolean) return string is
        variable res: string(1 to 3) := "r00";
    begin
        assert n < 32 report "Register number too large: " & natural'image(n) severity error;
    
        if fp then
            res(1) := 'f';
        end if;
        
        if n < 10 then
            res(3 to 3) := natural'image(n);
        else
            res(2 to 3) := natural'image(n);    
        end if;
        
        return res;
    end function;
begin
    
    case fmtDesc.decoding(1) is
        when 'a' =>
            d := slv2u(qa);
        when 'b' =>
            d := slv2u(qb);
        when 'c' =>
            d := slv2u(qc);
        when 'd' =>
            d := slv2u(qd);        
        when others =>
    end case;

    for i in 0 to 2 loop
        case fmtDesc.decoding(3 + i) is
            when 'a' =>
                sources(i) := slv2u(qa);
            when 'b' =>
                sources(i) := slv2u(qb);
            when 'c' =>
                sources(i) := slv2u(qc);
            when 'd' =>
                sources(i) := slv2u(qd);        
            when 'X' =>
                sources(i) := slv2s(w(9 downto 0));
            when 'H' =>
                sources(i) := slv2s(w(15 downto 0));
            when 'J' =>
                sources(i) := slv2s(w(20 downto 0));
            when 'L' =>
                sources(i) := slv2s(w(25 downto 0));                                                                                    
            when others =>
        end case;        
    end loop;
    
    -- Use fmtDesc.typeSpec to convert nums to strings
    case fmtDesc.typeSpec(1) is
       when 'i' =>
           destString := padLeft(reg2str(d, false), 10);
       when 'f' =>
           destString := padLeft(reg2str(d, true), 10);
       when others =>
    end case;    
    
    for i in 0 to 2 loop
        case fmtDesc.typeSpec(3 + i) is
           when 'i' =>
               argStrings(i) := padLeft(reg2str(sources(i), false), 10);
           when 'f' =>
               argStrings(i) := padLeft(reg2str(sources(i), true), 10);
           when 'c' =>
               argStrings(i) := padLeft(integer'image(sources(i)), 10);
           when others =>
        end case;        
    end loop;
    
    -- Use fmtDesc.asmForm to order proper strings
    res(1 to 10) := padLeft(ProcMnemonic'image(mnem), 10);
    res(11) := ' ';
    -- arg: 3
    -- ...
    ptr := 12;
    for i in 0 to 3 loop
        case fmtDesc.asmForm(1 + i) is
            when 'd' =>
                tmpString := destString;
            when '0' =>
                tmpString := argStrings(0);
            when '1' =>
                tmpString := argStrings(1);
            when '2' =>
                tmpString := argStrings(2);
                                    
            when others =>
        end case;
    
        if i = 3 or fmtDesc.asmForm(1 + i + 1) = ' ' then
            lastArg := true;
        end if;

        -- copy text before first space, inc ptr by the amount    
        for j in 0 to 9 loop
            if tmpString(j+1) = ' ' then
                exit;
            end if;
            res(ptr) := tmpString(j+1);
            ptr := ptr + 1;
        end loop;
        
        if lastArg then
            exit;
        else
            res(ptr to ptr + 1) := ", ";            
            ptr := ptr + 2;
        end if;
    end loop;
    
    return res;
end function;


end InstructionSet;
