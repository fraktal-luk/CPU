
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;
use work.InstructionSet.all;

use work.CpuText.all;


package Emulate is

        procedure divRem(a, b: Word; signed: boolean; quot, rm: out Word);


function memReadWord(memory: ByteArray; address: Mword) return Mword;
function memReadDword(memory: ByteArray; address: Mword) return Mword;
function memReadMword(memory: ByteArray; address: Mword) return Mword;

procedure memWriteWord(signal memory: inout ByteArray; address: in Mword; data: in Word);
procedure memWriteDword(signal memory: inout ByteArray; address: in Mword; data: in Dword);
procedure memWriteMword(signal memory: inout ByteArray; address: in Mword; data: in Mword);


type OpDescription is record
    jump:       std_logic;
   
    writeInt:   std_logic;
    writeFloat: std_logic;
    
    readMem:    std_logic;
    writeMem:   std_logic;
    
    readSys:    std_logic;
    writeSys:   std_logic;
    
    changeSys:  std_logic; -- Changes to sys regs other than explicit write
end record;

--                                           j     i   f   rm  wm   rs  ws   cs
constant DESC_DEFAULT:   OpDescription := ('0',  '0','0', '0','0', '0','0', '0');
constant DESC_JUMP:      OpDescription := ('1',  '1','0', '0','0', '0','0', '0'); -- Writes int reg, use r0 when doesn't want
constant DESC_INT:       OpDescription := ('0',  '1','0', '0','0', '0','0', '0');
constant DESC_FP:        OpDescription := ('0',  '0','1', '0','0', '0','0', '0');
constant DESC_INT_LOAD:  OpDescription := ('0',  '1','0', '1','0', '0','0', '0');
constant DESC_FP_LOAD:   OpDescription := ('0',  '0','1', '1','0', '0','0', '0');
constant DESC_INT_STORE: OpDescription := ('0',  '0','0', '0','1', '0','0', '0');
constant DESC_FP_STORE:  OpDescription := ('0',  '0','0', '0','1', '0','0', '0');
constant DESC_SYS_LOAD:  OpDescription := ('0',  '1','0', '0','0', '1','0', '0');
constant DESC_SYS_STORE: OpDescription := ('0',  '0','0', '0','0', '0','1', '0');
constant DESC_SYS_OP:    OpDescription := ('0',  '0','0', '0','0', '0','0', '1');


type CoreState is record
    nextIP: Mword;
    sysRegs: MwordArray(0 to 31);
    intRegs: MwordArray(0 to 31);
    floatRegs: MwordArray(0 to 31);
    
end record;

constant INIT_SYS_REGS: MwordArray(0 to 31) := (
    0 => (others => '1'), -- TMP
    
    others => (others => '0') -- TMP?
);

constant INIT_CORE_STATE: CoreState := (
    nextIP => i2slv(512, MWORD_SIZE), -- TODO: define START_ADDRESS as architectural constant!
    sysRegs => INIT_SYS_REGS,
    intRegs => (0 => (others => '0'), others => (others => 'U')),
    floatRegs => (others => (others => 'U'))
);


-- CAUTION: dynamic results can be implementation specific. Memory order fixes can't be predicted by non-exact simulation.
--          Phenomena that are not Visible can occur in hardware simulation without occuring in simple emulation.
type OperationResult is record
    visible: std_logic; -- Takes architectural effect - normal result or exception. Refetches for memory order fix aren't Visible
    normal:    std_logic; -- Happened without special events - intentional control events are not Normal. Refetches for memory order are not Normal
    dynamic:   std_logic; -- Causes control events determined at execution level: exception, memory order fix,...
                            -- Exception return or system call are not dynamic. External interrupt DOES NOT imply dynamic.
                            -- Undefined instructions are not dynamic.
                            -- TODO: what about privilege violations? Probably dynamic exceptions, unless a special instruction for "illegal access" exists
    exception: std_logic;
    interrupt: std_logic;
    
    jump:    std_logic;
    target:  Mword;
    
    memRead: std_logic;
    sysRead: std_logic;
    memWrite: std_logic;
    sysWrite: std_logic;
    readAddress: Mword;
    writeAddress: Mword;
    writeResult: Mword;
    
    intWrite: std_logic;
    intResult: Mword;
    floatWrite: std_logic;
    floatResult: Mword;
end record;


type AbstractOperation is (
    nop,
    
    logicAnd,
    logicOr,
    logicXor,
    logicShift,
    
    arithShift,
    rotate,
    
    add,
    sub,
    
    addHigh,
    
    mul,
    mulhs,
    mulhu,
    divs,
    divu,
    rems,
    remu,
    
    ldi,
    sti,
    ldf,
    stf,
    
    fpMove,
    fpOr,
    
    mfc,
    mtc,
    
    j,
    jl,
    jz,
    jnz,
    
    
    retE,
    retI,
	halt,
    sync,
    replay,
    error,
    call,
    send,
        
    undef
);

type OpTableRow is record
    desc:   OpDescription;
    op:     AbstractOperation;
end record;

 
type OpTable is array(ProcMnemonic range <>) of OpTableRow;

constant OP_TABLE: OpTable(ProcMnemonic'left to ProcMnemonic'right) := (
    and_i => (DESC_INT, logicAnd),
    or_i  => (DESC_INT, logicOr),
    
    add_i => (DESC_INT, add),
    
    add_h => (DESC_INT, addHigh),
    
    shl_i => (DESC_INT, logicShift),

    --shl_r, -- direction defined by shift value, not opcode
    --sha_i, sha_r
    
    add_r => (DESC_INT, add),
    sub_r => (DESC_INT, sub),

    and_r => (DESC_INT, logicAnd),
    or_r =>  (DESC_INT, logicOr),
    
        mult => (DESC_INT, mul),
        mulh_u => (DESC_INT, mulhu),
        mulh_s => (DESC_INT, mulhs),
        div_u => (DESC_INT, divu),
        div_s => (DESC_INT, divs),
        rem_u => (DESC_INT, remu),
        rem_s => (DESC_INT, rems),
        
    ja =>    (DESC_JUMP, j),
    jl =>    (DESC_JUMP, jl),
    jz_i =>  (DESC_JUMP, jz),
    jnz_i => (DESC_JUMP, jnz),
    
    jz_r =>  (DESC_JUMP, jz),
    jnz_r => (DESC_JUMP, jnz),
    
    ldi_i => (DESC_INT_LOAD, ldi),
    ldf_i => (DESC_FP_LOAD, ldf),
    sti_i => (DESC_INT_STORE, sti),
    stf_i => (DESC_FP_STORE, stf),
    -- ....
    
    lds  =>  (DESC_SYS_LOAD, mfc),   -- ??
    sts  =>  (DESC_SYS_STORE, mtc),  -- ??
    
    mov_f => (DESC_FP,  fpMove),
    or_f =>  (DESC_FP,  fpOr),
    
    
    -- NOTE: sys decoding must be implemented differently because there are no distinct mnemonics
    
    others => (DESC_DEFAULT, undef)
);



type InternalOperation is record
    ip: Mword;
    operation: AbstractOperation;
    
    isJump:     std_logic;
    hasImm:     std_logic;
    imm:        Word;
    
    intSources: RegNameArray(0 to 2);
    floatSources: RegNameArray(0 to 2);

    hasIntDest: std_logic;
    intDest: RegName;
    hasFloatDest: std_logic;
    floatDest: RegName;
    isMemLoad: std_logic;
    isSysLoad: std_logic;
    isMemStore: std_logic;
    isSysStore: std_logic;
    affectsSys: std_logic;
    
end record;

function defaultInternalOp return InternalOperation;

constant DEFAULT_INTERNAL_OPERATION: InternalOperation := --defaultInternalOp;
(
    ip => (others => 'U'),
    operation => nop,
    imm => (others => '0'),
    intSources => (others => "00000"),
    floatSources => (others => "00000"),
    intDest => "00000",
    floatDest => "00000",
    others => '0'    
);

constant DEFAULT_INTERNAL_OP: InternalOperation := DEFAULT_INTERNAL_OPERATION;

function decodeAbstract(adr: Mword; w: Word) return InternalOperation;

procedure performOp(signal state: inout CoreState; signal memory: inout ByteArray; op: in InternalOperation;
                    signal outSigs: out std_logic_vector(0 to 2);
                    result: out OperationResult);
end package;



package body Emulate is

function defaultInternalOp return InternalOperation is
    variable res: InternalOperation;
begin
    return res;
end function;


function getSystemOperation(mnem: ProcMnemonic) return AbstractOperation is
begin
    case mnem is
        when sys_retE => return retE;
        when sys_retI => return retI;
        when sys_halt => return halt;
        when sys_sync => return sync;
        when sys_replay => return replay;
        when sys_error => return error;
        when sys_call => return call;
        when sys_send => return send;
        
        when others => return undef;
    end case;
    
    return undef;
end function;


function getArgSpec2(new_fmt: NEW_Format; new_desc: NEW_FormatDescription; w: Word) return InternalOperation is
    variable res: InternalOperation := DEFAULT_INTERNAL_OPERATION;
    constant qa: RegName := w(25 downto 21);
    constant qb: RegName := w(20 downto 16);
    constant qc: RegName := w(9 downto 5);
    constant qd: RegName := w(4 downto 0);
    
    variable dest: RegName := (others => '0');
    variable sources: RegNameArray(0 to 2) := (others => (others => '0'));
    variable imm: Word := w;
begin
    if new_desc.decoding(1) = 'a' then
        dest := qa;
    end if;
    
    for i in 0 to 2 loop
        if new_desc.decoding(3 + i) = 'a' then
            sources(i) := qa;
        elsif new_desc.decoding(3 + i) = 'b' then
             sources(i) := qb;
        elsif new_desc.decoding(3 + i) = 'c' then
             sources(i) := qc;
        elsif new_desc.decoding(3 + i) = 'd' then
             sources(i) := qd;
        end if;
    end loop;
    
    res.hasImm := '1';
    if new_desc.decoding(4) = 'L' then    
        imm(31 downto 26) := (others => imm(25));
    elsif new_desc.decoding(4) = 'J' then    
        imm(31 downto 21) := (others => imm(20));
    elsif new_desc.decoding(4) = 'H' then    
        imm(31 downto 16) := (others => imm(15));
    elsif new_desc.decoding(4) = 'X' then    
        imm(31 downto 10) := (others => imm(9));
    else
        res.hasImm := '0';
        imm := (others => '0');
    end if;
    
    res.intSources := sources;
    res.floatSources := sources;
    
    res.imm := imm;
    
    res.intDest := dest;
    res.floatDest := dest;
    
    res.hasIntDest := '0';    
    res.hasFloatDest := '0';    
    if new_desc.typeSpec(1) = 'i' then
        res.hasIntDest := '1';    
    elsif new_desc.typeSpec(1) = 'f' then
        res.hasFloatDest := '1';
    end if;    
    
    return res;
end function;


function fillProperties(op: InternalOperation; desc: OpDescription) return InternalOperation is
    variable res: InternalOperation := op;
begin
    res.isJump := desc.jump;

    res.isMemLoad := desc.readMem;
    res.isMemStore := desc.writeMem;
    res.isSysLoad := desc.readSys;
    res.isSysStore := desc.writeSys;
    
    res.affectsSys := desc.changeSys;
       
    return res;
end function;


function memReadWord(memory: ByteArray; address: Mword) return Mword is
    variable res: Mword;
    constant index: natural := slv2u(address);
    variable bytes: ByteArray(0 to 3);
begin
    for i in 0 to 3 loop
        bytes(i) := memory(index + i);
    end loop;
    
    res := bytes(0) & bytes(1) & bytes(2) & bytes(3);
    return res;
end function;

function memReadDword(memory: ByteArray; address: Mword) return Mword is
    variable res: Mword;
    constant index: natural := slv2u(address);
    variable bytes: ByteArray(0 to 7);
begin
    for i in 0 to 7 loop
        bytes(i) := memory(index + i);
    end loop;
    
    res := bytes(0) & bytes(1) & bytes(2) & bytes(3)
         & bytes(4) & bytes(5) & bytes(6) & bytes(7);
    return res;
end function;


function memReadMword(memory: ByteArray; address: Mword) return Mword is
    variable res: Mword;
begin
    if MWORD_SIZE = 64 then
        return memReadDword(memory, address);
    else
        return memReadWord(memory, address);
    end if;
    
    return res;
end function;


procedure memWriteWord(signal memory: inout ByteArray; address: in Mword; data: in Word) is
    variable res: Mword;
    constant index: natural := slv2u(address);
    variable bytes: ByteArray(0 to 3);
begin
    for i in 3 downto 0 loop
        bytes(3-i) := data(8*i + 7 downto 8*i); -- Big endian!
    end loop;

    for i in 0 to 3 loop
        memory(index + i) <= bytes(i);
    end loop;
end procedure;

procedure memWriteDword(signal memory: inout ByteArray; address: in Mword; data: in Dword) is
    variable res: Mword;
    constant index: natural := slv2u(address);
    variable bytes: ByteArray(0 to 7);
begin
    for i in 7 downto 0 loop
        bytes(7-i) := data(8*i + 7 downto 8*i); -- Big endian!
    end loop;

    for i in 0 to 7 loop
        memory(index + i) <= bytes(i);
    end loop;
end procedure;

procedure memWriteMword(signal memory: inout ByteArray; address: in Mword; data: in Mword) is
begin
    if MWORD_SIZE = 64 then
        memWriteDword(memory, address, data);
    else
        memWriteWord(memory, address, data);
    end if;
end procedure;


function decodeAbstract(adr: Mword; w: Word) return InternalOperation is
    variable res: InternalOperation;
    variable res2: InternalOperation;
    variable mnem: ProcMnemonic := undef;
    variable desc: OpDescription := DESC_DEFAULT;
    variable isSystemOp: boolean := false;
    variable operation, systemOp: AbstractOperation := undef;
    variable i, j, k: integer;
    variable insDef: InstructionDefinition;
    variable new_fmt: NEW_Format;
    variable new_desc: NEW_FormatDescription;
begin
    i := slv2u(w(31 downto 26));
    j := slv2u(w(15 downto 10));
    k := slv2u(w(4 downto 0));

    insDef := getDef(i, j, k);

    mnem := insDef.mnem;
    case mnem is
        when
            sys_retE |
            sys_retI |
            sys_halt |
            sys_sync |
            sys_replay |
            sys_error |
            sys_call |
            sys_send
            =>

            isSystemOp := true;
        when others =>            
    end case;

    new_fmt :=  FormatList(mnem);
    new_desc := FormatDescriptions(new_fmt); -- NOTE: description of format, not of operation like 'desc'

    if isSystemOp then
        operation := getSystemOperation(mnem);
        desc := DESC_SYS_OP;
    else
        operation := OP_TABLE(mnem).op;
        desc := OP_TABLE(mnem).desc;
    end if;
    
    -- Get arg specifications
    res := getArgSpec2(new_fmt, new_desc, w);    
    res := fillProperties(res, desc);
    
    res.operation := operation;
    res.ip := adr;
    
    return res;
end function;



-- TODO: replace with reg2str
function reg2txt(n: natural; fp: boolean) return string is
    variable res: string(1 to 3) := "r00";
begin
    assert n < 32 report "Register number too large: " & natural'image(n) severity error;

    if fp then
        res(1) := 'f';
    end if;
    
    if n < 0 then
        null;
    elsif n < 10 then
        res(3 to 3) := natural'image(n);
    elsif n < 32 then
        res(2 to 3) := natural'image(n);
    end if;
    
    return res;
end function;


procedure getArgs(state: in CoreState; op: in InternalOperation; intArgs: out MwordArray(0 to 2); fpArgs: out MwordArray(0 to 2)) is
    variable intRegNum, fpRegNum: natural := 0;
begin
    for i in 0 to 2 loop
        intRegNum := slv2u(op.intSources(i));
        fpRegNum := slv2u(op.floatSources(i));
        intArgs(i) := state.intRegs(intRegNum);
        fpArgs(i) := state.floatRegs(fpRegNum);
    end loop;
    
    if op.hasImm = '1' then
        intArgs(1) := op.imm;
    end if;
end procedure;


    
function multiplyWords(a, b: Word; signed: boolean := false) return Dword is
    variable res, shifted: Dword := (others => '0');
begin
    if signed then
        shifted := signExtend(b, 64);
    else
        shifted := zeroExtend(b, 64);
    end if;
    
    for i in 0 to 30 loop
        if a(i) = '1' then
            res := add(res, shifted);
        end if;
        shifted := shifted(62 downto 0) & '0';
    end loop;

    if a(31) = '1' then
        if signed then
            res := sub(res, shifted);
        else
            res := add(res, shifted);
        end if;
    end if;
    
    return res;
end function;

function multiplyLow(a, b: Word) return Word is
    variable res: Word := (others => '0');
    variable longRes: Dword := multiplyWords(a, b);
begin
    res := longRes(31 downto 0);
    return res;
end function; 

function multiplyHigh(a, b: Word; signed: boolean := false) return Word is
    variable res: Word := (others => '0');
    variable longRes: Dword := multiplyWords(a, b, signed);
begin
    res := longRes(63 downto 32);
    return res;
end function;



function shiftLeft(w: Word; sh: natural) return Word is
    variable d: Dword := w & X"00000000";
    variable res: Word := d(63-sh downto 32-sh);
begin
    --return d(63-sh downto 32-sh);
    return res;
end function;

function shiftRightLogical(w: Word; sh: natural) return Word is
    variable d: Dword := zeroExtend(w, 64);
    variable res: Word := d(31+sh downto sh);
begin
    return --d(31+sh downto sh);
           res;
end function;

function shiftRightArithmetic(w: Word; sh: natural) return Word is
    variable d: Dword := signExtend(w, 64);
    variable res: Word := d(31+sh downto sh);
begin
    return --d(31+sh downto sh);
            res;
end function;


function addHigh(a, b: Word) return Word is
    constant bShifted: Word := shiftLeft(b, 16);
begin
    return add(a, bShifted);
end function;


procedure divRemU(a, b: Word; quot, rm: out Word) is
    variable aRem, bSh, result: Word := (others => '0');
    variable highA, highB, dlZ, k, resultIndex: natural := 0;

begin
    aRem := a;

    if isNonzero(b) /= '1' then
        quot := (others => 'U');
        rm := (others => 'U');
        return;
    end if;

    if isNonzero(aRem) /= '1' then
        quot := (others => '0');
        rm := (others => '0');
        return;
    end if;

    quot := (others => '0');

    for i in 31 downto 0 loop
        if aRem(i) = '1' then
            highA := i;
            exit;
        end if;
    end loop;

    for i in 31 downto 0 loop
        if b(i) = '1' then
            highB := i;
            exit;
        end if;
    end loop;
    
    if highB > highA then
        quot := (others => '0');
        rm := aRem;
        return;
    end if;

    -- Highest possible result bit:
    resultIndex := highA - highB;

    bSh := shiftLeft(b, resultIndex);
    loop
        if cmpGeU(aRem, bSh) = '1' then
            aRem := sub(aRem, bSh);
            result(resultIndex) := '1';
        end if;
        
        if resultIndex = 0 then
            exit;
        end if;
        bSh := shiftRightLogical(bSh, 1);
        resultIndex := resultIndex - 1;
    end loop;
    
    quot := result;
    rm := aRem;

end procedure;



procedure divRem(a, b: Word; signed: boolean; quot, rm: out Word) is
    variable sgA, sgB: std_logic := '0';
    variable aU, bU, qU, rU: Word := (others => '0');
begin
    sgA := a(31) and bool2std(signed);
    sgB := b(31) and bool2std(signed);
    
    if sgA = '1' then
        aU := minus(a);
    else
        aU := a;
    end if;

    if sgB = '1' then
        bU := minus(b);
    else
        bU := b;
    end if;
    
    divRemU(aU, bU, qU, rU);
    
    if (sgA xor sgB) = '1' then
        qU := minus(qU);
    end if;
    
    if sgA = '1' then
        rU := minus(rU);
    end if;
    
        if isNonzero(rU) = '1' and (sgA xor sgB) = '1' then
            qU := addInt(qU, -1);
            rU := add(rU, b);
        end if;
   -- end if;

    quot := qU;
    rm := rU;
end procedure;


function divide(a, b: Word; signed: boolean) return Word is
    variable res: Word := (others => '0');
    variable quot, rm: Word := (others => '0');
begin
    divRem(a, b, signed, quot, rm);
    return quot;
end function;

function remainder(a, b: Word; signed: boolean) return Word is
    variable res: Word := (others => '0');
    variable quot, rm: Word := (others => '0');
begin
    divRem(a, b, signed, quot, rm);
    return rm;
end function;


procedure performCalculation(intArgs: in MwordArray; fpArgs: in MwordArray; incrementedIP: in Mword; aop: in AbstractOperation; intResult: out Mword; fpResult: out Mword; outFlags: out std_logic_vector(0 to 2)) is
    constant ia0: Mword := intArgs(0);
    constant ia1: Mword := intArgs(1);
    constant ia2: Mword := intArgs(2);
    constant fa0: Mword := fpArgs(0);
    constant fa1: Mword := fpArgs(1);
    constant fa2: Mword := fpArgs(2);
    
    variable vDiv, vRem: Word;
begin
    intResult := (others => 'U');
    fpResult := (others => 'U');

    case aop is
        -- Int
        when logicAnd => intResult := ia0 and ia1;
        when logicOr => intResult := ia0 or ia1;
        when logicXor => intResult := ia0 xor ia1;
        when logicShift => intResult := bitLogicalShift(ia0, ia1);
        
        when arithShift => intResult := bitArithmeticShift(ia0, ia1);
        when add => intResult := add(ia0, ia1);
        when sub => intResult := sub(ia0, ia1);

        when addHigh => intResult := addHigh(ia0, ia1);

        when mul => intResult := multiplyLow(ia0, ia1); 
        when mulhu => intResult := multiplyHigh(ia0, ia1);
        when mulhs => intResult := multiplyHigh(ia0, ia1, true);

        when divu =>
            divRem(ia0, ia1, false, vDiv, vRem);
            intResult := vDiv;
            
        when divs =>
            divRem(ia0, ia1, true, vDiv, vRem);
            intResult := vDiv;
        
        when remu =>
            divRem(ia0, ia1, false, vDiv, vRem);
            intResult := vRem;
        
        when rems => 
            divRem(ia0, ia1, true, vDiv, vRem);
            intResult := vRem;

        when j | jl | jz | jnz => intResult := incrementedIP;
        
        -- FP
        when fpMove => fpResult := fa0;
        when fpOr   => fpResult := fa0 or fa1;
        
        when others =>
    end case;
    
    outFlags := (others => '0');
end procedure;


procedure calculateAddress(intArgs: in MwordArray; fpArgs: in MwordArray; aop: in AbstractOperation; address: out Mword) is
begin
    address := (others => 'U');

    case aop is
        when ldi | ldf | sti | stf =>   address := add(intArgs(0), intArgs(1));
        when mfc | mtc =>               address := intArgs(1);

        when others =>
    end case;
end procedure;


-- TODO: add exceptions etc
procedure calculateNextIP(intArgs: in MwordArray; op: in InternalOperation; nextIP: out Mword) is
    variable takenJump: boolean := false;
    variable target: Mword;
begin
    if op.isJump = '1' then
        case op.operation is
            when j | jl =>
                takenJump := true;
            when jz =>
                takenJump := isNonzero(intArgs(0)) = '0';
            when jnz =>
                takenJump := isNonzero(intArgs(0)) = '1';
            when others =>
        end case;
    end if;
  
    if op.hasImm = '1' then
        target := add(op.ip, intArgs(1));
    else
        target := intArgs(1);
    end if;
        
    if false then -- events
    
    elsif takenJump then
        nextIP := target;
    else
        nextIP := addInt(op.ip, 4);
    end if;
    
end procedure;


--    alias currentState is sysRegArray(1);
--    alias linkRegExc is sysRegArray(2);
--    alias linkRegInt is sysRegArray(3);
--    alias savedStateExc is sysRegArray(4);
--    alias savedStateInt is sysRegArray(5);

procedure performSystemOp(op: in InternalOperation; thisIP: in Mword; incIP: in Mword; excSignal, intSignal: in std_logic; signal sysRegs: inout MwordArray;
                          normalNextIP: in Mword; signal nextIP: out Mword) is
    variable exc: boolean := std2bool(excSignal);
    variable int: boolean := std2bool(intSignal);
    variable newIP: Mword := normalNextIP;
    variable isEvent: boolean := false;
begin
    case op.operation is
        when retE =>
            newIP := sysRegs(2);
            sysRegs(1) <= sysRegs(4); -- Restoring to Saved State

        when retI =>
            newIP := sysRegs(3);
            sysRegs(1) <= sysRegs(5); -- Restoring to Saved State
       
        when halt =>
            -- TODO
            
        when sync =>
            newIP := incIP;
           
        when replay =>
            newIP := thisIP;
           
        when error =>
            newIP := EXC_BASE;
            sysRegs(2) <= normalNextIP;
            sysRegs(4) <= sysRegs(1);
           
        when call =>
            newIP := CALL_BASE;
            sysRegs(2) <= normalNextIP;
            sysRegs(4) <= sysRegs(1);
                       
        when send =>
            newIP := incIP;

        when undef =>
            newIP := EXC_BASE; -- ???
            exc := true;
            
        when others =>
            newIP := normalNextIP;
    end case;
    
    if exc then
        sysRegs(2) <= normalNextIP;
        sysRegs(4) <= sysRegs(1);
        
         newIP := EXC_BASE;
    elsif int then
        sysRegs(3) <= normalNextIP;
        sysRegs(5) <= sysRegs(1);
        newIP := INT_BASE;
    end if;
    
    nextIP <= newIP;
end procedure;


procedure performOp(signal state: inout CoreState; signal memory: inout ByteArray; op: in InternalOperation;
                    signal outSigs: out std_logic_vector(0 to 2);
                    result: out OperationResult) is
    constant thisIP: Mword := op.ip;
    constant incrementedIP: Mword := addInt(op.ip, 4);
    variable normalNextIP: Mword; -- := addInt(op.ip, 4);
    variable intArgs: MwordArray(0 to 2);
    variable fpArgs:  MwordArray(0 to 2);
    variable intRes, fpRes, address, memValue: Mword;
    variable calcFlags: std_logic_vector(0 to 2);
begin
    -- Get arg values
    getArgs(state, op, intArgs, fpArgs);
    
    -- Calculation
    performCalculation(intArgs, fpArgs, incrementedIP, op.operation, intRes, fpRes, calcFlags);
    calculateAddress(intArgs, fpArgs, op.operation, address);
    
    -- Mem reading
    if op.isMemLoad = '1' then
        memValue := memReadMword(memory, address);
        if op.hasFloatDest = '1' then
            fpRes := memValue;
        elsif op.hasIntDest = '1' then
            intRes := memValue;
        end if;
    elsif op.isSysLoad = '1' then
        memValue := state.sysRegs(slv2u(address)); -- TODO: handle out of range?
        intRes := memValue;
    end if;
    
    -- Postprocessing
    
    
    -- Update regs
    -- TODO: don't do it if exception
    if op.hasIntDest = '1' and isNonzero(op.intDest) = '1' then
        state.intRegs(slv2u(op.intDest)) <= intRes;
    elsif op.hasFloatDest = '1' then
        state.floatRegs(slv2u(op.floatDest)) <= fpRes;
    end if;
    
    -- Mem store
    if op.isMemStore = '1' then            
        if op.operation = stf then
            memWriteMword(memory, address, fpArgs(2));
        else
            memWriteMword(memory, address, intArgs(2));
        end if;
    elsif op.isSysStore = '1' then
        state.sysRegs(slv2u(address)) <= intArgs(2);
    end if;
    
    -- Update IP and sys regs
    
    calculateNextIP(intArgs, op, normalNextIP);
    
    performSystemOp(op, thisIP, incrementedIP,
                        '0', '0',  -- TODO: Exec exception, interrupt
                        state.sysRegs, normalNextIP, state.nextIP);

    outSigs <= (others => '0');

        -- TMP
        if op.operation = error then
            outSigs <= "100";
        elsif op.operation = send then
            outSigs <= "001";
        end if;
    
end procedure;


procedure TMP_run(state: inout CoreState; memory: inout ByteArray; program: in WordArray; startAddress: in Mword) is
begin
    
end procedure;



end package body;