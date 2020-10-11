
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.Assembler.all;

use work.Arith.all;

package Emulate is

function memReadWord(memory: ByteArray; address: Mword) return Mword;
function memReadDword(memory: ByteArray; address: Mword) return Mword;
function memReadMword(memory: ByteArray; address: Mword) return Mword;

procedure memWriteWord(signal memory: inout ByteArray; address: in Mword; data: in Word);
procedure memWriteDword(signal memory: inout ByteArray; address: in Mword; data: in Dword);
procedure memWriteMword(signal memory: inout ByteArray; address: in Mword; data: in Mword);


-- TODO: probably should be defined in ArchDefs!
--type MnemonicCoding is record
--    mnemonic: ProcMnemonic;
    
--    opcode: ProcOpcode;
--    opcont: ProcOpcont;
--end record;


type OperationCoding is record
    opcode: ProcOpcode;
    opcont: ProcOpcont;
end record; 

type FormatSpec is record    
    imm: std_logic;
    imm16: std_logic;
    imm20: std_logic;
    imm26: std_logic;
    arg0inA: std_logic;
    arg2inA: std_logic;
    fpDestSelect: std_logic;
    fpArgSelect: std_logic_vector(0 to 2); 
end record;

--                                      imm   16   20   26   0a   2a   fp  fpArgs
constant FMT_DEFAULT:    FormatSpec := ('0', '0', '0', '0', '0', '0', '0', "000");  
constant FMT_IMM16:      FormatSpec := ('1', '1', '0', '0', '0', '0', '0', "000");
constant FMT_INT_REG:    FormatSpec := ('0', '0', '0', '0', '0', '0', '0', "000");
constant FMT_INT_LOAD:   FormatSpec := ('1', '1', '0', '0', '0', '0', '0', "000"); -- Equal to FMT_IMM16?
constant FMT_IMM10:      FormatSpec := ('1', '0', '0', '0', '0', '0', '0', "000");
constant FMT_INT_STORE:  FormatSpec := ('1', '1', '0', '0', '1', '1', '0', "000");
constant FMT_FP_REG:     FormatSpec := ('0', '0', '0', '0', '0', '0', '1', "111");
constant FMT_FP_LOAD:    FormatSpec := ('1', '1', '0', '0', '0', '0', '1', "000");
constant FMT_FP_STORE:   FormatSpec := ('1', '1', '0', '0', '1', '1', '0', "001");
constant FMT_JUMP:       FormatSpec := ('1', '0', '0', '1', '0', '0', '0', "000");
constant FMT_JUMP_LINK:  FormatSpec := ('1', '0', '1', '0', '0', '0', '0', "000");
constant FMT_JUMP_COND:  FormatSpec := ('1', '0', '1', '0', '1', '0', '0', "000");
constant FMT_JUMP_REG:   FormatSpec := ('0', '0', '0', '0', '0', '0', '0', "000"); -- Equal to default?
constant FMT_SHIFT:      FormatSpec := FMT_IMM10;

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
    
    add,
    sub,
    muls,
    mulu,
    divs,
    divu,
    
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
    opcode: ProcOpcode;
    opcont: ProcOpcont;
    format: FormatSpec;
    desc:   OpDescription;
    op:     AbstractOperation;
end record;

 
--type MnemonicCodingTable is array(ProcMnemonic range <>) of OperationCoding;
type OpTable is array(ProcMnemonic range <>) of OpTableRow;

constant OP_TABLE: OpTable(ProcMnemonic'left to ProcMnemonic'right) := (
    and_i => (andI, none,   FMT_IMM16,  DESC_INT, logicAnd),
    or_i  => (orI,  none,   FMT_IMM16,  DESC_INT, logicOr),
    
    add_i => (addI, none,   FMT_IMM16,  DESC_INT, add),
    
    shl_i => (ext0, shlC,   FMT_SHIFT,  DESC_INT, logicShift), 
    
    --shl_r, -- direction defined by shift value, not opcode 
    --sha_i, sha_r    
    
    add_r => (ext0, addR,   FMT_INT_REG, DESC_INT, add), 
    sub_r => (ext0, subR,   FMT_INT_REG, DESC_INT, sub), 

    and_r => (ext0, andR,   FMT_INT_REG, DESC_INT, logicAnd), 
    or_r =>  (ext0, orR,    FMT_INT_REG, DESC_INT, logicOr), 
    
    ja =>    (j,    none,   FMT_JUMP,         DESC_JUMP, j),
    jl =>    (jl,   none,   FMT_JUMP_LINK,    DESC_JUMP, jl),
    jz_i =>  (jz,    none,   FMT_JUMP_COND,    DESC_JUMP, jz),
    jnz_i => (jnz,    none,   FMT_JUMP_COND,    DESC_JUMP, jnz),
    
    jz_r =>  (ext1,    jzR,   FMT_JUMP_REG,    DESC_JUMP, jz),
    jnz_r => (ext1,    jnzR,   FMT_JUMP_REG,    DESC_JUMP, jnz),    
    
    ldi_i => (ld,       none,  FMT_INT_LOAD,    DESC_INT_LOAD, ldi),
    ldf_i => (ldf,       none,  FMT_FP_LOAD,    DESC_FP_LOAD, ldf),
    sti_i => (st,       none,  FMT_INT_STORE,    DESC_INT_STORE, sti),
    stf_i => (stf,       none,  FMT_FP_STORE,    DESC_FP_STORE, stf),
    -- ....
    
    lds  =>  (ext2,     mfc,   FMT_IMM10,    DESC_SYS_LOAD, mfc),   -- ??
    sts  =>  (ext2,     mtc,   FMT_IMM10,    DESC_SYS_STORE, mtc),  -- ??
    
    -- NOTE: sys decoding must be implemented differently because there are no distinct mnemonics
    
    others => (undef, undef, FMT_DEFAULT, DESC_DEFAULT, undef)
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




    -- TMP
    function decode(adr: Mword; w: Word) return InternalOperation;
    procedure performOp(signal state: inout CoreState; signal memory: inout ByteArray; op: in InternalOperation; signal outSigs: out std_logic_vector(0 to 2));



end package;



package body Emulate is


function bin2opcode(v: std_logic_vector(5 downto 0)) return ProcOpcode is
begin
    case v(5 downto 0) is
        when "000000" => return andI;
        when "000001" => return orI;
        when "000010" => return addI;
        when "000011" => return subI;

        when "000100" => return jz;
        when "000101" => return jnz;
        when "000110" => return j;
        when "000111" => return jl;
            
        when "001000" => return ld;
        when "001001" => return st;
        when "001010" => return ldf;
        when "001011" => return stf;

        when "001100" => return ext0;
        when "001101" => return ext1;
        when "001110" => return ext2;
        when "001111" => return fop;
                  
        when others   => return undef;
    end case;    
    
end function;

function bin2opcont(opcode: ProcOpcode; v: std_logic_vector(5 downto 0)) return ProcOpcont is
begin
    case opcode is
        --        
        when ext0 =>
            case v(5 downto 0) is
                when "000000" => return andR;
                when "000001" => return orR;
                when "000010" => return shlC;
                when "000011" => return shaC;
        
                when "000100" => return addR;
                when "000101" => return subR;
                when "000110" => return muls;
                when "000111" => return mulu;
                    
                when "001000" => return divs;
                when "001001" => return divu;
          
                when others => return undef;
            end case;
        
        --
        when ext1 =>
            case v(5 downto 0) is
                when "000000" => return jzR;
                when "000001" => return jnzR;

                when others => return undef;
            end case;        
        
        --      
        when ext2 =>
            case v(5 downto 0) is
                when "000000" => return retE;
                when "000001" => return retI;
                when "000010" => return halt;
                when "000011" => return sync;
        
                when "000100" => return replay;
                when "000101" => return error;
                when "000110" => return call;
                when "000111" => return send;
                    
                when "001000" => return mfc;
                when "001001" => return mtc;                
                
                when others => return undef;
            end case;          
        
        --
        when fop =>
            case v(5 downto 0) is
                when "000000" => return fmov;
                when "000001" => return forr;
                
                when others => return undef;
            end case;
            
        when others => return none;
    end case;
    
end function;

function getSystemOp(opcode: ProcOpcode; opcont: ProcOpcont) return AbstractOperation is
begin
    if opcode = ext2 then
        case opcont is
            when retE => return retE;
            when retI => return retI;
            when halt => return halt;
            when sync => return sync;
            when replay => return replay;
            when error => return error;
            when call => return call;
            when send => return send;
            
            when others => return undef;
        end case;
    end if;
    
    return undef;   
end function;


function getMnemonic(opcode: ProcOpcode; opcont: ProcOpcont) return ProcMnemonic is
begin
    for m in ProcMnemonic'left to ProcMnemonic'right loop
        if OP_TABLE(m).opcode = opcode and OP_TABLE(m).opcont = opcont then
            return m;
        end if;
    end loop;
    
    return undef;
end function;


function getArgSpec(fmt: FormatSpec; desc: OpDescription; w: Word) return InternalOperation is
    variable res: InternalOperation;
    constant qa: slv5 := w(25 downto 21);    
    constant qb: slv5 := w(20 downto 16);    
    constant qc: slv5 := w(9 downto 5);    
    constant qd: slv5 := w(4 downto 0);
    
    variable dest: slv5 := (others => '0');
    variable imm: Word := w;  
begin
    
    res.intSources := (qb, qc, qd);
    if fmt.arg0inA = '1' then
        res.intSources(0) := qa;
    end if;
    if fmt.arg2inA = '1' then
        res.intSources(2) := qa;
    end if;
    
    res.floatSources := res.intSources;
    
    res.hasImm := fmt.imm;
    
    if fmt.imm26 = '1' then
        imm(31 downto 26) := (others => imm(25));
    elsif fmt.imm20 = '1' then
        imm(31 downto 20) := (others => imm(19));
    elsif fmt.imm16 = '1' then
        imm(31 downto 16) := (others => imm(15));
    else
        imm(31 downto 10) := (others => imm(9));
    end if;
    
    res.imm := imm;
    
    if fmt.arg0inA = '1' or fmt.arg2inA = '1' then
        -- dest stays 0
    else
        dest := qa;
    end if;
    
    res.intDest := dest;
    res.floatDest := dest;
    res.hasIntDest := desc.writeInt;
    res.hasFloatDest := desc.writeFloat;
    
    
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
        bytes(i) := data(8*i + 7 downto 8*i);
    end loop;

    for i in 0 downto 3 loop
        memory(index + i) <= bytes(i);
    end loop;
end procedure;

procedure memWriteDword(signal memory: inout ByteArray; address: in Mword; data: in Dword) is
    variable res: Mword;
    constant index: natural := slv2u(address);
    variable bytes: ByteArray(0 to 7);    
begin
    for i in 7 downto 0 loop
        bytes(i) := data(8*i + 7 downto 8*i);
    end loop;

    for i in 0 downto 7 loop
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





function decode(adr: Mword; w: Word) return InternalOperation is
    variable res: InternalOperation;
    constant opcode: ProcOpcode := bin2opcode(w(31 downto 26));
    constant opcont: ProcOpcont := bin2opcont(opcode, w(15 downto 10));

    variable mnem: ProcMnemonic := undef;
    variable fmt: FormatSpec := FMT_DEFAULT;
    variable desc: OpDescription := DESC_DEFAULT;
    variable isSystemOp: boolean := false;
    variable operation, systemOp: AbstractOperation := undef;
begin
    -- Get menemonic
    mnem := getMnemonic(opcode, opcont);
    
    -- Get operation type description
    -- Different track for system instructions
    systemOp := getSystemOp(opcode, opcont);
    if systemOp /= undef then
        isSystemOp := true;
        operation := systemOp;
        fmt := FMT_DEFAULT;
        desc := DESC_SYS_OP;
    else
        operation := OP_TABLE(mnem).op;
        fmt := OP_TABLE(mnem).format;
        desc := OP_TABLE(mnem).desc;        
    end if;
    
    -- Get arg specifications
    res := getArgSpec(fmt, desc, w);
    
    res := fillProperties(res, desc);
    
    res.operation := operation;
    res.ip := adr;
    
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

procedure performCalculation(intArgs: in MwordArray; fpArgs: in MwordArray; incrementedIP: in Mword; aop: in AbstractOperation; intResult: out Mword; fpResult: out Mword; outFlags: out std_logic_vector(0 to 2)) is    
    constant ia0: Mword := intArgs(0);
    constant ia1: Mword := intArgs(1);
    constant ia2: Mword := intArgs(2);
    constant fa0: Mword := fpArgs(0);
    constant fa1: Mword := fpArgs(1);
    constant fa2: Mword := fpArgs(2);
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
        
        
        when jl => intResult := incrementedIP;

        
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
procedure calculateNextIP(intArgs: in MwordArray; op: in InternalOperation; signal nextIP: out Mword) is
    variable takenJump: boolean := false;
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

        
    if false then -- events
    
    elsif takenJump then 
        nextIP <= add(op.ip, intArgs(1));
    else
        nextIP <= addInt(op.ip, 4);
    end if;
    
end procedure;




-- TEMP Maybe change to use abstract operation type, add side effects
procedure performOp(signal state: inout CoreState; signal memory: inout ByteArray; op: in InternalOperation; signal outSigs: out std_logic_vector(0 to 2)) is
    constant incrementedIP: Mword := addInt(op.ip, 4);
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
    elsif op.isSysLoad = '1' then
        memValue := state.sysRegs(slv2u(address)); -- TODO: handle out of range?
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
        memWriteMword(memory, address, intArgs(2));
    elsif op.isSysStore = '1' then
        state.sysRegs(slv2u(address)) <= intArgs(1);
    end if;
    
    -- Update IP and sys regs
    
    calculateNextIP(intArgs, op, state.nextIP);
    
    outSigs <= (others => '0');
end procedure;


procedure TMP_run(state: inout CoreState; memory: inout ByteArray; program: in WordArray; startAddress: in Mword) is
begin
    
end procedure;



end package body;
