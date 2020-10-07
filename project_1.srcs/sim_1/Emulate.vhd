
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.Assembler.all;


package Emulate is


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
constant FMT_SHIFT:      FormatSpec := ('1', '0', '0', '0', '0', '0', '0', "000");
constant FMT_INT_STORE:  FormatSpec := ('1', '1', '0', '0', '1', '1', '0', "000"); -- Equal to FMT_IMM16?
constant FMT_FP_REG:     FormatSpec := ('0', '0', '0', '0', '0', '0', '1', "111");
constant FMT_FP_LOAD:    FormatSpec := ('1', '1', '0', '0', '0', '0', '1', "000"); -- Equal to FMT_IMM16?
constant FMT_FP_STORE:   FormatSpec := ('1', '1', '0', '0', '1', '1', '0', "001"); -- Equal to FMT_IMM16?
constant FMT_JUMP:       FormatSpec := ('1', '0', '0', '1', '0', '0', '0', "000");
constant FMT_JUMP_LINK:  FormatSpec := ('1', '0', '1', '0', '0', '0', '0', "000");
constant FMT_JUMP_COND:  FormatSpec := ('1', '0', '1', '0', '1', '0', '0', "000");


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
    
    memRead: std_logic;
    sysRead: std_logic;
    memWrite: std_logic;
    sysWrite: std_logic;
    
    intWrite: std_logic;
    floatWrite: std_logic;

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
    -- ....
    
    
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



end package;



package body Emulate is


function bin2opcode(v: std_logic_vector) return ProcOpcode is
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

function bin2opcont(opcode: ProcOpcode; v: std_logic_vector) return ProcOpcont is
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
        imm(31 downto 26) := (others => '0');
    elsif fmt.imm20 = '1' then
        imm(31 downto 20) := (others => '0');
    elsif fmt.imm16 = '1' then
        imm(31 downto 16) := (others => '0');
    else
        imm(31 downto 10) := (others => '0');
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



function decode(adr: Mword; w: Word) return InternalOperation is
    variable res: InternalOperation;
    constant opcode: ProcOpcode := bin2opcode(w(31 downto 26));
    constant opcont: ProcOpcont := bin2opcont(opcode, w(15 downto 10));

    variable mnem: ProcMnemonic := undef;
    variable fmt: FormatSpec := FMT_DEFAULT;
    variable desc: OpDescription := DESC_DEFAULT;
begin
    -- Get menemonic
    mnem := getMnemonic(opcode, opcont);
    
    -- Get operation type description
    fmt := OP_TABLE(mnem).format;
    desc := OP_TABLE(mnem).desc;
    
    -- Get arg specifications
    res := getArgSpec(fmt, desc, w);
    
    
    res.operation := OP_TABLE(mnem).op;
    res.ip := adr;
    
    return res;
end function;


-- TEMP Maybe change to use abstract operation type, add side effects
procedure performOp(state: inout CoreState; memory: inout ByteArray; op: in InternalOperation) is
    
begin
    
    
end procedure;


procedure TMP_run(state: inout CoreState; memory: inout ByteArray; program: in WordArray; startAddress: in Mword) is
begin
    
end procedure;



end package body;
