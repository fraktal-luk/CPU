

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;

use work.CoreConfig.all;

use work.PipelineGeneral.all;


package LogicIbuffer is

constant PTR_MASK_SN: SmallNumber := i2slv(IBUFFER_SIZE-1, SMALL_NUMBER_SIZE);
constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;    


constant MEM_WIDTH: natural := PIPE_WIDTH * 64; 


type BufferEntry is record
    full: std_logic;
    
    firstBr: std_logic; -- TEMP
    
    -- NOTE: for compresion maybe can be just 2 bits:
    --       (br NT, br T, br T confirmed, special) is 4 possibilities     
    branchIns: std_logic;
    frontBranch: std_logic;
    confirmedBranch: std_logic;
    specialAction: std_logic;
    
    --immSel: std_logic;
    fpRename: std_logic;           
    mainCluster: std_logic;
    secCluster: std_logic;
    useLQ:      std_logic;
    
    specificOperation: SpecificOp;

    constantArgs: InstructionConstantArgs;
    argSpec: InstructionArgSpec;
end record;

constant DEFAULT_BUFFER_ENTRY: BufferEntry := (
    specificOperation => sop(None, opNone),
    constantArgs => DEFAULT_CONSTANT_ARGS,
    argSpec => DEFAULT_ARG_SPEC,
    others => '0'
);

type BufferEntryArray is array(0 to PIPE_WIDTH-1) of BufferEntry;
type BufferEntryArray2D is array(0 to IBUFFER_SIZE-1, 0 to PIPE_WIDTH-1) of BufferEntry;

type SerialMemory is array(0 to IBUFFER_SIZE-1) of std_logic_vector(MEM_WIDTH-1 downto 0);

function formatInput(insVec: PipeStage) return PipeStage;

-- CAREFUL: only virtual because 5 bits per reg!
function serializeArgSpec(argSpec: InstructionArgSpec) return Word;
function deserializeArgSpec(w: Word) return InstructionArgSpec;

function serializeEntryArray(arr: BufferEntryArray) return std_logic_vector;
function deserializeEntryArray(v: std_logic_vector) return BufferEntryArray;

function getEntryArray(insVec: InstructionSlotArray) return BufferEntryArray;        
function getInsSlotArray(elemVec: BufferEntryArray) return InstructionSlotArray;

procedure updateQueue(signal content: inout BufferEntryArray2D; ptr: SmallNumber; newRow: BufferEntryArray);
function readQueue(content: BufferEntryArray2D; ptr: SmallNumber) return BufferEntryArray;

end package;



package body LogicIbuffer is

function formatInput(insVec: PipeStage) return PipeStage is
    variable res: PipeStage := insVec;
begin
    for i in res'range loop
        res(i).ins.controlInfo.newEvent := '0';
        
        if CLEAR_DEBUG_INFO then    
            res(i).ins := clearAbstractInfo(res(i).ins);
            res(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;
        end if;   
    end loop;            
    return res;
end function;


-- CAREFUL: only virtual because 5 bits per reg!
function serializeArgSpec(argSpec: InstructionArgSpec) return Word is
    variable res: Word := (others => '0');
    variable b: Byte := (others => '0');
begin
    for i in 0 to 2 loop
        b := '0' & argSpec.intArgSel(i) & argSpec.floatArgSel(i) & argSpec.args(i)(4 downto 0);
        res(7*i + 7 downto 7*i) := b;
    end loop;
    
    b := '0' & argSpec.intDestSel & argSpec.floatDestSel & argSpec.dest(4 downto 0);
    res(28 downto 21) := b;
    -- Bit 28 is '0' in the top byte!
                
    return res;
end function;

function deserializeArgSpec(w: Word) return InstructionArgSpec is
    variable res: InstructionArgSpec := DEFAULT_ARG_SPEC;
    variable b: Byte := (others => '0');
begin
    for i in 0 to 2 loop
        b := w(7*i + 7 downto 7*i);
        res.intArgSel(i) := b(6);
        res.floatArgSel(i) := b(5);
        res.args(i) := "000" & b(4 downto 0);
    end loop;

    b := w(28 downto 21);
    res.intDestSel := b(6);
    res.floatDestSel := b(5);
    res.dest := "000" & b(4 downto 0);
    return res;
end function;

function unfoldOp(op: SpecificOp) return SpecificOp is
    variable res: SpecificOp := op;
begin          
    case op.subpipe is
        when ALU =>
            res.arith := ArithOp'val(slv2u(op.bits));     
        when None =>
            res.system := SysOp'val(slv2u(op.bits));
        when FP =>
            res.float := FpOp'val(slv2u(op.bits));
        when others =>
            res.memory := MemOp'val(slv2u(op.bits));
    end case;
    return res;
end function;


function serializeEntry(elem: BufferEntry) return Dword is
    variable res: Dword := (others => '0');    
    variable controlByte, opByte, restByte: Byte := (others => '0');
    variable argSpecWord: Word := (others => '0');
begin 
    -- 8b
    controlByte := elem.branchIns & elem.frontBranch & elem.confirmedBranch & elem.specialAction
                &  elem.fpRename  & elem.mainCluster & elem.secCluster & elem.useLQ;
    -- 8b (probably 2 unused)
    opByte(7 downto 6) := i2slv(SubpipeType'pos(elem.specificOperation.subpipe), 2);
    opByte(OP_VALUE_BITS-1 downto 0) := elem.specificOperation.bits;
    
    -- 28b
    argSpecWord := serializeArgSpec(elem.argSpec);
    
    -- 4b (1 unused)
    restByte := "0000" & '0' & elem.full & elem.firstBr & elem.constantArgs.immSel;
    
    res := restByte(3 downto 0) & argSpecWord(27 downto 0)
         & elem.constantArgs.imm(15 downto 0) & controlByte & opByte;
    
    return res;
end function;

function serializeEntryArray(arr: BufferEntryArray) return std_logic_vector is
    variable res: std_logic_vector(MEM_WIDTH-1 downto 0);   
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(64*i + 63 downto 64*i) := serializeEntry(arr(i));
    end loop;
    return res;
end function;



function deserializeEntry(d: Dword) return BufferEntry is
    variable res: BufferEntry;
    
    constant controlByte: Byte := d(15 downto 8);
    constant opByte: Byte := d(7 downto 0);
    constant restByte: Byte := "0000" & d(63 downto 60);            
    constant argSpecWord: Word := d(63 downto 32);
begin
    
    res.branchIns := controlByte(7);
    res.frontBranch := controlByte(6);
    res.confirmedBranch := controlByte(5);
    res.specialAction := controlByte(4);
    res.fpRename := controlByte(3);
    res.mainCluster := controlByte(2);
    res.secCluster := controlByte(1);
    res.useLQ := controlByte(0);

    res.specificOperation.subpipe := SubpipeType'val(slv2u(opByte(7 downto 6)));
    res.specificOperation.bits := opByte(OP_VALUE_BITS-1 downto 0);
    res.specificOperation := unfoldOp(res.specificOperation);
    
    res.argSpec := deserializeArgSpec(argSpecWord);

    res.constantArgs.imm(31 downto 16) := (others => d(31));
    res.constantArgs.imm(15 downto 0) := d(31 downto 16);
    
    res.full := restByte(2);            
    res.firstBr := restByte(1);            
    res.constantArgs.immSel := restByte(0);            
    
    return res;
end function;

function deserializeEntryArray(v: std_logic_vector) return BufferEntryArray is
    variable res: BufferEntryArray;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := deserializeEntry(v(64*i + 63 downto 64*i));
    end loop;
    return res;
end function;


function getEntry(isl: InstructionSlot) return BufferEntry is
    variable res: BufferEntry;
begin
    res.full := isl.full;
    
    res.firstBr := isl.ins.controlInfo.firstBr;
    
    res.branchIns := isl.ins.classInfo.branchIns;
    res.frontBranch := isl.ins.controlInfo.frontBranch;
    res.confirmedBranch := isl.ins.controlInfo.confirmedBranch;
    res.specialAction := isl.ins.controlInfo.specialAction;

    res.fpRename := isl.ins.classInfo.fpRename;           
    res.mainCluster := isl.ins.classInfo.mainCluster;            
    res.secCluster := isl.ins.classInfo.secCluster;            
    res.useLQ   := isl.ins.classInfo.useLQ;
    
    res.specificOperation := isl.ins.specificOperation;
    
    res.constantArgs := isl.ins.constantArgs;
    res.argSpec := isl.ins.virtualArgSpec;
    
    return res;
end function;

function getEntryArray(insVec: InstructionSlotArray) return BufferEntryArray is
    variable res: BufferEntryArray;
begin
    for i in res'range loop
        res(i) := getEntry(insVec(i));
    end loop;            
    return res;
end function;


function getInsSlot(elem: BufferEntry) return InstructionSlot is
    variable res: InstructionSlot := DEFAULT_INS_SLOT;
begin
    res.full := elem.full;

    res.ins.controlInfo.firstBr := elem.firstBr;

    
    res.ins.classInfo.branchIns := elem.branchIns;
    res.ins.controlInfo.frontBranch := elem.frontBranch;
    res.ins.controlInfo.confirmedBranch := elem.confirmedBranch;
    res.ins.controlInfo.specialAction := elem.specialAction;

    res.ins.classInfo.fpRename := elem.fpRename;           
    res.ins.classInfo.mainCluster := elem.mainCluster;            
    res.ins.classInfo.secCluster := elem.secCluster;            
    res.ins.classInfo.useLQ := elem.useLQ;
    
    res.ins.specificOperation := unfoldOp(elem.specificOperation);
    
    res.ins.constantArgs := elem.constantArgs;
    res.ins.virtualArgSpec := elem.argSpec; 
    return res;
end function;

function getInsSlotArray(elemVec: BufferEntryArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(elemVec'range);
begin
    for i in res'range loop
        res(i) := getInsSlot(elemVec(i));
    end loop;
    return res;
end function;       


procedure updateQueue(signal content: inout BufferEntryArray2D; ptr: SmallNumber; newRow: BufferEntryArray) is
    constant indV:SmallNumber := ptr and PTR_MASK_SN;
    constant ind: natural := slv2u(indV);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        content(ind, i) <= newRow(i);
    end loop;
end procedure;

function readQueue(content: BufferEntryArray2D; ptr: SmallNumber) return BufferEntryArray is
    constant indV:SmallNumber := ptr and PTR_MASK_SN;
    constant ind: natural := slv2u(indV);
    variable res: BufferEntryArray;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := content(ind, i);
    end loop;
    
    return res;
end function;


end package body;
