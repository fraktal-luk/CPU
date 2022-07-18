
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

type SerialMemory is array(0 to IBUFFER_SIZE-1) of std_logic_vector(MEM_WIDTH-1 downto 0);

-- CAREFUL: only virtual because 5 bits per reg!
function serializeArgSpec(argSpec: InstructionArgSpec) return Word;
function deserializeArgSpec(w: Word) return InstructionArgSpec;

function serializeEntryArray(arr: BufferEntryArray) return std_logic_vector;
function deserializeEntryArray(v: std_logic_vector) return BufferEntryArray;

end package;



package body LogicIbuffer is

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
    variable res: BufferEntry := DEFAULT_BUFFER_ENTRY;
    
    constant controlByte: Byte := d(15 downto 8);
    constant opByte: Byte := d(7 downto 0);
    constant restByte: Byte := "0000" & d(63 downto 60);            
    constant argSpecWord: Word := d(63 downto 32);
begin
    
    res.branchIns := controlByte(7);
    --res.frontBranch := controlByte(6);
    --res.confirmedBranch := controlByte(5);
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


end package body;
