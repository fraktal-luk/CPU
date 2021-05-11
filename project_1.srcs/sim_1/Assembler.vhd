----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 23:25:20
-- Design Name: 
-- Module Name: Assembler - Behavioral
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

use std.textio.all;

use work.BasicTypes.all;
use work.ArchDefs.all;

use work.CpuText.all;

use work.InstructionSet.all;


package Assembler is

constant MAX_LABEL_SIZE: natural := 30;

type GroupBuffer is array(0 to 4) of string(1 to MAX_LABEL_SIZE);
type ProgramBuffer is array(0 to 999) of GroupBuffer;


type LabelArray is array(integer range <>) of string(1 to MAX_LABEL_SIZE);
constant EMPTY_LABEL_ARRAY: LabelArray(0 to 0) := (others => (others => ' '));


-- Structure for import or export of label: name and where the source or required replacement is
type Xref is record
    name: line;
    address: integer;
end record;


type XrefArray is array(integer range <>) of Xref;

procedure printXrefArray(xa: XrefArray);


function reg2str(n: natural; fp: boolean) return string;


function parseInstructionString(str: string) return GroupBuffer; -- TEMP!

function readSourceFile(name: string) return ProgramBuffer;

procedure processProgram(p: in ProgramBuffer; machineCode: out WordArray; imports, outExports: out XrefArray; constant USE_NEW: boolean);

type OpcodeArray is array(0 to 63) of ProcOpcode;
type OpcontArray is array(0 to 63) of ProcOpcont;

constant OPCODE_TABLE: OpcodeArray := (
      0 => andI,   -- 000000
      1 => orI,    -- 000001
      2 => addI,   -- 000010
      3 => subI,   -- 000011
      4 => jz,     -- 000100
      5 => jnz,    -- 000101
      6 => j,      -- 000110
      7 => jl,     -- 000111

      8 => ld,     -- 001000
      9 => st,     -- 001001
     10 => ldf,    -- 001010
     11 => stf,    -- 001011
     12 => ext0,   -- 001100
     13 => ext1,   -- 001101
     14 => ext2,   -- 001110
     15 => fop,    -- 001111
    
    others => undef
);


constant OPCONT_TABLE_EXT0: OpcontArray := (
      0 => andR,   -- 000000
      1 => orR,    -- 000001
      2 => shlC,   -- 000010
      3 => shaC,   -- 000011
      4 => addR,     -- 000100
      5 => subR,    -- 000101
      6 => muls,      -- 000110
      7 => mulu,     -- 000111

      8 => divs,     -- 001000
      9 => divu,     -- 001001
    
    others => undef 
);

constant OPCONT_TABLE_EXT1: OpcontArray := (
      0 => jzR,   -- 000000
      1 => jnzR,    -- 000001
    
    others => undef 
);

constant OPCONT_TABLE_EXT2: OpcontArray := (
      0 => retE,   -- 000000
      1 => retI,    -- 000001
      2 => halt,   -- 000010
      3 => sync,   -- 000011
      4 => replay,     -- 000100
      5 => error,    -- 000101
      6 => call,      -- 000110
      7 => send,     -- 000111
  
      8 => mfc,     -- 001000
      9 => mtc,     -- 001001

    others => undef 
);

constant OPCONT_TABLE_FP: OpcontArray := (
       0 => fmov,    -- 000000
       1 => forr,    -- 000001

    others => undef 
);

--function asm(str: string) return Word;
function asmNew(str: string) return Word;

function disasmWithAddress(a: natural; w: Word) return string;
function disasmWithAddress2(a: natural; w: Word) return string;
function disasmWord(w: Word) return string;
function disasmWord2(w: Word) return string;

procedure disasmToFile(name: string; arr: WordArray);

function matchXrefs(imp, exp: XrefArray) return IntArray;
function fillXrefs(code: WordArray; imports: XrefArray; offsets: IntArray; imgStart, libStart: integer) return WordArray;

procedure stringAssign(signal s: out string; sIn: string);
procedure stringFromLine(s: out string; variable ln: in line);

end Assembler;



package body Assembler is

procedure stringAssign(signal s: out string; sIn: string) is
    constant nOut: natural := s'length;
    variable nIn: natural := sIn'length;
begin
    if nIn > nOut then
        nIn := nOut;
    end if;
    
    s <= (others => ' ');
    s(1 to nIn) <= sIn(1 to nIn);
end procedure;


-- Differs from simple ln.all in that it's written to a string of predefined length
procedure stringFromLine(s: out string; variable ln: in line) is
begin
    s := (others => cr);
    s(1 to ln.all'length) := ln.all;        
end procedure; 


procedure printXrefArray(xa: XrefArray) is

begin
    report "Printing refs:";
    for i in xa'range loop
        if xa(i).name = null then
            return;
        end if;
        report xa(i).name.all; report natural'image(xa(i).address);
    end loop;
end procedure;


-- TODO: change name to something true (isExtendedAlphanum?)
function isAlphanum(c: character) return boolean is
begin
    return (c >= '0' and c <= '9')
        or (c >= 'A' and c <= 'Z')
        or (c >= 'a' and c <= 'z')
        or (c = '_')
        or (c = '$') -- To serve as label marker
        or (c = '@'); -- To serve as keyword marker
end function;


function parseInstructionString(str: string) return GroupBuffer is
    variable words: GroupBuffer := (others => (others => ' '));
    
    variable ind, grp, blockStart: integer := 0;    
begin
    -- To keep it simple: 
    --  - line will be either a label or instruction
    --  - label starts with '$'
    --  - if not label, cut line into alphanumeric groups, don't care about commas etc.

    ind := 1;
    grp := 0;
    words := (others => (others => cr));      
    while str(ind) /= cr and grp < words'length loop -- keep last char as end sign
        while str(ind) = ' ' or str(ind) = ','  or str(ind) = ht loop 
            -- skipping wspace
            ind := ind + 1;
        end loop;
        
        blockStart := ind - 1;
        while isAlphanum(str(ind)) or str(ind) = '-' loop -- and not tab nor cr!
            -- Copy to current group
            words(grp)(ind - blockStart) := str(ind); -- May overflow the word, but nvm                 
            ind := ind + 1;
        end loop;
        -- Inc group index
        grp := grp + 1;
    end loop;
    
    return words;
end function;


function readSourceFile(name: string) return ProgramBuffer is
    file src: text open read_mode is name;
    variable ln: line;
    variable str: string(1 to 100);
    variable ch: character;
    variable gl, good: boolean;
    variable lineNum, ind, grp, blockStart: integer := 0;
    variable labelFound: boolean := false;
    variable words: GroupBuffer := (others => (others => cr));
    variable program: ProgramBuffer := (others => (others => (others => cr)));
begin    
    loop
        ln := null;
        readline(src, ln);

        if ln = null or ln'length = 0 then
            exit;
        end if;

        str := (others => cr);
        for i in 1 to 100 loop
            read(ln, ch, good);
            if not good then
                str(i-1) := ' ';
                exit;
            elsif ch = cr or ch = ';' then -- Stop when line ends or comment starts
                str(i) := ' ';
                exit;
            end if;
            
            str(i) := ch;
        end loop;
   
        program(lineNum) := parseInstructionString(str);
        lineNum := lineNum + 1;
    end loop;
    
    return program;
end function;


function matches(a, b: string) return boolean is
    variable aLen, bLen: integer := 0;
begin
    aLen := a'length;
    bLen := b'length;
    if aLen > bLen then 
        aLen := bLen;
    end if;
    
    for i in 1 to aLen loop
        if a(i) /= b(i) then
            return false;
        end if;
    end loop;
    
    return true;
end function;


function findLabel(s: string; labels: LabelArray) return integer is
begin
    for i in labels'range loop
        if matches(s, labels(i)) then
            return i;
        end if;
    end loop;
    
    return -1;
end function;

function TMP_str2int(s: string) return integer is
    constant LEN: natural := s'length;
    variable str0: string(1 to LEN) := s;
begin
    for i in str0'range loop
        if str0(i) = cr then
            str0(i) := ' ';
        end if;
    end loop;
    return integer'value(str0);
end function;

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
        x := -1;
    end if;    
    return x;
end function;


function makeMachineWord(mnemonic: ProcMnemonic; vals: IntArray; strArg: string; undefOffset: boolean) return Word is
    variable res: Word := (others => '0');
begin
    case mnemonic is
        when and_i =>
            res := ins655H(andI, vals(1), vals(2), vals(3));
        when or_i =>
            res := ins655H(orI, vals(1), vals(2), vals(3));
        when xor_i =>
                --res := ins655H(xorI, vals(1), vals(2), vals(3));
        when add_i =>
            res := ins655H(addI, vals(1), vals(2), vals(3));
    
        when and_r =>
            res := ins655655(ext0, vals(1), vals(2), andR, vals(3), 0);
        when or_r =>
            res := ins655655(ext0, vals(1), vals(2), orR, vals(3), 0);
        when xor_r =>
                --res := ins655H(xorI, vals(1), vals(2), vals(3));
        when add_r =>
            res := ins655655(ext0, vals(1), vals(2), addR, vals(3), 0);
        when sub_r =>
            res := ins655655(ext0, vals(1), vals(2), subR, vals(3), 0);
                       
        when shl_i =>
            res := ins6556X(ext0, vals(1), vals(2), shlC, vals(3));
        when sha_i =>
            res := ins6556X(ext0, vals(1), vals(2), shaC, vals(3));
        when mul =>
            res := ins655655(ext0, vals(1), vals(2), muls, vals(3), 0);
        when ldi_i => 
            res := ins655H(ld, vals(1), vals(2), vals(3));
        when sti_i =>
            res := ins655H(st, vals(1), vals(2), vals(3));
        
        when ldf_i =>
            res := ins655H(ldf, vals(1), vals(2), vals(3));
        
        when stf_i =>
            res := ins655H(stf, vals(1), vals(2), vals(3));
        
        
        when lds =>
            res := ins6556X(ext2, vals(1), vals(2), mfc, vals(3));
    
        when sts =>
            res := ins6556X(ext2, vals(1), vals(2), mtc, vals(3));
    
    
        when jz_i =>
            res := ins65J(jz, vals(1), vals(2));
                if undefOffset then
                    res := ins65J(jz, vals(1));
                end if;
        when jz_r =>
            res := ins655655(ext1, vals(1), vals(2), jzR, vals(3), 0);
            
        when jnz_i =>
            res := ins65J(jnz, vals(1), vals(2));
                if undefOffset then
                    res := ins65J(jnz, vals(1));
                end if;            
        when jnz_r =>
            res := ins655655(ext1, vals(1), vals(2), jnzR, vals(3), 0);
            
        when ja =>
            res := ins6L(j, vals(1));
                if undefOffset then
                    res := ins6L(j);
                end if;            
        when jl =>
            res := ins65J(jl, vals(1), vals(2));
                if undefOffset then
                    res := ins65J(jl, vals(1));
                end if;            
            
        when sys =>
            if matches(strArg, "halt") then
               res := ins655655(ext2, 0, 0, halt, 0, 0);
            elsif matches(strArg, "reti") then
               res := ins655655(ext2, 0, 0, retI, 0, 0);
            elsif matches(strArg, "rete") then
               res := ins655655(ext2, 0, 0, retE, 0, 0);
            elsif matches(strArg, "sync") then
               res := ins655655(ext2, 0, 0, sync, 0, 0);            
            elsif matches(strArg, "replay") then
               res := ins655655(ext2, 0, 0, replay, 0, 0);            
            elsif matches(strArg, "error") then
               res := ins655655(ext2, 0, 0, error, 0, 0);
            elsif matches(strArg, "call") then
                  res := ins655655(ext2, 0, 0, call, 0, 0);
            elsif matches(strArg, "send") then
                  res := ins655655(ext2, 0, 0, send, 0, 0);                                            
            else
               res := ins6L(undef, 0);            
            end if;
        
        when mov_f =>
            res := ins655655(fop, vals(1), vals(2), fmov, -1, 0); -- TMP!
            
        when or_f =>
            res := ins655655(fop, vals(1), vals(2), fmov, vals(3), 0);
        
        when others => 
            res := ins6L(undef, 0);
    end case;
    
    return res;
end function;



function makeMachineWordNew(mnemonic: ProcMnemonic; vals: IntArray; strArg: string; undefOffset: boolean) return Word is
    variable res: Word := (others => 'X');
    variable insDef: InstructionDefinition;
    alias qa is res(25 downto 21); 
    alias qb is res(20 downto 16); 
    alias qc is res(9 downto 5); 
    alias qd is res(4 downto 0);
    alias imm16 is res(15 downto 0);
    alias imm10 is res(9 downto 0);
    constant TheTable: GeneralTable := buildGeneralTable;
begin
    insDef := --findEncoding(mnemonic);
              TheTable(mnemonic);

--    if mnemonic = sys then
--            insDef.fmt := noRegs;
--            --indDef.i := 
            
--            if matches(strArg, "halt") then
--               res := ins655655(ext2, 0, 0, halt, 0, 0);
--            elsif matches(strArg, "reti") then
--               res := ins655655(ext2, 0, 0, retI, 0, 0);
--            elsif matches(strArg, "rete") then
--               res := ins655655(ext2, 0, 0, retE, 0, 0);
--            elsif matches(strArg, "sync") then
--               res := ins655655(ext2, 0, 0, sync, 0, 0);            
--            elsif matches(strArg, "replay") then
--               res := ins655655(ext2, 0, 0, replay, 0, 0);            
--            elsif matches(strArg, "error") then
--               res := ins655655(ext2, 0, 0, error, 0, 0);
--            elsif matches(strArg, "call") then
--                  res := ins655655(ext2, 0, 0, call, 0, 0);
--            elsif matches(strArg, "send") then
--                  res := ins655655(ext2, 0, 0, send, 0, 0);                                            
--            else
--               res := ins6L(undef, 0);            
--            end if;
--    end if;
    
    --    report ProcMnemonic'image(mnemonic);
    --    report integer'image(insDef.i);
    res(31 downto 26) := i2slv(insDef.i, 6);
    res(15 downto 10) := i2slv(insDef.j, 6);
    res(4 downto 0) := i2slv(insDef.k, 5);

    -- Fill args
    case insDef.fmt is
        when noRegs =>
            qa := (others => '0');
            qb := (others => '0');
            qc := (others => '0');
            if insDef.k = -1 then
                qd := (others => '0');
            end if;            
        when jumpLong =>
            if undefOffset then
                res(25 downto 0) := (others => 'U');
            else
                res(25 downto 0) := i2slv(vals(1), 26);
            end if;
        when jumpCond | jumpLink =>
            qa := i2slv(vals(1), 5);
            if undefOffset then
                res(20 downto 0) := (others => 'U');
            else            
                res(20 downto 0) := i2slv(vals(2), 21);
            end if;        
        when intImm16 => 
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm16 := i2slv(vals(3), 16);
        when intImm10 => 
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm10 := i2slv(vals(3), 10);
        when intRR =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);            
            qc := i2slv(vals(3), 5);
            if insDef.k = -1 then
                qd := (others => '0');
            end if;
        when floatRR =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);            
            qc := i2slv(vals(3), 5);
            if insDef.k = -1 then
                qd := (others => '0');
            end if;
        
        when intStore16 =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm16 := i2slv(vals(3), 16);        
        when intStore10 =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm10 := i2slv(vals(3), 10);        
        when floatLoad10 =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm10 := i2slv(vals(3), 10);
        when floatLoad16 =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm16 := i2slv(vals(3), 16);
        when floatStore10 =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm10 := i2slv(vals(3), 10); 
        when floatStore16 =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(vals(2), 5);
            imm16 := i2slv(vals(3), 16);        
        when sysLoad =>
            qa := i2slv(vals(1), 5);
            qb := i2slv(0, 5);
            imm10 := i2slv(vals(3), 10);        
        when sysStore =>      
            qa := i2slv(vals(1), 5);
            qb := i2slv(0, 5);
            imm10 := i2slv(vals(3), 10);
        when others =>
            --report "Invalid ins format!" severity failure;
    end case;
    
    return res;
end function;


procedure processInstruction(ar: GroupBuffer; num: integer; labels, imports: LabelArray; fillLabels: boolean; command: out Word; hasImport: out boolean; import: out string;
                                constant USE_NEW: boolean) is
    variable mnem: ProcMnemonic;
    variable undefOffset: boolean := false; 
    variable vals: IntArray(0 to ar'length-1) := (others => -1);
begin
    hasImport := false;
    import := (others => cr);
    -- First elem must be opcode. Find it in opcode list
    mnem := undef;
    for m in ProcMnemonic loop
        if matches(ar(0), ProcMnemonic'image(m)) then
            mnem := m;
        end if;
    end loop;
            
    -- Convert other arg to numbers
    for i in 1 to ar'length-1 loop
        if ar(i)(1) = '$' then -- Label!
           undefOffset := true;
           vals(i) := -1;
           import(ar(i)'range) := ar(i);
           hasImport := true;
        else
           vals(i) := parseArg(ar(i));            
        end if;
    end loop;
    
    if USE_NEW then
        if matches(ar(0), "sys") then
            for m in ProcMnemonic loop
                --    report "sys_" & ar(1);      
                if matches("sys_" & ar(1), ProcMnemonic'image(m)) then
                    mnem := m;
                end if;
            end loop;            
        end if;
        
        command := makeMachineWordNew(mnem, vals, ar(1), undefOffset);
        
--                if matches(ar(0), "sys") then
--                    command := (others => '-');
--                end if;
                
--                if mnem = sys_call then
--                    command := (others => 'Z');
--                end if;
                
    else
        command := makeMachineWord(mnem, vals, ar(1), undefOffset);
    end if;
end procedure;


--function processSingleInstruction(gb: GroupBuffer) return Word is
--    variable tmpImport: string(1 to MAX_LABEL_SIZE) := (others => cr);
--    variable tmpHasImport: boolean := false;    
--begin
--    --processInstructionNew(gb, 0, EMPTY_LABEL_ARRAY, EMPTY_LABEL_ARRAY, true,  res, tmpHasImport, tmpImport);    
--    --return res;
--end function;

--function asm(str: string) return Word is
--    constant LEN: natural := str'length;
--    variable str0: string(1 to LEN+1) := (others => cr);    
--    variable gb: GroupBuffer;
--    variable tmpImport: string(1 to MAX_LABEL_SIZE) := (others => cr);
--    variable tmpHasImport: boolean := false;
--    variable res: Word;      
--begin
--    str0(1 to LEN) := str;
--    gb := parseInstructionString(str0);
--    processInstruction(gb, 0, EMPTY_LABEL_ARRAY, EMPTY_LABEL_ARRAY, true,  res, tmpHasImport, tmpImport, false);        
--    return res;
--end function;

function asmNew(str: string) return Word is
    constant LEN: natural := str'length;
    variable str0: string(1 to LEN+1) := (others => cr);    
    variable gb: GroupBuffer;
    variable tmpImport: string(1 to MAX_LABEL_SIZE) := (others => cr);
    variable tmpHasImport: boolean := false;
    variable res: Word;      
begin
    str0(1 to LEN) := str;
    gb := parseInstructionString(str0);
    processInstruction(gb, 0, EMPTY_LABEL_ARRAY, EMPTY_LABEL_ARRAY, true,  res, tmpHasImport, tmpImport, true);        
    return res;
end function;


-- TODO: make it dependent on addresses, not instruction and labels indices
function fillOffset(w: Word; k: natural; import: string; labels, imports: LabelArray) return Word is
    variable res: Word := w;
    variable numL, numI, offset: integer := -1;
    variable offsetWord: Word := (others => 'U'); 
begin
    numL := (findLabel(import, labels)); -- branch offset
    if numL /= -1 then
        offset := 4*(numL - k);
    else 
        numI := findLabel(import, imports);
        if numI /= -1 then
            offset := 4*(numI - k);
        else
            report "Using unknown label" severity error;
            return res; 
        end if;
    end if;
    
    offsetWord := i2slv(offset, 32);
    for i in res'range loop
        if res(i) = 'U' then
            res(i) := offsetWord(i);
        end if;
    end loop;
    return res;
end function;


function fillOffsetConst(w: Word; offset: integer) return Word is
    variable res: Word := w;
    variable offsetWord: Word := (others => 'U'); 
begin 
    offsetWord := i2slv(offset, 32);
    for i in res'range loop
        if res(i) = 'U' then
            res(i) := offsetWord(i);
        end if;
    end loop;
    return res;
end function;


function tmpStrip(s: string) return string is
begin
    for i in s'range loop
        if s(i) = ' ' or s(i) = ht or s(i) = cr then
            return s(1 to i-1);
        end if;
    end loop;
    return s;
end function;


procedure processProgram(p: in ProgramBuffer; machineCode: out WordArray; imports, outExports: out XrefArray; constant USE_NEW: boolean) is
    variable insIndex, procIndex: integer := 0; -- Actual number of instruction
    variable labels, exports: LabelArray(0 to p'length-1) := (others => (others => cr));
    variable startOffsets, endOffsets: IntArray(0 to p'length-1) := (others => -1);
    variable pSqueezed: ProgramBuffer := (others => (others => (others => cr))); 
    variable commands: WordArray(0 to p'length-1) := (others => ins655655(ext2, 0, 0, error, 0, 0));
    variable tmpStr: string(1 to MAX_LABEL_SIZE) :=(others => ' ');
    variable tmpImport: string(1 to MAX_LABEL_SIZE) := (others => cr);
    variable tmpHasImport: boolean := false;
    
    variable ne, ni: natural := 0;
begin
    for i in 0 to p'length-1 loop
        if p(i)(0)(1) = '@' then -- Keyword
            tmpStr(1 to  p(i)(0)'length-1) := p(i)(0)(2 to p(i)(0)'length);
            -- TODO
            -- when proc
            if matches(tmpStr, "proc") then
                -- add name to labels
                -- add name to export
                -- add insIndex to offsets
                labels(insIndex)(1 to 10) := p(i)(1);
                exports(insIndex)(2 to 11) := p(i)(1);
                exports(insIndex)(1) := '$';
                startOffsets(procIndex) := insIndex;
                outExports(ne) := (new string'('$' & tmpStrip(p(i)(1))), 4*insIndex);
                ne := ne + 1;
            elsif matches(tmpStr, "end") then
                -- ignore
                -- set proc end (if such action needed and defined)
                endOffsets(procIndex) := insIndex;
                procIndex := procIndex + 1;
            end if;
        elsif p(i)(0)(1) = '$' then -- label
           labels(insIndex)(1 to 10) := p(i)(0);            
        elsif p(i)(0)(1) = cr then -- the line is empty
           null;
        else -- instruction
           pSqueezed(insIndex) := p(i);
           insIndex := insIndex + 1;  
        end if;
        
    end loop;
    
    for i in 0 to p'length-1 loop
        processInstruction(pSqueezed(i), i, labels, EMPTY_LABEL_ARRAY, true, commands(i), tmpHasImport, tmpImport, USE_NEW);
        if tmpHasImport then
            commands(i) := fillOffset(commands(i), i, tmpImport, labels, EMPTY_LABEL_ARRAY);
            imports(ni) := (new string'(tmpStrip(tmpImport)), 4*i);
            ni := ni + 1;
        end if;
    end loop; 

    machineCode := commands;
end procedure;



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


function disasm655H(w: Word; opc: ProcOpcode) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable qa, qb, qc, qd, imm: integer;
    variable aFP: boolean := false; 
begin
    qa := slv2u(w(25 downto 21));
    qb := slv2u(w(20 downto 16));
    qc := slv2u(w(9 downto 5));
    qd := slv2u(w(4 downto 0));
    
    imm := slv2s(w(15 downto 0));
    
    case opc is
        when ldf | stf =>
            aFP := true;
        when others =>
    end case;

    res(1 to 5) := padLeft(ProcOpcode'image(opc), 5);
    res(8 to 10) := reg2str(qa, aFP);
    res(11 to 12) := ", ";
    res(13 to 15) := reg2str(qb, false);
    res(16 to 17) := ", ";
    
    res(18 to 25) := padLeft(integer'image(imm), 24-17+1);
    
    return res;
end function;


function disasmJump(w: Word; opc: ProcOpcode) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable qa, qb, qc, qd, imm: integer;
    variable aFP: boolean := false;
    variable immStart, immSize: natural := 0;
begin
    qa := slv2u(w(25 downto 21));
    qb := slv2u(w(20 downto 16));
    qc := slv2u(w(9 downto 5));
    qd := slv2u(w(4 downto 0));
    
    
    res(1 to 5) := padLeft(ProcOpcode'image(opc), 5);
    
    case opc is
        when j =>
            imm := slv2s(w(25 downto 0));
            immSize := 10;
            immStart := 8;           
        when jz | jnz | jl =>
            imm := slv2s(w(20 downto 0));
            immSize := 7;

            res(8 to 10) := reg2str(qa, aFP);
            res(11 to 12) := ", ";
            immStart := 14;
        when others =>
    end case;

    res(immStart to immStart - 1 + immSize) := padLeft(integer'image(imm), immSize);
    
    return res;
end function;


function disasmExt0(w: Word; opc: ProcOpcode) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable qa, qb, qc, qd, imm: integer;
    variable aFP, bFP, cFP, dFP: boolean := false;
    variable immStart, immSize: natural := 0;
    variable opct: ProcOpcont;
begin
    qa := slv2u(w(25 downto 21));
    qb := slv2u(w(20 downto 16));
    qc := slv2u(w(9 downto 5));
    qd := slv2u(w(4 downto 0));
    
    imm := slv2s(w(9 downto 0));
    
    opct := OPCONT_TABLE_EXT0(slv2u(w(15 downto 10)));

    res(1 to 5) := padLeft(ProcOpcont'image(opct), 5);
    res(8 to 10) := reg2str(qa, aFP);
    res(11 to 12) := ", ";
    res(13 to 15) := reg2str(qb, bFP);
    res(16 to 17) := ", ";
    
    case opct is
        -- 2 sources
        when andR | orR | addR | subR | divs | divu | muls | mulu =>             
            res(18 to 20) := reg2str(qc, cFP);
                     
        when shlC | shaC =>
        
            res(18 to 25) := padLeft(integer'image(imm), 24-17+1);
        when others =>
    end case;
    
    return res;
end function;

function disasmExt1(w: Word; opc: ProcOpcode) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable qa, qb, qc, qd, imm: integer;
    variable aFP, bFP, cFP, dFP: boolean := false;
    variable immStart, immSize: natural := 0;
    variable opct: ProcOpcont;
begin
    qa := slv2u(w(25 downto 21));
    qb := slv2u(w(20 downto 16));
    qc := slv2u(w(9 downto 5));
    qd := slv2u(w(4 downto 0));
    
    imm := slv2s(w(9 downto 0));
    
    opct := OPCONT_TABLE_EXT1(slv2u(w(15 downto 10)));

    res(1 to 5) := padLeft(ProcOpcont'image(opct), 5);
    res(8 to 10) := reg2str(qa, aFP);
    res(11 to 12) := ", ";
    res(13 to 15) := reg2str(qb, bFP);
    res(16 to 17) := ", ";
    
    case opct is
        -- 2 sources
        when jzR | jnzR =>             
            res(18 to 20) := reg2str(qc, cFP);
        when others =>
    end case;
    
    return res;
end function;

function disasmExt2(w: Word; opc: ProcOpcode) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable qa, qb, qc, qd, imm: integer;
    variable aFP, bFP, cFP, dFP: boolean := false;
    variable immStart, immSize: natural := 0;
    variable opct: ProcOpcont;
begin
    qa := slv2u(w(25 downto 21));
    qb := slv2u(w(20 downto 16));
    qc := slv2u(w(9 downto 5));
    qd := slv2u(w(4 downto 0));
    
    imm := slv2s(w(9 downto 0));
    
    opct := OPCONT_TABLE_EXT2(slv2u(w(15 downto 10)));

    res(1 to 5) := padLeft(ProcOpcont'image(opct), 5);

    case opct is
        -- 0 sources
        when retE | retI | halt | sync | replay | error | call | send =>
        
        -- system move
        when mfc | mtc =>

            res(8 to 10) := reg2str(qa, aFP);
            res(11 to 12) := ", ";
            res(13 to 15) := reg2str(0, bFP);
            res(16 to 17) := ", ";

            res(18 to 25) := padLeft(integer'image(imm), 24-17+1);
        -- FP 1 source
        when fmov => 
            res(8 to 10) := reg2str(qa, aFP);
            res(11 to 12) := ", ";
            res(13 to 15) := reg2str(qb, bFP);
            res(16 to 17) := ", ";
                    
        -- FP 2 sources
        when forr =>             
            res(18 to 20) := reg2str(qc, cFP);

        when others =>
    end case;
    
    return res;
end function;

function disasmFP(w: Word; opc: ProcOpcode) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable qa, qb, qc, qd, imm: integer;
    variable aFP, bFP, cFP, dFP: boolean := true;
    variable immStart, immSize: natural := 0;
    variable opct: ProcOpcont;
begin
    qa := slv2u(w(25 downto 21));
    qb := slv2u(w(20 downto 16));
    qc := slv2u(w(9 downto 5));
    qd := slv2u(w(4 downto 0));
    
    imm := slv2s(w(9 downto 0));
    
    opct := OPCONT_TABLE_FP(slv2u(w(15 downto 10)));

    res(1 to 5) := padLeft(ProcOpcont'image(opct), 5);

    res(8 to 10) := reg2str(qa, aFP);
    res(11 to 12) := ", ";
    res(13 to 15) := reg2str(qb, bFP);
    res(16 to 17) := ", ";

    case opct is
        when fmov => 
            
        -- FP 2 sources
        when forr =>             
            res(18 to 20) := reg2str(qc, cFP);

        when others =>
    end case;
    
    return res;
end function;



function disasmWord(w: Word) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable ind: integer := -1;
    variable opc: ProcOpcode;
begin
    
    ind := slv2u(w(31 downto 26)); 
    opc := OPCODE_TABLE(ind);
    
    case opc is
        -- format 655H
        when andI | orI | addI | subI | ld | st | ldf | stf =>
            return disasm655H(w, opc);
        -- constant jumps
        when j | jz | jnz | jl =>
            return disasmJump(w, opc);
        
        when ext0 =>
            return disasmExt0(w, opc);

        when ext1 =>
            return disasmExt1(w, opc);
            
        when ext2 =>
            return disasmExt2(w, opc);

        when fop =>
            return disasmFP(w, opc);
                                    
        when others =>
            res(1 to 3) := "???";
    end case;
      

    return res;
end function;


function disasmWord2(w: Word) return string is
    variable res: string(1 to 24) := (others => ' ');
    variable ind: integer := -1;
    variable opc: ProcOpcode;
begin
    res := padLeft(work.Emulate.getOpDisasm(w), 24);
    return res;
end function;




function disasmWithAddress(a: natural; w: Word) return string is
    variable res: string(1 to 1 + 10 + 10 + 30) := (others => ' ');
    variable aw: Word := i2slv(a, 32);
    constant HEX_TAB: string(1 to 16) := "0123456789abcdef";
    variable c: character;
begin
    -- synthesis translate_off   
    res(1 to 8) := w2hex(aw);
    res(1 + 8 to 1 + 9) := ": "; res(10) := ' ';--cr;
    res(11 to 18) := w2hex(w);
    res(19 to 21) := "   ";
    res(22 to 22 + 24-1) := disasmWord(w);
    -- synthesis translate_on
    
    return res;
end function;

function disasmWithAddress2(a: natural; w: Word) return string is
    variable res: string(1 to 1 + 10 + 10 + 30) := (others => ' ');
    variable aw: Word := i2slv(a, 32);
    constant HEX_TAB: string(1 to 16) := "0123456789abcdef";
    variable c: character;
begin
    -- synthesis translate_off   
    res(1 to 8) := w2hex(aw);
    res(1 + 8 to 1 + 9) := ": "; res(10) := ' ';--cr;
    res(11 to 18) := w2hex(w);
    res(19 to 21) := "   ";
    res(22 to 22 + 24-1) := disasmWord2(w);
    -- synthesis translate_on
    
    return res;
end function;


procedure disasmToFile(name: string; arr: WordArray) is
    file outFile: text open write_mode is name;
    variable outputLine: line;
    variable tmpStr: string(1 to 51);
begin
    for i in 0 to arr'length-1 loop 
       tmpStr := disasmWithAddress(4*i, arr(i));
       write(outputLine, tmpStr); 
       writeline(outFile, outputLine);
    end loop;

end procedure;


procedure assemblePrograms is
begin
    -- run first pass: assemble with possible missing labels
        -- local labels must be there, otherwise fail
        -- external labels are left to complete and marked
        -- export symbols from each file to common export list
    -- run second pass: fill missing addresses using export list

end procedure;


function matchXrefs(imp, exp: XrefArray) return IntArray is
    variable res: IntArray(imp'range) := (others => -1);
    variable str0, str1: string(1 to MAX_LABEL_SIZE);
    variable l0, l1: line; 
begin
    for i in imp'range loop
        if imp(i).name = null then exit; end if;
    
        for j in exp'range loop
            if exp(j).name = null then exit; end if;
            
            l0 := imp(i).name;
            l1 := exp(j).name;

            if matches(l0.all, l1.all) then
                res(i) := exp(j).address;
                exit;
            end if;
        end loop;
    end loop;
    return res;
end function;

function fillXrefs(code: WordArray; imports: XrefArray; offsets: IntArray; imgStart, libStart: integer) return WordArray is
    variable res: WordArray(code'range) := code;
    variable currentPos: integer := -1;
begin
    for i in imports'range loop
        if imports(i).name = null then
            return res;
        end if;
    
        currentPos := imports(i).address/4;
        res(currentPos) := fillOffsetConst(res(currentPos), offsets(i) - imports(i).address + libStart - imgStart);
    end loop;
    
    return res;
end function;

end Assembler;
