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


package Assembler is

type GroupBuffer is array(0 to 4) of string(1 to 10);
type ProgramBuffer is array(0 to 999) of GroupBuffer;

function readSourceFile(name: string) return ProgramBuffer;

function processProgram(p: ProgramBuffer) return WordArray;

    type TMP_StrArray is array(integer range <>) of string(1 to 10);


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
    
    others => none 
);

constant OPCONT_TABLE_EXT1: OpcontArray := (
      0 => jzR,   -- 000000
      1 => jnzR,    -- 000001
    
    others => none 
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

    others => none 
);

constant OPCONT_TABLE_FP: OpcontArray := (
       0 => fmov,    -- 000000
       1 => forr,    -- 000001

    others => none 
);


function disasmWithAddress(a: natural; w: Word) return string;
function disasmWord(w: Word) return string;

procedure disasmToFile(name: string; arr: WordArray);


end Assembler;



package body Assembler is

function isAlphanum(c: character) return boolean is
begin
    return (c >= '0' and c <= '9')
        or (c >= 'A' and c <= 'Z')
        or (c >= 'a' and c <= 'z')
        or (c = '_')
        or (c = '$'); -- To serve as label marker
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
        
        -- We need to save the label (if any) of this line for branch reference, and produce hex value for instruction.
        -- The latter is not possible until we have all the labels, so it must be done in steps.
        
        -- To keep it simple: 
        --  - line will be either a label or instruction
        --  - label starts with '@'
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
        
        -- Convert words to line structure
        program(lineNum) := words;
        
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


function findLabel(s: string; labels: TMP_StrArray) return integer is
begin
    for i in labels'range loop
        if matches(s, labels(i)) then
            return i;
        end if;
    end loop;
    
    return -1;
end function;


function processInstruction(ar: GroupBuffer; num: integer; labels: TMP_StrArray) return word is
    variable mnem: ProcMnemonic;
    variable res: word := (others => '0');
    variable vals: IntArray(0 to ar'length-1) := (others => -1);
    variable x: integer := 0;
begin
    -- First elem must be opcode. Find it in opcode list
    mnem := undef;
    for m in ProcMnemonic loop
        if matches(ar(0), ProcMnemonic'image(m)) then
            mnem := m;
        end if;
    end loop;
    
    -- Convert other arg to numbers
    for i in 1 to ar'length-1 loop
        if ar(i)(1) = '$' then
            -- Label!
            x := 4*(findLabel(ar(i), labels) - num); -- branch offset
            
        elsif ar(i)(1) = 'r' and ar(i)(2) >= '0' and ar(i)(2) <= '9' then
            -- register
            x := integer'value(ar(i)(2 to ar(i)'length)); -- ignore 'r'
        elsif ar(i)(1) = 'f' and ar(i)(2) >= '0' and ar(i)(2) <= '9' then
            -- register (FP)
            x := integer'value(ar(i)(2 to ar(i)'length)); -- ignore 'f'			
        elsif ar(i)(1) = '-' then
            x := -integer'value(ar(i)(2 to ar(i)'length)); 
        elsif not isAlphanum(ar(i)(1)) then
            x := -1;
        elsif ar(i)(1) >= '0' and ar(i)(1) <= '9' then
            -- Hope it's a number 
            x := integer'value(ar(i));
        else
            x := -1;
        end if;
        
        vals(i) := x;
    end loop;
    

    case mnem is
        when and_i =>
            res := ins655H(andI, vals(1), vals(2), vals(3));
        when or_i =>
            res := ins655H(orI, vals(1), vals(2), vals(3));
        when xor_i =>
                --res := ins655H(xorI, vals(1), vals(2), vals(3));
        when add_i =>
            res := ins655H(addI, vals(1), vals(2), vals(3));
        --when sub_i =>
        --        res := ins655H(subI, vals(1), vals(2), vals(3));

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
            
        when jz_r =>
            res := ins655655(ext1, vals(1), vals(2), jzR, vals(3), 0);
            
        when jnz_i =>
            res := ins65J(jnz, vals(1), vals(2));
            
        when jnz_r =>
            res := ins655655(ext1, vals(1), vals(2), jnzR, vals(3), 0);
            
        when ja =>
            res := ins6L(j, vals(1));
            
        when jl =>
            res := ins65J(jl, vals(1), vals(2));
            
            
        when sys =>
            if matches(ar(1), "halt") then
               res := ins655655(ext2, 0, 0, halt, 0, 0);
            elsif matches(ar(1), "reti") then
               res := ins655655(ext2, 0, 0, retI, 0, 0);
            elsif matches(ar(1), "rete") then
               res := ins655655(ext2, 0, 0, retE, 0, 0);
            elsif matches(ar(1), "sync") then
               res := ins655655(ext2, 0, 0, sync, 0, 0);            
            elsif matches(ar(1), "replay") then
               res := ins655655(ext2, 0, 0, replay, 0, 0);            
            elsif matches(ar(1), "error") then
               res := ins655655(ext2, 0, 0, error, 0, 0);
            elsif matches(ar(1), "call") then
                  res := ins655655(ext2, 0, 0, call, 0, 0);
            elsif matches(ar(1), "send") then
                  res := ins655655(ext2, 0, 0, send, 0, 0);                                            
            else
               res := ins6L(undef, 0);            
            end if;
        
		when mov_f =>
			res := ins6556X(fop, vals(1), vals(2), fmov, 0);
			
		when or_f =>
			res := ins655655(fop, vals(1), vals(2), fmov, vals(3), 0);
		
        when others => 
            res := ins6L(undef, 0);
    end case;
    
    return res;
end function;

function processProgram(p: ProgramBuffer) return WordArray is
    variable insIndex: integer := 0; -- Actual number of instruction
    variable labels: TMP_StrArray(0 to p'length-1) := (others => (others => cr));
    variable pSqueezed: ProgramBuffer := (others => (others => (others => cr))); 
    variable commands: WordArray(0 to p'length-1) := (others => ins655655(ext2, 0, 0, error, 0, 0));
begin
    for i in 0 to p'length-1 loop
    
        if p(i)(0)(1) = '$' then -- label
           labels(insIndex) := p(i)(0);            
        elsif p(i)(0)(1) = cr then -- the line is empty
           null;
        else -- instruction
           pSqueezed(insIndex) := p(i);
           insIndex := insIndex + 1;  
        end if;
    end loop;
    
    for i in 0 to p'length-1 loop
        if pSqueezed(i)(0)(1) = '$' then -- label
           null;
        elsif pSqueezed(i)(0)(1) = cr then -- the line is empty
           null;
        else -- instruction        
           commands(i) := processInstruction(pSqueezed(i), i, labels);
        end if;
    end loop;    
    
    return commands;
end function;




function padTo(s: string; n: natural) return string is
    variable res: string(1 to n) := (others => ' ');
    constant LEN: natural := s'length;
begin
    if LEN < n then
        res(1 to LEN) := s;
    else
        res := s(1 to n);    
    end if;
    
    return res;
end function;

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

    res(1 to 5) := padTo(ProcOpcode'image(opc), 5);
    res(8 to 10) := reg2str(qa, aFP);
    res(11 to 12) := ", ";
    res(13 to 15) := reg2str(qb, false);
    res(16 to 17) := ", ";
    
    res(18 to 25) := padTo(integer'image(imm), 24-17+1);
    
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
    
    
    res(1 to 5) := padTo(ProcOpcode'image(opc), 5);
    
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

    res(immStart to immStart - 1 + immSize) := padTo(integer'image(imm), immSize);
    
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

    res(1 to 5) := padTo(ProcOpcont'image(opct), 5);
    res(8 to 10) := reg2str(qa, aFP);
    res(11 to 12) := ", ";
    res(13 to 15) := reg2str(qb, bFP);
    res(16 to 17) := ", ";
    
    case opct is
        -- 2 sources
        when andR | orR | addR | subR | divs | divu | muls | mulu =>             
            res(18 to 20) := reg2str(qc, cFP);
                     
        when shlC | shaC =>
        
            res(18 to 25) := padTo(integer'image(imm), 24-17+1);
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

    res(1 to 5) := padTo(ProcOpcont'image(opct), 5);
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

    res(1 to 5) := padTo(ProcOpcont'image(opct), 5);

    case opct is
        -- 0 sources
        when retE | retI | halt | sync | replay | error | call | send =>
        
        -- system move
        when mfc | mtc =>

            res(8 to 10) := reg2str(qa, aFP);
            res(11 to 12) := ", ";
            res(13 to 15) := reg2str(0, bFP);
            res(16 to 17) := ", ";

            res(18 to 25) := padTo(integer'image(imm), 24-17+1);
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

    res(1 to 5) := padTo(ProcOpcont'image(opct), 5);

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


function disasmWithAddress(a: natural; w: Word) return string is
    variable res: string(1 to 1 + 10 + 10 + 30) := (others => ' ');
    variable aw: Word := i2slv(a, 32);
    constant HEX_TAB: string(1 to 16) := "0123456789abcdef";
    variable c: character;
begin
    -- synthesis translate_off

    for i in 0 to 7 loop
        c := HEX_TAB(1 + (slv2u(aw(31 - 4*i downto 31 - 4*i - 3))));
        res(1 + i) := c;
    end loop;
    
    res(1 + 8 to 1 + 9) := ": ";
    
    for i in 0 to 7 loop
        c := HEX_TAB(1 + (slv2u(w(31 - 4*i downto 31 - 4*i - 3))));
        res(1 + 10 + i) := c;
    end loop;    
    
    res(19 to 21) := "   ";
    res(22 to 22 + 24-1) := disasmWord(w);
    
    -- synthesis translate_on
    
    return res;   
end function;


procedure disasmToFile(name: string; arr: WordArray) is
    file outFile: text open write_mode is name;
    variable outputLine: line;
    variable tmpStr: string(1 to 51);
begin                            
    for i in 0 to arr'length-1 loop 
       tmpStr := disasmWithAddress(i, arr(i));
       write(outputLine, tmpStr); 
       writeline(outFile, outputLine);
    end loop;

end procedure;


end Assembler;
