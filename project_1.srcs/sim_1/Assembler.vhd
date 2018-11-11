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
        --report "lie read";

        if ln = null or ln'length = 0 then
            --report "File ended";
            exit;
        end if;

        --report ln.all;

        str := (others => cr);
        for i in 1 to 100 loop
            read(ln, ch, good);
            --report boolean'image(good);
            --report character'image(ch);
            
            if not good then
                str(i-1) := ' ';
                exit;
            elsif ch = cr or ch = ';' then -- Stop when line ends or comment starts
                str(i) := ' ';
                exit;
            end if;
            
            str(i) := ch;
        end loop;  
        
        --report str;
        
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
                while isAlphanum(str(ind)) loop -- and not tab nor cr!
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


function processInstruction(ar: GroupBuffer; labels: TMP_StrArray) return word is
    variable mnem: ProcMnemonic;
    variable res: word := (others => '0');
    variable vals: --WordArray(0 to ar'length-1) := (others => (others => '0'));
                    IntArray(0 to ar'length-1) := (others => 0);
    variable x: integer := 0;
begin

    -- First elem must be opcode. Find it in opcode list list
    -- ...
    -- 
        
    -- ("or_r", "r1", "r2", "r3") -> (fmt655655, ext0, 1, 2, orR, 3)
    -- Just use a "switch" with all mnemonics!
    
    -- Find a member of ProcMnemonic that matches the string (up to trailing non alnums) and return it
    -- Then use the retur value as selector 
    --report "Which opcode?";

    
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
            x := 4*(findLabel(ar(i), labels) - i); -- branch offset
            
        elsif ar(i)(1) = 'r' then 
            -- register
            x := integer'value(ar(i)(2 to ar(i)'length)); -- ignore 'r'
        elsif not isAlphanum(ar(i)(1)) then
            x := -1;
        else
            -- Hope it's a number 
            x := integer'value(ar(i));
        end if;
        
        vals(i) := x;--i2slv(x, 32);
        
        --        report integer'image(x);
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
            res := ins6556X(ext0, vals(1), vals(2), shlC, vals(3));
        when mul =>
            res := ins655655(ext0, vals(1), vals(2), muls, vals(3), 0);
        when ldi_i => 
            res := ins6556X(ext1, vals(1), vals(2), load, vals(3));
        when sti_i =>
            res := ins6556X(ext1, vals(1), vals(2), store, vals(3));
        
        when ldf_i =>
            res := ins6556X(ext1, vals(1), vals(2), loadFP, vals(3));
        
        when stf_i =>
            res := ins6556X(ext1, vals(1), vals(2), storeFP, vals(3));
        
        
        when lds =>
            res := ins6556X(ext2, vals(1), vals(2), mfc, vals(3));

        when sts =>
            res := ins6556X(ext2, vals(1), vals(2), mtc, vals(3));


        when jz_i =>
            res := ins65J(jz, vals(1), vals(2));
            
        when jz_r =>
            res := ins655655(ext1, vals(1), vals(2), jzR, vals(3), 0);
            
        when jnz_i =>
            res := ins65J(jz, vals(1), vals(2));
            
        when jnz_r =>
            res := ins655655(ext1, vals(1), vals(2), jnzR, vals(3), 0);
            
        when ja =>
            res := ins6L(j, vals(1));
            
        when jl =>
            res := ins65J(j, vals(1), vals(2));
            
            
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
            else
               res := ins6L(undef, 0);            
            end if;
                          
        when others => 
            res := ins6L(undef, 0);
    end case;
    
    return res;
end function;

function processProgram(p: ProgramBuffer) return WordArray is
    variable dummy: boolean;
    variable insIndex, j: integer := 0; -- Actual number of instruction
    variable labels: TMP_StrArray(0 to p'length-1) := (others => (others => cr));
    variable pSqueezed: ProgramBuffer := (others => (others => (others => cr))); 
    variable commands: WordArray(0 to p'length-1) := (others => (others => '0')); -- TODO: fill with undefined!
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
           commands(i) := processInstruction(pSqueezed(i), labels);
           --j := j + 1;
        end if;
    end loop;    
    
    return commands;
end function;


end Assembler;
