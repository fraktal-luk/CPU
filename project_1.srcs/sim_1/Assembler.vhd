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

type GroupBuffer is array(0 to 4) of string(1 to 9);
type ProgramBuffer is array(0 to 999) of GroupBuffer;

function readSourceFile(name: string) return ProgramBuffer;

function processProgram(p: ProgramBuffer) return boolean;

    type TMP_StrArray is array(integer range <>) of string(1 to 10);
    type TMP_WordArray is array(integer range <>) of Word;

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


function processInstruction(ar: GroupBuffer; labels: TMP_StrArray) return word is
    variable mnem: ProcMnemonic;
    variable res: word := (others => '0');
begin

    -- First elem must be opcode. Find it in opcode list list
    -- ...
    -- 
        
    -- ("or_r", "r1", "r2", "r3") -> (fmt655655, ext0, 1, 2, orR, 3)
    -- Just use a "switch" with all mnemonics!
    
    -- Find a member of ProcMnemonic that matches the string (up to trailing non alnums) and return it
    -- Then use the retur value as selector 
    report "Which opcode?";

    
    mnem := undef;
    for m in ProcMnemonic loop
        if matches(ar(0), ProcMnemonic'image(m)) then
            mnem := m;
        end if;
    end loop;    
    
    case mnem is
        when and_i =>
            
        
        when others => 
            res := (others => 'U');
    end case;
    
    return res;
end function;

function processProgram(p: ProgramBuffer) return boolean is
    variable dummy: boolean;
    variable labelIndex, insIndex: integer := 0; -- Actual number of instruction
    variable labels: TMP_StrArray(0 to p'length-1) := (others => (others => cr));
    variable commands: TMP_WordArray(0 to p'length-1) := (others => (others => '0')); -- TODO: fill with undefined!
begin
    for i in 0 to p'length-1 loop
    
        if p(i)(0)(1) = '$' then -- label
           labels(labelIndex) := p(i)(0);            
           labelIndex := labelIndex + 1;
        elsif p(i)(0)(1) = cr then -- the line is empty
           null;
        else -- instruction
           null; -- Wait until all labels are known 
        end if;
    end loop;
    
    for i in 0 to p'length-1 loop
    
        if p(i)(0)(1) = '$' then -- label
           null;
        elsif p(i)(0)(1) = cr then -- the line is empty
           null;
        else -- instruction        
           commands(insIndex) := processInstruction(p(i), labels);
           insIndex := insIndex + 1;
        end if;
    end loop;    
    
    return true;
end function;


end Assembler;
