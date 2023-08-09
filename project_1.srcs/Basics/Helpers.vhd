
-------------------------------------------------------------------------------------------------------------
-- This file defines some general functions to manipulate on bit vectors and numbers 
-------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;



package Helpers is

function find1free(mask: std_logic_vector) return integer;
function find2free(mask: std_logic_vector) return integer;

function findAvailable(mask: std_logic_vector) return integer;           -- finds first empty slot
function findCompletelyAvailable(mask: std_logic_vector) return integer; -- finds first empty [even:odd] pair

function findInsertion(pA, cA: integer; fullMask, inputMask: std_logic_vector) return integer;
function findInsertionSimplified(pA, cA: integer; fullMask, inputMask: std_logic_vector) return integer;

function allOf(v: std_logic_vector) return std_logic;
function anyOf(v: std_logic_vector) return std_logic;

end Helpers;


package body Helpers is


function find1free(mask: std_logic_vector) return integer is
begin
    for i in 0 to mask'length-1 loop
        if (mask(i)) /= '1' then
            return i;
        end if;
    end loop;   
    return -1;
end function;

function find2free(mask: std_logic_vector) return integer is
begin
    for i in 0 to mask'length-2 loop
        if (mask(i) or mask(i+1)) /= '1' then
            return i;
        end if;
    end loop;
    return -1;
end function;



function findAvailable(mask: std_logic_vector) return integer is
begin       
    if mask(mask'length-1) = '1' then
        return -1;
    end if;
    
    for i in mask'length-2 downto 0 loop    
        if mask(i) = '1' then
            -- last full
             return i + 1; 
        end if;
    end loop;
    return 0;
end function; 


function findCompletelyAvailable(mask: std_logic_vector) return integer is
    constant LEN: natural := mask'length;
begin        
    -- Simplified option
    if (mask(LEN-2) or mask(LEN-1)) = '1' then
        return -1;
    end if;
    
    for i in LEN/2 - 1 downto 0 loop    
        if mask(2*i) = '1' or mask(2*i+1) = '1' then
            -- last full -> next is FirstCompletelyAvailable
            return 2*i+2;
        end if;
    end loop;
    return 0;
end function;

--function findPartiallyAvailable(mask: std_logic_vector) return integer is
--begin
--        -- Non simplified option
--        for i in LEN/2 - 1 downto 0 loop
--            if fullMask(2*i+1) = '1' then
--                -- last full -> next is FirstCompletelyAvailable
--            elsif fullMask(2*i) = '1' then
--                -- this is FirstPartiallyAvailable
                
--                exit;
--            end if;
--        end loop; 

function findInsertion(pA, cA: integer; fullMask, inputMask: std_logic_vector) return integer is
    constant oddPa: boolean := (pA mod 2) /= 0;
begin
    if inputMask(0) = '1' then
        return cA;
    elsif inputMask(1) = '1' then
        if oddPa then
            return pA-1;
        else
            return pA;
        end if;
    elsif inputMask(2) = '1' then
        return cA-2;
    else
        if oddPa then
            return pA-1;
        else
            return pA;
        end if;        
    end if;
    
    return -1;
end function;

function findInsertionSimplified(pA, cA: integer; fullMask, inputMask: std_logic_vector) return integer is
    constant oddPa: boolean := (pA mod 2) /= 0;
    variable res: integer := 0;
begin
    if inputMask(0) = '1' then
        res := cA;
    elsif inputMask(1) = '1' then
        res := cA;
    elsif inputMask(2) = '1' then
        res := cA-2;
    else
        res := cA-2;
        if res < 0 then
            res := 0;
        end if;
    end if;
    
    return res;
end function;

function allOf(v: std_logic_vector) return std_logic is
begin
    for i in v'range loop
        if v(i) /= '1' then
            return '0';
        end if;
    end loop;
    return '1';
end function;

function anyOf(v: std_logic_vector) return std_logic is
begin
    for i in v'range loop
        if v(i) = '1' then
            return '1';
        end if;
    end loop;
    return '0';
end function;

end package body;
