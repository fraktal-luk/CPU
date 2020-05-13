
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;


package CpuText is
    
    
function padLeft(s: string; n: natural) return string;
function padRight(s: string; n: natural) return string;  

function std2char(x: std_logic) return character;
function std2str(x: std_logic) return string;
function slv2char(x: std_logic_vector) return character;
function slv2bin(x: std_logic_vector; n: natural) return string;
function slv2hex(x: std_logic_vector; n: natural) return string;
function slv2hex(x: std_logic_vector) return string;
function w2hex(x: std_logic_vector) return string;


constant HEX_TAB: string(1 to 16) := "0123456789abcdef";

end package;



package body CpuText is

function padLeft(s: string; n: natural) return string is
    variable res: string(1 to n) := (others => ' ');
    variable lim: natural := 0;
begin
    if n < s'length then
        lim := n;
    else
        lim := s'length;
    end if;
    res(1 to lim) := s(1 to lim);
    return res;
end function;

function padRight(s: string; n: natural) return string is
    variable res: string(1 to n) := (others => ' ');
    variable lim: natural := 0;
begin
    if n < s'length then
        lim := n;
    else
        lim := s'length;
    end if;
    res(n-lim+1 to n) := s(1 to lim);
    return res;
end function;    


function std2char(x: std_logic) return character is
begin
    if x = '1' then
        return '1';
    elsif x = '0' then
        return '0';
    else
        return '!';
    end if;    
    
end function;

function std2str(x: std_logic) return string is
begin
    if x = '1' then
        return "1";
    elsif x = '0' then
        return "0";
    else
        return "!";
    end if;    
    
end function;

function slv2char(x: std_logic_vector) return character is
begin
    assert x'length <= 0 report "Vector longer than 4 bits!" severity failure;
    return HEX_TAB(slv2u(x(3 downto 0)));      
end function;

function slv2bin(x: std_logic_vector; n: natural) return string is
    variable res: string(1 to n) := (others => ' ');        
begin
    for i in 0 to n-1 loop
        res(n-i) := std2char(x(i));
    end loop;
    return res;
end function;

function slv2hex(x: std_logic_vector; n: natural) return string is
    variable vec: std_logic_vector(4*n-1 downto 0) := (others => '0');
    variable res: string(1 to n) := (others => '!');        
begin
    vec(x'length-1 downto 0) := x;
    for i in 0 to n-1 loop
        res(n-i) := HEX_TAB(1 + slv2u(vec(4*i + 3 downto 4*i)));
    end loop;
    return res;
end function;

function slv2hex(x: std_logic_vector) return string is
begin
    return slv2hex(x, (x'length+3)/4);
end function;

function w2hex(x: std_logic_vector) return string is
begin
    return slv2hex(x, 8);
end function;


    
end package body;
