----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 22:40:42
-- Design Name: 
-- Module Name: CoreTB - Behavioral
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
use work.CoreConfig.all;

use work.Helpers.all;

use work.Assembler.all;
use work.Emulate.all;

ENTITY CoreWithCacheTB IS
END CoreWithCacheTB;


ARCHITECTURE Behavior OF CoreWithCacheTB IS

    signal clk : std_logic := '0';
    signal reset : std_logic := '0';
    signal en : std_logic := '0';

    signal int0a, int0b, int1sig: std_logic := '0';

    signal ints, intAcks, eventsOut: std_logic_vector(0 to 7) := (others => '0');

    -- Clock period definitions
    constant clk_period : time := 10 ns;

    procedure cycle(signal clk: in std_logic) is
    begin
        wait until rising_edge(clk);
    end procedure;

    procedure cycle is
    begin
        wait until rising_edge(clk);
    end procedure;

    procedure startTest(signal testToDo, int0b: out std_logic) is
    begin
      cycle(clk);
      --testToDo <= '1';
      int0b <= '1';
      cycle(clk);                     
      --testToDo <= '0';
      int0b <= '0';
    end procedure;

BEGIN

    ints(0) <= int0b;

   -- Clock process definitions
   clk_process: process
   begin
		clk <= '1';
		wait for clk_period/2;
		clk <= '0';
		wait for clk_period/2;
   end process;

    STIM: process
    begin
        wait for 15 ns;
        
        reset <= '1';
        
        wait for 20 ns;
        
        reset <= '0';
        
        wait for 20 ns;
        
        int0b <= '1';
        wait for 20 ns;
        int0b <= '0';
        wait;
    end process;


    UUT: entity work.CoreWithCaches
    port map (
        clk => clk,
        reset => reset,
        en => '0',
        
        ints => ints,
        intAcks => intAcks,
        
        dataInWrite => '0',
        dataInAdr => (others => '0'),
        dataInValue => (others => '0'),
        
        dataOutRead => '0',
        dataOutAdr => (others => '0'),
        dataOutValue => open,
        
        dataFw => open,
        dataFwAdr => open,
        dataFwValue => open,
        
        insInWrite => '0',
        insInAdr => (others => '0'),
        insInValue => (others => '0'),
        
        eventsOut => eventsOut
    );



END;
