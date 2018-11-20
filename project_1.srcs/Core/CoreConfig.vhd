----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 22:35:28
-- Design Name: 
-- Module Name: CoreConfig - Behavioral
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

use work.BasicTypes.all;
use work.ArchDefs.all;

package CoreConfig is

constant LOG2_PIPE_WIDTH: natural := 0 + 2;
constant PIPE_WIDTH: positive := 2**LOG2_PIPE_WIDTH;
constant ALIGN_BITS: natural := LOG2_PIPE_WIDTH + 2;
constant PC_INC: Mword := (ALIGN_BITS => '1', others => '0');    

constant FETCH_WIDTH: positive := PIPE_WIDTH; 

--constant FETCH_BLOCK_SIZE: positive := PIPE_WIDTH*2; -- in halfwords

constant IBUFFER_SIZE: positive := 2*FETCH_WIDTH;

constant USE_LINE_PREDICTOR: boolean := true;

end CoreConfig;



package body CoreConfig is



end CoreConfig;
