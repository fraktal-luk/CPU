----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 22:32:16
-- Design Name: 
-- Module Name: Core - Behavioral
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

use work.CoreConfig.all;


entity Core is
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           en : in  STD_LOGIC;
			  
			  -- address fot program mem
           iadrvalid: out std_logic;
			  iadr : out  Mword;
			  -- instruction input
			  ivalid: in std_logic;
           iin : in  WordArray(0 to PIPE_WIDTH-1);
			  
			  -- Mem load interface
			  dread: out std_logic;
           dadr : out  Mword;
			  dvalid: in std_logic;			  
           din : in  Mword;
			  
			  -- Mem store interface
			  dwrite: out std_logic;
			  doutadr: out Mword;
           dout : out  Mword;
			  
			  intallow: out std_logic;
			  intack: out std_logic;
			  -- Interrupt input (int0) and additional input (int1)
           int0 : in  STD_LOGIC;
           int1 : in  STD_LOGIC;
			  
			  filladr: in Mword;
			  fillready: in std_logic;
			  
			  -- Other buses for development 
           iaux : in  Mword;
           oaux : out  Mword			  
			  
			  );
end Core;


architecture Empty of Core is
begin

end Empty;