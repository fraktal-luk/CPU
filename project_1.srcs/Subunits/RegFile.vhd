----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:58:51 07/01/2016 
-- Design Name: 
-- Module Name:    RegisterFile0 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
use work.Helpers.all;
use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


entity RegFile is
	generic(
		WIDTH: natural := 1;
		WRITE_WIDTH: natural := 1;
		MAX_WIDTH: natural := 4		
	);
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           en : in  STD_LOGIC;

			  writeAllow: in std_logic;
					writeInput: in InstructionSlotArray(0 to WRITE_WIDTH-1);
			  
--			  writeVec: in std_logic_vector(0 to WIDTH-1);			  
--			  selectWrite: in PhysNameArray(0 to WIDTH-1);			  	
--			  writeValues: in MwordArray(0 to WIDTH-1);	

			  readAllowVec: in std_logic_vector(0 to 3*WIDTH-1);
			  selectRead: in PhysNameArray(0 to 3*WIDTH-1);
			  readValues: out MwordArray(0 to 3*WIDTH-1)
			  );
end RegFile;


architecture Behavioral of RegFile is
	signal resetSig, enSig: std_logic := '0';

	signal writeVecMW: std_logic_vector(0 to MAX_WIDTH-1) := (others => '0');
	signal selectWriteMW: PhysNameArray(0 to MAX_WIDTH-1) := (others => (others => '0'));
	signal writeValuesMW: MwordArray(0 to MAX_WIDTH-1) := (others => (others => '0'));
	signal selectReadMW: PhysNameArray(0 to 3*MAX_WIDTH-1) := (others => (others => '0'));	
	signal readValuesMW: MwordArray(0 to 3*MAX_WIDTH-1) := (others => (others => '0'));	


			signal  writeVec: std_logic_vector(0 to WIDTH-1) := (others => '0');
			signal  selectWrite: PhysNameArray(0 to WIDTH-1) := (others => (others => '0'));
			signal  writeValues: MwordArray(0 to WIDTH-1) := (others => (others => '0'));

	-- Memory block
	signal content: MwordArray(0 to N_PHYSICAL_REGS-1) := (others => (others => '0'));

	constant HAS_RESET_REGFILE: std_logic := '1';
	constant HAS_EN_REGFILE: std_logic := '1';
begin
	resetSig <= reset and HAS_RESET_REGFILE;
	enSig <= en or not HAS_EN_REGFILE;

		
		--writeVec(0 to INTEGER_WRITE_WIDTH-1) <= 
		--								getArrayDestMask(extractData(writeInput), extractFullMask(writeInput));
		writeVec(0) <= writeInput(0).ins.physicalArgSpec.intDestSel; -- CAREFUL: works only for integer								
		--selectWrite(0 to INTEGER_WRITE_WIDTH-1) <= getArrayPhysicalDests(extractData(writeInput));
		selectWrite(0) <= writeInput(0).ins.physicalArgSpec.dest;
		--writeValues(0 to INTEGER_WRITE_WIDTH-1) <= getArrayResults(extractData(writeInput));
        writeValues(0) <= writeInput(0).ins.result;

	writeVecMW(0 to WIDTH-1) <= writeVec;
		writeVecMW(WIDTH to MAX_WIDTH-1) <= (others => '0');
	selectWriteMW(0 to WIDTH-1) <= selectWrite;
	writeValuesMW(0 to WIDTH-1) <= writeValues;
	
	selectReadMW(0 to 3*WIDTH-1) <= selectRead;
	readValues <= readValuesMW(0 to 3*WIDTH-1);


	SYNCHRONOUS: process(clk)
	begin
		if rising_edge(clk) then
			-- Reading			
			for i in 0 to readValuesMW'length - 1 loop
				if readAllowVec(i) = '1' then
					readValuesMW(i) <= content(slv2u(selectReadMW(i)));
				end if;	
			end loop;
			
			-- Writing
			if writeAllow = '1' then
				for i in 0 to WRITE_WIDTH-1 loop
					if writeVecMW(i) = '1' then
						content(slv2u(selectWriteMW(i))) <= writeValuesMW(i);
					end if;
				end loop;
			end if;
		end if;
	end process;
	
end Behavioral;
