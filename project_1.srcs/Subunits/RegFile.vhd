----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


entity RegFile is
	generic(
	    IS_FP: boolean := false;
		WIDTH: natural := 1;
		WRITE_WIDTH: natural := 1;
		MAX_WIDTH: natural := 4		
	);
    Port (clk : in  STD_LOGIC;
          reset : in  STD_LOGIC;
          en : in  STD_LOGIC;

          writeInput: in ExecResultArray(0 to WRITE_WIDTH-1);
          
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

    --    signal  writeVec_T: std_logic_vector(0 to WIDTH-1) := (others => '0');
    --    signal  selectWrite_T: PhysNameArray(0 to WIDTH-1) := (others => (others => '0'));
    --    signal  writeValues_T: MwordArray(0 to WIDTH-1) := (others => (others => '0'));
        
        signal ch0, ch1, ch2: std_logic := '0';

	-- Memory block
	signal content: MwordArray(0 to N_PHYSICAL_REGS-1) := (others => (others => '0'));

    attribute ram_style: string;
    attribute ram_style of content: signal is "block";

	constant HAS_RESET_REGFILE: std_logic := '1';
	constant HAS_EN_REGFILE: std_logic := '1';
begin
	resetSig <= reset and HAS_RESET_REGFILE;
	enSig <= en or not HAS_EN_REGFILE;

    writeVec(0) <= writeInput(0).full and isNonzero(writeInput(0).dest);
    selectWrite(0) <= writeInput(0).dest;
    writeValues(0) <= writeInput(0).value;

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
					readValuesMW(i) <= content(slv2u(selectReadMW(i)(PHYS_REG_BITS_EFFECTIVE-1 downto 0)));
				end if;	
			end loop;
			
			-- Writing
			--if writeAllow = '1' then
				for i in 0 to WRITE_WIDTH-1 loop
					if writeVecMW(i) = '1' then
						content(slv2u(selectWriteMW(i)(PHYS_REG_BITS_EFFECTIVE-1 downto 0))) <= writeValuesMW(i);
					end if;
				end loop;
			--end if;
		end if;
	end process;
	
end Behavioral;
