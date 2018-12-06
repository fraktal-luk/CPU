
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;

use work.PipelineGeneral.all;


entity RegisterReadyTable is
	generic(
		WRITE_WIDTH: integer := 1
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		sendingToReserve: in std_logic;
		stageDataToReserve: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		newPhysDests: in PhysNameArray(0 to PIPE_WIDTH-1);
	
		stageDataReserved: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		writingMask: in std_logic_vector(0 to WRITE_WIDTH-1);
		writingData: in InstructionStateArray(0 to WRITE_WIDTH-1);
		
		readyRegFlagsNext: out std_logic_vector(0 to 3*PIPE_WIDTH-1)
	);
end RegisterReadyTable;



architecture Behavioral of RegisterReadyTable is
		constant WIDTH: natural := WRITE_WIDTH;

		signal readyTableClearAllow: std_logic := '0';
		signal readyTableClearSel: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');

		signal readyRegsSig: std_logic_vector(0 to N_PHYSICAL_REGS-1) := (0 to 31 => '1', others=>'0');
			signal altMask: std_logic_vector(0 to WRITE_WIDTH-1) := (others => '0');
			signal altDests: PhysNameArray(0 to WRITE_WIDTH-1) := (others => (others => '0'));
			
function extractReadyRegBits(bits: std_logic_vector; data: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to 3*data'length-1) := (others => '0'); -- 31) := (others=>'0');
begin
	for i in 0 to data'length-1 loop
		res(3*i + 0) := bits(slv2u(data(i).ins.physicalArgSpec.args(0)(PHYS_REG_BITS-1 downto 0)));
		res(3*i + 1) := bits(slv2u(data(i).ins.physicalArgSpec.args(1)(PHYS_REG_BITS-1 downto 0)));
		res(3*i + 2) := bits(slv2u(data(i).ins.physicalArgSpec.args(2)(PHYS_REG_BITS-1 downto 0)));
	end loop;		
	return res;
end function;	
begin
		readyTableClearAllow <= sendingToReserve; -- for ready table
		--readyTableClearSel <= getDestMask(stageDataToReserve);	-- for ready table		
		
		--altMask <= getArrayDestMask(writingData, writingMask);
		--altDests <= getArrayPhysicalDests(writingData);
		
		IMPL: block
			signal content: std_logic_vector(0 to N_PHYSICAL_REGS-1) := (0 to 31 => '1', others => '0');
		begin
				SYNCHRONOUS: process(clk)
				begin
					if rising_edge(clk) then
						for i in 0 to altMask'length-1 loop
							if altMask(i) = '1' then
								-- set 
								content(slv2u(altDests(i))) <= '1';
							end if;
						end loop;	
							
						if readyTableClearAllow = '1' then							
							for i in 0 to PIPE_WIDTH-1 loop								
								if readyTableClearSel(i) = '1' then
									-- clear
									content(slv2u(newPhysDests(i))) <= '0';						
								end if;
							end loop;
						end if;			
					end if;
				end process;
				
			readyRegFlagsNext <= extractReadyRegBits(content, stageDataReserved);				
		end block;
end Behavioral;
