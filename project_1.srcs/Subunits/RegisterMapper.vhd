----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:07:11 01/07/2017 
-- Design Name: 
-- Module Name:    RegisterMappingUnit - Behavioral 
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
use work.InstructionState.all;
use work.CoreConfig.all;

use work.PipelineGeneral.all;
use work.LogicRenaming.all;


entity RegisterMapper is
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		rewind: in std_logic;
		causingInstruction: in InstructionState; -- CAREFUL: not used now, mapping restored from stable
		
		sendingToReserve: in std_logic;
		stageDataToReserve: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		newPhysDests: in PhysNameArray(0 to PIPE_WIDTH-1); -- to write to newest map

		sendingToCommit: in std_logic;
		stageDataToCommit: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		physCommitDests_TMP: in PhysNameArray(0 to PIPE_WIDTH-1); -- to write to stable map
		
		prevNewPhysDests: out PhysNameArray(0 to PIPE_WIDTH-1);
		newPhysSources: out PhysNameArray(0 to 3*PIPE_WIDTH-1);
		
		prevStablePhysDests: out PhysNameArray(0 to PIPE_WIDTH-1);
		stablePhysSources: out PhysNameArray(0 to 3*PIPE_WIDTH-1)	
	);
end RegisterMapper;



architecture Behavioral of RegisterMapper is
	constant	WIDTH: natural := PIPE_WIDTH;

		signal virtDests, virtCommitDests: RegNameArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
		signal virtSources: RegNameArray(0 to 3*PIPE_WIDTH-1) := (others=>(others=>'0'));

	function initMap return PhysNameArray;
	
	signal newestMap, stableMap: PhysNameArray(0 to 31) := initMap;

	function initMap return PhysNameArray is
		variable res: PhysNameArray(0 to 31) := (others => (others=> '0'));
	begin
		for i in 0 to 31 loop
			res(i) := i2slv(i, PhysName'length);
		end loop;
		return res;
	end function;
    
    signal reserve, commit: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    signal selectReserve, selectCommit, selectStable
            : RegNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal selectNewest: RegNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal writeReserve: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	signal	readNewest, writeCommit: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
	signal	readStable: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	
    function getVirtualArgs(insVec: InstructionSlotArray) return RegNameArray is
        variable res: RegNameArray(0 to 3*insVec'length-1) := (others=>(others=>'0'));
    begin
        for i in insVec'range loop
            res(3*i+0) := insVec(i).ins.virtualArgSpec.args(0)(4 downto 0);
            res(3*i+1) := insVec(i).ins.virtualArgSpec.args(1)(4 downto 0);
            res(3*i+2) := insVec(i).ins.virtualArgSpec.args(2)(4 downto 0);
        end loop;
        return res;
    end function;

    
    function getVirtualDests(insVec: InstructionSlotArray) return RegNameArray is
        variable res: RegNameArray(0 to insVec'length-1) := (others=>(others=>'0'));
    begin
        for i in insVec'range loop
            res(i) := insVec(i).ins.virtualArgSpec.dest(4 downto 0);
        end loop;
        return res;
    end function;
    
    function selectPhysDests(newDests: PhysNameArray; taking: std_logic_vector) return PhysNameArray is
        variable res: PhysNameArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
        variable num: natural := 0;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            num := countOnes(taking(0 to i-1));
            res(i) := newDests(num);
        end loop;
        return res;
    end function;
begin	

	reserve <= whichTakeReg(stageDataToReserve) and not findOverriddenDests(stageDataToReserve);
--	commit <= gprCommitReq.sel;
	
	selectReserve <= getVirtualDests(stageDataToReserve);
--	selectCommitMW(0 to WIDTH-1) <= gprCommitReq.index;
	selectNewest <= getVirtualArgs(stageDataToReserve);
--	selectStableMW(0 to WIDTH-1) <= virtCommitDests;
	
	writeReserve <= selectPhysDests(newPhysDests, reserve);
--	writeCommit <= gprCommitReq.value;
	
--	newPhysSources <= readNewestMW(0 to 3*WIDTH-1); 
--	prevStablePhysDests <= readStableMW(0 to WIDTH-1);

--		virtSources <= getVirtualArgs(stageDataToReserve);
--		virtDests <= getVirtualDests(stageDataToReserve); -- // UNUSED?
			
--		gprReserveReq <= getRegMapRequest(stageDataToReserve, newPhysDests);	
--		gprCommitReq <= getRegMapRequest(stageDataToCommit, physCommitDests);

--		virtCommitDests <= getVirtualDests(stageDataToCommit);
--		physCommitDests <= getPhysicalDests(stageDataToCommit);


	-- Read
	READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
		readNewest(i) <= newestMap(slv2u(selectNewest(i)));
	end generate;

	READ_STABLE: for i in 0 to PIPE_WIDTH-1 generate
		readStable(i) <= stableMap(slv2u(selectStable(i)));
	end generate;
			
	SYNCHRONOUS: process(clk)
	begin
		if rising_edge(clk) then
			-- Rewind if commanded
			if rewind = '1' then
				newestMap <= stableMap;
			end if;
			
			-- Write
			if sendingToReserve = '1' and rewind = '0' then
				for i in 0 to PIPE_WIDTH-1 loop
					if reserve(i) = '1' then
						newestMap(slv2u(selectReserve(i))) <= writeReserve(i);
							assert isNonzero(writeReserve(i)) = '1' report "Mapping a speculative register to p0!";
					end if;
				end loop;	
			end if;

			if sendingToCommit = '1' then -- and rewind = '0' then -- block when rewinding??		
				for i in 0 to PIPE_WIDTH-1 loop
					if commit(i) = '1' then
						stableMap(slv2u(selectCommit(i))) <= writeCommit(i);
							assert isNonzero(writeCommit(i)) = '1' report "Mapping a stable register to p0!";						
					end if;
				end loop;	
			end if;
		end if;
	end process;
end Behavioral;
