----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.CoreConfig.all;

use work.PipelineGeneral.all;
use work.LogicRenaming.all;


entity RegisterMapper is
    generic(
        IS_FP: boolean := false
    );
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		rewind: in std_logic;
		
		  reserveTag: in InsTag;
		  commitTag: in InsTag;
		  rewindTag: in InsTag;

		sendingToReserve: in std_logic;
		reserveInfoA: in RenameInfoArray(0 to PIPE_WIDTH-1);
		newPhysDestsOrig: in PhysNameArray(0 to PIPE_WIDTH-1); -- to write to newest map

		sendingToCommit: in std_logic;
		commitInfoA: in RenameInfoArray(0 to PIPE_WIDTH-1);

		newPhysSources: out PhysNameArray(0 to 3*PIPE_WIDTH-1);		
		newPhysSources_NR: out PhysNameArray(0 to 3*PIPE_WIDTH-1);		
		newPhysSourcesAlt: out PhysNameArray(0 to 3*PIPE_WIDTH-1);
		newPhysSourceSelector: out std_logic_vector(0 to 3*PIPE_WIDTH-1);
		  
		  newProducers: out InsTagArray(0 to 3*PIPE_WIDTH-1);

		prevStablePhysDests: out PhysNameArray(0 to PIPE_WIDTH-1)
	);
end RegisterMapper;



architecture Behavioral of RegisterMapper is
        constant USE_OLD: boolean := true;

	constant WIDTH: natural := PIPE_WIDTH;

	signal newestMap, newestMap_NoRewind, stableMap, newestMapNext, newestMapNext_NoRewind, stableMapNext: PhysNameArray(0 to 31) := initMap(IS_FP);

    signal reserve, commit, psels: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    signal selectReserve, selectCommit: RegNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal selectNewest: RegNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal writeCommit, readStable, readStable_T, readStable_T2: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	signal readNewest, readNewest_NR, readNewest_T, readNewest_T2, readStableSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    	signal readProducers: InsTagArray(0 to 3*PIPE_WIDTH-1) := (others => (others => 'U'));

    signal readUseNewest: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');

    signal newSelectedA, stableSelectedA: RegMaskArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal ch0, ch1,  cha, chb, chc, chd, che, chf: std_logic := '0';    
begin
    newSelectedA <= getSelectedA(selectReserve, reserve, IS_FP);
    stableSelectedA <= getSelectedA(selectCommit, commit, IS_FP);

    newestMapNext <= getNextMap(newestMap, stableMap, newPhysDestsOrig, reserve, newSelectedA, sendingToReserve, rewind);
    stableMapNext <= getNextMap(stableMap, stableMap, writeCommit, "1111", stableSelectedA, sendingToCommit, '0');

    newestMapNext_NoRewind <= getNextMap(newestMap_NoRewind, stableMap, newPhysDestsOrig, reserve, newSelectedA, sendingToReserve, '0');

    psels <= --getPhysicalFloatDestSels(commitInfoA) when IS_FP else getPhysicalIntDestSels(commitInfoA);
                getPsels(commitInfoA);
    reserve <= whichTakeReg(reserveInfoA, IS_FP); 

	commit <= psels;
	
    selectReserve <= getVirtualDests(reserveInfoA);
    selectCommit <= getVirtualDests(commitInfoA);
    selectNewest <= getVirtualArgs(reserveInfoA);

    writeCommit <= getPhysicalDests(commitInfoA);	

	-- Read
	READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
	   readNewest(i) <= newestMap(slv2u(selectNewest(i)));
	   readNewest_NR(i) <= newestMap_NoRewind(slv2u(selectNewest(i)));        
       readStableSources(i) <= stableMap(slv2u(selectNewest(i)));
	end generate;

	READ_STABLE: for i in 0 to PIPE_WIDTH-1 generate
		readStable(i) <= stableMap(slv2u(selectCommit(i)));	
	end generate;
	
	-- Write	
	SYNCHRONOUS: process(clk)
	begin
	   if rising_edge(clk) then
          if sendingToReserve = '1' or rewind = '1' then
              newestMap <= newestMapNext;
              newestMap_NoRewind <= newestMapNext_NoRewind;
          end if;

          if sendingToCommit = '1' then
              stableMap <= stableMapNext;
          end if;

	   end if;
	end process;

	prevStablePhysDests <=     readStable_T when not USE_OLD
	                      else readStable;
	newPhysSources <=      readNewest_T when not USE_OLD
	                  else readNewest;

	newPhysSources_NR <= readNewest_NR;
    newPhysSourcesAlt <= readStableSources;
    newPhysSourceSelector <= not readUseNewest;

    TMP_VIEW: block
        --signal tagReserve, tagCommit: InsTag := (others => '0');

--        signal useNewest: std_logic_vector(0 to 31) := (others => '0');

--        subtype RegSubmap is PhysNameArray(0 to 31);
--        type RegMap is array(0 to PIPE_WIDTH-1) of RegSubmap;

--        type RegSelectMap is array(0 to PIPE_WIDTH-1) of std_logic_vector(0 to 31);
--        type RegSelectMap2b is array(0 to 31) of std_logic_vector(2 downto 0);

--        constant N_MAPPINGS: natural := 8 + 8 + 16;

--        subtype PhysNameRow is PhysNameArray(0 to 3);
--        type PhysNameTable is array(0 to N_MAPPINGS-1) of PhysNameRow;

--        signal mappingTable, mappingTableStable: PhysNameTable := (others => (others => (others => '0')));
--        signal mappingTablePtr, mappingTableStablePtr, mappingTablePtrNext, mappingTableStablePtrNext: natural range 0 to N_MAPPINGS-1 := 0;

--        signal mapMem0, mapMem1, mapMem2, mapMem3, mapMemS1, mapMemS2, mapMemS3: PhysNameArray(0 to N_MAPPINGS-1) := (others => (others => '0'));
--        signal mapMemS0: PhysNameArray(0 to N_MAPPINGS-1) := initMap(IS_FP);

--        --signal rowMap, rowMapStable: PhysNameArray(0 to 31) := (others => (others => '0'));
--        signal newestSelMap2b, stableSelMap2b: RegSelectMap2b := (others => (others => '0'));
        signal compressedDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

	    signal newestRowMap, newestRowMap_NoRewind, stableRowMap, newestRowMapNext, newestRowMapNext_NoRewind, stableRowMapNext: SmallNumberArray(0 to 31) := (others => sn(0));
	    signal newestSubMap, newestSubMap_NoRewind, stableSubMap, newestSubMapNext, newestSubMapNext_NoRewind, stableSubMapNext: SmallNumberArray(0 to 31) := (others => sn(0));

            signal rewindD, rewindDD, reserveD, reserveDD, commitD, commitDD: std_logic := '0';
    begin

        compressedDests <= assignDests(reserveInfoA, newPhysDestsOrig, IS_FP);

        process (clk)
        begin
            if rising_edge(clk) then
                if rewind = '1' then
                    newestRowMap <= stableRowMap;
                    newestSubMap <= stableSubMap;
                elsif sendingToReserve = '1' then
                    --newestMap_NoRewind <= newestMapNext_NoRewind;
                    for i in 0 to PIPE_WIDTH-1 loop
                        if reserve(i) = '1' then
                            newestRowMap(slv2u(selectReserve(i))) <= reserveTag(7 downto 0);
                            newestSubMap(slv2u(selectReserve(i))) <= sn(i);
                        end if;
                    end loop;
                end if;
      
                if sendingToCommit = '1' then
                    for i in 0 to PIPE_WIDTH-1 loop
                        if commit(i) = '1' then
                            stableRowMap(slv2u(selectCommit(i))) <= commitTag(7 downto 0);
                            stableSubMap(slv2u(selectCommit(i))) <= sn(i);
                        end if;
                    end loop;
                end if;
            end if;
        end process;

	READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
	   readNewest_T(i)(7 downto 2) <= newestRowMap(slv2u(selectNewest(i))) (7 downto 2);
	   readNewest_T(i)(1 downto 0) <= newestSubMap(slv2u(selectNewest(i))) (1 downto 0);
	  -- readNewest_NR(i) <= newestRowMap_NoRewind(slv2u(selectNewest(i)));        
	end generate;

    READ_STABLE: for i in 0 to PIPE_WIDTH-1 generate
		readStable_T(i)(7 downto 2) <= stableRowMap(slv2u(selectCommit(i))) (7 downto 2);	
		readStable_T(i)(1 downto 0) <= stableRowMap(slv2u(selectCommit(i))) (1 downto 0);	
	end generate;


        DELAYES: process (clk)
        begin
            if rising_edge(clk) then
                rewindD <= rewind;
                rewindDD <= rewindD;
                reserveD <= sendingToReserve;
                reserveDD <= reserveD;
                commitD <= sendingToCommit;
                commitDD <= commitD;
            end if;
        end process;

    end block;

    --newProducers <= readProducers;

    
end Behavioral;
