----------------------------------------------------------------------------------

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
    generic(
        IS_FP: boolean := false
    );
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

	   signal newestMap_N, newestMapNext_N, stableMap_N, stableMapNext_N: PhysNameArray(0 to 31) := initMap;

	function initMap return PhysNameArray is
		variable res: PhysNameArray(0 to 31) := (others => (others=> '0'));
	begin
		for i in 0 to 31 loop
			res(i) := i2slv(i, PhysName'length);
			if IS_FP then
			 res(i) := i2slv(i + 1, PhysName'length); -- CAREFUL: No reg 0 for FP
			end if;
		end loop;
		return res;
	end function;
    
    signal reserve, reserveNotOv, commit, commitNotOv: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    signal selectReserve, selectCommit, selectStable
            : RegNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal selectNewest: RegNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal writeReserve, writeCommit: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	signal	readNewest: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
	signal	readStable: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    
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
    
    
    signal selMask0, selMask1, selMask2, selMask3: std_logic_vector(0 to 31) := (others => '0');
    signal selMaskS0, selMaskS1, selMaskS2, selMaskS3: std_logic_vector(0 to 31) := (others => '0');
    
    function getSelMask(adr: RegName; enInt, enFP: std_logic; IS_FP: boolean) return std_logic_vector is
        variable res: std_logic_vector(0 to 31) := (others => '0');
    begin
        for i in 0 to 31 loop
            if IS_FP then
                res(i) := bool2std(adr(4 downto 0) & enFP = i2slv(i, 5) & '1');
            else
                res(i) := bool2std(adr(4 downto 0) & enInt = i2slv(i, 5) & '1');
            end if;
        end loop;
        
        if not IS_FP then
            res(0) := '0'; -- if Integer
        end if;
        return res;
    end function;
    
    function getSelection(p0, p1, p2, p3, st, prev: PhysName; s0, s1, s2, s3, rew: std_logic) return PhysName is
        variable res: PhysName := prev;
        variable t0, t1, t2: PhysName := (others => '0');
    begin       
        if s1 = '1' then
            t0 := p1;
        elsif s0 = '1' then
            t0 := p0;
        else
            t0 := prev;
        end if;

        if rew = '1' then
            t1 := st;
        elsif s3 = '1' then
            t1 := p3;
        else
            t1 := p2;
        end if;
        
        if (rew or s3 or s2) = '1' then
            res := t1;
        else
            res := t0;
        end if;
        
        return res;
    end function;
    
    function getNextMap(content, stable: PhysNameArray; inputArr: PhysNameArray; sm0, sm1, sm2, sm3: std_logic_vector(0 to 31); rew: std_logic) return PhysNameArray is
        variable res: PhysNameArray(0 to 31) := (others => (others => '0'));
    begin
        for i in 0 to 31 loop
            res(i) := getSelection(inputArr(0), inputArr(1), inputArr(2), inputArr(3), stable(i), content(i),
                                   sm0(i), sm1(i), sm2(i), sm3(i), rew);
        end loop;
        return res;
    end function;
    
    signal ch0, ch1: std_logic := '0';
begin	
        selMask0 <= getSelMask(selectReserve(0), reserve(0), reserve(0), IS_FP);
        selMask1 <= getSelMask(selectReserve(1), reserve(1), reserve(1), IS_FP);
        selMask2 <= getSelMask(selectReserve(2), reserve(2), reserve(2), IS_FP);
        selMask3 <= getSelMask(selectReserve(3), reserve(3), reserve(3), IS_FP);

        newestMapNext_N <= getNextMap(newestMap, stableMap, writeReserve, selMask0, selMask1, selMask2, selMask3, rewind);
        
        selMaskS0 <= getSelMask(selectCommit(0), commit(0), commit(0), IS_FP);
        selMaskS1 <= getSelMask(selectCommit(1), commit(1), commit(1), IS_FP);
        selMaskS2 <= getSelMask(selectCommit(2), commit(2), commit(2), IS_FP);
        selMaskS3 <= getSelMask(selectCommit(3), commit(3), commit(3), IS_FP);

        stableMapNext_N <= getNextMap(stableMap, stableMap, writeCommit, selMaskS0, selMaskS1, selMaskS2, selMaskS3, '0');
    
            ch0 <= bool2std(newestMap_N = newestMap);
            ch1 <= bool2std(stableMap_N = stableMap);
    

	reserve <= whichTakeReg(stageDataToReserve, IS_FP);
	reserveNotOv <= reserve and not findOverriddenDests(stageDataToReserve, IS_FP);
	commit <= whichPutReg(stageDataToCommit, IS_FP) and not getExceptionMask(stageDataToCommit);
	commitNotOv <= commit and not findOverriddenDests(stageDataToCommit, IS_FP);
	
	selectReserve <= getVirtualDests(stageDataToReserve);
	selectCommit <= getVirtualDests(stageDataToCommit);
	selectNewest <= getVirtualArgs(stageDataToReserve);
	selectStable <= getVirtualDests(stageDataToCommit);
	
	writeReserve <= selectPhysDests(newPhysDests, reserve);
	writeCommit <= getPhysicalDests(stageDataToCommit);
	
	newPhysSources <= readNewest; 
	--prevStablePhysDests <= readStable;


	-- Read
	READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
		readNewest(i) <= newestMap(slv2u(selectNewest(i)));
	end generate;

	READ_STABLE: for i in 0 to PIPE_WIDTH-1 generate
		readStable(i) <= stableMap(slv2u(selectStable(i)));
	end generate;
	
	-- Write	
	SYNCHRONOUS: process(clk)
	begin
		if rising_edge(clk) then
		      if sendingToReserve = '1' or rewind = '1' then
		          newestMap <= newestMapNext_N;
		      end if;
		      
		      if sendingToCommit = '1' then
		          stableMap <= stableMapNext_N;
		      end if;
		      
--			-- Rewind if commanded
--			if rewind = '1' then
--				newestMap <= stableMap;
--			end if;
			
--			-- Write
--			if sendingToReserve = '1' and rewind = '0' then
--				for i in 0 to PIPE_WIDTH-1 loop
--					if reserveNotOv(i) = '1' then
--						newestMap(slv2u(selectReserve(i))) <= writeReserve(i);
--							assert isNonzero(writeReserve(i)) = '1' report "Mapping a speculative register to p0!";
--					end if;
--				end loop;	
--			end if;

--			if sendingToCommit = '1' then -- and rewind = '0' then -- block when rewinding??		
--				for i in 0 to PIPE_WIDTH-1 loop
--					if commitNotOv(i) = '1' then
--						stableMap(slv2u(selectCommit(i))) <= writeCommit(i);
--							assert isNonzero(writeCommit(i)) = '1' report "Mapping a stable register to p0!";						
--					end if;
--				end loop;	
--			end if;
			
			prevStablePhysDests <= readStable;
		end if;
	end process;
end Behavioral;
