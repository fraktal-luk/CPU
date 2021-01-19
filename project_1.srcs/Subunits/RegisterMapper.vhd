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
		newPhysDestsOrig: in PhysNameArray(0 to PIPE_WIDTH-1); -- to write to newest map

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

	function initMap return PhysNameArray;
	
	signal newestMap, newestMap_T, stableMap, stableMap_T, newestMapNext, newestMapNext_T, stableMapNext, stableMapNext_T: PhysNameArray(0 to 31) := initMap;
    
    signal reserve, reserve_T, commit, psels, fullMaskReserve, fullMaskCommit,
             excMaskReserve, excMaskCommit, causingMaskReserve, causingMaskCommit, killedMaskReserve, killedMaskCommit,
             ignoredMaskReserve, ignoredMaskCommit,
             igVselReserve, igVselCommit, kiVselReserve, kiVselCommit,
            vselReserve, vselCommit, pselReserve, pselCommit: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    signal selectReserve, selectCommit, selectStable: RegNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal selectNewest: RegNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal writeReserve, writeCommit: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	signal readNewest: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
	signal readStable: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));

    signal selMask0, selMask1, selMask2, selMask3: std_logic_vector(0 to 31) := (others => '0');
    signal selMaskS0, selMaskS1, selMaskS2, selMaskS3: std_logic_vector(0 to 31) := (others => '0');
   
   
        signal tagReserve, tagCommit: InsTag := (others => '0');

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
    
    function selectPhysDests(newDests: PhysNameArray; taking: std_logic_vector) return PhysNameArray is
        variable res: PhysNameArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
        variable num: natural := 0;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            num := countOnes(taking(0 to i-1));
            res(i) := newDests(num);
        end loop;
        
            if TMP_PARAM_DEST_MOVE then
                res := newDests;
            end if;
        
        return res;
    end function;    
 
    function getSelMask(adr: RegName; enInt, enFP: std_logic; IS_FP: boolean) return std_logic_vector is
        variable res: std_logic_vector(0 to 31) := (others => '0');
    begin
        for i in 0 to 31 loop -- selected if reg number agrees and enabled flag set
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
    
    -- Selection:   prev when rew
    --        else {p3:p0} by priority {s3:s0} (s1 overrides s0 etc.)
    --        else st (stable) (when not rew and not any of s0-s3)   
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


        function getSelection_New(p0, p1, p2, p3, st, prev: PhysName; reserve: std_logic_vector; s0, s1, s2, s3, rew: std_logic) return PhysName is
            variable res: PhysName := prev;
            variable t0, t1, t2: PhysName := (others => '0');
            constant resVec: PhysNameArray(0 to 3) := (p0, p1, p2, p3);
            constant r0: std_logic := reserve(0);
            constant r1: std_logic := reserve(1);
            constant r2: std_logic := reserve(2);
            constant r3: std_logic := reserve(3);
            variable sel: std_logic_vector(1 downto 0) := (others => '0'); 
        begin   
                if (s3 and r2 and r1 and r0) = '1' then
                    sel := "11";
                elsif ((s3 and ((r1 and r0) or (r2 and r0) or (r2 and r1))) or (s2 and r1 and r0)) = '1' then
                    sel := "10";
                elsif ((s3 and (r2 or r1 or r0)) or (s2 and (r1 or r0)) or (s1 and r0)) = '1' then
                    sel := "01";
                else
                    sel := "00";
                end if;
                ----------
                if --(s0 or s1 or s2 or s3)  = '0' then
                    rew = '1' then
                    res := st;
                else
                    res := resVec(slv2u(sel));                    
                end if;
            return res;
        end function;


        function getNextMap_New(content, stable: PhysNameArray; inputArr: PhysNameArray; reserve: std_logic_vector; sm0, sm1, sm2, sm3: std_logic_vector(0 to 31); sending, rew: std_logic)
        return PhysNameArray is
            variable res: PhysNameArray(0 to 31) := content;--(others => (others => '0'));
            variable enMask: std_logic_vector(0 to 31) := (others => '0');
        begin
            for i in 0 to 31 loop
                enMask(i) :=  (sending and (sm0(i) or sm1(i) or sm2(i) or sm3(i))) 
                            or rew;
                
                if enMask(i) = '1' then
                    res(i) := getSelection_New(inputArr(0), inputArr(1), inputArr(2), inputArr(3), stable(i), content(i), reserve,
                                           sm0(i), sm1(i), sm2(i), sm3(i), rew);
                end if;                                   
            end loop;
            return res;
        end function;
    
    signal ch0, ch1: std_logic := '0';
begin	
    selMask0 <= getSelMask(selectReserve(0), reserve(0), reserve(0), IS_FP);
    selMask1 <= getSelMask(selectReserve(1), reserve(1), reserve(1), IS_FP);
    selMask2 <= getSelMask(selectReserve(2), reserve(2), reserve(2), IS_FP);
    selMask3 <= getSelMask(selectReserve(3), reserve(3), reserve(3), IS_FP);

    newestMapNext <= --getNextMap(newestMap, stableMap, writeReserve, selMask0, selMask1, selMask2, selMask3, rewind);
                      getNextMap_New(newestMap, stableMap, newPhysDestsOrig, reserve, selMask0, selMask1, selMask2, selMask3, sendingToReserve, rewind);
        newestMapNext_T <= getNextMap_New(newestMap_T, stableMap, newPhysDestsOrig, reserve, selMask0, selMask1, selMask2, selMask3, sendingToReserve, rewind);
    
        ch0 <= bool2std(stableMapNext_T = stableMapNext);
        ch1 <= bool2std(stableMap_T = stableMap);
    
    selMaskS0 <= getSelMask(selectCommit(0), commit(0), commit(0), IS_FP);
    selMaskS1 <= getSelMask(selectCommit(1), commit(1), commit(1), IS_FP);
    selMaskS2 <= getSelMask(selectCommit(2), commit(2), commit(2), IS_FP);
    selMaskS3 <= getSelMask(selectCommit(3), commit(3), commit(3), IS_FP);

    stableMapNext_T <= getNextMap(stableMap_T, stableMap, writeCommit, selMaskS0, selMaskS1, selMaskS2, selMaskS3, '0');
          
          stableMapNext <= getNextMap_New(stableMap, stableMap, writeCommit, "1111", selMaskS0, selMaskS1, selMaskS2, selMaskS3, sendingToCommit, '0');

    psels <= getPhysicalFloatDestSels(stageDataToCommit) when IS_FP else getPhysicalIntDestSels(stageDataToCommit);

	reserve <= whichTakeReg(stageDataToReserve, IS_FP);
	   reserve_T <= whichTakeReg_T(stageDataToReserve, IS_FP);

        vselReserve <= getVirtualIntDestSels(stageDataToReserve) when sendingToReserve = '1' else (others => '0');
	    pselReserve <= getPhysicalIntDestSels(stageDataToReserve) when sendingToReserve = '1' else (others => '0');
	    fullMaskReserve <= extractFullMask(stageDataToReserve) when sendingToReserve = '1' else (others => '0');
	       tagReserve <= stageDataToReserve(0).ins.tags.renameIndex;

            excMaskReserve <= getExceptionMask(stageDataToReserve) when sendingToReserve = '1' else (others => '0');
            causingMaskReserve <= getCausingMask(stageDataToReserve) when sendingToReserve = '1' else (others => '0');
            killedMaskReserve <= getKilledMask(stageDataToReserve)  when sendingToReserve = '1' else (others => '0');
            ignoredMaskReserve <= getIgnoredMask(stageDataToReserve)  when sendingToReserve = '1' else (others => '0');

	           igVselReserve <= ignoredMaskReserve and vselReserve;
	
	
        vselCommit <= getVirtualIntDestSels(stageDataToCommit)  when sendingToCommit = '1' else (others => '0');
        pselCommit <= getPhysicalIntDestSels(stageDataToCommit) when sendingToCommit = '1' else (others => '0');
        fullMaskCommit <= extractFullMask(stageDataToCommit) when sendingToCommit = '1' else (others => '0');
	       tagCommit <= stageDataToCommit(0).ins.tags.renameIndex;
    
            excMaskCommit <= getExceptionMask(stageDataToCommit) when sendingToCommit = '1' else (others => '0');
            causingMaskCommit <= getCausingMask(stageDataToCommit) when sendingToCommit = '1' else (others => '0');
            killedMaskCommit <= getKilledMask(stageDataToCommit) when sendingToCommit = '1' else (others => '0');
            ignoredMaskCommit <= getIgnoredMask(stageDataToCommit) when sendingToCommit = '1' else (others => '0');
	
	           igVselCommit <= ignoredMaskCommit and vselCommit;
	           kiVselCommit <= killedMaskCommit and vselCommit;
	
	commit <= psels;
	
	selectReserve <= getVirtualDests(stageDataToReserve);
	selectCommit <= getVirtualDests(stageDataToCommit);
	selectNewest <= getVirtualArgs(stageDataToReserve);
	selectStable <= selectCommit;
	
	writeReserve <= selectPhysDests(newPhysDests, reserve);
	writeCommit <= getPhysicalDests(stageDataToCommit);
	
	newPhysSources <= readNewest;

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
              newestMap <= newestMapNext;
          end if;
          
                newestMap_T <= newestMapNext_T;
                stableMap_T <= stableMapNext_T;
          
          if sendingToCommit = '1' then
              stableMap <= stableMapNext;
          end if;			
	      prevStablePhysDests <= readStable;
	   end if;
	end process;


    OBSERVE: if VIEW_ON generate
        constant N_BANKS_NEWEST: natural := 8;
        constant N_BANKS_STABLE: natural := 4;
        constant BANK_SIZE_NEWEST: natural := 32/N_BANKS_NEWEST;
        constant BANK_SIZE_STABLE: natural := 32/N_BANKS_STABLE;
        
        signal written: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        signal banksWritten: std_logic_vector(0 to N_BANKS_NEWEST-1) := (others => '0');
        signal banksWrittenMultiple: std_logic_vector(0 to N_BANKS_NEWEST-1) := (others => '0');        
        signal banksHist, banksHistC: IntArray(0 to N_BANKS_NEWEST-1) := (others => 0);
        signal nDestsWritten, nBanksWritten, nBanksWrittenMultiple: natural := 0;
        
        type AccessMatrix is array(0 to PIPE_WIDTH-1, 0 to N_BANKS_NEWEST-1) of natural;
        signal am, cam: AccessMatrix := (others => (others => 0));
    begin
        nDestsWritten <= countOnes(written);
        nBanksWritten <= countOnes(banksWritten);
        nBanksWrittenMultiple <= countOnes(banksWrittenMultiple);
        
        process (clk)
            variable iHi, iLo: natural := 0;
            variable tmpHist, tmpHistC: IntArray(0 to N_BANKS_NEWEST-1) := (others => 0);
            variable tmpAm, tmpCam: AccessMatrix := (others => (others => 0));
        begin
            if rising_edge(clk) then
                tmpHist := (others => 0);
                tmpAm := (others => (others => 0));
                
                banksWritten <= (others => '0');
                banksWrittenMultiple <= (others => '0');
                banksHist <= (others => 0);
                written <= (others => '0');
                am <= (others => (others => 0));
                --cam <= (others => (others => 0));
                        
                if sendingToReserve = '1' then
                    for i in 0 to PIPE_WIDTH-1 loop
                        iHi := slv2u(selectReserve(i))/BANK_SIZE_NEWEST;
                        iLo := slv2u(selectReserve(i)) mod BANK_SIZE_NEWEST;
                        if reserve(i) = '1' then
                            tmpAm(i, iHi) := tmpAm(i, iHi) + 1;
                        
                            tmpHist(iHi) := tmpHist(iHi) + 1;
                            --banksWritten(iHi) <= '1';
                        end if;
                    end loop;
                    am <= tmpAm;
                    banksHist <= tmpHist;
                    
                    for i in 0 to PIPE_WIDTH-1 loop
                        banksWritten(i) <= bool2std(tmpHist(i) > 0);
                        banksWrittenMultiple(i) <= bool2std(tmpHist(i) > 1);
                    end loop;
                    
                    written <= reserve;
                end if;
                
                -- TODO: clear on redirection
                if sendingToReserve = '1' then
                    for i in 0 to PIPE_WIDTH-1 loop
                        for j in 0 to N_BANKS_NEWEST-1 loop
                            if tmpCam(i, j) > 0 then                   
                            --    tmpCam(i, j) := tmpCam(i, j) - 1;
                            end if;
                            
                            --tmpCam(i, j) := tmpCam(i, j) + tmpAm(i, j);
                        end loop;
                    end loop;
                end if;
                
                tmpHistC := banksHistC;
                for i in 0 to PIPE_WIDTH-1 loop
                    for j in 0 to N_BANKS_NEWEST-1 loop
                        --tmpHistC(j) := tmpHistC(j) + tmpCam(i, j);
                    end loop;
                end loop;

                for j in 0 to N_BANKS_NEWEST-1 loop
                    if tmpHistC(j) > 0 then
                        tmpHistC(j) := tmpHistC(j) - 1;
                    end if;
                    
                    tmpHistC(j) := tmpHistC(j) + tmpHist(j);
                end loop;
                
                cam <= tmpCam;
                banksHistC <= tmpHistC;
                
            end if;
        end process;
    end generate;
    
end Behavioral;
