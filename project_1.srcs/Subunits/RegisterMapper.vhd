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
		newPhysDestsOrig: in PhysNameArray(0 to PIPE_WIDTH-1); -- to write to newest map

		sendingToCommit: in std_logic;
		stageDataToCommit: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		
		newPhysSources: out PhysNameArray(0 to 3*PIPE_WIDTH-1);		
		newPhysSourcesAlt: out PhysNameArray(0 to 3*PIPE_WIDTH-1);
		newPhysSourceSelector: out std_logic_vector(0 to 3*PIPE_WIDTH-1);		
		prevStablePhysDests: out PhysNameArray(0 to PIPE_WIDTH-1)
	);
end RegisterMapper;



architecture Behavioral of RegisterMapper is
	constant WIDTH: natural := PIPE_WIDTH;
	
	signal newestMap, newestMap_NoRewind, stableMap, newestMapNext, newestMapNext_NoRewind, stableMapNext: PhysNameArray(0 to 31) := initMap(IS_FP);
    
    signal reserve, commit, psels: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    signal selectReserve, selectCommit: RegNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal selectNewest: RegNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal writeCommit: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	signal readNewest, readNewest_T, readStableSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
	signal readStable: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    
    	signal readUseNewest: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
    
    signal newSelectedA, stableSelectedA: RegMaskArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    
    signal ch0, ch1: std_logic := '0';    
begin
    newSelectedA <= getSelectedA(selectReserve, reserve, IS_FP);
    stableSelectedA <= getSelectedA(selectCommit, commit, IS_FP);

    newestMapNext <= getNextMap(newestMap, stableMap, newPhysDestsOrig, reserve, newSelectedA, sendingToReserve, rewind);
    stableMapNext <= getNextMap(stableMap, stableMap, writeCommit, "1111", stableSelectedA, sendingToCommit, '0');

        newestMapNext_NoRewind <= getNextMap(newestMap_NoRewind, stableMap, newPhysDestsOrig, reserve, newSelectedA, sendingToReserve, '0');


    psels <= getPhysicalFloatDestSels(stageDataToCommit) when IS_FP else getPhysicalIntDestSels(stageDataToCommit);

	reserve <= whichTakeReg(stageDataToReserve, IS_FP); 
	commit <= psels;
	
	selectReserve <= getVirtualDests(stageDataToReserve);
	selectCommit <= getVirtualDests(stageDataToCommit);
	selectNewest <= getVirtualArgs(stageDataToReserve);
	
	writeCommit <= getPhysicalDests(stageDataToCommit);	

	-- Read
	READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
		readNewest(i) <= newestMap(slv2u(selectNewest(i)));-- when not IS_FP
		                 --  readNewest_T(i);
		          
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
	      prevStablePhysDests <= readStable;
	   end if;
	end process;

	newPhysSources <= readNewest;
        newPhysSourcesAlt <= readStableSources;
        newPhysSourceSelector <= readUseNewest;

    TMP_VIEW: block
        signal tagReserve, tagCommit: InsTag := (others => '0');

            signal useNewest: std_logic_vector(0 to 31) := (others => '0');

        signal --reserve, commit, psels,
    
            reserve_T,
            fullMaskReserve, fullMaskCommit,
             excMaskReserve, excMaskCommit, causingMaskReserve, causingMaskCommit, killedMaskReserve, killedMaskCommit,
             ignoredMaskReserve, ignoredMaskCommit,
             igVselReserve, igVselCommit, kiVselReserve, kiVselCommit,
            vselReserve, vselCommit, pselReserve, pselCommit: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        
        subtype RegSubmap is PhysNameArray(0 to 31);-- := (others => (others => '0'));
        type RegMap is array(0 to PIPE_WIDTH-1) of RegSubmap;
        
        type RegSelectMap is array(0 to PIPE_WIDTH-1) of std_logic_vector(0 to 31);
        type RegSelectMap2b is array(0 to 31) of std_logic_vector(2 downto 0);

            signal rs0, rs1, rs2, rs3: RegSubmap := (others => (others => '0'));
 
            attribute ram_style: string;
            attribute ram_style of rs0, rs1, rs2, rs3: signal is "distributed";
                    
        signal newestMap_T, stableMap_T: RegMap := (others => (others => (others => '0')));
        signal newestSelMap, stableSelMap: RegSelectMap := (others => (others => '0'));
        
        signal newestSelMap2b: RegSelectMap2b := (others => (others => '0'));
        
        function readRegMap(nm0, nm1,nm2, nm3: RegSubmap; stableMapOld: PhysNameArray; selMap: RegSelectMap; ind: natural) return PhysName is
            variable reg: SmallNumber := stableMapOld(ind);
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                if selMap(i)(ind) = '1' then
                    if i = 0 then
                        reg := nm0(ind);                    
                    elsif i = 1 then
                        reg := nm1(ind);                    
                    elsif i = 2 then
                        reg := nm2(ind);                    
                    else
                        reg := nm3(ind);
                    end if;
                end if;
            end loop;
            
            return reg;
        end function;
        
        function readRegMap2(nm0, nm1,nm2, nm3: PhysName; stableMapOld: PhysName; selMap: RegSelectMap; ind: natural) return PhysName is
            variable reg: SmallNumber := stableMapOld;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                if selMap(i)(ind) = '1' then
                    if i = 0 then
                        reg := nm0;                    
                    elsif i = 1 then
                        reg := nm1;                    
                    elsif i = 2 then
                        reg := nm2;                    
                    else
                        reg := nm3;
                    end if;
                end if;
            end loop;
            
            return reg;
        end function;
        
        function readRegMap3(nm0, nm1,nm2, nm3: PhysName; stableMapOld: PhysName; sel: std_logic_vector; ind: natural) return PhysName is
            variable reg: SmallNumber := stableMapOld;
        begin
            --if sel(2) = '1' then
            --    reg := stableMapOld;
            --else
                case sel(1 downto 0) is
                    when "11" => reg := nm3;
                    when "10" => reg := nm2;
                    when "01" => reg := nm1;
                    when others => reg := nm0;
                end case;
            --end if;
            
            return reg;
        end function;
    begin
        --   reserve_T <= whichTakeReg_T(stageDataToReserve, IS_FP);
	   
	    
	    READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
            --readNewest(i) <= readRegMap(newestMap_T, stableMap_T, stableMap, newestSelMap, slv2u(selectNewest(i)));
            --readNewest(i) <= readRegMap(rs0, rs1, rs2, rs3, stableMap, newestSelMap, slv2u(selectNewest(i)));
            readNewest_T(i) <= 
--                             readRegMap2(rs0(slv2u(selectNewest(i))),
--                                         rs1(slv2u(selectNewest(i))),
--                                         rs2(slv2u(selectNewest(i))),
--                                         rs3(slv2u(selectNewest(i))),
--                                         stableMap(slv2u(selectNewest(i))),
--                                         newestSelMap,
--                                         slv2u(selectNewest(i)));
                                         
--                              readRegMap3(rs0(slv2u(selectNewest(i))),
--                                          rs1(slv2u(selectNewest(i))),
--                                          rs2(slv2u(selectNewest(i))),
--                                          rs3(slv2u(selectNewest(i))),
--                                          stableMap(slv2u(selectNewest(i))),
--                                          newestSelMap2b(slv2u(selectNewest(i))),
--                                          slv2u(selectNewest(i)));
                                     newestMap_NoRewind(slv2u(selectNewest(i))) when useNewest(slv2u(selectNewest(i))) = '1' -- newestSelMap2b(slv2u(selectNewest(i)))(2) = '1'
                                else stableMap(slv2u(selectNewest(i)));
                                
            readUseNewest(i) <= useNewest(slv2u(selectNewest(i)));
        end generate;

        WRITE_NEWEST: for i in 0 to PIPE_WIDTH-1 generate
                
        end generate;
        
        
        process (clk)
            variable rsm: RegSelectMap := (others => (others => '0'));
        begin
            if rising_edge(clk) then
                if rewind = '1' then
                    newestSelMap <= (others => (others => '0'));
                    newestSelMap2b <= (others => "100");
                    
                    useNewest <= (others => '0');
                elsif sendingToReserve = '1' then
                    for i in 0 to PIPE_WIDTH-1 loop
                        if reserve(i) = '1' then
                                useNewest(slv2u(selectReserve(i))) <= '1';
                        
                            newestMap_T(i)(slv2u(selectReserve(i))) <= newPhysDestsOrig(i);
                            newestSelMap(i)(slv2u(selectReserve(i))) <= '0';
                            
                                newestSelMap2b(slv2u(selectReserve(i))) <= (others => '0');
                            
                            if i = 0 then
                                rs0(slv2u(selectReserve(i))) <= newPhysDestsOrig(i);
                            elsif i = 1 then
                                rs1(slv2u(selectReserve(i))) <= newPhysDestsOrig(i);
                            elsif i = 2 then
                                rs2(slv2u(selectReserve(i))) <= newPhysDestsOrig(i);
                            else
                                rs3(slv2u(selectReserve(i))) <= newPhysDestsOrig(i);
                            end if;
                        end if;
                    end loop;
                    
                    for i in 0 to PIPE_WIDTH-1 loop
                        if reserve(i) = '1' then
                            newestSelMap(i)(slv2u(selectReserve(i))) <= '1';
                            rsm(i)(slv2u(selectReserve(i))) := '1';
                        end if;
                    end loop;
                    
                    for i in 0 to 31 loop
--                        if rsm(3)(i) = '1' then
--                            newestSelMap2b(i) <= "011";
--                        elsif rsm(2)(i) = '1' then
--                            newestSelMap2b(i) <= "010";
--                        elsif rsm(1)(i) = '1' then
--                            newestSelMap2b(i) <= "001";                        
--                        else
--                            newestSelMap2b(i) <= "000";
--                        end if;
                    end loop;
                end if;
            end if;
        end process;
        
        
        
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
    end block;

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
