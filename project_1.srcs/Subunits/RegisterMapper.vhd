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
		--causingInstruction: in InstructionState; -- CAREFUL: not used now, mapping restored from stable
		
		sendingToReserve: in std_logic;
		stageDataToReserve: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		  reserveInfoA: in RenameInfoArray(0 to PIPE_WIDTH-1);
		newPhysDestsOrig: in PhysNameArray(0 to PIPE_WIDTH-1); -- to write to newest map

		sendingToCommit: in std_logic;
		stageDataToCommit: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		  commitInfoA: in RenameInfoArray(0 to PIPE_WIDTH-1);

		newPhysSources: out PhysNameArray(0 to 3*PIPE_WIDTH-1);		
		newPhysSources_NR: out PhysNameArray(0 to 3*PIPE_WIDTH-1);		
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

            signal reserve_C, commit_C, psels_C: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
            signal selectReserve_C, selectCommit_C: RegNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
            signal selectNewest_C: RegNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));

    signal writeCommit: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	signal readNewest, readNewest_NR, readNewest_T, readNewest_T2, readStableSources: PhysNameArray(0 to 3*PIPE_WIDTH-1) := (others => (others => '0'));
	signal readStable, readStable_T, readStable_T2, prevDests, prevDests_T, prevDests_T2: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    
    	signal readUseNewest: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
    
    signal newSelectedA, stableSelectedA: RegMaskArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    
    signal ch0, ch1,  cha, chb, chc, chd, che, chf: std_logic := '0';    
begin
            ch0 <= bool2std(readStable_T2 = readStable);
            ch1 <= bool2std(readNewest_T2 = readNewest_T);

    newSelectedA <= getSelectedA(selectReserve, reserve, IS_FP);
    stableSelectedA <= getSelectedA(selectCommit, commit, IS_FP);

    newestMapNext <= getNextMap(newestMap, stableMap, newPhysDestsOrig, reserve, newSelectedA, sendingToReserve, rewind);
    stableMapNext <= getNextMap(stableMap, stableMap, writeCommit, "1111", stableSelectedA, sendingToCommit, '0');

        newestMapNext_NoRewind <= getNextMap(newestMap_NoRewind, stableMap, newPhysDestsOrig, reserve, newSelectedA, sendingToReserve, '0');


    psels <= getPhysicalFloatDestSels(stageDataToCommit) when IS_FP else getPhysicalIntDestSels(stageDataToCommit);
        --psels_C <= getPhysicalFloatDestSels(stageDataToCommit) when IS_FP else getPhysicalIntDestSels(stageDataToCommit);

	reserve_C <= whichTakeReg(stageDataToReserve, IS_FP); 
    	reserve <= whichTakeReg(reserveInfoA, IS_FP); 

            cha <= bool2std(reserve = reserve_C);
            chb <= bool2std(selectReserve = selectReserve_C);
            chc <= bool2std(selectCommit = selectCommit_C);
            chd <= bool2std(selectNewest = selectNewest_C);

	commit <= psels;
	
	selectReserve_C <= getVirtualDests(stageDataToReserve);
    	selectReserve <= getVirtualDests(reserveInfoA);
	selectCommit <= getVirtualDests(stageDataToCommit);
    	selectCommit_C <= getVirtualDests(reserveInfoA);
	selectNewest_C <= getVirtualArgs(stageDataToReserve);
    	selectNewest <= getVirtualArgs(reserveInfoA);
	
	writeCommit <= getPhysicalDests(stageDataToCommit);	

	-- Read
	READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
		readNewest(i) <= newestMap(slv2u(selectNewest(i)));-- when not IS_FP
		readNewest_NR(i) <= newestMap_NoRewind(slv2u(selectNewest(i)));-- when not IS_FP
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
          
          prevDests <= readStable;
          prevDests_T2 <= readStable_T2;

	   end if;
	end process;

	prevStablePhysDests <= --readStable when IS_FP else 
	                       readStable_T2;

	newPhysSources <= --readNewest when IS_FP else
	                  readNewest_T2;
	                  
	newPhysSources_NR <= readNewest_NR;
        newPhysSourcesAlt <= readStableSources;
        newPhysSourceSelector <= not readUseNewest;

    TMP_VIEW: block
        signal tagReserve, tagCommit: InsTag := (others => '0');

            signal useNewest: std_logic_vector(0 to 31) := (others => '0');

        signal
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

            constant N_MAPPINGS: natural := 8 + 8 + 16;
    
            subtype PhysNameRow is PhysNameArray(0 to 3);
            type PhysNameTable is array(0 to N_MAPPINGS-1) of PhysNameRow;

            signal mappingTable, mappingTableStable: PhysNameTable := (others => (others => (others => '0')));
            signal mappingTablePtr, mappingTableStablePtr, mappingTablePtrNext, mappingTableStablePtrNext: natural range 0 to N_MAPPINGS-1 := 0;

                signal mapMem0, mapMem1, mapMem2, mapMem3, mapMemS1, mapMemS2, mapMemS3: PhysNameArray(0 to N_MAPPINGS-1) := (others => (others => '0'));
                signal mapMemS0: PhysNameArray(0 to N_MAPPINGS-1) := initMap(IS_FP);
                --signal mapMemS1, mapMemS2, mapMemS3: PhysNameArray(0 to N_MAPPINGS-1) := (others => (others => '0'));

            signal rowMap, rowMapStable: PhysNameArray(0 to 31) := (others => (others => '0'));

            signal rs0, rs1, rs2, rs3: RegSubmap := (others => (others => '0'));
 
            attribute ram_style: string;
            attribute ram_style of rs0, rs1, rs2, rs3: signal is "distributed";
                    
        signal newestMap_T, stableMap_T: RegMap := (others => (others => (others => '0')));
        signal newestSelMap, stableSelMap: RegSelectMap := (others => (others => '0'));
        
        signal newestSelMap2b, stableSelMap2b: RegSelectMap2b := (others => (others => '0'));
        
        signal compressedDests: PhysNameArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
        
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
            case sel(1 downto 0) is
                when "11" => reg := nm3;
                when "10" => reg := nm2;
                when "01" => reg := nm1;
                when others => reg := nm0;
            end case;
            
            return reg;
        end function;
    begin

	    READ_NEWEST: for i in 0 to 3*PIPE_WIDTH-1 generate
	       signal rowNum: natural := 0;
	       signal colNum: natural := 0;
	       signal index: natural := 0;
	           signal iv: std_logic_vector(2 downto 0) := "000";
	    begin
	                   iv(1 downto 0) <= newestSelMap2b(slv2u(selectNewest(i)))(1 downto 0);
	                   iv(2) <= useNewest(slv2u(selectNewest(i)));
	                   
	               rowNum <= slv2u(rowMap(slv2u(selectNewest(i))));
                   colNum <= slv2u(newestSelMap2b(slv2u(selectNewest(i))));
	               index <= slv2u(iv);
	    
            readNewest_T(i) <= --      newestMap_NoRewind(slv2u(selectNewest(i))) when useNewest(slv2u(selectNewest(i))) = '1' -- newestSelMap2b(slv2u(selectNewest(i)))(2) = '1'
                               -- else stableMap(slv2u(selectNewest(i)));
                               mappingTable(rowNum)(colNum) when useNewest(slv2u(selectNewest(i))) = '1'
                          else stableMap(slv2u(selectNewest(i)));
                     
--                 readNewest_T2(i) <= --stableMap(slv2u(selectNewest(i))) when useNewest(slv2u(selectNewest(i))) = '0'
--                            --else     
--                                     mapMem0(slv2u(selectNewest(i))) --rowNum)
--                                                     when colNum = 0
--                            else     mapMem1(slv2u(selectNewest(i))) --rowNum) 
--                                                     when colNum = 1
--                            else     mapMem2(slv2u(selectNewest(i))) --rowNum) 
--                                                     when colNum = 2
--                            else     mapMem3(slv2u(selectNewest(i))); --rowNum);

                 readNewest_T2(i) <= --stableMap(slv2u(selectNewest(i))) when useNewest(slv2u(selectNewest(i))) = '0'
                            --else     
                                     mapMem0(slv2u(selectNewest(i))) --rowNum)
                                                     when index = 4
                            else     mapMem1(slv2u(selectNewest(i))) --rowNum) 
                                                     when index = 5
                            else     mapMem2(slv2u(selectNewest(i))) --rowNum) 
                                                     when index = 6
                            else     mapMem3(slv2u(selectNewest(i))) --rowNum);
                                                     when index = 7
                            else     mapMemS0(slv2u(selectNewest(i))) --rowNum)
                                                     when index = 0
                            else     mapMemS1(slv2u(selectNewest(i))) --rowNum) 
                                                     when index = 1
                            else     mapMemS2(slv2u(selectNewest(i))) --rowNum) 
                                                     when index = 2
                            else     mapMemS3(slv2u(selectNewest(i))); --rowNum);
                                                                                                                          
                                
            readUseNewest(i) <= useNewest(slv2u(selectNewest(i)));
        end generate;

	    READ_STABLE: for i in 0 to PIPE_WIDTH-1 generate
	       signal rowNum: natural := 0;
	       signal colNum: natural := 0;
	       signal index: natural := 0;
	           signal iv: std_logic_vector(2 downto 0) := "000";
	    begin
                   index <= slv2u(stableSelMap2b(slv2u(selectCommit(i))));
	    
                 readStable_T2(i) <= 
                                       mapMemS0(slv2u(selectCommit(i))) --rowNum)
                                                       when index = 0
                              else     mapMemS1(slv2u(selectCommit(i))) --rowNum) 
                                                       when index = 1
                              else     mapMemS2(slv2u(selectCommit(i))) --rowNum) 
                                                       when index = 2
                              else     mapMemS3(slv2u(selectCommit(i))); --rowNum);	    
	    end generate;

            compressedDests <= assignDests(stageDataToReserve, newPhysDestsOrig, IS_FP); -- TODO: different for FP!


                mappingTablePtrNext <=    0 when mappingTablePtr = N_MAPPINGS-1 else     mappingTablePtr + 1;
                mappingTableStablePtrNext <=    0 when mappingTableStablePtr = N_MAPPINGS-1 else     mappingTableStablePtr + 1;
                                                                                                       
        process (clk)
            variable rsm: RegSelectMap := (others => (others => '0'));
        begin
            if rising_edge(clk) then
                if sendingToCommit = '1' then  
                    for i in 0 to 31 loop
                        if slv2u(rowMap(i)) = mappingTableStablePtrNext then
                          --  useNewest(i) <= '0';
                        end if; 
                    end loop;
                end if;
            
                if rewind = '1' then
                    newestSelMap2b <= stableSelMap2b;
                    --rowMap <= rowMapStable;
                    mappingTablePtr <= mappingTableStablePtrNext;
                    
                    useNewest <= (others => '0');
                elsif sendingToReserve = '1' then
                    if isNonzero(reserve) = '1' then
                        if mappingTablePtr = N_MAPPINGS-1 then
                            mappingTablePtr <= 0;
                        else
                            mappingTablePtr <= mappingTablePtr + 1;
                        end if;
                    end if;
                
                    for i in 0 to PIPE_WIDTH-1 loop
                        if reserve(i) = '1' then
                            useNewest(slv2u(selectReserve(i))) <= '1';                        
                            --newestSelMap(i)(slv2u(selectReserve(i))) <= '1';

                            newestSelMap2b(slv2u(selectReserve(i))) <= i2slv(i, 3);                            
                            
                            mappingTable(mappingTablePtrNext)(i) <= compressedDests(i);
                            rowMap(slv2u(selectReserve(i))) <= i2slv(mappingTablePtrNext, SMALL_NUMBER_SIZE);
                        end if;
                    end loop;
                        
                        if reserve(0) = '1' then
                            mapMem0(slv2u(selectReserve(0))) --mappingTablePtrNext)
                                                         <= compressedDests(0);
                        end if;
                        
                        if reserve(1) = '1' then
                            mapMem1(slv2u(selectReserve(1))) --mappingTablePtrNext) 
                                                         <= compressedDests(1);
                        end if;
                        
                        if reserve(2) = '1' then                          
                            mapMem2(slv2u(selectReserve(2))) --mappingTablePtrNext) 
                                                         <= compressedDests(2);
                        end if;
                        
                        if reserve(3) = '1' then
                            mapMem3(slv2u(selectReserve(3))) --mappingTablePtrNext) 
                                                         <= compressedDests(3);
                        end if;
                end if;
                
                if sendingToCommit = '1' then
                    if isNonzero(commit) = '1' then
                        mappingTableStablePtr <= mappingTableStablePtrNext;
                    end if;
                
                    for i in 0 to PIPE_WIDTH-1 loop
                        if commit(i) = '1' then
                            stableSelMap2b(slv2u(selectCommit(i))) <= i2slv(i, 3);                            
                            
                            mappingTableStable(mappingTableStablePtrNext)(i) <= writeCommit(i);
                            rowMapStable(slv2u(selectCommit(i))) <= i2slv(mappingTableStablePtrNext, SMALL_NUMBER_SIZE);
                        end if;
                    end loop;
                    
                    
                    if commit(0) = '1' then
                        mapMemS0(slv2u(selectCommit(0))) --mappingTablePtrNext)
                                                     <= writeCommit(0);
                    end if;
                    
                    if commit(1) = '1' then                                 
                        mapMemS1(slv2u(selectCommit(1))) --mappingTablePtrNext) 
                                                     <= writeCommit(1);
                    end if;
                    
                    if commit(2) = '1' then
                        mapMemS2(slv2u(selectCommit(2))) --mappingTablePtrNext) 
                                                     <= writeCommit(2);
                    end if;
                    
                    if commit(3) = '1' then
                        mapMemS3(slv2u(selectCommit(3))) --mappingTablePtrNext) 
                                                     <= writeCommit(3);
                    end if;               
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
