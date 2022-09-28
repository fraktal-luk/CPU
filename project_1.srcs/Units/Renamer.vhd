
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicRenaming.all;

use work.LogicROB.all;


entity Renamer is
    port (
        clk: in std_logic;
        evt: in EventState;
        
        prevSending: in std_logic;
        
        frontData: in BufferEntryArray;
        
        maskAlu: in std_logic_vector(0 to RENAME_W-1);
        maskMul: in std_logic_vector(0 to RENAME_W-1);
        maskMem: in std_logic_vector(0 to RENAME_W-1);
        
        TMP_tagsAlu: in SmallNumberArray(0 to RENAME_W-1);
        TMP_tagsMul: in SmallNumberArray(0 to RENAME_W-1);
        TMP_tagsMem: in SmallNumberArray(0 to RENAME_W-1);

        TMP_destsOut: out SmallNumberArray(0 to RENAME_W-1);
        TMP_sourcesOut: out SmallNumberArray(0 to 3*RENAME_W-1);

            renameSending: in std_logic;
            robSending: in std_logic;

        dummy: out std_logic
    );
end Renamer;


architecture Behavioral of Renamer is
    signal newTags, newTagsNext: SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));
    
    signal tagTable: SmallNumberArray(0 to 31) := (others => sn(0));
    signal virtualDests: RegNameArray(0 to RENAME_W-1) := (others => (others => '0'));

    signal virtualSrcs: RegNameArray(0 to 3*RENAME_W-1) := (others => (others => '0'));
    signal destMask: std_logic_vector(0 to RENAME_W-1) := (others => '0');
    
    signal depVecBasic, depVec: DependencyVec;
    
    signal baseSrcs, finalSrcs, finalSrcsReg: SmallNumberArray(0 to 3*RENAME_W-1) := (others => sn(0));

    
    type AbstractMapping is record
        used: std_logic;
        virtual: RegName;
        assigned: std_logic;
        physical: PhysName;
    end record;
    
    type AbstractMappingGroup is array(0 to RENAME_W-1) of AbstractMapping;

    type MapRow is record
        used: std_logic;
        tag: InsTag;
        mappings: AbstractMappingGroup;
    end record;

    type MapList is array(0 to ROB_SIZE-1) of MapRow;

    signal abstractMapList: MapList;
    signal listWritePtr: integer := -1;
        
       -- Same as in ROB
	   signal startPtr, startPtrNext, endPtr, endPtrNext, renamedPtr, renamedPtrNext, causingPtr: SmallNumber := (others => '0');	

    signal newFreeDests: PhysNameArray(0 to RENAME_W-1) := (others => (others => '0'));


    function makeMapRow(frontData: BufferEntryArray; newPhysDests: PhysNameArray) return MapRow is
        variable res: MapRow;
    begin
        res.used := '0';
        res.tag := (others => 'Z');
        for i in 0 to RENAME_W-1 loop
            res.mappings(i).used := frontData(i).argSpec.intDestSel;
            res.mappings(i).virtual := frontData(i).argSpec.dest(4 downto 0);
            res.mappings(i).assigned := '1';
            res.mappings(i).physical := newPhysDests(i);
            
            res.used := res.used or res.mappings(i).used;          
        end loop;
        return res;
    end function;

    signal newMapRow: MapRow;

    function findFreeRow(list: MapList) return integer is
    begin
        for i in list'range loop
            if list(i).used /= '1' then
                return i;
            end if;
        end loop;
        return -1;
    end function;


    function TMP_replaceNewDests(baseSrcs: SmallNumberArray; depVec: DependencyVec; newDests: SmallNumberArray) return SmallNumberArray is
        variable res: SmallNumberArray(baseSrcs'range) := baseSrcs;
    begin
        
        for i in 0 to RENAME_W-1 loop
            for j in 0 to 2 loop
                --res(i).sourcesNew(j) := isNonzero(res(i).deps(j));
                for k in RENAME_W-1 downto 0 loop
                    if depVec(i)(j)(k) = '1' then
                        res(3*i + j) := newDests(k);
                        exit;
                    end if;
                end loop;
            end loop;  
        end loop;

        return res;
    end function;
begin

    depVecBasic <= findDeps(frontData);
    depVec <= getRealDepVecInt(frontData, depVecBasic);

    MUX_DESTS: for i in 0 to RENAME_W-1 generate
        constant LANE_SPEC: SmallNumber := sn(4*i);
    begin
        destMask(i) <= frontData(i).argSpec.intDestSel;
        virtualDests(i) <= frontData(i).argSpec.dest(4 downto 0);

        newTagsNext(i) <= TMP_tagsAlu(i) or LANE_SPEC or X"00" when maskAlu(i) = '1'
                     else TMP_tagsMul(i) or LANE_SPEC or X"10" when maskMul(i) = '1'
                     else TMP_tagsMem(i) or LANE_SPEC or X"20" when maskMem(i) = '1'
                     else X"30";
    end generate;

    virtualSrcs <= getVirtualArgs(frontData);

    MUX_SRCS: for i in 0 to 3*RENAME_W-1 generate
        baseSrcs(i) <= tagTable(slv2u(virtualSrcs(i)));
    end generate;

    finalSrcs <= TMP_replaceNewDests(baseSrcs, depVec, newTagsNext);

    process (clk)
    begin
        if rising_edge(clk) then
            newTags <= newTagsNext;
            finalSrcsReg <= finalSrcs;

            if prevSending = '1' then
                for i in 0 to RENAME_W-1 loop
                    if destMask(i) = '1' then
                        tagTable(slv2u(virtualDests(i))) <= newTagsNext(i);
                    end if;
                end loop;
            end if;
            
            --------------------------------------------
            -- Abstract map for reference
            --------------------------------------------
            --if prevSending = '1' and newMapRow.used = '1' then
            if prevSending = '1' then
                abstractMapList(p2i(renamedPtrNext, ROB_SIZE)) <= newMapRow;
            end if;

            if robSending = '1' then
                abstractMapList(p2i(startPtr, ROB_SIZE)).used <= '0';
            end if;
            
            startPtr <= startPtrNext;
            endPtr <= endPtrNext;
            renamedPtr <= renamedPtrNext;
            
        end if;
    end process;

    	causingPtr <= getTagHighSN(evt.execCausing.tag) and PTR_MASK_SN_LONG;


        startPtrNext <= addIntTrunc(startPtr, 1, ROB_PTR_SIZE+1) when robSending = '1'
                   else startPtr;

        endPtrNext <= startPtrNext when evt.lateEvent = '1'
                    else  addIntTrunc(causingPtr, 1, ROB_PTR_SIZE+1) when evt.execEvent = '1'
                    else  addIntTrunc(endPtr, 1, ROB_PTR_SIZE+1) when prevSending = '1'
                    else  endPtr;

        renamedPtrNext <= startPtrNext when evt.lateEvent = '1'
                    else  addIntTrunc(causingPtr, 1, ROB_PTR_SIZE+1) when evt.execEvent = '1'
                    else  addIntTrunc(renamedPtr, 1, ROB_PTR_SIZE+1) when prevSending = '1'
                    else  renamedPtr;


        newMapRow <= makeMapRow(frontData, newFreeDests);
        listWritePtr <= findFreeRow(abstractMapList);




    TMP_destsOut <= newTags;
    TMP_sourcesOut <= finalSrcsReg;

end Behavioral;

