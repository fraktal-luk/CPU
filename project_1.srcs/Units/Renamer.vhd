
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

use work.AbstractRenaming.all;


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
    signal evtD, evtD2: EventState := DEFAULT_EVENT_STATE;
    signal newTags, newTagsNext: SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));
    
    signal tagTable: SmallNumberArray(0 to 31) := (others => sn(0));
    signal virtualDests: RegNameArray(0 to RENAME_W-1) := (others => (others => '0'));

    signal virtualSrcs: RegNameArray(0 to 3*RENAME_W-1) := (others => (others => '0'));
    signal destMask: std_logic_vector(0 to RENAME_W-1) := (others => '0');

    signal depVecBasic, depVec: DependencyVec;

    signal baseSrcs, finalSrcs, finalSrcsReg: SmallNumberArray(0 to 3*RENAME_W-1) := (others => sn(0));

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

    TMP_destsOut <= newTags;
    TMP_sourcesOut <= finalSrcsReg;
        
    process (clk)
    begin
        if rising_edge(clk) then
            evtD <= evt;
            evtD2 <= evtD;

            newTags <= newTagsNext;
            finalSrcsReg <= finalSrcs;

            if prevSending = '1' then
                for i in 0 to RENAME_W-1 loop
                    if destMask(i) = '1' then
                        tagTable(slv2u(virtualDests(i))) <= newTagsNext(i);
                    end if;
                end loop;
            end if;
         end if;
     end process;


    -- Abstract mapping
    ABSTRACT_MAPPER: block
        signal abstractLatestTable, abstractPersistentTable: PhysNameArray(0 to 31) := initMap(false);
        signal abstractChangedTable, abstractFlushedTable: PhysNameArray(0 to 31) := (others => nothing);


        signal abstractMapList: MapList := (others => DEFAULT_MAP_ROW);
        -- Same as in ROB
        signal startPtr, startPtrNext, endPtr, endPtrNext, renamedPtr, renamedPtrNext, causingPtr: SmallNumber := (others => '0');    
    
        signal newMapRow, committingMapRow: MapRow := DEFAULT_MAP_ROW;
        signal causingMapping, lastUsedMapping: AbstractMapping := DEFAULT_ABSTRACT_MAPPING;
    

        signal physRegFreeList: PhysNameArray(0 to FREE_LIST_SIZE-1) := initFreeList(false);
        signal listPtrTake, listPtrTakeStable, listPtrTakeStableNext, listCausingTag: natural := 0;
        signal listPtrPut, listPtrPutNext: natural := (N_PHYS - 32);
        signal newFreeDests, releasedDests: PhysNameArray(0 to RENAME_W-1) := (others => (others => '0'));
    
        -- check status of very physical reg: zero/free/speculative/persistent
        -- zero iff its number is 0
        -- free iff in freeList
        -- speculative iff in abstractMapper temporary
        -- persistent iff in stableMap
    
        signal physRegStateTable: std_logic_vector(0 to N_PHYS-1) := (others => '0'); -- TODO: change to array of PhysRegState (create this type first)
        signal mapCounts, freeListCounts, stableCounts: IntArray(0 to N_PHYS-1) := (others => 0);
    begin
    
        mapCounts <= scanMappings(abstractMapList, evt.execCausing.tag, startPtr, endPtr);
        freeListCounts <= scanFreeList(physRegFreeList, evt.execCausing.tag, listPtrTake, listPtrPut);
        stableCounts <= scanMappingTable(abstractPersistentTable);
    
        process (clk)
            variable lastUsedMappingVar: AbstractMapping;
            variable releasingDestsVar: PhysNameArray(0 to RENAME_W-1) := (others => nothing);
            variable stableProxyVar, changedMapVar, flushedMapVar: PhysNameArray(0 to 31) := (others => nothing);
            variable listPtrTakeStableVar: natural := 0;
            variable nReleased: natural:= 9;
        begin
            if rising_edge(clk) then
                releasingDestsVar := (others => nothing);
                stableProxyVar := (others => nothing);
                listPtrTakeStableVar := 0;
                nReleased := 0;
    
                if prevSending = '1' then
                    abstractMapList(p2i(renamedPtr, ROB_SIZE)) <= newMapRow;
                end if;
    
                if robSending = '1' then
                    abstractMapList(p2i(startPtr, ROB_SIZE)) <= DEFAULT_MAP_ROW;
                end if;
                
                startPtr <= startPtrNext;
                endPtr <= endPtrNext;
                renamedPtr <= renamedPtrNext;
    
                if prevSending = '1' then
                    for i in 0 to RENAME_W-1 loop
                        if newMapRow.mappings(i).used = '1' then
                            abstractLatestTable(slv2u(virtualDests(i))) <= newMapRow.mappings(i).physical;
                        end if;
                    end loop;
                end if;
    
                stableProxyVar := abstractPersistentTable;
                if robSending = '1' then            
                    for i in 0 to RENAME_W-1 loop
                        if committingMapRow.mappings(i).used = '1' then
                            releasingDestsVar(i) := stableProxyVar(slv2u(committingMapRow.mappings(i).virtual));
                            stableProxyVar(slv2u(committingMapRow.mappings(i).virtual)) := committingMapRow.mappings(i).physical;
                        end if;
                    end loop;
    
                    abstractPersistentTable <= stableProxyVar;
                    releasedDests <= releasingDestsVar;
                end if;
                
                if prevSending = '1' then
                    listPtrTake <= (listPtrTake + getNumUsed(newMapRow)) mod FREE_LIST_SIZE;
                end if;
                
                listPtrTakeStableVar := listPtrTakeStable;
                if robSending = '1' then
                    listPtrTakeStable <= (listPtrTakeStable + getNumUsed(committingMapRow)) mod FREE_LIST_SIZE;
                    listPtrTakeStableVar := (listPtrTakeStable + getNumUsed(committingMapRow)) mod FREE_LIST_SIZE;
                end if;
    
                if evt.lateEvent = '1' then -- Flush to persistent state (persistent map can't be updated when lateEvent is signalled - Commit is locked)
                    listPtrTake <= listPtrTakeStableVar;
                elsif evt.execEvent = '1' then -- Partial flush (persistent map can be updated in this cycle and directly following ones!)
                    -- Rewind free list ptr to 1 after last active mapping. If none exists, use committed state
                    lastUsedMappingVar := findLastMappingByTag(abstractMapList, evt.execCausing.tag, startPtr);
                    if lastUsedMappingVar.used = '1' then
                        listPtrTake <= (lastUsedMappingVar.freeListIndex + 1) mod FREE_LIST_SIZE;
                    else
                        listPtrTake <= listPtrTakeStableVar;
                    end if;
    
                    lastUsedMapping <= lastUsedMappingVar;
                end if;
    
                -- Write released mappings to free list
                for i in 0 to RENAME_W-1 loop
                    if releasingDestsVar(i) /= nothing then
                        physRegFreeList((listPtrPut + nReleased) mod FREE_LIST_SIZE) <= releasingDestsVar(i);
                        nReleased := nReleased + 1;
                    end if;
                    listPtrPut <= (listPtrPut + nReleased) mod FREE_LIST_SIZE;
                end loop;
                
                if evt.lateEvent = '1' then
                    abstractMapList <= (others => DEFAULT_MAP_ROW);
    
                    --abstractChangedTable <= scanChangedMappings(abstractMapList, evtD.execCausing.tag, startPtr, endPtr);
                    --abstractFlushedTable <= scanFlushedMappings(abstractMapList, evtD.execCausing.tag, startPtr, endPtr);
                    
                    abstractLatestTable <= stableProxyVar;
                elsif evt.execEvent = '1' then
                    abstractMapList <= clearAbstractMappingsPartial(abstractMapList, evt.execCausing.tag, startPtr, endPtr);
    
                    abstractChangedTable <= scanChangedMappings(abstractMapList, evt.execCausing.tag, startPtr, endPtr);
                    changedMapVar := scanChangedMappings(abstractMapList, evt.execCausing.tag, startPtr, endPtr);
                    
                    abstractFlushedTable <= scanFlushedMappings(abstractMapList, evt.execCausing.tag, startPtr, endPtr);
                    flushedMapVar := scanFlushedMappings(abstractMapList, evt.execCausing.tag, startPtr, endPtr);
                    
                    abstractLatestTable <= applyChangedMappings(stableProxyVar, changedMapVar);    
                end if;
                
            end if;
            
        end process;
    
        causingMapping <= findMappingByTag(abstractMapList, evt.execCausing.tag);
    
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
    
        newFreeDests <= getFreeRegs(physRegFreeList, listPtrTake);
        newMapRow <= makeMapRow(frontData, newFreeDests, listPtrTake);
    
        committingMapRow <= abstractMapList(p2i(startPtr, ROB_SIZE));
    end block;

end Behavioral;

