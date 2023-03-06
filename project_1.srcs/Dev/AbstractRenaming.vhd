
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicRenaming.all;

package AbstractRenaming is

    constant nothing: PhysName := (others => '-');

    
    type AbstractMapping is record
        used: std_logic;
        virtual: RegName;
        assigned: std_logic;
        physical: PhysName;
        freeListIndex: integer;
    end record;
    
    constant DEFAULT_ABSTRACT_MAPPING: AbstractMapping := (
        used => '0',
        virtual => (others => 'U'),
        assigned => '0',
        physical => nothing,
        freeListIndex => -1
    );
    
    type AbstractMappingGroup is array(0 to RENAME_W-1) of AbstractMapping;

    type MapRow is record
        used: std_logic;
        tag: InsTag;
        mappings: AbstractMappingGroup;
    end record;

    constant DEFAULT_MAP_ROW: MapRow := (
        used => '0',
        tag => (others => 'U'),
        mappings => (others => DEFAULT_ABSTRACT_MAPPING)
    );

    type MapList is array(0 to ROB_SIZE-1) of MapRow;

    function makeMapRow(frontData: BufferEntryArray; newPhysDests: PhysNameArray; freeListIndex: natural) return MapRow;

    function getNumUsed(row: MapRow) return natural;


    function findFreeRow(list: MapList) return integer;


    function TMP_replaceNewDests(baseSrcs: SmallNumberArray; depVec: DependencyVec; newDests: SmallNumberArray) return SmallNumberArray;
    
    function getFreeRegs(list: PhysNameArray; ptr: natural) return PhysNameArray;
    
    function findMappingByTag(list: MapList; tag: InsTag) return AbstractMapping;
    
    -- latest used mapping in [startTag, tag] range 
    function findLastMappingByTag(list: MapList; tag: InsTag; startPtr: SmallNumber) return AbstractMapping;
    
    -- Those between persistent and redirecting op 
    function scanChangedMappings(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return PhysNameArray;

    -- Those after redirecting op
    function scanFlushedMappings(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return PhysNameArray;

    -- check if a tag is in valid OOO window range (tags wrap around after 2 ROB generations)
    function tagInRange(tag, startTag, endTag: InsTag) return std_logic;
    
    -- check if pointer in range (indices wrap around after 1 buffer generation)
    function indexInRange(ind, startInd, endInd: SmallNumber) return std_logic;

    function clearAbstractMappingsPartial(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return MapList;

    function scanMappings(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return IntArray;
    function scanFreeList(freeList: PhysNameArray; tag: InsTag; startPtr, endPtr: natural) return IntArray;
    function scanMappingTable(table: PhysNameArray; skipR0: boolean := true) return IntArray;

    function applyChangedMappings(table, changed: PhysNameArray) return PhysNameArray;

end package;


package body AbstractRenaming is

    function makeMapRow(frontData: BufferEntryArray; newPhysDests: PhysNameArray; freeListIndex: natural) return MapRow is
        variable res: MapRow;
        variable j: natural := 0;
    begin
        res.used := '0';
        res.tag := (others => 'Z');
        for i in 0 to RENAME_W-1 loop
            res.mappings(i).used := frontData(i).argSpec.intDestSel;
            res.mappings(i).virtual := frontData(i).argSpec.dest(4 downto 0);
            if frontData(i).argSpec.intDestSel = '1' then
                res.mappings(i).assigned := '1';
                res.mappings(i).physical := newPhysDests(j);
                res.mappings(i).freeListIndex := (freeListIndex + j) mod FREE_LIST_SIZE;
                j := j + 1;
            else
                res.mappings(i).assigned := '0';
                res.mappings(i).physical := (others => '-');
                res.mappings(i).freeListIndex := -1;
            end if;
            
            res.used := res.used or res.mappings(i).used;          
        end loop;
        return res;
    end function;

    function getNumUsed(row: MapRow) return natural is
        variable res: natural := 0;
    begin
        for i in 0 to RENAME_W-1 loop
            if row.mappings(i).used = '1' then
                res := res + 1;
            end if;
        end loop;
        return res;
    end function;


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
    
    function getFreeRegs(list: PhysNameArray; ptr: natural) return PhysNameArray is
        variable res: PhysNameArray(0 to RENAME_W-1) := (others => (others => '0'));
    begin
        for i in 0 to RENAME_W-1 loop
            res(i) := list((ptr+i) mod FREE_LIST_SIZE);
        end loop;
        return res;
    end function;
    
    function findMappingByTag(list: MapList; tag: InsTag) return AbstractMapping is
        --constant tagHigh: InsTagHighPart := getTagHigh(tag);
        constant tagHighSN: SmallNumber := getTagHighSN(tag);
        constant tagLow: InsTagLowPart := getTagLow(tag);
        variable res: AbstractMapping;
    begin
        res := list(p2i(tagHighSN, ROB_SIZE)).mappings(slv2u(tagLow));

        return res;
    end function;
    
    -- latest used mapping in [startTag, tag] range 
    function findLastMappingByTag(list: MapList; tag: InsTag; startPtr: SmallNumber) return AbstractMapping is
        --constant tagHigh: InsTagHighPart := getTagHigh(tag);
        constant tagHighSN: SmallNumber := getTagHighSN(tag);
        constant tagLow: InsTagLowPart := getTagLow(tag);
        variable res: AbstractMapping;
        variable ptr: natural := p2i(tagHighSN, ROB_SIZE);
        constant start: natural := p2i(startPtr, ROB_SIZE); 
        variable subindex: natural := slv2u(tagLow);
        variable found: boolean := false;
    begin
        loop
            loop
                res := list(ptr).mappings(subindex);

                if res.used = '1' then 
                    found := true;
                    exit;
                end if;
                
                if subindex = 0 then
                    exit;
                end if;
                subindex := subindex-1;
            end loop;
            
            if found then
                exit;
            end if;
            
            if ptr = start then
                exit;
            end if;
            
            if ptr = 0 then
                ptr := ROB_SIZE-1;
            else
                ptr := ptr - 1;
            end if;
            
            subindex := RENAME_W-1;
        end loop;
        
        return res;
    end function;
    
    -- Those between persistent and redirecting op 
    function scanChangedMappings(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return PhysNameArray is
        variable res: PhysNameArray(0 to 31) := (others => (others => '-'));
        variable mapping: AbstractMapping;
        constant tagHighSN: SmallNumber := getTagHighSN(tag);
        constant tagLow: InsTagLowPart := getTagLow(tag);
        variable ptr: natural := p2i(tagHighSN, ROB_SIZE);
        constant start: natural := p2i(startPtr, ROB_SIZE); 
        variable subindex: natural := slv2u(tagLow);
        variable found: boolean := false;
    begin
        -- nothing to see?
        if startPtr = endPtr then
            return res;
        end if;
        
        loop
            loop
                mapping := list(ptr).mappings(subindex);

                if mapping.used = '1' then 
                    if res(slv2u(mapping.virtual)) = nothing then
                        res(slv2u(mapping.virtual)) := mapping.physical;
                    end if;
                end if;
                
                if subindex = 0 then
                    exit;
                end if;
                subindex := subindex-1;
            end loop;
            
            if ptr = start then
                exit;
            end if;
            
            if ptr = 0 then
                ptr := ROB_SIZE-1;
            else
                ptr := ptr - 1;
            end if;
            
            subindex := RENAME_W-1;
        end loop;        
        
        return res;
    end function;

    -- Those after redirecting op
    function scanFlushedMappings(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return PhysNameArray is
        variable res: PhysNameArray(0 to 31) := (others => (others => '-'));
        variable mapping: AbstractMapping;
        constant tagHighSN: SmallNumber := getTagHighSN(tag);
        constant tagLow: InsTagLowPart := getTagLow(tag);
        variable ptr: natural := p2i(tagHighSN, ROB_SIZE);
        constant start: natural := p2i(startPtr, ROB_SIZE); 
        constant pEnd: natural := p2i(endPtr, ROB_SIZE); 
        variable subindex: natural := slv2u(tagLow);
        variable found: boolean := false;
    begin
        -- nothing to see?
        if startPtr = endPtr then
            return res;
        end if;
        
        -- if causing is last in row, go to next row
        if subindex = RENAME_W-1 then
            ptr := (ptr + 1) mod ROB_SIZE;
            subindex := 0;
        else
            subindex := subindex + 1;
        end if;
 
        loop
            if ptr = pEnd then
                exit;
            end if;

            loop
                mapping := list(ptr).mappings(subindex);
    
                if mapping.used = '1' then 
                    --if res(slv2u(mapping.virtual)) = nothing then
                        res(slv2u(mapping.virtual)) := mapping.physical;
                    --end if;
                end if;
                
                if subindex = RENAME_W-1 then
                    exit;
                else
                    subindex := subindex + 1;
                end if;
            end loop;
            
            if ptr = ROB_SIZE-1 then
                ptr := 0;
            else
                ptr := (ptr + 1) mod ROB_SIZE;
            end if;
            
            subindex := 0;
        end loop;
        
        return res;
    end function;

    -- check if a tag is in valid OOO window range (tags wrap around after 2 ROB generations)
    function tagInRange(tag, startTag, endTag: InsTag) return std_logic is
    begin
        return compareTagBefore(tag, endTag) and not compareTagBefore(tag, startTag);
    end function;
    
    -- check if pointer in range (indices wrap around after 1 buffer generation)
    function indexInRange(ind, startInd, endInd: SmallNumber) return std_logic is
    begin
        -- TODO
        -- (startInd <= ind and ind < endInd) OR (endInd <= startInd and startInd <= ind) OR (ind < endInd and endInd <= startInd)
    end function;

    function clearAbstractMappings(list: MapList) return MapList is
    begin

    end function;

    function clearAbstractMappingsPartial(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return MapList is
        variable res: MapList := list;
        variable mapping: AbstractMapping;
        constant tagHighSN: SmallNumber := getTagHighSN(tag);
        constant tagLow: InsTagLowPart := getTagLow(tag);
        variable ptr: natural := p2i(tagHighSN, ROB_SIZE);
        constant start: natural := p2i(startPtr, ROB_SIZE); 
        constant pEnd: natural := p2i(endPtr, ROB_SIZE); 
        variable subindex: natural := slv2u(tagLow);
        variable found: boolean := false;
    begin
        -- nothing to see?
        if startPtr = endPtr then
            return res;
        end if;
    
        -- if causing is last in row, go to next row
        if subindex = RENAME_W-1 then
            ptr := (ptr + 1) mod ROB_SIZE;
            subindex := 0;
        else
            subindex := subindex + 1;
        end if;
    
        loop
            if ptr = pEnd then
                exit;
            end if;
    
            loop
                mapping := list(ptr).mappings(subindex);
    
                if mapping.used = '1' then 
                    --if res(slv2u(mapping.virtual)) = nothing then
                        --res(slv2u(mapping.virtual)) := mapping.physical;
                    --end if;
                end if;
                
                res(ptr).mappings(subindex) := DEFAULT_ABSTRACT_MAPPING;
                
                if subindex = RENAME_W-1 then
                    exit;
                else
                    subindex := subindex + 1;
                end if;
            end loop;
            
            if ptr = ROB_SIZE-1 then
                ptr := 0;
            else
                ptr := (ptr + 1) mod ROB_SIZE;
            end if;
            
            subindex := 0;
        end loop;
        
        -- Clear all rows after the causing one
        ptr := (p2i(tagHighSN, ROB_SIZE) + 1) mod ROB_SIZE;
        
        loop
            if ptr = start then
                exit;
            end if;
            res(ptr) := DEFAULT_MAP_ROW;
            ptr := (ptr + 1) mod ROB_SIZE;
        end loop;
        
        return res;
        
    end function;

    function scanMappings(list: MapList; tag: InsTag; startPtr, endPtr: SmallNumber) return IntArray is
        variable res: IntArray(0 to N_PHYS-1) := (others => 0);
        variable ptr: natural := 0;--p2i(tagHighSN, ROB_SIZE);
        constant start: natural := p2i(startPtr, ROB_SIZE); 
        constant pEnd: natural := p2i(endPtr, ROB_SIZE);
        variable regNum: natural := 0;
    begin
        if startPtr = endPtr then
            return res;
        end if;
        
        ptr := start;
        
        while ptr /= pEnd loop
            for i in 0 to RENAME_W-1 loop 
                if list(ptr).mappings(i).used = '1' then
                    regNum := slv2u(list(ptr).mappings(i).physical);
                    res(regNum) := res(regNum) + 1;
                end if;
            end loop;
            ptr := (ptr + 1) mod ROB_SIZE;
        end loop;
        
        return res;
    end function;

    function scanFreeList(freeList: PhysNameArray; tag: InsTag; startPtr, endPtr: natural) return IntArray is
        variable res: IntArray(0 to N_PHYS-1) := (others => 0);
        variable ptr: natural := 0;--p2i(tagHighSN, ROB_SIZE);
        --constant start: natural := p2i(startPtr, ROB_SIZE); 
        --constant pEnd: natural := p2i(endPtr, ROB_SIZE);
        variable regNum: natural := 0;
    begin
        ptr := startPtr;
        loop
            -- CAREFUL: should this be done here or at loop end?
            --          Assumptions about list never being full/empty...
            if ptr = endPtr then
                return res;
            end if;
            
            regNum := slv2u(freeList(ptr));
            res(regNum) := res(regNum) + 1;
            
            ptr := (ptr + 1) mod FREE_LIST_SIZE;
        end loop;
    end function;

    function scanMappingTable(table: PhysNameArray; skipR0: boolean := true) return IntArray is
        variable res: IntArray(0 to N_PHYS-1) := (others => 0);
        variable regNum: natural := 0;
        variable iStart: natural := 0;
    begin
        if skipR0 then
            iStart := 1;
        end if;

        for i in iStart to 31 loop
            regNum := slv2u(table(i));
            res(regNum) := res(regNum) + 1;
        end loop;
        
        return res;
    end function;

    function applyChangedMappings(table, changed: PhysNameArray) return PhysNameArray is
        variable res: PhysNameArray(table'range) := table;
    begin
        for i in res'range loop
            if changed(i) /= nothing then
                res(i) := changed(i);
            end if;
        end loop;
        
        return res;
    end function;

end AbstractRenaming;
