
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;
use work.PipelineGeneral.all;


package LogicQueues is

    type QueueEntry is record
        isSysOp: std_logic;
        first: std_logic;           -- input
        -- TODO: add operation type - needed when writing (mem or sys) and whe comparing (mem or sys), also access size!
        
        hasEvent: std_logic;        -- random

        completedA: std_logic;      -- random
        completedV: std_logic;      -- random
        address: Mword;             -- random
        value: Mword;               -- random
        
    end record;
    
    constant DEFAULT_QUEUE_ENTRY: QueueEntry := (
        address => (others => '0'),
        value => (others => '0'),
        others => '0'
    );
        
    type QueueEntryArray is array (natural range <>) of QueueEntry;

    procedure updateElemOnInput(signal content: inout QueueEntryArray; ind: natural; isl: InstructionSlot; constant IS_LOAD_QUEUE: boolean);    
    procedure updateOnInput(signal content: inout QueueEntryArray; ptr: SmallNumber; insVec: InstructionSlotArray; constant IS_LOAD_QUEUE: boolean);
    procedure updateAddress(signal content: inout QueueEntryArray; isl: InstructionSlot; constant IS_LOAD_QUEUE: boolean);
    procedure updateValue(signal content: inout QueueEntryArray; isl: InstructionSlot);
    
    function getAddressCompleted(content: QueueEntryArray) return std_logic_vector;
    function getAddressMatching(content: QueueEntryArray; adr: Mword) return std_logic_vector;
    function getWhichMemOp(content: QueueEntryArray) return std_logic_vector;
    --function getQueueContent_T(ia: InstructionStateArray) return QueueEntryArray;
    function getDrainOutput_T(elem: QueueEntry; value: Mword) return InstructionState;
    
    --function TMP_clearOutputDebug(ins: InstructionState) return InstructionState;
    function TMP_cmpIndexBefore(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber) return std_logic_vector;
    function TMP_cmpIndexAfter(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber) return std_logic_vector;
    function findNewestMatchIndex2(olderSQ: std_logic_vector; pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber;
    function getNumCommittedEffective(robData: InstructionSlotArray; isLQ: boolean) return SmallNumber; 
    function getNumCommitted(robData: InstructionSlotArray; isLQ: boolean) return SmallNumber;
end package;


package body LogicQueues is

        procedure updateElemOnInput(signal content: inout QueueEntryArray; ind: natural; isl: InstructionSlot; constant IS_LOAD_QUEUE: boolean) is
        begin
            if not IS_LOAD_QUEUE then
                content(ind).isSysOp <= isStoreSysOp(isl.ins);
            else
                content(ind).isSysOp <= isLoadSysOp(isl.ins);                
            end if;
            
            content(ind).first <= '0'; -- TMP
            content(ind).hasEvent <= '0';

            content(ind).completedA <= '0';
            content(ind).completedV <= '0';
        end procedure;
        
        
        procedure updateOnInput(signal content: inout QueueEntryArray; ptr: SmallNumber; insVec: InstructionSlotArray; constant IS_LOAD_QUEUE: boolean) is
            constant LEN: natural := content'length;
            constant PTR_MASK_SN: SmallNumber := i2slv(LEN-1, SMALL_NUMBER_SIZE);
            constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);        
            variable queueInds, inputRevInds: IntArray(insVec'range);
            variable inputInds: IntArray(insVec'range) := (others => insVec'length);
            variable tmpPtr: SmallNumber := ptr and PTR_MASK_SN;
            --variable compressedInds
            constant fullMask: std_logic_vector(insVec'range) := extractFullMask(insVec);
        begin
            for i in insVec'range loop                
                queueInds(i) := slv2u(tmpPtr);
                tmpPtr := addIntTrunc(tmpPtr, 1, QUEUE_PTR_SIZE);
                inputRevInds(i) := countOnes(fullMask(0 to i-1)); -- which slot input[i] takes after compression
                inputInds(inputRevInds(i)) := i;                  -- 
            end loop;
            
            for i in insVec'range loop
                if i < countOnes(fullMask) then
                    updateElemOnInput(content, queueInds(i), insVec(inputInds(i)), IS_LOAD_QUEUE);
                end if;
            end loop;
            
            content(queueInds(0)).first <= '1';
        end procedure;


        
        procedure updateAddress(signal content: inout QueueEntryArray; isl: InstructionSlot; constant IS_LOAD_QUEUE: boolean) is
            constant LEN: natural := content'length;
            constant PTR_MASK_SN: SmallNumber := i2slv(LEN-1, SMALL_NUMBER_SIZE);
            constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);        
            variable indV: SmallNumber;
            variable ind: natural;
            variable allow: std_logic;
        begin
            if not IS_LOAD_QUEUE then
                indV := isl.ins.tags.sqPointer and PTR_MASK_SN;
                allow := isStoreOp(isl.ins);           
            else
                indV := isl.ins.tags.lqPointer and PTR_MASK_SN;
                allow := isLoadOp(isl.ins);
            end if;
            
            ind := slv2u(indV);
            
            if allow = '1' then
                content(ind).completedA <= '1';
                content(ind).address <= isl.ins.result;
            end if;        
        end procedure;

        procedure updateValue(signal content: inout QueueEntryArray; isl: InstructionSlot) is
            constant LEN: natural := content'length;
            constant PTR_MASK_SN: SmallNumber := i2slv(LEN-1, SMALL_NUMBER_SIZE);
            constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
            constant indV: SmallNumber := isl.ins.tags.sqPointer and PTR_MASK_SN;
            constant ind: natural := slv2u(indV);
        begin
            content(ind).completedV <= '1';
            --content(ind).value <= isl.ins.result;
        end procedure;

        
        function getAddressCompleted(content: QueueEntryArray) return std_logic_vector is
            variable res: std_logic_vector(content'range);
        begin
            for i in content'range loop
                res(i) := content(i).completedA;
            end loop;
            return res;
        end function;

        function getAddressMatching(content: QueueEntryArray; adr: Mword) return std_logic_vector is
            variable res: std_logic_vector(content'range);
        begin
            for i in content'range loop
                res(i) := bool2std(content(i).address = adr);
            end loop;
            return res;
        end function;
        
        function getWhichMemOp(content: QueueEntryArray) return std_logic_vector is
            variable res: std_logic_vector(content'range);
        begin
            for i in content'range loop
                res(i) := not content(i).isSysOp;
            end loop;
            return res;
        end function;
        
--            function getQueueContent_T(ia: InstructionStateArray) return QueueEntryArray is
--                variable res: QueueEntryArray;
--            begin
--                for i in ia'range loop
--                    res(i).isSysOp := isStoreSysOp(ia(i));
--                    res(i).hasEvent := ia(i).controlInfo.newEvent;
                    
--                    res(i).first := ia(i).controlInfo.firstBr;
--                    res(i).completedA := ia(i).controlInfo.completed;
--                    res(i).completedV := ia(i).controlInfo.completed2;
                    
--                    res(i).address := ia(i).target;
--                    res(i).value := ia(i).result;
                    
--                end loop;
--                return res;
--            end function;


            function getDrainOutput_T(elem: QueueEntry; value: Mword) return InstructionState is
                variable res: InstructionState := DEFAULT_INS_STATE;
            begin
                if elem.isSysOp = '1' then
                    res.specificOperation := sop(None, opStoreSys);
                else
                    res.specificOperation := sop(None, opStore);                        
                end if;
                
                res.controlInfo.newEvent := elem.hasEvent; 
                
                res.controlInfo.firstBr := elem.first ;
                res.controlInfo.completed := elem.completedA;
                res.controlInfo.completed2 := elem.completedV;
                
                res.target := elem.address;
                res.result := elem.value;
                   
                   res.result := value;
                    
                return res;
            end function;


  
--    function TMP_clearOutputDebug(ins: InstructionState) return InstructionState is
--        variable res: InstructionState := ins;
--    begin
--        if CLEAR_DEBUG_INFO then                    
--            res.classInfo := DEFAULT_CLASS_INFO;
--            res.constantArgs := DEFAULT_CONSTANT_ARGS;
--            res.virtualArgSpec := DEFAULT_ARG_SPEC;
--            res.physicalArgSpec := DEFAULT_ARG_SPEC;
            
--        end if;
--        return res;
--    end function;

    function TMP_cmpIndexBefore(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber)
    return std_logic_vector is
        constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
        variable res: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
        variable iv: SmallNumber := (others => '0');
        constant index: SmallNumber := cmpIndexLong and PTR_MASK_SN;
        constant pStart: SmallNumber := pStartLong and PTR_MASK_SN;
        constant pEnd: SmallNumber := pEndLong and PTR_MASK_SN;
        constant sign: std_logic := pStartLong(QUEUE_PTR_SIZE) xor cmpIndexLong(QUEUE_PTR_SIZE);             
    begin
        -- A) if index > start then i >= start && i < index
        -- B) if index < start then i >= start || i < index
        -- C) if index = start then none -> can be coalesced into A):
        -- A') if index >= start then i >= start && i < index    =>    i >= start && i < start   =>   i empty
    
        for i in 0 to res'length-1 loop
            iv := i2slv(i, SMALL_NUMBER_SIZE);
            if sign = '0' then --cmpGeU(index, pStart) = '1' then
                res(i) := cmpGeU(iv, pStart) and cmpLtU(iv, index);
            else
                res(i) := cmpGeU(iv, pStart) or cmpLtU(iv, index);
            end if;
            --res(i) := compareTagBefore(content(i).tags.renameIndex, tag); -- If grTag < tag then diff(high) = '1'
        end loop;
        return res;
    end function;

    function TMP_cmpIndexAfter(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber)
    return std_logic_vector is
        constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
        variable res: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
        variable iv: SmallNumber := (others => '0');
        constant index: SmallNumber := cmpIndexLong and PTR_MASK_SN;
        constant pStart: SmallNumber := pStartLong and PTR_MASK_SN;
        constant pEnd: SmallNumber := pEndLong and PTR_MASK_SN;
        constant sign: std_logic := pEndLong(QUEUE_PTR_SIZE) xor cmpIndexLong(QUEUE_PTR_SIZE);                     
    begin
        -- A) if index > end then i < end || i => index
        -- B) if index < end then i < end && i => index
        -- C) if index = end then all (because in this case start = end and queue is full; otherwise index = end wouldn't be possible)
        --           -> can be coalesced into A):
        -- A') if index >= end then i < end || i => index    =>    i < end || i => end   =>   i all
    
        for i in 0 to res'length-1 loop
            iv := i2slv(i, SMALL_NUMBER_SIZE);
            if sign = '1' then
                res(i) := cmpLtU(iv, pEnd) or cmpGeU(iv, index);
            else
                res(i) := cmpLtU(iv, pEnd) and cmpGeU(iv, index);
            end if;
            --res(i) := compareTagBefore(content(i).tags.renameIndex, tag); -- If grTag < tag then diff(high) = '1'
        end loop;
        return res;
    end function;

    function findNewestMatchIndex2(olderSQ: std_logic_vector; pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural)
    return SmallNumber is
        constant LEN: integer := olderSQ'length;      
        variable tmpVec1: std_logic_vector(0 to LEN-1) := (others => '0');
        variable tmpVecExt: std_logic_vector(0 to 2*LEN-1) := (others => '0');
        
        variable res: SmallNumber := (others => '0');
        variable nShift, count: natural := 0;
    begin
        -- Shift by pStart
        nShift := slv2u(pStart);
        count := slv2u(subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE));
        
        tmpVecExt := olderSQ & olderSQ;
        
        for i in 0 to LEN-1 loop
            tmpVec1(i) := tmpVecExt(i + nShift);
        end loop;
        
        -- Find first index
        for i in LEN-1 downto 0 loop
            if tmpVec1(i) = '1' and i < count then
                res := i2slv(i, SMALL_NUMBER_SIZE);
                exit;
            end if;
        end loop;
        -- Add pStart
        res := addTruncZ(res, pStart, QUEUE_PTR_SIZE);
        return res;
    end function;
    
     function getNumCommittedEffective(robData: InstructionSlotArray; isLQ: boolean) return SmallNumber is
        variable res: SmallNumber := (others => '0');
        variable k: integer := 0;
        variable found: boolean := false;
     begin
        for i in 0 to PIPE_WIDTH-1 loop
            if robData(i).full = '1' and hasSyncEvent(robData(i).ins) = '1' then
                exit;
            end if;
            
            if isLQ then
                if robData(i).full = '1' and robData(i).ins.classInfo.useLQ = '1' then
                    k := k + 1;
                end if;            
            else
                if robData(i).full = '1' and robData(i).ins.classInfo.secCluster = '1' then
                    k := k + 1;
                end if;
            end if;
         
        end loop;
        return i2slv(k, SMALL_NUMBER_SIZE);
     end function;
     
     function getNumCommitted(robData: InstructionSlotArray; isLQ: boolean) return SmallNumber is
        variable res: SmallNumber := (others => '0');
        variable k: integer := 0;
        variable found: boolean := false;
     begin
        for i in 0 to PIPE_WIDTH-1 loop
            -- A redirected branch cuts a group in SQ, so it must stop there
            if robData(i).ins.controlInfo.newEvent = '1' and hasSyncEvent(robData(i).ins) = '0' then
                exit;
            end if;
        
            -- Not only full, because exceptions clear following 'full' bits
            if isLQ then
                if robData(i).ins.classInfo.useLQ = '1' then
                    k := k + 1;
                end if;
            else
                if robData(i).ins.classInfo.secCluster = '1' then
                    k := k + 1;
                end if;
            end if;
        end loop;
        return i2slv(k, SMALL_NUMBER_SIZE);
     end function;


end package body;