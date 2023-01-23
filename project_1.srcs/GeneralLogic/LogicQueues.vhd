
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
        first: std_logic;
        
        hasEvent: std_logic;
    
        completedA: std_logic;
        completedV: std_logic;
        address: Mword;
        value: Mword;
    end record;
    
    constant DEFAULT_QUEUE_ENTRY: QueueEntry := (
        address => (others => '0'),
        value => (others => '0'),
        others => '0'
    );

    type QueueEntryArray is array (natural range <>) of QueueEntry;

    procedure updateElemOnInput(signal content: inout QueueEntryArray; ind: natural; isl: InstructionSlot; sysOp: std_logic; constant IS_LOAD_QUEUE: boolean);    
    procedure updateOnInput(signal content: inout QueueEntryArray; ptr: SmallNumber; fullMask, sysMask: std_logic_vector; constant IS_LOAD_QUEUE: boolean);
    procedure updateAddress(signal content: inout QueueEntryArray; er: ExecResult; constant IS_LOAD_QUEUE: boolean);
    
    procedure updateValue(signal content: inout QueueEntryArray; ind: SmallNumber);

    constant CMP_ADDRESS_LENGTH: natural := 12;    
        
    function getAddressCompleted(content: QueueEntryArray) return std_logic_vector;
    function getAddressMatching(content: QueueEntryArray; adr: Mword) return std_logic_vector;
    
    function addressLowMatching(a, b: Mword) return std_logic;
    function addressHighMatching(a, b: Mword) return std_logic;
    
    function getWhichMemOp(content: QueueEntryArray) return std_logic_vector;

    function getDrainOutput(elem: QueueEntry; adr, value: Mword) return ControlPacket;
    
    function cmpIndexBefore(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber) return std_logic_vector;
    function cmpIndexAfter(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber) return std_logic_vector;
    function findNewestMatchIndex(olderSQ: std_logic_vector; pStart, nFull: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber;

    function getCommittedEffectiveMask(robData: ControlPacketArray; isLQ: boolean) return std_logic_vector;
    function getCommittedMask(robData: ControlPacketArray; isLQ: boolean) return std_logic_vector;

    function getCommittedEffectiveMaskBr(robData: ControlPacketArray) return std_logic_vector;
    function getCommittedMaskBr(robData: ControlPacketArray) return std_logic_vector;

    function shiftQueueContent(content: QueueEntryArray; startPtrNext, nFullNext: SmallNumber; ev, prevSending, draining, compareInputFull: std_logic;
                               sqPtr: SmallNumber; op: SpecificOp; adr: Mword; constant QUEUE_PTR_SIZE: natural)
    return QueueEntryArray;

end package;


package body LogicQueues is

    procedure updateElemOnInput(signal content: inout QueueEntryArray; ind: natural; isl: InstructionSlot; sysOp: std_logic; constant IS_LOAD_QUEUE: boolean) is
    begin
        if not IS_LOAD_QUEUE then
            content(ind).isSysOp <= isStoreSysOp(isl.ins.specificOperation);
        else
            content(ind).isSysOp <= isLoadSysOp(isl.ins.specificOperation);                
        end if;
        
        content(ind).isSysOp <= sysOp;
        
        content(ind).first <= '0'; -- TMP
        content(ind).hasEvent <= '0';
    
        content(ind).completedA <= '0';
        content(ind).completedV <= '0';
    end procedure;

    procedure updateOnInput(signal content: inout QueueEntryArray; ptr: SmallNumber; fullMask, sysMask: std_logic_vector; constant IS_LOAD_QUEUE: boolean) is
        constant LEN: natural := content'length;
        constant PTR_MASK_SN: SmallNumber := i2slv(LEN-1, SMALL_NUMBER_SIZE);
        constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);        
        variable queueInds, inputRevInds: IntArray(fullMask'range);
        variable inputInds: IntArray(fullMask'range) := (others => fullMask'length);
        variable tmpPtr: SmallNumber := ptr and PTR_MASK_SN;
    begin
        for i in fullMask'range loop                
            queueInds(i) := slv2u(tmpPtr);
            tmpPtr := addIntTrunc(tmpPtr, 1, QUEUE_PTR_SIZE);
            inputRevInds(i) := countOnes(fullMask(0 to i-1)); -- which slot input[i] takes after compression
            inputInds(inputRevInds(i)) := i;                  -- 
        end loop;
        
        for i in fullMask'range loop
            if i < countOnes(fullMask) then
                updateElemOnInput(content, queueInds(i), DEFAULT_INS_SLOT, sysMask(inputInds(i)), IS_LOAD_QUEUE);
            end if;
        end loop;
    
        content(queueInds(0)).first <= '1';
    end procedure;

    procedure updateAddress(signal content: inout QueueEntryArray; er: ExecResult; constant IS_LOAD_QUEUE: boolean) is
        constant LEN: natural := content'length;
        constant PTR_MASK_SN: SmallNumber := i2slv(LEN-1, SMALL_NUMBER_SIZE);
        constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);        
        variable indV: SmallNumber;
        variable ind: natural;
        variable allow: std_logic;
    begin
        allow := er.full;
        indV := er.dest and PTR_MASK_SN;
        ind := slv2u(indV);
 
        if allow = '1' then
            content(ind).completedA <= '1';
            content(ind).address <= er.value;
        end if;        
    end procedure;

    procedure updateValue(signal content: inout QueueEntryArray;-- isl: InstructionSlot;
                                ind: SmallNumber) is
        constant LEN: natural := content'length;
        constant PTR_MASK_SN: SmallNumber := i2slv(LEN-1, SMALL_NUMBER_SIZE);
        constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
        constant indV: SmallNumber := ind and PTR_MASK_SN;
        constant index: natural := slv2u(indV);
    begin
        content(index).completedV <= '1';
    end procedure;

    function getAddressCompleted(content: QueueEntryArray) return std_logic_vector is
        variable res: std_logic_vector(content'range);
    begin
        for i in content'range loop
            res(i) := content(i).completedA;
        end loop;
        return res;
    end function;
    
    
    function addressLowMatching(a, b: Mword) return std_logic is
    begin
        return bool2std(a(CMP_ADDRESS_LENGTH-1 downto 0) = b(CMP_ADDRESS_LENGTH-1 downto 0));
    end function;
    
    function addressHighMatching(a, b: Mword) return std_logic is
    begin
        return bool2std(a(31 downto CMP_ADDRESS_LENGTH) = b(31 downto CMP_ADDRESS_LENGTH));
    end function;
    
    
    function getAddressMatching(content: QueueEntryArray; adr: Mword) return std_logic_vector is
        variable res: std_logic_vector(content'range);
    begin
        for i in content'range loop      
            res(i) := addressLowMatching(content(i).address, adr);
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

    function getDrainOutput(elem: QueueEntry; adr, value: Mword) return ControlPacket is
        variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
    begin
        if elem.isSysOp = '1' then
            res.op := sop(None, opStoreSys);
        else
            res.op := sop(None, opStore);                        
        end if;

        res.controlInfo.newEvent := elem.hasEvent;
        res.controlInfo.firstBr := elem.first;
        res.controlInfo.sqMiss := not elem.completedV;   
        res.target := adr;
        res.nip := value;    
        return res;
    end function;

    function cmpIndexBefore(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber)
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
        end loop;
        return res;
    end function;
    
    function cmpIndexAfter(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural; constant PTR_MASK_SN: SmallNumber)
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
        end loop;
        return res;
    end function;

    function findNewestMatchIndex(olderSQ: std_logic_vector; pStart, nFull: SmallNumber; constant QUEUE_PTR_SIZE: natural)
    return SmallNumber is
        constant LEN: integer := olderSQ'length;      
        variable tmpVec1: std_logic_vector(0 to LEN-1) := (others => '0');
        variable tmpVecExt: std_logic_vector(0 to 2*LEN-1) := (others => '0');
        
        variable res: SmallNumber := (others => '0');
        variable nShift, count: natural := 0;
    begin
        -- Shift by pStart
        nShift := slv2u(pStart);
        count := slv2u(nFull);
        
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
        res := addTruncZ(res, pStart, QUEUE_PTR_SIZE);
        return res;
    end function;


-- scan: full and syncEvent; full and [usingQ]
 function getCommittedEffectiveMask(robData: ControlPacketArray; isLQ: boolean) return std_logic_vector is
    variable res: std_logic_vector(robData'range) := (others => '0');
 begin
    for i in 0 to PIPE_WIDTH-1 loop
        if robData(i).controlInfo.full = '1' and hasSyncEvent(robData(i).controlInfo) = '1' then
            exit;
        end if;
        
        if isLQ then
            res(i) := robData(i).controlInfo.full and robData(i).classInfo.useLQ;--'1';
        else
            res(i) := robData(i).controlInfo.full and robData(i).classInfo.secCluster;--'1';
        end if;
     
    end loop;
    return res;
 end function;
    
    -- scan: newEvent and syncEvent; [usingQ] 
    function getCommittedMask(robData: ControlPacketArray; isLQ: boolean) return std_logic_vector is
        variable res: std_logic_vector(robData'range) := (others => '0');
    begin
    for i in 0 to PIPE_WIDTH-1 loop
        -- A redirected branch cuts a group in SQ, so it must stop there
        if robData(i).controlInfo.newEvent = '1' and hasSyncEvent(robData(i).controlInfo) = '0' then
            exit;
        end if;
    
        -- Not only full, because exceptions clear following 'full' bits
        if isLQ then
            res(i) := robData(i).classInfo.useLQ;
        else
            res(i) := robData(i).classInfo.secCluster;
        end if;
    end loop;
    return res;
    end function;

    -- scan: full and syncEvent; full and branch
     function getCommittedEffectiveMaskBr(robData: ControlPacketArray) return std_logic_vector is
        variable res: std_logic_vector(robData'range) := (others => '0');
     begin
        for i in 0 to PIPE_WIDTH-1 loop
            if robData(i).controlInfo.full = '1' and hasSyncEvent(robData(i).controlInfo) = '1' then
                exit;
            end if;

            res(i) := robData(i).controlInfo.full and robData(i).classInfo.branchIns;
        end loop;
        return res;
     end function;

     -- scan: newEvent and syncEvent; branch
     function getCommittedMaskBr(robData: ControlPacketArray) return std_logic_vector is
        variable res: std_logic_vector(robData'range) := (others => '0');
     begin
        for i in 0 to PIPE_WIDTH-1 loop
            -- A redirected branch cuts a group in SQ, so it must stop there
            if robData(i).controlInfo.newEvent = '1' and hasSyncEvent(robData(i).controlInfo) = '0' then
                exit;
            end if;

            -- Not only full, because exceptions clear following 'full' bits
            res(i) := robData(i).classInfo.branchIns;
        end loop;
        return res;
     end function;



    function shiftQueueContent(content: QueueEntryArray; startPtrNext, nFullNext: SmallNumber; ev, prevSending, draining, compareInputFull: std_logic;
                               sqPtr: SmallNumber; op: SpecificOp; adr: Mword; constant QUEUE_PTR_SIZE: natural)
    return QueueEntryArray is
        variable res: QueueEntryArray(0 to content'length-1) := content;
        constant LEN: natural := content'length;
        variable currentPtr: SmallNumber := (others => '0');
    begin
        if draining = '1' then -- Move forward     
            res(0 to LEN-2) := res(1 to LEN-1);
            res(LEN-1).completedA := '0';
        end if;

        currentPtr := subTruncZ(sqPtr, startPtrNext, QUEUE_PTR_SIZE);

        if compareInputFull = '1' and isStoreOp(op) = '1' then
            res(slv2u(currentPtr)).completedA := '1';
        end if;

        if compareInputFull = '1' and isStoreMemOp(op) = '1' then
            res(slv2u(currentPtr)).address := adr;
        end if;

        if ev = '1' then
            for i in 0 to LEN-1 loop -- clear 'completed' for empty slots
                if i >= slv2u(nFullNext) then
                    res(i).completedA := '0';
                end if;
            end loop;
        elsif prevSending = '1' then
            -- move ptr, but it's handled by pointer mechanics
        end if;

        return res;
    end function;

end package body;
