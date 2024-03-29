
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.PipelineGeneral.all;


package LogicQueues is

    type QueueEntry is record
        isSysOp: std_logic;
        first: std_logic;

        hasEvent: std_logic; -- Never set?

        completedA: std_logic;
        completedLowA: std_logic;
        completedV: std_logic;
        address: Mword;
        addressLow: Mword;
        value: Mword;
    end record;

    constant DEFAULT_QUEUE_ENTRY: QueueEntry := (
        address => (others => '0'),
        addressLow => (others => '0'),
        value => (others => '0'),
        others => '0'
    );

    type QueueEntryArray is array (natural range <>) of QueueEntry;

    procedure updateElemOnInput(signal content: inout QueueEntryArray; ind: natural; sysOp: std_logic);    
    procedure updateOnInput(signal content: inout QueueEntryArray; ptr: SmallNumber; fullMask, sysMask: std_logic_vector);

    constant CMP_ADDRESS_LENGTH: natural := 12;    

    function getAddressCompleted(content: QueueEntryArray) return std_logic_vector;
    function getAddressCompleted_Low(content: QueueEntryArray) return std_logic_vector;
    function getAddressMatching(content: QueueEntryArray; adr: Mword) return std_logic_vector;
    function getAddressMatching_Low(content: QueueEntryArray; adr: Mword) return std_logic_vector;

    function addressLowMatching(a, b: Mword) return std_logic;
    function addressHighMatching(a, b: Mword) return std_logic;

    function getWhichMemOp(content: QueueEntryArray) return std_logic_vector;

    function getDrainOutput(elem: QueueEntry; adr, value: Mword) return ControlPacket;

    function cmpIndexBefore(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural)--; constant PTR_MASK_SN: SmallNumber)
    return std_logic_vector;
    function cmpIndexAfter(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural)--; constant PTR_MASK_SN: SmallNumber)
    return std_logic_vector;

    function findNewestMatchIndex(olderSQ: std_logic_vector; pStart, nFull: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber;
    function findNewestMatchIndex_2(olderSQ: std_logic_vector; pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber;

    function getCommittedEffectiveMask(robData: ControlPacketArray; isLQ: boolean) return std_logic_vector;
    function getCommittedMaskBr(robData: ControlPacketArray) return std_logic_vector;

    procedure shiftQueueContent_Drain(signal content: inout QueueEntryArray);
    procedure shiftQueueContent_Evt(signal content: inout QueueEntryArray; nFullNext: SmallNumber);


    function makeSelectedOutputSQ(selectedOutput: ControlPacket; isSelected, sqMissed: std_logic) return ControlPacket;
    function makeCommittedOutputSQ(drainOutput: ControlPacket; isDrainingPrev: std_logic) return ControlPacket;
    function makeSelectedOutputLQ(isSelected: std_logic) return ControlPacket;
    function selDataRes(dest: SmallNumber) return ExecResult;
    procedure DB_logFW(adrInput: ExecResult; pSelect: SmallNumber; IS_LOAD_QUEUE: boolean);

end package;


package body LogicQueues is

    procedure updateElemOnInput(signal content: inout QueueEntryArray; ind: natural; sysOp: std_logic) is
    begin
        content(ind).isSysOp <= sysOp;

        content(ind).first <= '0'; -- TMP
        content(ind).hasEvent <= '0';

        content(ind).completedA <= '0';
        content(ind).completedLowA <= '0';
        content(ind).completedV <= '0';
    end procedure;

    procedure updateOnInput(signal content: inout QueueEntryArray; ptr: SmallNumber; fullMask, sysMask: std_logic_vector --; constant IS_LOAD_QUEUE: boolean
    ) is
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
                updateElemOnInput(content, queueInds(i), sysMask(inputInds(i)));
            end if;
        end loop;

        content(queueInds(0)).first <= '1';
    end procedure;


    function getAddressCompleted(content: QueueEntryArray) return std_logic_vector is
        variable res: std_logic_vector(content'range);
    begin
        for i in content'range loop
            res(i) := content(i).completedA;
        end loop;
        return res;
    end function;


    function getAddressCompleted_Low(content: QueueEntryArray) return std_logic_vector is
        variable res: std_logic_vector(content'range);
    begin
        for i in content'range loop
            res(i) := content(i).completedLowA;
        end loop;
        return res;
    end function;

    function addressLowMatching(a, b: Mword) return std_logic is
    begin
        return bool2std(a(CMP_ADDRESS_LENGTH-1 downto 2) = b(CMP_ADDRESS_LENGTH-1 downto 2));
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

    function getAddressMatching_Low(content: QueueEntryArray; adr: Mword) return std_logic_vector is
        variable res: std_logic_vector(content'range);
    begin
        for i in content'range loop      
            res(i) := addressLowMatching(content(i).addressLow, adr);
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

    function cmpIndexBefore(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural) --; constant PTR_MASK_SN: SmallNumber)
    return std_logic_vector is
    	constant PTR_MASK_SN: SmallNumber := sn(QUEUE_SIZE-1);
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
            iv := --i2slv(i, SMALL_NUMBER_SIZE);
                    sn(i);
            if sign = '0' then --cmpGeU(index, pStart) = '1' then
                res(i) := cmpGeU(iv, pStart) and cmpLtU(iv, index);
            else
                res(i) := cmpGeU(iv, pStart) or cmpLtU(iv, index);
            end if;
        end loop;
        return res;
    end function;

    function cmpIndexAfter(pStartLong, pEndLong, cmpIndexLong: SmallNumber; constant QUEUE_SIZE: natural) --; constant PTR_MASK_SN: SmallNumber)
    return std_logic_vector is
    	constant PTR_MASK_SN: SmallNumber := sn(QUEUE_SIZE-1);
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
            iv := --i2slv(i, SMALL_NUMBER_SIZE);
                    sn(i);
            if sign = '1' then
                res(i) := cmpLtU(iv, pEnd) or cmpGeU(iv, index);
            else
                res(i) := cmpLtU(iv, pEnd) and cmpGeU(iv, index);
            end if;
        end loop;
        return res;
    end function;

    -- NOTE: pStart is for generality, in shifting adr queue it's always 0 
    function findNewestMatchIndex(olderSQ: std_logic_vector; pStart, nFull: SmallNumber; constant QUEUE_PTR_SIZE: natural)
    return SmallNumber is
        constant LEN: integer := olderSQ'length;      
        variable tmpVec1: std_logic_vector(0 to LEN-1) := (others => '0');
        variable tmpVecExt: std_logic_vector(0 to 2*LEN-1) := (others => '0');

        variable res: SmallNumber := (others => '0');
        constant nShift: natural := slv2u(pStart);
        constant count: natural := slv2u(nFull);
    begin
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


    function findNewestMatchIndex_2(olderSQ: std_logic_vector; pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural)
    return SmallNumber is
        constant LEN: integer := olderSQ'length;
        constant PTR_MASK_SN: SmallNumber := sn(LEN-1);  
        variable beforeStart: std_logic_vector(0 to LEN-1) := (others => '0');
 --       constant wrap: std_logic := pStart(QUEUE_PTR_SIZE) xor pEnd(QUEUE_PTR_SIZE);

 --       variable res: SmallNumber := (others => '0');

        constant pStartTr: SmallNumber := pStart and PTR_MASK_SN;
        constant pEndTr: SmallNumber := pEnd and PTR_MASK_SN;
    begin        
        for i in 0 to LEN-1 loop
            beforeStart(i) := cmpLtU(sn(i), pStartTr) and olderSQ(i);
        end loop;

        -- beforeStart is older than after start, so we scan it first
        for i in LEN-1 downto 0 loop
            if beforeStart(i) = '1' then
                return sn(i);
            end if;
        end loop;
        
        -- if not found there, scan the rest
        for i in LEN-1 downto 0 loop
            if olderSQ(i) = '1' then
                return sn(i);
            end if;
        end loop;

        return sn(0);
    end function;


    -- scan: full and syncEvent; full and [usingQ]
     function getCommittedEffectiveMask(robData: ControlPacketArray; isLQ: boolean) return std_logic_vector is
        variable res: std_logic_vector(robData'range) := (others => '0');
     begin
        for i in 0 to PIPE_WIDTH-1 loop
            if robData(i).controlInfo.c_full = '1' then
                if hasSyncEvent(robData(i).controlInfo) = '1' then
                    exit;
                end if;
    
                if isLQ then
                    res(i) := robData(i).classInfo.useLQ;
                else
                    res(i) := robData(i).classInfo.secCluster;
                end if;
            end if;
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



    procedure shiftQueueContent_Drain(signal content: inout QueueEntryArray) is
        constant LEN: natural := content'length;
    begin
        content(0 to LEN-2) <= content(1 to LEN-1);

        content(LEN-1).isSysOp <= '0';

        content(LEN-1).first <= '0'; -- TMP
        content(LEN-1).hasEvent <= '0';

        content(LEN-1).completedA <= '0';
        content(LEN-1).completedLowA <= '0';
        content(LEN-1).completedV <= '0';
   end procedure;

    procedure shiftQueueContent_Evt(signal content: inout QueueEntryArray; nFullNext: SmallNumber) is
        constant LEN: natural := content'length;
    begin
        -- Clear empty slots
        --if ev = '1' then
            for i in 0 to LEN-1 loop -- clear 'completed' for empty slots
                if i >= slv2u(nFullNext) then
                    content(i).completedA <= '0';
                    content(i).completedLowA <= '0';
                end if;
            end loop;
        --end if;
    end procedure;


    function makeSelectedOutputSQ(selectedOutput: ControlPacket; isSelected, sqMissed: std_logic) return ControlPacket is
        variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
    begin
        res.controlInfo.c_full := isSelected;
        res.controlInfo.newEvent := selectedOutput.controlInfo.newEvent;
        res.controlInfo.firstBr := selectedOutput.controlInfo.firstBr;
        res.controlInfo.sqMiss := sqMissed;

        res.op := selectedOutput.op;
        res.target := selectedOutput.target;
        res.nip := selectedOutput.nip;
        return res;
    end function;

    function makeCommittedOutputSQ(drainOutput: ControlPacket; isDrainingPrev: std_logic) return ControlPacket is
        variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
    begin
        res.full := isDrainingPrev;
        res.controlInfo.c_full := isDrainingPrev;

        res.op := drainOutput.op;
        res.target := drainOutput.target;
        res.nip := drainOutput.nip;
        return res;
    end function;

    function makeSelectedOutputLQ(isSelected: std_logic) return ControlPacket is
        variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
    begin
        res.controlInfo.c_full := isSelected;
        return res;
    end function;

    function selDataRes(dest: SmallNumber) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
    begin
        res.dest(7 downto 0) := dest(7 downto 0);
        return res;
    end function;

    procedure DB_logFW(adrInput: ExecResult; pSelect: SmallNumber; IS_LOAD_QUEUE: boolean) is
    begin
         -- pragma synthesis off
        if DB_LSQ_TRACKING then
            report "";
            report "DEBUG: Store FW(" & boolean'image(IS_LOAD_QUEUE) & "): seqNum " & work.CpuText.w2hex(adrInput.dbInfo.seqNum) & " from entry " & work.CpuText.slv2hex(pSelect);
            report "";
        end if;
        -- pragma synthesis on
    end procedure;

end package body;
