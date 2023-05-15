--

--

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.DecodingDev.all;


package LogicFront is

function getEarlyEvent(fetchLine: WordArray(0 to FETCH_WIDTH-1); target, predictedAddress: Mword; fetchStall, send: std_logic) return ControlPacket;

function decodeGroup(fetchLine: WordArray(0 to FETCH_WIDTH-1); nWords: natural; ip: Mword; ctrl: ControlPacket) return BufferEntryArray;
function getControlA(fetchLine: WordArray(0 to FETCH_WIDTH-1); nWords: natural;ip: Mword; hasBranch: std_logic) return ControlPacketArray;

function groupHasBranch(ea: BufferEntryArray) return std_logic;

-- DEBUG
function assignSeqNum(cpa: ControlPacketArray; seqNum: Word) return ControlPacketArray;
function assignSeqNum(ba: BufferEntryArray; seqNum: Word; ctrl: ControlPacket) return BufferEntryArray;

function DB_addBitsAndIp(dbi: InstructionDebugInfo; bits: Word; ip: Mword) return InstructionDebugInfo;
function DB_addSeqNum(dbi: InstructionDebugInfo; sn: Word) return InstructionDebugInfo;

end LogicFront;



package body LogicFront is


function isJumpLink(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = "001001");
end function;

function isJumpCond(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = "001010" or w(31 downto 26) = "001011");
end function;

function isJumpLong(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = "001000");
end function;

function isJumpReg(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = "000000" and w(15 downto 10) = "000010" and (w(4 downto 0) = "00000" or w(4 downto 0) = "00001"));
end function;


function partialMask(adr: Mword) return std_logic_vector is
    variable res: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
    constant nSkip: natural := slv2u(adr(ALIGN_BITS-1 downto 2));
begin
    for i in 0 to FETCH_WIDTH-1 loop
        res(i) := '1';
        if i < nSkip then
            res(i) := '0';
        end if;
    end loop;
    return res; 
end function;


function getFrontEvent(ip, target: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1)) return ControlPacket is
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
	variable tempOffset, tempIP, tempTarget: Mword := (others => '0');
	variable branchIns, predictedTaken, uncondJump, longJump: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
	constant partMask: std_logic_vector(0 to FETCH_WIDTH-1) := partialMask(ip);
	variable regularJump, jumpLink, jumpCond, jumpLong, jumpReg: std_logic := '0';
begin

    -- Calculate target for each instruction, even if it's to be skipped
    for i in 0 to FETCH_WIDTH-1 loop
        jumpLink := isJumpLink(fetchLine(i));
        jumpCond := isJumpCond(fetchLine(i));
        jumpLong := isJumpLong(fetchLine(i));
        jumpReg  := isJumpReg(fetchLine(i));

        uncondJump(i) := jumpLink or jumpLong; 
        regularJump := jumpLink or jumpCond;
        longJump(i) := jumpLong;
        
        predictedTaken(i) := jumpLink or jumpLong or (jumpCond and fetchLine(i)(20));

        branchIns(i) := regularJump or longJump(i) or jumpReg;
	end loop;

    res.tags.bqPointer := --sn(3); -- CAREFUL, TMP: here used to indicate index of taken branch within fetch group
                          sn(FETCH_WIDTH-1);
                        
    -- Find if any branch predicted
    for i in 0 to FETCH_WIDTH-1 loop
        if partMask(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            if longJump(i) = '1' then
                tempOffset := (others => fetchLine(i)(25));
                tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
            else
                tempOffset := (others => fetchLine(i)(20));
                tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);
            end if;
            
            tempIP := ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(i*4, ALIGN_BITS);    -- !! Only for BQ, indirect
            tempTarget := add(tempIP, tempOffset);
            res.target := tempTarget;

            res.controlInfo.confirmedBranch := uncondJump(i);
            res.controlInfo.frontBranch := '1';
            res.controlInfo.full := '1';

            -- Here check if the next line from line predictor agrees with the target predicted now.
            --	If so, don't cause the event but set invalidation mask that next line will use.
            if res.target(MWORD_SIZE-1 downto ALIGN_BITS) /= target(MWORD_SIZE-1 downto ALIGN_BITS) then
                res.controlInfo.newEvent := '1';         -- !! Only for BQ
            end if;

            -- CAREFUL: When not using line predictor, branches predicted taken must always be done here 
            if not USE_LINE_PREDICTOR then
                res.controlInfo.newEvent := '1';         -- !! Only for BQ
            end if;

            res.tags.bqPointer := sn(i); -- TMP!

            exit;
        end if;
    end loop;

	return res;
end function;


function getEarlyEvent(fetchLine: WordArray(0 to FETCH_WIDTH-1); target, predictedAddress: Mword; fetchStall, send: std_logic)
return ControlPacket is
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
	constant cp: ControlPacket := getFrontEvent(predictedAddress, target, fetchLine);
begin
    if fetchStall = '1' then -- Need refetching
        res.target := predictedAddress;
        res.controlInfo.newEvent := '1';
        res.controlInfo.refetch := '1';
    elsif send = '1' then
        res.target := target;

        if cp.controlInfo.full = '1' and cp.controlInfo.frontBranch = '1' then
            res.target := cp.target; -- Correcting target within subsequent fetch line is still needed even if no redirection!
            res.controlInfo.newEvent := cp.controlInfo.newEvent; -- CAREFUL: event only if needs redirection, but break group at any taken jump
            res.controlInfo.frontBranch := '1';
        end if;
    end if;

    res.tags.bqPointer := cp.tags.bqPointer;

    return res;
end function;


function decodeGroup(fetchLine: WordArray(0 to FETCH_WIDTH-1); nWords: natural; ip: Mword; ctrl: ControlPacket)
return BufferEntryArray is
    variable res: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
    variable tmpIP: Mword := (others => '0');
    variable classInfo: InstructionClassInfo := DEFAULT_CLASS_INFO;
    variable op: SpecificOp := DEFAULT_SPECIFIC_OP;
    variable constantArgs: InstructionConstantArgs := DEFAULT_CONSTANT_ARGS;
    variable argSpec: InstructionArgSpec := DEFAULT_ARG_SPEC;
    variable anyBranch, isFull: std_logic := '0';
begin
    for i in 0 to FETCH_WIDTH-1 loop
	    decodeFromWord(fetchLine(i), classInfo, op, constantArgs, argSpec);

        isFull := bool2std(i < nWords);

        if isFull = '1' and classInfo.branchIns = '1' then
            anyBranch := '1';
        end if; 

        if isFull = '0' then
            argSpec.intDestSel := '0';
            argSpec.floatDestSel := '0';
        end if;          

        res(i).full := isFull;

        res(i).specialAction := not (classInfo.mainCluster or classInfo.secCluster);

        res(i).classInfo := classInfo;
        res(i).specificOperation := op;
        res(i).constantArgs := constantArgs;
        res(i).argSpec := argSpec;

        if DB_ENABLE then
            tmpIP := addInt(ip, 4*i);
            res(i).dbInfo := DB_addBitsAndIp(ctrl.dbInfo, fetchLine(i), tmpIP);
        end if;
    end loop;

    res(0).firstBr := anyBranch;

    return res;
end function;


function groupHasBranch(ea: BufferEntryArray) return std_logic is
begin
    for i in 0 to FETCH_WIDTH-1 loop
        if ea(i).full = '1' and ea(i).classInfo.branchIns = '1' then
            return '1';
        end if;   
    end loop;

    return '0';
end function;


function getControlA(fetchLine: WordArray(0 to FETCH_WIDTH-1); nWords: natural;ip: Mword; hasBranch: std_logic) return ControlPacketArray is
	variable res: ControlPacketArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
	variable tempIP, tempOffset: Mword := (others => '0');
	variable full, branchIns, predictedTaken, uncondJump, longJump: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
	variable regularJump, jumpLink, jumpCond, jumpLong, jumpReg: std_logic := '0';
begin

    -- Calculate target for each instruction, even if it's to be skipped
    for i in 0 to FETCH_WIDTH-1 loop
        jumpLink := isJumpLink(fetchLine(i));
        jumpCond := isJumpCond(fetchLine(i));
        jumpLong := isJumpLong(fetchLine(i));
        jumpReg  := isJumpReg(fetchLine(i));

        uncondJump(i) := jumpLink or jumpLong; 
        regularJump := jumpLink or jumpCond;
        longJump(i) := jumpLong;
        
        predictedTaken(i) := jumpLink or jumpLong or (jumpCond and fetchLine(i)(20));
        branchIns(i) := regularJump or longJump(i) or jumpReg;

        if longJump(i) = '1' then
            tempOffset := (others => fetchLine(i)(25));
            tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
        else
            tempOffset := (others => fetchLine(i)(20));
            tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);
        end if;


        tempIP := addInt(ip, 4*i);
        res(i).target := add(tempIP, tempOffset);
        res(i).nip := addInt(ip, 4*(i + 1));

        full(i) := bool2std(i < nWords);

        if full(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            res(i).controlInfo.confirmedBranch := uncondJump(i);
            res(i).controlInfo.frontBranch := '1';
        end if;

    end loop;

    -- TMP!
    res(0).ip(MWORD_SIZE-1 downto ALIGN_BITS) := ip(MWORD_SIZE-1 downto ALIGN_BITS);
    res(0).controlInfo.firstBr := hasBranch;

	return res;
end function;



function assignSeqNum(cpa: ControlPacketArray; seqNum: Word) return ControlPacketArray is
    variable res: ControlPacketArray(0 to cpa'length-1) := cpa;
    variable sn: Word := seqNum;
begin
    for i in res'range loop
        if res(i).controlInfo.full /= '1' then
            res(i).dbInfo := DEFAULT_DEBUG_INFO;
            next;
        end if;

        res(i).dbInfo := DB_addSeqNum(res(i).dbInfo, sn);
        sn := addInt(sn, 1);
    end loop;
    return res;
end function;

function assignSeqNum(ba: BufferEntryArray; seqNum: Word; ctrl: ControlPacket) return BufferEntryArray is
    variable res: BufferEntryArray := ba;
    variable sn: Word := seqNum;
begin
    for i in res'range loop
        if res(i).full /= '1' then
            res(i).dbInfo := DEFAULT_DEBUG_INFO;
            next;
        end if;

        res(i).dbInfo := DB_addSeqNum(res(i).dbInfo, sn);        
        sn := addInt(sn, 1);
    end loop;
    return res;
end function;


-- Debug functions
function DB_addBitsAndIp(dbi: InstructionDebugInfo; bits: Word; ip: Mword) return InstructionDebugInfo is
    variable res: InstructionDebugInfo := dbi;
begin
    -- pragma synthesis off
    res.bits := bits;
    res.adr := ip;
    res.str := work.Assembler.disasmWord(bits)(res.str'range);
    -- pragma synthesis on
    return res;
end function;

function DB_addSeqNum(dbi: InstructionDebugInfo; sn: Word) return InstructionDebugInfo is
    variable res: InstructionDebugInfo := dbi;
begin
    -- pragma synthesis off
    res.seqNum := sn;
    -- pragma synthesis on
    return res;
end function;

end LogicFront;
