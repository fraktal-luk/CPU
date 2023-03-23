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

function getFrontEvent(ip, target: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1); partMask: std_logic_vector) return ControlPacket;

function getControlA(ip: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1);-- partMask: std_logic_vector;
                        nWords: natural; hasBranch: std_logic) return ControlPacketArray;

function getEarlyEvent(cp: ControlPacket; target, predictedAddress: Mword; fetchStall, send: std_logic) return ControlPacket;

function partialMask(adr: Mword) return std_logic_vector;

function decodeGroup(fetchLine: WordArray(0 to PIPE_WIDTH-1); nWords: natural; ip: Mword; ctrl: ControlPacket) return BufferEntryArray;

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
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    constant nSkip: natural := slv2u(adr(ALIGN_BITS-1 downto 2));
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := '1';
        if i < nSkip then
            res(i) := '0';
        end if;
    end loop;
    return res; 
end function;


function decodeGroup(fetchLine: WordArray(0 to PIPE_WIDTH-1); nWords: natural;
                     ip: Mword; ctrl: ControlPacket) -- These are DB only
return BufferEntryArray is
    variable res: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
    variable tmpIP: Mword := (others => '0');
    variable classInfo: InstructionClassInfo := DEFAULT_CLASS_INFO;
    variable op: SpecificOp := DEFAULT_SPECIFIC_OP;
    variable constantArgs: InstructionConstantArgs := DEFAULT_CONSTANT_ARGS;
    variable argSpec: InstructionArgSpec := DEFAULT_ARG_SPEC;
    variable anyBranch, isFull, specialAction: std_logic := '0';
begin
    for i in 0 to FETCH_WIDTH-1 loop
	    decodeFromWord(fetchLine(i), classInfo, op, constantArgs, argSpec);

        specialAction := not (classInfo.mainCluster or classInfo.secCluster);

        isFull := bool2std(i < nWords);

        if isFull = '1' and classInfo.branchIns = '1' then
            anyBranch := '1';
        end if; 

        if isFull = '0' then
            argSpec.intDestSel := '0';
            argSpec.floatDestSel := '0';
        end if;          

        res(i).full := isFull;

        res(i).specialAction := specialAction;

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
    for i in 0 to PIPE_WIDTH-1 loop
        if ea(i).full = '1' and ea(i).classInfo.branchIns = '1' then
            return '1';
        end if;   
    end loop;

    return '0';
end function;


function getFrontEvent(ip, target: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1); partMask: std_logic_vector) return ControlPacket is
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
	variable tempOffset, tempIP, tempTarget: Mword := (others => '0');
	variable branchIns, predictedTaken, uncondJump, longJump: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable regularJump, regJump: std_logic := '0';
begin

    -- Calculate target for each instruction, even if it's to be skipped
    for i in 0 to PIPE_WIDTH-1 loop        
        regularJump := '0';
        longJump(i) := '0';
        regJump := '0';

        -- jmp reg has src1
        -- jmp cnd has src0
        -- jmp lnk has dest
        -- jmp lng has none

        if isJumpLink(fetchLine(i)) = '1' then
            uncondJump(i) := '1';
            regularJump := '1';	
            predictedTaken(i) := '1';       -- jump link is unconditional
        elsif isJumpCond(fetchLine(i)) = '1' then
            regularJump := '1';				
            predictedTaken(i) := fetchLine(i)(20);		-- CAREFUL: temporary predicted taken iff backwards
        elsif isJumpLong(fetchLine(i)) = '1' then
            uncondJump(i) := '1';
            longJump(i) := '1';
            predictedTaken(i) := '1'; -- Long jump is unconditional (no space for register encoding!)
        elsif isJumpReg(fetchLine(i)) = '1' then   
            regJump := '1';
            predictedTaken(i) := '0';               -- TEMP: register jumps predicted not taken
        end if;

        branchIns(i) := regularJump or longJump(i) or regJump;
	end loop;

    res.tags.bqPointer := sn(3); -- CAREFUL, TMP: here used to indicate index of taken branch within fetch group

    -- Find if any branch predicted
    for i in 0 to FETCH_WIDTH-1 loop
        if partMask(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            if uncondJump(i) = '1' then -- CAREFUL: setting it here, so that if implementation treats is as NOP in Exec, it still gets this flag
                res.controlInfo.confirmedBranch := '1';
            end if;

            res.controlInfo.full := '1';
            res.controlInfo.frontBranch := '1';					

            if longJump(i) = '1' then
                tempOffset := (others => fetchLine(i)(25));
                tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
            else
                tempOffset := (others => fetchLine(i)(20));
                tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);
            end if;
            
            tempIP := ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(i*4, ALIGN_BITS);    -- !! Only for BQ, indirect
            tempTarget := add(tempIP, tempOffset);

            -- Here check if the next line from line predictor agrees with the target predicted now.
            --	If so, don't cause the event but set invalidation mask that next line will use.
            if tempTarget(MWORD_SIZE-1 downto ALIGN_BITS) /= target(MWORD_SIZE-1 downto ALIGN_BITS) then
                res.controlInfo.newEvent := '1';         -- !! Only for BQ
            end if;

            -- CAREFUL: When not using line predictor, branches predicted taken must always be done here 
            if not USE_LINE_PREDICTOR then
                res.controlInfo.newEvent := '1';         -- !! Only for BQ
            end if;

            res.target := tempTarget;

            res.tags.bqPointer := sn(i); -- TMP!

            exit;
        end if;
    end loop;

	return res;
end function;


function getEarlyEvent(cp: ControlPacket; target, predictedAddress: Mword; fetchStall, send: std_logic)
return ControlPacket is
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
begin
    if fetchStall = '1' then -- Need refetching
        res.target := predictedAddress;
        res.controlInfo.newEvent := '1';
        res.controlInfo.refetch := '1';
    elsif send = '1' then
        res.target := target;

        if cp.controlInfo.full = '1' and cp.controlInfo.frontBranch = '1' then
            res.controlInfo.newEvent := cp.controlInfo.newEvent; -- CAREFUL: event only if needs redirection, but break group at any taken jump 
            res.controlInfo.frontBranch := '1';
            res.target := cp.target; -- Correcting target within subsequent fetch line is still needed even if no redirection!               
        end if;
    end if;

    return res;
end function;


function getControlA(ip: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1);-- partMask: std_logic_vector;
                        nWords: natural; hasBranch: std_logic) return ControlPacketArray is
	variable res: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
	variable tempIP, tempOffset, lastRes: Mword := (others => '0');
	variable targets, results: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	variable full, branchIns, predictedTaken, uncondJump: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable regularJump, longJump, regJump: std_logic := '0';
begin

    -- Calculate target for each instruction, even if it's to be skipped
    for i in 0 to FETCH_WIDTH-1 loop        
        regularJump := '0';
        longJump := '0';
        regJump := '0';

        -- jmp reg has src1
        -- jmp cnd has src0
        -- jmp lnk has dest
        -- jmp lng has none

        if isJumpLink(fetchLine(i)) = '1' then
            uncondJump(i) := '1';
            regularJump := '1';	
            predictedTaken(i) := '1';       -- jump link is unconditional
        elsif isJumpCond(fetchLine(i)) = '1' then
            regularJump := '1';				
            predictedTaken(i) := fetchLine(i)(20);		-- CAREFUL: temporary predicted taken iff backwards
        elsif isJumpLong(fetchLine(i)) = '1' then
            uncondJump(i) := '1';
            longJump := '1';
            predictedTaken(i) := '1'; -- Long jump is unconditional (no space for register encoding!)
        elsif isJumpReg(fetchLine(i)) = '1' then   
            regJump := '1';
            predictedTaken(i) := '0';               -- TEMP: register jumps predicted not taken
        end if;

        if longJump = '1' then
            tempOffset := (others => fetchLine(i)(25));
            tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
        else
            tempOffset := (others => fetchLine(i)(20));
            tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);
        end if;

        branchIns(i) := regularJump or longJump or regJump;

        tempIP := addInt(ip, 4*i);
        targets(i) := add(tempIP, tempOffset);
	    results(i) := addInt(ip, 4*(i + 1));

        res(i).target := targets(i);
        res(i).nip := results(i);

        full(i) := bool2std(i < nWords);

        if full(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            if uncondJump(i) = '1' then -- CAREFUL: setting it here, so that if implementation treats is as NOP in Exec, it still gets this flag
                res(i).controlInfo.confirmedBranch := '1';
            end if;

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
