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

function decodeInstructionNew(bits: Word) return InstructionState;

function getFrontEvent(ip, target: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1); partMask: std_logic_vector) return ControlPacket;

function getControlA(ip: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1); partMask: std_logic_vector; nWords: natural; hasBranch: std_logic) return ControlPacketArray;

function getEarlyEvent(cp: ControlPacket; target, predictedAddress: Mword; fetchStall, send: std_logic) return ControlPacket;

function partialMask(adr: Mword) return std_logic_vector;

function decodeGroup(ctrl: ControlPacket; fetchLine: WordArray(0 to PIPE_WIDTH-1); ip: Mword; full: std_logic_vector; nWords: natural) return BufferEntryArray;

function groupHasBranch(ea: BufferEntryArray) return std_logic;

-- DEBUG
function assignSeqNum(cpa: ControlPacketArray; seqNum: Word) return ControlPacketArray;
function assignSeqNum(ba: BufferEntryArray; seqNum: Word) return BufferEntryArray;

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


function decodeInstructionNew(bits: Word) return InstructionState is
	variable res: InstructionState := DEFAULT_INS_STATE;
    variable decodedIns: InstructionState := DEFAULT_INSTRUCTION_STATE;
    variable classInfo: InstructionClassInfo := DEFAULT_CLASS_INFO;
    variable op: SpecificOp := DEFAULT_SPECIFIC_OP;
    variable constantArgs: InstructionConstantArgs := DEFAULT_CONSTANT_ARGS;
    variable argSpec: InstructionArgSpec := DEFAULT_ARG_SPEC;
begin
	decodeFromWord(bits, classInfo, op, constantArgs, argSpec);

    res.typeInfo := classInfo;

    res.specificOperation := op;
    res.constantArgs := constantArgs;
    res.virtualArgSpec := argSpec;

    if res.specificOperation.subpipe = none then
        res.typeInfo.mainCluster := '0';
        res.typeInfo.secCluster := '0';

        res.controlInfo.specialAction := '1';

        res.controlInfo.hasException := bool2std(res.specificOperation.system = opUndef);--'1';
    end if;

    res.controlInfo.specialAction := not (res.typeInfo.mainCluster or res.typeInfo.secCluster);

	return res;
end function;

function decodeGroup(ctrl: ControlPacket; fetchLine: WordArray(0 to PIPE_WIDTH-1); ip: Mword; full: std_logic_vector; nWords: natural) return BufferEntryArray is
    variable res: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    variable res_N: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
    variable tmpIP: Mword := (others => '0');
    variable insState: InstructionState := DEFAULT_INS_STATE;
    variable anyBranch: std_logic := '0';
begin
    for i in 0 to FETCH_WIDTH-1 loop
        tmpIP := addInt(ip, 4*i);

        res(i).ins := decodeInstructionNew(fetchLine(i)); -- Here decoding!
        res(i).ins.dbInfo := DB_addBitsAndIp(ctrl.dbInfo, fetchLine(i), tmpIP);
    end loop;
    
    for i in 0 to FETCH_WIDTH-1 loop
        if full(i) = '1' and res(i).ins.typeInfo.branchIns = '1' then
           -- anyBranch := '1';
            --res(0).ins.controlInfo.firstBr := '1'; -- TMP, indicating that group has a branch
        end if;   
    end loop;

    for i in 0 to FETCH_WIDTH-1 loop
        res(i).full := bool2std(i < nWords);

        if res(i).full = '1' and res(i).ins.typeInfo.branchIns = '1' then
            anyBranch := '1';
            --res(0).ins.controlInfo.firstBr := '1'; -- TMP, indicating that group has a branch
        end if; 

        if res(i).full = '0' then
            res(i).ins.virtualArgSpec.intDestSel := '0';
            res(i).ins.virtualArgSpec.floatDestSel := '0';
        end if;          
    end loop;

    for i in 0 to FETCH_WIDTH-1 loop
        res_N(i).full := res(i).full;
        
        res_N(i).firstBr := res(i).ins.controlInfo.firstBr;
                            
        res_N(i).frontBranch := res(i).ins.controlInfo.frontBranch;
        res_N(i).confirmedBranch := res(i).ins.controlInfo.confirmedBranch;
        res_N(i).specialAction := res(i).ins.controlInfo.specialAction;
    
        res_N(i).classInfo := res(i).ins.typeInfo;
    
        res_N(i).specificOperation := res(i).ins.specificOperation;
        res_N(i).constantArgs := res(i).ins.constantArgs;
        res_N(i).argSpec := res(i).ins.virtualArgSpec;
        
        res_N(i).dbInfo := res(i).ins.dbInfo;
    end loop;

    res_N(0).firstBr := anyBranch;

    return res_N;
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


function getControlA(ip: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1); partMask: std_logic_vector; nWords: natural; hasBranch: std_logic) return ControlPacketArray is
	variable res: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
	variable tempIP, tempOffset, lastRes: Mword := (others => '0');
	variable targets, results: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	variable fullOut, branchIns, predictedTaken, uncondJump: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
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

        fullOut(i) := partMask(i);
        if partMask(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            if uncondJump(i) = '1' then -- CAREFUL: setting it here, so that if implementation treats is as NOP in Exec, it still gets this flag
                res(i).controlInfo.confirmedBranch := '1';
            end if;

            res(i).controlInfo.frontBranch := '1';
        end if;

       res(i).target := targets(i);
       res(i).nip := results(i);
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

function assignSeqNum(ba: BufferEntryArray; seqNum: Word) return BufferEntryArray is
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
