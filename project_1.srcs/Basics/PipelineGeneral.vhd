
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;


package PipelineGeneral is


type PhysicalSubpipe is (ALU, Mem, FP, StoreDataInt, StoreDataFloat);


type ExecResult is record
    full: std_logic;
    tag: InsTag;
    dest: PhysName;
    value: Mword;
end record;

constant DEFAULT_EXEC_RESULT: ExecResult := ('0', tag => (others => '0'), dest => (others => '0'), value => (others => '0'));

type ExecResultArray is array(integer range <>) of ExecResult;

function makeExecResult(isl: InstructionSlot; full: std_logic) return ExecResult;
function makeExecResult(isl: SchedulerEntrySlot; full: std_logic) return ExecResult;


type EventState is record
    lateEvent: std_logic;
    execEvent: std_logic;
    execCausing: InstructionState;
    preExecCausing: InstructionState;
end record;

constant DEFAULT_EVENT_STATE: EventState := ('0', '0', DEFAULT_INSTRUCTION_STATE, DEFAULT_INSTRUCTION_STATE);

type IssueQueueSignals is record
    sending: std_logic;
    cancelled: std_logic;
    ready: std_logic;
    empty: std_logic;
end record;

type ForwardingInfo is record
	nextTagsM3:	PhysNameArray(0 to 2);
	nextTagsM2:	PhysNameArray(0 to 2);
	nextTagsM1: PhysNameArray(0 to 2);
	tags0: PhysNameArray(0 to 2);
	tags1: PhysNameArray(0 to 2);
	values0: MwordArray(0 to 2);
	values1: MwordArray(0 to 2);	
end record;

type ForwardingMatches is record
    -- src0
    a0cmpM3: std_logic_vector(0 to 2);
    a0cmpM2: std_logic_vector(0 to 2);
    a0cmpM1: std_logic_vector(0 to 2);
    a0cmp1: std_logic_vector(0 to 2);    
	a0cmp0: std_logic_vector(0 to 2);
    
    -- src1
	a1cmpM3: std_logic_vector(0 to 2);
	a1cmpM2: std_logic_vector(0 to 2);
	a1cmpM1: std_logic_vector(0 to 2);
	a1cmp1: std_logic_vector(0 to 2);	    
	a1cmp0: std_logic_vector(0 to 2);
end record;

type ForwardingMatchesArray is array(integer range <>) of ForwardingMatches; 


constant DEFAULT_FORWARDING_INFO: ForwardingInfo := (
	nextTagsM3 => (others => (others => '0')),
	nextTagsM2 => (others => (others => '0')),
	nextTagsM1 => (others => (others => '0')),
    tags0 => (others => (others => '0')),
    tags1 => (others => (others => '0')),
    values0 => (others => (others => '0')),
    values1 => (others => (others => '0'))
);

constant DEFAULT_FORWARDING_MATCHES: ForwardingMatches := (
    others => (others => '0')
);


    type DependencySpec is array(0 to 2) of std_logic_vector(0 to PIPE_WIDTH-1); 
    type DependencyVec is array(0 to PIPE_WIDTH-1) of DependencySpec;
    
    constant DEFAULT_DEP_VEC: DependencyVec := (others => (others => (others => '0')));
    

    type RenameInfo is record
        destSel: std_logic;
        destSelFP: std_logic;
        virtualDest: RegName;
        physicalDest: PhysName;
        sourceSel: std_logic_vector(0 to 2);
        sourceConst: std_logic_vector(0 to 2); 
        virtualSources: RegNameArray(0 to 2);
        physicalSources: PhysNameArray(0 to 2);
        physicalSourcesStable: PhysNameArray(0 to 2);
        physicalSourcesNew: PhysNameArray(0 to 2); -- sources in case of of group dependency        
        deps: DependencySpec;
        sourcesStable: std_logic_vector(0 to 2); -- true if source is from stable map - always ready
        sourcesNew: std_logic_vector(0 to 2);   -- true if group dependency
        sourcesReady: std_logic_vector(0 to 2); -- ready as read from ReadyTable based on NewestMap 
    end record;

    type RenameInfoArray is array(natural range <>) of RenameInfo;

    constant DEFAULT_RENAME_INFO: RenameInfo := (
        destSel => '0',
        destSelFP => '0',
        virtualDest => (others => '0'),
        physicalDest => (others => '0'),
        sourceSel => (others => '0'),
        sourceConst => (others => '0'),
        virtualSources => (others => (others => '0')),
        physicalSources => (others => (others => '0')),
        physicalSourcesStable => (others => (others => '0')),
        physicalSourcesNew => (others => (others => '0')),
        deps => (others => (others => '0')),
        sourcesStable => (others => '0'),
        sourcesNew => (others => '0'),
        sourcesReady => (others => '0')        
    );

    function mergeRenameInfoFP(intArgs, floatArgs: RenameInfoArray) return RenameInfoArray;

        function TMP_getPhysicalArgsNew(ri: RenameInfoArray) return PhysNameArray;


function adjustStage(content: InstructionSlotArray) return InstructionSlotArray;

function compactMask(vec: std_logic_vector) return std_logic_vector; -- WARNING: only for 4 elements
function getSelector(mr, mi: std_logic_vector(0 to 2)) return std_logic_vector;

function getNewElem(remv: std_logic_vector; newContent: InstructionSlotArray) return InstructionSlot; -- UNUSED 
function getNewElemSch(remv: std_logic_vector; newContent: SchedulerEntrySlotArray) return SchedulerEntrySlot;

-- general InstructionState handling 
function setInstructionIP(ins: InstructionState; ip: Mword) return InstructionState; -- UNUSED
function setInstructionTarget(ins: InstructionState; target: Mword) return InstructionState; -- UNUSED
function setInstructionResult(ins: InstructionState; result: Mword) return InstructionState;
--
-- dest handling
function clearFloatDest(insArr: InstructionSlotArray) return InstructionSlotArray;
function clearIntDest(insArr: InstructionSlotArray) return InstructionSlotArray;
function mergePhysDests(insS0, insS1: InstructionSlot) return InstructionSlot;

function extractFullMask(queueContent: InstructionSlotArray) return std_logic_vector;
function extractFullMask(queueContent: SchedulerEntrySlotArray) return std_logic_vector;

function extractData(queueContent: InstructionSlotArray) return InstructionStateArray;
function extractData(queueContent: SchedulerEntrySlotArray) return InstructionStateArray;

function setFullMask(insVec: InstructionSlotArray; mask: std_logic_vector) return InstructionSlotArray;

function makeSlotArray(ia: InstructionStateArray; fm: std_logic_vector) return InstructionSlotArray;

function compareIndBefore(tagA, tagB: SmallNumber; PTR_SIZE: natural) return std_logic;

-- UNUSED?
function getKillMask(content: InstructionStateArray; causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector;


-- TODO: deprecate?
function getExceptionMask(insVec: InstructionSlotArray) return std_logic_vector;

function getCausingMask(insVec: InstructionSlotArray) return std_logic_vector;
function getKilledMask(insVec: InstructionSlotArray) return std_logic_vector;
function getIgnoredMask(insVec: InstructionSlotArray) return std_logic_vector;

function stageArrayNext(livingContent, newContent: InstructionSlotArray; full, sending, receiving, kill, keepDest: std_logic)
return InstructionSlotArray;

-- general op tag handling
function getTagHigh(tag: std_logic_vector) return std_logic_vector;
function getTagLow(tag: std_logic_vector) return std_logic_vector;
function getTagHighSN(tag: InsTag) return SmallNumber;
function getTagLowSN(tag: InsTag) return SmallNumber;	
function clearTagLow(tag: std_logic_vector) return std_logic_vector;	
function clearTagHigh(tag: std_logic_vector) return std_logic_vector;	
function alignAddress(adr: std_logic_vector) return std_logic_vector;
function clearLowBits(vec: std_logic_vector; n: integer) return std_logic_vector;
function getLowBits(vec: std_logic_vector; n: integer) return std_logic_vector;

function compareTagBefore(tagA, tagB: InsTag) return std_logic;
function compareTagAfter(tagA, tagB: InsTag) return std_logic;

function restoreRenameIndex(content: InstructionSlotArray) return InstructionSlotArray;

function getSpecialActionSlot(insVec: InstructionSlotArray) return InstructionSlot;

function getAddressIncrement(ins: InstructionState) return Mword;

function hasSyncEvent(ins: InstructionState) return std_logic;

function isBranchIns(ins: InstructionState) return std_logic;
function isLoadOp(ins: InstructionState) return std_logic;
function isStoreOp(ins: InstructionState) return std_logic;
function isLoadMemOp(ins: InstructionState) return std_logic;
function isStoreMemOp(ins: InstructionState) return std_logic;
function isLoadSysOp(ins: InstructionState) return std_logic;
function isStoreSysOp(ins: InstructionState) return std_logic;

-- sorting subpipelines
function getBranchMask(insVec: InstructionSlotArray) return std_logic_vector;
function getLoadMask(insVec: InstructionSlotArray) return std_logic_vector;
function getStoreMask(insVec: InstructionSlotArray) return std_logic_vector;
function getFloatStoreMask(insVec, insVecF: InstructionSlotArray) return std_logic_vector;
function getAluMask(insVec: InstructionSlotArray) return std_logic_vector;
function getFpuMask(insVec: InstructionSlotArray) return std_logic_vector;
function getMemMask(insVec: InstructionSlotArray) return std_logic_vector;


        function getBranchMask1(insVec: InstructionSlotArray) return std_logic_vector;
        function getLoadMask1(insVec: InstructionSlotArray) return std_logic_vector;
        function getStoreMask1(insVec: InstructionSlotArray) return std_logic_vector;
        function getIntStoreMask1(insVec: InstructionSlotArray) return std_logic_vector;       
        function getFloatStoreMask1(insVec: InstructionSlotArray) return std_logic_vector;
        function getAluMask1(insVec: InstructionSlotArray) return std_logic_vector;
        function getFpMask1(insVec: InstructionSlotArray) return std_logic_vector;
        function getMemMask1(insVec: InstructionSlotArray) return std_logic_vector;


function TMP_recodeMem(insVec: InstructionSlotArray) return InstructionSlotArray;
function TMP_recodeFP(insVec: InstructionSlotArray) return InstructionSlotArray;
function TMP_recodeALU(insVec: InstructionSlotArray) return InstructionSlotArray;

function prepareForStoreValueIQ(insVec: InstructionSlotArray) return InstructionSlotArray;
function prepareForStoreValueFloatIQ(--insVecInt,
                                     insVecFloat: InstructionSlotArray) return InstructionSlotArray;

--function removeArg2(insVec: InstructionStateArray) return InstructionStateArray;

function TMP_removeArg2(insVec: InstructionSlotArray) return InstructionSlotArray;

function removeArg2(ria: RenameInfoArray) return RenameInfoArray;
function useStoreArg2(ria: RenameInfoArray) return RenameInfoArray;

function updateArgStates(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector;
function updateArgStatesFloat(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector;

function replaceDests(insVec: InstructionSlotArray; ria: RenameInfoArray) return InstructionSlotArray;


function clearRawInfo(ins: InstructionState) return InstructionState;      -- ip, bits; this is raw program data 
function clearFollowInfo(ins: InstructionState) return InstructionState;  -- result, target; they are about what follows
function clearAbstractInfo(ins: InstructionState) return InstructionState; -- ip, bits, result, target; because they are independent of implementation -> abstract

function clearRawInfo(ia: InstructionStateArray) return InstructionStateArray;
function clearFollowInfo(ia: InstructionStateArray) return InstructionStateArray;
function clearAbstractInfo(ia: InstructionStateArray) return InstructionStateArray;

function clearRawInfo(ins: InstructionSlot) return InstructionSlot;
function clearFollowInfo(ins: InstructionSlot) return InstructionSlot;
function clearAbstractInfo(ins: InstructionSlot) return InstructionSlot;

function clearRawInfo(ia: InstructionSlotArray) return InstructionSlotArray;
function clearFollowInfo(ia: InstructionSlotArray) return InstructionSlotArray;
function clearAbstractInfo(ia: InstructionSlotArray) return InstructionSlotArray;

function clearDbCounters(ins: InstructionState) return InstructionState;      -- ip, bits; this is raw program data 
function clearDbCounters(isl: InstructionSlot) return InstructionSlot;

function clearDbCausing(ins: InstructionState) return InstructionState;

-- For setting dest sel flags in stages after ROB!
function setDestFlags(insVec: InstructionSlotArray) return InstructionSlotArray;


function getQueueEmpty(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return std_logic;
function getNumFull(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber;

function mergeFP(dataInt: InstructionSlotArray; dataFloat: InstructionSlotArray) return InstructionSlotArray; 


function setPhysSources(insVec: InstructionSlotArray; newPhysSources: PhysNameArray; newestSelector, depVec: std_logic_vector) return InstructionSlotArray;


function buildForwardingNetwork(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo;

function buildForwardingNetworkFP(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                  s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                  s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo;

end package;



package body PipelineGeneral is


function mergeRenameInfoFP(intArgs, floatArgs: RenameInfoArray) return RenameInfoArray is
    variable res: RenameInfoArray(intArgs'range) := intArgs;
begin
    for i in res'range loop
        if floatArgs(i).destSelFP = '1' then        
            res(i).destSel := '1';
            res(i).destSelFP := '1';
            res(i).virtualDest := floatArgs(i).virtualDest;
            res(i).physicalDest := floatArgs(i).physicalDest;
        end if;
    end loop;
    return res;
end function;


        function TMP_getPhysicalArgsNew(ri: RenameInfoArray) return PhysNameArray is
            variable res: PhysNameArray(0 to 3*PIPE_WIDTH-1);
        begin
            res(0 to 2) := ri(0).physicalSourcesNew;-- & ri(1).physicalSourcesNew & ri(2).physicalSourcesNew & ri(3).physicalSourcesNew;
            res(3 to 5) := ri(1).physicalSourcesNew;
            res(6 to 8) := ri(2).physicalSourcesNew;
            res(9 to 11) := ri(3).physicalSourcesNew;
            return res;
        end function;



-- TODO: move to LogicFront? 
function adjustStage(content: InstructionSlotArray)
return InstructionSlotArray is
    constant LEN: positive := content'length;
    variable res: InstructionSlotArray(0 to LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    variable contentExt: InstructionSlotArray(0 to 2*LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    variable fullMask: std_logic_vector(0 to LEN-1) := (others => '0');
    variable nShift, j: integer := 0;
begin
    contentExt(0 to LEN-1) := content;
    contentExt(LEN to 2*LEN-1) := (others => ('0', content(LEN-1).ins)); -- leave it instead of rotating
    fullMask := extractFullMask(content);
    nShift := getFirstOnePosition(fullMask);
    if isNonzero(fullMask) = '0' then
        nShift := 0;
    end if; 
    
    for i in 0 to LEN-1 loop
        res(i) := contentExt(nShift + i);
        
        if res(i).full = '0' then
            res(i).ins.virtualArgSpec.intDestSel := '0';
            res(i).ins.virtualArgSpec.floatDestSel := '0';
        end if;          
    end loop;
      
        -- TMP!
        res(0).ins.controlInfo.firstBr := content(0).ins.controlInfo.firstBr;
        
    return res;
end function;


function clearRawInfo(ins: InstructionState) return InstructionState is
    variable res: InstructionState := ins;
begin
    res.ip := (others => '0');
    res.bits := (others => '0');
    return res;
end function;

function clearFollowInfo(ins: InstructionState) return InstructionState is
    variable res: InstructionState := ins;
begin
    res.target := (others => '0');
    res.result := (others => '0');
    return res;
end function;

function clearAbstractInfo(ins: InstructionState) return InstructionState is
    variable res: InstructionState := ins;
begin
    res.ip := (others => '0');
    res.bits := (others => '0');
    res.target := (others => '0');
    res.result := (others => '0');
    return res;
end function;

function clearRawInfo(ia: InstructionStateArray) return InstructionStateArray is
   variable res: InstructionStateArray(ia'range) := ia;
begin
    for i in ia'range loop
        res(i) := clearRawInfo(res(i));
    end loop;
    return res;
end function;

function clearFollowInfo(ia: InstructionStateArray) return InstructionStateArray is
   variable res: InstructionStateArray(ia'range) := ia;
begin
    for i in ia'range loop
        res(i) := clearRawInfo(res(i));
    end loop;
    return res;
end function;

function clearAbstractInfo(ia: InstructionStateArray) return InstructionStateArray is
   variable res: InstructionStateArray(ia'range) := ia;
begin
    for i in ia'range loop
        res(i) := clearAbstractInfo(res(i));
    end loop;
    return res;
end function;


function clearDbCounters(ins: InstructionState) return InstructionState is
    variable res: InstructionState := ins;
begin
    res.tags.fetchCtr := (others => '0');
    res.tags.decodeCtr := (others => '0');
    res.tags.renameCtr := (others => '0');
    res.tags.commitCtr := (others => '0');    
    return res;
end function;

function clearDbCounters(isl: InstructionSlot) return InstructionSlot is
    variable res: InstructionSlot := isl;
begin   
    res.ins := clearDbCounters(res.ins);
    return res;
end function;


function clearRawInfo(ins: InstructionSlot) return InstructionSlot is
    variable res: InstructionSlot := ins;
begin
    res.ins.ip := (others => '0');
    res.ins.bits := (others => '0');
    return res;
end function;

function clearFollowInfo(ins: InstructionSlot) return InstructionSlot is
    variable res: InstructionSlot := ins;
begin
    res.ins.target := (others => '0');
    res.ins.result := (others => '0');
    return res;
end function;

function clearAbstractInfo(ins: InstructionSlot) return InstructionSlot is
    variable res: InstructionSlot := ins;
begin
    res.ins.ip := (others => '0');
    res.ins.bits := (others => '0');
    res.ins.target := (others => '0');
    res.ins.result := (others => '0');
    return res;
end function;

function clearRawInfo(ia: InstructionSlotArray) return InstructionSlotArray is
   variable res: InstructionSlotArray(ia'range) := ia;
begin
    for i in ia'range loop
        res(i) := clearRawInfo(res(i));
    end loop;
    return res;
end function;

function clearFollowInfo(ia: InstructionSlotArray) return InstructionSlotArray is
   variable res: InstructionSlotArray(ia'range) := ia;
begin
    for i in ia'range loop
        res(i) := clearRawInfo(res(i));
    end loop;
    return res;
end function;

function clearAbstractInfo(ia: InstructionSlotArray) return InstructionSlotArray is
   variable res: InstructionSlotArray(ia'range) := ia;
begin
    for i in ia'range loop
        res(i) := clearAbstractInfo(res(i));
    end loop;
    return res;
end function;

-----

function compactMask(vec: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to 3) := (others => '0');
    variable vec4: std_logic_vector(0 to 3) := vec;
begin
    case vec4 is
        when "0000" =>
            res := "0000";
        when "1111" =>
            res := "1111";
        when "1000" | "0100" | "0010" | "0001" =>
            res := "1000";
        when "0111" | "1011" | "1101" | "1110" =>
            res := "1110";
        when others =>
            res := "1100";
    end case;
    
    return res;
end function;

-- REFAC
function getSelector(mr, mi: std_logic_vector(0 to 2)) return std_logic_vector is
    variable res: std_logic_vector(1 downto 0) := "00";
    variable m6: std_logic_vector(0 to 5) := mr & mi;
    variable n0: integer := 0;
begin
    n0 := 6 - countOnes(m6);
    case m6 is
        --when "111000" =>
        --    res := "11";
        when "111001" => 
            res := "10";
        when "111010" | "111011" => 
            res := "01";
        when "111100" | "111101" | "111110" | "111111" => 
            res := "00";
 
        --when "110000" => 
        --    res := "11";
        --when "110001" | "110010" => 
        --    res := "11";
        when "110011" | "110101" => 
            res := "10";
        when "110110" | "110111" =>
            res := "01";

        when "100111" => 
            res := "10";

        when others =>
            res := "11";
    end case;
    return res;
end function;

-- UNUSED?
function getNewElem(remv: std_logic_vector; newContent: InstructionSlotArray) return InstructionSlot is
    variable res: InstructionSlot := newContent(0);
    variable inputMask: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
    variable sel: std_logic_vector(1 downto 0) := "00";
    variable remVec: std_logic_vector(0 to 2) := remv;               
begin
    inputMask := extractFullMask(newContent);
    sel := getSelector(remVec, inputMask(0 to 2));
    res := newContent(slv2u(sel));        
    return res;    
end function;

-- UNUSED?
function getNewElemSch(remv: std_logic_vector; newContent: SchedulerEntrySlotArray)
return SchedulerEntrySlot is
    variable res: SchedulerEntrySlot := newContent(0);
    variable inputMask: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
    variable sel: std_logic_vector(1 downto 0) := "00";
    variable remVec: std_logic_vector(0 to 2) := remv;               
begin
    inputMask := extractFullMask(newContent);
    sel := getSelector(remVec, inputMask(0 to 2));
    res := newContent(slv2u(sel));        
    return res;    
end function;

function setInstructionIP(ins: InstructionState; ip: Mword) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.ip := ip;
	return res;
end function;

function setInstructionTarget(ins: InstructionState; target: Mword) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.target := target;
	return res;
end function;

function setInstructionResult(ins: InstructionState; result: Mword) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.result := result;
	return res;
end function;

function getAddressIncrement(ins: InstructionState) return Mword is
	variable res: Mword := (others => '0');
begin
	if ins.classInfo.short = '1' then
		res(1) := '1'; -- 2
	else
		res(2) := '1'; -- 4
	end if;
	return res;
end function;


function compareTagBefore(tagA, tagB: InsTag) return std_logic is
	variable tC: InsTag := (others => '0');
begin
	tC := sub(tagA, tagB);
	return tC(TAG_SIZE-1);
end function;

function compareTagAfter(tagA, tagB: InsTag) return std_logic is
	variable wA, wB, wC: word := (others => '0');
begin
	return compareTagBefore(tagB, tagA);
end function;

function extractFullMask(queueContent: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to queueContent'length-1) := (others => '0');
begin
	for i in res'range loop
		res(i) := queueContent(i).full;
	end loop;
	return res;
end function;

function extractFullMask(queueContent: SchedulerEntrySlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to queueContent'length-1) := (others => '0');
begin
	for i in res'range loop
		res(i) := queueContent(i).full;
	end loop;
	return res;
end function;

function extractData(queueContent: InstructionSlotArray) return InstructionStateArray is
	variable res: InstructionStateArray(0 to queueContent'length-1) := (others => DEFAULT_INS_STATE);
begin
	for i in res'range loop
		res(i) := queueContent(i).ins;
	end loop;
	return res;
end function;


function extractData(queueContent: SchedulerEntrySlotArray) return InstructionStateArray is
	variable res: InstructionStateArray(0 to queueContent'length-1) := (others => DEFAULT_INS_STATE);
begin
	for i in res'range loop
		res(i) := queueContent(i).ins;
	end loop;
	return res;
end function;

function compareIndBefore(tagA, tagB: SmallNumber; PTR_SIZE: natural) return std_logic is
    variable tC: SmallNumber := (others => '0');
begin
    tC := sub(tagA, tagB);
    return tC(PTR_SIZE-1);
end function; 

function getKillMask(content: InstructionStateArray; causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector is
	variable res: std_logic_vector(0 to content'length-1);
begin
	for i in 0 to content'length-1 loop
		res(i) := (compareTagBefore(causing.tags.renameIndex, content(i).tags.renameIndex) and execEventSig)
		   or lateEventSig;
	end loop;
	return res;
end function;


function stageArrayNext(livingContent, newContent: InstructionSlotArray; full, sending, receiving, kill, keepDest: std_logic)
return InstructionSlotArray is 
    constant LEN: natural := livingContent'length;
	variable res: InstructionSlotArray(0 to LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
begin
	res := livingContent;
	if kill = '1' then
		for i in 0 to LEN-1 loop
			res(i).full := '0';
		end loop;
	end if;
		
	if receiving = '1' then -- take full
		res := newContent;
	elsif sending = '1' or full = '0' then -- take empty
		-- CAREFUL: clearing result tags for empty slots
		for i in 0 to LEN-1 loop
		    if keepDest = '0' then
                if CLEAR_DEST_SEL_ON_EMPTY then
                   res(i).ins.physicalArgSpec.intDestSel := '0';
                   res(i).ins.virtualArgSpec.floatDestSel := '0';
                end if;		    
			    res(i).ins.physicalArgSpec.dest := (others => '0');
			                                       --(others => '1');
			end if;
			res(i).ins.controlInfo.newEvent := '0';
		end loop;
		for i in 0 to LEN-1 loop
			res(i).full := '0';
		end loop;
	end if;			
			
	return res;
end function;


function getTagHigh(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(tag'high-LOG2_PIPE_WIDTH downto 0) := (others => '0');
begin
	res := tag(tag'high downto LOG2_PIPE_WIDTH);
	return res;
end function;

function getTagLow(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(LOG2_PIPE_WIDTH-1 downto 0) := (others => '0');
begin
	res := tag(LOG2_PIPE_WIDTH-1 downto 0);
	return res;
end function;

function getTagHighSN(tag: InsTag) return SmallNumber is
	variable res: SmallNumber := (others => '0');
begin
	res(TAG_SIZE-1-LOG2_PIPE_WIDTH downto 0) := tag(TAG_SIZE-1 downto LOG2_PIPE_WIDTH);
	return res;
end function;

function getTagLowSN(tag: InsTag) return SmallNumber is
	variable res: SmallNumber := (others => '0');
begin
	res(LOG2_PIPE_WIDTH-1 downto 0) := tag(LOG2_PIPE_WIDTH-1 downto 0);
	return res;
end function;


function clearTagLow(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(tag'high downto 0) := (others => '0');
begin
	res := tag;
	res(LOG2_PIPE_WIDTH-1 downto 0) := (others => '0');
	return res;
end function;	

function clearTagHigh(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(tag'high downto 0) := (others => '0');
begin
	res := tag;
	res(tag'high downto LOG2_PIPE_WIDTH) := (others => '0');
	return res;
end function;

function alignAddress(adr: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(adr'high downto 0) := (others => '0');
begin
	res := adr;
	res(ALIGN_BITS-1 downto 0) := (others => '0');
	return res;
end function;

function clearLowBits(vec: std_logic_vector; n: integer) return std_logic_vector is
	variable res: std_logic_vector(vec'high downto 0) := (others => '0');
begin
	res := vec;
	res(n-1 downto 0) := (others => '0');
	return res;
end function;

function getLowBits(vec: std_logic_vector; n: integer) return std_logic_vector is
	variable res: std_logic_vector(n-1 downto 0) := (others => '0');
begin
	res(n-1 downto 0) := vec(n-1 downto 0);
	return res;
end function;


function getExceptionMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others=>'0');
begin
	for i in insVec'range loop
		res(i) := insVec(i).ins.controlInfo.hasException
		       or insVec(i).ins.controlInfo.specialAction; -- CAREFUL: what if special actions are allowed to write registers?
	end loop;			
	return res;
end function;



function getCausingMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others=>'0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.controlInfo.causing; -- CAREFUL: what if special actions are allowed to write registers?
    end loop;            
    return res;
end function;


function getKilledMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others=>'0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.controlInfo.killed;
    end loop;            
    return res;
end function;

function getIgnoredMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others=>'0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).ins.controlInfo.ignored;
    end loop;            
    return res;
end function;


function getBranchMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		res(i) := insVec(i).full and insVec(i).ins.classInfo.branchIns;
	end loop;	
	return res;
end function;


-- Base for implementing subpipe selection
function getSubpipeMask(insVec: InstructionSlotArray; subpipe: SubpipeType) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others => '0');
begin
    for i in insVec'range loop
        res(i) := insVec(i).full and bool2std(insVec(i).ins.specificOperation.subpipe = subpipe);
    end loop;
    return res;
end function;


function getLoadMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 		insVec(i).full = '1'
			and insVec(i).ins.specificOperation.subpipe = Mem 
			and (insVec(i).ins.specificOperation.memory = opLoad or insVec(i).ins.specificOperation.memory = opLoadSys) 
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;

function getStoreMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 	  insVec(i).full = '1' and insVec(i).ins.specificOperation.subpipe = Mem 
		      and (insVec(i).ins.specificOperation.memory = opStore or insVec(i).ins.specificOperation.memory = opStoreSys)			
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;

function getFloatStoreMask(insVec, insVecF: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 	  insVecF(i).full = '1' and insVec(i).ins.specificOperation.subpipe = Mem 
		      and (insVec(i).ins.specificOperation.memory = opStore or insVec(i).ins.specificOperation.memory = opStoreSys)			
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;


function getAluMask(insVec: InstructionSlotArray) return std_logic_vector is
begin
	return getSubpipeMask(insVec, ALU);
end function;

function getFpuMask(insVec: InstructionSlotArray) return std_logic_vector is
begin
	return getSubpipeMask(insVec, FP);
end function;

function getMemMask(insVec: InstructionSlotArray) return std_logic_vector is
begin
	return getSubpipeMask(insVec, Mem);
end function;


        
        function getBranchMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.branchIns;
            end loop;	
            return res;
        end function;
        
        function getLoadMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.useLQ;
            end loop;
            
            return res;
        end function;
        
        function getStoreMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.useSQ;
            end loop;
            
            return res;
        end function;


        function getIntStoreMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.storeInt;
            end loop;
            
            return res;
        end function;
        
        function getFloatStoreMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.storeFP;
            end loop;
            
            return res;
        end function;
        
        
        function getAluMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.useAlu;
            end loop;
            
            return res;
        end function;
        
        function getFpMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.useFP;
            end loop;
            
            return res;
        end function;
        
        function getMemMask1(insVec: InstructionSlotArray) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i) := insVec(i).ins.classInfo.useMem;
            end loop;
            
            return res;
        end function;






function setFullMask(insVec: InstructionSlotArray; mask: std_logic_vector) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        res(i).full := mask(i);
    end loop;
    
    return res;
end function;

function makeSlotArray(ia: InstructionStateArray; fm: std_logic_vector) return InstructionSlotArray is
    variable res: InstructionSlotArray(ia'range) := (others => DEFAULT_INS_SLOT);
begin
    for i in res'range loop
        res(i) := (fm(i), ia(i));
    end loop;
    return res;
end function;




function prepareForStoreValueIQ(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
begin
    for i in 0 to PIPE_WIDTH-1 loop    
        res(i).ins.constantArgs.immSel := '0';

        res(i).ins.virtualArgSpec := DEFAULT_ARG_SPEC;
        res(i).ins.physicalArgSpec := DEFAULT_ARG_SPEC;               
        
        res(i).ins.virtualArgSpec.intArgSel(0) := insVec(i).ins.virtualArgSpec.intArgSel(2);
        res(i).ins.virtualArgSpec.args(0) := insVec(i).ins.virtualArgSpec.args(2);

        res(i).ins.physicalArgSpec.intArgSel(0) := insVec(i).ins.physicalArgSpec.intArgSel(2);
        res(i).ins.physicalArgSpec.args(0) := insVec(i).ins.physicalArgSpec.args(2);                                             
    end loop;
    
    return res;
end function;


function prepareForStoreValueFloatIQ(insVecFloat: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVecFloat;
begin
    for i in 0 to PIPE_WIDTH-1 loop    
        res(i).ins.constantArgs.immSel := '0';
        
        res(i).ins.virtualArgSpec := DEFAULT_ARG_SPEC;
        res(i).ins.physicalArgSpec := DEFAULT_ARG_SPEC;
              
        res(i).ins.virtualArgSpec.floatArgSel(0) := insVecFloat(i).ins.virtualArgSpec.floatArgSel(2);
        res(i).ins.virtualArgSpec.args(0) := insVecFloat(i).ins.virtualArgSpec.args(2);
               
        res(i).ins.physicalArgSpec.floatArgSel(0) := insVecFloat(i).ins.physicalArgSpec.floatArgSel(2);
        res(i).ins.physicalArgSpec.args(0) := insVecFloat(i).ins.physicalArgSpec.args(2);                                            
    end loop;
    
    return res;
end function;


--function removeArg2(insVec: InstructionStateArray) return InstructionStateArray is
--    variable res: InstructionStateArray(0 to PIPE_WIDTH-1) := insVec;
--begin
--    for i in 0 to PIPE_WIDTH-1 loop
--        res(i).virtualArgSpec.intArgSel(2) := '0';
--        res(i).virtualArgSpec.args(2) := (others => '0');
        
--        res(i).physicalArgSpec.intArgSel(2) := '0';
--        res(i).physicalArgSpec.args(2) := (others => '0');                                                
--    end loop;
    
--    return res;
--end function;

function TMP_removeArg2(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i).ins.virtualArgSpec.intArgSel(2) := '0';
        res(i).ins.virtualArgSpec.args(2) := (others => '0');
        
        res(i).ins.physicalArgSpec.intArgSel(2) := '0';
        res(i).ins.physicalArgSpec.args(2) := (others => '0');                                                
    end loop;
    
    return res;
end function;


function removeArg2(ria: RenameInfoArray) return RenameInfoArray is
    variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := ria;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i).sourceSel(2) := '0';
        res(i).sourceConst(2) := '1'; 
        res(i).virtualSources(2) := (others => '0');
        res(i).physicalSources(2) := (others => '0');
        res(i).physicalSourcesStable(2) := (others => '0');
        res(i).physicalSourcesNew(2) := (others => '0'); -- sources in case of of group dependency        
        res(i).deps(2) := (others => '0');
        res(i).sourcesStable(2) := '1'; -- true if source is from stable map - always ready
        res(i).sourcesNew(2) := '0';   -- true if group dependency
        res(i).sourcesReady(2) := '0'; -- ready as read from ReadyTable based on NewestMap 
    end loop;
    
    return res;
end function;

function useStoreArg2(ria: RenameInfoArray) return RenameInfoArray is
    variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := ria;
begin
    for i in 0 to PIPE_WIDTH-1 loop
            res(i).destSel := '0';
            res(i).destSelFP := '0';
            res(i).physicalDest := (others => '0');
    
        res(i).sourceSel(0) := res(i).sourceSel(2);
        res(i).sourceConst(0) := res(i).sourceConst(2); 
        res(i).virtualSources(0) := res(i).virtualSources(2);
        res(i).physicalSources(0) := res(i).physicalSources(2);
        res(i).physicalSourcesStable(0) := res(i).physicalSourcesStable(2);
        res(i).physicalSourcesNew(0) := res(i).physicalSourcesNew(2); -- sources in case of of group dependency        
        res(i).deps(0) := res(i).deps(2);
        res(i).sourcesStable(0) := res(i).sourcesStable(2); -- true if source is from stable map - always ready
        res(i).sourcesNew(0) := res(i).sourcesNew(2);   -- true if group dependency
        res(i).sourcesReady(0) := res(i).sourcesReady(2); -- ready as read from ReadyTable based on NewestMap

        res(i).sourceSel(1) := '0';
        res(i).sourceConst(1) := '1'; 
        res(i).virtualSources(1) := (others => '0');
        res(i).physicalSources(1) := (others => '0');
        res(i).physicalSourcesStable(1) := (others => '0');
        res(i).physicalSourcesNew(1) := (others => '0'); -- sources in case of of group dependency        
        res(i).deps(1) := (others => '0');
        res(i).sourcesStable(1) := '1'; -- true if source is from stable map - always ready
        res(i).sourcesNew(1) := '0';   -- true if group dependency
        res(i).sourcesReady(1) := '0'; -- ready as read from ReadyTable based on NewestMap
        
        res(i).sourceSel(2) := '0';
        res(i).sourceConst(2) := '1'; 
        res(i).virtualSources(2) := (others => '0');
        res(i).physicalSources(2) := (others => '0');
        res(i).physicalSourcesStable(2) := (others => '0');
        res(i).physicalSourcesNew(2) := (others => '0'); -- sources in case of of group dependency        
        res(i).deps(2) := (others => '0');
        res(i).sourcesStable(2) := '1'; -- true if source is from stable map - always ready
        res(i).sourcesNew(2) := '0';   -- true if group dependency
        res(i).sourcesReady(2) := '0'; -- ready as read from ReadyTable based on NewestMap        
    end loop;
    
    return res;
end function;


function updateArgStates(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(3*i to 3*i + 2) := ((riaInt(i).sourcesStable or readyRegFlags(3*i to 3*i + 2)) and not riaInt(i).sourcesNew);-- and not riaFloat(i).sourcesNew);
    end loop;
    return res;
end function;

function updateArgStatesFloat(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(3*i to 3*i + 2) := ((riaFloat(i).sourcesStable or readyRegFlags(3*i to 3*i + 2)) and not riaFloat(i).sourcesNew);-- and not riaFloat(i).sourcesNew);
    end loop;
    return res;
end function;


function clearFloatDest(insArr: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insArr'range) := insArr;
begin
    for i in res'range loop
        if res(i).ins.physicalArgSpec.floatDestSel = '1' then
           res(i).ins.physicalArgSpec.dest := (others => '0');
        end if;
    end loop;
    return res;
end function;

function clearIntDest(insArr: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insArr'range) := insArr;
begin
    for i in res'range loop
        if res(i).ins.physicalArgSpec.floatDestSel = '0' then
           res(i).ins.physicalArgSpec.dest := (others => '0');
        end if;
    end loop;
    return res;
end function;

function mergePhysDests(insS0, insS1: InstructionSlot) return InstructionSlot is
    variable res: InstructionSlot := insS0;
begin
    res.ins.physicalArgSpec.dest := insS0.ins.physicalArgSpec.dest or insS1.ins.physicalArgSpec.dest;
    return res;
end function;
        
        
function replaceDests(insVec: InstructionSlotArray; ria: RenameInfoArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
         res(i).ins.physicalArgSpec.intDestSel := ria(i).destSel and not ria(i).destSelFP;
         res(i).ins.physicalArgSpec.floatDestSel := ria(i).destSelFP;
         
         res(i).ins.physicalArgSpec.dest := ria(i).physicalDest;
    end loop;
    return res;
end function;
        
        
        
function restoreRenameIndex(content: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := content;
begin
    for i in 1 to PIPE_WIDTH-1 loop
        res(i).ins.tags.renameIndex := clearTagLow(res(0).ins.tags.renameIndex) or i2slv(i, TAG_SIZE);
    end loop;

    return res;
end function;


function getSpecialActionSlot(insVec: InstructionSlotArray) return InstructionSlot is
   variable res: InstructionSlot := insVec(0);
begin
   res.full := '0';  
   for i in PIPE_WIDTH-1 downto 0 loop
       -- TODO: simpler to get last full slot because if a static event is present, nothing will be after it in group.
       --       Then the 'full' bit of 'special' would be set if specialAction/exc/dbTrap
       if (insVec(i).full and hasSyncEvent(insVec(i).ins)) = '1' then
           res := insVec(i);
           res.ins.specificOperation.system := SysOp'val(slv2u(res.ins.specificOperation.bits));
           exit;
       end if;
   end loop;
   
   if CLEAR_DEBUG_INFO then
       res.ins.classInfo := DEFAULT_CLASS_INFO;
       
       res.ins.ip := (others => '0');
       res.ins.bits := (others => '0');       
       res.ins.target := (others => '0');
       res.ins.result := (others => '0');
       
       res.ins.tags := DEFAULT_INSTRUCTION_TAGS;
       res.ins.constantArgs := DEFAULT_CONSTANT_ARGS;
       res.ins.virtualArgSpec := DEFAULT_ARG_SPEC;
       res.ins.physicalArgSpec := DEFAULT_ARG_SPEC;    
   end if;
   
   return res;
end function;    


function TMP_recodeMem(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        res(i).ins.specificOperation.memory := MemOp'val(slv2u(res(i).ins.specificOperation.bits));
        
        --    res(i).ins.constantArgs.immSel := '1';
    end loop;

    return res;
end function;

function TMP_recodeFP(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        res(i).ins.specificOperation.float := FpOp'val(slv2u(res(i).ins.specificOperation.bits));
        res(i).ins.virtualArgSpec.intDestSel := '0';          
        res(i).ins.physicalArgSpec.intDestSel := '0';        
    end loop;
    return res;
end function;

function TMP_recodeALU(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop     
        res(i).ins.specificOperation.arith := ArithOp'val(slv2u(res(i).ins.specificOperation.bits));
        res(i).ins.virtualArgSpec.floatDestSel := '0';          
        res(i).ins.physicalArgSpec.floatDestSel := '0';          
    end loop;  
    return res;
end function;


function isBranchIns(ins: InstructionState) return std_logic is
begin
    return bool2std(ins.specificOperation.arith = opJz or ins.specificOperation.arith = opJnz or ins.specificOperation.arith = opJ or ins.specificOperation.arith = opJl);
end function;

function isLoadOp(ins: InstructionState) return std_logic is
begin
    return bool2std(ins.specificOperation.memory = opLoad or ins.specificOperation.memory = opLoadSys);            
end function;

function isStoreOp(ins: InstructionState) return std_logic is
begin
    return bool2std(ins.specificOperation.memory = opStore or ins.specificOperation.memory = opStoreSys);              
end function;

function isLoadMemOp(ins: InstructionState) return std_logic is
begin
    return bool2std(ins.specificOperation.memory = opLoad);        
end function;

function isStoreMemOp(ins: InstructionState) return std_logic is
begin
    return bool2std(ins.specificOperation.memory = opStore);
end function;

function isLoadSysOp(ins: InstructionState) return std_logic is
begin
    return bool2std(ins.specificOperation.memory = opLoadSys);
end function;

function isStoreSysOp(ins: InstructionState) return std_logic is
begin
    return bool2std(ins.specificOperation.memory = opStoreSys);        
end function;

function hasSyncEvent(ins: InstructionState) return std_logic is
begin
    return  ins.controlInfo.hasException or ins.controlInfo.specialAction or ins.controlInfo.dbtrap; 
end function;


function clearDbCausing(ins: InstructionState) return InstructionState is
    variable res: InstructionState := ins;
begin
    if CLEAR_DEBUG_INFO then
        res := DEFAULT_INS_STATE;
        
        res.tags.renameIndex := ins.tags.renameIndex;
        res.tags.intPointer := ins.tags.intPointer;
        res.tags.floatPointer := ins.tags.floatPointer;
        res.tags.bqPointer := ins.tags.bqPointer;
        res.tags.sqPointer := ins.tags.sqPointer;
        res.tags.lqPointer := ins.tags.lqPointer;
     
        res.target := ins.target;
    end if;
    return res;
end function;


function setDestFlags(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
    variable found: boolean := false;
begin
    
    for i in 0 to PIPE_WIDTH-1 loop
        if insVec(i).full = '0' or found then
            res(i).full := '0';
            res(i).ins.virtualArgSpec.intDestSel := '0';
            res(i).ins.virtualArgSpec.floatDestSel := '0';                        
        end if;            
    
        if insVec(i).ins.controlInfo.hasException = '1' or insVec(i).ins.controlInfo.specialAction = '1' then
            found := true;
        end if;
        
        if insVec(i).full = '0' or found then
            res(i).ins.physicalArgSpec.intDestSel := '0';
            res(i).ins.physicalArgSpec.floatDestSel := '0';                        
        else
            res(i).ins.physicalArgSpec.intDestSel := res(i).ins.virtualArgSpec.intDestSel;
            res(i).ins.physicalArgSpec.floatDestSel := res(i).ins.virtualArgSpec.floatDestSel;  
        end if;
            found := false;
    end loop;
    
    return res;
end function;


function getQueueEmpty(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return std_logic is
    constant xored: SmallNumber := pStart xor pEnd;
    constant template: SmallNumber := (others => '0');
begin
    return bool2std(xored(QUEUE_PTR_SIZE downto 0) = template(QUEUE_PTR_SIZE downto 0));
end function;


function getNumFull(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber is
    constant diff: SmallNumber := subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE);
    constant xored: SmallNumber := pStart xor pEnd;        
    variable result: SmallNumber := diff;
begin
    result(QUEUE_PTR_SIZE) := xored(QUEUE_PTR_SIZE) and not isNonzero(xored(QUEUE_PTR_SIZE-1 downto 0));
    return result;      
end function;

function mergeFP(dataInt: InstructionSlotArray; dataFloat: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := dataInt;
begin
    for i in res'range loop
        res(i).full := dataFloat(i).full;
        res(i).ins.physicalArgSpec.args := dataFloat(i).ins.physicalArgSpec.args;
    end loop;
    return res;
end function; 



function makeExecResult(isl: InstructionSlot; full: std_logic) return ExecResult is
    variable res: ExecResult := DEFAULT_EXEC_RESULT;
begin
    res.full := full;
    res.tag := isl.ins.tags.renameIndex;
    res.dest := isl.ins.physicalArgSpec.dest;
    res.value := isl.ins.result;    
    return res;
end function;

function makeExecResult(isl: SchedulerEntrySlot; full: std_logic) return ExecResult is
    variable res: ExecResult := DEFAULT_EXEC_RESULT;
begin
    res.full := full;
    res.tag := isl.ins.tags.renameIndex;
    res.dest := isl.ins.physicalArgSpec.dest;
    res.value := isl.ins.result;    
    return res;
end function;


    function setPhysSources(insVec: InstructionSlotArray;
                            newPhysSources: PhysNameArray;
                            newestSelector, depVec: std_logic_vector)
    return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
    begin
        -- Assign src registers
        for i in 0 to PIPE_WIDTH-1 loop                    
            res(i).ins.physicalArgSpec.args(0) := newPhysSources(3*i+0);
            res(i).ins.physicalArgSpec.args(1) := newPhysSources(3*i+1);
            res(i).ins.physicalArgSpec.args(2) := newPhysSources(3*i+2);                 
        end loop;

        return res;
    end function;


function buildForwardingNetwork(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo is
    variable fni: ForwardingInfo := DEFAULT_FORWARDING_INFO;
begin
         -- Forwarding network
		 fni.nextTagsM3 := (0 => s0_M3.dest,                                                                             2 => s2_M3.dest,                             others => (others => '0'));
		 fni.nextTagsM2 := (0 => s0_M2.dest,                                                                             2 => s2_M2.dest,                             others => (others => '0'));
		 fni.nextTagsM1 := (0 => s0_M1.dest,                                                                             2 => s2_M1.dest,                             others => (others => '0'));        
         fni.tags0 :=      (0 => s0_R0.dest,                              1 => s1_R0.dest,                               2 => s2_R0.dest,                             others => (others => '0')); 
         fni.tags1 :=      (0 => s0_R1.dest,                                                                             2 => s2_R1.dest,                             others => (others => '0'));
         fni.values0 :=    (0 => s0_R0.value,                             1 => s1_R0.value,                              2 => s2_R0.value,                            others => (others => '0'));
         fni.values1 :=    (0 => s0_R1.value,                                                                            2 => s2_R1.value,                            others => (others => '0'));                 

    return fni;
end function;

function buildForwardingNetworkFP(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                  s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                  s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo is
    variable fni: ForwardingInfo := DEFAULT_FORWARDING_INFO;
begin
         -- Forwarding network
		 fni.nextTagsM3 := (0 => s0_M3.dest,                                                                             2 => s2_M3.dest,                             others => (others => '0'));
		 fni.nextTagsM2 := (0 => s0_M2.dest,                                                                             2 => s2_M2.dest,                             others => (others => '0'));
		 fni.nextTagsM1 := (0 => s0_M1.dest,                                                                             2 => s2_M1.dest,                             others => (others => '0'));        
         fni.tags0 :=      (0 => s0_R0.dest,                                                                             2 => s2_R0.dest,                             others => (others => '0')); 
         fni.tags1 :=      (0 => s0_R1.dest,                                                                             2 => s2_R1.dest,                             others => (others => '0'));
         fni.values0 :=    (0 => s0_R0.value,                                                                            2 => s2_R0.value,                            others => (others => '0'));
         fni.values1 :=    (0 => s0_R1.value,                                                                            2 => s2_R1.value,                            others => (others => '0'));                 

    return fni;
end function;


end package body;
