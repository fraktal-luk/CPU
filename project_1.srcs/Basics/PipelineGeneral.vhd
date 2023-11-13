
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;


package PipelineGeneral is

type slv2D is array(natural range <>, natural range <>) of std_logic;

type EventState is record
    preExecTags: InstructionTags;
    execTags: InstructionTags;
    execCausing: ExecResult;
    lateCausing: ExecResult;
    memFail: std_logic;
end record;

type DbCoreState is record
    dummy: DummyType;
    dbSignal: std_logic;
end record;

constant DEFAULT_EVENT_STATE: EventState := (
    DEFAULT_INSTRUCTION_TAGS,
    DEFAULT_INSTRUCTION_TAGS,
    DEFAULT_EXEC_RESULT,
    DEFAULT_EXEC_RESULT,
    '0'
    );

constant DEFAULT_DB_STATE: DbCoreState := (dummy => DUMMY_VALUE, dbSignal => '0');


-- TODO: move to basictypes
function sn(n: integer) return SmallNumber;

function p2i(p: SmallNumber; n: natural) return natural;

function iqInds2tags(inds: SmallNumberArray) return SmallNumberArray;


function makeExecResult(isl: SchedulerState) return ExecResult;

function squashOnMemFail(val: std_logic) return std_logic;

type IssueQueueSignals is record
    sending: std_logic;
    sentKilled: std_logic;
    trialPrev1: std_logic;
    trialPrev2: std_logic;
end record;

constant DEFAULT_ISSUE_QUEUE_SIGNALS: IssueQueueSignals := (
    others => '0'
);

function killFollower(trial: std_logic; events: EventState) return std_logic;


type RegisterState is record
    ready: std_logic;
end record;

type RegisterStateArray is array(integer range <>) of RegisterState;

type RegisterStateArray2D is array(integer range <>) of RegisterStateArray(0 to 2);


type DependencySpec is array(0 to 2) of std_logic_vector(0 to PIPE_WIDTH-1); 
type DependencyVec is array(0 to PIPE_WIDTH-1) of DependencySpec;

constant DEFAULT_DEP_VEC: DependencyVec := (others => (others => (others => '0')));


type ArgRenameState is record
    sel: std_logic;
    const: std_logic;
    virtual: RegName;
    physical_O: PhysName;
    physicalNew: PhysName;
    deps: std_logic_vector(0 to PIPE_WIDTH-1);
    hasDep: std_logic;
end record;

constant DEFAULT_ARG_RENAME_STATE: ArgRenameState := (
    sel => '0',
    const => '1',
    virtual => (others => '0'),
    physical_O => (others => '0'),
    physicalNew => (others => '0'),
    deps => (others => '0'),
    hasDep => '0'
);

type ArgRenameStateArray is array(natural range <>) of ArgRenameState;

type RenameInfo is record
    dbInfo: InstructionDebugInfo;

    dbDepTags: InsTagArray(0 to 2); -- TODO: replace with DbDepedency because content of DB infos should be encapsulated

    destSel: std_logic;
    destSelFP: std_logic;
    psel: std_logic;
    virtualDest: RegName;
    physicalDest: PhysName;
    argStates: ArgRenameStateArray(0 to 2);
end record;

type RenameInfoArray is array(natural range <>) of RenameInfo;

constant DEFAULT_RENAME_INFO: RenameInfo := (
    dbInfo => DEFAULT_DEBUG_INFO, 

    dbDepTags => (others => (others => 'U')),

    destSel => '0',
    destSelFP => '0',
    psel => '0',
    virtualDest => (others => '0'),
    physicalDest => (others => '0'),
    argStates => (others => DEFAULT_ARG_RENAME_STATE)    
);

-- Is it needed? Unify with ExecResult?
type BypassEntry is record
    en: std_logic;
    failed: std_logic;
    tag: InsTag;
    iqTag: SmallNumber;
    dest: PhysName;
    value: Mword;
end record;


function mergeRenameInfoFP(intArgs, floatArgs: RenameInfoArray) return RenameInfoArray;

function TMP_getPhysicalArgsNew(ri: RenameInfoArray) return PhysNameArray;
function TMP_getPhysicalDestsNew(ri: RenameInfoArray) return PhysNameArray;

function extractFullMask(queueContent: InstructionSlotArray) return std_logic_vector;
function extractFullMask(cpa: ControlPacketArray) return std_logic_vector;
function extractFullMask(ba: BufferEntryArray) return std_logic_vector;

function setFullMask(insVec: InstructionSlotArray; mask: std_logic_vector) return InstructionSlotArray;

function compareIndBefore(tagA, tagB: SmallNumber; PTR_SIZE: natural) return std_logic;


subtype InsTagHighPart is std_logic_vector(TAG_SIZE-LOG2_PIPE_WIDTH-1 downto 0);
subtype InsTagLowPart is std_logic_vector(LOG2_PIPE_WIDTH-1 downto 0);

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

function findDeps(ia: BufferEntryArray) return DependencyVec;
function getRealDepVecInt(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec;
function getRealDepVecFloat(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec;   

function hasSyncEvent(ct: InstructionControlInfo) return std_logic;


function isLoadMemOp(op: SpecificOp) return std_logic;
function isStoreMemOp(op: SpecificOp) return std_logic;
function isLoadOp(op: SpecificOp) return std_logic;
function isStoreOp(op: SpecificOp) return std_logic;
function isLoadSysOp(op: SpecificOp) return std_logic;
function isStoreSysOp(op: SpecificOp) return std_logic;


function getStoreSysMask(insVec: InstructionSlotArray) return std_logic_vector;
function getLoadSysMask(insVec: InstructionSlotArray) return std_logic_vector;

function getBranchMask1(insVec: InstructionSlotArray) return std_logic_vector;
function getLoadMask1(insVec: InstructionSlotArray) return std_logic_vector;
function getStoreMask1(insVec: InstructionSlotArray) return std_logic_vector;
function getIntStoreMask1(insVec: InstructionSlotArray) return std_logic_vector;       
function getFloatStoreMask1(insVec: InstructionSlotArray) return std_logic_vector;
function getAluMask1(insVec: InstructionSlotArray) return std_logic_vector;
function getMulMask1(insVec: InstructionSlotArray) return std_logic_vector;
function getFpMask1(insVec: InstructionSlotArray) return std_logic_vector;
function getMemMask1(insVec: InstructionSlotArray) return std_logic_vector;


function TMP_recodeMem(insVec: InstructionSlotArray) return InstructionSlotArray;
function TMP_recodeFP(insVec: InstructionSlotArray) return InstructionSlotArray;
function TMP_recodeALU(insVec: InstructionSlotArray) return InstructionSlotArray;
function TMP_recodeMul(insVec: InstructionSlotArray) return InstructionSlotArray;


function swapArgs12(ri: RenameInfo) return RenameInfo;
function swapArgs12(ria: RenameInfoArray) return RenameInfoArray;

function removeArg2(ria: RenameInfoArray) return RenameInfoArray;
function useStoreArg2(ria: RenameInfoArray) return RenameInfoArray;

--function updateArgStates(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector;
--function updateArgStatesFloat(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector;

function replaceDests(insVec: InstructionSlotArray; ria: RenameInfoArray) return InstructionSlotArray;

function getQueueEmpty(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return std_logic;
function getNumFull(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber;

function unfoldOp(op: SpecificOp) return SpecificOp;

function resolving(p: PoisonInfo) return std_logic;

function setMemFail(er: ExecResult; fail: std_logic; memResult: Mword) return ExecResult;
function updateMemDest(er: ExecResult; used: std_logic) return ExecResult;


function TMP_slotIssueM0mq(mqReexecCtrlIs: ControlPacket; mqReexecResIS: ExecResult; mqIssueSending: std_logic) return SchedulerState;

function TMP_missedMemResult(er: ExecResult; memoryMissed: std_logic; memResult: Mword) return ExecResult;
function TMP_missedMemResultEP(ep: ExecPacket; memoryMissed: std_logic; memResult: Mword) return ExecPacket;

function TMP_missedMemCtrl(subpipeM0_E1, subpipeM0_E1f: ExecResult;
                           ctrlE1, ctrlE1u: ControlPacket;
                           resOutSQ: ExecResult)
return ControlPacket;

function selectOrdered(ar: ExecResultArray) return ExecResult;

function isDivOp(op: SpecificOp) return std_logic;
function usesDivider(ss: SchedulerState) return std_logic;

function TMP_restoreOperation(so: SpecificOp) return SpecificOp;

function TMP_mergeStatic(a, b: SchedulerState) return SchedulerState;

function countSN(v: std_logic_vector) return SmallNumber;
function std2int(s: std_logic) return integer;



    type DispatchMasks is record
        alu: std_logic_vector(0 to PIPE_WIDTH-1);
        mul: std_logic_vector(0 to PIPE_WIDTH-1);
        mem: std_logic_vector(0 to PIPE_WIDTH-1);
        branch: std_logic_vector(0 to PIPE_WIDTH-1);
        load: std_logic_vector(0 to PIPE_WIDTH-1);
        store: std_logic_vector(0 to PIPE_WIDTH-1);
        fp: std_logic_vector(0 to PIPE_WIDTH-1);
        intStore: std_logic_vector(0 to PIPE_WIDTH-1);
        floatStore: std_logic_vector(0 to PIPE_WIDTH-1);

    end record;

    constant DEFAULT_DISPATCH_MASKS: DispatchMasks := (others => (others => '0'));


    function getInsSlotArray(elemVec: BufferEntryArray) return InstructionSlotArray;

    function suppressAfterEvent(isa: InstructionSlotArray; frontData: BufferEntryArray) return InstructionSlotArray;
    function getDispatchMasks(fd: BufferEntryArray) return DispatchMasks;
    function getDispatchMasks(cpa: ControlPacketArray) return DispatchMasks;

    function convertExecStoreValue(sx: SchedulerState; args: MwordArray(0 to 2)) return ExecResult;

    function advancePoison(p: PoisonInfo) return PoisonInfo;
        function advancePoison(p: PoisonInfo; memFail: std_logic) return PoisonInfo;

    function makeEP(ss: SchedulerState) return ExecPacket;
    function updateEP_Async(ep: ExecPacket; evt: EventState) return ExecPacket;
    function updateEP(ep: ExecPacket; evt: EventState) return ExecPacket;
    function mergeEP(epA, epB: ExecPacket) return ExecPacket;
    function applyFail(ep: ExecPacket; fail: std_logic) return ExecPacket;

    function makeMemResult(stage: SchedulerState; hit, fail: std_logic; dest: PhysName; value: Mword) return ExecResult;

end package;


package body PipelineGeneral is

function killFollower(trial: std_logic; events: EventState) return std_logic is
begin
    return (trial and events.execCausing.full) or events.lateCausing.full;
end function;


function iqInds2tags(inds: SmallNumberArray) return SmallNumberArray is
    variable res: SmallNumberArray(0 to RENAME_W-1) := (others => sn(0));
    variable lane: SmallNumber := sn(0);
begin
    for i in 0 to RENAME_W-1 loop 
        lane := sn(i);
        res(i)(SMALL_NUMBER_SIZE-3+LOG2_PIPE_WIDTH downto LOG2_PIPE_WIDTH) := inds(i)(SMALL_NUMBER_SIZE-3 downto 0);
        res(i) := res(i) or lane;
    end loop;
    return res;
end function;


function sn(n: integer) return SmallNumber is
begin
    return i2slv(n, SMALL_NUMBER_SIZE);
end function;

function p2i(p: SmallNumber; n: natural) return natural is
    constant mask: SmallNumber := sn(n-1);
    constant pT: SmallNumber := p and mask;
begin
    return slv2u(pT);
end function;

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
    for i in 0 to PIPE_WIDTH-1 loop
        for j in 0 to 2 loop
            res(3*i + j) := ri(i).argStates(j).physicalNew;
        end loop;
    end loop;

    return res;
end function;

function TMP_getPhysicalDestsNew(ri: RenameInfoArray) return PhysNameArray is
    variable res: PhysNameArray(0 to PIPE_WIDTH-1);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := ri(i).physicalDest;
    end loop;

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

function extractFullMask(cpa: ControlPacketArray) return std_logic_vector is
	variable res: std_logic_vector(0 to cpa'length-1) := (others => '0');
begin
	for i in res'range loop
		res(i) := cpa(i).controlInfo.c_full;
	end loop;
	return res;
end function;

function extractFullMask(ba: BufferEntryArray) return std_logic_vector is
	variable res: std_logic_vector(0 to ba'length-1) := (others => '0');
begin
	for i in res'range loop
		res(i) := ba(i).full;
	end loop;
	return res;
end function;

function compareIndBefore(tagA, tagB: SmallNumber; PTR_SIZE: natural) return std_logic is
    variable tC: SmallNumber := (others => '0');
begin
    tC := sub(tagA, tagB);
    return tC(PTR_SIZE-1);
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


function getBranchMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.typeInfo.branchIns;
    end loop;	
    return res;
end function;

function getLoadMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.typeInfo.useLQ;
    end loop;
    
    return res;
end function;

function getStoreMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.typeInfo.useSQ;
    end loop;
    
    return res;
end function;


function getIntStoreMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.dispatchInfo.storeInt;
    end loop;
    
    return res;
end function;

function getFloatStoreMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.dispatchInfo.storeFP;
    end loop;
    
    return res;
end function;

function getStoreSysMask(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    variable sm1: std_logic_vector(0 to PIPE_WIDTH-1) := getStoreMask1(insVec);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        if       sm1(i) = '1'
               and insVec(i).ins.specificOperation.subpipe = Mem 
              and (insVec(i).ins.specificOperation.memory = opStoreSys)        
        then
            res(i) := '1';
        end if;
    end loop;
    
    return res;
end function;

function getLoadSysMask(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    variable lm1: std_logic_vector(0 to PIPE_WIDTH-1) := getLoadMask1(insVec);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        if       lm1(i) = '1' -- insVec(i).full = '1' 
              and insVec(i).ins.specificOperation.subpipe = Mem 
              and (insVec(i).ins.specificOperation.memory = opLoadSys)        
        then
            res(i) := '1';
        end if;
    end loop;
    
    return res;
end function;

function getAluMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.dispatchInfo.useAlu;
    end loop;
    
    return res;
end function;

function getMulMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.dispatchInfo.useMul;
    end loop;
    
    return res;
end function;

function getFpMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.dispatchInfo.useFP;
    end loop;
    
    return res;
end function;

function getMemMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.dispatchInfo.useMem;
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

function swapArgs12(ri: RenameInfo) return RenameInfo is
    variable res: RenameInfo := ri;
begin
    if QQQ = 1 then
        res.argStates(2) := ri.argStates(1);
        res.argStates(1) := ri.argStates(2);
    end if;
    
    return res;
end function;

function swapArgs12(ria: RenameInfoArray) return RenameInfoArray is
    variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := ria;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := swapArgs12(res(i));
    end loop;
    
    return res;
end function;

function removeArg2(ria: RenameInfoArray) return RenameInfoArray is
    variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := ria;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i).argStates(2) := DEFAULT_ARG_RENAME_STATE;
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

        res(i).argStates(0) := res(i).argStates(2);        
        res(i).argStates(1) := DEFAULT_ARG_RENAME_STATE;        
        res(i).argStates(2) := DEFAULT_ARG_RENAME_STATE;        
    end loop;
    
    return res;
end function;


--function updateArgStates(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector is
--    variable res: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
--begin
--    for i in 0 to PIPE_WIDTH-1 loop
--        for j in 0 to 2 loop
--            res(3*i + j) := ((riaInt(i).argStates(j).sourceStable or readyRegFlags(3*i +j)) and not riaInt(i).argStates(j).sourceNew);-- and not riaFloat(i).sourcesNew);
--        end loop;
--    end loop;
--    return res;
--end function;

--function updateArgStatesFloat(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector is
--    variable res: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
--begin
--    for i in 0 to PIPE_WIDTH-1 loop
--        for j in 0 to 2 loop
--            res(3*i + j) := ((riaFloat(i).argStates(j).sourceStable or readyRegFlags(3*i +j)) and not riaFloat(i).argStates(j).sourceNew);-- and not riaFloat(i).sourcesNew);
--        end loop;
--    end loop;
--    return res;
--end function;

        
function replaceDests(insVec: InstructionSlotArray; ria: RenameInfoArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        res(i).ins.dest_T := ria(i).physicalDest;
    end loop;
    return res;
end function;


function findDeps(ia: BufferEntryArray) return DependencyVec is
    variable res: DependencyVec := DEFAULT_DEP_VEC;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        for k in 0 to 2 loop -- For each of 3 possible source arguments
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if ia(i).argSpec.args(k)(4 downto 0) = ia(j).argSpec.dest(4 downto 0) -- name match       
                then
                    res(i)(k)(j) := '1';                   
                end if;
            end loop;
        end loop;
    end loop;
    
    return res;
end function;


function getRealDepVecInt(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec is
    variable res: DependencyVec := (others => (others => (others => '0')));
begin
    for i in 0 to PIPE_WIDTH-1 loop
        for k in 0 to 2 loop -- For each of 3 possible source arguments
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if depVec(i)(k)(j) = '1' and ia(i).argSpec.intArgSel(k) = '1' and ia(j).argSpec.intDestSel = '1' -- intSel match
                then
                    res(i)(k)(j) := '1';
                    exit;                        
                end if;
            end loop;
        end loop;                     

    end loop;
    return res;
end function;

function getRealDepVecFloat(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec is
    variable res: DependencyVec := (others => (others => (others => '0')));
begin
    for i in 0 to PIPE_WIDTH-1 loop
        for k in 0 to 2 loop -- For each of 3 possible source arguments
            for j in PIPE_WIDTH-1 downto 0 loop
                if j >= i then
                    next;
                end if;
                
                if depVec(i)(k)(j) = '1' and ia(i).argSpec.floatArgSel(k) = '1' and ia(j).argSpec.floatDestSel = '1'
                then
                    res(i)(k)(j) := '1';
                    exit;                   
                end if;
            end loop;
        end loop;                     

    end loop;        
    return res;
end function;


function TMP_recodeMem(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        if slv2u(res(i).ins.specificOperation.bits) > MemOp'pos(MemOp'right) then
            res(i).ins.specificOperation.memory := opLoad;
        else
            res(i).ins.specificOperation.memory := MemOp'val(slv2u(res(i).ins.specificOperation.bits));
        end if;
    end loop;

    return res;
end function;

function TMP_recodeFP(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        if slv2u(res(i).ins.specificOperation.bits) > FpOp'pos(FpOp'right) then
            res(i).ins.specificOperation.float := opMove;
        else
            res(i).ins.specificOperation.float := FpOp'val(slv2u(res(i).ins.specificOperation.bits));
        end if;
        res(i).ins.virtualArgSpec.intDestSel := '0';          
    end loop;
    return res;
end function;

function TMP_recodeALU(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        if slv2u(res(i).ins.specificOperation.bits) > ArithOp'pos(ArithOp'right) then
            res(i).ins.specificOperation.arith := opAnd;
        else
            res(i).ins.specificOperation.arith := ArithOp'val(slv2u(res(i).ins.specificOperation.bits));
        end if;
        res(i).ins.virtualArgSpec.floatDestSel := '0';          
    end loop;  
    return res;
end function;

function TMP_recodeMul(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop     
        if slv2u(res(i).ins.specificOperation.bits) > ArithOp'pos(ArithOp'right) then
            res(i).ins.specificOperation.arith := opAnd;
        else
            res(i).ins.specificOperation.arith := ArithOp'val(slv2u(res(i).ins.specificOperation.bits));
        end if;
        res(i).ins.virtualArgSpec.floatDestSel := '0';          
    end loop;
    return res;
end function;


function isLoadMemOp(op: SpecificOp) return std_logic is
begin
    return bool2std(op.memory = opLoad);        
end function;

function isStoreMemOp(op: SpecificOp) return std_logic is
begin
    return bool2std(op.memory = opStore);
end function;

function isLoadOp(op: SpecificOp) return std_logic is
begin
    return bool2std(op.memory = opLoad or op.memory = opLoadSys);        
end function;

function isStoreOp(op: SpecificOp) return std_logic is
begin
    return bool2std(op.memory = opStore or op.memory = opStoreSys);
end function;

function isLoadSysOp(op: SpecificOp) return std_logic is
begin
    return bool2std(op.memory = opLoadSys);        
end function;

function isStoreSysOp(op: SpecificOp) return std_logic is
begin
    return bool2std(op.memory = opStoreSys);
end function;


function hasSyncEvent(ct: InstructionControlInfo) return std_logic is
begin
    return ct.hasException or ct.specialAction or ct.dbtrap; 
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


function makeExecResult(isl: SchedulerState) return ExecResult is
    variable res: ExecResult := DEFAULT_EXEC_RESULT;
begin
    res.full := isl.full;
    res.dbInfo := isl.st.dbInfo;
    
        res.poison := isl.poison;
    
    res.tag := isl.st.tags.renameIndex;
    res.dest := isl.dest;
    return res;
end function;

function squashOnMemFail(val: std_logic) return std_logic is
begin
    return val and bool2std(SQUASH_ON_MEM_FAIL);
end function;

function resolving(p: PoisonInfo) return std_logic is
begin
    return p.isOn and p.degrees(2);
end function;

function setMemFail(er: ExecResult; fail: std_logic; memResult: Mword) return ExecResult is
    variable res: ExecResult := er;
begin
    res.full := er.full and not fail;
    res.failed := er.full and fail;

    if fail = '1' then
        --res.dest := (others => '0');
    end if;

    res.value := memResult;
    return res;
end function;

function updateMemDest(er: ExecResult; used: std_logic) return ExecResult is
    variable res: ExecResult := er;
begin
    res.full := er.full and used;
    if used /= '1' then
        res.dest := (others => '0');
    end if;
    return res;
end function;


function TMP_slotIssueM0mq(mqReexecCtrlIs: ControlPacket; mqReexecResIs: ExecResult; mqIssueSending: std_logic) return SchedulerState is
    variable res: SchedulerState := DEFAULT_SCHED_STATE;
begin
    res.full := mqIssueSending;
    res.st.dbInfo := mqReexecCtrlIs.dbInfo;
    
    res.st.operation := mqReexecCtrlIS.op;
    res.st.tags := mqReexecCtrlIs.tags;
    
    -- adr
--    res.argValues(0) := mqReexecCtrlIs.target;
--    res.argValues(1) := (others => '0');

    res.argSrc(0) := "00000000";
    res.argSrc(1) := "00000000";

    res.st.zero := "110";
    res.st.immValue := mqReexecResIs.value(15 downto 0);

    res.dest := mqReexecResIs.dest;
    res.intDestSel := not mqReexecCtrlIs.classInfo.useFP and isNonzero(mqReexecResIs.dest);
    res.floatDestSel := mqReexecCtrlIs.classInfo.useFP;

    return res;
end function;



function TMP_missedMemResult(er: ExecResult; memoryMissed: std_logic; memResult: Mword) return ExecResult is
    variable res: ExecResult := er;
begin
    res.full := res.full and memoryMissed;
    res.value := memResult;
    return res;
end function;

    function TMP_missedMemResultEP(ep: ExecPacket; memoryMissed: std_logic; memResult: Mword) return ExecPacket is
        variable res: ExecPacket := ep;
    begin
        res.full := res.full and memoryMissed;
        
        --res.value := memResult;
        return res;
    end function;
   
function TMP_missedMemCtrl(subpipeM0_E1, subpipeM0_E1f: ExecResult;
                           ctrlE1, ctrlE1u: ControlPacket;
                           resOutSQ: ExecResult)
return ControlPacket is
    variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
begin
    res.ip := subpipeM0_E1.value;
    res.op := ctrlE1.op;
    res.tags := ctrlE1u.tags;
    res.target(SMALL_NUMBER_SIZE-1 downto 0) := resOutSQ.dest; -- TMP: SQ tag for data forwarding; valid if forwarded or SQ miss
    res.classInfo.useFP := subpipeM0_E1f.full;
    res.controlInfo.tlbMiss := ctrlE1u.controlInfo.tlbMiss;
    res.controlInfo.dataMiss := ctrlE1u.controlInfo.dataMiss;
    res.controlInfo.sqMiss := ctrlE1u.controlInfo.sqMiss;                    
    return res;
end function;

function selectOrdered(ar: ExecResultArray) return ExecResult is
begin
    for i in ar'low to ar'high-1 loop
        if ar(i).full = '1' then
            return ar(i);
        end if;
    end loop;
    return ar(ar'high);
end function;

function isDivOp(op: SpecificOp) return std_logic is
begin
    return bool2std(op.subpipe = ALU
                    and (op.arith = opDivU or op.arith = opDivS or op.arith = opRemU or op.arith = opRemS));
end function;

function usesDivider(ss: SchedulerState) return std_logic is
begin
   return bool2std(ss.st.operation.arith = opDivU or ss.st.operation.arith = opDivS or ss.st.operation.arith = opRemU or ss.st.operation.arith = opRemS);
end function;


function unfoldOp(op: SpecificOp) return SpecificOp is
    variable res: SpecificOp := op;
begin
        return TMP_restoreOperation(op);

    --------
    case op.subpipe is
        when ALU =>
            res.arith := ArithOp'val(slv2u(op.bits));     
        when None =>
            res.system := SysOp'val(slv2u(op.bits));
        when FP =>
            res.float := FpOp'val(slv2u(op.bits));
        when others =>
            res.memory := MemOp'val(slv2u(op.bits));
    end case;
    return res;
end function;

-- TODO: rename
function TMP_restoreOperation(so: SpecificOp) return SpecificOp is
    variable res: SpecificOp := so;
begin
    if slv2u(so.bits) > ArithOp'pos(ArithOp'right) then
        res.arith := opAnd;
    else
        res.arith := ArithOp'val(slv2u(so.bits));
    end if;

    if slv2u(so.bits) > MemOp'pos(MemOp'right) then
        res.memory := opLoad;
    else
        res.memory := MemOp'val(slv2u(so.bits));
    end if;

    if slv2u(so.bits) > FpOp'pos(FpOp'right) then
        res.float := opMove;
    else
        res.float := FpOp'val(slv2u(so.bits));
    end if;

    if slv2u(so.bits) > SysOp'pos(SysOp'right) then
        res.system := opNone;
    else
        res.system := SysOp'val(slv2u(so.bits));
    end if;

    return res;
end function;

function TMP_mergeStatic(a, b: SchedulerState) return SchedulerState is
    variable res: SchedulerState := a;
begin
    res.st := b.st;
    return res;
end function;

function countSN(v: std_logic_vector) return SmallNumber is
    variable res: SmallNumber := sn(0);
begin
    res := sn(countOnes(v));
    return res;
end function;

function std2int(s: std_logic) return integer is
begin
    if s = '1' then
        return 1;
    else
        return 0;
    end if;
end function;



    function classifyForDispatch(inSlot: InstructionSlot) return InstructionSlot is
        variable outSlot: InstructionSlot := inSlot;
        variable div, mul: boolean := false;
    begin
        div := inSlot.ins.specificOperation.arith = opDivU
            or inSlot.ins.specificOperation.arith = opDivS                     
            or inSlot.ins.specificOperation.arith = opRemU
            or inSlot.ins.specificOperation.arith = opRemS;
        mul := inSlot.ins.specificOperation.arith = opMul
            or inSlot.ins.specificOperation.arith = opMulhU
            or inSlot.ins.specificOperation.arith = opMulhS;

        outSlot.ins.dispatchInfo.useDiv := bool2std(div and (inSlot.ins.specificOperation.subpipe = ALU));
        
        if (inSlot.ins.specificOperation.subpipe = ALU) then
            if mul or div then
                outSlot.ins.dispatchInfo.useMul := '1';
            else
                outSlot.ins.dispatchInfo.useAlu := '1';
            end if;
        elsif inSlot.ins.specificOperation.subpipe = FP then
            outSlot.ins.dispatchInfo.useFP := '1';
        elsif inSlot.ins.specificOperation.subpipe = Mem then
            outSlot.ins.dispatchInfo.useMem := '1';

            if (inSlot.ins.specificOperation.memory = opLoad or inSlot.ins.specificOperation.memory = opLoadSys) then 
                outSlot.ins.typeInfo.useLQ := '1';
            elsif (inSlot.ins.specificOperation.memory = opStore or inSlot.ins.specificOperation.memory = opStoreSys) then
                outSlot.ins.typeInfo.useSQ := '1';
                if outSlot.ins.typeInfo.useFP = '1' then
                    outSlot.ins.dispatchInfo.storeFP := '1';
                else
                    outSlot.ins.dispatchInfo.storeInt := '1';
                end if;
            end if;

        end if;
        return outSlot;
    end function;

    function getInsSlot(elem: BufferEntry) return InstructionSlot is
      variable res: InstructionSlot := DEFAULT_INS_SLOT;
    begin
      res.full := elem.full;
      res.ins.dbInfo := elem.dbInfo;

      res.ins.specificOperation := unfoldOp(elem.specificOperation);

      res.ins.typeInfo.mainCluster := elem.classInfo.mainCluster;
      res.ins.typeInfo.secCluster := elem.classInfo.secCluster;
      res.ins.typeInfo.branchIns := elem.classInfo.branchIns;
      res.ins.typeInfo.useLQ := elem.classInfo.useLQ;
      --res.ins.typeInfo.useSQ := elem.classInfo.useSQ;
      res.ins.typeInfo.useFP := elem.classInfo.useFP;
      res.ins.typeInfo.useSpecial := elem.classInfo.useSpecial;

      res.ins.typeInfo.useSQ := elem.classInfo.secCluster;

      res := classifyForDispatch(res);

      res.ins.constantArgs := elem.constantArgs;
      res.ins.virtualArgSpec := elem.argSpec; 

      return res;
    end function;

    function getInsSlotArray(elemVec: BufferEntryArray) return InstructionSlotArray is
      variable res: InstructionSlotArray(elemVec'range);
    begin
      for i in res'range loop
          res(i) := getInsSlot(elemVec(i));
      end loop;
      
      --res := classifyForDispatch(res);
      
      return res;
    end function;



    function getInsSlot(cp: ControlPacket) return InstructionSlot is
      variable res: InstructionSlot := DEFAULT_INS_SLOT;
    begin
      res.full := cp.full;
      res.ins.dbInfo := cp.dbInfo;

      res.ins.specificOperation := unfoldOp(cp.op);

      res.ins.typeInfo.mainCluster := cp.classInfo.mainCluster;
      res.ins.typeInfo.secCluster := cp.classInfo.secCluster;
      res.ins.typeInfo.branchIns := cp.classInfo.branchIns;
      res.ins.typeInfo.useLQ := cp.classInfo.useLQ;
      --res.ins.typeInfo.useSQ := elem.classInfo.useSQ;
      res.ins.typeInfo.useFP := cp.classInfo.useFP;
      res.ins.typeInfo.useSpecial := cp.classInfo.useSpecial;

      res.ins.typeInfo.useSQ := cp.classInfo.secCluster;

      res := classifyForDispatch(res);

      --res.ins.constantArgs := elem.constantArgs;
      --res.ins.virtualArgSpec := elem.argSpec; 

      return res;
    end function;

    function getInsSlotArray(cpVec: ControlPacketArray) return InstructionSlotArray is
      variable res: InstructionSlotArray(cpVec'range);
    begin
      for i in res'range loop
          res(i) := getInsSlot(cpVec(i));
      end loop;
      
      --res := classifyForDispatch(res);
      
      return res;
    end function;


    function suppressAfterEvent(isa: InstructionSlotArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := isa;
        variable found: boolean := false;
    begin
         for i in 0 to PIPE_WIDTH-1 loop
            if found then
                res(i).full := '0';
            end if;

            if res(i).full /= '1' then
                res(i).ins.typeInfo := DEFAULT_TYPE_INFO;
                res(i).ins.dispatchInfo := DEFAULT_CLASS_INFO_DISPATCH;                            
            end if;

            if res(i).ins.typeInfo.useSpecial = '1' then
                found := true;
            end if;
        end loop;

        return res;
    end function;

    function suppressAfterEvent(isa: InstructionSlotArray; frontData: BufferEntryArray) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := isa;
        variable found: boolean := false;
    begin
         for i in 0 to PIPE_WIDTH-1 loop
            if found then
                res(i).full := '0';
            end if;

            if res(i).full /= '1' then
                res(i).ins.typeInfo := DEFAULT_TYPE_INFO;
                res(i).ins.dispatchInfo := DEFAULT_CLASS_INFO_DISPATCH;                            
            end if;

            if frontData(i).specialAction = '1' then
                found := true;
            end if;
        end loop;

        return res;
    end function;


        function getDispatchMasks(fd: BufferEntryArray) return DispatchMasks is
            variable res: DispatchMasks := DEFAULT_DISPATCH_MASKS;
            constant isa: InstructionSlotArray(0 to PIPE_WIDTH-1) := suppressAfterEvent(getInsSlotArray(fd), fd);
        begin
            res.alu := getAluMask1(isa);
            res.mul := getMulMask1(isa);
            res.mem := getMemMask1(isa);
            res.branch := getBranchMask1(isa);
            res.load := getLoadMask1(isa);
            res.store := getStoreMask1(isa);
            res.fp := getFpMask1(isa);
            res.intStore := getIntStoreMask1(isa);
            res.floatStore := getFloatStoreMask1(isa);
            return res;
        end function;

        function getDispatchMasks(cpa: ControlPacketArray) return DispatchMasks is
            variable res: DispatchMasks := DEFAULT_DISPATCH_MASKS;
            constant isa: InstructionSlotArray(0 to PIPE_WIDTH-1) := suppressAfterEvent(
                                                                            getInsSlotArray(cpa)
                                                                     --           , fd
                                                                     );
        begin
            res.alu := getAluMask1(isa);
            res.mul := getMulMask1(isa);
            res.mem := getMemMask1(isa);
            res.branch := getBranchMask1(isa);
            res.load := getLoadMask1(isa);
            res.store := getStoreMask1(isa);
            res.fp := getFpMask1(isa);
            res.intStore := getIntStoreMask1(isa);
            res.floatStore := getFloatStoreMask1(isa);
            return res;
        end function;

    function convertExecStoreValue(sx: SchedulerState; args: MwordArray(0 to 2)) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
    begin
        res.full := sx.full;
        res.tag := sx.st.tags.renameIndex;
        res.dest := sx.st.tags.sqPointer;
        res.value := --sx.argValues(0);
                     args(0);
        return res;
    end function;

    function advancePoison(p: PoisonInfo) return PoisonInfo is
        variable res: PoisonInfo := p;
    begin
        if resolving(p) = '1' then
            res.yes := '1'; -- TMP!
            res.no := '1';  -- TMP!
        end if;
        
        res.degrees := '0' & p.degrees(0 to 3);
        if isNonzero(res.degrees) /= '1' then
            res.isOn := '0';
        end if;
        return res;
    end function;

        function advancePoison(p: PoisonInfo; memFail: std_logic) return PoisonInfo is
            variable res: PoisonInfo := p;
        begin
            if resolving(p) = '1' then
                res.yes := res.yes or memFail; -- TMP!
                res.no := --res.no and not memFail;  -- TMP!
                          not res.yes;
            end if;

            res.degrees := '0' & p.degrees(0 to 3);
            if isNonzero(res.degrees) /= '1' then
                res.isOn := '0';
            end if;
            return res;
        end function;


    function makeEP(ss: SchedulerState) return ExecPacket is
        variable res: ExecPacket := DEFAULT_EXEC_PACKET; 
    begin
        res.full := ss.full;
        
        res.tag := ss.st.tags.renameIndex;
            
            res.poison := ss.poison;
        return res;
    end function;

    function updateEP_Async(ep: ExecPacket; evt: EventState) return ExecPacket is
        variable res: ExecPacket := ep; 
    begin
        if ep.full = '1' and (evt.lateCausing.full = '1' or (evt.execCausing.full and compareTagBefore(evt.execCausing.tag, ep.tag)) = '1') then
            res.killed := '1';
        end if;

        return res;
    end function;

    function updateEP(ep: ExecPacket; evt: EventState) return ExecPacket is
        variable res: ExecPacket := ep; 
    begin
            res.poison := advancePoison(ep.poison, evt.memFail);
        
        if ep.killed = '1' then
            res.full := '0';
        end if;

        if ep.full = '1' and (evt.lateCausing.full = '1' or (evt.execCausing.full and compareTagBefore(evt.execCausing.tag, ep.tag)) = '1') then
            res.killed := '1';
        end if;

        return res;
    end function;

    function mergeEP(epA, epB: ExecPacket) return ExecPacket is
        variable res: ExecPacket := epA; 
    begin
        if epB.full = '1' then
            res := epB;
        end if;

        return res;
    end function;

    function applyFail(ep: ExecPacket; fail: std_logic) return ExecPacket is
        variable res: ExecPacket := ep; 
    begin
        if fail = '1' then
            res.full := '0';
            res.fail := '1';
        end if;

        return res;
    end function;


    function makeMemResult(stage: SchedulerState; hit, fail: std_logic; dest: PhysName; value: Mword) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
    begin
        res.full := hit;
        res.dbInfo := stage.st.dbInfo;
        res.failed := fail;
        res.poison := stage.poison;
        res.tag  := stage.st.tags.renameIndex;
        res.dest := dest;
        res.value := value;
        return res;
    end function;

end package body;
