
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;


package PipelineGeneral is

function sn(n: integer) return SmallNumber;

function p2i(p: SmallNumber; n: natural) return natural;

type PhysicalSubpipe is (ALU, Mem, FP, StoreDataInt, StoreDataFloat);


function makeExecResult(isl: SchedulerState) return ExecResult;


type IssueQueueSignals is record
    sending: std_logic;
    cancelled: std_logic;
    ready: std_logic;
    empty: std_logic;
    killSel: std_logic;
    killSel1: std_logic;
    killSel2: std_logic;
    killSel3: std_logic;
end record;


type RegisterState is record
    ready: std_logic;
end record;

type RegisterStateArray is array(integer range <>) of RegisterState;

type RegisterStateArray2D is array(integer range <>) of RegisterStateArray(0 to 2);



type DependencySpec is array(0 to 2) of std_logic_vector(0 to PIPE_WIDTH-1); 
type DependencyVec is array(0 to PIPE_WIDTH-1) of DependencySpec;

constant DEFAULT_DEP_VEC: DependencyVec := (others => (others => (others => '0')));


type RenameInfo is record
        dbInfo: InstructionDebugInfo;

        dbDepTags: InsTagArray(0 to 2); -- TODO: replace with DbDepedency because content of DB infos should be encapsulated

    destSel: std_logic;
    destSelFP: std_logic;
        psel: std_logic;
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
        dbInfo => DEFAULT_DEBUG_INFO, 
        
        dbDepTags => (others => (others => 'U')),
                   
    destSel => '0',
    destSelFP => '0',
        psel => '0',
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

-- Is it needed? Unify with ExecResult?
type BypassEntry is record
    en: std_logic;
    failed: std_logic;
    tag: InsTag;
    iqTag: SmallNumber;
    dest: PhysName;
    value: Mword;
end record;


--type MapEntry is record
    
--    tag: InsTag;
--    virtual: RegName;
--    physical: PhysName;
--end record;


function mergeRenameInfoFP(intArgs, floatArgs: RenameInfoArray) return RenameInfoArray;

function TMP_getPhysicalArgsNew(ri: RenameInfoArray) return PhysNameArray;

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

function restoreRenameIndex(content: InstructionSlotArray) return InstructionSlotArray;

function getSpecialActionSlot(insVec: InstructionSlotArray) return InstructionSlot;

function getAddressIncrement(ins: InstructionState) return Mword;

function findDeps(ia: BufferEntryArray) return DependencyVec;
function getRealDepVecInt(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec;
function getRealDepVecFloat(ia: BufferEntryArray; depVec: DependencyVec) return DependencyVec;   

function hasSyncEvent(ins: InstructionState) return std_logic;
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

function prepareForStoreValueIQ(insVec: InstructionSlotArray) return InstructionSlotArray;
function prepareForStoreValueFloatIQ(insVecFloat: InstructionSlotArray) return InstructionSlotArray;

function TMP_removeArg2(insVec: InstructionSlotArray) return InstructionSlotArray;

function removeArg2(ria: RenameInfoArray) return RenameInfoArray;
function useStoreArg2(ria: RenameInfoArray) return RenameInfoArray;

function updateArgStates(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector;
function updateArgStatesFloat(riaInt, riaFloat: RenameInfoArray; readyRegFlags: std_logic_vector) return std_logic_vector;

function replaceDests(insVec: InstructionSlotArray; ria: RenameInfoArray) return InstructionSlotArray;

-- For setting dest sel flags in stages after ROB!
function setDestFlags(insVec: InstructionSlotArray) return InstructionSlotArray;


function getQueueEmpty(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return std_logic;
function getNumFull(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber;


function convertROBData(isa: InstructionSlotArray) return ControlPacketArray;

function unfoldOp(op: SpecificOp) return SpecificOp;

function getInsSlotArray(elemVec: BufferEntryArray) return InstructionSlotArray;

function setMemFail(er: ExecResult; fail: std_logic; memResult: Mword) return ExecResult;
function updateMemDest(er: ExecResult; used: std_logic) return ExecResult;

                function TMP_slotRegReadM0mq(mqReexecCtrlRR: ControlPacket; mqReexecResRR: ExecResult; mqRegReadSending: std_logic) return SchedulerState;
                
                function TMP_missedMemResult(er: ExecResult; memoryMissed: std_logic; memResult: Mword) return ExecResult;
                                
                function TMP_missedMemCtrl(subpipeM0_E1, subpipeM0_E1f: ExecResult;
                                           ctrlE1, ctrlE1u: ControlPacket;
                                           resOutSQ: ExecResult)
                return ControlPacket;

end package;


package body PipelineGeneral is

function sn(n: integer) return SmallNumber is
begin
    return i2slv(n, SMALL_NUMBER_SIZE);
end function;

function p2i(p: SmallNumber; n: natural) return natural is
    constant mask: SmallNumber := i2slv(n-1, SMALL_NUMBER_SIZE);
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
        res(0 to 2) := ri(0).physicalSourcesNew;
        res(3 to 5) := ri(1).physicalSourcesNew;
        res(6 to 8) := ri(2).physicalSourcesNew;
        res(9 to 11) := ri(3).physicalSourcesNew;
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

function extractFullMask(cpa: ControlPacketArray) return std_logic_vector is
	variable res: std_logic_vector(0 to cpa'length-1) := (others => '0');
begin
	for i in res'range loop
		res(i) := cpa(i).controlInfo.full;
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

function getStoreSysMask(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    variable sm1: std_logic_vector(0 to PIPE_WIDTH-1) := getStoreMask1(insVec);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        if       sm1(i) = '1' --insVec(i).full = '1'
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
        res(i) := insVec(i).ins.classInfo.useAlu;
    end loop;
    
    return res;
end function;

function getMulMask1(insVec: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
    for i in 0 to PIPE_WIDTH-1 loop
        res(i) := insVec(i).ins.classInfo.useMul;
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
        res(i).ins.specificOperation.memory := MemOp'val(slv2u(res(i).ins.specificOperation.bits));        
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

function TMP_recodeMul(insVec: InstructionSlotArray) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop     
        res(i).ins.specificOperation.arith := ArithOp'val(slv2u(res(i).ins.specificOperation.bits));
        res(i).ins.virtualArgSpec.floatDestSel := '0';          
        res(i).ins.physicalArgSpec.floatDestSel := '0';          
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


function hasSyncEvent(ins: InstructionState) return std_logic is
begin
    return  ins.controlInfo.hasException or ins.controlInfo.specialAction or ins.controlInfo.dbtrap; 
end function;

function hasSyncEvent(ct: InstructionControlInfo) return std_logic is
begin
    return  ct.hasException or ct.specialAction or ct.dbtrap; 
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


function makeExecResult(isl: SchedulerState) return ExecResult is
    variable res: ExecResult := DEFAULT_EXEC_RESULT;
begin
    res.full := isl.full;
    res.tag := isl.renameIndex;
    res.dest := isl.argSpec.dest;

    return res;
end function;

function convertROBData(isa: InstructionSlotArray) return ControlPacketArray is
    variable res: ControlPacketArray(isa'range) := (others => DEFAULT_CONTROL_PACKET);
begin
    for i in res'range loop
            res(i).dbInfo := isa(i).ins.dbInfo;
        res(i).controlInfo := isa(i).ins.controlInfo;
        res(i).controlInfo.full := isa(i).full;
        res(i).classInfo := isa(i).ins.classInfo;        
    end loop;
    return res;
end function;

function unfoldOp(op: SpecificOp) return SpecificOp is
    variable res: SpecificOp := op;
begin          
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

function getInsSlot(elem: BufferEntry) return InstructionSlot is
    variable res: InstructionSlot := DEFAULT_INS_SLOT;
begin
    res.full := elem.full;
        res.ins.dbInfo := elem.dbInfo;
    
    res.ins.controlInfo.firstBr := elem.firstBr;
    res.ins.classInfo.branchIns := elem.branchIns;
    --res.ins.controlInfo.frontBranch := elem.frontBranch;
    --res.ins.controlInfo.confirmedBranch := elem.confirmedBranch;
    res.ins.controlInfo.specialAction := elem.specialAction;

    res.ins.classInfo.fpRename := elem.fpRename;           
    res.ins.classInfo.mainCluster := elem.mainCluster;            
    res.ins.classInfo.secCluster := elem.secCluster;            
    res.ins.classInfo.useLQ := elem.useLQ;
    res.ins.classInfo.useSQ := elem.secCluster;
    
    res.ins.specificOperation := unfoldOp(elem.specificOperation);
    
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
    return res;
end function;

function setMemFail(er: ExecResult; fail: std_logic; memResult: Mword) return ExecResult is
    variable res: ExecResult := er;
begin
    res.full := er.full and not fail;
    res.failed := er.full and fail;
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

        function TMP_slotRegReadM0mq(mqReexecCtrlRR: ControlPacket; mqReexecResRR: ExecResult; mqRegReadSending: std_logic) return SchedulerState is
            variable res: SchedulerState := DEFAULT_SCHED_STATE;
        begin
            res.full := mqRegReadSending;
            res.operation := mqReexecCtrlRR.op;
            res.renameIndex := mqReexecCtrlRR.tags.renameIndex;
            res.tags := mqReexecCtrlRR.tags;
            
            -- adr
            res.args(1) := mqReexecCtrlRR.target;
            
            res.argSpec.dest := mqReexecResRR.dest;
            res.argSpec.intDestSel := not mqReexecCtrlRR.classInfo.useFP and isNonzero(mqReexecResRR.dest);
            res.argSpec.floatDestSel := mqReexecCtrlRR.classInfo.useFP;

            return res;
        end function;

        function TMP_missedMemResult(er: ExecResult; memoryMissed: std_logic; memResult: Mword) return ExecResult is
            variable res: ExecResult := er;
        begin
            res.full := res.full and memoryMissed;
            res.value := memResult;
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
            res.controlInfo.tlbMiss := ctrlE1u.controlInfo.tlbMiss;  -- TODO: should be form E1, not E2? 
            res.controlInfo.dataMiss := ctrlE1u.controlInfo.dataMiss;
            res.controlInfo.sqMiss := ctrlE1u.controlInfo.sqMiss;                    
            return res;
        end function;


end package body;
