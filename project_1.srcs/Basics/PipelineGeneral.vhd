
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;


package PipelineGeneral is

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



type ForwardingInfo is record
	nextTagsM3:	PhysNameArray(0 to 2);
	nextTagsM2:	PhysNameArray(0 to 2);
	nextTagsM1: PhysNameArray(0 to 2);
	tags0: PhysNameArray(0 to 2);
	tags1: PhysNameArray(0 to 2);
	values0: MwordArray(0 to 2);
	values1: MwordArray(0 to 2);
	failedM2: std_logic_vector(0 to 2);	
	failedM1: std_logic_vector(0 to 2);	
	failed0: std_logic_vector(0 to 2);	
	failed1: std_logic_vector(0 to 2);	
end record;


type ForwardingComparisons is record
    cmpM3: std_logic_vector(0 to 2);
    cmpM2: std_logic_vector(0 to 2);
    cmpM1: std_logic_vector(0 to 2);
    cmp0: std_logic_vector(0 to 2);
    cmp1: std_logic_vector(0 to 2);
    reg:  std_logic;
end record;

constant DEFAULT_FORWARDING_COMPARISONS: ForwardingComparisons := (reg => '0', others => (others => '0'));

type ForwardingComparisonsArray is array(natural range <>) of ForwardingComparisons;

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
	
	cmps: ForwardingComparisonsArray(0 to 1);
end record;

type ForwardingMatchesArray is array(integer range <>) of ForwardingMatches; 


constant DEFAULT_FORWARDING_INFO: ForwardingInfo := (
	nextTagsM3 => (others => (others => '0')),
	nextTagsM2 => (others => (others => '0')),
	nextTagsM1 => (others => (others => '0')),
    tags0 => (others => (others => '0')),
    tags1 => (others => (others => '0')),
    values0 => (others => (others => '0')),
    values1 => (others => (others => '0')),
    failedM2 => (others => '0'),
    failedM1 => (others => '0'),
    failed0 => (others => '0'),
    failed1 => (others => '0')
);

constant DEFAULT_FORWARDING_MATCHES: ForwardingMatches := (
    cmps => (others => DEFAULT_FORWARDING_COMPARISONS),
    others => (others => '0')
);

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

    function mergeRenameInfoFP(intArgs, floatArgs: RenameInfoArray) return RenameInfoArray;

    function TMP_getPhysicalArgsNew(ri: RenameInfoArray) return PhysNameArray;

function getSelector(mr, mi: std_logic_vector(0 to 2)) return std_logic_vector;

function extractFullMask(queueContent: InstructionSlotArray) return std_logic_vector;
function extractFullMask(cpa: ControlPacketArray) return std_logic_vector;
function extractFullMask(ba: BufferEntryArray) return std_logic_vector;

function setFullMask(insVec: InstructionSlotArray; mask: std_logic_vector) return InstructionSlotArray;

function compareIndBefore(tagA, tagB: SmallNumber; PTR_SIZE: natural) return std_logic;

-- TODO: deprecate?
function getExceptionMask(insVec: InstructionSlotArray) return std_logic_vector;

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
function hasSyncEvent(ct: InstructionControlInfo) return std_logic;

function isBranchIns(ins: InstructionState) return std_logic;
function isLoadOp(ins: InstructionState) return std_logic;
function isStoreOp(ins: InstructionState) return std_logic;
function isLoadMemOp(ins: InstructionState) return std_logic;
function isStoreMemOp(ins: InstructionState) return std_logic;
function isLoadSysOp(ins: InstructionState) return std_logic;
function isStoreSysOp(ins: InstructionState) return std_logic;

function isLoadMemOp(op: SpecificOp) return std_logic;
function isStoreMemOp(op: SpecificOp) return std_logic;
function isLoadOp(op: SpecificOp) return std_logic;
function isStoreOp(op: SpecificOp) return std_logic;
function isLoadSysOp(op: SpecificOp) return std_logic;
function isStoreSysOp(op: SpecificOp) return std_logic;


-- sorting subpipelines
function getBranchMask(insVec: InstructionSlotArray) return std_logic_vector;
function getLoadMask(insVec: InstructionSlotArray) return std_logic_vector;
function getStoreMask(insVec: InstructionSlotArray) return std_logic_vector;
function getStoreSysMask(insVec: InstructionSlotArray) return std_logic_vector;
function getLoadSysMask(insVec: InstructionSlotArray) return std_logic_vector;
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
        res(0 to 2) := ri(0).physicalSourcesNew;-- & ri(1).physicalSourcesNew & ri(2).physicalSourcesNew & ri(3).physicalSourcesNew;
        res(3 to 5) := ri(1).physicalSourcesNew;
        res(6 to 8) := ri(2).physicalSourcesNew;
        res(9 to 11) := ri(3).physicalSourcesNew;
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

function getStoreSysMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable sm1: std_logic_vector(0 to PIPE_WIDTH-1) := getStoreMask1(insVec);
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 	  sm1(i) = '1' --insVec(i).full = '1'
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
		if 	  lm1(i) = '1' -- insVec(i).full = '1' 
		      and insVec(i).ins.specificOperation.subpipe = Mem 
		      and (insVec(i).ins.specificOperation.memory = opLoadSys)		
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
        
        --res(i).target := isa(i).ins.target_D;
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
         fni.failedM2 :=   (0 => s0_M2.failed,                                                                           2 => s2_M2.failed,                           others => '0');                 
         fni.failedM1 :=   (0 => s0_M1.failed,                                                                           2 => s2_M1.failed,                           others => '0');                 
         fni.failed0  :=   (0 => s0_R0.failed,                                                                           2 => s2_R0.failed,                           others => '0');                 
         fni.failed1  :=   (0 => s0_R1.failed,                                                                           2 => s2_R1.failed,                           others => '0');                 

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
         fni.failedM2 :=   (0 => s0_M2.failed,                                                                           2 => s2_M2.failed,                           others => '0');                 
         fni.failedM1 :=   (0 => s0_M1.failed,                                                                           2 => s2_M1.failed,                           others => '0');                 
         fni.failed0  :=   (0 => s0_R0.failed,                                                                           2 => s2_R0.failed,                           others => '0');                 
         fni.failed1  :=   (0 => s0_R1.failed,                                                                           2 => s2_R1.failed,                           others => '0');                 

    return fni;
end function;


end package body;
