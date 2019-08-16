--
--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 
--
--   To use any of the example code shown below, uncomment the lines and modify as necessary
--

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.ArchDefs.all;

use work.CoreConfig.all;

package InstructionState is
	
	subtype PhysName is SmallNumber;
	type PhysNameArray is array(natural range <>) of PhysName;


type ExecUnit is (General, ALU, MAC, Divide, Jump, Memory, System, FPU );
type ExecFunc is (unknown,

						arithAdd, arithSub, arithSha,
						logicAnd, logicOr, logicShl,
						
						mulS, mulU, 
					
						divS, divU,
						
						load, store,
						
						jump,
						jumpZ,
						jumpNZ,
						
						sysRetI, sysRetE,
						sysHalt,
						sysSync, sysReplay,
						sysMTC, sysMFC, -- move to/from control
						sysError,
						sysCall,
						sysSend,
						
						fpuMov,
						fpuOr,
						
						sysUndef
						);	


constant TAG_SIZE: integer := 7 + LOG2_PIPE_WIDTH;
subtype InsTag is std_logic_vector(TAG_SIZE-1 downto 0);

constant INITIAL_RENAME_CTR: InsTag := i2slv(-1, TAG_SIZE);
constant INITIAL_GROUP_TAG: InsTag := i2slv(-PIPE_WIDTH, TAG_SIZE);
constant INITIAL_GROUP_TAG_INC: InsTag := i2slv(0, TAG_SIZE);

type InsTagArray is array (integer range <>) of InsTag;


type BinomialOp is record
	unit: ExecUnit;
	func: ExecFunc;
end record;

type InstructionControlInfo is record
		squashed: std_logic;
		skipped: std_logic;
	completed: std_logic;
		completed2: std_logic;
	newEvent: std_logic; -- True if any new event appears
	--	hasReset: std_logic;
	hasInterrupt: std_logic;
	hasException: std_logic;
	--hasBranch: std_logic;
	--hasReturn: std_logic;
	   refetch: std_logic;
	   frontBranch: std_logic;
	   confirmedBranch: std_logic;
	specialAction: std_logic;
	dbtrap: std_logic;
	   orderViolation: std_logic;
	   tlbMiss: std_logic;
	   dataMiss: std_logic;
	   sqMiss:    std_logic;
	       firstBr: std_logic;
	exceptionCode: SmallNumber; -- Set when exception occurs, remains cause exception can be only 1 per op
end record;

type InstructionClassInfo is record
	short: std_logic;
		mainCluster: std_logic;
		secCluster: std_logic;
	--branchCond: std_logic;
	fpRename: std_logic; -- true if instruction is routed to FP renamer (NOTE, CHECK: Int renamer is used for all ops, even those that don't go to any IQ)
	pipeA, pipeB, pipeC, load, store, branchIns: std_logic;
end record;


type InstructionConstantArgs is record
	immSel: std_logic;
	imm: word;
	--c0: slv5;
	--c1: slv5;
end record;

type InstructionVirtualArgs is record
	sel: std_logic_vector(0 to 2);
	s0: RegName;
	s1: RegName;
	s2: RegName;
end record;

type InstructionVirtualDestArgs is record
	sel: std_logic_vector(0 to 0);
	d0: RegName;
end record;

type InstructionPhysicalArgs is record
	sel: std_logic_vector(0 to 2);
	s0: PhysName;
	s1: PhysName;
	s2: PhysName;
end record;

type InstructionPhysicalDestArgs is record
	sel: std_logic_vector(0 to 0);
	d0: PhysName;
end record;


		type InstructionArgSpec is record
			intDestSel: std_logic;
			floatDestSel: std_logic;
			dest: SmallNumber;
			destAlt: SmallNumber;
			intArgSel: std_logic_vector(0 to 2);
			floatArgSel: std_logic_vector(0 to 2);
			args: SmallNumberArray(0 to 2);
		end record;

		type InstructionTags is record
			fetchCtr: word;	-- Ctr is never reset!
			decodeCtr: word; -- Ctr is never reset!
			renameCtr: word;
			renameSeq: InsTag;
			renameIndex: InsTag;	-- group + group position
			intPointer: SmallNumber;
			floatPointer: SmallNumber;
		end record;
		

type InstructionArgValues is record
	issued: std_logic;
	newInQueue: std_logic;
	immediate: std_logic;
	zero: std_logic_vector(0 to 2);
	readyNow: std_logic_vector(0 to 2);
	readyNext: std_logic_vector(0 to 2);
	readyM2:	std_logic_vector(0 to 2);
	locs: SmallNumberArray(0 to 2);
	nextLocs: SmallNumberArray(0 to 2);
	locsM2: SmallNumberArray(0 to 2);
	missing: std_logic_vector(0 to 2);
	stored:  std_logic_vector(0 to 2);
	arg0: Mword;
	arg1: Mword;
	arg2: Mword;
	
	argLocsPipe: SmallNumberArray(0 to 2);
	argLocsPhase: SmallNumberArray(0 to 2);
end record;

type InstructionState is record
	controlInfo: InstructionControlInfo;
	ip: Mword;
	bits: word; -- instruction word
	tags: InstructionTags;
	operation: BinomialOp;
	classInfo: InstructionClassInfo;
	constantArgs: InstructionConstantArgs;
	virtualArgSpec: InstructionArgSpec;
	physicalArgSpec: InstructionArgSpec;
	--argValues: InstructionArgValues;
	result: Mword;
	target: Mword;
end record;

type InstructionStateArray is array(integer range <>) of InstructionState;


function defaultControlInfo return InstructionControlInfo;
function defaultClassInfo return InstructionClassInfo;
function defaultConstantArgs return InstructionConstantArgs;
function defaultVirtualArgs return InstructionVirtualArgs;
function defaultVirtualDestArgs return InstructionVirtualDestArgs;
function defaultPhysicalArgs return InstructionPhysicalArgs;
function defaultPhysicalDestArgs return InstructionPhysicalDestArgs;
function defaultArgValues return InstructionArgValues;

function defaultInstructionState return InstructionState;

constant DEFAULT_CONTROL_INFO: InstructionControlInfo := defaultControlInfo;
constant DEFAULT_CLASS_INFO: InstructionClassInfo := defaultClassInfo;
constant DEFAULT_CONSTANT_ARGS: InstructionConstantArgs := defaultConstantArgs;
constant DEFAULT_VIRTUAL_ARGS: InstructionVirtualArgs := defaultVirtualArgs;
constant DEFAULT_VIRTUAL_DEST_ARGS: InstructionVirtualDestArgs := defaultVirtualDestArgs;
constant DEFAULT_PHYSICAL_ARGS: InstructionPhysicalArgs := defaultPhysicalArgs;
constant DEFAULT_PHYSICAL_DEST_ARGS: InstructionPhysicalDestArgs := defaultPhysicalDestArgs;
constant DEFAULT_ARG_VALUES: InstructionArgValues := defaultArgValues;

constant DEFAULT_ARG_SPEC: InstructionArgSpec := InstructionArgSpec'(
			intDestSel => '0',
			floatDestSel => '0',
			dest => (others => '0'),
			destAlt => (others => '0'),
			intArgSel => (others => '0'),
			floatArgSel => (others => '0'),
			args => ((others => '0'), (others => '0'), (others => '0'))
			);

constant DEFAULT_INSTRUCTION_TAGS: InstructionTags := (
			fetchCtr => (others => '0'),
			decodeCtr => (others => '0'),
			renameCtr => (others => '0'),
			renameSeq => (others => '0'), 
			renameIndex => (others => '0'),
			intPointer => (others => '0'),
			floatPointer => (others => '0')
);

constant DEFAULT_INSTRUCTION_STATE: InstructionState := defaultInstructionState;
constant DEFAULT_INS_STATE: InstructionState := defaultInstructionState;
	
-- Created to enable *Array				
type InstructionSlot is record 
	full: std_logic;
	ins: InstructionState;
end record;
	
constant DEFAULT_INSTRUCTION_SLOT: InstructionSlot := ('0', defaultInstructionState);
constant DEFAULT_INS_SLOT: InstructionSlot := ('0', defaultInstructionState);

-- NOTE: index can be negative to enable logical division into 2 different ranges 
type InstructionSlotArray is array(integer range <>) of InstructionSlot;


type SchedulerState is record
	argValues: InstructionArgValues;
end record;

constant DEFAULT_SCHEDULER_STATE: SchedulerState := (argValues => DEFAULT_ARG_VALUES);
constant DEFAULT_SCHED_STATE: SchedulerState := (argValues => DEFAULT_ARG_VALUES);
																				
type SchedulerEntrySlot is record
	full: std_logic;
	ins: InstructionState;
	state: SchedulerState;
end record;

constant DEFAULT_SCHEDULER_ENTRY_SLOT: SchedulerEntrySlot := (full => '0',
																				ins => DEFAULT_INS_STATE,
																				state => DEFAULT_SCHEDULER_STATE);
constant DEFAULT_SCH_ENTRY_SLOT: SchedulerEntrySlot := (full => '0',
																				ins => DEFAULT_INS_STATE,
																				state => DEFAULT_SCHEDULER_STATE);

type SchedulerEntrySlotArray is array(integer range <>) of SchedulerEntrySlot;

function insText(ins: InstructionState) return string;

type InstructionText is record
    tagTxt: string(1 to 40);
    hexTxt: string(1 to 40);
    virtTxt: string(1 to 40);
    physTxt: string(1 to 40);
    controlTxt: string(1 to 40);       
end record;

type InstructionTextArray is array(integer range <>) of InstructionText;


function insText2(ins: InstructionState) return InstructionText;

function insSlotArrayText(insVec: InstructionSlotArray) return InstructionTextArray;

end InstructionState;



package body InstructionState is
	

function defaultControlInfo return InstructionControlInfo is
begin
	return InstructionControlInfo'(
													squashed => '0',
													skipped => '0',
												completed => '0',
													completed2 => '0',
												newEvent => '0',
												hasInterrupt => '0',
												--	hasReset => '0',
												hasException => '0',
												--hasBranch => '0',
												--hasReturn => '0',
												    refetch => '0',
												    frontBranch => '0',
                                                    confirmedBranch => '0',												    											
												specialAction => '0',
												dbtrap => '0',
												    orderViolation => '0',
												    tlbMiss => '0',
												    dataMiss => '0',
												    sqMiss => '0',
												        firstBr => '0',
												exceptionCode => (others=>'0')
												);
end function;

function defaultClassInfo return InstructionClassInfo is
begin
	return InstructionClassInfo'( short => '0',
											mainCluster => '0',
											secCluster => '0',
											--branchCond => '0',
											fpRename => '0',
											pipeA => '0',
											pipeB => '0',
											pipeC => '0',
											load => '0',
											store => '0',
											branchIns => '0'
											);	
end function;

function defaultConstantArgs return InstructionConstantArgs is
begin
	return InstructionConstantArgs'('0', (others=>'0'));
end function;

function defaultVirtualArgs return InstructionVirtualArgs is
begin
	return InstructionVirtualArgs'("000", "00000", "00000", "00000");
end function;


function defaultVirtualDestArgs return InstructionVirtualDestArgs is
begin
	return InstructionVirtualDestArgs'("0", "00000");
end function;

function defaultPhysicalArgs return InstructionPhysicalArgs is
begin
	return InstructionPhysicalArgs'("000", (others => '0'), (others => '0'), (others => '0'));
end function;

function defaultPhysicalDestArgs return InstructionPhysicalDestArgs is
begin
	return InstructionPhysicalDestArgs'("0", (others => '0'));
end function;

function defaultArgValues return InstructionArgValues is
begin
	return (  issued => '0',
	          newInQueue => '0',
			  immediate => '0',
			  zero => (others => '0'),
			  --readyBefore => (others=>'0'),
			  readyNow => (others=>'0'),
			  readyNext => (others=>'0'),
			  readyM2 => (others => '0'),
			  locs => (others => (others => '0')),
			  nextLocs => (others => (others => '0')),
			  locsM2 => (others => (others => '0')),
			  missing => (others=>'0'),
			  stored => (others => '0'),
			  arg0 => (others=>'0'),
			  arg1 => (others=>'0'),
			  arg2 => (others=>'0'),
				argLocsPipe => (others => (others => '0')),
				argLocsPhase => (others => (others => '0'))
			  );
end function;

function defaultInstructionState return InstructionState is
	variable res: InstructionState;
begin 
	res.controlInfo := defaultControlInfo;
	res.ip := (others => '0');
	res.bits := (others=>'0');
	res.tags := DEFAULT_INSTRUCTION_TAGS;
	res.classInfo := defaultClassInfo;
	res.constantArgs := defaultConstantArgs;
	res.virtualArgSpec := DEFAULT_ARG_SPEC;
	res.physicalArgSpec := DEFAULT_ARG_SPEC;
	--res.argValues := defaultArgValues;
	res.result := (others => '0');
	res.target := (others => '0');
	return res;
end function;



function reg2txt(reg: std_logic_vector) return string is
    variable res: string(1 to 2);
    constant letters: string(1 to 16) := "0123456789abcdef";
begin
    if reg(4) = '1' then
        res(1) := '1';
    else
        res(1) := '0';
    end if;
    
    res(2) := letters(slv2u(reg(3 downto 0)) + 1);
    
    return res;
end function;

function physReg2txt(reg: std_logic_vector) return string is
    variable res: string(1 to 2);
    constant letters: string(1 to 16) := "0123456789abcdef";
begin
    res(1) := letters(slv2u(reg(7 downto 4)) + 1);
    res(2) := letters(slv2u(reg(3 downto 0)) + 1);
    
    return res;
end function;

function w2hex(w: Word) return string is
    constant letters: string(1 to 16) := "0123456789abcdef";
    variable res: string(1 to 8) := (others => '0');
begin
    for i in 7 downto 0 loop
        res(8-i) := letters(slv2u(w(4*i+3 downto 4*i)) + 1);
    end loop;
    return res;
end function;

function tag2hex(t: InsTag) return string is
    constant letters: string(1 to 16) := "0123456789abcdef";
    variable res: string(1 to 3) := (others => '0');
begin
    res(1) := letters(slv2u(t(8 downto 8)) + 1);
    res(2) := letters(slv2u(t(7 downto 4)) + 1);
    res(3) := letters(slv2u(t(3 downto 0)) + 1);    
    return res;
end function;

function strExt(str: string; n: positive) return string is 
    variable res: string(1 to n) := (others => ' ');
begin
    for i in 1 to str'length loop
        if i > n then
            exit;
        end if;
        res(i) := str(i);
    end loop;
    return res;
end function;

function insText(ins: InstructionState) return string is
    variable dest, src0, src1, src2: string(1 to 3) := (others => '*');
begin
    
    dest(2 to 3) := reg2txt(ins.virtualArgSpec.dest);
    src0(2 to 3) := reg2txt((ins.virtualArgSpec.args(0)));
    src1(2 to 3) := reg2txt((ins.virtualArgSpec.args(1)));
    src2(2 to 3) := reg2txt((ins.virtualArgSpec.args(2)));
    
    if ins.virtualArgSpec.intDestSel = '1' then
        dest(1) := 'r';
    end if;
    if ins.virtualArgSpec.intArgSel(0) = '1' then
        src0(1) := 'r';
    end if;
    if ins.virtualArgSpec.intArgSel(1) = '1' then
        src1(1) := 'r';
    end if;
    
    -- Length 35
    return strExt(ExecFunc'image(ins.operation.func), 9) & "  " &
     dest & ", " &
     src0 & ", " &
     src1 & ", #" &
     w2hex(ins.constantArgs.imm);    
    
    --return "";
end function;



--type InstructionState is record
--	controlInfo: InstructionControlInfo;
--	ip: Mword;
--	bits: word; -- instruction word
--	tags: InstructionTags;
--	operation: BinomialOp;
--	classInfo: InstructionClassInfo;
--	constantArgs: InstructionConstantArgs;
--	virtualArgSpec: InstructionArgSpec;
--	physicalArgSpec: InstructionArgSpec;
--	--argValues: InstructionArgValues;
--	result: Mword;
--	target: Mword;
--end record;
--
-- Example
-- tags(45, 4a, 22, 12)
-- adr  hex
-- virtual txt
-- physical txt
-- some control info
-- 
-- fetch 22, buf 20, ren 22, seq 31
-- 00000010: 2f130045
-- sub_r r05, r06, r02, ?ff, #00000045
-- sub_r p32, p19, p08, ?ff
-- none // this is control status
-- 

function insText2(ins: InstructionState) return InstructionText is
    variable dest, src0, src1, src2: string(1 to 3) := (others => '*');
    variable tagStr, hexStr, virtStr, physStr, controlStr: string(1 to 40) := (others => nul);
    variable res: InstructionText;
    variable hexTarget: string(1 to 8);
begin
    -- Tag text
    tagStr(1 to 8) := w2hex(ins.tags.fetchCtr);
    tagStr(9) := '/';
    tagStr(10 to 17) := w2hex(ins.tags.decodeCtr);
    tagStr(18) := '/';
    tagStr(19 to 26) := w2hex(ins.tags.renameCtr);
    tagStr(27) := '/';
    tagStr(28 to 30) := tag2hex(ins.tags.renameIndex);
    
    -- Hex text
    hexStr(1 to 8) := w2hex(ins.ip); -- TODO: introduce 64b address when needed
    hexStr(9 to 10) := ": ";
    hexStr(11 to 18) := w2hex(ins.bits);
    
    -- Virtual txt
    dest(2 to 3) := reg2txt(ins.virtualArgSpec.dest);
    src0(2 to 3) := reg2txt((ins.virtualArgSpec.args(0)));
    src1(2 to 3) := reg2txt((ins.virtualArgSpec.args(1)));
    src2(2 to 3) := reg2txt((ins.virtualArgSpec.args(2)));
    
    if ins.virtualArgSpec.intDestSel = '1' then
        dest(1) := 'r';
    elsif ins.virtualArgSpec.floatDestSel = '1' then
        dest(1) := 'f';
    end if;
    
    if ins.virtualArgSpec.intArgSel(0) = '1' then
        src0(1) := 'r';
    elsif ins.virtualArgSpec.floatArgSel(0) = '1' then
        src0(1) := 'f';        
    end if;
    
    if ins.virtualArgSpec.intArgSel(1) = '1' then
        src1(1) := 'r';
    elsif ins.virtualArgSpec.floatArgSel(1) = '1' then
        src1(1) := 'f';        
    end if;
    
    -- When imm value is used replace src1
    if ins.constantArgs.immSel = '1' then
        src1 := "imm";
    end if;
    
    
    if ins.virtualArgSpec.intArgSel(2) = '1' then
        src2(1) := 'r';
    elsif ins.virtualArgSpec.floatArgSel(2) = '1' then
        src2(1) := 'f';        
    end if;    
    
    virtStr(1 to 9) := strExt(ExecFunc'image(ins.operation.func), 9);
    virtStr(10) := ' ';
    virtStr(11 to 13) := dest;
    virtStr(14 to 15) := ", ";
    virtStr(16 to 18) := src0;
    virtStr(19 to 20) := ", ";
    virtStr(21 to 23) := src1;
    virtStr(24 to 25) := ", ";
    virtStr(26 to 28) := src2;   

    --Physical txt
    dest := (others => '*');
    src0 := (others => '*');
    src1 := (others => '*');
    src2 := (others => '*');
    
    dest(2 to 3) := physReg2txt(ins.physicalArgSpec.dest);
    src0(2 to 3) := physReg2txt((ins.physicalArgSpec.args(0)));
    src1(2 to 3) := physReg2txt((ins.physicalArgSpec.args(1)));
    src2(2 to 3) := physReg2txt((ins.physicalArgSpec.args(2)));
    
    if ins.physicalArgSpec.intDestSel = '1' then
        dest(1) := 'r';
    elsif ins.physicalArgSpec.floatDestSel = '1' then
        dest(1) := 'f';
    end if;
    
    if ins.physicalArgSpec.intArgSel(0) = '1' then
        src0(1) := 'r';
    elsif ins.physicalArgSpec.floatArgSel(0) = '1' then
        src0(1) := 'f';        
    end if;
    
    if ins.physicalArgSpec.intArgSel(1) = '1' then
        src1(1) := 'r';
    elsif ins.physicalArgSpec.floatArgSel(1) = '1' then
        src1(1) := 'f';        
    end if;
    
    -- When imm value is used replace src1
    if ins.constantArgs.immSel = '1' then
        src1 := "imm";
    end if;
    
    
    if ins.physicalArgSpec.intArgSel(2) = '1' then
        src2(1) := 'r';
    elsif ins.physicalArgSpec.floatArgSel(2) = '1' then
        src2(1) := 'f';        
    end if;    
    
    physStr(1 to 9) := strExt(ExecFunc'image(ins.operation.func), 9);
    physStr(10) := ' ';
    physStr(11 to 13) := dest;
    physStr(14 to 15) := ", ";
    physStr(16 to 18) := src0;
    physStr(19 to 20) := ", ";
    physStr(21 to 23) := src1;
    physStr(24 to 25) := ", ";
    physStr(26 to 28) := src2;


--type InstructionControlInfo is record
--		squashed: std_logic;
--		skipped: std_logic;
--	completed: std_logic;
--		completed2: std_logic;
--	newEvent: std_logic; -- True if any new event appears
--	--	hasReset: std_logic;
--	hasInterrupt: std_logic;
--	hasException: std_logic;
--	--hasBranch: std_logic;
--	--hasReturn: std_logic;
--	   refetch: std_logic;
--	   frontBranch: std_logic;
--	   confirmedBranch: std_logic;
--	specialAction: std_logic;
--	dbtrap: std_logic;
--	   orderViolation: std_logic;
--	   tlbMiss: std_logic;
--	   dataMiss: std_logic;
--	   sqMiss:    std_logic;
--	exceptionCode: SmallNumber; -- Set when exception occurs, remains cause exception can be only 1 per op
--end record;

    -- Control txt
            -- BrP: T/N/-, Ref: Y/N, BrC: T/N,-, Exc: Y/N, Sp: Y/N
    if ins.controlInfo.refetch = '1' then
       controlStr(1 to 7) := "Refetch";
    elsif ins.controlInfo.hasException = '1' then
       controlStr(1 to 3) := "Exc";
    elsif ins.controlInfo.orderViolation = '1' then
          controlStr(1 to 3) := "Ord";       
    elsif ins.controlInfo.specialAction = '1' then
       controlStr(1 to 7) := "Special";   
    elsif ins.classInfo.branchIns = '1' then
       hexTarget := w2hex(ins.target);
       controlStr(1 to 3) := "Bf:";
       controlStr(4) := '0';
       if ins.controlInfo.frontBranch = '1' then
           controlStr(4) := '1';
       end if;
       controlStr(5) := ',';
       controlStr(6 to 13) := hexTarget;
       controlStr(14) := ';';

       controlStr(15 to 17) := "Bc:";
       controlStr(18) := '0';
       if ins.controlInfo.confirmedBranch = '1' then
           controlStr(18) := '1';
       end if;
       controlStr(19) := ',';
       controlStr(20 to 27) := hexTarget;
       --controlStr(28) := ';';
    
       --controlStr(1 to 16) := "Bf:" & std_logic'image(ins.controlInfo.frontBranch) & ',' & hexTarget & ';';
       --controlStr(17 to 31) := "Bc:" & std_logic'image(ins.controlInfo.confirmedBranch) & ',' & hexTarget;
    end if;
    
    
    -- Make structure
    res.tagTxt := tagStr;
    res.hexTxt := hexStr;
    res.virtTxt := virtStr;
    res.physTxt := physStr;
    res.controlTxt := controlStr;
   
    return res;
end function;

function insSlotArrayText(insVec: InstructionSlotArray) return InstructionTextArray is
    variable res: InstructionTextArray(insVec'range);
begin    
    for i in res'range loop
        if insVec(i).full = '1' then
            res(i) := insText2(insVec(i).ins);
        end if;
    end loop;
    return res;
end function;


end InstructionState;
