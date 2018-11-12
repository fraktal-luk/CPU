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


	-- TODO: move config info to general config file included in higher level definition files?
	constant PHYS_REG_BITS: natural := 6 + LOG2_PIPE_WIDTH;
	constant N_PHYSICAL_REGS: natural := 64 * PIPE_WIDTH;
	constant N_PHYS: natural := N_PHYSICAL_REGS;
	
	subtype PhysName is SmallNumber;
	type PhysNameArray is array(natural range <>) of PhysName;


type ExecUnit is (General, ALU, MAC, Divide, Jump, Memory, System );
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
						
						sysUndef
						);	


constant TAG_SIZE: integer := 7 + LOG2_PIPE_WIDTH;
subtype InsTag is std_logic_vector(TAG_SIZE-1 downto 0);
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
		hasReset: std_logic;
	hasInterrupt: std_logic;
	hasException: std_logic;
	hasBranch: std_logic;
	hasReturn: std_logic;
		specialAction: std_logic;
		dbtrap: std_logic;
	exceptionCode: SmallNumber; -- Set when exception occurs, remains cause exception can be only 1 per op
end record;

type InstructionClassInfo is record
	short: std_logic;
		mainCluster: std_logic;
		secCluster: std_logic;
	branchCond: std_logic;
	
	pipeA, pipeB, pipeC, load, store, branchIns: std_logic;
end record;


type InstructionConstantArgs is record
	immSel: std_logic;
	imm: word;
	c0: slv5;
	c1: slv5;
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
			intArgSel: std_logic_vector(0 to 2);
			floatArgSel: std_logic_vector(0 to 2);
			args: SmallNumberArray(0 to 2);
		end record;

		type InstructionTags is record
			fetchCtr: word;	-- Ctr is never reset!
			decodeCtr: InsTag; -- Ctr is never reset!
			renameSeq: InsTag;
			renameIndex: InsTag;	-- group + group position
			intPointer: SmallNumber;
			floatPointer: SmallNumber;
		end record;
		

type InstructionArgValues is record
	newInQueue: std_logic;
	immediate: std_logic;
	zero: std_logic_vector(0 to 2);
	--readyBefore: std_logic_vector(0 to 2);
	readyNow: std_logic_vector(0 to 2);
	readyNext: std_logic_vector(0 to 2);
	readyM2:	std_logic_vector(0 to 2);
	locs: SmallNumberArray(0 to 2);
	nextLocs: SmallNumberArray(0 to 2);
	locsM2: SmallNumberArray(0 to 2);
	missing: std_logic_vector(0 to 2);
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
			intArgSel => (others => '0'),
			floatArgSel => (others => '0'),
			args => ((others => '0'), (others => '0'), (others => '0'))
			);

constant DEFAULT_INSTRUCTION_TAGS: InstructionTags := (
			fetchCtr => (others => '0'),
			decodeCtr => (others => '0'),
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
													hasReset => '0',
												hasException => '0',
												hasBranch => '0',
												hasReturn => '0',												
													specialAction => '0',
													dbtrap => '0',
												exceptionCode => (others=>'0')
												);
end function;

function defaultClassInfo return InstructionClassInfo is
begin
	return InstructionClassInfo'( short => '0',
											mainCluster => '0',
											secCluster => '0',
											branchCond => '0',
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
	return InstructionConstantArgs'('0', (others=>'0'), "00000", "00000");
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
	return (newInQueue => '0',
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


end InstructionState;
