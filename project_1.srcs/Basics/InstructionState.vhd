

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.ArchDefs.all;

use work.CoreConfig.all;


package InstructionState is
	
subtype PhysName is SmallNumber;
type PhysNameArray is array(natural range <>) of PhysName;


type ExecUnit is (General, ALU, MAC, Divide, Jump, Memory, System, FPU );
type ExecFunc is (
                unknown,

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


--------------
type SubpipeType is (None, ALU, Mem, FP);

type ArithOp is (opAnd, opOr, opXor, opAdd, opSub, opShl, opSha, opJz, opJnz, opJ, opJl, opMul, opMulshs, opMulhu, opDiv);
type MemOp is (opLoad, opStore, opLoadSys, opStoreSys);

type FpOp is (opMove, opOr);

type SysOp is (opNone, opUndef, opHalt, opSync, opReplay,  opRetI, opRetE, opCall, opError, opSend);

function getSpecificOpSize return natural;

function findLog2(n: positive) return natural;

constant OP_TYPE_BITS: natural := findLog2(SubpipeType'pos(SubpipeType'high) - SubpipeType'pos(SubpipeType'low) + 1);
constant OP_VALUE_BITS: natural := getSpecificOpSize;

constant SYS_OP_SIZE: natural := findLog2(SysOp'pos(SysOp'high) - SysOp'pos(SysOp'low) + 1);

type SpecificOp is record
    subpipe: SubpipeType;
    bits: std_logic_vector(OP_VALUE_BITS-1 downto 0);
    arith: ArithOp;
    memory: MemOp;
    float: FpOp;
    system: SysOp;
end record;

constant DEFAULT_SPECIFIC_OP: SpecificOp := (   subpipe => None,
                                                bits => (others => '0'),
                                                arith => opAnd,
                                                memory => opLoad,
                                                float => opMove,
                                                system => opNone);
                                                
function sop(sub: SubpipeType; func: ArithOp) return SpecificOp;
function sop(sub: SubpipeType; func: MemOp) return SpecificOp;
function sop(sub: SubpipeType; func: FpOp) return SpecificOp;
function sop(sub: SubpipeType; func: SysOp) return SpecificOp;
                                                
---------------

constant TAG_SIZE: integer := 7 + LOG2_PIPE_WIDTH;
subtype InsTag is std_logic_vector(TAG_SIZE-1 downto 0);

constant INITIAL_RENAME_CTR: InsTag := i2slv(-1, TAG_SIZE);
constant INITIAL_GROUP_TAG: InsTag := i2slv(-PIPE_WIDTH, TAG_SIZE);
constant INITIAL_GROUP_TAG_INC: InsTag := i2slv(0, TAG_SIZE);

type InsTagArray is array (integer range <>) of InsTag;


type InstructionControlInfo is record
	completed: std_logic;
	completed2: std_logic;
	
	newEvent: std_logic; -- True if any new event appears
	hasInterrupt: std_logic;
	hasException: std_logic;
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
end record;

type InstructionClassInfo is record
	short: std_logic;
	mainCluster: std_logic;
	secCluster: std_logic;
	fpRename: std_logic; -- true if instruction is routed to FP renamer (NOTE, CHECK: Int renamer is used for all ops, even those that don't go to any IQ)
	branchIns: std_logic;
	useLQ: std_logic;
end record;


type InstructionConstantArgs is record
	immSel: std_logic;
	imm: word;
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
    fetchCtr: Word;	-- Ctr is never reset!
    decodeCtr: Word; -- Ctr is never reset!
    renameCtr: Word;
    renameIndex: InsTag;	-- group + group position
    intPointer: SmallNumber;
    floatPointer: SmallNumber;
        bqPointer: SmallNumber;
    commitCtr: Word;
end record;

type InstructionState is record
	controlInfo: InstructionControlInfo;
	ip: Mword;
	bits: Word; -- instruction word
	tags: InstructionTags;
	specificOperation: SpecificOp;
	classInfo: InstructionClassInfo;
	constantArgs: InstructionConstantArgs;
	virtualArgSpec: InstructionArgSpec;
	physicalArgSpec: InstructionArgSpec;
	result: Mword;
	target: Mword;
end record;

type InstructionStateArray is array(integer range <>) of InstructionState;

constant DEFAULT_CONTROL_INFO: InstructionControlInfo := (
                                    completed => '0',
                                    completed2 => '0',
                                    newEvent => '0',
                                    hasInterrupt => '0',
                                    hasException => '0',
                                    refetch => '0',
                                    frontBranch => '0',
                                    confirmedBranch => '0',												    											
                                    specialAction => '0',
                                    dbtrap => '0',
                                    orderViolation => '0',
                                    tlbMiss => '0',
                                    dataMiss => '0',
                                    sqMiss => '0',
                                    firstBr => '0'
                                 );

constant DEFAULT_CLASS_INFO: InstructionClassInfo := ( 
                                    short => '0',
                                    mainCluster => '0',
                                    secCluster => '0',
                                    fpRename => '0',
                                    branchIns => '0',
                                    useLQ => '0'									
                                  );


constant DEFAULT_CONSTANT_ARGS: InstructionConstantArgs := ('0', (others=>'0'));

constant DEFAULT_ARG_SPEC: InstructionArgSpec := (
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
			renameCtr => (others => '0'),
			renameIndex => (others => '0'),
			intPointer => (others => '0'),
			floatPointer => (others => '0'),
			    bqPointer => (others => '0'),
			commitCtr => (others => '0')
);

constant DEFAULT_INSTRUCTION_STATE: InstructionState := (
	controlInfo => DEFAULT_CONTROL_INFO,
	ip => (others => '0'),
	bits => (others=>'0'),
	
	specificOperation => DEFAULT_SPECIFIC_OP,
	tags => DEFAULT_INSTRUCTION_TAGS,
	classInfo => DEFAULT_CLASS_INFO,
	constantArgs => DEFAULT_CONSTANT_ARGS,
	virtualArgSpec => DEFAULT_ARG_SPEC,
	physicalArgSpec => DEFAULT_ARG_SPEC,
	result => (others => '0'),
	target => (others => '0')
);

constant DEFAULT_INS_STATE: InstructionState := DEFAULT_INSTRUCTION_STATE;
	
-- Created to enable *Array				
type InstructionSlot is record 
	full: std_logic;
	ins: InstructionState;
end record;
	
constant DEFAULT_INSTRUCTION_SLOT: InstructionSlot := ('0', DEFAULT_INSTRUCTION_STATE);
constant DEFAULT_INS_SLOT: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;

-- NOTE: index can be negative to enable logical division into 2 different ranges 
type InstructionSlotArray is array(integer range <>) of InstructionSlot;


type SchedulerState is record
	issued: std_logic;
    newInQueue: std_logic;
    immediate: std_logic;
    zero: std_logic_vector(0 to 2);
    readyNow: std_logic_vector(0 to 2);
    readyNext: std_logic_vector(0 to 2);
    readyM2:    std_logic_vector(0 to 2);
    locs: SmallNumberArray(0 to 2);
    nextLocs: SmallNumberArray(0 to 2);
    locsM2: SmallNumberArray(0 to 2);
    missing: std_logic_vector(0 to 2);
    stored:  std_logic_vector(0 to 2);
    args: MwordArray(0 to 2);
    
    argLocsPipe: SmallNumberArray(0 to 2);
    argLocsPhase: SmallNumberArray(0 to 2);	
end record;

constant DEFAULT_SCHEDULER_STATE: SchedulerState := ( 
              issued => '0',
	          newInQueue => '0',
			  immediate => '0',
			  zero => (others => '0'),
			  readyNow => (others=>'0'),
			  readyNext => (others=>'0'),
			  readyM2 => (others => '0'),
			  locs => (others => (others => '0')),
			  nextLocs => (others => (others => '0')),
			  locsM2 => (others => (others => '0')),
			  missing => (others=>'0'),
			  stored => (others => '0'),
			  args => (others => (others=>'0')),
		      argLocsPipe => (others => (others => '0')),
			  argLocsPhase => (others => (others => '0'))
			  );

constant DEFAULT_SCHED_STATE: SchedulerState := DEFAULT_SCHEDULER_STATE;
																				
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


    
subtype PipeStage is InstructionSlotArray(0 to PIPE_WIDTH-1);
type PipeStageArray is array(natural range <>) of PipeStage;

constant DEFAULT_PIPE_STAGE: PipeStage := (others => DEFAULT_INS_SLOT); 


end InstructionState;



package body InstructionState is     

function sop(sub: SubpipeType; func: ArithOp) return SpecificOp is
    variable res: SpecificOp := DEFAULT_SPECIFIC_OP;
begin
    res.subpipe := ALU;
    res.arith := func;
    res.bits := i2slv(ArithOp'pos(func), OP_VALUE_BITS);
    return res;
end function;

function sop(sub: SubpipeType; func: MemOp) return SpecificOp is
    variable res: SpecificOp := DEFAULT_SPECIFIC_OP;
begin
    res.subpipe := Mem;
    res.memory := func;
    res.bits := i2slv(MemOp'pos(func), OP_VALUE_BITS);
    return res;
end function;

function sop(sub: SubpipeType; func: FpOp) return SpecificOp is
    variable res: SpecificOp := DEFAULT_SPECIFIC_OP;
begin
    res.subpipe := FP;
    res.float := func;
    res.bits := i2slv(FpOp'pos(func), OP_VALUE_BITS);
    return res;
end function;

function sop(sub: SubpipeType; func: SysOp) return SpecificOp is
    variable res: SpecificOp := DEFAULT_SPECIFIC_OP;
begin
    res.subpipe := None;
    res.system := func;
    res.bits := i2slv(SysOp'pos(func), OP_VALUE_BITS);
    return res;
end function;



function findLog2(n: positive) return natural is
    variable i: natural := 0;
    variable pow2: positive := 1;
begin
    loop
        if pow2 >= n then
            return i;
        end if;
        i := i + 1;
        pow2 := 2*pow2;
    end loop;
    return 0;    
end function;

function getSpecificOpSize return positive is
    variable res: positive := 1;
    constant A: natural := ArithOp'pos(ArithOp'high) - ArithOp'pos(ArithOp'low) + 1;
    constant M: natural := MemOp'pos(MemOp'high) - MemOp'pos(MemOp'low) + 1;
    constant F: natural := FpOp'pos(FpOp'high) - FpOp'pos(FpOp'low) + 1;
    constant S: natural := SysOp'pos(SysOp'high) - SysOp'pos(SysOp'low) + 1;
    variable maxAM, maxAMF, maxAMFS, maxAll: natural := 0;
    variable i: positive := 1;
    variable pow2: positive := 2;
begin
    if A > M then
       maxAM := A;
    else
       maxAM := M;
    end if;
    
    if maxAM > F then
        maxAMF := maxAM;
    else
        maxAMF := F;
    end if;
    
    if maxAMF > S then
        maxAMFS := maxAMF;
    else
        maxAMFS := S;
    end if;
    maxAll := maxAMFS;
    
    return findLog2(maxAll);
end function;
	

end InstructionState;
