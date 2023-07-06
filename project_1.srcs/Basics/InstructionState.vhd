

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

                arithAdd, 
                    arithAddH,
                arithSub, arithSha,
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

type ArithOp is (opAnd, opOr, opXor, opAdd, opAddH, opSub, opShl, opSha, opRot, opJz, opJnz, opJ, opMul, opMulHS, opMulHU, opDivU, opDivS, opRemU, opRemS);

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

constant DEFAULT_SPECIFIC_OP: SpecificOp := (   
    subpipe => None,
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

type DummyType is (DUMMY_VALUE);

type InstructionDebugInfo is record
    -- pragma synthesis off
    cycle: Word;
    index: Word;
    seqNum: Word;
    tag: InsTag;
    commit: Word;
    adr: Mword;
    bits: Word;
    str: string(1 to 30);
    -- pragma synthesis on
    
    dummy: DummyType;
end record;


type InstructionControlInfo is record
    full: std_logic;
	newEvent: std_logic; -- True if any new event appears
	hasInterrupt: std_logic;   -- CP
	hasException: std_logic;
    refetch: std_logic;
    frontBranch: std_logic;
    confirmedBranch: std_logic;
	specialAction: std_logic;
	dbtrap: std_logic;
    orderViolation: std_logic;
    tlbMiss: std_logic;         -- MQ
    dataMiss: std_logic;        -- MQ
    sqMiss:    std_logic;       -- MQ
    firstBr: std_logic;
    killed: std_logic;
    causing: std_logic;
    ignored: std_logic;
end record;

type InstructionControlInfo_T is record
	specialAction_T: std_logic;
    firstBr_T: std_logic;
end record;


type ClassInfo_Dispatch is record
	storeInt: std_logic;
    storeFP: std_logic;
    useAlu: std_logic;
    useMul: std_logic;
    useDiv: std_logic;
    useMem: std_logic;
    useFP: std_logic;
end record;


type InstructionClassInfo is record
	mainCluster: std_logic; --
	secCluster: std_logic; --
	branchIns: std_logic; --
	useLQ: std_logic; --
	useSQ: std_logic; -- 
	useFP: std_logic; -- true if instruction is routed to FP renamer (NOTE, CHECK: Int renamer is used for all ops, even those that don't go to any IQ)
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
    renameIndex: InsTag;	-- group + group position
    intPointer: SmallNumber;
    floatPointer: SmallNumber;
    bqPointer: SmallNumber;
    sqPointer: SmallNumber;
    lqPointer: SmallNumber;
    bqPointerSeq: SmallNumber;
end record;


type BufferEntry is record
    dbInfo: InstructionDebugInfo;
    full: std_logic;
    firstBr: std_logic; -- TEMP

    -- NOTE: for compresion maybe can be just 2 bits:
    --       (br NT, br T, br T confirmed, special) is 4 possibilities     
    frontBranch: std_logic;     -- seems unused
    confirmedBranch: std_logic; -- seems unused
    specialAction: std_logic;

    classInfo: InstructionClassInfo;
    specificOperation: SpecificOp;

    constantArgs: InstructionConstantArgs;
    argSpec: InstructionArgSpec;
end record;


type InstructionState is record
    dbInfo: InstructionDebugInfo;
	controlInfo: InstructionControlInfo_T;
	tags: InstructionTags;
	specificOperation: SpecificOp;
	typeInfo: InstructionClassInfo;
	dispatchInfo: ClassInfo_Dispatch;

	constantArgs: InstructionConstantArgs;
	virtualArgSpec: InstructionArgSpec;
	--physicalArgSpec: InstructionArgSpec;
	   dest_T: PhysName; 
end record;

type InstructionStateArray is array(integer range <>) of InstructionState;

type ControlPacket is record
    dbInfo: InstructionDebugInfo;
    controlInfo: InstructionControlInfo;
    classInfo: InstructionClassInfo;
    op: SpecificOp;
    tags: InstructionTags;

    tag: InsTag;
    ip: Mword;
    nip: Mword;
    target: Mword;
end record;

type ControlPacketArray is array(integer range <>) of ControlPacket;



-- DB stuff
type DbDependency is record
    -- pragma synthesis off
    producer: InsTag;
    cyclesWaiting: integer;
    cyclesReady: integer;
    -- pragma synthesis on
    
    dummy: DummyType;
end record;

type DbDependencyArray is array(natural range <>) of DbDependency;


-- Scheduler structure
type ArgumentState is record
    used: std_logic;
    reg: PhysName;
    iqTag: SmallNumber;
    zero: std_logic;
    imm: std_logic;
    value: Hword;

    --activeCounter: SmallNumber;
    readyCtr: SmallNumber;
    failed: std_logic;

    waiting: std_logic;
    stored:  std_logic;
    srcPipe: SmallNumber;
    srcStage: SmallNumber;
    
    dbDep: DbDependency;   
end record;


type ArgumentStateArray is array(natural range <>) of ArgumentState;

-- Scheduler structure

type StaticInfo is record
    dbInfo: InstructionDebugInfo;
    operation: SpecificOp;
    branchIns: std_logic;
            divIns: std_logic;
    tags: InstructionTags;    
    immediate: std_logic;    
    immValue: Hword;
    zero: std_logic_vector(0 to 2);    
end record;


--type StaticInfoArray is array(natural range <>) of StaticInfo;

-- Scheduler structure

type EntryState is (empty, suspended, active, issued);

type EntryStatus is record
    state: EntryState;

    active: std_logic;
    suspend: std_logic;
    issued: std_logic;
    freed: std_logic;
    trial: std_logic;
    --stageCtr: SmallNumber;
    issuedCtr: SmallNumber;
end record;

--type IqState is (empty, active, issued, suspended, freed);
subtype IqState is EntryState;

type IqStateArray is array(natural range <>) of IqState;

type IqEvent is (none,
                    insert, -- empty -> active/suspended
                    kill,   --  killMask(i)
                    issue,  -- active -> issued
                    retract, -- issued -> active/suspended
                    retire); -- issued -> empty and not killMask(i)

type IqEventArray is array(natural range <>) of IqEvent;

type DynamicInfo is record
    full: std_logic;
    status: EntryStatus;

    renameIndex: InsTag;
    intDestSel: std_logic;
    floatDestSel: std_logic;
    dest: PhysName;

    argStates: ArgumentStateArray(0 to 2);

    --    db0, db1, db2, db3, db4, db5: std_logic;
end record;


-- Scheduler structure

type SchedulerInfo is record
    dynamic: DynamicInfo;
    static: StaticInfo;
end record;

type SchedulerInfoArray is array(natural range <>) of SchedulerInfo;

type SchedulerState is record
    full: std_logic;

    st: StaticInfo;

    argSpec: InstructionArgSpec;
    destTag: SmallNumber;
    poisoned: std_logic;

    readNew: std_logic_vector(0 to 2);
    args: MwordArray(0 to 2);

    argLocsPipe: SmallNumberArray(0 to 2);
    argSrc: SmallNumberArray(0 to 2);
end record;


-- Created to enable *Array				
type InstructionSlot is record 
	full: std_logic;
	ins: InstructionState;
end record;
	

-- NOTE: index can be negative to enable logical division into 2 different ranges 
type InstructionSlotArray is array(integer range <>) of InstructionSlot;

type ExecResult is record
    dbInfo: InstructionDebugInfo;
    full: std_logic;
    failed: std_logic;
    tag: InsTag;
    dest: PhysName;
    value: Mword;
end record;

type ExecResult_N is record
    dbInfo: InstructionDebugInfo;
    full: std_logic;
    failed: std_logic;
    tag: InsTag;
    iqTag: SmallNumber;
    dest: PhysName;
    value: Mword;
end record;

type ExecResultArray is array(integer range <>) of ExecResult;


type BufferEntryArray is array(0 to PIPE_WIDTH-1) of BufferEntry;
type BufferEntryArray2D is array(0 to IBUFFER_SIZE-1, 0 to PIPE_WIDTH-1) of BufferEntry;


-- Default values

constant DEFAULT_DEBUG_INFO: InstructionDebugInfo := (
    -- pragma synthesis off
    cycle => (others => 'U'),
    index => (others => 'U'),
    seqNum => (others => 'U'),
    tag => (others => 'U'),
    commit => (others => 'U'),
    adr => (others => 'U'),
    bits => (others => 'U'),
    str => (others => ' '),
    -- pragma synthesis on
 
    dummy => DUMMY_VALUE
);


constant DEFAULT_CLASS_INFO_DISPATCH: ClassInfo_Dispatch := (others => '0');

constant DEFAULT_INSTRUCTION_TAGS: InstructionTags := (
    renameIndex => (others => '0'),
    intPointer => (others => '0'),
    floatPointer => (others => '0'),
    bqPointer => (others => '0'),
    sqPointer => (others => '0'),
    lqPointer => (others => '0'),
    bqPointerSeq => (others => '0')
);


constant DEFAULT_STATIC_INFO: StaticInfo := (
    dbInfo => DEFAULT_DEBUG_INFO,
    operation => DEFAULT_SPECIFIC_OP,
    branchIns => '0',
        divIns => '0',
    tags => DEFAULT_INSTRUCTION_TAGS,
    immediate => '0',
    immValue => (others => '0'),
    zero => (others => '0')    
);


constant DEFAULT_CONTROL_INFO: InstructionControlInfo := (
    full => '0',
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
    firstBr => '0',
    killed => '0',  -- ??
    causing => '0', -- ??
    ignored => '0'  -- ??
);

    constant DEFAULT_CONTROL_INFO_T: InstructionControlInfo_T := (											    											
        specialAction_T => '0',
        firstBr_T => '0'
    );

constant DEFAULT_CLASS_INFO: InstructionClassInfo := ( 
    mainCluster => '0',
    secCluster => '0',
    branchIns => '0',
    useLQ => '0',
	useSQ => '0',
    useFP => '0'
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


constant DEFAULT_INSTRUCTION_STATE: InstructionState := (
    dbInfo => DEFAULT_DEBUG_INFO,
	controlInfo => DEFAULT_CONTROL_INFO_T,	
	specificOperation => DEFAULT_SPECIFIC_OP,
	tags => DEFAULT_INSTRUCTION_TAGS,
	typeInfo => DEFAULT_CLASS_INFO,
	dispatchInfo => DEFAULT_CLASS_INFO_DISPATCH,
	constantArgs => DEFAULT_CONSTANT_ARGS,
	virtualArgSpec => DEFAULT_ARG_SPEC,
	--physicalArgSpec => DEFAULT_ARG_SPEC,
	   dest_T => (others => '0') 
);

constant DEFAULT_INS_STATE: InstructionState := DEFAULT_INSTRUCTION_STATE;

constant DEFAULT_CONTROL_PACKET: ControlPacket := (
    dbInfo => DEFAULT_DEBUG_INFO,

    controlInfo => DEFAULT_CONTROL_INFO,
    classInfo => DEFAULT_CLASS_INFO,
    op => DEFAULT_SPECIFIC_OP,
    tags => DEFAULT_INSTRUCTION_TAGS,
    tag => (others => '0'),
    ip => (others => '0'),
    nip => (others => '0'),
    target => (others => '0')
);


constant DEFAULT_INSTRUCTION_SLOT: InstructionSlot := ('0', DEFAULT_INSTRUCTION_STATE);
constant DEFAULT_INS_SLOT: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;

constant DEFAULT_EXEC_RESULT: ExecResult := (
    DEFAULT_DEBUG_INFO,
    '0',
    '0',
    tag => (others => '0'),
    dest => (others => '0'),
    value => (others => '0')
);

constant DEFAULT_EXEC_RESULT_N: ExecResult_N := (
    DEFAULT_DEBUG_INFO,
    '0',
    '0',
    tag => (others => '0'),
    iqTag => (others => '0'),
    dest => (others => '0'),
    value => (others => '0')
);

constant DEFAULT_BUFFER_ENTRY: BufferEntry := (
    dbInfo => DEFAULT_DEBUG_INFO,
    specificOperation => sop(None, opNone),
    classInfo => DEFAULT_CLASS_INFO,
    constantArgs => DEFAULT_CONSTANT_ARGS,
    argSpec => DEFAULT_ARG_SPEC,
    others => '0'
);

constant DEFAULT_SCHEDULER_STATE: SchedulerState := (
      full => '0',

      st => DEFAULT_STATIC_INFO,

      argSpec => DEFAULT_ARG_SPEC,
      destTag => (others => '0'),

      poisoned => '0',

      readNew => (others => '0'),
      args => (others => (others=>'0')),
      argLocsPipe => (others => (others => '0')),
      argSrc => (others => (others => '0'))
      );

constant DEFAULT_SCHED_STATE: SchedulerState := DEFAULT_SCHEDULER_STATE;


constant DEFAULT_DB_DEPENDENCY: DbDependency := (
                                    -- pragma synthesis off
                                    producer => (others => 'U'),
                                    cyclesWaiting => -1,
                                    cyclesReady => -1,
                                    -- pragma synthesis on

                                    dummy => DUMMY_VALUE
                                    );

constant DEFAULT_ARGUMENT_STATE: ArgumentState := (
    used => '0',
    reg => (others => '0'),
    iqTag => (others => '0'),
    zero => '0',
    imm => '0',
    value => (others => '0'),
    --activeCounter => (others => '0'),
    readyCtr => (others => '0'),
    failed => '0',
    waiting => '0',
    stored => '0',
    srcPipe => (others => '0'),
    srcStage => (others => '0'),
    
    dbDep => DEFAULT_DB_DEPENDENCY
); 

constant DEFAULT_ARG_STATE: ArgumentState := DEFAULT_ARGUMENT_STATE;

constant DEFAULT_ENTRY_STATUS: EntryStatus := (
    state => empty,

    active => '0',
    suspend => '0',
    issued => '0',
    freed => '0',
    trial => '0',
    --stageCtr => (others => '0'),
    issuedCtr => (others => '0')
);

constant DEFAULT_DYNAMIC_INFO: DynamicInfo := (
    full => '0',

    status => DEFAULT_ENTRY_STATUS,
    renameIndex => (others => '0'),
    intDestSel => '0',
    floatDestSel => '0',
    dest => (others => '0'),
    argStates => (others => DEFAULT_ARG_STATE)
            --db0 => '0', db1 => '0', db2 => '0', db3 => '0', db4 => '0', db5 => '0'
);

constant DEFAULT_SCHEDULER_INFO: SchedulerInfo := (
    DEFAULT_DYNAMIC_INFO,
    DEFAULT_STATIC_INFO
);


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
