--

--

library IEEE;
use IEEE.STD_LOGIC_1164.all;


use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.ForwardingNetwork.all;


package LogicIssue is

type StaticInfo is record
    operation: SpecificOp;
    
    branchIns: std_logic;
    bqPointer: SmallNumber;
    sqPointer: SmallNumber;
    lqPointer: SmallNumber;        
    
    immediate: std_logic;    
    immValue: Hword;
    zero: std_logic_vector(0 to 2);    
end record;

constant DEFAULT_STATIC_INFO: StaticInfo := (
    operation => DEFAULT_SPECIFIC_OP,
    branchIns => '0',
    
    bqPointer => (others => '0'),
    sqPointer => (others => '0'),
    lqPointer => (others => '0'),  
    
    immediate => '0',
    immValue => (others => '0'),
    zero => (others => '0')    
);

type StaticInfoArray is array(natural range <>) of StaticInfo;

type DynamicInfo is record
    full: std_logic;

    issued: std_logic;
    newInQueue: std_logic;

    renameIndex: InsTag;
    argSpec: InstructionArgSpec;

    staticPtr: SmallNumber; -- points to entry with static info

    stored:  std_logic_vector(0 to 2);
    missing: std_logic_vector(0 to 2);
    readyNow: std_logic_vector(0 to 2);
    readyNext: std_logic_vector(0 to 2);
    readyM2:    std_logic_vector(0 to 2);

    argLocsPipe: SmallNumberArray(0 to 2);
    argLocsPhase: SmallNumberArray(0 to 2);
end record;


constant DEFAULT_DYNAMIC_INFO: DynamicInfo := (
    full => '0',

    issued => '0',
    newInQueue => '0',

    renameIndex => (others => '0'),
    argSpec => DEFAULT_ARG_SPEC,

    staticPtr => (others => '0'),

    stored => (others => '0'),
    missing => (others => '0'),
    readyNow => (others => '0'),
    readyNext => (others => '0'),
    readyM2 => (others => '0'),

    argLocsPipe => (others => (others => '0')),
    argLocsPhase => (others => (others => '0'))
);

type DynamicInfoArray is array(natural range <>) of DynamicInfo;

type SchedulerInfo is record
    dynamic: DynamicInfo;
    static: StaticInfo;
end record;

constant DEFAULT_SCHEDULER_INFO: SchedulerInfo := (
    DEFAULT_DYNAMIC_INFO,
    DEFAULT_STATIC_INFO
);

type SchedulerInfoArray is array(natural range <>) of SchedulerInfo;

type Wakeups is record
    matchR1: std_logic_vector(0 to 2);
    matchR0: std_logic_vector(0 to 2);
    matchM1: std_logic_vector(0 to 2);
    matchM2: std_logic_vector(0 to 2);
    
    argLocsPipe: SmallNumberArray(0 to 2);
    argLocsPhase: SmallNumberArray(0 to 2);
    wakeupVec: std_logic_vector(0 to 2);
    
    match:   std_logic;
    multiMatch: std_logic;
    const:   std_logic; -- immediate or PR 0
    missing: std_logic;
        wakeup: std_logic;           
end record;

type WakeupStruct is record
    argLocsPipe: SmallNumber;
    argLocsPhase: SmallNumber;
    --wakeupVec: std_logic_vector(0 to 2);
    match:   std_logic;         
end record;

constant DEFAULT_WAKEUP_STRUCT: WakeupStruct := ((others => '0'), "00000010", '0');
type WakeupArray2D is array(natural range <>, natural range <>) of WakeupStruct;



type WakeupsArray2D is array(natural range <>, natural range <>) of Wakeups;

function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean) return StaticInfo; 
function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean) return DynamicInfo;

function getIssueStaticInfoArray(insVec: InstructionSlotArray; constant HAS_IMM: boolean) return StaticInfoArray;
function getIssueDynamicInfoArray(insVec: InstructionSlotArray; stA: StaticInfoArray; constant HAS_IMM: boolean) return DynamicInfoArray;

function getIssueInfoArray(insVec: InstructionSlotArray; constant HAS_IMM: boolean) return SchedulerInfoArray;


function getSchedEntrySlot(info: SchedulerInfo) return SchedulerEntrySlot;
function getSchedEntrySlotArray(infoA: SchedulerInfoArray) return SchedulerEntrySlotArray;


function TMP_restoreState(full: std_logic; ins: InstructionState; st: SchedulerState) return SchedulerEntrySlot;

function TMP_prepareDispatchSlot(input: SchedulerEntrySlot; prevSending: std_logic) return SchedulerEntrySlot;

function getDispatchArgValues_NEW(input: SchedulerEntrySlot;
                                    fni: ForwardingInfo;
                                    prevSending: std_logic;
                                    USE_IMM: boolean; REGS_ONLY: boolean)
return SchedulerEntrySlot;

function updateDispatchArgs_NEW(st: SchedulerState; vals: MwordArray; regValues: MwordArray; TMP_DELAY: boolean) return SchedulerState;

function TMP_setUntil(selVec: std_logic_vector; nextAccepting: std_logic) return std_logic_vector;

function iqContentNext(queueContent: SchedulerInfoArray; inputDataS: SchedulerInfoArray;
                         killMask, selMask: std_logic_vector;
                         livingMaskInput, selMaskInput: std_logic_vector;
                         sends, sent, sentUnexpected, prevSending: std_logic)
return SchedulerInfoArray;

function extractReadyMask(entryVec: SchedulerInfoArray) return std_logic_vector;

function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector;

function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; dynamic: boolean; forwardingModes: ForwardingModeArray;
                                TMP_NEW: boolean)
return SchedulerInfoArray;


        function getWakeupArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; dynamic: boolean; forwardingModes: ForwardingModeArray;
            TMP_NEW: boolean)
        return WakeupArray2D;

function findForwardingMatchesArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo) return ForwardingMatchesArray;

function extractFullMask(queueContent: SchedulerInfoArray) return std_logic_vector;


function prioSelect(elems: SchedulerInfoArray; selVec: std_logic_vector) return SchedulerInfo;
function iqInputStageNext(content, newContent: SchedulerInfoArray; selMask: std_logic_vector; prevSending, isSending, execEventSignal, lateEventSignal: std_logic) return SchedulerInfoArray;
function updateRR(newContent: SchedulerInfoArray; rr: std_logic_vector) return SchedulerInfoArray;

function getKillMask(content: SchedulerInfoArray; causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector;


    function prioSelect16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return SchedulerInfo;


        function indl16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return std_logic_vector;
        function indh16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return std_logic_vector;


end LogicIssue;



package body LogicIssue is

function extractFullMask(queueContent: SchedulerInfoArray) return std_logic_vector is
    variable res: std_logic_vector(0 to queueContent'length-1) := (others => '0');
begin
    for i in res'range loop
        res(i) := queueContent(i).dynamic.full;
    end loop;
    return res;
end function;

function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean) return StaticInfo is
    variable res: StaticInfo;
begin
    res.operation := isl.ins.specificOperation;

    res.branchIns := isl.ins.classInfo.branchIns;
    res.bqPointer := isl.ins.tags.bqPointer;
    res.sqPointer := isl.ins.tags.sqPointer;
    res.lqPointer := isl.ins.tags.lqPointer;        
    
    res.immediate := isl.ins.constantArgs.immSel and bool2std(HAS_IMM);    
    res.immValue := isl.ins.constantArgs.imm(15 downto 0);
    
    for j in 0 to 2 loop
        res.zero(j) :=         (isl.ins.physicalArgSpec.intArgSel(j) and not isNonzero(isl.ins.virtualArgSpec.args(j)(4 downto 0)))
                                       or (not isl.ins.physicalArgSpec.intArgSel(j) and not isl.ins.physicalArgSpec.floatArgSel(j));
    end loop;
    
    if HAS_IMM and isl.ins.constantArgs.immSel = '1' then
        res.zero(1) := '1';
    
        if IMM_AS_REG then    
            if CLEAR_DEBUG_INFO then
                res.immValue(PhysName'length-1 downto 0) := (others => '0');
            end if;
        end if;
    end if;
    
    if not HAS_IMM then
        res.immediate := '0';            
    end if;        
                   
    return res;
end function; 


function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean) return DynamicInfo is
    variable res: DynamicInfo;
begin
    res.full := isl.full;

    res.issued := '0';
    res.newInQueue := '1';
    
    res.renameIndex := isl.ins.tags.renameIndex;
    res.argSpec := isl.ins.physicalArgSpec;
    
    res.staticPtr := (others => '0'); -- points to entry with static info
    
    res.stored := (others => '0');

    res.missing := (isl.ins.physicalArgSpec.intArgSel and not stInfo.zero)
                                       or (isl.ins.physicalArgSpec.floatArgSel);                                   
    res.readyNow := (others => '0');
    res.readyNext := (others => '0');
    res.readyM2  := (others => '0');

    res.argLocsPipe := (others => (others => '0'));
    res.argLocsPhase:= (others => "00000010");
    
    if HAS_IMM and isl.ins.constantArgs.immSel = '1' then
        if IMM_AS_REG then
            res.argSpec.args(1) := isl.ins.constantArgs.imm(PhysName'length-1 downto 0);
        end if;
    end if;
                 
    return res;
end function; 

function getIssueStaticInfoArray(insVec: InstructionSlotArray; constant HAS_IMM: boolean) return StaticInfoArray is
    variable res: StaticInfoArray(insVec'range);
begin
    for i in res'range loop
        res(i) := getIssueStaticInfo(insVec(i), HAS_IMM);
    end loop;
    return res;
end function;

function getIssueDynamicInfoArray(insVec: InstructionSlotArray; stA: StaticInfoArray; constant HAS_IMM: boolean) return DynamicInfoArray is
    variable res: DynamicInfoArray(insVec'range);
begin
    for i in res'range loop
        res(i) := getIssueDynamicInfo(insVec(i), stA(i), HAS_IMM);
    end loop;
    return res;
end function;

function getIssueInfoArray(insVec: InstructionSlotArray; constant HAS_IMM: boolean) return SchedulerInfoArray is
    variable res: SchedulerInfoArray(insVec'range);
begin
    for i in res'range loop
        res(i).static := getIssueStaticInfo(insVec(i), HAS_IMM);
        res(i).dynamic := getIssueDynamicInfo(insVec(i), res(i).static, HAS_IMM);
    end loop;
    return res;
end function;


function getSchedEntrySlot(info: SchedulerInfo) return SchedulerEntrySlot is
    variable res: SchedulerEntrySlot;
begin
    res.ins := DEFAULT_INS_STATE;

    res.state.operation := info.static.operation;

    res.state.branchIns := info.static.branchIns;
    res.state.bqPointer := info.static.bqPointer;
    res.state.sqPointer := info.static.sqPointer;
    res.state.lqPointer := info.static.lqPointer;        
    
    res.state.immediate := info.static.immediate;    
    res.state.immValue := info.static.immValue;
        
    res.state.zero := info.static.zero;

    res.full := info.dynamic.full;

    res.state.issued := info.dynamic.issued;
    res.state.newInQueue := info.dynamic.newInQueue;
    
    res.state.renameIndex := info.dynamic.renameIndex;
    res.state.argSpec := info.dynamic.argSpec;
    
    res.state.stored := info.dynamic.stored;

    res.state.missing := info.dynamic.missing;
    
    res.state.readyNow := info.dynamic.readyNow;
    res.state.readyNext := info.dynamic.readyNext;
    res.state.readyM2  := info.dynamic.readyM2;

    res.state.argLocsPipe := info.dynamic.argLocsPipe;
    res.state.argLocsPhase:= info.dynamic.argLocsPhase;

    res.state.args := (others => (others => '0'));

    return res;
end function;

function getSchedEntrySlotArray(infoA: SchedulerInfoArray) return SchedulerEntrySlotArray is
    variable res: SchedulerEntrySlotArray(infoA'range);
begin
    for i in res'range loop
        res(i) := getSchedEntrySlot(infoA(i));
    end loop;
    return res;
end function;


function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector is
	variable res: std_logic_vector(list'range) := (others => '0');
begin
	for i in list'range loop
		if tag(PHYS_REG_BITS-1 downto 0) = list(i)(PHYS_REG_BITS-1 downto 0) then
			res(i) := '1';
		end if;
	end loop;
	return res;
end function;


function updateIssueArgLocs(ss: SchedulerState)
return SchedulerState is
	variable res: SchedulerState := ss;
begin
	for i in 0 to 1 loop
        case res.argLocsPhase(i)(2 downto 0) is
            when "110" =>
                res.argLocsPhase(i) := "00000111";        
            when "111" =>
                res.argLocsPhase(i) := "00000000";
            when "000" =>
                res.argLocsPhase(i) := "00000001";				
            when others =>
                res.argLocsPhase(i) := "00000010";
        end case;

		res.argLocsPhase(i)(7 downto 3) := "00000";
		res.argLocsPipe(i)(7 downto 3) := "00000";
	end loop;

	return res;
end function;


function updateArgInfo(ss: DynamicInfo;
                       arg: natural;
                       wakeups: WakeupStruct)
return DynamicInfo is
    variable res: DynamicInfo := ss;
    variable wakeupVec: std_logic_vector(0 to 2) := (others => '0');
    variable wakeupPhases: SmallNumberArray(0 to 2) := (others => (others => '0'));  
begin
    if ss.missing(arg) = '1' then
        res.argLocsPhase(arg) := wakeups.argLocsPhase;
        res.argLocsPipe(arg) := wakeups.argLocsPipe;
        res.missing(arg) := not wakeups.match;
    else -- update loc
        case ss.argLocsPhase(arg)(2 downto 0) is
            when "110" =>
                res.argLocsPhase(arg) := "00000111";
            when "111" =>
                res.argLocsPhase(arg) := "00000000";
            when "000" =>
                res.argLocsPhase(arg) := "00000001";                
            when others =>
                res.argLocsPhase(arg) := "00000010";
        end case;     
    end if;

    return res;
end function;


function TMP_restoreState(full: std_logic; ins: InstructionState; st: SchedulerState) return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	variable v0, v1: std_logic_vector(1 downto 0) := "00";
	variable selected0, selected1: Mword := (others => '0');
	variable ready: std_logic_vector(0 to 2) := (others=>'0');
	variable locs: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	constant Z3: std_logic_vector(0 to 2) := (others => '0');
	constant ZZ3: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	variable imm: Word := (others => '0');
begin
    res.full := full;
	res.ins := ins;
	res.state := st;

    res.ins.tags.renameIndex := st.renameIndex;
    res.ins.tags.bqPointer := st.bqPointer;
    res.ins.tags.sqPointer := st.sqPointer;
    res.ins.tags.lqPointer := st.lqPointer;

    res.ins.specificOperation := st.operation;

    res.ins.physicalArgSpec.dest := res.state.argSpec.dest;
    res.ins.physicalArgSpec.intDestSel := res.state.argSpec.intDestSel;
    res.ins.physicalArgSpec.floatDestSel := res.state.argSpec.floatDestSel;
    
    res.ins.physicalArgSpec.intArgSel := (others => '0');
    res.ins.physicalArgSpec.floatArgSel := (others => '0');
        
    res.ins.physicalArgSpec.args := res.state.argSpec.args;

    -- Clear dest if empty
    if not res.full = '1' then
        res.ins.physicalArgSpec.intDestSel := '0';
        res.ins.physicalArgSpec.floatDestSel := '0';
        res.ins.physicalArgSpec.dest := (others => '0');
    end if;
    
    -- Clear unused fields       
    if CLEAR_DEBUG_INFO then
        res.ins := clearAbstractInfo(res.ins);
    end if;
    res.ins.controlInfo.newEvent := '0';
    res.ins.controlInfo.hasInterrupt := '0';
        
	return res;
end function;


function TMP_prepareDispatchSlot(input: SchedulerEntrySlot; prevSending: std_logic) return SchedulerEntrySlot is
    variable res: SchedulerEntrySlot := input;
begin
    if prevSending = '0' or (input.state.argSpec.intDestSel = '0' and input.state.argSpec.floatDestSel = '0') then
        res.ins.physicalArgSpec.dest := (others => '0'); -- Don't allow false notifications of args
        res.state.argSpec.dest := (others => '0'); -- Don't allow false notifications of args
    end if;
    
    return res;
end function;


function getDispatchArgValues_NEW(input: SchedulerEntrySlot;
                                    fni: ForwardingInfo;
                                    prevSending: std_logic;
                                    USE_IMM: boolean; REGS_ONLY: boolean)
return SchedulerEntrySlot is
    variable res: SchedulerEntrySlot := input;
begin

    if res.state.zero(0) = '1' then
        res.state.args(0) := (others => '0');
        res.state.stored(0) := '1';
    elsif res.state.argLocsPhase(0)(1 downto 0) = "00" then
        res.state.args(0) := fni.values0(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
        res.state.stored(0) := '1';
    else --elsif res.state.argPhase(1 downto 0) := "01" then
        res.state.args(0) := fni.values1(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
        if res.state.argLocsPhase(0)(1 downto 0) = "01" then
            res.state.stored(0) := '1';
        end if;
    end if;

    if res.state.zero(1) = '1' then
        if USE_IMM then
            res.state.args(1)(31 downto 16) := (others => res.state.immValue(15));
            res.state.args(1)(15 downto 0) := res.state.immValue;
            if IMM_AS_REG then
                res.state.args(1)(PhysName'length-1 downto 0) := res.state.argSpec.args(1);
            end if;                                                               
        else
            res.state.args(1) := (others => '0');
        end if;
        res.state.stored(1) := '1';
    elsif res.state.argLocsPhase(1)(1 downto 0) = "00" then
        res.state.args(1) := fni.values0(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
        res.state.stored(1) := '1';
    else --elsif res.state.argPhase(1 downto 0) := "01" then
        res.state.args(1) := fni.values1(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
        if res.state.argLocsPhase(1)(1 downto 0) = "01" then
            res.state.stored(1) := '1';
        end if;				
    end if;
            
    if REGS_ONLY then
        res.state.stored := (others => '0');
    end if;

    res.state := updateIssueArgLocs(res.state);
    
    return res;
end function;


function updateDispatchArgs_NEW(st: SchedulerState; vals: MwordArray; regValues: MwordArray; TMP_DELAY: boolean)
return SchedulerState is
    variable res: SchedulerState := st;
begin
    if TMP_DELAY then
        return res;
    end if;

    if res.stored(0) = '1' then
        null; -- Using stored arg
    elsif res.argLocsPhase(0)(1 downto 0) = "00" then -- Forwarding from new outputs
        res.args(0) := vals(slv2u(res.argLocsPipe(0)(1 downto 0)));
    else
        res.args(0) := regValues(0);
    end if;
    res.stored(0) := '1';

    if res.stored(1) = '1' then
        null; -- Using stored arg
    elsif res.argLocsPhase(1)(1 downto 0) = "00" then -- Forwarding from new outputs
        res.args(1) := vals(slv2u(res.argLocsPipe(1)(1 downto 0)));
    else
        res.args(1) := regValues(1);
    end if;
    res.stored(1) := '1';         

    return res;
end function;



function extractReadyMask(entryVec: SchedulerInfoArray) return std_logic_vector is
    variable res: std_logic_vector(entryVec'range);
begin	
    for i in res'range loop
        res(i) := not isNonzero(entryVec(i).dynamic.missing(0 to 1))      and not entryVec(i).dynamic.issued;
    end loop;
    return res;
end function;

function TMP_getIssuedMask(elems: SchedulerInfoArray) return std_logic_vector is
    variable res: std_logic_vector(0 to elems'length-1) := (others => '0');
begin
    for i in 0 to elems'length-1 loop
        res(i) := elems(i).dynamic.issued;
    end loop;
    return res;
end function;

function TMP_setUntil(selVec: std_logic_vector; nextAccepting: std_logic) return std_logic_vector is
    variable res: std_logic_vector(0 to selVec'length-1) := (others => '0');
begin
    for i in res'range loop
        if (selVec(i) and nextAccepting) = '1' then
            exit;
        else
            res(i) := '1';
        end if;
    end loop;
    return res;
end function;


function getNewElemSch_N(remv: std_logic_vector; newContent: SchedulerInfoArray)
return SchedulerInfo is
    variable res: SchedulerInfo := newContent(0);
    variable inputMask: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
    variable sel: std_logic_vector(1 downto 0) := "00";
    variable remVec: std_logic_vector(0 to 2) := remv;               
begin
    inputMask := extractFullMask(newContent);
    sel := getSelector(remVec, inputMask(0 to 2));
    res := newContent(slv2u(sel));        
    return res;    
end function;
	
function iqContentNext(queueContent: SchedulerInfoArray; inputDataS: SchedulerInfoArray;
                         killMask, selMask: std_logic_vector;
                         livingMaskInput, selMaskInput: std_logic_vector;
                         sends, sent, sentUnexpected, prevSending: std_logic)
return SchedulerInfoArray is
	constant QUEUE_SIZE: natural := queueContent'length;
	variable res: SchedulerInfoArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO);
	
	-- TODO: change newMask to use living rather than full
	   -- !!!! this is useless because killing is ordered: positions in compacted mask are not affected, only the total numer of living elems	
	constant newMask: std_logic_vector(0 to PIPE_WIDTH-1) := --extractFullMask(inputDataS);
	                                                           livingMaskInput;
	constant compMask: std_logic_vector(0 to PIPE_WIDTH-1) := compactMask(newMask);
	variable dataNewDataS: SchedulerInfoArray(0 to PIPE_WIDTH-1) := inputDataS;
	
	variable iqDataNextS: SchedulerInfoArray(0 to QUEUE_SIZE - 1) := (others => DEFAULT_SCHEDULER_INFO);
	variable iqFullMaskNext: std_logic_vector(0 to QUEUE_SIZE - 1) :=	(others => '0');
    variable iqRemainingMaskSh: std_logic_vector(0 to QUEUE_SIZE + 4 - 1) := (others => '0');

	variable xVecS: SchedulerInfoArray(0 to QUEUE_SIZE + PIPE_WIDTH - 1);
	constant fullMask: std_logic_vector(0 to QUEUE_SIZE-1) := extractFullMask(queueContent);
	constant issuedMask: std_logic_vector(0 to QUEUE_SIZE-1) := TMP_getIssuedMask(queueContent);
	constant remainMask: std_logic_vector(0 to QUEUE_SIZE-1) := TMP_setUntil(issuedMask, '1');
	
	variable fullMaskSh: std_logic_vector(0 to QUEUE_SIZE-1) := fullMask;
	constant livingMask: std_logic_vector(0 to QUEUE_SIZE-1) := fullMask and not killMask;	
	variable livingMaskSh: std_logic_vector(0 to QUEUE_SIZE-1) := livingMask;
	variable fillMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');	
begin
        for i in 0 to PIPE_WIDTH-1 loop
            dataNewDataS(i).dynamic.issued := selMaskInput(i); -- To preserve 'issued' state if being issued from input stage
        end loop;

	-- Important, new instrucitons in queue must be marked!	
	for i in 0 to PIPE_WIDTH-1 loop
		dataNewDataS(i).dynamic.newInQueue := '1';
	end loop;
  
	xVecS := queueContent & dataNewDataS;
	
	-- What is being issued now is marked
    for i in 0 to QUEUE_SIZE-1 loop
        if selMask(i) = '1' and sends = '1' then
            xVecS(i).dynamic.issued := '1';
        end if;
        
        -- Retraction into IQ when sending turns out disallowed
        if issuedMask(i) = '1' and sentUnexpected = '1' then
        --    xVecS(i).state.issued := '0';
        end if;  
    end loop;	
	
	xVecS(QUEUE_SIZE) := xVecS(QUEUE_SIZE-1);
	for i in 0 to QUEUE_SIZE + PIPE_WIDTH - 1 loop
		xVecS(i).dynamic.newInQueue := '0';
	end loop;

	for i in 0 to QUEUE_SIZE-2 loop
		livingMaskSh(i) := livingMask(i) and (livingMask(i+1) or not sent);
		fullMaskSh(i) := fullMask(i) and (fullMask(i+1) or not sent);			
	end loop;	livingMaskSh(QUEUE_SIZE-1) := livingMask(QUEUE_SIZE-1) and ('0' or not sent);
	fullMaskSh(QUEUE_SIZE-1) := fullMask(QUEUE_SIZE-1) and ('0' or not sent);

	-- Now assign from x or y
	iqRemainingMaskSh(0 to 3) := (others => '1');
	iqRemainingMaskSh(4 to QUEUE_SIZE + 4 - 1) := fullMaskSh;
	iqDataNextS := queueContent;
	for i in 0 to QUEUE_SIZE-1 loop		    
        fillMask(i) := '0';
        for k in 0 to 3 loop -- Further beyond end requires more full inputs to be filled: !! equiv to remainingMask(-1-k), where '1' for k < 0
            fillMask(i) := fillMask(i) or (iqRemainingMaskSh(i + 3-k) and compMask(k));
        end loop;
	    
	    
	    -- TODO: don't put '1' for input slots that are killed - use livingVec for input group
		iqFullMaskNext(i) := livingMaskSh(i) or (fillMask(i) and prevSending);
		if fullMaskSh(i) = '1' then -- From x	
			if remainMask(i) = '1' then
				iqDataNextS(i) := xVecS(i);
			else
				iqDataNextS(i) := xVecS(i + 1);
			end if;
		else -- From y
			iqDataNextS(i) := getNewElemSch_N(iqRemainingMaskSh(i+1 to i+3), dataNewDataS);
		end if;
	end loop;

	-- Fill output array
	for i in 0 to res'right loop
	   res(i) := iqDataNextS(i);	
	   res(i).dynamic.full := iqFullMaskNext(i);
	   res(i).dynamic.stored := (others => '0');
	end loop;

	return res;
end function;

    
function getWakeupStruct(arg: natural; fnm: ForwardingMap; cmpR1, cmpR0, cmpM1, cmpM2, cmpM3: std_logic_vector) return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    variable nMatches: natural;
    variable matchR1, matchR0, matchM1, matchM2, matchM3: std_logic_vector(0 to 2) := (others => '0');
begin
    matchR1 := cmpR1 and fnm.maskR1;
    matchR0 := cmpR0 and fnm.maskR0;
    matchM1 := cmpM1 and fnm.maskM1;
    matchM2 := cmpM2 and fnm.maskM2;
    matchM3 := cmpM3 and fnm.maskM3;

    res.match := isNonzero(matchR1 or matchR0 or matchM1 or matchM2);
    
    if isNonzero(cmpR1) = '1' then
        res.argLocsPhase := "00000010";
        res.argLocsPipe := i2slv(getFirstOnePosition(cmpR1), 8);
    elsif isNonzero(cmpR0) = '1' then
        res.argLocsPhase := "00000001";
        res.argLocsPipe := i2slv(getFirstOnePosition(cmpR0), 8);            
    elsif isNonzero(cmpM1) = '1' then
        res.argLocsPhase := "00000000";
        res.argLocsPipe := i2slv(getFirstOnePosition(cmpM1), 8);            
    elsif isNonzero(cmpM2) = '1' then
        res.argLocsPhase := "00000111";
        res.argLocsPipe := i2slv(getFirstOnePosition(cmpM2), 8);
    elsif isNonzero(cmpM3) = '1' then
        res.argLocsPhase := "00000110";
        res.argLocsPipe := i2slv(getFirstOnePosition(cmpM2), 8); 
    else
        res.argLocsPhase := "00000010";                                                            
    end if;
        
    return res;
end function;


function getWakeupStructStatic(arg: natural; fnm: ForwardingMap; cmpR1, cmpR0, cmpM1, cmpM2, cmpM3: std_logic_vector; forwardingModes: ForwardingModeArray) return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    variable nMatches: natural;
    variable matchVec, matchR1, matchR0, matchM1, matchM2, matchM3: std_logic_vector(0 to 2) := (others => '0');
begin
    matchR1 := cmpR1;
    matchR0 := cmpR0;
    matchM1 := cmpM1;
    matchM2 := cmpM2;
    matchM3 := cmpM3;
    
    for p in forwardingModes'range loop
        case forwardingModes(p).stage is
            when -3 =>
                matchVec(p) := matchM3(p);
            when -2 =>
                matchVec(p) := matchM2(p);
            when -1 =>
                matchVec(p) := matchM1(p);
            when 0 =>
                matchVec(p) := matchR0(p);
            when 1 =>
                matchVec(p) := matchR1(p);
            when others =>
                matchVec(p) := '0';
        end case;
    
        if matchVec(p) = '1' then
            res.argLocsPipe(2 downto 0) := i2slv(p, 3);
            res.argLocsPhase(2 downto 0) := i2slv(forwardingModes(p).stage + 1, 3);
        end if;
    end loop;
    
    res.match := isNonzero(matchVec);
    
    return res;
end function;

function getWakeupStructDynamic(arg: natural; fnm: ForwardingMap; cmpR1, cmpR0, cmpM1, cmpM2, cmpM3: std_logic_vector; forwardingModes: ForwardingModeArray) return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    variable nMatches: natural;
    variable matchVec, matchR1, matchR0, matchM1, matchM2, matchM3: std_logic_vector(0 to 2) := (others => '0');
begin
    matchR1 := cmpR1;
    matchR0 := cmpR0;
    matchM1 := cmpM1;
    matchM2 := cmpM2;
    matchM3 := cmpM3;
    
    for p in forwardingModes'range loop
        for q in -3 to 1 loop
            if forwardingModes(p).stage <= q then
                case q is
                    when -3 =>
                        matchVec(p) := matchM3(p);
                    when -2 =>
                        matchVec(p) := matchM2(p);
                    when -1 =>
                        matchVec(p) := matchM1(p);
                    when 0 =>
                        matchVec(p) := matchR0(p);
                    when 1 =>
                        matchVec(p) := matchR1(p);
                    when others =>
                        matchVec(p) := '0';
                end case;
                
                if matchVec(p) = '1' then
                    res.argLocsPipe(2 downto 0) := i2slv(p, 3);
                    res.argLocsPhase(2 downto 0) := i2slv(q + 1, 3);
                    exit;
                end if;               
            end if;
        end loop;
    end loop;
    
    res.match := isNonzero(matchVec);
    
    return res;
end function;


function updateSchedulerState(state: SchedulerInfo;
                                fni: ForwardingInfo;
                                fm: ForwardingMatches;
                                fnm: ForwardingMap;
                                    TMP_NEW: boolean;
                                dynamic: boolean;
                                forwardingModes: ForwardingModeArray)
return SchedulerInfo is
	variable res: SchedulerInfo := state;
	variable wakeups0, wakeups1: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
begin
    if TMP_NEW then
        wakeups0 := getWakeupStructStatic(0, fnm, fm.a0cmp1, fm.a0cmp0, fm.a0cmpM1, fm.a0cmpM2, fm.a0cmpM3, forwardingModes);
        wakeups1 := getWakeupStructStatic(1, fnm, fm.a1cmp1, fm.a1cmp0, fm.a1cmpM1, fm.a1cmpM2, fm.a1cmpM3, forwardingModes);
    else
        --wakeups0 := getWakeupStruct      (0, fnm, fm.a0cmp1, fm.a0cmp0, fm.a0cmpM1, fm.a0cmpM2);
        --wakeups1 := getWakeupStruct      (1, fnm, fm.a1cmp1, fm.a1cmp0, fm.a1cmpM1, fm.a1cmpM2);
        
        wakeups0 := getWakeupStructDynamic(0, fnm, fm.a0cmp1, fm.a0cmp0, fm.a0cmpM1, fm.a0cmpM2, fm.a0cmpM3, forwardingModes);
        wakeups1 := getWakeupStructDynamic(1, fnm, fm.a1cmp1, fm.a1cmp0, fm.a1cmpM1, fm.a1cmpM2, fm.a1cmpM3, forwardingModes);
    end if;

    res.dynamic := updateArgInfo(res.dynamic, 0, wakeups0);
    res.dynamic := updateArgInfo(res.dynamic, 1, wakeups1);

	return res;
end function;

function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; dynamic: boolean; forwardingModes: ForwardingModeArray;
            TMP_NEW: boolean)
return SchedulerInfoArray is
	variable res: SchedulerInfoArray(0 to schedArray'length-1);-- := insArray;
begin
	for i in schedArray'range loop
		res(i) := updateSchedulerState(schedArray(i), fni, fma(i), fnm, TMP_NEW, dynamic, forwardingModes);
	end loop;	
	return res;
end function;


        function getWakeupArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; dynamic: boolean; forwardingModes: ForwardingModeArray;
            TMP_NEW: boolean)
        return WakeupArray2D is
            variable res: WakeupArray2D(schedArray'range, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));
        begin
            for i in res'range loop
                if TMP_NEW then
                    res(i, 0) := getWakeupStructStatic(0, fnm, fma(i).a0cmp1, fma(i).a0cmp0, fma(i).a0cmpM1, fma(i).a0cmpM2, fma(i).a0cmpM3, forwardingModes);
                    res(i, 1) := getWakeupStructStatic(1, fnm, fma(i).a1cmp1, fma(i).a1cmp0, fma(i).a1cmpM1, fma(i).a1cmpM2, fma(i).a1cmpM3, forwardingModes);
                else
                    res(i, 0) := getWakeupStruct(0, fnm, fma(i).a0cmp1, fma(i).a0cmp0, fma(i).a0cmpM1, fma(i).a0cmpM2, fma(i).a0cmpM3);
                    res(i, 1) := getWakeupStruct(1, fnm, fma(i).a1cmp1, fma(i).a1cmp0, fma(i).a1cmpM1, fma(i).a1cmpM2, fma(i).a1cmpM3);
                end if;
            end loop;
            
            return res;
        end function;


function findForwardingMatches(info: SchedulerInfo; fni: ForwardingInfo) return ForwardingMatches is
    variable res: ForwardingMatches := DEFAULT_FORWARDING_MATCHES;
begin
	res.a0cmp0 := findRegTag(info.dynamic.argSpec.args(0), fni.tags0);
    res.a1cmp0 := findRegTag(info.dynamic.argSpec.args(1), fni.tags0);
    res.a0cmp1 := findRegTag(info.dynamic.argSpec.args(0), fni.tags1);
    res.a1cmp1 := findRegTag(info.dynamic.argSpec.args(1), fni.tags1);
    res.a0cmpM1 := findRegTag(info.dynamic.argSpec.args(0), fni.nextTagsM1);
    res.a1cmpM1 := findRegTag(info.dynamic.argSpec.args(1), fni.nextTagsM1);
    res.a0cmpM2 := findRegTag(info.dynamic.argSpec.args(0), fni.nextTagsM2);
    res.a1cmpM2 := findRegTag(info.dynamic.argSpec.args(1), fni.nextTagsM2); 
    
    return res;
end function;

function findForwardingMatchesArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo) return ForwardingMatchesArray is
    variable res: ForwardingMatchesArray(schedArray'range) := (others => DEFAULT_FORWARDING_MATCHES);
begin
    for i in schedArray'range loop
        res(i) := findForwardingMatches(schedArray(i), fni);
    end loop;
    return res;
end function;



function prioSelect(elems: SchedulerInfoArray; selVec: std_logic_vector) return SchedulerInfo is
    variable ind, ind0, ind1: std_logic_vector(2 downto 0) := "000";
    variable ch0, ch1: SchedulerInfo;
begin
    if selVec(0 to 3) = "0000" then
        ind(2) := '1';
    else
        ind(2) := '0';
    end if;
    
    if selVec(0) = '1' then
        ch0 := elems(0);
    elsif selVec(1) = '1' then
        ch0 := elems(1);
    elsif selVec(2) = '1' then
        ch0 := elems(2);
    else
        ch0 := elems(3);
    end if;

    if selVec(4) = '1' then
        ch1 := elems(4);
    elsif selVec(5) = '1' then
        ch1 := elems(5);
    elsif selVec(6) = '1' then
        ch1 := elems(6);
    else
        ch1 := elems(7);
    end if;

    if ind(2) = '0' then
        return ch0;
    else
        return ch1;
    end if;
end function;


    
    function getIndex4(inSelVec: std_logic_vector) return std_logic_vector is
        constant selVec: std_logic_vector(0 to 3) := inSelVec;
        variable res: std_logic_vector(1 downto 0) := "11";
    begin
        case selVec is
            when "1000" | "1001" | "1010" | "1011" | "1100" | "1101" | "1110" | "1111" =>
                res := "00";
            when "0100" | "0101" | "0110" | "0111"  =>
                res := "01";
            when "0010" | "0011" =>
                res := "10";                                    
            when others =>
                res := "11";
        end case;

        return res;
    end function;

    function select4(inElems: SchedulerInfoArray; index: std_logic_vector(1 downto 0)) return SchedulerInfo is
        constant elems: SchedulerInfoArray(0 to 3) := inElems;
    begin
        case index is
            when "00" =>    
                return elems(0);
            when "01" =>
                return elems(1);
            when "10" =>
                return elems(2);
            when others =>
                return elems(3);
        end case;                
    end function;
    

    function prioSelect16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return SchedulerInfo is
        variable elems: SchedulerInfoArray(0 to 15) := (others => DEFAULT_SCHEDULER_INFO);
        
        variable selVec: std_logic_vector(0 to 15) := (others => '0');
        variable selVec4: std_logic_vector(0 to 3) := (others => '0');
        
        variable indL0, indL1, indL2, indL3, indH: std_logic_vector(1 downto 0) := "11";
        variable ch0, ch1, ch2, ch3: SchedulerInfo;
        variable groupReady: std_logic_vector(0 to 3) := (others => '0');
    begin
        elems(0 to inputElems'length-1) := inputElems;
        selVec(0 to inputElems'length-1) := inputSelVec;
    
        for i in 0 to 3 loop
            selVec4 := selVec(4*i to 4*i + 3);
            groupReady(i) := isNonzero(selVec4);
        end loop;

        indL0 := getIndex4(selVec(0 to 3));
        indL1 := getIndex4(selVec(4 to 7));
        indL2 := getIndex4(selVec(8 to 11));
        indL3 := getIndex4(selVec(12 to 15));
        
        ch0 := select4(elems(0 to 3), indL0);
        ch1 := select4(elems(4 to 7), indL1);
        ch2 := select4(elems(8 to 11), indL2);
        ch3 := select4(elems(12 to 15), indL3);

        indH := getIndex4(groupReady);
        
        return select4((0 => ch0, 1 => ch1, 2=> ch2, 3 => ch3), indH);        
    end function;


        function indl16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return std_logic_vector is
            variable elems: SchedulerInfoArray(0 to 15) := (others => DEFAULT_SCHEDULER_INFO);
            
            variable selVec: std_logic_vector(0 to 15) := (others => '0');
            variable selVec4: std_logic_vector(0 to 3) := (others => '0');
            
            variable indL0, indL1, indL2, indL3, indH: std_logic_vector(1 downto 0) := "11";
            variable ch0, ch1, ch2, ch3: SchedulerInfo;
            variable groupReady: std_logic_vector(0 to 3) := (others => '0');
        begin
            elems(0 to inputElems'length-1) := inputElems;
            selVec(0 to inputElems'length-1) := inputSelVec;
        
            for i in 0 to 3 loop
                selVec4 := selVec(4*i to 4*i + 3);
                groupReady(i) := isNonzero(selVec4);
            end loop;
    
            indL0 := getIndex4(selVec(0 to 3));
            indL1 := getIndex4(selVec(4 to 7));
            indL2 := getIndex4(selVec(8 to 11));
            indL3 := getIndex4(selVec(12 to 15));
            
            ch0 := select4(elems(0 to 3), indL0);
            ch1 := select4(elems(4 to 7), indL1);
            ch2 := select4(elems(8 to 11), indL2);
            ch3 := select4(elems(12 to 15), indL3);
    
            indH := getIndex4(groupReady);
            
            case indH is
                when "00" => return indL0;
                when "01" => return indL1;
                when "10" => return indL2;
                when others => return indL3;
            end case;
            
            --return select4((0 => ch0, 1 => ch1, 2=> ch2, 3 => ch3), indH);        
        end function;

        function indh16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return std_logic_vector is
            variable elems: SchedulerInfoArray(0 to 15) := (others => DEFAULT_SCHEDULER_INFO);
            
            variable selVec: std_logic_vector(0 to 15) := (others => '0');
            variable selVec4: std_logic_vector(0 to 3) := (others => '0');
            
            variable indL0, indL1, indL2, indL3, indH: std_logic_vector(1 downto 0) := "11";
            variable ch0, ch1, ch2, ch3: SchedulerInfo;
            variable groupReady: std_logic_vector(0 to 3) := (others => '0');
        begin
            elems(0 to inputElems'length-1) := inputElems;
            selVec(0 to inputElems'length-1) := inputSelVec;
        
            for i in 0 to 3 loop
                selVec4 := selVec(4*i to 4*i + 3);
                groupReady(i) := isNonzero(selVec4);
            end loop;
    
            indL0 := getIndex4(selVec(0 to 3));
            indL1 := getIndex4(selVec(4 to 7));
            indL2 := getIndex4(selVec(8 to 11));
            indL3 := getIndex4(selVec(12 to 15));
            
            ch0 := select4(elems(0 to 3), indL0);
            ch1 := select4(elems(4 to 7), indL1);
            ch2 := select4(elems(8 to 11), indL2);
            ch3 := select4(elems(12 to 15), indL3);
    
            indH := getIndex4(groupReady);
            
            return indH;
            
            --return select4((0 => ch0, 1 => ch1, 2=> ch2, 3 => ch3), indH);        
        end function;



function iqInputStageNext(content, newContent: SchedulerInfoArray; selMask: std_logic_vector; prevSending, isSending, execEventSignal, lateEventSignal: std_logic) return SchedulerInfoArray is
   variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content;
begin
      -- Handle ops leaving the "subqueue" when it's stalled
      for i in 0 to PIPE_WIDTH-1 loop
         if res(i).dynamic.issued = '1' then
            res(i).dynamic.full := '0';
         end if;
         res(i).dynamic.issued := selMask(i);
      end loop;

   if execEventSignal = '1' or lateEventSignal = '1' then
       for i in 0 to PIPE_WIDTH-1 loop
           res(i).dynamic.full := '0';
           
       end loop;
       
   elsif prevSending = '1' then
       res := newContent;       
   elsif isSending = '1' then -- Clearing everything - sent to main queue
       for i in 0 to PIPE_WIDTH-1 loop
           res(i).dynamic.full := '0';
       end loop;    
   end if;
   
        if prevSending = '0' then
            for i in res'range loop
                res(i).dynamic.newInQueue := '0';
            end loop;
        end if;
   
   return res;
end function;

function updateRR(newContent: SchedulerInfoArray; rr: std_logic_vector) return SchedulerInfoArray is
   variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := newContent;
   variable rrf: std_logic_vector(0 to 2) := (others=>'0');      	   
begin
   for i in 0 to PIPE_WIDTH-1 loop
       rrf := rr(3*i to 3*i + 2);                                           
       res(i).dynamic.missing := res(i).dynamic.missing and not rrf;	       
   end loop;
   
   for i in 1 to PIPE_WIDTH-1 loop
       res(i).dynamic.renameIndex := clearTagLow(res(0).dynamic.renameIndex) or i2slv(i, TAG_SIZE);
   end loop;         
   return res;
end function;

function getKillMask(content: SchedulerInfoArray; causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector is
    variable res: std_logic_vector(0 to content'length-1);
begin
    for i in 0 to content'length-1 loop
        res(i) := killByTag(compareTagBefore(causing.tags.renameIndex, content(i).dynamic.renameIndex),
                                    execEventSig, lateEventSig);
    end loop;
    return res;
end function;

end LogicIssue;
