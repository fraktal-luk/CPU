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
        
        type WakeupsArray2D is array(natural range <>, natural range <>) of Wakeups;
        
--            function getWakeupsForArg(arg: natural;
--                                            fni: ForwardingInfo;
--                                            fm: ForwardingMatches;
--                                            fnm: ForwardingMap; progressLocs, dynamic: boolean)
--            return Wakeups;

--            function getWakeupsForArg_CMP(arg: natural;
--                                            fni: ForwardingInfo;
--                                            fm: ForwardingMatches;
--                                            fnm: ForwardingMap; progressLocs, dynamic: boolean)
--            return Wakeups;        

        function getWakeups(state: SchedulerInfo; arg: natural; cmpR1, cmpR0, cmpM1, cmpM2: std_logic_vector) return Wakeups;
        function getWakeupsAllArgs(stateArray: SchedulerInfoArray; fma: ForwardingMatchesArray) return WakeupsArray2D;

        function getWakeupsTest(stateArrayBefore, stateArrayAfter: SchedulerInfoArray; fma: ForwardingMatchesArray) return WakeupsArray2D;
        function cmpWakeups(state: SchedulerInfoArray; a, b: WakeupsArray2D) return std_logic_vector;


function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean) return StaticInfo; 
function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean) return DynamicInfo;

function getIssueStaticInfoArray(insVec: InstructionSlotArray; constant HAS_IMM: boolean) return StaticInfoArray;
function getIssueDynamicInfoArray(insVec: InstructionSlotArray; stA: StaticInfoArray; constant HAS_IMM: boolean) return DynamicInfoArray;

function getIssueInfoArray(insVec: InstructionSlotArray; constant HAS_IMM: boolean) return SchedulerInfoArray;


function getSchedEntrySlot(info: SchedulerInfo) return SchedulerEntrySlot;
function getSchedEntrySlotArray(infoA: SchedulerInfoArray) return SchedulerEntrySlotArray;


function TMP_restoreState(full: std_logic; ins: InstructionState; st: SchedulerState) return SchedulerEntrySlot;

function getDispatchArgValues(ins: InstructionState; st: SchedulerState; fni: ForwardingInfo;
											prevSending: std_logic;
											USE_IMM: boolean; REGS_ONLY, DELAY_ONLY, TMP_DELAY: boolean)
return SchedulerEntrySlot;

function updateDispatchArgs(ins: InstructionState; st: SchedulerState; vals: MwordArray; regValues: MwordArray)
return SchedulerEntrySlot;

function TMP_setUntil(selVec: std_logic_vector; nextAccepting: std_logic) return std_logic_vector;

function iqContentNext(queueContent: SchedulerInfoArray; inputDataS: SchedulerInfoArray;
                                 killMask, selMask: std_logic_vector;
								 sends, sent, sentUnexpected, prevSending: std_logic)
return SchedulerInfoArray;

function extractReadyMask(entryVec: SchedulerInfoArray) return std_logic_vector;

function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector;

function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; progressLocs, dynamic: boolean)
return SchedulerInfoArray;


--    function updateSchedulerArray_DYN(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; progressLocs, dynamic: boolean)
--    return SchedulerInfoArray;


function findForwardingMatchesArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo) return ForwardingMatchesArray;

function extractFullMask(queueContent: SchedulerInfoArray) return std_logic_vector;


function prioSelect(elems: SchedulerInfoArray; selVec: std_logic_vector) return SchedulerInfo;
function iqInputStageNext(content, newContent: SchedulerInfoArray; prevSending, isSending, execEventSignal, lateEventSignal: std_logic) return SchedulerInfoArray;
function updateRR(newContent: SchedulerInfoArray; rr: std_logic_vector) return SchedulerInfoArray;

function getKillMask(content: SchedulerInfoArray; fullMask: std_logic_vector; causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector;


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



function getWakeupPhase(fnm: ForwardingMap; ready1, ready0, readyM1, readyM2: std_logic_vector; constant progress: boolean) return SmallNumberArray is
    variable res: SmallNumberArray(0 to 2) := (others => "11111110"); -- -2 
    variable phase: integer := 2; 
begin
    for i in res'range loop
    
                if ready1(i) = '1' then
                    phase := 1;
                elsif ready0(i) = '1' then
                    phase := 0;
                elsif readyM1(i) = '1' then
                    phase := -1;
                elsif readyM2(i) = '1' then
                    phase := -2;
                end if;    
                
                if progress then
                    phase := phase + 1; -- Don't check if 2 because it's not possible here
                end if;
                
                res(i)(1 downto 0) := i2slv(phase, 2);
                res(i)(7 downto 2) := (others => '0');
    
--        if progress then
--            if ready1(i) = '1' then
--                res(i) := "00000010";        
--            elsif ready0(i) = '1' then
--                res(i) := "00000001";
--            elsif readyM1(i) = '1' then
--                res(i) := "00000000";
--            else
--                res(i) := "11111111";                       
--            end if;    
--        else
--            if ready1(i) = '1' then
--                res(i) := "00000001";        
--            elsif ready0(i) = '1' then
--                res(i) := "00000000";
--            elsif readyM1(i) = '1' then
--                res(i) := "11111111";
--            else
--                res(i) := "11111110";
--            end if;
--        end if;
--        res(i)(7 downto 2) := (others => '0');
    end loop;

    return res;
end function;


--function getWakeupVector(fnm: ForwardingMap; ready1, ready0, readyM1, readyM2: std_logic_vector; constant dynamic: boolean) return std_logic_vector is
--    variable res: std_logic_vector(0 to 2) := (others => '0');
--begin
--    for i in res'range loop
----        if dynamic then
----            res(i) := ready1(i) and ready0(i) and readyM1(i) and readyM2(i);
----        else
--            if fnm.maskR1(i) = '1' then
--                res(i) := ready1(i);
--            elsif fnm.maskR0(i) = '1' then
--                res(i) := ready0(i);
--            elsif fnm.maskM1(i) = '1' then
--                res(i) := readyM1(i);                       
--            else
--                res(i) := readyM2(i);  
--            end if;
----        end if;
--    end loop;

--    return res;
--end function;

    function getWakeupVectorDynamic(fnm: ForwardingMap; ready1, ready0, readyM1, readyM2: std_logic_vector; constant dynamic: boolean) return std_logic_vector is
        variable res: std_logic_vector(0 to 2) := (others => '0');
    begin
        for i in res'range loop
            if dynamic then
                res(i) := ready1(i) or ready0(i) or readyM1(i) or readyM2(i);
            else
                if fnm.maskR1(i) = '1' then
                    res(i) := ready1(i);
                elsif fnm.maskR0(i) = '1' then
                    res(i) := ready0(i);
                elsif fnm.maskM1(i) = '1' then
                    res(i) := readyM1(i);                       
                else
                    res(i) := readyM2(i);  
                end if;
            end if;
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
        case res.argLocsPhase(i)(1 downto 0) is
            when "11" =>
                res.argLocsPhase(i) := "00000000";
            when "00" =>
                res.argLocsPhase(i) := "00000001";				
            when others =>
                res.argLocsPhase(i) := "00000010";
        end case;

		res.argLocsPhase(i)(7 downto 2) := "000000";
		res.argLocsPipe(i)(7 downto 2) := "000000";
	end loop;

	return res;
end function;


function updateArgLocs(ss: DynamicInfo;
                              readyBefore: std_logic_vector;
                              wakeupPhases0, wakeupPhases1: SmallNumberArray;
							  wakeupVec0, wakeupVec1: std_logic_vector;
							  progress: boolean)
return DynamicInfo is
	variable res: DynamicInfo := ss;
	variable wakeupVec: std_logic_vector(0 to 2) := (others => '0');
	variable wakeupPhases: SmallNumberArray(0 to 2) := (others => (others => '0'));  
begin
	for i in 0 to 1 loop
	   if i = 0 then
	       wakeupVec := wakeupVec0;
	       wakeupPhases := wakeupPhases0;
	   elsif i = 1 then
	       wakeupVec := wakeupVec1;
	       wakeupPhases := wakeupPhases1;
	   end if;
	
        if readyBefore(i) = '1' then
            if progress then
                case res.argLocsPhase(i)(1 downto 0) is
                    when "11" =>
                        res.argLocsPhase(i) := "00000000";
                    when "00" =>
                        res.argLocsPhase(i) := "00000001";				
                    when others =>
                        res.argLocsPhase(i) := "00000010";
                end case;
            end if;
        else
            for j in 0 to 2 loop
                if wakeupVec(j) = '1' then
                    res.argLocsPipe(i) := i2slv(j, SMALL_NUMBER_SIZE);                    
                    res.argLocsPhase(i) := wakeupPhases(j);
                    exit;                    
                end if;
            end loop;
        end if;
       
		res.argLocsPhase(i)(7 downto 2) := "000000";
		res.argLocsPipe(i)(7 downto 2) := "000000";
	end loop;

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


function getDispatchArgValues(ins: InstructionState; st: SchedulerState; fni: ForwardingInfo;
											prevSending: std_logic;
											USE_IMM: boolean; REGS_ONLY, DELAY_ONLY, TMP_DELAY: boolean)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
begin
	res.ins := ins;
	res.state := st;
    
    if TMP_DELAY then
        return res;
    end if;
    
    if prevSending = '0' or (st.argSpec.intDestSel = '0' and st.argSpec.floatDestSel = '0') then
        res.ins.physicalArgSpec.dest := (others => '0'); -- Don't allow false notifications of args
        res.state.argSpec.dest := (others => '0'); -- Don't allow false notifications of args
    end if;

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

    if REGS_ONLY or DELAY_ONLY then
        res.state.stored := (others => '0');
    end if;

    res.state := updateIssueArgLocs(res.state);
    
    if CLEAR_DEBUG_INFO then
        res.ins := clearDbCounters(res.ins);
        res.ins := clearAbstractInfo(res.ins);
        
        res.ins.controlInfo := DEFAULT_CONTROL_INFO;

        res.ins.classInfo := DEFAULT_CLASS_INFO;
        res.ins.constantArgs.imm := (others => '0');

        res.ins.virtualArgSpec := DEFAULT_ARG_SPEC;
    end if;
    
	return res;
end function;


function updateDispatchArgs(ins: InstructionState; st: SchedulerState; vals: MwordArray; regValues: MwordArray)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
begin
	res.ins := ins;
	res.state := st;

    if res.state.stored(0) = '1' then
        null; -- Using stored arg
    elsif res.state.argLocsPhase(0)(1 downto 0) = "00" then -- Forwarding from new outputs
        res.state.args(0) := vals(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
    else
        res.state.args(0) := regValues(0);
    end if;
        res.state.stored(0) := '1';

    if res.state.stored(1) = '1' then
        null; -- Using stored arg
    elsif res.state.argLocsPhase(1)(1 downto 0) = "00" then -- Forwarding from new outputs
        res.state.args(1) := vals(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
    else
        res.state.args(1) := regValues(1);
    end if;
	   res.state.stored(1) := '1';
	   
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
								 sends, sent, sentUnexpected, prevSending: std_logic)
return SchedulerInfoArray is
	constant QUEUE_SIZE: natural := queueContent'length;
	variable res: SchedulerInfoArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_SCHEDULER_INFO); 	
	constant newMask: std_logic_vector(0 to PIPE_WIDTH-1) := extractFullMask(inputDataS);--inputData.fullMask;--
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
	end loop;
	livingMaskSh(QUEUE_SIZE-1) := livingMask(QUEUE_SIZE-1) and ('0' or not sent);
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


--    function getWakeupsForArg(arg: natural;
--                                    fni: ForwardingInfo;
--                                    fm: ForwardingMatches;
--                                    fnm: ForwardingMap; progressLocs, dynamic: boolean)
--    return Wakeups is
--        variable res: Wakeups;
        
--	    variable cmp0toM2, cmp0toM1, cmp0toR0, cmp0toR1, cmp1toM2, cmp1toM1, cmp1toR0, cmp1toR1,
--                    readyNew, readyBefore, wakeupVec0, wakeupVec1: std_logic_vector(0 to 2) := (others=>'0');
--        variable wakeupPhases0, wakeupPhases1: SmallNumberArray(0 to 2) := (others=>(others=>'0'));            
--    begin
--        cmp0toR0 := fm.a0cmp0 and fnm.maskR0;
--        cmp1toR0 := fm.a1cmp0 and fnm.maskR0;
--        cmp0toR1 := fm.a0cmp1 and fnm.maskR1;
--        cmp1toR1 := fm.a1cmp1 and fnm.maskR1;
--        cmp0toM1 := fm.a0cmpM1 and fnm.maskM1;
--        cmp1toM1 := fm.a1cmpM1 and fnm.maskM1;
--        cmp0toM2 := fm.a0cmpM2 and fnm.maskM2;
--        cmp1toM2 := fm.a1cmpM2 and fnm.maskM2;
    
--        if dynamic then
--            wakeupPhases0 := getWakeupPhase(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, progressLocs);
--            wakeupPhases1 := getWakeupPhase(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, progressLocs);
--        else
--            wakeupPhases0 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
--            wakeupPhases1 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
--        end if;
    
--        wakeupVec0 := getWakeupVectorDynamic(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, dynamic);  
--        wakeupVec1 := getWakeupVectorDynamic(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, dynamic);
     
--        if arg = 0 then
--            res.matchR1 := cmp0toR1;
--            res.matchR0 := cmp0toR0;
--            res.matchM1 := cmp0toM1;
--            res.matchM2 := cmp0toM2;
            
--            res.argLocsPhase := wakeupPhases0;
            
--            res.wakeupVec := wakeupVec0;
--        else
--            res.matchR1 := cmp1toR1;
--            res.matchR0 := cmp1toR0;
--            res.matchM1 := cmp1toM1;
--            res.matchM2 := cmp1toM2;
            
--            res.argLocsPhase := wakeupPhases1;
            
--            res.wakeupVec := wakeupVec1;                                
--        end if;
--                res.wakeupVec(2) := '0';
        
--        return res;
--    end function;

--    function getWakeupsForArg_CMP(arg: natural;
--                                    fni: ForwardingInfo;
--                                    fm: ForwardingMatches;
--                                    fnm: ForwardingMap; progressLocs, dynamic: boolean)
--    return Wakeups is
--        variable res: Wakeups;
        
--	    variable cmp0toM2, cmp0toM1, cmp0toR0, cmp0toR1, cmp1toM2, cmp1toM1, cmp1toR0, cmp1toR1,
--                    readyNew, readyBefore, wakeupVec0, wakeupVec1: std_logic_vector(0 to 2) := (others=>'0');
--        variable wakeupPhases0, wakeupPhases1: SmallNumberArray(0 to 2) := (others=>(others=>'0'));            
--    begin
--        cmp0toR0 := fm.a0cmp0 and fnm.maskR0;
--        cmp1toR0 := fm.a1cmp0 and fnm.maskR0;
--        cmp0toR1 := fm.a0cmp1 and fnm.maskR1;
--        cmp1toR1 := fm.a1cmp1 and fnm.maskR1;
--        cmp0toM1 := fm.a0cmpM1 and fnm.maskM1;
--        cmp1toM1 := fm.a1cmpM1 and fnm.maskM1;
--        cmp0toM2 := fm.a0cmpM2 and fnm.maskM2;
--        cmp1toM2 := fm.a1cmpM2 and fnm.maskM2;
    
--        if dynamic then
--            wakeupPhases0 := getWakeupPhase(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, progressLocs);
--            wakeupPhases1 := getWakeupPhase(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, progressLocs);
--        else
--            wakeupPhases0 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
--            wakeupPhases1 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
--        end if;
    
--        wakeupVec0 := getWakeupVector(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, dynamic);  
--        wakeupVec1 := getWakeupVector(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, dynamic);
     
--        if arg = 0 then
--            res.matchR1 := cmp0toR1;
--            res.matchR0 := cmp0toR0;
--            res.matchM1 := cmp0toM1;
--            res.matchM2 := cmp0toM2;
            
--            res.argLocsPhase := wakeupPhases0;
            
--            res.wakeupVec := wakeupVec0;
--        else
--            res.matchR1 := cmp1toR1;
--            res.matchR0 := cmp1toR0;
--            res.matchM1 := cmp1toM1;
--            res.matchM2 := cmp1toM2;
            
--            res.argLocsPhase := wakeupPhases1;
            
--            res.wakeupVec := wakeupVec1;                                
--        end if;
        
--                res.wakeupVec(2) := '0';
        
--        return res;
--    end function;


    function getWakeups(state: SchedulerInfo; arg: natural; cmpR1, cmpR0, cmpM1, cmpM2: std_logic_vector) return Wakeups is
        variable res: Wakeups;
        variable nMatches: natural;
    begin
        res.matchR1 := cmpR1;
        res.matchR0 := cmpR0;
        res.matchM1 := cmpM1;
        res.matchM2 := cmpM2;
        
        nMatches := countOnes(cmpR1) + countOnes(cmpR0) + countOnes(cmpM1) + countOnes(cmpM2);
        
        res.match := bool2std(nMatches /= 0);
        res.multiMatch := bool2std(nMatches > 1);
        
        if isNonzero(cmpR1) = '1' then
            res.argLocsPhase(0) := "00000001";
            res.argLocsPipe(0) := i2slv(getFirstOnePosition(cmpR1), 8);
        elsif isNonzero(cmpR0) = '1' then
            res.argLocsPhase(0) := "00000000";
            res.argLocsPipe(0) := i2slv(getFirstOnePosition(cmpR0), 8);            
        elsif isNonzero(cmpM1) = '1' then
            res.argLocsPhase(0) := "11111111";
            res.argLocsPipe(0) := i2slv(getFirstOnePosition(cmpM1), 8);            
        elsif isNonzero(cmpM2) = '1' then
            res.argLocsPhase(0) := "11111110";
            res.argLocsPipe(0) := i2slv(getFirstOnePosition(cmpM2), 8);            
        else
            res.argLocsPhase(0) := "10000000";                                                            
        end if;
            
        res.const := state.static.zero(arg);
        res.missing := state.dynamic.missing(arg);
        
        res.wakeupVec := cmpR1 or cmpR0 or cmpM1 or cmpM2;
            
            res.wakeup := res.missing and res.match;
        return res;
    end function;




    function getWakeupsAllArgs(stateArray: SchedulerInfoArray; fma: ForwardingMatchesArray) return WakeupsArray2D is
        variable res: WakeupsArray2D(0 to PIPE_WIDTH-1, 0 to 1);
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            res(i, 0) := getWakeups(stateArray(i), 0, fma(i).a0cmp1, fma(i).a0cmp0, fma(i).a0cmpM1, fma(i).a0cmpM2); 
            res(i, 1) := getWakeups(stateArray(i), 1, fma(i).a1cmp1, fma(i).a1cmp0, fma(i).a1cmpM1, fma(i).a1cmpM2); 
        end loop;
        
        return res;
    end function;

        function getWakeupsTest(stateArrayBefore, stateArrayAfter: SchedulerInfoArray; fma: ForwardingMatchesArray) return WakeupsArray2D is
            variable res: WakeupsArray2D(0 to PIPE_WIDTH-1, 0 to 1);
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i, 0).wakeup := stateArrayBefore(i).dynamic.missing(0) and not stateArrayAfter(i).dynamic.missing(0);
                res(i, 1).wakeup := stateArrayBefore(i).dynamic.missing(1) and not stateArrayAfter(i).dynamic.missing(1); 
            end loop;
            return res;
        end function;

        function cmpWakeups(state: SchedulerInfoArray; a, b: WakeupsArray2D) return std_logic_vector is
            variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0'); 
        begin
            for i in res'range loop
                if state(i).dynamic.full = '1' and (a(i, 0).wakeup /= b(i, 0).wakeup or a(i, 1).wakeup /= b(i, 1).wakeup) then
                    res(i) := '1';
                end if; 
            end loop;
            return res;
        end function;

    

function updateSchedulerState(state: SchedulerInfo;
                                fni: ForwardingInfo;
                                fm: ForwardingMatches;
                                fnm: ForwardingMap; progressLocs, dynamic: boolean)
return SchedulerInfo is
	variable res: SchedulerInfo := state;
	variable cmp0toM2, cmp0toM1, cmp0toR0, cmp0toR1, cmp1toM2, cmp1toM1, cmp1toR0, cmp1toR1,
    			readyNew, readyBefore, wakeupVec0, wakeupVec1: std_logic_vector(0 to 2) := (others=>'0');
	variable wakeupPhases0, wakeupPhases1: SmallNumberArray(0 to 2) := (others=>(others=>'0'));	
begin       		
    cmp0toR0 := fm.a0cmp0 and fnm.maskR0;
    cmp1toR0 := fm.a1cmp0 and fnm.maskR0;
    cmp0toR1 := fm.a0cmp1 and fnm.maskR1;
    cmp1toR1 := fm.a1cmp1 and fnm.maskR1;
    cmp0toM1 := fm.a0cmpM1 and fnm.maskM1;
    cmp1toM1 := fm.a1cmpM1 and fnm.maskM1;
    cmp0toM2 := fm.a0cmpM2 and fnm.maskM2;
    cmp1toM2 := fm.a1cmpM2 and fnm.maskM2;

    if dynamic then
        wakeupPhases0 := getWakeupPhase(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, progressLocs);
        wakeupPhases1 := getWakeupPhase(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, progressLocs);
    else
        wakeupPhases0 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
        wakeupPhases1 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
    end if;

    wakeupVec0 := getWakeupVectorDynamic(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, dynamic);
    wakeupVec1 := getWakeupVectorDynamic(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, dynamic);
            
	readyBefore := not res.dynamic.missing;
    readyNew := (isNonzero(wakeupVec0), isNonzero(wakeupVec1), '0');

	-- Update arg tracking
	res.dynamic := updateArgLocs(   res.dynamic,
                                    readyBefore,
                                    wakeupPhases0,
                                    wakeupPhases1,
                                    wakeupVec0,
                                    wakeupVec1,
                                    progressLocs
                                    );
											
	res.dynamic.missing := res.dynamic.missing and not readyNew;
	
	return res;
end function;

function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; progressLocs, dynamic: boolean)
return SchedulerInfoArray is
	variable res: SchedulerInfoArray(0 to schedArray'length-1);-- := insArray;
begin
	for i in schedArray'range loop
		res(i) := updateSchedulerState(schedArray(i), fni, fma(i), fnm, progressLocs, dynamic);
	end loop;	
	return res;
end function;




    
--    function updateSchedulerState_DYN(state: SchedulerInfo;
--                                    fni: ForwardingInfo;
--                                    fm: ForwardingMatches;
--                                    fnm: ForwardingMap; progressLocs, dynamic: boolean)
--    return SchedulerInfo is
--        variable res: SchedulerInfo := state;
--        variable cmp0toM2, cmp0toM1, cmp0toR0, cmp0toR1, cmp1toM2, cmp1toM1, cmp1toR0, cmp1toR1,
--                    readyNew, readyBefore, wakeupVec0, wakeupVec1: std_logic_vector(0 to 2) := (others=>'0');
--        variable wakeupPhases0, wakeupPhases1: SmallNumberArray(0 to 2) := (others=>(others=>'0'));	
--    begin       		
--        cmp0toR0 := fm.a0cmp0 and fnm.maskR0;
--        cmp1toR0 := fm.a1cmp0 and fnm.maskR0;
--        cmp0toR1 := fm.a0cmp1 and fnm.maskR1;
--        cmp1toR1 := fm.a1cmp1 and fnm.maskR1;
--        cmp0toM1 := fm.a0cmpM1 and fnm.maskM1;
--        cmp1toM1 := fm.a1cmpM1 and fnm.maskM1;
--        cmp0toM2 := fm.a0cmpM2 and fnm.maskM2;
--        cmp1toM2 := fm.a1cmpM2 and fnm.maskM2;
    
--        if dynamic then
--            wakeupPhases0 := getWakeupPhase(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, progressLocs);
--            wakeupPhases1 := getWakeupPhase(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, progressLocs);
--        else
--            wakeupPhases0 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
--            wakeupPhases1 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
--        end if;
    
--        wakeupVec0 := getWakeupVectorDynamic(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, dynamic);
--        wakeupVec1 := getWakeupVectorDynamic(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, dynamic);
                
--        readyBefore := not res.dynamic.missing;
--        readyNew := (isNonzero(wakeupVec0), isNonzero(wakeupVec1), '0');
    
--        -- Update arg tracking
--        res.dynamic := updateArgLocs(   res.dynamic,
--                                        readyBefore,
--                                        wakeupPhases0,
--                                        wakeupPhases1,
--                                        wakeupVec0,
--                                        wakeupVec1,
--                                        progressLocs
--                                        );
                                                
--        res.dynamic.missing := res.dynamic.missing and not readyNew;
        
--        return res;
--    end function;
    
--    function updateSchedulerArray_DYN(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; progressLocs, dynamic: boolean)
--    return SchedulerInfoArray is
--        variable res: SchedulerInfoArray(0 to schedArray'length-1);-- := insArray;
--    begin
--        for i in schedArray'range loop
--            res(i) := updateSchedulerState_DYN(schedArray(i), fni, fma(i), fnm, progressLocs, dynamic);
--        end loop;	
--        return res;
--    end function;






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

function iqInputStageNext(content, newContent: SchedulerInfoArray; prevSending, isSending, execEventSignal, lateEventSignal: std_logic) return SchedulerInfoArray is
   variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content;
begin
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

function getKillMask(content: SchedulerInfoArray; fullMask: std_logic_vector;
                            causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
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
