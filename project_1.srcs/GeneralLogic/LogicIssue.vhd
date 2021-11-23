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

constant PHYS_NAME_NONE: PhysName := (others => '1');

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
    active: std_logic;

    issued: std_logic;
    trial: std_logic;
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
    argSrc: SmallNumberArray(0 to 2);
end record;


constant DEFAULT_DYNAMIC_INFO: DynamicInfo := (
    full => '0',
    active => '0',

    issued => '0',
    trial => '0',
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
    argSrc => (others => (others => '0'))
);

type DynamicInfoArray is array(natural range <>) of DynamicInfo;

type SchedulerInfo is record
    dynamic: DynamicInfo;
            dynamic_T: DynamicInfo;
    static: StaticInfo;
end record;

constant DEFAULT_SCHEDULER_INFO: SchedulerInfo := (
    DEFAULT_DYNAMIC_INFO,
        DEFAULT_DYNAMIC_INFO,
    DEFAULT_STATIC_INFO
);

type SchedulerInfoArray is array(natural range <>) of SchedulerInfo;

type WakeupStruct is record
    argLocsPipe: SmallNumber;
    argLocsPhase: SmallNumber;
    argSrc: SmallNumber;
    match:   std_logic;         
end record;

constant DEFAULT_WAKEUP_STRUCT: WakeupStruct := ((others => '0'), "00000010", "00000010", '0');
type WakeupArray2D is array(natural range <>, natural range <>) of WakeupStruct;

function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean; ri: RenameInfo) return StaticInfo; 
function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean; ri: RenameInfo) return DynamicInfo;

function getIssueInfoArray(insVec: InstructionSlotArray; mask: std_logic_vector; constant USE_IMM: boolean; ria: RenameInfoArray; constant USE_OLD: boolean := false) return SchedulerInfoArray;

function getSchedEntrySlot(info: SchedulerInfo) return SchedulerEntrySlot;

function TMP_restoreState(full: std_logic; ins: InstructionState; st: SchedulerState) return SchedulerEntrySlot;

function TMP_prepareDispatchSlot(input: SchedulerEntrySlot; prevSending: std_logic) return SchedulerEntrySlot;

function getDispatchArgValues(input: SchedulerEntrySlot;
                                    fni: ForwardingInfo;
                                    prevSending: std_logic;
                                    USE_IMM: boolean; REGS_ONLY: boolean; TMP_DELAY: boolean)
return SchedulerEntrySlot;

function updateDispatchArgs(st: SchedulerState; vals: MwordArray; regValues: MwordArray; TMP_DELAY: boolean; REGS_ONLY: boolean) return SchedulerState;

    function iqNext_N(queueContent: SchedulerInfoArray;
                      inputData: SchedulerInfoArray;
                      
                      prevSending, sends: std_logic;
                      killMask, selMask: std_logic_vector;
                      TEST_MODE: natural)
    return SchedulerInfoArray;

    function iqNext_N2(queueContent: SchedulerInfoArray;
                      inputData: SchedulerInfoArray;
                      
                      prevSending, sends: std_logic;
                      killMask, trialMask, selMask: std_logic_vector;
                      TEST_MODE: natural)
    return SchedulerInfoArray;

function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector;

function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray;-- fnm: ForwardingMap;
                dynamic: boolean;
                selection: boolean;
                dontMatch1: boolean;
                forwardingModes0, forwardingModes1: ForwardingModeArray
            )
return SchedulerInfoArray;

function findForwardingMatchesArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo) return ForwardingMatchesArray;

function extractFullMask(queueContent: SchedulerInfoArray) return std_logic_vector;


--function prioSelect(elems: SchedulerInfoArray; selVec: std_logic_vector) return SchedulerInfo;

function updateRR(newContent: SchedulerInfoArray; rr: std_logic_vector) return SchedulerInfoArray;

function getKillMask(content: SchedulerInfoArray; causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector;

function prioSelect16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return SchedulerInfo;

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

function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean; ri: RenameInfo) return StaticInfo is
    variable res: StaticInfo;
begin
    res.operation := isl.ins.specificOperation;

    res.branchIns := isl.ins.classInfo.branchIns;
    res.bqPointer := isl.ins.tags.bqPointer;
    res.sqPointer := isl.ins.tags.sqPointer;
    res.lqPointer := isl.ins.tags.lqPointer;        
    
    res.immediate := isl.ins.constantArgs.immSel and bool2std(HAS_IMM);    
    res.immValue := isl.ins.constantArgs.imm(15 downto 0);
    
    if HAS_IMM and isl.ins.constantArgs.immSel = '1' then    
        if IMM_AS_REG then    
            if CLEAR_DEBUG_INFO then
                res.immValue(PhysName'length-2 downto 0) := (others => '0');
            end if;
        end if;
    end if;
    
    res.zero := ri.sourceConst;
    
    if not HAS_IMM then
        res.immediate := '0';            
    end if;        
                   
    return res;
end function; 


function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean; ri: RenameInfo) return DynamicInfo is
    variable res: DynamicInfo;
begin
    res.full := isl.full;
        res.active := res.full;
    res.issued := '0';
        res.trial := '0';
    res.newInQueue := '1';
    
    res.renameIndex := isl.ins.tags.renameIndex;
    res.staticPtr := (others => '0'); -- points to entry with static info

    res.argSpec := isl.ins.physicalArgSpec;
        
    res.stored := (others => '0');
    res.missing := not stInfo.zero;                               
    res.readyNow := (others => '0');
    res.readyNext := (others => '0');
    res.readyM2  := (others => '0');

    res.argLocsPipe := (others => (others => '0'));
    res.argSrc := (others => "00000010");
    
    if HAS_IMM and isl.ins.constantArgs.immSel = '1' then
        if IMM_AS_REG then
            res.argSpec.args(1) := isl.ins.constantArgs.imm(PhysName'length-1 downto 0);
                res.argSpec.args(1)(7) := '0';
        end if;
    end if;
                 
    return res;
end function; 


    function getIssueDynamicInfo2(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean; ri: RenameInfo) return DynamicInfo is
        variable res: DynamicInfo;
    begin
        res.full := isl.full;
            res.active := res.full;
        res.issued := '0';
            res.trial := '0';
        res.newInQueue := '1';
        
        res.renameIndex := isl.ins.tags.renameIndex;
        res.staticPtr := (others => '0'); -- points to entry with static info
    
        res.argSpec := isl.ins.physicalArgSpec;
        
        for i in 0 to 2 loop
            if ri.sourcesNew(i) = '1' then
                res.argSpec.args(i) := ri.physicalSourcesNew(i);
            elsif ri.sourcesStable(i) = '1' then
                res.argSpec.args(i) := ri.physicalSourcesStable(i);
            else
                res.argSpec.args(i) := ri.physicalSources(i);
            end if;
        end loop;

        res.stored := (others => '0');
        res.missing := not stInfo.zero;                               
        res.readyNow := (others => '0');
        res.readyNext := (others => '0');
        res.readyM2  := (others => '0');
    
        res.argLocsPipe := (others => (others => '0'));
        res.argSrc := (others => "00000010");
        
        if HAS_IMM and isl.ins.constantArgs.immSel = '1' then
            if IMM_AS_REG then
                res.argSpec.args(1) := isl.ins.constantArgs.imm(PhysName'length-1 downto 0);
                    res.argSpec.args(1)(7) := '0';
            end if;
        end if;
                     
        return res;
    end function; 

function getIssueInfoArray(insVec: InstructionSlotArray; mask: std_logic_vector; constant USE_IMM: boolean; ria: RenameInfoArray; constant USE_OLD: boolean := false) return SchedulerInfoArray is
    variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1);
    variable slot: InstructionSlot := DEFAULT_INS_SLOT;
begin
    for i in res'range loop
        slot := insVec(i);
        slot.full := mask(i);
        res(i).static := getIssueStaticInfo(slot, USE_IMM, ria(i));
        if USE_OLD then
            res(i).dynamic := getIssueDynamicInfo(slot, res(i).static, USE_IMM, ria(i));
        else
            res(i).dynamic := getIssueDynamicInfo2(slot, res(i).static, USE_IMM, ria(i));
        end if;
           --     assert res(i).dynamic = res(i).dynamic_T;
    end loop;
    return res;
    
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
    res.state.readNew := (others => '0');

    res.state.missing := info.dynamic.missing;
    
    res.state.readyNow := info.dynamic.readyNow;
    res.state.readyNext := info.dynamic.readyNext;
    res.state.readyM2  := info.dynamic.readyM2;

    res.state.argLocsPipe := info.dynamic.argLocsPipe;
    res.state.argSrc := info.dynamic.argSrc;

    res.state.args := (others => (others => '0'));

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


function updateArgInfo(ss: DynamicInfo; arg: natural; wakeups: WakeupStruct; selection: boolean)
return DynamicInfo is
    variable res: DynamicInfo := ss;
    variable wakeupVec: std_logic_vector(0 to 2) := (others => '0');
    variable wakeupPhases: SmallNumberArray(0 to 2) := (others => (others => '0'));  
begin
    if ss.missing(arg) = '1' then
        res.argLocsPipe(arg) := wakeups.argLocsPipe;
        res.missing(arg) := not wakeups.match; 
        res.argSrc(arg) := wakeups.argSrc;        
    else -- update loc
        if not selection then
            case res.argSrc(arg)(1 downto 0) is
                when "11" =>
                    res.argSrc(arg) := "00000000";
                when "00" =>
                    res.argSrc(arg) := "00000001";               
                when others =>
                    res.argSrc(arg) := "00000010";
            end case;                
        end if;
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
        res.ins.physicalArgSpec.dest := PHYS_NAME_NONE;
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
        res.ins.physicalArgSpec.dest := PHYS_NAME_NONE; -- Don't allow false notifications of args
        res.state.argSpec.dest := PHYS_NAME_NONE; -- Don't allow false notifications of args
    end if;
    
    return res;
end function;


function getDispatchArgValues(input: SchedulerEntrySlot;
                                    fni: ForwardingInfo;
                                    prevSending: std_logic;
                                    USE_IMM: boolean; REGS_ONLY: boolean; TMP_DELAY: boolean)
return SchedulerEntrySlot is
    variable res: SchedulerEntrySlot := input;
begin
    if TMP_DELAY then
        if IMM_AS_REG then
            res.state.immValue(PhysName'length-2 downto 0) := res.state.argSpec.args(1)(6 downto 0);
        end if;
        return res;
    end if;

    if REGS_ONLY then
        res.state.stored := (others => '0');
        return res;    
    end if;


    if res.state.zero(0) = '1' then
        res.state.args(0) := (others => '0');
        res.state.stored(0) := '1';
    elsif res.state.argSrc(0)(1 downto 0) = "00" then
        res.state.args(0) := fni.values0(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
        res.state.stored(0) := '1';
    elsif res.state.argSrc(0)(1 downto 0) = "01" then
        res.state.args(0) := fni.values1(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
        if res.state.argSrc(0)(1 downto 0) = "01" then -- becomes redundant
            res.state.stored(0) := '1';
        end if;
    else
        res.state.args(0) := (others => '0');           
    end if;

    if res.state.zero(1) = '1' then
        if USE_IMM then
            res.state.args(1)(31 downto 16) := (others => res.state.immValue(15));
            res.state.args(1)(15 downto 0) := res.state.immValue;
        else
            res.state.args(1) := (others => '0');
        end if;
        res.state.stored(1) := '1';
    elsif res.state.argSrc(1)(1 downto 0) = "00" then
        res.state.args(1) := fni.values0(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
        res.state.stored(1) := '1';
    elsif res.state.argSrc(1)(1 downto 0) = "01" then
        res.state.args(1) := fni.values1(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
        res.state.stored(1) := '1';
    else
        res.state.args(1) := (others => '0');
    end if;
    
    return res;
end function;


function updateDispatchArgs(st: SchedulerState; vals: MwordArray; regValues: MwordArray; TMP_DELAY: boolean; REGS_ONLY: boolean)
return SchedulerState is
    variable res: SchedulerState := st;
begin
    if TMP_DELAY then
        res.readNew(0) := bool2std(res.argSrc(0)(1 downto 0) = "11");
        res.readNew(1) := bool2std(res.argSrc(1)(1 downto 0) = "11");

            if res.argSrc(0)(1) /= '1' then
                res.argSpec.args(0) := (others => '0');
            end if;
    
            if res.argSrc(1)(1) /= '1' or res.zero(1) = '1' then
                res.argSpec.args(1) := (others => '0');
            end if;
        
        return res;
    end if;

    if REGS_ONLY then
        res.args(0) := regValues(0);
        res.args(1) := regValues(1);
        return res;
    end if;

    if res.readNew(0) = '1' then
        res.args(0) := vals(slv2u(res.argLocsPipe(0)(1 downto 0)));
    else
        res.args(0) := res.args(0) or regValues(0);
    end if;
    
    res.stored(0) := '1';

    if res.readNew(1) = '1' then
        res.args(1) := vals(slv2u(res.argLocsPipe(1)(1 downto 0)));
    else
        res.args(1) := res.args(1) or regValues(1);
    end if;

    res.stored(1) := '1';         

    return res;
end function;


function moveEarlyStage(content: SchedulerInfoArray; insA, insD: integer) return SchedulerInfoArray is
    variable res: SchedulerInfoArray(content'range) := content;
    constant LEN: natural := content'length;
    constant MAIN_LEN: natural := content'length - PIPE_WIDTH;
begin
    for k in 0 to 2 loop
        if insA + k >= 0 and insA + k < MAIN_LEN then
            if res(MAIN_LEN + k).dynamic.full = '1' then
                res(insA + k) := res(MAIN_LEN + k);
            end if;
        end if;
    end loop;
    
    if insD >= 0 and insD < MAIN_LEN then
        if res(LEN-1).dynamic.full = '1' then
            res(insD) := res(LEN-1);
        end if;
    end if;
    
    res(MAIN_LEN to LEN-1) := (others => DEFAULT_SCHEDULER_INFO);
    
    return res;
end function;

    function iqNext_N(queueContent: SchedulerInfoArray;
                      inputData: SchedulerInfoArray;               
                      prevSending, sends: std_logic;
                      killMask, selMask: std_logic_vector;
                      TEST_MODE: natural
                             )
    return SchedulerInfoArray is
        constant LEN: natural := queueContent'length;
        constant MAIN_LEN: natural := queueContent'length - PIPE_WIDTH;
        variable res: SchedulerInfoArray(queueContent'range) := queueContent;
        variable shifted1, shifted2: SchedulerInfoArray(0 to MAIN_LEN-1) := (others => DEFAULT_SCHEDULER_INFO);
        variable fullMask, fullMaskNew: std_logic_vector(queueContent'range) := extractFullMask(queueContent);
        variable fullMaskIn: std_logic_vector(0 to PIPE_WIDTH-1) := extractFullMask(inputData);
        variable fullMaskEarly: std_logic_vector(0 to PIPE_WIDTH-1) := fullMask(MAIN_LEN to LEN-1);
        variable e1, e2, pAv, cAv: integer := -1;
        variable insA, insD: integer := -1;
    begin
            for i in 0 to LEN-1 loop
                if queueContent(i).dynamic.issued = '1' then       
                    res(i).dynamic.full := '0';
                end if;
                
                if (selMask(i) and sends) = '1' then
                    res(i).dynamic.issued := '1';
                    res(i).dynamic.active := '0';
                end if;
                
                if killMask(i) = '1' then
                    res(i).dynamic.full := '0';
                    res(i).dynamic.active := '0';
                 end if;
            end loop;
    
            fullMask := extractFullMask(res);

        -- Scan full mask for first empty and first available slot
        -- First available is first after last full
        e1 := find1free(fullMask(0 to MAIN_LEN-1));
        e2 := find2free(fullMask(0 to MAIN_LEN-1));
        pAv := findAvailable(fullMask(0 to MAIN_LEN-1));
        cAv := findCompletelyAvailable(fullMask(0 to MAIN_LEN-1));
        
        -- Shift (compact) content!
        shifted1(0 to MAIN_LEN-2) := res(1 to MAIN_LEN-1); 
        shifted2(0 to MAIN_LEN-3) := res(2 to MAIN_LEN-1); 
        
        if e1 = e2 and e1 >= 0 and e1 < MAIN_LEN-2 then
            for i in 0 to MAIN_LEN-1 loop
                if i >= e1 then
                    res(i) := shifted2(i);
                end if;
            end loop;
        elsif e1 >= 0 and e1 < MAIN_LEN-1 then
            for i in 0 to MAIN_LEN-1 loop
                if i >= e1 then
                    res(i) := shifted1(i);
                end if;
            end loop;
        end if;

                -- Find av slots in updated mask
                fullMaskNew := extractFullMask(res);
                pAv := findAvailable(fullMaskNew(0 to MAIN_LEN-1));
                cAv := findCompletelyAvailable(fullMaskNew(0 to MAIN_LEN-1));                
                
            if pAv = -1 or cAv = -1 then
               return res; 
            end if;

        -- Find insertion point: index where earlyStage(0) will go (even if it's empty so won't be written)
        insA := findInsertionSimplified(pAv, cAv, fullMaskNew(0 to MAIN_LEN-1), fullMaskEarly);

        if fullMask(MAIN_LEN+1 to MAIN_LEN+2) = "00" then
            insD := insA + 1; -- compress 1001 -> 11
        else
            insD := insA + 3;
        end if;
        
        if insA <= MAIN_LEN - PIPE_WIDTH then -- 
            res := moveEarlyStage(res, insA, insD);
        end if;

        if prevSending = '1' then
            res(MAIN_LEN to LEN-1) := inputData;
        end if;
        
        return res;
    end function;



    function iqNext_N2(queueContent: SchedulerInfoArray;
                      inputData: SchedulerInfoArray;               
                      prevSending, sends: std_logic;
                      killMask, trialMask, selMask: std_logic_vector;
                      TEST_MODE: natural
                             )
    return SchedulerInfoArray is
        constant LEN: natural := queueContent'length;
        constant MAIN_LEN: natural := queueContent'length - PIPE_WIDTH;
        variable res: SchedulerInfoArray(queueContent'range) := queueContent;
        variable shifted1, shifted2: SchedulerInfoArray(0 to MAIN_LEN-1) := (others => DEFAULT_SCHEDULER_INFO);
        variable fullMask, activeMask, fullMaskBeforeKill, fullMaskNew, fullMaskCompressed, fullMaskSh1, fullMaskSh2: std_logic_vector(queueContent'range) := extractFullMask(queueContent);
        variable fullMaskIn: std_logic_vector(0 to PIPE_WIDTH-1) := extractFullMask(inputData);
        variable fullMaskEarly: std_logic_vector(0 to PIPE_WIDTH-1) := fullMask(MAIN_LEN to LEN-1);
        variable e1, e2, pAv, cAv: integer := -1;
        variable insA, insD: integer := -1;
        variable caVec: std_logic_vector(0 to MAIN_LEN-1) := (others => '0');
        variable lastFound: boolean := false;
        variable iPrev2, iPrev1, iCurrent, iNext: integer := 0;
        variable mPrev2, mPrev1, mCurrent, mNext: std_logic := '0';
    begin
    
        fullMaskBeforeKill := fullMask;
        
            for i in 0 to LEN-1 loop
                if queueContent(i).dynamic.issued = '1' then       
                    res(i).dynamic.full := '0';
                end if;
                
                if (selMask(i) and sends) = '1' then
                    res(i).dynamic.issued := '1';
                    res(i).dynamic.active := '0';
                end if;
            end loop;    

            --fullMaskBeforeKill := extractFullMask(res);

            for i in 0 to LEN-1 loop
                if killMask(i) = '1' then
                    res(i).dynamic.full := '0';
                    res(i).dynamic.active := '0';
                 end if;
                 
                 if trialMask(i) = '1' then
                     res(i).dynamic.trial := '1';
                 end if;
            end loop;
    
            fullMask := extractFullMask(res);

        -- Scan full mask for first empty and first available slot
        -- First available is first after last full
        e1 := find1free(fullMaskBeforeKill(0 to MAIN_LEN-1));
        e2 := find2free(fullMaskBeforeKill(0 to MAIN_LEN-1));
        --pAv := findAvailable(fullMask(0 to MAIN_LEN-1));
        --cAv := findCompletelyAvailable(fullMask(0 to MAIN_LEN-1));
        
        -- Shift (compact) content!
        shifted1(0 to MAIN_LEN-2) := res(1 to MAIN_LEN-1); 
        shifted2(0 to MAIN_LEN-3) := res(2 to MAIN_LEN-1); 

        fullMaskSh1(0 to MAIN_LEN-2) := fullMaskBeforeKill(1 to MAIN_LEN-1); 
        fullMaskSh2(0 to MAIN_LEN-3) := fullMaskBeforeKill(2 to MAIN_LEN-1);
        
--        if e1 = e2 and e1 >= 0 and e1 < MAIN_LEN-2 then
--            for i in 0 to MAIN_LEN-1 loop
--                if i >= e1 then
--                    res(i) := shifted2(i);
--                    fullMaskCompressed(i) := fullMaskSh2(i);
--                end if;
--            end loop;
--        elsif e1 >= 0 and e1 < MAIN_LEN-1 then
--            for i in 0 to MAIN_LEN-1 loop
--                if i >= e1 then
--                    res(i) := shifted1(i);
--                    fullMaskCompressed(i) := fullMaskSh1(i);
--                end if;
--            end loop;
--        end if;
               
                if e2 >= 0 and e2 < MAIN_LEN-2 then
                    for i in 0 to MAIN_LEN-1 loop
                        if i >= e2 then
                            res(i) := shifted2(i);
                            fullMaskCompressed(i) := fullMaskSh2(i);
                        end if;
                    end loop;
                end if;
        
        
            if isNonzero(fullMaskBeforeKill(MAIN_LEN-4 to MAIN_LEN-1)) = '1' then
                return res;
            end if;
        
                -- Find av slots in updated mask
                fullMaskNew := --extractFullMask(res);
                               fullMaskCompressed;
                pAv := findAvailable(fullMaskNew(0 to MAIN_LEN-1));
                cAv := findCompletelyAvailable(fullMaskNew(0 to MAIN_LEN-1));
                
                -- Set CA pairs
                for i in MAIN_LEN/2-1 downto 0 loop
                    if (fullMaskNew(2*i) or fullMaskNew(2*i + 1)) /= '1' then
                        caVec(2*i) := '1';
                    end if;
                end loop;
                -- Exclude those before last content
                for i in MAIN_LEN/2-1 downto 0 loop
                    if lastFound then
                        caVec(2*i) := '0';                    
                    elsif caVec(2*i) /= '1' then
                        lastFound := true;
                    end if;
                end loop;
                
                -- Set prev, at, after
                for i in 0 to MAIN_LEN/2-1 loop
                    iPrev2 := 2*i - 4;
                    iPrev1 := 2*i - 2;
                    iCurrent := 2*i;
                    iNext := 2*i + 2;
                    
                    if iPrev2 < 0 then
                        mPrev2 := '0';
                    else
                        mPrev2 := caVec(iPrev2);
                    end if;
                    
                    if iPrev1 < 0 then
                        mPrev1 := '0';
                    else
                        mPrev1 := caVec(iPrev1);
                    end if;
                    
                    mCurrent := caVec(iCurrent);
                    
                    if iNext >= MAIN_LEN then
                        mNext := '1';
                    else
                        mNext := caVec(iNext);
                    end if;
                    
                    if std_logic_vector'(mPrev2 & mPrev1 & mCurrent & mNext) = "0001" then -- [-2]
                        null;
                        
                    elsif std_logic_vector'(mPrev2 & mPrev1 & mCurrent & mNext) = "0011" then -- [0]
                        if isNonzero(fullMaskEarly(0 to 1)) = '1' then
                            res(iCurrent) := res(MAIN_LEN);
                        else
                            res(iCurrent) := res(MAIN_LEN + 2);
                        end if;
                        
                        if isNonzero(fullMaskEarly(1 to 2)) = '1' then
                            res(iCurrent + 1) := res(MAIN_LEN + 1);
                        else
                            res(iCurrent + 1) := res(MAIN_LEN + 3);
                        end if;
                        
                    elsif std_logic_vector'(mPrev2 & mPrev1 & mCurrent & mNext) = "0111" then -- [2]
                        if isNonzero(fullMaskEarly(0 to 1)) = '1' then
                            res(iCurrent) := res(MAIN_LEN + 2);
                        end if;
                        
                        if fullMaskEarly(1) = '1' or (fullMaskEarly(0) or fullMaskEarly(2)) = '1' then
                            res(iCurrent + 1) := res(MAIN_LEN + 3);
                        end if;
                    end if;
                    
                    -- 0000  ?? ??   00xx -> cd --     0: {a if x1xx | 1xxx, else c}
                    -- 0001  ad --   01xx -> ab cd     1: {d if x00x, else b}
                    -- 0010  cd --   100x -> ad --     2: {c if x1xx | 1xxx, else -}
                    -- 0011  cd --   11xx -> ab cd     3: {d if x1xx | 1x1x, else -}
                    -- 0100  ab cd*  1x1x -> ab cd
                    -- 0101  ab cd
                    -- 0110  ab cd
                    -- 0111  ab cd
                    -- 1000  ad*-- 
                    -- 1001  ad --
                    -- 1010  ab cd
                    -- 1011  ab cd
                    -- 1100  ab --
                    -- 1101  ab cd
                    -- 1110  ab cd
                    -- 1111  ab cd             
                end loop;
                
                res(MAIN_LEN to LEN-1) := (others => DEFAULT_SCHEDULER_INFO);
                
                
                if prevSending = '1' then
                    res(MAIN_LEN to LEN-1) := inputData;
                end if;
                
                return res;
                
            if pAv = -1 or cAv = -1 then
               return res; 
            end if;

        -- Find insertion point: index where earlyStage(0) will go (even if it's empty so won't be written)
        insA := findInsertionSimplified(pAv, cAv, fullMaskNew(0 to MAIN_LEN-1), fullMaskEarly);

        if fullMask(MAIN_LEN+1 to MAIN_LEN+2) = "00" then
            insD := insA + 1; -- compress 1001 -> 11
        else
            insD := insA + 3;
        end if;
        
        if insA <= MAIN_LEN - PIPE_WIDTH then -- 
            res := moveEarlyStage(res, insA, insD);
        end if;

        if prevSending = '1' then
            res(MAIN_LEN to LEN-1) := inputData;
        end if;
        
        return res;
    end function;
    
    
function getWakeupStructStatic(arg: natural; cmpR1, cmpR0, cmpM1, cmpM2, cmpM3: std_logic_vector; forwardingModes: ForwardingModeArray; selection: boolean)
return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    variable matchVec: std_logic_vector(0 to 2) := (others => '0');
begin
    for p in forwardingModes'range loop
        case forwardingModes(p).stage is
            when -3 =>
                matchVec(p) := cmpM3(p);
            when -2 =>
                matchVec(p) := cmpM2(p);
            when -1 =>
                matchVec(p) := cmpM1(p);
            when 0 =>
                matchVec(p) := cmpR0(p);
            when 1 =>
                matchVec(p) := cmpR1(p);
            when others =>
                matchVec(p) := '0';
        end case;

        if matchVec(p) = '1' then
            res.argLocsPipe(2 downto 0) := i2slv(p, 3);
            res.argLocsPhase(2 downto 0) := i2slv(forwardingModes(p).stage + 1, 3);
            if selection then
                if forwardingModes(p).stage + 1 > 2 then
                    res.argSrc(1 downto 0) := i2slv(2, 2);
                else
                    res.argSrc(1 downto 0) := i2slv(forwardingModes(p).stage + 1, 2);
                end if;             
            else
                if forwardingModes(p).stage + 2 > 2 then
                    res.argSrc(1 downto 0) := i2slv(2, 2);
                else
                    res.argSrc(1 downto 0) := i2slv(forwardingModes(p).stage + 2, 2);
                end if;
            end if;
        end if;
    end loop;   
    res.match := isNonzero(matchVec);
    
    return res;
end function;

function getWakeupStructDynamic(arg: natural; cmpR1, cmpR0, cmpM1, cmpM2, cmpM3: std_logic_vector; forwardingModes: ForwardingModeArray) return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    variable matchVec: std_logic_vector(0 to 2) := (others => '0');
begin
    
    for p in forwardingModes'range loop
        for q in -3 to 1 loop
            if forwardingModes(p).stage <= q then
                case q is
                    when -3 =>
                        matchVec(p) := cmpM3(p);
                    when -2 =>
                        matchVec(p) := cmpM2(p);
                    when -1 =>
                        matchVec(p) := cmpM1(p);
                    when 0 =>
                        matchVec(p) := cmpR0(p);
                    when 1 =>
                        matchVec(p) := cmpR1(p);
                    when others =>
                        matchVec(p) := '0';
                end case;
                
                if matchVec(p) = '1' then
                    res.argLocsPipe(2 downto 0) := i2slv(p, 3);
                    
                    if q + 2 > 2 then
                        res.argSrc(1 downto 0) := i2slv(2, 2);
                    else
                        res.argSrc(1 downto 0) := i2slv(q + 2, 2);
                    end if;
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
                                dynamic: boolean;
                                selection: boolean;
                                dontMatch1: boolean;
                                forwardingModes0, forwardingModes1: ForwardingModeArray)
return SchedulerInfo is
	variable res: SchedulerInfo := state;
	variable wakeups0, wakeups1: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
begin
    if not dynamic then
        wakeups0 := getWakeupStructStatic(0, fm.a0cmp1, fm.a0cmp0, fm.a0cmpM1, fm.a0cmpM2, fm.a0cmpM3, forwardingModes0, selection);
        wakeups1 := getWakeupStructStatic(1, fm.a1cmp1, fm.a1cmp0, fm.a1cmpM1, fm.a1cmpM2, fm.a1cmpM3, forwardingModes1, selection);
    else
        wakeups0 := getWakeupStructDynamic(0, fm.a0cmp1, fm.a0cmp0, fm.a0cmpM1, fm.a0cmpM2, fm.a0cmpM3, forwardingModes0);
        wakeups1 := getWakeupStructDynamic(1, fm.a1cmp1, fm.a1cmp0, fm.a1cmpM1, fm.a1cmpM2, fm.a1cmpM3, forwardingModes1);
    end if;

    res.dynamic := updateArgInfo(res.dynamic, 0, wakeups0, selection);
    
    if dontMatch1 then
        res.dynamic.missing(1) := '0';
    else
        res.dynamic := updateArgInfo(res.dynamic, 1, wakeups1, selection);
    end if;

	return res;
end function;

function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray;
                dynamic: boolean; selection: boolean; dontMatch1: boolean; forwardingModes0, forwardingModes1: ForwardingModeArray)
return SchedulerInfoArray is
	variable res: SchedulerInfoArray(0 to schedArray'length-1);
begin
	for i in schedArray'range loop
		res(i) := updateSchedulerState(schedArray(i), fni, fma(i), dynamic, selection, dontMatch1, forwardingModes0, forwardingModes1);
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
    res.a0cmpM3 := findRegTag(info.dynamic.argSpec.args(0), fni.nextTagsM3);
    res.a1cmpM3 := findRegTag(info.dynamic.argSpec.args(1), fni.nextTagsM3);    
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
    
    return select4((0 => ch0, 1 => ch1, 2 => ch2, 3 => ch3), indH);        
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
        res(i) := killByTag(compareTagBefore(causing.tags.renameIndex, content(i).dynamic.renameIndex), execEventSig, lateEventSig);
    end loop;
    return res;
end function;

end LogicIssue;
