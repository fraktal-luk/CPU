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

constant PHYS_NAME_NONE: PhysName := (others => '0');

constant IQ_HOLD_TIME: natural := 3;

type DbDependency is record
    -- pragma synthesis off
    producer: InsTag;
    cyclesWaiting: integer;
    cyclesReady: integer;
    -- pragma synthesis on
    
    dummy: DummyType;
end record;

constant DEFAULT_DB_DEPENDENCY: DbDependency := (
                                    -- pragma synthesis off
                                    producer => (others => 'U'),
                                    cyclesWaiting => -1,
                                    cyclesReady => -1,
                                    -- pragma synthesis on

                                    dummy => DUMMY_VALUE
                                    );

type DbDependencyArray is array(natural range <>) of DbDependency;

type ArgumentState is record
    used: std_logic;
    reg: PhysName;
    iqTag: SmallNumber;
    zero: std_logic;
    imm: std_logic;
    value: Hword;
    
    canFail: std_logic; -- maybe a counter is needed 

    activeCounter: SmallNumber;
    failed: std_logic;

    waiting: std_logic;
    stored:  std_logic;
    srcPipe: SmallNumber;
    srcStage: SmallNumber;
    
    dbDep: DbDependency;   
end record;

constant DEFAULT_ARGUMENT_STATE: ArgumentState := (
    used => '0',
    reg => (others => '0'),
    iqTag => (others => '0'),
    zero => '0',
    imm => '0',
    value => (others => '0'),
    canFail => '0',
    
    activeCounter => (others => '0'),
    failed => '0',
    
    waiting => '0',
    stored => '0',
    srcPipe => (others => '0'),
    srcStage => (others => '0'),
    
    dbDep => DEFAULT_DB_DEPENDENCY
); 

constant DEFAULT_ARG_STATE: ArgumentState := DEFAULT_ARGUMENT_STATE;

type ArgumentStateArray is array(natural range <>) of ArgumentState;


type StaticInfo is record
    dbInfo: InstructionDebugInfo;

    operation: SpecificOp;
    
    branchIns: std_logic;
    
    bqPointer: SmallNumber;
    sqPointer: SmallNumber;
    lqPointer: SmallNumber;        
    bqPointerSeq: SmallNumber;
    
    tags: InstructionTags;
    
    immediate: std_logic;    
    immValue: Hword;
    zero: std_logic_vector(0 to 2);    
end record;

constant DEFAULT_STATIC_INFO: StaticInfo := (
    dbInfo => DEFAULT_DEBUG_INFO,

    operation => DEFAULT_SPECIFIC_OP,
    branchIns => '0',
    
    bqPointer => (others => '0'),
    sqPointer => (others => '0'),
    lqPointer => (others => '0'),  
    bqPointerSeq => (others => '0'),  
    
    tags => DEFAULT_INSTRUCTION_TAGS,
    
    immediate => '0',
    immValue => (others => '0'),
    zero => (others => '0')    
);

type StaticInfoArray is array(natural range <>) of StaticInfo;

type DynamicInfo is record
    full: std_logic;
    active: std_logic;

    issued: std_logic;
    freed: std_logic;
    
    trial: std_logic;

    poisoned: std_logic;

    pulledBack: std_logic;
    stageCtr: SmallNumber;

    renameIndex: InsTag;

    intDestSel: std_logic;
    floatDestSel: std_logic;
    dest: PhysName;
    --destTag: SmallNumber;
    
    argStates: ArgumentStateArray(0 to 2);
end record;


constant DEFAULT_DYNAMIC_INFO: DynamicInfo := (
    full => '0',
    active => '0',

    issued => '0',
    freed => '0',
    
    trial => '0',
        
    poisoned => '0',

    pulledBack => '0',
    stageCtr => (others => '0'),

    renameIndex => (others => '0'),
        
    intDestSel => '0',
    floatDestSel => '0',
    dest => (others => '0'),
    --destTag => (others => '0'),

    argStates => (others => DEFAULT_ARG_STATE)
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

type WakeupStruct is record
    argLocsPipe: SmallNumber;
    argSrc: SmallNumber;
    match:   std_logic;         
end record;

type slv2D is array(natural range <>, natural range <>) of std_logic;

    type SchedulerUpdateConfig is record
        dynamic: boolean;
        selection: boolean;
        fp: boolean;
        ignoreMemFail: boolean;
        fwModes: ForwardingModeArray(0 to 2);
        matchIQ: boolean;
        --matchNonzero: boolean;
    end record;
    
    constant DEFUALT_SCHEDULER_UPDATE_CONFIG: SchedulerUpdateConfig := (
        dynamic => false,
        selection => false,
        fp => false,
        ignoreMemFail => false,
        fwModes => FORWARDING_MODES_NONE,
        matchIQ => false
        --matchNonzero => false
    );

        function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray;-- forwardingModes0: ForwardingModeArray;
                        memFail: std_logic;
                        config: SchedulerUpdateConfig)
        return SchedulerInfoArray;


    
    type ArgWakeup is record
        active: std_logic;
        mode: WakeupMode;
        producer: InsTag;
        iqTag: SmallNumber;
        
        match: std_logic;
        pipe:  SmallNumber;
        stage: SmallNumber;
    end record;


    type WakeupInfo is record
        active: std_logic; -- if happening this cycle
        arg0: ArgWakeup;
        arg1: ArgWakeup;
    end record;


    type WakeupInfoArray is array(natural range <>) of WakeupInfo;

        function getWakeupInfoA(content, newArray: SchedulerInfoArray; prevSending: std_logic; insertionLocs: slv2D; fni: ForwardingInfo; readyRegFlags: std_logic_vector; config: SchedulerUpdateConfig)
        return WakeupInfoArray;


constant DEFAULT_WAKEUP_STRUCT: WakeupStruct := ((others => '0'), "00000010", '0');

function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean; ri: RenameInfo) return StaticInfo; 
function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean; ri: RenameInfo;
                                TMP_renamedDest: SmallNumber; TMP_renamedSrcs: SmallNumberArray(0 to 2)) return DynamicInfo;

function getIssueInfoArray(insVec: InstructionSlotArray; mask: std_logic_vector; constant USE_IMM: boolean; ria: RenameInfoArray;
                           TMP_renamedDests: SmallNumberArray; TMP_renamedSources: SmallNumberArray) return SchedulerInfoArray;

function getSchedEntrySlot(info: SchedulerInfo; full: std_logic; iqTag: SmallNumber) return SchedulerState;

function orSchedEntrySlot(a, b: SchedulerInfo) return SchedulerInfo;

function TMP_prepareDispatchSlot(input: SchedulerState; prevSending: std_logic) return SchedulerState;

function getDispatchArgValues_Is(input: SchedulerState; prevSending: std_logic) return SchedulerState;

function getDispatchArgValues_RR(input: SchedulerState;
                                 prevSending: std_logic;
                                 fni: ForwardingInfo;
                                 USE_IMM: boolean; REGS_ONLY: boolean)
return SchedulerState;

function updateDispatchArgs_Is(st: SchedulerState) return SchedulerState;

function updateDispatchArgs_RR(st: SchedulerState; vals: MwordArray; regValues: MwordArray; REGS_ONLY: boolean) return SchedulerState;


    function prepareNewArr(input: SchedulerInfoArray; rrf: std_logic_vector) return SchedulerInfoArray;
    
    function insertElements(content: SchedulerInfoArray; newArr: SchedulerInfoArray; insertionLocs: slv2D) return SchedulerInfoArray;

function iqNext_NS(queueContent: SchedulerInfoArray;
                  sends: std_logic; killMask, trialMask, selMask: std_logic_vector; memFail: std_logic)
return SchedulerInfoArray;

    function iqNext_NS_2(queueContent: SchedulerInfoArray;
                      inputData: SchedulerInfoArray;               
                      prevSending: std_logic;
                      insertionLocs: slv2D)
    return SchedulerInfoArray;

function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector;


function findForwardingMatchesArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; config: SchedulerUpdateConfig; rrf: std_logic_vector) return ForwardingMatchesArray;

     function findForwardingMatchesArray_N(schedArray: SchedulerInfoArray; fni: ForwardingInfo; rrf: std_logic_vector; regInfo: RegisterStateArray2D) return ForwardingMatchesArray;

function extractFullMask(queueContent: SchedulerInfoArray) return std_logic_vector;

function updateRR(newContent: SchedulerInfoArray; rr: std_logic_vector) return SchedulerInfoArray;

function restoreRenameIndex(sia: SchedulerInfoArray) return SchedulerInfoArray;

function prioSelect16(inputElems: SchedulerInfoArray; inputSelVec: std_logic_vector) return SchedulerInfo;

    type SlotControl is record
        full: std_logic;
        active: std_logic;
        issued: std_logic;
        freed: std_logic;
        killed: std_logic;
            killed_T: std_logic;
        trial: std_logic;
            trial_T: std_logic;
        trialUpdated: std_logic;
        living: std_logic;
        ready: std_logic;
        readyFull: std_logic;
        readyLiving: std_logic;
        selected: std_logic;
    end record;
    
    constant DEFAULT_SLOT_CONTROL: SlotControl := (others => '0');
    
    type SlotControlArray is array(natural range <>) of SlotControl;
    
    function getControlSignals(content: SchedulerInfoArray; events: EventState) return SlotControlArray;
    function getFullVec(arr: SlotControlArray) return std_logic_vector;
    function getLivingVec(arr: SlotControlArray) return std_logic_vector;
    function getActiveVec(arr: SlotControlArray) return std_logic_vector;
    function getIssuedVec(arr: SlotControlArray) return std_logic_vector;
    function getFreedVec(arr: SlotControlArray) return std_logic_vector;
    function getKilledVec(arr: SlotControlArray) return std_logic_vector;
        function getKilledVec_T(arr: SlotControlArray) return std_logic_vector;
    function getTrialVec(arr: SlotControlArray) return std_logic_vector;
        function getTrialVec_T(arr: SlotControlArray) return std_logic_vector;
    function getTrialUpdatedVec(arr: SlotControlArray) return std_logic_vector;
    function getReadyVec(arr: SlotControlArray) return std_logic_vector;
    function getReadyFullVec(arr: SlotControlArray) return std_logic_vector;
    function getReadyLiveVec(arr: SlotControlArray) return std_logic_vector;
    function getSelectedVec(arr: SlotControlArray) return std_logic_vector;


    function getNewLocsBanked(fullMask: std_logic_vector) return slv2D;
    function getBankCounts(fullMask: std_logic_vector) return SmallNumberArray;
    function acceptingBanked(counts: SmallNumberArray; constant LIMIT: natural) return std_logic;
    function acceptingMoreBanked(counts: SmallNumberArray; constant LIMIT: natural) return std_logic;
    function updateAgeMatrix(ageMatrix, insertionLocs: slv2D; fullMask: std_logic_vector) return slv2D;
    function getSelMask(readyMask: std_logic_vector; ageMatrix: slv2D) return std_logic_vector;

        function getNewLocs_N(fullMask: std_logic_vector; tags: SmallNumberArray; newArr: SchedulerInfoArray) return slv2D;

    function updateRenameIndex(content: SchedulerInfoArray) return SchedulerInfoArray;
    function queueSelect(inputElems: SchedulerInfoArray; selMask: std_logic_vector) return SchedulerInfo;


    -- Debug functions
    function DB_setProducer(dbd: DbDependency; tag: InsTag) return DbDependency;
    function DB_incCyclesWaiting(dbd: DbDependency) return DbDependency;
    function DB_incCyclesReady(dbd: DbDependency) return DbDependency;

end LogicIssue;



package body LogicIssue is

    function getControlSignals(content: SchedulerInfoArray; events: EventState) return SlotControlArray is
        variable res: SlotControlArray(content'range);        
        variable readyFullVec, selectedVec: std_logic_vector(content'range) := (others => '0');
    begin
        for i in res'range loop        
            res(i).full := content(i).dynamic.full;
            res(i).active := content(i).dynamic.active;
            res(i).issued := content(i).dynamic.issued;
            res(i).freed := content(i).dynamic.freed;
            
            res(i).trial := compareTagBefore(events.preExecTags.renameIndex, content(i).dynamic.renameIndex);
            res(i).trial_T := compareIndBefore(events.preExecTags.bqPointerSeq, content(i).static.bqPointerSeq, 6); -- TODO: temp value of PTR_SIZE!

            if false then -- Use bqPointerSeq to flush IQ
               res(i).trial := res(i).trial_T;
            end if;
                
            res(i).trialUpdated := content(i).dynamic.trial;
            res(i).killed := (res(i).trialUpdated and events.execEvent) or events.lateEvent;
            res(i).living := res(i).full and not res(i).killed;

            res(i).ready := not content(i).dynamic.argStates(0).waiting and not content(i).dynamic.argStates(1).waiting and content(i).dynamic.active;          
            res(i).readyFull := res(i).ready;
            res(i).readyLiving := res(i).ready and res(i).living;

            readyFullVec(i) := res(i).readyFull;          
        end loop;
        
        selectedVec := getFirstOne(readyFullVec);
        
        for i in res'range loop
            res(i).selected := selectedVec(i);
        end loop;
        
        return res;
    end function;

    function getFullVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).full;
        end loop;
        return res;
    end function;

    function getLivingVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).living;
        end loop;
        return res;
    end function;

    function getActiveVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).active;
        end loop;
        return res;
    end function;
    
    function getIssuedVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).issued;
        end loop;
        return res;
    end function;
    
    function getFreedVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).freed;
        end loop;
        return res;
    end function;


    function getKilledVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).killed;
        end loop;
        return res;
    end function;

        function getKilledVec_T(arr: SlotControlArray) return std_logic_vector is
            variable res: std_logic_vector(arr'range) := (others => '0');
        begin
            for i in res'range loop
                res(i) := arr(i).killed_T;
            end loop;
            return res;
        end function;

    function getTrialVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).trial;
        end loop;
        return res;
    end function;

        function getTrialVec_T(arr: SlotControlArray) return std_logic_vector is
            variable res: std_logic_vector(arr'range) := (others => '0');
        begin
            for i in res'range loop
                res(i) := arr(i).trial_T;
            end loop;
            return res;
        end function;

    function getTrialUpdatedVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).trialUpdated;
        end loop;
        return res;
    end function;
    
    function getReadyVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).ready;
        end loop;
        return res;
    end function;

    function getReadyFullVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).readyFull;
        end loop;
        return res;
    end function;
    
    function getReadyLiveVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).readyLiving;
        end loop;
        return res;
    end function;    
    
    function getSelectedVec(arr: SlotControlArray) return std_logic_vector is
        variable res: std_logic_vector(arr'range) := (others => '0');
    begin
        for i in res'range loop
            res(i) := arr(i).selected;
        end loop;
        return res;
    end function;
    
    
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
        res.dbInfo := isl.ins.dbInfo;
            
        res.operation := isl.ins.specificOperation;
    
        res.branchIns := isl.ins.classInfo.branchIns;
        res.bqPointer := isl.ins.tags.bqPointer;
        res.sqPointer := isl.ins.tags.sqPointer;
        res.lqPointer := isl.ins.tags.lqPointer;        
        res.bqPointerSeq := isl.ins.tags.bqPointerSeq;        
        
        res.tags := isl.ins.tags;
        
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


    function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean; ri: RenameInfo;
                                    TMP_renamedDest: SmallNumber; TMP_renamedSrcs: SmallNumberArray(0 to 2)) return DynamicInfo is
        variable res: DynamicInfo := DEFAULT_DYNAMIC_INFO;
    begin
        res.full := isl.full;
        res.active := res.full;
        res.issued := '0';
        res.trial := '0';
    
        res.renameIndex := isl.ins.tags.renameIndex;

        res.intDestSel := ri.destSel and not ri.destSelFP;
        res.floatDestSel := ri.destSelFP;
        res.dest := ri.physicalDest;
    
        for i in 0 to 2 loop
            res.argStates(i).dbDep := DB_setProducer(res.argStates(i).dbDep, ri.dbDepTags(i));
        
            res.argStates(i).used := ri.sourceSel(i);
            res.argStates(i).zero := ri.sourceConst(i);

            res.argStates(i).reg := ri.physicalSourcesNew(i);
            res.argStates(i).iqTag := TMP_renamedSrcs(i);

            -- Possibility to implement late allocation or advanced renaming schemes - delayed selection of args
            if false then
                if ri.sourcesNew(i) = '1' then
                    res.argStates(i).reg := ri.physicalSourcesNew(i);
                elsif ri.sourcesStable(i) = '1' then
                    res.argStates(i).reg := ri.physicalSourcesStable(i);
                else
                    res.argStates(i).reg := ri.physicalSources(i);
                end if;
            end if;

            if i = 1 then
                res.argStates(i).imm := stInfo.immediate;
                res.argStates(i).value := stInfo.immValue;
            end if;
            
            res.argStates(i).canFail := '0';
            res.argStates(i).waiting := not stInfo.zero(i);
            res.argStates(i).stored := '0';

            res.argStates(i).srcPipe := (others => '0');
            res.argStates(i).srcStage := "00000010";
        end loop;
        
        if IMM_AS_REG and HAS_IMM and isl.ins.constantArgs.immSel = '1' then
           res.argStates(1).reg := isl.ins.constantArgs.imm(PhysName'length-1 downto 0);
           res.argStates(1).reg(7) := '0';
        end if;
                     
        return res;
    end function; 
    
    
    function getIssueInfoArray(insVec: InstructionSlotArray; mask: std_logic_vector; constant USE_IMM: boolean; ria: RenameInfoArray;
                                    TMP_renamedDests: SmallNumberArray; TMP_renamedSources: SmallNumberArray) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1);
        variable slot: InstructionSlot := DEFAULT_INS_SLOT;
    begin
        for i in res'range loop
            slot := insVec(i);
            slot.full := mask(i);
            res(i).static := getIssueStaticInfo(slot, USE_IMM, ria(i));
            res(i).dynamic := getIssueDynamicInfo(slot, res(i).static, USE_IMM, ria(i), TMP_renamedDests(i), TMP_renamedSources(3*i to 3*i + 2));
        end loop;
        return res;    
    end function;


    function getSchedEntrySlot(info: SchedulerInfo; full: std_logic; iqTag: SmallNumber) return SchedulerState is
        variable res: SchedulerState := DEFAULT_SCHED_STATE;
    begin
        res.full := full;
    
        res.operation := info.static.operation;
    
        res.branchIns := info.static.branchIns;

        res.tags := info.static.tags;
        
        res.immediate := info.static.immediate;    
        res.immValue := info.static.immValue;
            
        res.poisoned := info.dynamic.poisoned;
            
        res.zero := info.static.zero;
        
        res.renameIndex := info.dynamic.renameIndex;

        res.readNew := (others => '0');
        
        res.argSpec.args(0) := info.dynamic.argStates(0).reg;
        res.argSpec.args(1) := info.dynamic.argStates(1).reg;
        res.argSpec.args(2) := info.dynamic.argStates(2).reg;

        res.argSpec.intArgSel := (others => '0');
        res.argSpec.floatArgSel := (others => '0');

        res.argSpec.intDestSel := info.dynamic.intDestSel;
        res.argSpec.floatDestSel := info.dynamic.floatDestSel;
        res.argSpec.dest := info.dynamic.dest;

            res.destTag := iqTag;

        for k in 0 to 2 loop
            res.argLocsPipe(k) := info.dynamic.argStates(k).srcPipe;
            res.argSrc(k) := info.dynamic.argStates(k).srcStage;
        end loop;

        return res;
    end function;
    
    
    function TMP_prepareDispatchSlot(input: SchedulerState; prevSending: std_logic) return SchedulerState is
        variable res: SchedulerState := input;
    begin
        res.full := prevSending;
        if prevSending = '0' or (input.argSpec.intDestSel = '0' and input.argSpec.floatDestSel = '0') then
           res.argSpec.dest := PHYS_NAME_NONE; -- Don't allow false notifications of args
           res.destTag := (others => '1');
        end if;
    
        return res;
    end function;

    function getDispatchArgValues_Is(input: SchedulerState; prevSending: std_logic) return SchedulerState is
        variable res: SchedulerState := TMP_prepareDispatchSlot(input, prevSending);
    begin
        if IMM_AS_REG then
            res.immValue(PhysName'length-2 downto 0) := res.argSpec.args(1)(6 downto 0);
        end if;
        
        return res;
    end function;
    
    function updateDispatchArgs_Is(st: SchedulerState) return SchedulerState is
        variable res: SchedulerState := st;
    begin
        res.readNew(0) := bool2std(res.argSrc(0)(1 downto 0) = "11");
        res.readNew(1) := bool2std(res.argSrc(1)(1 downto 0) = "11");
    
        if res.argSrc(0)(1) /= '1' then
            res.argSpec.args(0) := (others => '0');
        end if;
    
        if res.argSrc(1)(1) /= '1' or res.zero(1) = '1' then
            res.argSpec.args(1) := (others => '0');
        end if;
        
        return res;
    
    end function;
    
    
    function getDispatchArgValues_RR(input: SchedulerState;
                                     prevSending: std_logic;
                                     fni: ForwardingInfo;
                                     USE_IMM: boolean; REGS_ONLY: boolean)
    return SchedulerState is
        variable res: SchedulerState := TMP_prepareDispatchSlot(input, prevSending);
    begin
    
        if REGS_ONLY then
            return res;    
        end if;
    
        if res.zero(0) = '1' then
            res.args(0) := (others => '0');
        elsif res.argSrc(0)(1 downto 0) = "00" then
            res.args(0) := fni.values0(slv2u(res.argLocsPipe(0)(1 downto 0)));
        elsif res.argSrc(0)(1 downto 0) = "01" then
            res.args(0) := fni.values1(slv2u(res.argLocsPipe(0)(1 downto 0)));
        else
            res.args(0) := (others => '0');           
        end if;
    
        if res.zero(1) = '1' then
            if USE_IMM then
                res.args(1)(31 downto 16) := (others => res.immValue(15));
                res.args(1)(15 downto 0) := res.immValue;
            else
                res.args(1) := (others => '0');
            end if;
        elsif res.argSrc(1)(1 downto 0) = "00" then
            res.args(1) := fni.values0(slv2u(res.argLocsPipe(1)(1 downto 0)));
        elsif res.argSrc(1)(1 downto 0) = "01" then
            res.args(1) := fni.values1(slv2u(res.argLocsPipe(1)(1 downto 0)));
        else
            res.args(1) := (others => '0');
        end if;
        
        return res;
    end function;
    
    
    function updateDispatchArgs_RR(st: SchedulerState; vals: MwordArray; regValues: MwordArray; REGS_ONLY: boolean)
    return SchedulerState is
        variable res: SchedulerState := st;
    begin
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
    
        if res.readNew(1) = '1' then
            res.args(1) := vals(slv2u(res.argLocsPipe(1)(1 downto 0)));
        else
            res.args(1) := res.args(1) or regValues(1);
        end if;
    
        return res;
    end function;



        function TMP_incSrcStage(stage: SmallNumber) return SmallNumber is
        begin
            case stage(1 downto 0) is
                when "11" =>
                    return "00000000";
                when "00" =>
                    return "00000001";               
                when others =>
                    return "00000010";
            end case;            
        end function;

        function setMatch(matchVec: std_logic_vector; p: natural; stage: integer; fc: ForwardingComparisons) return std_logic_vector is
            variable res: std_logic_vector(matchVec'range) := matchVec;
        begin
            case stage is
                when -3 =>
                    res(p) := fc.cmpM3(p);
                when -2 =>
                    res(p) := fc.cmpM2(p);
                when -1 =>
                    res(p) := fc.cmpM1(p);
                when 0 =>
                    res(p) := fc.cmp0(p);
                when 1 =>
                    res(p) := fc.cmp1(p);
                when others =>
                    res(p) := '0';
            end case;
            return res;
        end function;

        function tmpMin(a, b: integer) return integer is
        begin
            if b < a then
                return b;
            else
                return a;
            end if;
        end function;

    function getWakeupStructStatic(fc: ForwardingComparisons; forwardingModes: ForwardingModeArray; selection: boolean)
    return WakeupStruct is
        variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
        variable matchVec: std_logic_vector(0 to 2) := (others => '0');
        variable srcStage: natural := 2;
    begin
        for p in forwardingModes'range loop
            matchVec := setMatch(matchVec, p, forwardingModes(p).stage, fc);
            if matchVec(p) = '1' then
                res.argLocsPipe(2 downto 0) := i2slv(p, 3);
                
                srcStage := tmpMin(forwardingModes(p).stage + 1, 2);
                if not selection then -- if not selection: move srcStage by 1
                    srcStage := tmpMin(forwardingModes(p).stage + 2, 2);
                end if;

                res.argSrc(1 downto 0) := i2slv(srcStage, 2);              
            end if;
        end loop;
        res.match := isNonzero(matchVec);

        return res;
    end function;

    function getWakeupStructDynamic(fc: ForwardingComparisons; forwardingModes: ForwardingModeArray) return WakeupStruct is
        variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
        variable matchVec: std_logic_vector(0 to 2) := (others => '0');
        variable latestStage: integer := 1;
        variable srcStage: natural := 2;
    begin
        for p in forwardingModes'range loop
                if forwardingModes(p).stage < 0 then
                    latestStage := -1;
                else
                    latestStage := 1;
                end if;

            for q in -3 to 1 loop
                if forwardingModes(p).stage <= q and q <= latestStage then
                    matchVec := setMatch(matchVec, p, q, fc);   
                    if matchVec(p) = '1' then
                        res.argLocsPipe(2 downto 0) := i2slv(p, 3);
                        srcStage := tmpMin(q + 2, 2);
                        res.argSrc(1 downto 0) := i2slv(srcStage, 2);

                        exit;
                    end if;               
                end if;
            end loop;
        end loop;
        res.match := isNonzero(matchVec);

        return res;
    end function;

            function getWakeupStruct(fc: ForwardingComparisons; config: SchedulerUpdateConfig)
            return WakeupStruct is
                variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
            begin
                if not config.dynamic then
                    res := getWakeupStructStatic(fc, config.fwModes, config.selection);
                else
                    res := getWakeupStructDynamic(fc, config.fwModes);
                end if;            

                return res;
            end function;

        -- TODO: include config to distinguish memFail 2 cycles after wakeup (Int) and 1 cycle after wakeup (FP)
        function dependsOnMemHit(state: ArgumentState; constant IS_FP: boolean) return std_logic is
            variable matchingCtr: SmallNumber := sn(1);
        begin
            if IS_FP then
                matchingCtr := sn(0);
            end if;
            return bool2std(state.srcPipe(1 downto 0) = "10" and state.activeCounter = matchingCtr) and not state.zero and not state.waiting;
        end function;

        function updateArgInfo_A(argState: ArgumentState) return ArgumentState is
            variable res: ArgumentState := argState;  
        begin
            res.activeCounter := addIntTrunc(res.activeCounter, 1, 2);
            res.srcStage := TMP_incSrcStage(res.srcStage);
            return res;
        end function;

        function setArgReady(argState: ArgumentState; wakeups: WakeupStruct)
        return ArgumentState is
            variable res: ArgumentState := argState;
        begin
            -- apply wakeup
            res.srcPipe := wakeups.argLocsPipe;
            res.srcStage := wakeups.argSrc;
            res.waiting := '0';
            res.activeCounter := sn(0);--(others => '0');

            return res;
        end function;

        function updateWaitingArg(argState: ArgumentState; wakeups: WakeupStruct)
        return ArgumentState is
            variable res: ArgumentState := argState;
        begin
            if argState.waiting = '1' and wakeups.match = '1' then
                res := setArgReady(res, wakeups);
            end if;

            return res;
        end function;

        function retractArg(argState: ArgumentState) return ArgumentState is
            variable res: ArgumentState := argState;
        begin
            res.waiting := '1';
            return res;
        end function;

    function updateSchedulerState(state: SchedulerInfo;
                                  fm: ForwardingMatches;
                                  memFail: std_logic;
                                  config: SchedulerUpdateConfig
                                  )
    return SchedulerInfo is
        variable res: SchedulerInfo := state;
        variable wakeups: WakeupStruct := DEFAULT_WAKEUP_STRUCT;  
    begin
        -- Fast wakeups: 
        if config.selection then
            -- wakeup
            for a in 0 to 1 loop
                wakeups := getWakeupStruct(fm.cmps(a), config);
                res.dynamic.argStates(a) := updateWaitingArg(res.dynamic.argStates(a), wakeups);
            end loop;
        end if;

        if not config.selection then
            for a in 0 to 1 loop
                wakeups := getWakeupStruct(fm.cmps(a), config);
                res.dynamic.argStates(a) := updateArgInfo_A(res.dynamic.argStates(a));

                if memFail = '1' and not config.ignoreMemFail then
                -- Resetting to waiting state
                    if dependsOnMemHit(state.dynamic.argStates(a), config.fp) = '1' then -- Remember, this depends on "old" state, before counter increments!
                        res.dynamic.argStates(a) := retractArg(res.dynamic.argStates(a));
                    end if;
                else
                -- wakeup
                    res.dynamic.argStates(a) := updateWaitingArg(res.dynamic.argStates(a), wakeups);
                end if;
            end loop;
        end if;

        return res;
    end function;
    
    
    function insertElements(content: SchedulerInfoArray; newArr: SchedulerInfoArray; insertionLocs: slv2D) return SchedulerInfoArray is
        constant LEN: natural := content'length;
        variable res: SchedulerInfoArray(content'range) := content;
    begin
            for i in 0 to PIPE_WIDTH-1 loop
                for k in 0 to LEN-1 loop
                    if insertionLocs(k, i) = '1' then
                        res(k) := newArr(i);
                            res(k).dynamic.trial := '1'; -- set by default because new elems are obviously younger than an issued branch. will be cleared next cycle if no more on trial
                        exit;
                    end if;
                end loop;
            end loop;
        return res;
    end function;


    function handleIqDbInfo(queueContent: SchedulerInfoArray) return SchedulerInfoArray is
        constant LEN: natural := queueContent'length;
        variable res: SchedulerInfoArray(queueContent'range) := queueContent;    
    begin
        for i in 0 to LEN-1 loop
            for j in 0 to 2 loop
                if res(i).dynamic.issued = '1' then
                    null;
                elsif res(i).dynamic.argStates(j).waiting = '1' then
                    res(i).dynamic.argStates(j).dbDep := DB_incCyclesWaiting(res(i).dynamic.argStates(j).dbDep);
                else
                    res(i).dynamic.argStates(j).dbDep := DB_incCyclesReady(res(i).dynamic.argStates(j).dbDep);
                end if;
            end loop;
        end loop;

        for i in 0 to LEN-1 loop
            if res(i).dynamic.full /= '1' then
                res(i).static.dbInfo := DEFAULT_DEBUG_INFO;
                res(i).dynamic.argStates(0).dbDep := DEFAULT_DB_DEPENDENCY;
                res(i).dynamic.argStates(1).dbDep := DEFAULT_DB_DEPENDENCY;
                res(i).dynamic.argStates(2).dbDep := DEFAULT_DB_DEPENDENCY;
            end if;
        end loop;        
        return res;
    end function;

    function prepareNewArr(input: SchedulerInfoArray; rrf: std_logic_vector) return SchedulerInfoArray is
        --constant LEN: natural := queueContent'length;
        variable res: SchedulerInfoArray(input'range) := input;
        variable rm, rrfFull: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
    begin
        for j in 0 to PIPE_WIDTH-1 loop
            rm(3*j to 3*j + 2) := (others => input(j).dynamic.full); 
        end loop;

        rrfFull := rm and rrf;
    
        res := restoreRenameIndex(updateRR(input, rrfFull));
   
        return res;
    end function;

        function removeEntry(entry: SchedulerInfo; freed: std_logic) return SchedulerInfo is
            variable res: SchedulerInfo := entry;
        begin
            res.dynamic.full := '0';
            res.dynamic.issued := '0';
            --res.dynamic.freed := freed;
            res.dynamic.active := '0';
            res.dynamic.stageCtr := sn(0);
            
            return res;
        end function;
        
        function pullbackEntry(entry: SchedulerInfo) return SchedulerInfo is
            variable res: SchedulerInfo := entry;
        begin
            res.dynamic.issued := '0';
            res.dynamic.active := '1';
            res.dynamic.stageCtr := sn(0);
            
            return res;
        end function;
        
        function updateIssuedEntry(entry: SchedulerInfo) return SchedulerInfo is
            variable res: SchedulerInfo := entry;
        begin
            res.dynamic.stageCtr := addInt(res.dynamic.stageCtr, 1);

            return res;
        end function;
        
        function issueEntry(entry: SchedulerInfo) return SchedulerInfo is
            variable res: SchedulerInfo := entry;
        begin
            res.dynamic.issued := '1';
            res.dynamic.active := '0';
            
            -- TODO: change to hardcoded 1h (then maybe shoudl be made 0 and all checks corrected by -1?
            res.dynamic.stageCtr := --addInt(res.dynamic.stageCtr, 1);
                                    --sn(1);
                                    sn(0);
            return res;
        end function;


--    function TMP_updateIQ(queueContent: SchedulerInfoArray;
--                      sends: std_logic;
--                      killMask, trialMask, selMask: std_logic_vector;
--                      memFail: std_logic)
--    return SchedulerInfoArray is
--        constant LEN: natural := queueContent'length;
--        variable res: SchedulerInfoArray(queueContent'range) := queueContent;
--        variable rm, rrfFull: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
--    begin
--        for i in 0 to LEN-1 loop
--            res(i).dynamic.freed := '0'; -- This is set for 1 cycle when freeing

--            if queueContent(i).dynamic.issued = '1' then
--                -- Remove after successful issue
--                if slv2u(res(i).dynamic.stageCtr) = IQ_HOLD_TIME then
--                    -- if isDone
--                    res(i) := removeEntry(res(i), '1');
--                    res(i).dynamic.freed := '1';

--                -- pull back because mem miss
--                -- if needsPullback
--                elsif memFail = '1' and queueContent(i).dynamic.stageCtr(1 downto 0) = "01" then
--                    res(i) := pullbackEntry(res(i));
--                else
--                    res(i) := updateIssuedEntry(res(i));
--                end if;
--            end if;

--            -- set issued
--            if (selMask(i) and sends) = '1' then
--                res(i) := issueEntry(res(i));
--            end if;

--            -- flush on event
--            if killMask(i) = '1' then -- the same as at removing successfully issued?
--                res(i) := removeEntry(res(i), '0');                
--             end if;

--             -- set age comparison for possible subsequent flush
--             -- this is done regardless of other parts of state
--             res(i).dynamic.trial := trialMask(i);        

--             res(i).dynamic.stageCtr(SMALL_NUMBER_SIZE-1 downto 2) := (others => '0'); -- clear unused bits 
--        end loop;

--        return res;
--    end function;


    function iqNext_NS(queueContent: SchedulerInfoArray;
                      sends: std_logic; killMask, trialMask, selMask: std_logic_vector; memFail: std_logic)
    return SchedulerInfoArray is
        constant LEN: natural := queueContent'length;
        variable res: SchedulerInfoArray(queueContent'range) := queueContent;
       -- variable newArr: SchedulerInfoArray(0 to PIPE_WIDTH-1) := inputData;                
        variable rm, rrfFull: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
    begin
        for i in 0 to LEN-1 loop
            res(i).dynamic.freed := '0'; -- This is set for 1 cycle when freeing

            if queueContent(i).dynamic.issued = '1' then
                -- Remove after successful issue
                if slv2u(res(i).dynamic.stageCtr) = IQ_HOLD_TIME
                                                               - 1   then
                    -- if isDone
                    res(i) := removeEntry(res(i), '1');
                    res(i).dynamic.freed := '1';

                -- pull back because mem miss
                -- if needsPullback
                elsif memFail = '1' and queueContent(i).dynamic.stageCtr(1 downto 0) = --"01" then
                                                                                        "00" then
                    res(i) := pullbackEntry(res(i));
                else
                    res(i) := updateIssuedEntry(res(i));
                end if;
            end if;

            -- set issued
            if (selMask(i) and sends) = '1' then
                res(i) := issueEntry(res(i));
            end if;

            -- flush on event
            if killMask(i) = '1' then -- the same as at removing successfully issued?
                res(i) := removeEntry(res(i), '0');
                    --res(i).dynamic.freed := '0';
             end if;

             -- set age comparison for possible subsequent flush
             -- this is done regardless of other parts of state
             res(i).dynamic.trial := trialMask(i);        

             res(i).dynamic.stageCtr(SMALL_NUMBER_SIZE-1 downto 2) := (others => '0'); -- clear unused bits 
        end loop;

        return res;
    end function;


    
        function iqNext_NS_2(queueContent: SchedulerInfoArray;
                          inputData: SchedulerInfoArray;               
                          prevSending: std_logic;
                          insertionLocs: slv2D)
        return SchedulerInfoArray is
            constant LEN: natural := queueContent'length;
            variable res: SchedulerInfoArray(queueContent'range) := queueContent;
            variable newArr: SchedulerInfoArray(0 to PIPE_WIDTH-1) := inputData;                
            variable rm, rrfFull: std_logic_vector(0 to 3*PIPE_WIDTH-1) := (others => '0');
        begin
            newArr := inputData;
                        
            if prevSending = '1' then
                res := insertElements(res, newArr, insertionLocs);
            end if;
    
            res := handleIqDbInfo(res);
    
            return res;
        end function;


    function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector is
        variable res: std_logic_vector(list'range) := (others => '0');
    begin
        for i in list'range loop
            if tag(PHYS_REG_BITS-1 downto 0) = list(i)(PHYS_REG_BITS-1 downto 0)
                 --   and isNonzero(tag) = '1' 
            then
                res(i) := '1';
            end if;
        end loop;
        return res;
    end function;

    function findIqTag(tag: SmallNumber; list: SmallNumberArray) return std_logic_vector is
        variable res: std_logic_vector(list'range) := (others => '0');
        variable candidate: SmallNumber := sn(0);
        variable iqSelector: SmallNumber := sn(0);
    begin
        for i in list'range loop
            candidate := list(i) and X"0f";
            iqSelector := sn(16*(1+i));
            candidate := candidate or iqSelector;
            if tag(5 downto 0) = candidate(5 downto 0) and list(i)(7) /= '1' then
                res(i) := '1';
            end if;
        end loop;
        return res;
    end function;


            function updateSchedulerArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; fma: ForwardingMatchesArray;-- forwardingModes0: ForwardingModeArray;
                            memFail: std_logic;
                            config: SchedulerUpdateConfig)
            return SchedulerInfoArray is
                variable res: SchedulerInfoArray(0 to schedArray'length-1);
            begin
                for i in schedArray'range loop
                    res(i) := updateSchedulerState(schedArray(i), fma(i), memFail, config);
                end loop;    
                return res;
            end function;

    function findForwardingMatches(info: SchedulerInfo; fni: ForwardingInfo; config: SchedulerUpdateConfig; readyRegs: std_logic_vector) return ForwardingMatches is
        variable res: ForwardingMatches := DEFAULT_FORWARDING_MATCHES;
        constant arg0: PhysName := info.dynamic.argStates(0).reg;
        constant arg1: PhysName := info.dynamic.argStates(1).reg;
        
        constant tag0: SmallNumber := info.dynamic.argStates(0).iqTag;
        constant tag1: SmallNumber := info.dynamic.argStates(1).iqTag;
    begin
        if not config.matchIQ then
            res.cmps(0).reg   := '0';
            res.cmps(0).cmp1  := findRegTag(arg0, fni.tags1);
            res.cmps(0).cmp0  := findRegTag(arg0, fni.tags0);        
            res.cmps(0).cmpM1 := findRegTag(arg0, fni.nextTagsM1);
            res.cmps(0).cmpM2 := findRegTag(arg0, fni.nextTagsM2);        
            res.cmps(0).cmpM3 := findRegTag(arg0, fni.nextTagsM3);
    
            res.cmps(1).reg   := '0';
            res.cmps(1).cmp1  := findRegTag(arg1, fni.tags1);
            res.cmps(1).cmp0  := findRegTag(arg1, fni.tags0);        
            res.cmps(1).cmpM1 := findRegTag(arg1, fni.nextTagsM1);
            res.cmps(1).cmpM2 := findRegTag(arg1, fni.nextTagsM2);        
            res.cmps(1).cmpM3 := findRegTag(arg1, fni.nextTagsM3);
        else
            res.cmps(0).reg   := '0';
            res.cmps(0).cmp1  := findIqTag(tag0, fni.iqTags1);
            res.cmps(0).cmp0  := findIqTag(tag0, fni.iqTags0);        
            res.cmps(0).cmpM1 := findIqTag(tag0, fni.iqTagsM1);
            res.cmps(0).cmpM2 := findIqTag(tag0, fni.iqTagsM2);        
            res.cmps(0).cmpM3 := findIqTag(tag0, fni.iqTagsM3);

            res.cmps(1).reg   := '0';
            res.cmps(1).cmp1  := findIqTag(tag1, fni.iqTags1);
            res.cmps(1).cmp0  := findIqTag(tag1, fni.iqTags0);        
            res.cmps(1).cmpM1 := findIqTag(tag1, fni.iqTagsM1);
            res.cmps(1).cmpM2 := findIqTag(tag1, fni.iqTagsM2);        
            res.cmps(1).cmpM3 := findIqTag(tag1, fni.iqTagsM3);
        end if;

        return res;
    end function;

    function findForwardingMatches_N(info: SchedulerInfo; fni: ForwardingInfo; regInfo: RegisterStateArray) return ForwardingMatches is
        variable res: ForwardingMatches := DEFAULT_FORWARDING_MATCHES;
        constant arg0: PhysName := info.dynamic.argStates(0).reg;
        constant arg1: PhysName := info.dynamic.argStates(1).reg;
    begin
        res.cmps(0).reg   := regInfo(0).ready;
        res.cmps(0).cmp1  := findRegTag(arg0, fni.tags1);
        res.cmps(0).cmp0  := findRegTag(arg0, fni.tags0);
        res.cmps(0).cmpM1 := findRegTag(arg0, fni.nextTagsM1);
        res.cmps(0).cmpM2 := findRegTag(arg0, fni.nextTagsM2);
        res.cmps(0).cmpM3 := findRegTag(arg0, fni.nextTagsM3);

        res.cmps(1).reg   := regInfo(1).ready;
        res.cmps(1).cmp1  := findRegTag(arg1, fni.tags1);
        res.cmps(1).cmp0  := findRegTag(arg1, fni.tags0);
        res.cmps(1).cmpM1 := findRegTag(arg1, fni.nextTagsM1);
        res.cmps(1).cmpM2 := findRegTag(arg1, fni.nextTagsM2);
        res.cmps(1).cmpM3 := findRegTag(arg1, fni.nextTagsM3);

        return res;
    end function;
    
    
    function findForwardingMatchesArray(schedArray: SchedulerInfoArray; fni: ForwardingInfo; config: SchedulerUpdateConfig; rrf: std_logic_vector) return ForwardingMatchesArray is
        variable res: ForwardingMatchesArray(schedArray'range) := (others => DEFAULT_FORWARDING_MATCHES);
        
        constant readyFlags: std_logic_vector(0 to 3*schedArray'length-1) := (others => '0');
        variable readyFlagsSlice: std_logic_vector(0 to 2) := "000";
    begin
        for i in schedArray'range loop
            readyFlagsSlice := readyFlags(3*i to 3*i + 2);
            res(i) := findForwardingMatches(schedArray(i), fni, config, readyFlagsSlice);
        end loop;
        return res;
    end function;
    
        function findForwardingMatchesArray_N(schedArray: SchedulerInfoArray; fni: ForwardingInfo; rrf: std_logic_vector; regInfo: RegisterStateArray2D) return ForwardingMatchesArray is
            variable res: ForwardingMatchesArray(schedArray'range) := (others => DEFAULT_FORWARDING_MATCHES);
            
            constant readyFlags: std_logic_vector(0 to 3*schedArray'length-1) := (others => '0');
            variable readyFlagsSlice: std_logic_vector(0 to 2) := "000";
        begin
            for i in schedArray'range loop
                readyFlagsSlice := readyFlags(3*i to 3*i + 2);
                res(i) := findForwardingMatches_N(schedArray(i), fni, regInfo(i));
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
       variable rrf: std_logic_vector(0 to 2) := (others => '0');      	   
    begin
       for i in 0 to PIPE_WIDTH-1 loop
           rrf := rr(3*i to 3*i + 2);
           for j in 0 to 2 loop
                res(i).dynamic.argStates(j).waiting := res(i).dynamic.argStates(j).waiting and not rrf(j);
           end loop;                
       end loop;
    
       return res;
    end function;
    
    function restoreRenameIndex(sia: SchedulerInfoArray) return SchedulerInfoArray is
       variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := sia;
    begin
       for i in 1 to PIPE_WIDTH-1 loop
           res(i).dynamic.renameIndex := clearTagLow(res(0).dynamic.renameIndex) or i2slv(i, TAG_SIZE);
       end loop;
       return res;
    end function;
    
    function orSchedEntrySlot(a, b: SchedulerInfo) return SchedulerInfo is
        variable res: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
    begin
    
        if a.static.operation.subpipe /= None then
            res.static.operation.subpipe := a.static.operation.subpipe;  
        else
            res.static.operation.subpipe := b.static.operation.subpipe;
        end if;
        res.static.operation.bits := a.static.operation.bits or b.static.operation.bits;
        res.static.operation.arith := ArithOp'val(slv2u(res.static.operation.bits));
        res.static.operation.memory := MemOp'val(slv2u(res.static.operation.bits));
        res.static.operation.float := FpOp'val(slv2u(res.static.operation.bits));
    
        res.static.branchIns := a.static.branchIns or b.static.branchIns;
        res.static.bqPointer := a.static.bqPointer or b.static.bqPointer;
        res.static.sqPointer := a.static.sqPointer or b.static.sqPointer;
        res.static.lqPointer := a.static.lqPointer or b.static.lqPointer;   
        res.static.bqPointerSeq := a.static.bqPointerSeq or b.static.bqPointerSeq;
        
            res.static.tags.renameIndex := a.static.tags.renameIndex or b.static.tags.renameIndex;
            res.static.tags.bqPointer := a.static.tags.bqPointer or b.static.tags.bqPointer;
            res.static.tags.sqPointer := a.static.sqPointer or b.static.tags.sqPointer;
            res.static.tags.lqPointer := a.static.lqPointer or b.static.tags.lqPointer;   
            res.static.tags.bqPointerSeq := a.static.bqPointerSeq or b.static.tags.bqPointerSeq;
            
        res.static.immediate :=  a.static.immediate or b.static.immediate;
        res.static.immValue :=  a.static.immValue or b.static.immValue;
        res.static.zero :=  a.static.zero or b.static.zero;
        
    
        res.dynamic.full := a.dynamic.full or b.dynamic.full;
        res.dynamic.active := a.dynamic.active or b.dynamic.active;
        res.dynamic.issued := a.dynamic.issued or b.dynamic.issued;
        res.dynamic.trial := a.dynamic.trial or b.dynamic.trial;

        res.dynamic.poisoned := a.dynamic.poisoned or b.dynamic.poisoned;
        
        res.dynamic.renameIndex := a.dynamic.renameIndex or b.dynamic.renameIndex;

        res.dynamic.dest := a.dynamic.dest or b.dynamic.dest;
        res.dynamic.intDestSel := a.dynamic.intDestSel or b.dynamic.intDestSel;
        res.dynamic.floatDestSel := a.dynamic.floatDestSel or b.dynamic.floatDestSel;

        for i in 0 to 2 loop
            res.dynamic.argStates(i).used := a.dynamic.argStates(i).used or b.dynamic.argStates(i).used;
            res.dynamic.argStates(i).reg := a.dynamic.argStates(i).reg or b.dynamic.argStates(i).reg;
            res.dynamic.argStates(i).zero := a.dynamic.argStates(i).used or b.dynamic.argStates(i).zero;
            res.dynamic.argStates(i).imm := a.dynamic.argStates(i).imm or b.dynamic.argStates(i).imm;
            res.dynamic.argStates(i).value := a.dynamic.argStates(i).value or b.dynamic.argStates(i).value;

            res.dynamic.argStates(i).canFail := a.dynamic.argStates(i).canFail or b.dynamic.argStates(i).canFail;
            res.dynamic.argStates(i).waiting := a.dynamic.argStates(i).waiting or b.dynamic.argStates(i).waiting;
            res.dynamic.argStates(i).stored := a.dynamic.argStates(i).stored or b.dynamic.argStates(i).stored;

            res.dynamic.argStates(i).srcPipe := a.dynamic.argStates(i).srcPipe or b.dynamic.argStates(i).srcPipe;
            res.dynamic.argStates(i).srcStage := a.dynamic.argStates(i).srcStage or b.dynamic.argStates(i).srcStage;
        end loop;
        
        return res;
    end function;
    
    
    function getNewLocsBanked(fullMask: std_logic_vector) return slv2D is
        constant QUEUE_SIZE_EXT: natural := fullMask'length;
        variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
        variable cnt: natural := 0;
        constant N_BANKS: natural := 4;
        constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
    begin
       for b in 0 to N_BANKS-1 loop
           for i in 0 to BANK_SIZE-1 loop
                if fullMask(i * N_BANKS + b) /= '1' then
                    res(i * N_BANKS + b, b) := '1';
                    exit;
                end if;
            end loop;
        end loop;

        return res;
    end function;

    function getBankCounts(fullMask: std_logic_vector) return SmallNumberArray is
        constant QUEUE_SIZE_EXT: natural := fullMask'length;
        variable res: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
        variable cnt: natural := 0;
        constant N_BANKS: natural := 4;
        constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
    begin
       for i in 0 to BANK_SIZE-1 loop
           for b in 0 to N_BANKS-1 loop
                if fullMask(i * N_BANKS + b) = '1' then
                    res(b) := addInt(res(b), 1);
                end if;
            end loop;
        end loop;            
        return res;
    end function;
    
    function acceptingBanked(counts: SmallNumberArray; constant LIMIT: natural) return std_logic is
        variable cnt: natural := 0;
        constant N_BANKS: natural := counts'length;
    begin
       for b in 0 to N_BANKS-1 loop
            if slv2u(counts(b)) > LIMIT - 1  then
                return '0';
            end if;
        end loop;
        return '1';
    end function;
    
    function acceptingMoreBanked(counts: SmallNumberArray; constant LIMIT: natural) return std_logic is
        variable cnt: natural := 0;
        constant N_BANKS: natural := counts'length;
    begin
       for b in 0 to N_BANKS-1 loop
            if slv2u(counts(b)) > LIMIT - 2  then
                return '0';
            end if;
        end loop;
        return '1';
    end function;
    
    function updateAgeMatrix(ageMatrix, insertionLocs: slv2D; fullMask: std_logic_vector) return slv2D is
        constant QUEUE_SIZE_EXT: natural := fullMask'length;
        variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := ageMatrix;
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            -- insert current fullMask into proper row    
            for j in 0 to QUEUE_SIZE_EXT-1 loop
                if insertionLocs(j, i) = '1' then
                    -- Sorry, assigment doesn;t work for ranges in >= 2D
                    for k in 0 to QUEUE_SIZE_EXT-1 loop
                        res(j, k) := fullMask(k); -- Maybe setting to all ones would work too?
                    end loop;
                    
                    -- Preceding in this group are also masked!
                    for p in 0 to PIPE_WIDTH-1 loop
                        if p = i then
                            exit;
                        end if;
                        
                        for k in 0 to QUEUE_SIZE_EXT-1 loop
                            if insertionLocs(k, p) = '1' then
                                res(j, k) := '1';
                                exit;
                            end if;
                        end loop;
                    end loop;
                    
                    -- Clear all dependencies on this new op
                    for k in 0 to QUEUE_SIZE_EXT-1 loop
                        res(k, j) := '0';
                    end loop;
                    
                    exit;
                end if;
            end loop;
        end loop;
        return res;
    end function;
    
    function getSelMask(readyMask: std_logic_vector; ageMatrix: slv2D) return std_logic_vector is
        constant QUEUE_SIZE_EXT: natural := readyMask'length;
        variable res: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        variable row: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            for j in 0 to QUEUE_SIZE_EXT-1 loop
                row(j) := readyMask(j) and ageMatrix(i, j);
            end loop;
            row(i) := '0';
    
            if isNonzero(row(0 to QUEUE_SIZE_EXT/2 - 1)) = '1' or isNonzero(row(QUEUE_SIZE_EXT/2 to QUEUE_SIZE_EXT-1)) = '1' then
                res(i) := '0';
            else
                res(i) := readyMask(i);
            end if;
        end loop;
             
        return res;
    end function;

        function getNewLocs_N(fullMask: std_logic_vector; tags: SmallNumberArray; newArr: SchedulerInfoArray) return slv2D is
            constant QUEUE_SIZE_EXT: natural := fullMask'length;
            variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
            variable cnt: natural := 0;
            constant N_BANKS: natural := 4;
            constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
        begin
           for b in 0 to N_BANKS-1 loop
            --if fullMask(i * N_BANKS + b) /= '1' then
                res(slv2u(tags(b)) * N_BANKS + b, b) := newArr(b).dynamic.full;
            --    exit;
            --end if;
            end loop;

            return res;
        end function;


        function getWakeupInfoA(content, newArray: SchedulerInfoArray; prevSending: std_logic; insertionLocs: slv2D; fni: ForwardingInfo; readyRegFlags: std_logic_vector; config: SchedulerUpdateConfig)
        return WakeupInfoArray is
            constant LEN: natural := content'length;
            variable res: WakeupInfoArray(0 to LEN-1);
            variable queueElem: SchedulerInfo;
            variable ready: std_logic := '0';
            variable waiting0, waiting1: std_logic := '0';
            variable wakeups0, wakeups1: WakeupStruct;
            constant fma: ForwardingMatchesArray(0 to LEN-1) := findForwardingMatchesArray(content, fni, config, "000");
        begin
            -- ignore new elements for now
            for i in 0 to LEN-1 loop
                    res(i).active := '0';
            
                queueElem := content(i);
                --ready := '0';
                if queueElem.dynamic.full = '1' then            
                    queueElem := updateSchedulerState(queueElem, fma(i), '0', config);
                    waiting0 := queueElem.dynamic.argStates(0).waiting;
                    waiting1 := queueElem.dynamic.argStates(1).waiting;
                    
                    if (not waiting0 and content(i).dynamic.argStates(0).waiting) = '1' then
                        wakeups0 := getWakeupStruct(fma(i).cmps(0), config);
                            res(i).arg0.active := '1';
                        res(i).active := '1';
                    end if;
                    
                    if (not waiting1 and content(i).dynamic.argStates(1).waiting) = '1' then
                        wakeups1 := getWakeupStruct(fma(i).cmps(1), config);
                            res(i).arg1.active := '1';
                        res(i).active := '1';
                    end if;

                end if;
            end loop;
            
            -- handle new elements
            for k in 0 to RENAME_W-1 loop
                if (newArray(k).dynamic.full and prevSending) /= '1' then
                    next;
                end if;
                
                for i in 0 to LEN-1 loop
                    
                end loop;
            end loop;
            
            return res;
        end function;


    function updateRenameIndex(content: SchedulerInfoArray) return SchedulerInfoArray is
        constant QUEUE_SIZE_EXT: natural := content'length;
        variable res: SchedulerInfoArray(content'range) := content;
        variable earlyStage: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content(QUEUE_SIZE_EXT - PIPE_WIDTH to QUEUE_SIZE_EXT-1);
    begin
        earlyStage := restoreRenameIndex(earlyStage);
    
        res(QUEUE_SIZE_EXT - PIPE_WIDTH to QUEUE_SIZE_EXT-1) := earlyStage;
        return res;
    end function;
    
    function queueSelect(inputElems: SchedulerInfoArray; selMask: std_logic_vector) return SchedulerInfo is
        constant QUEUE_SIZE_EXT: natural := inputElems'length;
        variable res, a, b: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
        variable maskedQueue: SchedulerInfoArray(inputElems'range) := (others => DEFAULT_SCHEDULER_INFO);
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            if selMask(i) = '1' then
                maskedQueue(i) := inputElems(i);
            end if;
        end loop;
    
        for i in 0 to QUEUE_SIZE_EXT/2-1 loop
            a := orSchedEntrySlot(a, maskedQueue(i));
            b := orSchedEntrySlot(b, maskedQueue(i + QUEUE_SIZE_EXT/2));
        end loop;
        
        res := orSchedEntrySlot(a, b);
        return res;
    end function;

    
    -- Debug functions
    function DB_setProducer(dbd: DbDependency; tag: InsTag) return DbDependency is
        variable res: DbDependency := dbd;
    begin
        -- pragma synthesis off
        res.producer := tag;
        res.cyclesWaiting := 0;
        res.cyclesReady := 0;
        -- pragma synthesis on
        return res;
    end function;

    function DB_incCyclesWaiting(dbd: DbDependency) return DbDependency is
        variable res: DbDependency := dbd;
    begin
        -- pragma synthesis off
        res.cyclesWaiting := res.cyclesWaiting + 1;
        -- pragma synthesis on
        return res;
    end function;

    function DB_incCyclesReady(dbd: DbDependency) return DbDependency is
        variable res: DbDependency := dbd;
    begin
        -- pragma synthesis off
        res.cyclesReady := res.cyclesReady + 1;
        -- pragma synthesis on
        return res;
    end function;

end LogicIssue;
