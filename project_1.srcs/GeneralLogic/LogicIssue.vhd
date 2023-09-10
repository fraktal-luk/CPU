--

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.ForwardingNetwork.all;


package LogicIssue is

constant IQ_HOLD_TIME: natural := 3;

-- Scheduler transient
type WakeupStruct is record
    argLocsPipe: SmallNumber;
    argSrc: SmallNumber;
    match:   std_logic;
    reg: PhysName;
    iqTag: SmallNumber;
    active: std_logic;
end record;

constant DEFAULT_WAKEUP_STRUCT: WakeupStruct := ((others => '0'), "00000010", '0', (others => '0'), sn(0), '0');

-- struct for experimental code
type ArgWakeup is record
    active: std_logic;
    mode: WakeupMode;
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


-- CONFIG
type SchedulerUpdateConfig is record
    dynamic: boolean;
    fp: boolean;
    ignoreMemFail: boolean;
    fwModes: ForwardingModeArray(0 to 2);
    matchIQ: boolean;
end record;

constant DEFUALT_SCHEDULER_UPDATE_CONFIG: SchedulerUpdateConfig := (
    dynamic => false,
    fp => false,
    ignoreMemFail => false,
    fwModes => FORWARDING_MODES_NONE,
    matchIQ => false
);


type WakeupStructArray2D is array(natural range <>, natural range <>) of WakeupStruct;

type IqSelector is (I0, I1, M0, SVI, SVF, F0);

-- Enqueue
function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean; ri: RenameInfo) return StaticInfo; 
function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean; ri: RenameInfo;
                                renamedDest: SmallNumber; renamedSrcs: SmallNumberArray(0 to 2); rrfSlice: std_logic_vector(0 to 2)) return DynamicInfo;

function getIssueInfoArray(insVec: InstructionSlotArray; constant USE_IMM: boolean; ria: RenameInfoArray; rrf: std_logic_vector;
                           renamedDests: SmallNumberArray; renamedSources: SmallNumberArray; iqSel: IqSelector) return SchedulerInfoArray;
                           
function getNewLocs(fullMask: std_logic_vector; tags: SmallNumberArray; newArr: SchedulerInfoArray) return slv2D;


-- API, enqueue and inside IQ
function getSlowWakeups(content: SchedulerInfoArray; bypass: BypassState; config: SchedulerUpdateConfig) return WakeupStructArray2D;
function getFastWakeups(content: SchedulerInfoArray; bypass: BypassState; config: SchedulerUpdateConfig) return WakeupStructArray2D;
function getInitWakeups(content: SchedulerInfoArray; bypass: BypassState; config: SchedulerUpdateConfig) return WakeupStructArray2D;

function updateSchedulerArray(schedArray: SchedulerInfoArray; wakeups: WakeupStructArray2D; memFail: std_logic; config: SchedulerUpdateConfig)
return SchedulerInfoArray;

function updateOnDispatch(schedArray: SchedulerInfoArray; wakeups: WakeupStructArray2D; memFail: std_logic; config: SchedulerUpdateConfig)
return SchedulerInfoArray;

function updateSchedulerArray_S(schedArray: SchedulerInfoArray; wakeups: WakeupStructArray2D; memFail: std_logic; config: SchedulerUpdateConfig)
return SchedulerInfoArray;

function insertElements(content: SchedulerInfoArray; newArr: SchedulerInfoArray; insertionLocs: slv2D) return SchedulerInfoArray;

function updateQueueState(queueContent: SchedulerInfoArray; nextAccepting, sends: std_logic; killMask, trialMask, selMask: std_logic_vector; memFail, unlockDiv: std_logic)
return SchedulerInfoArray;

function storeInput(queueContent: SchedulerInfoArray; inputData: SchedulerInfoArray; prevSending, evt: std_logic; insertionLocs: slv2D)
return SchedulerInfoArray;

function updateAgeMatrix(ageMatrix, insertionLocs: slv2D; fullMask: std_logic_vector) return slv2D;

function getTrialMask(content: SchedulerInfoArray; events: EventState) return std_logic_vector;
function getReadyMask(content: SchedulerInfoArray) return std_logic_vector;
function getFullMask(content: SchedulerInfoArray) return std_logic_vector;
function getFreedMask(content: SchedulerInfoArray) return std_logic_vector;
function getTrialUpdatedMask(content: SchedulerInfoArray) return std_logic_vector;

-- issue
function getSelMask(readyMask: std_logic_vector; ageMatrix: slv2D) return std_logic_vector;
function getSelMask_H(readyMask: std_logic_vector; ageMatrix: slv2D) return std_logic_vector;

function queueSelect(inputElems: SchedulerInfoArray; selMask: std_logic_vector) return SchedulerInfo;
function queueSelect_N(inputElems: SchedulerInfoArray; readyMask: std_logic_vector; ageMatrix: slv2D) return SchedulerInfo;

function TMP_slowSelect(content: SchedulerInfoArray; tag: SmallNumber) return SchedulerInfo;
function TMP_slowSelect(content: std_logic_vector; tag: SmallNumber) return std_logic;

function getSchedEntrySlot(info: SchedulerInfo; full: std_logic; iqTag: SmallNumber) return SchedulerState;

function getIssueTag(sends: std_logic; selMask: std_logic_vector) return SmallNumber;

function getCurrentStates(queueContent: SchedulerInfoArray) return IqStateArray;
-- Debug functions
function DB_setProducer(dbd: DbDependency; tag: InsTag) return DbDependency;
procedure DB_reportEvents(content: SchedulerInfoArray; lastEvents: IqEventArray);

-- experimental, don't export
--function getWakeup(argState: ArgumentState; fni: ForwardingInfo; constant MODES: WakeupSpec; constant MODE_IND: natural) return ArgWakeup;
--function getWakeupArray(content: SchedulerInfoArray; fni: ForwardingInfo; constant WAKEUP_SPEC: WakeupSpec; constant CFG: SchedulerUpdateConfig) return WakeupInfoArray;

end LogicIssue;



package body LogicIssue is

function restoreRenameIndex(sia: SchedulerInfoArray) return SchedulerInfoArray is
   variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1) := sia;
begin
   for i in 1 to PIPE_WIDTH-1 loop
       res(i).dynamic.renameIndex := clearTagLow(res(0).dynamic.renameIndex) or i2slv(i, TAG_SIZE);
   end loop;
   return res;
end function;


function getIssueStaticInfo(isl: InstructionSlot; constant HAS_IMM: boolean; ri: RenameInfo) return StaticInfo is
    variable res: StaticInfo;
begin
    res.dbInfo := isl.ins.dbInfo;

    res.operation := isl.ins.specificOperation;

    res.branchIns := isl.ins.typeInfo.branchIns;      
    res.divIns := isl.ins.dispatchInfo.useDiv;
    res.tags := isl.ins.tags;

    res.immediate := isl.ins.constantArgs.immSel and bool2std(HAS_IMM);    
    res.immValue := isl.ins.constantArgs.imm(15 downto 0);

    if HAS_IMM and isl.ins.constantArgs.immSel = '1' then    
        if IMM_AS_REG then
            res.immValue(PhysName'length-2 downto 0) := (others => '0');
        end if;
    end if;

    for i in 0 to 2 loop
        res.zero(i) := ri.argStates(i).const;
    end loop;
    
    if not HAS_IMM then
        res.immediate := '0';            
    end if;        

    return res;
end function;


function getIssueDynamicInfo(isl: InstructionSlot; stInfo: StaticInfo; constant HAS_IMM: boolean; ri: RenameInfo;
                                renamedDest: SmallNumber; renamedSrcs: SmallNumberArray(0 to 2); rrfSlice: std_logic_vector(0 to 2)) return DynamicInfo is
    variable res: DynamicInfo := DEFAULT_DYNAMIC_INFO;
begin
    res.full := isl.full;

    res.status.trial := '1'; -- Must be 1 because it's younger than any Exec instruction

    if stInfo.divIns = '1' then
        res.status_N.suspended := '1';
    else
        res.status_N.active := '1';
    end if;

    res.renameIndex := isl.ins.tags.renameIndex;

    res.intDestSel := ri.destSel and not ri.destSelFP;
    res.floatDestSel := ri.destSelFP;
    res.dest := ri.physicalDest;

    for a in 0 to 2 loop
        res.argStates(a).dbDep := DB_setProducer(res.argStates(a).dbDep, ri.dbDepTags(a));

        res.argStates(a).zero_T := ri.argStates(a).const;

        res.argStates(a).reg := ri.argStates(a).physicalNew;
        res.argStates(a).iqTag := renamedSrcs(a);

        res.argStates(a).waiting := (not stInfo.zero(a) and not rrfSlice(a)) or ri.argStates(a).hasDep;

        res.argStates(a).srcPipe := (others => '0');
        res.argStates(a).srcStage := "00000010";
    end loop;

    if IMM_AS_REG and HAS_IMM and isl.ins.constantArgs.immSel = '1' then
       res.argStates(1).reg := isl.ins.constantArgs.imm(PhysName'length-1 downto 0);
       res.argStates(1).reg(7) := '0';
    end if;

    return res;
end function;


function updateReadyRegs(sch: SchedulerInfo; rrfSlice: std_logic_vector(0 to 2)) return SchedulerInfo is
    variable res: SchedulerInfo := sch;
begin
    for a in 0 to 2 loop
        res.dynamic.argStates(a).waiting := res.dynamic.argStates(a).waiting and not rrfSlice(a);
    end loop;
    return res;
end function;

function getIssueInfoArray(insVec: InstructionSlotArray; constant USE_IMM: boolean; ria: RenameInfoArray; rrf: std_logic_vector;
                                renamedDests: SmallNumberArray; renamedSources: SmallNumberArray; iqSel: IqSelector) return SchedulerInfoArray is
    variable res: SchedulerInfoArray(0 to PIPE_WIDTH-1);
    variable slot: InstructionSlot := DEFAULT_INS_SLOT;
    variable argInfo: RenameInfo := DEFAULT_RENAME_INFO;
    
    variable recoded: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
    variable mask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    variable ria_N: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
    variable rrfSlice: std_logic_vector(0 to 2) := (others => '0');

begin
    case iqSel is
        when I0 =>
            recoded := TMP_recodeAlu(insVec);
            mask := getAluMask1(insVec);
            ria_N := removeArg2(ria);
        when I1 =>
            recoded := TMP_recodeMul(insVec);
            mask := getMulMask1(insVec);
            ria_N := removeArg2(ria);
        when M0 =>
            recoded := insVec;
            mask := getMemMask1(insVec);
            ria_N := removeArg2(swapArgs12(ria));
        when SVI =>
            recoded := insVec;
            mask := getIntStoreMask1(insVec);
            ria_N := useStoreArg2(swapArgs12(ria));
        when SVF =>
            recoded := insVec;
            mask := getFloatStoreMask1(insVec);
            ria_N := useStoreArg2(swapArgs12(ria));
        when F0 =>
            recoded := TMP_recodeFP(insVec);
            mask := getFpMask1(insVec);
            ria_N := ria;
    end case;

    for i in res'range loop
        argInfo := ria_N(i);
        rrfSlice := rrf(3*i to 3*i + 2);
        slot := recoded(i);
        slot.full := mask(i);
        res(i).static := getIssueStaticInfo(slot, USE_IMM, argInfo);
        res(i).dynamic := getIssueDynamicInfo(slot, res(i).static, USE_IMM, argInfo, renamedDests(i), renamedSources(3*i to 3*i + 2), rrfSlice);
        
        --res(i) := updateReadyRegs(res(i), rrfSlice);
    end loop;

    return res;    
end function;


function updateOnDispatch(schedArray: SchedulerInfoArray; wakeups: WakeupStructArray2D; memFail: std_logic; config: SchedulerUpdateConfig)
return SchedulerInfoArray is
    variable res: SchedulerInfoArray(0 to schedArray'length-1);
begin
    res := updateSchedulerArray(schedArray, wakeups, memFail, config);
    res := restoreRenameIndex(res);
    return res;
end function;


function getNewLocs(fullMask: std_logic_vector; tags: SmallNumberArray; newArr: SchedulerInfoArray) return slv2D is
    constant QUEUE_SIZE_EXT: natural := fullMask'length;
    constant N_BANKS: natural := PIPE_WIDTH;
    constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
    variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
begin
    for b in 0 to N_BANKS-1 loop
        res(slv2u(tags(b)) * N_BANKS + b, b) := newArr(b).dynamic.full;
    end loop;
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

function incSrcStage(stage: SmallNumber) return SmallNumber is
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

function incReadyCounter(ctr: SmallNumber) return SmallNumber is
begin
    case ctr(1 downto 0) is
        when "00" =>
            return "00000001";            
        when others =>
            return "00000010";
    end case;
end function;


function dependsOnMemHit(state: ArgumentState; constant IS_FP: boolean) return std_logic is
    variable matchingCtr: SmallNumber := sn(1);
begin
    if IS_FP then
        matchingCtr := sn(0);
    end if;
    return bool2std(state.srcPipe(1 downto 0) = "10" and state.readyCtr = matchingCtr) and not state.zero_T and not state.waiting;
end function;


function getSlowWakeup(si: SchedulerInfo; a: natural; bypass: BypassState; fwModes: ForwardingModeArray) return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    constant arg: PhysName := si.dynamic.argStates(a).reg;
begin
    for p in 0 to bypass.used'length-1 loop
        if bypass.used(p) /= '1' then
            next;
        end if;

        if bypass.obj(p).dest(PHYS_REG_BITS-1 downto 0) = arg(PHYS_REG_BITS-1 downto 0) then
            res.match := '1';
            res.argLocsPipe(2 downto 0) := i2slv(p, 3);
            res.argSrc(1 downto 0) := i2slv(bypass.phase(p), 2);
            exit;
        end if;
    end loop;
    return res;
end function;


function getFastWakeup(si: SchedulerInfo; a: natural; bypass: BypassState; fwModes: ForwardingModeArray) return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    constant arg: PhysName := si.dynamic.argStates(a).reg;
    constant argTag: SmallNumber := si.dynamic.argStates(a).iqTag;
    variable matchVec: std_logic_vector(0 to 2) := (others => '0');
    variable srcStage: natural := 2;
begin
    for p in 0 to bypass.used'length-1 loop
        if bypass.usedFast(p) /= '1' then
            next;
        end if;

        -- NOTE: other method - comparing phys reg names, used for slow wakeup
        --if bypass.obj(p).dest(PHYS_REG_BITS-1 downto 0) = arg(PHYS_REG_BITS-1 downto 0) then

        if bypass.objTags(p)(4 downto 0) = argTag(4 downto 0) then
            res.match := '1';
            res.argLocsPipe(2 downto 0) := i2slv(p, 3);
            res.argSrc(1 downto 0) := i2slv(bypass.phase(p) - 1, 2);

            res.reg   := bypass.obj(p).dest;
            res.iqTag := bypass.objTags(p);
            exit;
        end if;
    end loop;

    return res;
end function;


function getInitWakeup(si: SchedulerInfo; a: natural; bypass: BypassState; fwModes: ForwardingModeArray) return WakeupStruct is
    variable res: WakeupStruct := DEFAULT_WAKEUP_STRUCT;
    constant arg: PhysName := si.dynamic.argStates(a).reg;
begin
    for p in 0 to bypass.used'length-1 loop
        if bypass.used(p) /= '1' then
            next;
        end if;

        if bypass.obj(p).dest(PHYS_REG_BITS-1 downto 0) = arg(PHYS_REG_BITS-1 downto 0) then
            res.match := '1';
            res.argLocsPipe(2 downto 0) := i2slv(p, 3);
            res.argSrc(1 downto 0) := i2slv(bypass.phase(p), 2);
            exit;
        elsif bypass.objNext(p).dest(PHYS_REG_BITS-1 downto 0) = arg(PHYS_REG_BITS-1 downto 0) then
            res.match := '1';
            res.argLocsPipe(2 downto 0) := i2slv(p, 3);
            res.argSrc(1 downto 0) := i2slv(bypass.phase(p) + 1, 2);
            if bypass.phase(p) + 1 > 2 then
                res.argSrc(1 downto 0) := "10";
            end if;
            exit;
        elsif bypass.objNext2(p).dest(PHYS_REG_BITS-1 downto 0) = arg(PHYS_REG_BITS-1 downto 0) then
            res.match := '1';
            res.argLocsPipe(2 downto 0) := i2slv(p, 3);
            res.argSrc(1 downto 0) := i2slv(bypass.phase(p) + 2, 2);
            if bypass.phase(p) + 2 > 2 then
                res.argSrc(1 downto 0) := "10";
            end if;
            exit;
        end if;

    end loop;

    return res;
end function;


function getSlowWakeups(content: SchedulerInfoArray; bypass: BypassState; config: SchedulerUpdateConfig) return WakeupStructArray2D is
    constant LEN: natural := content'length;
    variable res: WakeupStructArray2D(0 to LEN-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));
    variable fma: ForwardingMatchesArray(0 to LEN-1) := (others => DEFAULT_FORWARDING_MATCHES);
begin
    for i in 0 to LEN-1 loop	
        res(i, 0) := getSlowWakeup(content(i), 0, bypass, config.fwModes);
        res(i, 1) := getSlowWakeup(content(i), 1, bypass, config.fwModes);
    end loop;
    return res;
end function;


function getFastWakeups(content: SchedulerInfoArray; bypass: BypassState; config: SchedulerUpdateConfig) return WakeupStructArray2D is
	constant LEN: natural := content'length;
	variable res: WakeupStructArray2D(0 to LEN-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));
	variable fma: ForwardingMatchesArray(0 to LEN-1) := (others => DEFAULT_FORWARDING_MATCHES);
begin
	for i in 0 to LEN-1 loop
		res(i, 0) := getFastWakeup(content(i), 0, bypass, config.fwModes);
		res(i, 1) := getFastWakeup(content(i), 1, bypass, config.fwModes);
	end loop;
	return res;
end function;


function getInitWakeups(content: SchedulerInfoArray; bypass: BypassState; config: SchedulerUpdateConfig) return WakeupStructArray2D is
    constant LEN: natural := content'length;
    variable res: WakeupStructArray2D(0 to LEN-1, 0 to 1) := (others => (others => DEFAULT_WAKEUP_STRUCT));
    variable fma: ForwardingMatchesArray(0 to LEN-1) := (others => DEFAULT_FORWARDING_MATCHES);
begin
    for i in 0 to LEN-1 loop
        res(i, 0) := getInitWakeup(content(i), 0, bypass, config.fwModes);
        res(i, 1) := getInitWakeup(content(i), 1, bypass, config.fwModes);
    end loop;
    return res;
end function;


function updateWaitingArg(argState: ArgumentState; wakeups: WakeupStruct)
return ArgumentState is
    variable res: ArgumentState := argState;
begin
    if (argState.waiting and wakeups.match) /= '1' then
        return res;
    end if;

    res.srcPipe := wakeups.argLocsPipe;
    res.srcStage := wakeups.argSrc;
    res.waiting := '0';
    return res;
end function;

function retractArg(argState: ArgumentState) return ArgumentState is
    variable res: ArgumentState := argState;
begin
    res.waiting := '1';
    return res;
end function;

function updateSchedulerState_S(state: SchedulerInfo; wups: WakeupStructArray2D; k: natural)
return SchedulerInfo is
    variable res: SchedulerInfo := state;
begin
    for a in 0 to 1 loop
        res.dynamic.argStates(a) := updateWaitingArg(res.dynamic.argStates(a), wups(k, a));
    end loop;

    return res;
end function;


function updateSchedulerState(state: SchedulerInfo; wups: WakeupStructArray2D; k: natural; memFail: std_logic; config: SchedulerUpdateConfig)
return SchedulerInfo is
    variable res: SchedulerInfo := state;
begin
    for a in 0 to 1 loop
        res.dynamic.argStates(a).srcStage := incSrcStage(res.dynamic.argStates(a).srcStage);

        if res.dynamic.argStates(a).waiting = '1' then
            res.dynamic.argStates(a).readyCtr := sn(0);
        else
            res.dynamic.argStates(a).readyCtr := incReadyCounter(res.dynamic.argStates(a).readyCtr);
        end if;

        if memFail = '1' and not config.ignoreMemFail then
        -- Resetting to waiting state
            if dependsOnMemHit(state.dynamic.argStates(a), config.fp) = '1' then
                res.dynamic.argStates(a) := retractArg(res.dynamic.argStates(a));
            end if;
        else
        -- wakeup
            res.dynamic.argStates(a) := updateWaitingArg(res.dynamic.argStates(a), wups(k, a));
        end if;
    end loop;

    return res;
end function;

function updateSchedulerArray_S(schedArray: SchedulerInfoArray; wakeups: WakeupStructArray2D; memFail: std_logic; config: SchedulerUpdateConfig)
return SchedulerInfoArray is
    variable res: SchedulerInfoArray(0 to schedArray'length-1);
begin
    for i in schedArray'range loop
        res(i) := updateSchedulerState_S(schedArray(i), wakeups, i);
    end loop;    
    return res;
end function;

function updateSchedulerArray(schedArray: SchedulerInfoArray; wakeups: WakeupStructArray2D; memFail: std_logic; config: SchedulerUpdateConfig)
return SchedulerInfoArray is
    variable res: SchedulerInfoArray(0 to schedArray'length-1);
begin
    for i in schedArray'range loop
        res(i) := updateSchedulerState(schedArray(i), wakeups, i, memFail, config);
    end loop;    
    return res;
end function;


function hasDivOp(entry: SchedulerInfo) return std_logic is
begin
    return entry.dynamic.full and entry.static.divIns;
end function;


function removeEntry(entry: SchedulerInfo) return SchedulerInfo is
    variable res: SchedulerInfo := entry;
begin
    res.dynamic.full := '0';
 --   res.dynamic.status.state := empty;
    return res;
end function;

function pullbackEntry(entry: SchedulerInfo) return SchedulerInfo is
    variable res: SchedulerInfo := entry;
begin
--    if res.static.divIns = '1' then
--        res.dynamic.status.state := suspended;
--    else
--        res.dynamic.status.state := active;
--    end if;

    return res;
end function;

function updateIssuedEntry(entry: SchedulerInfo) return SchedulerInfo is
    variable res: SchedulerInfo := entry;
begin
    return res;
end function;

function issueEntry(entry: SchedulerInfo) return SchedulerInfo is
    variable res: SchedulerInfo := entry;
begin
  --  res.dynamic.status.state := issued;
    return res;
end function;


function suspendEntry(entry: SchedulerInfo) return SchedulerInfo is
    variable res: SchedulerInfo := entry;
begin
 --   res.dynamic.status.state := suspended;
    return res;
end function;

function resumeEntry(entry: SchedulerInfo) return SchedulerInfo is
    variable res: SchedulerInfo := entry;
begin
--    res.dynamic.status.active := '1';
--    res.dynamic.status.suspend := '0';
  --  res.dynamic.status.state := active;
    return res;
end function;


function updateQueueState(queueContent: SchedulerInfoArray; nextAccepting, sends: std_logic; killMask, trialMask, selMask: std_logic_vector; memFail, unlockDiv: std_logic)
return SchedulerInfoArray is
    constant LEN: natural := queueContent'length;
    variable res: SchedulerInfoArray(queueContent'range) := queueContent;
begin
    for i in 0 to LEN-1 loop
        res(i).dynamic.status.freed := '0'; -- This is set for 1 cycle when freeing
        -- Set age comparison for possible subsequent flush. This is done regardless of other parts of state      
        res(i).dynamic.status.trial := trialMask(i);

        if (nextAccepting and selMask(i)) = '1' then
            res(i).dynamic.status_N.active := '0';
            res(i).dynamic.status_N.issued0 := '1';
        end if;

        if queueContent(i).dynamic.status_N.issued0 = '1' then
            res(i).dynamic.status_N.issued0 := '0';

            if memFail = '1' then
                res(i).dynamic.status_N.active := not queueContent(i).static.divIns;
                res(i).dynamic.status_N.suspended := queueContent(i).static.divIns;
            else
                res(i).dynamic.status_N.issued1 := '1';
            end if;
        end if;

        if queueContent(i).dynamic.status_N.issued1 = '1' then
            res(i).dynamic.status_N.issued1 := '0';
            res(i).dynamic.status_N.issued2 := '1';
        end if;

        if queueContent(i).dynamic.status_N.issued2 = '1' then
            res(i).dynamic.status_N.issued2 := '0';

            res(i).dynamic.full := '0';
            res(i).dynamic.status.freed := '1';
        end if;

        if hasDivOp(queueContent(i)) = '1' then
            if (sends and not selMask(i)) = '1' then 
                res(i).dynamic.status_N.suspended := '1';
                res(i).dynamic.status_N.active := '0';
            elsif unlockDiv = '1' then
                res(i).dynamic.status_N.suspended := '0';
                res(i).dynamic.status_N.active := '1';
            end if;
        end if;

        if killMask(i) = '1' then
            res(i).dynamic.status_N := DEFAULT_ENTRY_STATUS_N;

            res(i).dynamic.full := '0';
        end if;

    end loop;

    return res;
end function;



function insertElements(content: SchedulerInfoArray; newArr: SchedulerInfoArray; insertionLocs: slv2D) return SchedulerInfoArray is
    constant LEN: natural := content'length;
    variable res: SchedulerInfoArray(content'range) := content;
    variable newElement: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        for k in 0 to LEN-1 loop
            if insertionLocs(k, i) = '1' then
                newElement := newArr(i);
                newElement.dynamic.status.trial := '1';
                newElement.dynamic.argStates(0).readyCtr := res(k).dynamic.argStates(0).readyCtr;
                newElement.dynamic.argStates(1).readyCtr := res(k).dynamic.argStates(1).readyCtr;

                res(k) := newElement;
                exit;
            end if;
        end loop;
    end loop;

    return res;
end function;


function getCurrentStates(queueContent: SchedulerInfoArray) return IqStateArray is
    variable res: IqStateArray(queueContent'range) := (others => empty);
begin
    for i in res'range loop
        if queueContent(i).dynamic.status_N.active = '1' then
            res(i) := active;
        elsif queueContent(i).dynamic.status_N.suspended = '1' then
            res(i) := suspended;
        elsif (queueContent(i).dynamic.status_N.issued0 or queueContent(i).dynamic.status_N.issued1 or queueContent(i).dynamic.status_N.issued2) = '1' then
            res(i) := issued;
        else
            res(i) := empty;
        end if;    
    end loop;
    return res;
end function;


-- DB
procedure DB_reportEvents(content: SchedulerInfoArray; lastEvents: IqEventArray) is
    use work.CpuText.all;
    constant DB_TRACKED_STR: string(1 to 8) := slv2hex(DB_TRACKED_SEQ_NUM);
begin
    -- pragma synthesis off
    for i in 0 to content'length-1 loop
        if DB_OP_TRACKING and content(i).static.dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
            if lastEvents(i) /= none then
                report "DEBUG: IQ Tracked seqNum " & DB_TRACKED_STR & "; " & IqEvent'image(lastEvents(i));
            end if;

            return;
        end if;                
    end loop;
    -- pragma synthesis on
end procedure;

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


-- TODO: DB?
function handleIqDbInfo(queueContent: SchedulerInfoArray) return SchedulerInfoArray is
    constant LEN: natural := queueContent'length;
    variable res: SchedulerInfoArray(queueContent'range) := queueContent;    
begin
    for i in 0 to LEN-1 loop
        if res(i).dynamic.full /= '1' then
            res(i).static.dbInfo := DEFAULT_DEBUG_INFO;
            res(i).dynamic.argStates(0).dbDep := DEFAULT_DB_DEPENDENCY;
            res(i).dynamic.argStates(1).dbDep := DEFAULT_DB_DEPENDENCY;
            res(i).dynamic.argStates(2).dbDep := DEFAULT_DB_DEPENDENCY;
        elsif (res(i).dynamic.status_N.issued0 or res(i).dynamic.status_N.issued1 or res(i).dynamic.status_N.issued2) /= '1' then
            -- pragma synthesis off
            for j in 0 to 2 loop
                if res(i).dynamic.argStates(j).waiting = '1' then
                    res(i).dynamic.argStates(j).dbDep.cyclesWaiting := res(i).dynamic.argStates(j).dbDep.cyclesWaiting + 1;
                else
                    res(i).dynamic.argStates(j).dbDep.cyclesReady := res(i).dynamic.argStates(j).dbDep.cyclesReady + 1;
                end if;
            end loop;
            -- pragma synthesis on
        end if;
    end loop;        
    return res;
end function;

-- Insert new elements, update db info
function storeInput(queueContent: SchedulerInfoArray; inputData: SchedulerInfoArray; prevSending, evt: std_logic; insertionLocs: slv2D)
return SchedulerInfoArray is
    variable res: SchedulerInfoArray(queueContent'range) := queueContent;
begin
    if (prevSending and not evt) = '1' then
        res := insertElements(res, inputData, insertionLocs);
    end if;

    res := handleIqDbInfo(res);
    return res;
end function;


function updateAgeMatrix(ageMatrix, insertionLocs: slv2D; fullMask: std_logic_vector) return slv2D is
    constant QUEUE_SIZE_EXT: natural := fullMask'length;
    variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := ageMatrix;
begin
    for i in 0 to PIPE_WIDTH-1 loop
        -- insert current fullMask into proper row
        for j in 0 to QUEUE_SIZE_EXT-1 loop
            if insertionLocs(j, i) = '1' then
                -- Sorry, assigment doesn't work for ranges in >= 2D
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


-- issue
function getSchedEntrySlot(info: SchedulerInfo; full: std_logic; iqTag: SmallNumber) return SchedulerState is
    variable res: SchedulerState := DEFAULT_SCHED_STATE;
begin
    res.full := full;
    res.maybeFull := full;

    res.st := info.static;

    res.intDestSel := info.dynamic.intDestSel;
    res.floatDestSel := info.dynamic.floatDestSel;
    res.dest := info.dynamic.dest;
    res.destTag := iqTag;

    for k in 0 to 2 loop
        res.args(k) := info.dynamic.argStates(k).reg;
        res.argLocsPipe(k) := info.dynamic.argStates(k).srcPipe;
        res.argSrc(k) := info.dynamic.argStates(k).srcStage;
    end loop;

    if IMM_AS_REG then
        res.st.immValue(PhysName'length-2 downto 0) := res.args(1)(6 downto 0);
    end if;

    res.readNew := (others => '0');

    return res;
end function;


function getTrialMask(content: SchedulerInfoArray; events: EventState) return std_logic_vector is
    variable res: std_logic_vector(content'range) := (others => '0');        
begin
    for i in res'range loop
        res(i) := compareTagBefore(events.preExecTags.renameIndex, content(i).dynamic.renameIndex);
    end loop;
    return res;
end function;

function getReadyMask(content: SchedulerInfoArray) return std_logic_vector is
    variable res: std_logic_vector(content'range) := (others => '0');        
begin
    for i in res'range loop
        res(i) := content(i).dynamic.--status.active
                                      status_N.active
                    and not content(i).dynamic.argStates(0).waiting and not content(i).dynamic.argStates(1).waiting;
    end loop;
    return res;
end function;

function getFullMask(content: SchedulerInfoArray) return std_logic_vector is
    variable res: std_logic_vector(content'range) := (others => '0');        
begin
    for i in res'range loop
        res(i) := content(i).dynamic.full;
    end loop;
    return res;
end function;

function getFreedMask(content: SchedulerInfoArray) return std_logic_vector is
    variable res: std_logic_vector(content'range) := (others => '0');        
begin
    for i in res'range loop
        res(i) := content(i).dynamic.status.freed;
    end loop;
    return res;
end function;

function getTrialUpdatedMask(content: SchedulerInfoArray) return std_logic_vector is
    variable res: std_logic_vector(content'range) := (others => '0');        
begin
    for i in res'range loop
        res(i) := content(i).dynamic.status.trial;
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
    res.static.operation := TMP_restoreOperation(res.static.operation);

    res.static.branchIns := a.static.branchIns or b.static.branchIns;
    res.static.divIns := a.static.divIns or b.static.divIns;

    res.static.tags.renameIndex := a.static.tags.renameIndex or b.static.tags.renameIndex;
    res.static.tags.intPointer := a.static.tags.intPointer or b.static.tags.intPointer;
    res.static.tags.floatPointer := a.static.tags.floatPointer or b.static.tags.floatPointer;
    res.static.tags.bqPointer := a.static.tags.bqPointer or b.static.tags.bqPointer;
    res.static.tags.sqPointer := a.static.tags.sqPointer or b.static.tags.sqPointer;
    res.static.tags.lqPointer := a.static.tags.lqPointer or b.static.tags.lqPointer;   
    res.static.tags.bqPointerSeq := a.static.tags.bqPointerSeq or b.static.tags.bqPointerSeq;

    res.static.immediate :=  a.static.immediate or b.static.immediate;
    res.static.immValue :=  a.static.immValue or b.static.immValue;
    res.static.zero :=  a.static.zero or b.static.zero;


    res.dynamic.full := a.dynamic.full or b.dynamic.full;

    res.dynamic.status.freed := a.dynamic.status.freed or b.dynamic.status.freed;
    res.dynamic.status.trial := a.dynamic.status.trial or b.dynamic.status.trial;
    res.dynamic.status := DEFAULT_ENTRY_STATUS;
    res.dynamic.status_N := DEFAULT_ENTRY_STATUS_N;

    res.dynamic.renameIndex := a.dynamic.renameIndex or b.dynamic.renameIndex;

    res.dynamic.dest := a.dynamic.dest or b.dynamic.dest;
    res.dynamic.intDestSel := a.dynamic.intDestSel or b.dynamic.intDestSel;
    res.dynamic.floatDestSel := a.dynamic.floatDestSel or b.dynamic.floatDestSel;

    for i in 0 to 2 loop
        --res.dynamic.argStates(i).used_T := a.dynamic.argStates(i).used_T or b.dynamic.argStates(i).used_T;
        res.dynamic.argStates(i).reg := a.dynamic.argStates(i).reg or b.dynamic.argStates(i).reg;
        res.dynamic.argStates(i).iqTag := a.dynamic.argStates(i).iqTag or b.dynamic.argStates(i).iqTag;
        res.dynamic.argStates(i).zero_T := a.dynamic.argStates(i).zero_T or b.dynamic.argStates(i).zero_T;
        --res.dynamic.argStates(i).imm_T := a.dynamic.argStates(i).imm_T or b.dynamic.argStates(i).imm_T;
        res.dynamic.argStates(i).value := a.dynamic.argStates(i).value or b.dynamic.argStates(i).value;

        res.dynamic.argStates(i).readyCtr := a.dynamic.argStates(i).readyCtr or b.dynamic.argStates(i).readyCtr;
        res.dynamic.argStates(i).waiting := a.dynamic.argStates(i).waiting or b.dynamic.argStates(i).waiting;

        res.dynamic.argStates(i).srcPipe := a.dynamic.argStates(i).srcPipe or b.dynamic.argStates(i).srcPipe;
        res.dynamic.argStates(i).srcStage := a.dynamic.argStates(i).srcStage or b.dynamic.argStates(i).srcStage;

        res.dynamic.argStates(i).dbDep := DEFAULT_DB_DEPENDENCY;
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

    function getAgeMatrixH(ageMatrix: slv2D) return slv2D is
        constant QUEUE_SIZE_EXT: natural := ageMatrix'length;
        variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            for j in 0 to i-1 loop
                res(i, j) := not ageMatrix(j, i);
            end loop;

            for j in i+1 to QUEUE_SIZE_EXT-1 loop
                res(i, j) := ageMatrix(i, j);
            end loop;
        end loop;

        return res;
    end function;


    function getSelMask_H(readyMask: std_logic_vector; ageMatrix: slv2D) return std_logic_vector is
        constant QUEUE_SIZE_EXT: natural := readyMask'length;
        variable res: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        variable row: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        variable ageMatrixH: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := getAgeMatrixH(ageMatrix);
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            for j in 0 to QUEUE_SIZE_EXT-1 loop
                row(j) := readyMask(j) and ageMatrixH(i, j);
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

function selectDbInfo(inputElems: SchedulerInfoArray; selMask: std_logic_vector) return InstructionDebugInfo is
    constant QUEUE_SIZE_EXT: natural := inputElems'length;
    variable res: InstructionDebugInfo := DEFAULT_DEBUG_INFO;
    variable maskedQueue: SchedulerInfoArray(inputElems'range) := (others => DEFAULT_SCHEDULER_INFO);
begin
    -- pragma synthesis off
    for i in 0 to QUEUE_SIZE_EXT-1 loop
        if selMask(i) = '1' then
            return inputElems(i).static.dbInfo;
        end if;
    end loop;
    -- pragma synthesis on

    return DEFAULT_DEBUG_INFO;
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
    res.static.dbInfo := selectDbInfo(inputElems, selMask);
    return res;
end function;

    function getEffectiveStopMatrix(readyMask: std_logic_vector; ageMatrix: slv2D) return slv2D is
        constant QUEUE_SIZE_EXT: natural := readyMask'length;
        variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := ageMatrix;
    begin
        for what in 0 to QUEUE_SIZE_EXT-1 loop
            for why in 0 to QUEUE_SIZE_EXT-1 loop
                if what = why then
                    next;
                end if;
                res(what, why) := ageMatrix(what, why) and readyMask(why);
            end loop;
        end loop;
        
        return res;
    end function;

    type Span is record
        start, past: natural;
    end record;

    function stopByRange(ind: natural; byRange: Span; readyMask: std_logic_vector; stopMatrix: slv2D) return std_logic is
    begin
        for by in byRange.start to byRange.past-1 loop
            if (not readyMask(ind) or stopMatrix(ind, by)) = '1' then
                return '1';
            end if;
        end loop;
        return '0';
    end function;

    function stopRangeByRange(whichRange: Span; byRange: Span; readyMask: std_logic_vector; stopMatrix: slv2D) return std_logic is
        variable res, stopped: std_logic := '1';
    begin
        for which in whichRange.start to whichRange.past-1 loop
            stopped := stopByRange(which, byRange, readyMask, stopMatrix);
            if not stopped = '1' then
                return '0';
            end if;
        end loop;
        return '1';
    end function;

    function selectFrom(first, second: SchedulerInfo; stopFirst: std_logic) return SchedulerInfo is
    begin
        if stopFirst = '1' then
            return second;
        else
            return first;
        end if;
    end function;



    function queueSelect_N(inputElems: SchedulerInfoArray; readyMask: std_logic_vector; ageMatrix: slv2D) return SchedulerInfo is
        constant QUEUE_SIZE_EXT: natural := inputElems'length;
        variable res, sel01, sel23, sel0123: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
        variable selMask: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := getSelMask(readyMask, ageMatrix);
        variable stopMatrix: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := getEffectiveStopMatrix(readyMask, ageMatrix);

        variable stop0, stop2: std_logic := '0';
        variable stop01, stopPivot: std_logic := '0';

        variable level2, level4, level8, level16: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
        variable pivot: natural := 0;
    begin
        -- Build level 2
        for i in 0 to 5 loop
            pivot := 2*i;
            stopPivot := stopRangeByRange((pivot, pivot+1), (pivot+1, pivot+2), readyMask, stopMatrix);
            level2(pivot) := selectFrom(inputElems(pivot), inputElems(pivot+1), stopPivot);
        end loop;

        for i in 0 to 2 loop
            pivot := 4*i;
            stopPivot := stopRangeByRange((pivot, pivot+2), (pivot+2, pivot+4), readyMask, stopMatrix);
            level4(pivot) := selectFrom(level2(pivot), level2(pivot+2), stopPivot);
        end loop;

            pivot := 0;
            stopPivot := stopRangeByRange((pivot, pivot+4), (pivot+4, pivot+8), readyMask, stopMatrix);
            level8(pivot) := selectFrom(level4(pivot), level4(pivot+4), stopPivot);

            level8(pivot+8) := level4(pivot+8);


            pivot := 0;
            stopPivot := stopRangeByRange((pivot, pivot+8), (pivot+8, pivot+12), readyMask, stopMatrix);
            level16(pivot) := selectFrom(level8(pivot), level8(pivot+8), stopPivot);

        res := level16(0);

        res.dynamic.status := DEFAULT_ENTRY_STATUS;

        res.dynamic.argStates(0).dbDep := DEFAULT_DB_DEPENDENCY;
        res.dynamic.argStates(1).dbDep := DEFAULT_DB_DEPENDENCY;
        res.dynamic.argStates(2).dbDep := DEFAULT_DB_DEPENDENCY;

        res.static.operation := TMP_restoreOperation(res.static.operation);

        res.static.dbInfo := selectDbInfo(inputElems, selMask);

        return res;
    end function;

    function TMP_getTagLowPart(selMask: std_logic_vector) return SmallNumber is
        variable res: SmallNumber := sn(-1);
    begin
        for i in selMask'range loop
            if selMask(i) = '1' then
                res := sn(i);
            end if;
        end loop;
        res(7 downto 4) := (others => '0');
        return res;
    end function;

    function getIssueTag(sends: std_logic; selMask: std_logic_vector) return SmallNumber is
        variable res: SmallNumber := TMP_getTagLowPart(selMask);
    begin
        res(4) := sends;
        return res;
    end function;

    function TMP_slowSelect(content: SchedulerInfoArray; tag: SmallNumber) return SchedulerInfo is
        constant LEN: natural := content'length;
        variable ind: natural := slv2u(tag(3 downto 0));
    begin
        if ind > LEN-1 then
            return content(LEN-1);
        else
            return content(ind);
        end if; 
    end function;

    function TMP_slowSelect(content: std_logic_vector; tag: SmallNumber) return std_logic is
        constant LEN: natural := content'length;
        variable ind: natural := slv2u(tag(3 downto 0));
    begin
        if ind > LEN-1 then
            return content(LEN-1);
        else
            return content(ind);
        end if; 
    end function;


-- wups experimental
    function getWakeup(argState: ArgumentState; fni: ForwardingInfo; constant MODES: WakeupSpec; constant MODE_IND: natural) return ArgWakeup is
        variable res: ArgWakeup;
        variable mode: WakeupMode := NONE;
        constant N_SRCS: natural := MODES'length(2);
        variable matched, matchedM3, matchedM2, matchedM1: std_logic := '0';
        variable iqTagFull, iqTagFullM2, iqTagFullM1: SmallNumber := sn(0);
    begin
        for i in 0 to N_SRCS-1 loop
            mode := MODES(MODE_IND, i);
            
            case mode is
                when FAST =>
                    iqTagFull := fni.iqTagsM2(i);
                    iqTagFull := iqTagFull or sn(16*(1+i));
                    matched := bool2std(argState.iqTag = iqTagFull);
                    if (matched and argState.waiting) = '1' then
                        res.active := '1';
                        res.mode := FAST;
                        res.iqTag := iqTagFull;
                        res.match := '1';
                        res.pipe := sn(i);
                        res.stage := X"00";
                    end if;
                    
                when SLOW =>
                    matched := bool2std(argState.reg = fni.nextTagsM3(i));
                    if (matched and argState.waiting) = '1' then
                        res.active := '1';
                        res.mode := SLOW;
                        res.iqTag := fni.iqTagsM3(i);
                        res.match := '1';
                        res.pipe := sn(i);
                        res.stage := X"03";
                    end if;
                    
                when REG =>
                    matched := bool2std(argState.reg = fni.tags0(i));
                    if (matched and argState.waiting) = '1' then
                        res.active := '1';
                        res.mode := REG;
                        res.iqTag := fni.iqTags0(i);
                        res.match := '1';
                        res.pipe := sn(i);
                        res.stage := X"02";
                    end if;
                    
                when INIT_FAST => ------------------------------------------------
                    iqTagFullM2 := fni.iqTagsM2(i);
                    iqTagFullM2 := iqTagFull or sn(16*(1+i));
                    matchedM2 := bool2std(argState.iqTag = iqTagFullM2);
                    iqTagFullM1 := fni.iqTagsM1(i);
                    iqTagFullM1 := iqTagFullM1 or sn(16*(1+i));
                    matchedM1 := bool2std(argState.iqTag = iqTagFullM1);
                    matched := matchedM2 or matchedM1;
                    if (matched and argState.waiting) = '1' then
                        res.active := '1';
                        res.mode := INIT_FAST;
                        res.match := '1';
                        res.pipe := sn(i);
                        
                        if matchedM1 = '1' then
                            res.iqTag := iqTagFullM1;
                            res.stage := X"01";
                        else
                            res.iqTag := iqTagFullM2;
                            res.stage := X"00";
                        end if;
                    end if;

                when INIT_SLOW => -----------------------------------------------
                    matchedM3 := bool2std(argState.reg = fni.nextTagsM3(i));
                    matchedM2 := bool2std(argState.reg = fni.nextTagsM2(i));
                    matchedM1 := bool2std(argState.reg = fni.nextTagsM1(i));
                    matched := matchedM3 or matchedM2 or matchedM1; 
                    if (matched and argState.waiting) = '1' then
                        res.active := '1';
                        res.mode := INIT_SLOW;
                        res.match := '1';
                        res.pipe := sn(i);
                        
                        if matchedM1 = '1' then
                            res.iqTag := fni.iqTagsM1(i);
                            res.stage := X"01";
                        elsif matchedM2 = '1' then
                            res.iqTag := fni.iqTagsM2(i);
                            res.stage := X"00";                      
                        else
                            res.iqTag := fni.iqTagsM3(i);
                            res.stage := X"03";
                        end if;
                    end if;

                when INIT_REG => ------------------------------------------------
                    matched := bool2std(argState.reg = fni.tags0(i) or argState.reg = fni.tags0(i));
                    if (matched and argState.waiting) = '1' then
                        res.active := '1';
                        res.mode := INIT_REG;
                        res.iqTag := fni.iqTags0(i);
                        res.match := '1';
                        res.pipe := sn(i);
                        res.stage := X"02";
                    end if;                        

                when CONST =>
                    res.mode := CONST;

                when others =>
            end case;

        end loop;
        return res;
    end function;

    function getWakeupArray(content: SchedulerInfoArray; fni: ForwardingInfo; constant WAKEUP_SPEC: WakeupSpec; constant CFG: SchedulerUpdateConfig) return WakeupInfoArray is
        constant LEN: natural := content'length; 
        variable res: WakeupInfoArray(content'range);
    begin

        for i in 0 to LEN-1 loop
            res(i).arg0 := getWakeup(content(i).dynamic.argStates(0), fni, WAKEUP_SPEC, 0);
            res(i).arg1 := getWakeup(content(i).dynamic.argStates(1), fni, WAKEUP_SPEC, 1);
            res(i).active := res(i).arg0.active or res(i).arg1.active;
        end loop;
        
        return res;
    end function;

-----------------------------------

end LogicIssue;
