----------------------------------------------------------------------------------

----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;


package LogicROB is

constant PTR_MASK_TAG: InsTag := i2slv(ROB_SIZE-1, TAG_SIZE);
constant PTR_MASK_SN: SmallNumber := i2slv(ROB_SIZE-1, SMALL_NUMBER_SIZE);
constant ROB_PTR_SIZE: natural := countOnes(PTR_MASK_SN);	
    
type StaticGroupInfo is record
    specialOp: std_logic_vector(3 downto 0); -- TMP
    useBQ:      std_logic; -- Maybe only here?
end record;

type StaticOpInfo is record
    virtualIntDestSel:     std_logic;
    virtualFloatDestSel:   std_logic;
    virtualDest:    RegName;    
    physicalDest:   PhysName;

    useSQ:      std_logic;
    useLQ:      std_logic;
    useBQ:      std_logic;  -- May be better a Group param?
end record;

constant DEFAULT_STATIC_GROUP_INFO: StaticGroupInfo := (
    specialOp => (others => '0'),
    others => '0'
);

constant DEFAULT_STATIC_OP_INFO: StaticOpInfo := (
    virtualDest => (others => '0'),
    physicalDest => (others => '0'),
    others => '0'
);

type DynamicGroupInfo is record
    full: std_logic;
end record;

type DynamicOpInfo is record
    full:       std_logic;
    killed:     std_logic;
    causing:    std_logic;
    completed0: std_logic;
    completed1: std_logic;
    
    hasEvent:     std_logic;
    hasException: std_logic;
    confirmedBranch: std_logic;
    specialAction: std_logic;
    refetch: std_logic;
end record;

constant DEFAULT_DYNAMIC_GROUP_INFO: DynamicGroupInfo := (
    others => '0'
);

constant DEFAULT_DYNAMIC_OP_INFO: DynamicOpInfo := (
    others => '0'
);

type StaticOpInfoArray is array(0 to PIPE_WIDTH-1) of StaticOpInfo;
type StaticOpInfoArray2D is array(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) of StaticOpInfo;

type StaticGroupInfoArray is array(0 to ROB_SIZE-1) of StaticGroupInfo;

type DynamicOpInfoArray is array(0 to PIPE_WIDTH-1) of DynamicOpInfo;
type DynamicOpInfoArray2D is array(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) of DynamicOpInfo;

type DynamicGroupInfoArray is array(0 to ROB_SIZE-1) of DynamicGroupInfo;


function getStaticGroupInfo(isa: InstructionSlotArray; ssl: InstructionSlot) return StaticGroupInfo;
function getDynamicGroupInfo(isa: InstructionSlotArray; ssl: InstructionSlot) return DynamicGroupInfo;
function getStaticOpInfo(isl: InstructionSlot) return StaticOpInfo;
function getStaticOpInfoA(isa: InstructionSlotArray) return StaticOpInfoArray;
function getDynamicOpInfo(isl: InstructionSlot) return DynamicOpInfo;
function getDynamicOpInfoA(isa: InstructionSlotArray) return DynamicOpInfoArray;

    function getOutputSlot_T(stat: StaticOpInfo; dyn: DynamicOpInfo) return InstructionSlot;   
    function getInstructionSlotArray_T(sa: StaticOpInfoArray; da: DynamicOpInfoArray; sgi: StaticGroupInfo; dgi: DynamicGroupInfo) return InstructionSlotArray;
    function getSpecialSlot_T(si: StaticGroupInfo; di: DynamicGroupInfo) return InstructionSlot;

function serializeOp(isl: InstructionSlot) return std_logic_vector;
function deserializeOp(isl: InstructionSlot; serialData: std_logic_vector) return InstructionSlotArray; 
function serializeSpecialAction(isl: InstructionSlot) return std_logic_vector;
function deserializeSpecialAction(isl: InstructionSlot; serialData: std_logic_vector) return InstructionSlotArray;    
function serializeOpGroup(insVec: InstructionSlotArray) return std_logic_vector;
function deserializeOpGroup(insVec: InstructionSlotArray; serialData: std_logic_vector) return InstructionSlotArray;


procedure writeStaticInput(signal content: inout StaticOpInfoArray2D; input: StaticOpInfoArray; ptr: SmallNumber);
procedure writeDynamicInput(signal content: inout DynamicOpInfoArray2D; input: DynamicOpInfoArray; ptr: SmallNumber);
procedure writeStaticGroupInput(signal content: inout StaticGroupInfoArray; input: StaticGroupInfo; ptr: SmallNumber);
procedure writeDynamicGroupInput(signal content: inout DynamicGroupInfoArray; input: DynamicGroupInfo; ptr: SmallNumber);

function readStaticOutput(content: StaticOpInfoArray2D; ptr: SmallNumber) return StaticOpInfoArray;
function readDynamicOutput(content: DynamicOpInfoArray2D; ptr: SmallNumber) return DynamicOpInfoArray;
function readStaticGroupOutput(content: StaticGroupInfoArray; ptr: SmallNumber) return StaticGroupInfo;
function readDynamicGroupOutput(content: DynamicGroupInfoArray; ptr: SmallNumber) return DynamicGroupInfo;

procedure updateDynamicContent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlotArray; constant CLUSTER: natural);
procedure updateDynamicGroupBranch(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot; ind: natural);
procedure updateDynamicContentBranch(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot);
procedure updateDynamicGroupMemEvent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot; ind: natural);
procedure updateDynamicContentMemEvent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot);

function groupCompleted(insVec: InstructionSlotArray) return std_logic;
   
end package;



package body LogicROB is

function getStaticGroupInfo(isa: InstructionSlotArray; ssl: InstructionSlot) return StaticGroupInfo is
    variable res: StaticGroupInfo;
begin
    res.specialOp := sop(None, ssl.ins.specificOperation.system).bits;       
    res.useBQ := isa(0).ins.controlInfo.firstBr;
    return res;
end function;
    

function getDynamicGroupInfo(isa: InstructionSlotArray; ssl: InstructionSlot) return DynamicGroupInfo is
    variable res: DynamicGroupInfo;
begin
    res.full := '0';                
    return res;
end function;



function getStaticOpInfo(isl: InstructionSlot) return StaticOpInfo is
    variable res: StaticOpInfo;
begin
    res.virtualIntDestSel := isl.ins.virtualArgSpec.intDestSel;
    res.virtualFloatDestSel := isl.ins.virtualArgSpec.floatDestSel;     
    res.virtualDest := isl.ins.virtualArgSpec.dest(4 downto 0);    
    res.physicalDest := isl.ins.physicalArgSpec.dest;

    res.useSQ := isl.ins.classInfo.secCluster; -- ??
    res.useLQ := isl.ins.classInfo.useLQ;
    res.useBQ := isl.ins.classInfo.branchIns;
    
    return res;
end function;

function getStaticOpInfoA(isa: InstructionSlotArray) return StaticOpInfoArray is
    variable res: StaticOpInfoArray;
begin
    for i in isa'range loop
        res(i) := getStaticOpInfo(isa(i));
    end loop;        
    return res;
end function;


function getDynamicOpInfo(isl: InstructionSlot) return DynamicOpInfo is
    variable res: DynamicOpInfo;
begin
    res.full := isl.full;
    res.killed := '0';
    res.causing := '0';
    res.completed0 := not isl.ins.classInfo.mainCluster; 
    res.completed1 := not isl.ins.classInfo.secCluster;
    
    res.hasEvent := '0';
    res.hasException := '0';
    res.confirmedBranch := isl.ins.controlInfo.confirmedBranch;
    res.specialAction := isl.ins.controlInfo.specialAction; -- ???
    res.refetch := '0';
    return res;
end function;

function getDynamicOpInfoA(isa: InstructionSlotArray) return DynamicOpInfoArray is
    variable res: DynamicOpInfoArray;
begin
    for i in isa'range loop
        res(i) := getDynamicOpInfo(isa(i));
    end loop;       
    return res;
end function;

    function getOutputSlot_T(stat: StaticOpInfo; dyn: DynamicOpInfo) return InstructionSlot is
        variable res: InstructionSlot := DEFAULT_INS_SLOT;
    begin
        res.full := dyn.full;
        res.ins.controlInfo.killed := dyn.killed;
        res.ins.controlInfo.causing := dyn.causing;
        res.ins.controlInfo.completed := dyn.completed0; 
        res.ins.controlInfo.completed2 := dyn.completed1;
        
        res.ins.controlInfo.newEvent := dyn.hasEvent;
        res.ins.controlInfo.hasException := dyn.hasException;
        res.ins.controlInfo.confirmedBranch := dyn.confirmedBranch;
        res.ins.controlInfo.specialAction := dyn.specialAction; -- ???
        res.ins.controlInfo.refetch := dyn.refetch; --isl.ins.controlInfo.refetch;
        

        res.ins.virtualArgSpec.intDestSel := stat.virtualIntDestSel;
        res.ins.virtualArgSpec.floatDestSel := stat.virtualFloatDestSel;
 
        res.ins.virtualArgSpec.dest(4 downto 0) := stat.virtualDest;    
        res.ins.physicalArgSpec.dest := stat.physicalDest;

        res.ins.classInfo.secCluster := stat.useSQ;
        res.ins.classInfo.useLQ := stat.useLQ;
        res.ins.classInfo.branchIns := stat.useBQ;
                   
        return res;
    end function;   

    function getInstructionSlotArray_T(sa: StaticOpInfoArray; da: DynamicOpInfoArray; sgi: StaticGroupInfo; dgi: DynamicGroupInfo) return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
    begin
        for i in res'range loop
            res(i) := getOutputSlot_T(sa(i), da(i));
        end loop; 
        
        res(0).ins.controlInfo.firstBr := sgi.useBQ;
        
        return res;
    end function;


    function getSpecialSlot_T(si: StaticGroupInfo; di: DynamicGroupInfo) return InstructionSlot is
        variable res: InstructionSlot := DEFAULT_INS_SLOT;
    begin
        res.ins.specificOperation.subpipe := None;
        res.ins.specificOperation.system := SysOp'val(slv2u(si.specialOp));
        res.ins.specificOperation.bits := si.specialOp;
        
        --res.useBQ := isa(0).ins.controlInfo.firstBr;
        return res;
    end function;


----------------------------------------------------------
function serializeOp(isl: InstructionSlot) return std_logic_vector is
begin

end function;

function deserializeOp(isl: InstructionSlot; serialData: std_logic_vector) return InstructionSlotArray is
begin

end function; 


function serializeSpecialAction(isl: InstructionSlot) return std_logic_vector is
begin

end function;

function deserializeSpecialAction(isl: InstructionSlot; serialData: std_logic_vector) return InstructionSlotArray is
begin

end function;    

function serializeOpGroup(insVec: InstructionSlotArray) return std_logic_vector is
begin

end function;

function deserializeOpGroup(insVec: InstructionSlotArray; serialData: std_logic_vector) return InstructionSlotArray is
begin

end function;


procedure writeStaticInput(signal content: inout StaticOpInfoArray2D; input: StaticOpInfoArray; ptr: SmallNumber) is
begin
    for i in input'range loop
        content(slv2u(ptr), i) <= input(i);
    end loop;
end procedure;

procedure writeDynamicInput(signal content: inout DynamicOpInfoArray2D; input: DynamicOpInfoArray; ptr: SmallNumber) is
begin
    for i in input'range loop
        content(slv2u(ptr), i) <= input(i);
    end loop;
end procedure;

procedure writeStaticGroupInput(signal content: inout StaticGroupInfoArray; input: StaticGroupInfo; ptr: SmallNumber) is
begin
    content(slv2u(ptr)) <= input;
end procedure;

procedure writeDynamicGroupInput(signal content: inout DynamicGroupInfoArray; input: DynamicGroupInfo; ptr: SmallNumber) is
begin
    content(slv2u(ptr)) <= input;
end procedure;


function readStaticOutput(content: StaticOpInfoArray2D; ptr: SmallNumber) return StaticOpInfoArray is
    variable res: StaticOpInfoArray;
begin
    for i in res'range loop
        res(i):= content(slv2u(ptr), i);
    end loop;
    return res;
end function;

function readDynamicOutput(content: DynamicOpInfoArray2D; ptr: SmallNumber) return DynamicOpInfoArray is
    variable res: DynamicOpInfoArray;
begin
    for i in res'range loop
        res(i):= content(slv2u(ptr), i);
    end loop;
    return res;
end function;

function readStaticGroupOutput(content: StaticGroupInfoArray; ptr: SmallNumber) return StaticGroupInfo is
begin
    return content(slv2u(ptr));
end function;

function readDynamicGroupOutput(content: DynamicGroupInfoArray; ptr: SmallNumber) return DynamicGroupInfo is
begin
    return content(slv2u(ptr));
end function;


procedure updateDynamicContent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlotArray; constant CLUSTER: natural) is
    variable groupInd, opInd: natural;
    variable tagHigh,tagHighTrunc: SmallNumber;
begin
    for i in execInfo'range loop
        tagHigh := getTagHighSN(execInfo(i).ins.tags.renameIndex);
        tagHighTrunc := tagHigh and PTR_MASK_SN;
        groupInd := slv2u(tagHighTrunc);
        opInd := slv2u(getTagLow(execInfo(i).ins.tags.renameIndex));
    
        if execInfo(i).full = '1' then
            if CLUSTER = 0 then
                content(groupInd, opInd).completed0 <= '1';
            else--elsif cluster = 1 then
                content(groupInd, opInd).completed1 <= '1';                   
            end if;
        end if;
    end loop;
    
end procedure;

procedure updateDynamicGroupBranch(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot; ind: natural) is
    variable groupInd, opInd: natural;
    variable tagHigh,tagHighTrunc: SmallNumber;
    variable eventFound: boolean := false;
begin
    tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
    tagHighTrunc := tagHigh and PTR_MASK_SN;
    groupInd := slv2u(tagHighTrunc);
    opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));

    for i in 0 to PIPE_WIDTH-1 loop
        if eventFound then
            content(groupInd, i).full <= '0';
            content(groupInd, i).killed <= '1';              
        elsif opInd = i then
            if execInfo.ins.controlInfo.confirmedBranch = '1' then
                content(groupInd, i).confirmedBranch <= '1';                    
            end if;                
        
            if execInfo.ins.controlInfo.newEvent = '1' then
                content(groupInd, i).causing <= '1';                        
                eventFound:= true;
            end if;
        end if;
    end loop;
end procedure;
 
procedure updateDynamicContentBranch(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot) is
    variable groupInd, opInd: natural;
    variable tagHigh,tagHighTrunc: SmallNumber;
begin
    tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
    tagHighTrunc := tagHigh and PTR_MASK_SN;
    groupInd := slv2u(tagHighTrunc);
    --opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));
    if execInfo.full = '1' then
        updateDynamicGroupBranch(content, execInfo, groupInd);
    end if;         
end procedure;         

procedure updateDynamicGroupMemEvent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot; ind: natural) is
    variable groupInd, opInd: natural;
    variable tagHigh,tagHighTrunc: SmallNumber;
    variable eventFound: boolean := false;
begin
    tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
    tagHighTrunc := tagHigh and PTR_MASK_SN;
    groupInd := slv2u(tagHighTrunc);
    opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));

    for i in 0 to PIPE_WIDTH-1 loop
        if eventFound then
            content(groupInd, i).full <= '0';
            content(groupInd, i).killed <= '1';                   
        elsif opInd = i then
            if execInfo.ins.controlInfo.specialAction = '1' then
                content(groupInd, i).specialAction <= '1';   
                content(groupInd, i).refetch <= '1';                    
                content(groupInd, i).causing <= '1';                    
                eventFound:= true;
            end if;
        end if;
        
    end loop;
end procedure;

 
procedure updateDynamicContentMemEvent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot) is
    variable groupInd, opInd: natural;
    variable tagHigh,tagHighTrunc: SmallNumber;
begin
        tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
        tagHighTrunc := tagHigh and PTR_MASK_SN;
        groupInd := slv2u(tagHighTrunc);
        --opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));
    
        if execInfo.full = '1' then
            updateDynamicGroupMemEvent(content, execInfo, groupInd);
        end if;        
end procedure;




function groupCompleted(insVec: InstructionSlotArray) return std_logic is
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if      insVec(i).full = '1' 
		    and (insVec(i).ins.controlInfo.completed and insVec(i).ins.controlInfo.completed2) = '0'
		then
			return '0'; 
		end if;
	end loop;
	return '1';
end function;

end package body;
