
library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.Arith.all;

use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package LogicSequence is

function getNextPC(pc: Mword; jumpPC: Mword; jump: std_logic) return Mword;

function getLatePCData(commitCausing: InstructionState; int: std_logic; intType: std_logic_vector;
								currentState, linkExc, linkInt, stateExc, stateInt: Mword; special: InstructionSlot)
return InstructionState;

function newPCData( commitEvent: std_logic; commitCausing: InstructionState;
						  execEvent: std_logic; execCausing: InstructionState;	
						  frontEvent: std_logic; frontCausing: InstructionState;
						  pcNext: Mword)
return InstructionState;

-- Unifies content of ROB slot with BQ, other queues etc. to restore full state at Commit
function recreateGroup(insVec: InstructionSlotArray; bqGroup: InstructionSlotArray; prevTarget: Mword; commitCtr32: Word)
return InstructionSlotArray;


-- UNUSED
function clearControlEvents(ins: InstructionState) return InstructionState;

function getNewEffective(sendingToCommit: std_logic; robDataLiving, dataFromBQV: InstructionSlotArray; bqTargetData: InstructionSlot;
								 lastEffectiveIns, lateTargetIns: InstructionState;
								 evtPhase2: std_logic)
return InstructionSlot;


function anyEvent(insVec: InstructionSlotArray) return std_logic;

end LogicSequence;



package body LogicSequence is

function getNextPC(pc: Mword; jumpPC: Mword; jump: std_logic) return Mword is
	variable res, pcBase: Mword := (others => '0'); 
begin
	pcBase := pc and i2slv(-PIPE_WIDTH*4, MWORD_SIZE); -- Clearing low bits
	if jump = '1' then
		res := jumpPC;
	else
		res := add(pcBase, PC_INC);
	end if;
	return res;
end function;


function getLatePCData(commitCausing: InstructionState; int: std_logic; intType: std_logic_vector;
								currentState, linkExc, linkInt, stateExc, stateInt: Mword; special: InstructionSlot)
return InstructionState is
	variable res: InstructionState := commitCausing;-- DEFAULT_INSTRUCTION_STATE;-- content;
	variable newPC: Mword := (others=>'0');
	constant MINUS_4: Mword := i2slv(-4, MWORD_SIZE);
begin
    res.controlInfo.hasInterrupt := int;
    if int = '1' then
        if intType = "01" then
            res.ip := X"00000280";
        else
            res.ip := INT_BASE; -- TEMP!
        end if;
        res.result := currentState or X"00000001";
        res.result := res.result and X"fdffffff"; -- Clear dbtrap
    elsif commitCausing.controlInfo.hasException = '1'
        or commitCausing.controlInfo.dbtrap = '1' then
        res.ip := EXC_BASE;
        -- TODO: if exception, it overrides dbtrap, but if only dbtrap, a specific vector address
        res.result := currentState or X"00000100";
        res.result := res.result and X"fdffffff";	-- Clear dbtrap
    elsif commitCausing.controlInfo.specialAction = '1' then
        res.result := currentState;
        if     special.ins.specificOperation.system = opSync 
            or special.ins.specificOperation.system = opSend then                              
            res.ip := commitCausing.target;
        elsif special.ins.specificOperation.system = opReplay       
            or commitCausing.controlInfo.refetch = '1' then
            res.ip := add(commitCausing.target, MINUS_4); -- CAREFUL: wouldn't work if branch or short
        elsif special.ins.specificOperation.system = opHalt then
            res.ip := commitCausing.target; -- ???
        elsif special.ins.specificOperation.system = opRetI then
            res.result := stateInt;
            res.ip := linkInt;
        elsif special.ins.specificOperation.system = opRetE then 
            res.result := stateExc;
            res.ip := linkExc;
        elsif special.ins.specificOperation.system = opError then
            res.ip := EXC_BASE;
        elsif special.ins.specificOperation.system = opCall then
            res.ip := CALL_BASE; -- TEMP			    
        end if;
    end if;		
   
    res.target := res.ip;
    res.ip := commitCausing.target;
    
    res.tags := DEFAULT_INSTRUCTION_TAGS; 
	return res;
end function;


function newPCData( commitEvent: std_logic; commitCausing: InstructionState;
						  execEvent: std_logic; execCausing: InstructionState;	
						  frontEvent: std_logic; frontCausing: InstructionState;
						  pcNext: Mword)
return InstructionState is
	variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
	variable newPC: Mword := (others=>'0');
begin
	if commitEvent = '1' then
		res.ip := commitCausing.target;
	elsif execEvent = '1' then		
		res.ip := execCausing.target;
	elsif frontEvent = '1' then
		res.ip := frontCausing.target;
	else	-- Go to the next line
		res.ip := pcNext;
	end if;

	return res;
end function;


function clearControlEvents(ins: InstructionState) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.controlInfo.newEvent := '0';
	res.controlInfo.hasInterrupt := '0';
	res.controlInfo.hasException := '0';	
	res.controlInfo.specialAction := '0';
	return res;
end function;


-- Unifies content of ROB slot with BQ, others queues etc. to restore full state needed at Commit
function recreateGroup(insVec: InstructionSlotArray; bqGroup: InstructionSlotArray; prevTarget: Mword; commitCtr32: Word)
return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
	variable targets: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	variable confBr: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable ind: integer := 0;
	variable prevTrg: Mword := prevTarget;
begin
	
	for i in 0 to PIPE_WIDTH-1 loop
		targets(i) := prevTrg;
	end loop;
	
	-- Take branch targets to correct places
	for i in 0 to PIPE_WIDTH-1 loop
		if bqGroup(i).full = '1' then
			ind := --slv2u(getTagLow(bqGroup(i).ins.tags.renameIndex));
			       i;  
			targets(ind) := bqGroup(i).ins.target;
			confBr(ind) := '1';
		end if;
	end loop;

	for i in 0 to PIPE_WIDTH-1 loop
		if confBr(i) = '0' then
			targets(i) := add(prevTrg, getAddressIncrement(insVec(i).ins));
		end if;
		res(i).ins.ip := prevTrg;
		prevTrg := targets(i);
		res(i).ins.target := targets(i);
		res(i).ins.tags.commitCtr := i2slv(slv2u(commitCtr32) + i, 32);
	end loop;
	
	return res;
end function;


function getLastEffective(newContent: InstructionSlotArray)-- effectiveMask: std_logic_vector)
return InstructionState is
    variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
begin
    res := newContent(0).ins;
    for i in PIPE_WIDTH-1 downto 0 loop
        if newContent(i).full = '1' then
            res := newContent(i).ins;
            res.controlInfo.newEvent := hasSyncEvent(newContent(i).ins); -- Announce that event is to happen now!                       
            exit;
        end if;
    end loop;

    return res;
end function;


function getNewEffective(sendingToCommit: std_logic; robDataLiving, dataFromBQV: InstructionSlotArray; bqTargetData: InstructionSlot;
						 lastEffectiveIns, lateTargetIns: InstructionState; evtPhase2: std_logic)
return InstructionSlot is
	variable res: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	variable insToLastEffective: InstructionState;	
	variable targetInc: Mword := (others => '0');
	variable anyConfirmed: boolean := false;
begin
	insToLastEffective := getLastEffective(robDataLiving);

    for i in PIPE_WIDTH-1 downto 0 loop
        if robDataLiving(i).full = '1' then
            targetInc := i2slv(4*(i+1), MWORD_SIZE);  -- CAREFUL: only for 4b instructions
            exit;
        end if;
    end loop;
   
    for i in 0 to PIPE_WIDTH-1 loop 
        if dataFromBQV(i).full = '1' and dataFromBQV(i).ins.controlInfo.confirmedBranch = '1' then
            anyConfirmed := true;
        end if;
    end loop;
	   
	if evtPhase2 = '1' then
	   return ('1', lateTargetIns);
	end if;
	
    res := (sendingToCommit, insToLastEffective);

    if bqTargetData.full = '1' and anyConfirmed then -- TODO, CHECK: bqTargetData.full will always be '1' if anyConfirmed?
        res.ins.target := bqTargetData.ins.target;
    else
        res.ins.target := add(lastEffectiveIns.target, targetInc);        
    end if;
    
    if CLEAR_DEBUG_INFO then
        res.ins.ip := (others => '0');
        res.ins.bits := (others => '0');
        res.ins.virtualArgSpec := DEFAULT_ARG_SPEC;
        res.ins.physicalArgSpec := DEFAULT_ARG_SPEC;
        
        res.ins.classInfo := DEFAULT_CLASS_INFO;
        res.ins.constantArgs := DEFAULT_CONSTANT_ARGS;
        
        res.ins.specificOperation.arith := ArithOp'(opAnd);
        res.ins.specificOperation.memory := MemOp'(opLoad);
        res.ins.specificOperation.float := FpOp'(opMove);
        
        res.ins.tags := DEFAULT_INSTRUCTION_TAGS;
    end if;    
	return res;
end function;


function anyEvent(insVec: InstructionSlotArray) return std_logic is
begin
    for i in 0 to PIPE_WIDTH-1 loop
        if (insVec(i).full = '1') and hasSyncEvent(insVec(i).ins) = '1' then
            return '1';
        end if;            
    end loop;
    return '0'; 
end function;

end LogicSequence;
