
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

function getLatePCData(commitCausing: InstructionState;
                        commitCt: InstructionControlInfo; commitTarget: Mword;
                        int: std_logic; intType: std_logic_vector;
								currentState, linkExc, linkInt, stateExc, stateInt: Mword; special: InstructionSlot;
								                                                           specialOp: SpecificOp)
return InstructionState;

function newPCData( commitEvent: std_logic; lateTarget: Mword;
						  execEvent: std_logic; execTarget: Mword;	
						  frontEvent: std_logic; frontTarget: Mword;
						  pcNext: Mword)
return InstructionState;

-- Unifies content of ROB slot with BQ, other queues etc. to restore full state at Commit
function recreateGroup(insVec: InstructionSlotArray; prevTarget: Mword; commitCtr32: Word)
return InstructionSlotArray;

function getNewEffective(sendingToCommit: std_logic; robDataLiving: InstructionSlotArray;
                            bqTargetFull: std_logic;
                            bqTarget, lastEffectiveTarget: Mword;
						 lateTargetIns: InstructionState;
						 lateCt: InstructionControlInfo; lateTarget: Mword;
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


function getLatePCData(commitCausing: InstructionState;
                        commitCt: InstructionControlInfo; commitTarget: Mword;
                        int: std_logic; intType: std_logic_vector;
								currentState, linkExc, linkInt, stateExc, stateInt: Mword; special: InstructionSlot;
								                                                           specialOp: SpecificOp)
return InstructionState is
	variable res: InstructionState := --commitCausing;-- 
	                                   DEFAULT_INSTRUCTION_STATE;-- content;
	variable newPC: Mword := (others=>'0');
	constant MINUS_4: Mword := i2slv(-4, MWORD_SIZE);
begin
        res.controlInfo := --commitCausing.controlInfo;
                            commitCt;

    res.controlInfo.hasInterrupt := int;
    if int = '1' then
        if intType = "01" then
            res.ip := X"00000280";
        else
            res.ip := RESET_BASE; -- TEMP!
        end if;
        res.result := currentState or X"00000001";
        res.result := res.result and X"fdffffff"; -- Clear dbtrap
    elsif commitCt.hasException = '1'
        or commitCt.dbtrap = '1' then
        res.ip := EXC_BASE;
        -- TODO: if exception, it overrides dbtrap, but if only dbtrap, a specific vector address
        res.result := currentState or X"00000100";
        res.result := res.result and X"fdffffff";	-- Clear dbtrap
    elsif commitCt.specialAction = '1' then
        res.result := currentState;
        if     specialOp.system = opSync 
            or specialOp.system = opSend then                              
            res.ip := commitTarget;
        elsif specialOp.system = opReplay       
            or commitCt.refetch = '1' then
            res.ip := add(commitTarget, MINUS_4); -- CAREFUL: wouldn't work if branch or short
        elsif specialOp.system = opHalt then
            res.ip := commitTarget; -- ???
        elsif specialOp.system = opRetI then
            res.result := stateInt;
            res.ip := linkInt;
        elsif specialOp.system = opRetE then 
            res.result := stateExc;
            res.ip := linkExc;
        elsif specialOp.system = opError then
            res.ip := EXC_BASE;
        elsif specialOp.system = opCall then
            res.ip := CALL_BASE; -- TEMP			    
        end if;
    end if;		
   
    res.target := res.ip;
    res.ip := commitTarget;
    
    res.tags := DEFAULT_INSTRUCTION_TAGS; 
	return res;
end function;


function newPCData(       commitEvent: std_logic; lateTarget: Mword;
						  execEvent: std_logic; execTarget: Mword;	
						  frontEvent: std_logic; frontTarget: Mword;
						  pcNext: Mword)
return InstructionState is
	variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
	variable newPC: Mword := (others=>'0');
begin
	if commitEvent = '1' then
		res.ip := lateTarget;
	elsif execEvent = '1' then		
		res.ip := execTarget;
	elsif frontEvent = '1' then
		res.ip := frontTarget;
	else	-- Go to the next line
		res.ip := pcNext;
	end if;

	return res;
end function;


-- TODO: move to Visibility?
-- Unifies content of ROB slot with BQ, others queues etc. to restore full state needed at Commit
-- CAREFUL: probably not useful at all
function recreateGroup(insVec: InstructionSlotArray; prevTarget: Mword; commitCtr32: Word)
return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
	variable targets: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	variable confBr: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable prevTrg: Mword := prevTarget;
begin
	for i in 0 to PIPE_WIDTH-1 loop
		targets(i) := prevTrg;
	end loop;
	
	-- Take branch targets to correct places
	for i in 0 to PIPE_WIDTH-1 loop
		if insVec(i).full = '1' and insVec(i).ins.controlInfo.confirmedBranch = '1' then
			confBr(i) := '1';
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


function getLastEffective(newContent: InstructionSlotArray) return InstructionState is
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


function getNewEffective(sendingToCommit: std_logic; robDataLiving: InstructionSlotArray;
                            bqTargetFull: std_logic;
                            bqTarget, lastEffectiveTarget: Mword;
						 lateTargetIns: InstructionState;
						 lateCt: InstructionControlInfo; lateTarget: Mword;						 
						 evtPhase2: std_logic)
return InstructionSlot is
	variable res: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	variable lastEff: InstructionState := DEFAULT_INS_STATE;
	variable targetInc: Mword := (others => '0');
	variable anyConfirmed: boolean := false;
begin
    for i in PIPE_WIDTH-1 downto 0 loop
        if robDataLiving(i).full = '1' then
            targetInc := i2slv(4*(i+1), MWORD_SIZE);  -- CAREFUL: only for 4b instructions
            exit;
        end if;
    end loop;
   
    for i in 0 to PIPE_WIDTH-1 loop 
        if robDataLiving(i).full = '1' and robDataLiving(i).ins.controlInfo.confirmedBranch = '1' then 
            anyConfirmed := true;
        end if;
    end loop;
	   
	if evtPhase2 = '1' then
	   res.full := '1';
	   res.ins.controlInfo := lateCt;--lateTargetIns.controlInfo;
	   res.ins.target := lateTarget;-- lateTargetIns.target;	   
	   return res;--('1', lateTargetIns);
	end if;


--function getLastEffective(newContent: InstructionSlotArray) return InstructionState is
--    variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
--begin
    lastEff := robDataLiving(0).ins;
    for i in PIPE_WIDTH-1 downto 0 loop
        if robDataLiving(i).full = '1' then
            lastEff := robDataLiving(i).ins;
            lastEff.controlInfo.newEvent := hasSyncEvent(robDataLiving(i).ins.controlInfo); -- Announce that event is to happen now!                       
            exit;
        end if;
    end loop;

--    return res;
--end function;
	
    --res := (sendingToCommit, lastEff);--getLastEffective(robDataLiving));
        res.full := sendingToCommit;
        res.ins.controlInfo := lastEff.controlInfo;
        res.ins.target := lastEff.target;

    if bqTargetFull = '1' and anyConfirmed then -- TODO, CHECK: bqTargetData.full will always be '1' if anyConfirmed?
        res.ins.target := bqTarget;
    else
        res.ins.target := add(lastEffectiveTarget, targetInc);        
    end if;
    
--    if CLEAR_DEBUG_INFO then
--        res.ins.ip := (others => '0');
--        res.ins.bits := (others => '0');
--        res.ins.virtualArgSpec := DEFAULT_ARG_SPEC;
--        res.ins.physicalArgSpec := DEFAULT_ARG_SPEC;
        
--        res.ins.classInfo := DEFAULT_CLASS_INFO;
--        res.ins.constantArgs := DEFAULT_CONSTANT_ARGS;
        
--        res.ins.specificOperation.arith := ArithOp'(opAnd);
--        res.ins.specificOperation.memory := MemOp'(opLoad);
--        res.ins.specificOperation.float := FpOp'(opMove);
        
--        res.ins.tags := DEFAULT_INSTRUCTION_TAGS;
--    end if;    
	return res;
end function;


function anyEvent(insVec: InstructionSlotArray) return std_logic is
begin
    for i in 0 to PIPE_WIDTH-1 loop
        if (insVec(i).ins.controlInfo.full = '1') and hasSyncEvent(insVec(i).ins.controlInfo) = '1' then
            return '1';
        end if;            
    end loop;
    return '0'; 
end function;

end LogicSequence;
