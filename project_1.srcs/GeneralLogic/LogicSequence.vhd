
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

-- Unifies content of ROB slot with BQ, others queues etc. to restore full state needed at Commit
function recreateGroup(insVec: InstructionSlotArray; bqGroup: InstructionSlotArray; prevTarget: Mword; commitCtr32: Word)
return InstructionSlotArray;


function clearControlEvents(ins: InstructionState) return InstructionState;

function getNewEffective(sendingToCommit: std_logic; robDataLiving, dataFromBQV: InstructionSlotArray; effectiveMask: std_logic_vector;
								 lastEffectiveIns, lateTargetIns: InstructionState;
								 evtPhase2: std_logic)

return InstructionSlot;

function getEffectiveMask(newContent: InstructionSlotArray) return std_logic_vector;

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
            res.ip := --(others => 'U');
                      commitCausing.target;  
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
			ind := slv2u(getTagLow(bqGroup(i).ins.tags.renameIndex));
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


function getLastEffective(newContent: InstructionSlotArray; effectiveMask: std_logic_vector) return InstructionState is
    variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
begin
    res := newContent(0).ins;
    for i in PIPE_WIDTH-1 downto 0 loop
        if effectiveMask(i) = '1' then
            res := newContent(i).ins;
            res.controlInfo.newEvent := hasSyncEvent(newContent(i).ins); -- Announce that event is to happen now!                       
            exit;
        end if;
    end loop;

    return res;
end function;


function getEffectiveMask(newContent: InstructionSlotArray) return std_logic_vector is
    variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
        return extractFullMask(newContent);

    -- Seeking from right side cause we need the last one 
    for i in newContent'range loop
        if newContent(i).full /= '1' then -- Count only full instructions
            exit;
        end if;
        res(i) := '1';
        
        -- If this one has an event, following ones don't count
        if hasSyncEvent(newContent(i).ins) = '1' then
            exit;
        end if;
        
    end loop;
    return res;
end function;

function getNewEffective(sendingToCommit: std_logic; robDataLiving, dataFromBQV: InstructionSlotArray; effectiveMask: std_logic_vector;
						 lastEffectiveIns, lateTargetIns: InstructionState; evtPhase2: std_logic)
return InstructionSlot is
	variable res: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	--variable sdToCommit: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	variable insToLastEffective: InstructionState;	
	variable takenBranchVec, bqTakenBranchVec, differenceVec: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable branchTarget: Mword := lastEffectiveIns.target;
	variable ind: std_logic_vector(LOG2_PIPE_WIDTH-1 downto 0) := (others => '0');
	variable targetInc, targetInc_T: Mword := (others => '0');
	variable lastConfirmedBranchInd, lastEffectiveInd: std_logic_vector(LOG2_PIPE_WIDTH downto 0) := (others => '0');
begin
        lastConfirmedBranchInd := i2slv(-1, LOG2_PIPE_WIDTH+1);

	--sdToCommit := robDataLiving;
    --effectiveVec := getEffectiveMask(robDataLiving);	
	
	insToLastEffective := getLastEffective(robDataLiving, effectiveMask);
	
	if evtPhase2 = '1' then
	   res := ('1', lateTargetIns);
	   return res;
	end if;
	
    res := (sendingToCommit, insToLastEffective);

    -- Find taken jumps in ROB entry and last effective index
    
    for i in robDataLiving'range loop
        takenBranchVec(i) := effectiveMask(i) and robDataLiving(i).ins.controlInfo.confirmedBranch;			
    end loop;		

    for i in robDataLiving'range loop
        if effectiveMask(i) = '1' then
            lastEffectiveInd := i2slv(i, LOG2_PIPE_WIDTH+1);
        end if;			
    end loop;
    
    -- Find taken jumps in BQ group and select last as target
    branchTarget := lastEffectiveIns.target;
    -- Now last effective and taken target from BQ
    for i in 0 to PIPE_WIDTH-1 loop
        -- Corresponding ROB entry must be effective, otherwise branch doesn't happen!
        -- But if not effective, BQ entry would've been killed, so no need to check
        if dataFromBQV(i).full = '0' then
            exit;
        end if;
        
        if dataFromBQV(i).ins.controlInfo.confirmedBranch = '1' then
            branchTarget := dataFromBQV(i).ins.target;
            lastConfirmedBranchInd := --i2slv(i, LOG2_PIPE_WIDTH+1);
                                      '0' & dataFromBQV(i).ins.tags.renameIndex(LOG2_PIPE_WIDTH-1 downto 0);  
        end if;
    end loop;		
    
    -- Find number of effective instructions after the jump
    for i in PIPE_WIDTH-1 downto 0 loop
        if takenBranchVec(i) = '1' then
            exit;
        elsif effectiveMask(i) = '1' then
            differenceVec(i) := '1';
        end if;
    end loop;
    -- CAREFUL: works only for 32b instructions
    targetInc(LOG2_PIPE_WIDTH + 2 downto 2) := i2slv(countOnes(differenceVec), LOG2_PIPE_WIDTH+1);
    targetInc_T(LOG2_PIPE_WIDTH + 2 downto 2) := --i2slv(countOnes(effectiveVec), LOG2_PIPE_WIDTH+1);
                                                 sub(lastEffectiveInd, lastConfirmedBranchInd);
    
    res.ins.target := add(branchTarget, --targetInc);
                                        targetInc_T);
    --        res.ins.ip := targetInc xor targetInc_T;
    
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
    end if;    
	return res;
end function;


function anyEvent(insVec: InstructionSlotArray) return std_logic is
    variable effectiveMask: std_logic_vector(0 to PIPE_WIDTH-1) := getEffectiveMask(insVec);
                                                                    --extractFullMask(insVec);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        if (effectiveMask(i) = '1') and hasSyncEvent(insVec(i).ins) = '1' then
            return '1';
        end if;            
    end loop;
    return '0'; 
end function;

end LogicSequence;
