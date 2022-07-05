
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

function getLatePCData(commitCt: InstructionControlInfo; commitTarget: Mword; int: std_logic; intType: std_logic_vector;
					   currentState, linkExc, linkInt, stateExc, stateInt: Mword; specialOp: SpecificOp)
return ControlPacket;

function newPCData(commitEvent: std_logic; lateTarget: Mword;
                   execEvent: std_logic; execTarget: Mword;	
                   frontEvent: std_logic; frontTarget: Mword;
                   pcNext: Mword)
return InstructionState;

function getNewEffective(sendingToCommit: std_logic;
                         robData_N: ControlPacketArray; bqTargetFull: std_logic;
                         bqTarget, lastEffectiveTarget: Mword;
						 lateCt: InstructionControlInfo; lateTarget: Mword;
						 evtPhase2: std_logic)
return ControlPacket;

function anyEvent(insVec: InstructionSlotArray) return std_logic;
function anyEvent(cpa: ControlPacketArray) return std_logic;

function assignCommitNumbers(cpa: ControlPacketArray; ctr: Word) return ControlPacketArray;

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


function getLatePCData(commitCt: InstructionControlInfo; commitTarget: Mword; int: std_logic; intType: std_logic_vector;
					   currentState, linkExc, linkInt, stateExc, stateInt: Mword; specialOp: SpecificOp)
return ControlPacket is
	variable res_O: InstructionState := DEFAULT_INSTRUCTION_STATE;
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
	variable newPC: Mword := (others=>'0');
	constant MINUS_4: Mword := i2slv(-4, MWORD_SIZE);
begin
    res_O.controlInfo := commitCt;

    res_O.controlInfo.hasInterrupt := int;
    if int = '1' then
        if intType = "01" then
            res_O.ip := X"00000280";
        else
            res_O.ip := RESET_BASE; -- TEMP!
        end if;
        res_O.result := currentState or X"00000001";
        res_O.result := res_O.result and X"fdffffff"; -- Clear dbtrap
    elsif commitCt.hasException = '1'
        or commitCt.dbtrap = '1' then
        res_O.ip := EXC_BASE;
        -- TODO: if exception, it overrides dbtrap, but if only dbtrap, a specific vector address
        res_O.result := currentState or X"00000100";
        res_O.result := res_O.result and X"fdffffff";	-- Clear dbtrap
    elsif commitCt.specialAction = '1' then
        res_O.result := currentState;
        if     specialOp.system = opSync 
            or specialOp.system = opSend then                              
            res_O.ip := commitTarget;
        elsif specialOp.system = opReplay       
            or commitCt.refetch = '1' then
            res_O.ip := add(commitTarget, MINUS_4); -- CAREFUL: wouldn't work if branch or short
        elsif specialOp.system = opHalt then
            res_O.ip := commitTarget; -- ???
        elsif specialOp.system = opRetI then
            res_O.result := stateInt;
            res_O.ip := linkInt;
        elsif specialOp.system = opRetE then 
            res_O.result := stateExc;
            res_O.ip := linkExc;
        elsif specialOp.system = opError then
            res_O.ip := EXC_BASE;
        elsif specialOp.system = opCall then
            res_O.ip := CALL_BASE; -- TEMP			    
        end if;
    end if;		
   
    res_O.target := res_O.ip;
    res_O.ip := commitTarget;
    
    res_O.tags := DEFAULT_INSTRUCTION_TAGS;
    
    res.controlInfo := res_O.controlInfo;
    res.ip := res_O.ip;
    res.target := res_O.target;
    res.nip := res_O.result;

	return res;
end function;


function newPCData(commitEvent: std_logic; lateTarget: Mword;
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


function getNewEffective(sendingToCommit: std_logic;
                         robData_N: ControlPacketArray;
                         bqTargetFull: std_logic;
                         bqTarget, lastEffectiveTarget: Mword;
						 lateCt: InstructionControlInfo; lateTarget: Mword;						 
						 evtPhase2: std_logic)
return ControlPacket is
	variable res_O: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
	variable lastEff: InstructionState := DEFAULT_INS_STATE;
	variable targetInc: Mword := (others => '0');
	variable anyConfirmed: boolean := false;
begin
    for i in PIPE_WIDTH-1 downto 0 loop
        if robData_N(i).controlInfo.full = '1' then
           targetInc := i2slv(4*(i+1), MWORD_SIZE);  -- CAREFUL: only for 4b instructions
           exit;
        end if;
    end loop;
   
    for i in 0 to PIPE_WIDTH-1 loop 
        if robData_N(i).controlInfo.full = '1' and robData_N(i).controlInfo.confirmedBranch = '1' then 
            anyConfirmed := true;
        end if;
    end loop;
	   
	if evtPhase2 = '1' then
	   res_O.full := '1';
	   res_O.ins.controlInfo := lateCt;
	   res_O.ins.target := lateTarget;   
	   
	   res.controlInfo := res_O.ins.controlInfo;
	   res.target := res_O.ins.target;
	   return res;
	end if;

    lastEff.controlInfo := robData_N(0).controlInfo;
    lastEff.target := robData_N(0).target;

    for i in PIPE_WIDTH-1 downto 0 loop
        if robData_N(i).controlInfo.full = '1' then
           lastEff.controlInfo := robData_N(i).controlInfo;
           lastEff.controlInfo.newEvent := hasSyncEvent(robData_N(i).controlInfo);
           lastEff.target := robData_N(i).target;
           exit;
        end if;
    end loop;

    res_O.full := sendingToCommit;
    res_O.ins.controlInfo := lastEff.controlInfo;
    res_O.ins.target := lastEff.target;

    if bqTargetFull = '1' and anyConfirmed then -- TODO, CHECK: bqTargetData.full will always be '1' if anyConfirmed?
        res_O.ins.target := bqTarget;
    else
        res_O.ins.target := add(lastEffectiveTarget, targetInc);        
    end if;
        
    res.controlInfo := res_O.ins.controlInfo;
    res.target := res_O.ins.target;
    
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

function anyEvent(cpa: ControlPacketArray) return std_logic is
begin
    for i in 0 to PIPE_WIDTH-1 loop
        if (cpa(i).controlInfo.full = '1') and hasSyncEvent(cpa(i).controlInfo) = '1' then
            return '1';
        end if;            
    end loop;
    return '0'; 
end function;

-- DEBUG
function assignCommitNumbers(cpa: ControlPacketArray; ctr: Word) return ControlPacketArray is
    variable res: ControlPacketArray(0 to PIPE_WIDTH-1) := cpa;
begin
    for i in res'range loop
        if cpa(i).controlInfo.full /= '1' then
--            res(i).dbInfo := DEFAULT_DEBUG_INFO;
        else
--            res(i).dbInfo.commit := addInt(ctr, i);
        end if;
    end loop;
    return res;
end function; 

end LogicSequence;
