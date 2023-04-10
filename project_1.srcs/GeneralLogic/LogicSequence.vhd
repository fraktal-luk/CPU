
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
return Mword;

function getNewEffective(sendingToCommit: std_logic;
                         robData: ControlPacketArray; bqTargetFull: std_logic;
                         bqTarget, lastEffectiveTarget: Mword;
						 lateCt: InstructionControlInfo; lateTarget: Mword;
						 evtPhase2: std_logic)
return ControlPacket;

function anyEvent(cpa: ControlPacketArray) return std_logic;

function assignCommitNumbers(cpa: ControlPacketArray; ctr: Word) return ControlPacketArray;


-- Debug functions
function DB_addCommit(dbi: InstructionDebugInfo; commit: Word) return InstructionDebugInfo;
function DB_addIndex(dbi: InstructionDebugInfo; index: Word) return InstructionDebugInfo;
function DB_incIndex(dbi: InstructionDebugInfo) return InstructionDebugInfo;
function DB_addCycle(dbi: InstructionDebugInfo; cycle: Word) return InstructionDebugInfo;

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
	variable ct: InstructionControlInfo := DEFAULT_CONTROL_INFO;
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
	variable target, state: Mword := (others=>'0');
	constant MINUS_4: Mword := i2slv(-4, MWORD_SIZE);
begin
    --res.dbInfo := commitCt.dbInfo;

    ct := commitCt;
    ct.hasInterrupt := int;

    if int = '1' then
        if intType = "01" then
            target := X"00000280";
        else
            target := RESET_BASE; -- TEMP!
        end if;
        state := currentState or X"00000001";
        state := state and X"fdffffff"; -- Clear dbtrap
    elsif commitCt.hasException = '1'
        or commitCt.dbtrap = '1' then
        target := EXC_BASE;
        -- TODO: if exception, it overrides dbtrap, but if only dbtrap, a specific vector address
        state := currentState or X"00000100";
        state := state and X"fdffffff";	-- Clear dbtrap
    elsif commitCt.specialAction = '1' then
        state := currentState;
        if     specialOp.system = opSync 
            or specialOp.system = opSend then                              
            target := commitTarget;
        elsif specialOp.system = opReplay       
            or commitCt.refetch = '1' then
            target := addInt(commitTarget, -4); -- CAREFUL: wouldn't work if branch or short
        elsif specialOp.system = opHalt then
            target := commitTarget; -- ???
        elsif specialOp.system = opRetI then
            state := stateInt;
            target := linkInt;
        elsif specialOp.system = opRetE then 
            state := stateExc;
            target := linkExc;
        elsif specialOp.system = opError or specialOp.system = opUndef then
            target := EXC_BASE;
        elsif specialOp.system = opCall then
            target := CALL_BASE; -- TEMP			    
        end if;
    end if;

    res.controlInfo := ct;
    res.ip := commitTarget;
    res.target := target;
    res.nip := state;

	return res;
end function;


function newPCData(commitEvent: std_logic; lateTarget: Mword;
                   execEvent: std_logic; execTarget: Mword;	
                   frontEvent: std_logic; frontTarget: Mword;
                   pcNext: Mword)
return Mword is
begin
	if commitEvent = '1' then
		return lateTarget;
	elsif execEvent = '1' then		
		return execTarget;
	elsif frontEvent = '1' then
		return frontTarget;
	else	-- Go to the next line
		return pcNext;
	end if;
end function;


function getNewEffective(sendingToCommit: std_logic;
                         robData: ControlPacketArray;
                         bqTargetFull: std_logic;
                         bqTarget, lastEffectiveTarget: Mword;
						 lateCt: InstructionControlInfo; lateTarget: Mword;						 
						 evtPhase2: std_logic)
return ControlPacket is
	variable ct_O: InstructionControlInfo := DEFAULT_CONTROL_INFO;
	variable target_O: Mword := (others => '0');
	variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
	variable targetInc: Mword := (others => '0');
	variable anyConfirmed: boolean := false;
begin
    for i in PIPE_WIDTH-1 downto 0 loop
        if robData(i).controlInfo.full = '1' then
           targetInc := i2slv(4*(i+1), MWORD_SIZE);  -- CAREFUL: only for 4b instructions
           exit;
        end if;
    end loop;
   
    for i in 0 to PIPE_WIDTH-1 loop 
        if robData(i).controlInfo.full = '1' and robData(i).controlInfo.confirmedBranch = '1' then 
            anyConfirmed := true;
        end if;
    end loop;

	if evtPhase2 = '1' then
	   res.controlInfo := lateCt;
	   res.target := lateTarget;
	   return res;
	end if;

    ct_O := robData(0).controlInfo;
    --target_O := robData(0).target;

    for i in PIPE_WIDTH-1 downto 0 loop
        if robData(i).controlInfo.full = '1' then
           ct_O := robData(i).controlInfo;
           ct_O.newEvent := hasSyncEvent(robData(i).controlInfo);
           --target_O := robData(i).target;
           exit;
        end if;
    end loop;

    if anyConfirmed then
        target_O := bqTarget;
    else
        target_O := add(lastEffectiveTarget, targetInc);      
    end if;

    res.controlInfo := ct_O;
    res.target := target_O;
    
	return res;
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
            res(i).dbInfo := DEFAULT_DEBUG_INFO;
        else
            res(i).dbInfo := DB_addCommit(res(i).dbInfo, addInt(ctr, i));
        end if;
    end loop;
    return res;
end function; 

-- Debug functions
function DB_addCommit(dbi: InstructionDebugInfo; commit: Word) return InstructionDebugInfo is
    variable res: InstructionDebugInfo := dbi;
begin
    -- pragma synthesis off
    res.commit := commit;
    -- pragma synthesis on
    return res;
end function;

function DB_addIndex(dbi: InstructionDebugInfo; index: Word) return InstructionDebugInfo is
    variable res: InstructionDebugInfo := dbi;
begin
    -- pragma synthesis off
    res.index := index;
    -- pragma synthesis on
    return res;
end function;

function DB_incIndex(dbi: InstructionDebugInfo) return InstructionDebugInfo is
    variable res: InstructionDebugInfo := dbi;
begin
    -- pragma synthesis off
    res.index := addInt(res.index, 1);
    -- pragma synthesis on
    return res;
end function;

function DB_addCycle(dbi: InstructionDebugInfo; cycle: Word) return InstructionDebugInfo is
    variable res: InstructionDebugInfo := dbi;
begin
    -- pragma synthesis off
    res.cycle := cycle;
    -- pragma synthesis on
    return res;
end function;


end LogicSequence;
