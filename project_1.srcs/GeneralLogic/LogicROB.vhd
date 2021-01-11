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

type ReorderBufferEntry is record
    full: std_logic;
    ops: InstructionSlotArray(0 to PIPE_WIDTH-1);
    special: InstructionSlot;
end record;

constant DEFAULT_ROB_ENTRY: ReorderBufferEntry := (full => '0', ops => (others => DEFAULT_INSTRUCTION_SLOT), special => DEFAULT_INSTRUCTION_SLOT);

type ReorderBufferArray is array (0 to ROB_SIZE-1) of ReorderBufferEntry;
constant DEFAULT_ROB_ARRAY: ReorderBufferArray := (others => DEFAULT_ROB_ENTRY); 

function getMaskBetween(nBits: integer; startP, endP: SmallNumber; full: std_logic) return std_logic_vector;

function updateOpGroup(ops: InstructionSlotArray; execResult: InstructionSlot; num: natural; allowMem: boolean)
return InstructionSlotArray;

function groupCompleted(insVec: InstructionSlotArray) return std_logic;
   
end package;



package body LogicROB is

function groupCompleted(insVec: InstructionSlotArray) return std_logic is
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if   insVec(i).full = '1' and (
			   insVec(i).ins.controlInfo.completed = '0'
			or	 insVec(i).ins.controlInfo.completed2 = '0')
		then
			return '0'; 
		end if;
	end loop;
	return '1';
end function;

-- startP is inclusive; endP is exclusive: first empty slot
-- CAREFUL: if startP = endP, it may be either empty or full. Hence the need of 'full' flag
function getMaskBetween(nBits: integer; startP, endP: SmallNumber; full: std_logic) return std_logic_vector is
    variable res: std_logic_vector(0 to nBits-1) := (others => '0');
    variable iv: SmallNumber := (others => '0');
begin        
    -- true if either:
    --      when full, '1'; otherwise when end = start then nothing is between
    --      i >= start && i < end && end > start
    --      (i >= start || i < end) && end < start
    for i in 0 to nBits-1 loop
        iv := i2slv(i, SMALL_NUMBER_SIZE);
        if full = '1' then
            res(i) := '1';
        elsif cmpGtU(endP, startP) = '1' then
            res(i) := cmpGeU(iv, startP) and cmpLtU(iv, endP);
        else
            res(i) := cmpGeU(iv, startP) or cmpLtU(iv, endP);
        end if;
    end loop;
    
    return res;
end function;

function updateOpGroup(ops: InstructionSlotArray; execResult: InstructionSlot; num: natural; allowMem: boolean)
return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to ops'length-1) := ops;
    variable il: SmallNumber := getTagLowSN(execResult.ins.tags.renameIndex);
    variable eventFound: boolean := false;
begin 
    if execResult.full = '0' then
        return res;
    end if;
    
    for k in 0 to PIPE_WIDTH-1 loop
       if eventFound then
            res(k).full := '0';
                res(k).ins.controlInfo.killed := '1';
       end if;
    
        if slv2u(il) = k then
            if execResult.ins.controlInfo.newEvent = '1' then -- CAREFUL: branches corrected to not taken need this!
                eventFound := true;
                   res(k).ins.controlInfo.causing := '1';                   
            end if;
                    
            if execResult.ins.controlInfo.confirmedBranch = '1' then
                --res(k).ins.controlInfo.newEvent := '1'; --- !!!
                res(k).ins.controlInfo.confirmedBranch := '1';
            end if;

            if execResult.ins.controlInfo.hasException = '1' then
                --res(k).ins.controlInfo.newEvent := '1'; --- !!!
                res(k).ins.controlInfo.hasException := '1';
                eventFound := true;
            end if;

            -- Only if this is Memory subpipe:
            if allowMem and execResult.ins.controlInfo.specialAction = '1' then -- TODO: remove it, not handled by Exec engine/
                --res(k).ins.controlInfo.newEvent := '1'; --- !!!
                res(k).ins.controlInfo.specialAction := '1';
                res(k).ins.controlInfo.refetch := '1';
                eventFound := true;
            end if;

--            -- CAREFUL: this is handled in SQ. Probably not implemented here, but can/should be done for debugging
--            if execResult.ins.controlInfo.orderViolation = '1' then
--                res(k).ins.controlInfo.newEvent := '1'; --- !!!
--                res(k).ins.controlInfo.orderViolation := '1';
--            end if;

            if num = 1 then
                res(k).ins.controlInfo.completed2 := '1';
            else
                res(k).ins.controlInfo.completed := '1';
            end if;                                
        end if;
    end loop;
    
    return res;
end function;

    
end package body;
