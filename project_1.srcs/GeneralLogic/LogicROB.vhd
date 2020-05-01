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

function updateOpGroup(ops: InstructionSlotArray; execResult: InstructionSlot; num: natural)
return InstructionSlotArray;

function groupCompleted(insVec: InstructionSlotArray) return std_logic;

type RobText is array(0 to ROB_SIZE-1) of string(1 to 70);

function getRobView(arr: ReorderBufferArray) return RobText;
   
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
            res(i) := cmpLeU(iv, startP) and cmpLtU(iv, endP);
        else
            res(i) := cmpLtU(iv, startP) and cmpLtU(iv, endP);
        end if;
    end loop;
    
    return res;
end function;

function updateOpGroup(ops: InstructionSlotArray; execResult: InstructionSlot; num: natural)
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
       end if;
    
        if slv2u(il) = k then
            if execResult.ins.controlInfo.newEvent = '1' then -- CAREFUL: branches corrected to not taken need this!
                eventFound := true;                    
            end if;
                    
            if execResult.ins.controlInfo.confirmedBranch = '1' then
                res(k).ins.controlInfo.newEvent := '1'; --- !!!
                res(k).ins.controlInfo.confirmedBranch := '1';
            end if;

            if execResult.ins.controlInfo.hasException = '1' then
                res(k).ins.controlInfo.newEvent := '1'; --- !!!
                res(k).ins.controlInfo.hasException := '1';
                eventFound := true;
            end if;

            if execResult.ins.controlInfo.specialAction = '1' then
                res(k).ins.controlInfo.newEvent := '1'; --- !!!
                res(k).ins.controlInfo.specialAction := '1';
                res(k).ins.controlInfo.refetch := '1';
                eventFound := true;
            end if;
                            
            if num = 1 then
                res(k).ins.controlInfo.completed2 := '1';
            else
                res(k).ins.controlInfo.completed := '1';
            end if;                                
        end if;
    end loop;
    
    return res;
end function;

function getRobView(arr: ReorderBufferArray) return RobText is
    variable str: string(1 to 70);
    variable res: RobText;
    
    use work.Viewing.all; -- TEMP!
begin


    for i in 0 to ROB_SIZE-1 loop
        --res(i) := "1 [ 1 #99494944 or ; 1 #84995595 jnz   ; 0  #5059555    ; ---------- ]";
                -- C [ CCE #num ; CCE #num ; CCE #num ; CCE #num ]
        str := (others => ' ');
                
        if arr(i).full = '1' then
            str(1) := std_logic'image(groupCompleted(arr(i).ops))(2);
        else
            str(1) := '-';
            res(i) := str;
            next;
        end if;
        
        str(2 to 4) := " [ ";
        
        for j in 0 to PIPE_WIDTH-1 loop
            if arr(i).ops(j).full = '1' then
                str(5 + 16*j) := std_logic'image(arr(i).ops(j).ins.controlInfo.completed)(2);
                str(6 + 16*j) := std_logic'image(arr(i).ops(j).ins.controlInfo.completed2)(2);
                str(7 + 16*j) := ' ';
                if arr(i).ops(j).ins.controlInfo.hasException = '1' then
                    str(7 + 16*j) := 'E';
                end if;
                str(8 + 16*j) := '#';
                str(9 + 16*j to 16 + 16*j) := w2hex(arr(i).ops(j).ins.tags.fetchCtr);  
            else
                str(5 + 16*j to 16 + 16*j) := (others => ' ');
            end if;
            str(17 + 16*j to 19 + 16*j) := " ; ";
        end loop;    
            
        str(18 + 16*(PIPE_WIDTH-1)) := ']';     
        str(18 + 16*(PIPE_WIDTH-1) + 2) := std_logic'image(arr(i).special.full)(2);
        
        res(i) := str;
        
    end loop;
    
    return res;
end function; 
    
end package body;
