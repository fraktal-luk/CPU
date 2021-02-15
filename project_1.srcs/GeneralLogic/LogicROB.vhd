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




type ReorderBufferEntry is record
    full: std_logic;
    ops: InstructionSlotArray(0 to PIPE_WIDTH-1);
    special: InstructionSlot;
end record;

constant DEFAULT_ROB_ENTRY: ReorderBufferEntry := (full => '0', ops => (others => DEFAULT_INSTRUCTION_SLOT), special => DEFAULT_INSTRUCTION_SLOT);

type ReorderBufferArray is array (0 to ROB_SIZE-1) of ReorderBufferEntry;
constant DEFAULT_ROB_ARRAY: ReorderBufferArray := (others => DEFAULT_ROB_ENTRY); 

function getMaskBetween(constant LEN: natural; startP, endP: SmallNumber; full: std_logic) return std_logic_vector;

function updateOpGroup(ops: InstructionSlotArray; execResult: InstructionSlot; constant NUM: natural; constant ALLOW_MEM: boolean)
return InstructionSlotArray;

function updateOpGroupCompleted(ops: InstructionSlotArray; execResult: InstructionSlot; constant NUM: natural; constant ALLOW_MEM: boolean)
return InstructionSlotArray;


function groupCompleted(insVec: InstructionSlotArray) return std_logic;
 
 
 

	function getNextRobContent_Input(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           newSpecialAction: InstructionSlot;
	                           receiving: std_logic;
	                           endPtr: SmallNumber)
	return ReorderBufferArray; 
	
	function getNextRobContent_Update(content: ReorderBufferArray;
	                           execInfo1, execInfo2: InstructionSlotArray
	                           )
	return ReorderBufferArray;

	function getNextRobContent_ClearDB(content: ReorderBufferArray) --;
	                           --execInfo1, execInfo2: InstructionSlotArray;
	                           --execEvent, lateEventSignal: std_logic;
	                           --startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray;
	
	function getNextRobContent(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           newSpecialAction: InstructionSlot;
	                           execInfo1, execInfo2: InstructionSlotArray;
	                           sends, receiving, execEvent, lateEventSignal: std_logic;
	                           startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray;
	
	function replaceConstantInformation(insVec: InstructionSlotArray; constInfo, constInfo2, constInfo3: Word) return InstructionSlotArray;	
	function replaceConstantInformationSpecial(special: InstructionSlot; constInfo: Word) return InstructionSlot; 

   
end package;



package body LogicROB is

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

-- startP is inclusive; endP is exclusive: first empty slot
-- CAREFUL: if startP = endP, it may be either empty or full. Hence the need of 'full' flag
function getMaskBetween(constant LEN: natural; startP, endP: SmallNumber; full: std_logic) return std_logic_vector is
    variable res: std_logic_vector(0 to LEN-1) := (others => '0');
    variable iv: SmallNumber := (others => '0');
begin        
    -- true if either:
    --      when full, '1'; otherwise when end = start then nothing is between
    --      i >= start && i < end && end > start
    --      (i >= start || i < end) && end < start
    for i in 0 to LEN-1 loop
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

function updateOpGroup(ops: InstructionSlotArray; execResult: InstructionSlot; constant NUM: natural; constant ALLOW_MEM: boolean)
return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to ops'length-1) := ops;
    variable il: SmallNumber := getTagLowSN(execResult.ins.tags.renameIndex);
    constant ind: natural := slv2u(il);
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

        if ind = k then
            if execResult.ins.controlInfo.confirmedBranch = '1' then
                res(k).ins.controlInfo.confirmedBranch := '1';
            end if;
                
            if execResult.ins.controlInfo.newEvent = '1' then -- CAREFUL: branches corrected to not taken need this!
                res(k).ins.controlInfo.causing := '1';
                eventFound := true;                   
            end if;

            if execResult.ins.controlInfo.hasException = '1' then
                res(k).ins.controlInfo.hasException := '1';
                res(k).ins.controlInfo.causing := '1';                
                eventFound := true;
            end if;

            -- Only if this is Memory subpipe:
            if ALLOW_MEM and execResult.ins.controlInfo.specialAction = '1' then -- TODO: remove it, not handled by Exec engine/
                res(k).ins.controlInfo.specialAction := '1';
                res(k).ins.controlInfo.refetch := '1';
                res(k).ins.controlInfo.causing := '1';
                eventFound := true;
            end if;
                               
        end if;
    end loop;
    
    return res;
end function;



function updateOpGroupCompleted(ops: InstructionSlotArray; execResult: InstructionSlot; constant NUM: natural; constant ALLOW_MEM: boolean)
return InstructionSlotArray is
    variable res: InstructionSlotArray(0 to ops'length-1) := ops;
    variable il: SmallNumber := getTagLowSN(execResult.ins.tags.renameIndex);
    constant ind: natural := slv2u(il);
    variable eventFound: boolean := false;
begin
    if execResult.full = '0' then
        return res;
    end if;
    
    if NUM = 1 then
        res(ind).ins.controlInfo.completed2 := '1';
    else
        res(ind).ins.controlInfo.completed := '1';
    end if;                                
    
    return res;
end function;



	function getNextRobContent_Input(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           newSpecialAction: InstructionSlot;
	                           receiving: std_logic;
	                           endPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	begin
	   
	   -- Input
	   if receiving = '1' then
	       res(slv2u(endPtr)).ops := newGroup;
	       res(slv2u(endPtr)).special := newSpecialAction;  
	   end if;
	   
	   return res;
	end function; 
	
	function getNextRobContent_Update(content: ReorderBufferArray;
	                           execInfo1, execInfo2: InstructionSlotArray
	                           )
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable ptr1, ptr2,  iv: SmallNumber := (others => '0');
	   variable newInsState: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
	   
	   -- Update
       for j in 0 to 3 loop
	   
	       
	       -- Update group!
	       -- NOTE: tag comparison for slot will be simplified because tag bits directly show ROB slot:
	       --          [ upper bits | ROB slot | index in group]
	       --            (getTagHighSN([tag]) and PTR_MASK) == iv ?
	       --             getTagLowSN == [group index] ? // this is within updateOpGroup  
    	   for i in 0 to ROB_SIZE-1 loop
    	       iv := i2slv(i, SMALL_NUMBER_SIZE);
	       
	           ptr1 := getTagHighSN(execInfo1(j).ins.tags.renameIndex) and PTR_MASK_SN;
	           if ptr1 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe	               
	           end if;
	           
               ptr2 := getTagHighSN(execInfo2(j).ins.tags.renameIndex) and PTR_MASK_SN;	           
	           if ptr2 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo2(j), 1, false);
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo2(j), 1, false);	               
	           end if;
	       end loop;

	   end loop;
        
       return res;
    end function;

	function getNextRobContent_ClearDB(content: ReorderBufferArray) --;
	                           --execInfo1, execInfo2: InstructionSlotArray;
	                           --execEvent, lateEventSignal: std_logic;
	                           --startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable ptr1, ptr2,  iv: SmallNumber := (others => '0');
	   variable newInsState: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
       -- Clear unused fields for better synthesis
       if CLEAR_DEBUG_INFO then
           for j in 0 to ROB_SIZE-1 loop
               for i in 0 to PIPE_WIDTH-1 loop
                   -- controlInfo survives here, other useful data is stored in separate mem array because the rest is immutable
                   newInsState := res(j).ops(i).ins;
                   res(j).ops(i).ins := DEFAULT_INSTRUCTION_STATE;
                   res(j).ops(i).ins.controlInfo := newInsState.controlInfo;
                        -- CAREFUL: info aobut stores needed for StoreQueue
                        res(j).ops(i).ins.classInfo.secCluster := newInsState.classInfo.secCluster;
                        res(j).ops(i).ins.classInfo.useLQ := newInsState.classInfo.useLQ;
               end loop;

                   newInsState := res(j).special.ins;
                   res(j).special.ins.controlInfo := newInsState.controlInfo;
                   res(j).special.ins.specificOperation := newInsState.specificOperation;                          
           end loop;
       end if;
	   
	   return res;
	end function;
	
	function getNextRobContent(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           newSpecialAction: InstructionSlot;
	                           execInfo1, execInfo2: InstructionSlotArray;
	                           sends, receiving, execEvent, lateEventSignal: std_logic;
	                           startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable ptr1, ptr2,  iv: SmallNumber := (others => '0');
	   variable newInsState: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
	   
	   -- Input
	   if receiving = '1' then
	       res(slv2u(endPtr)).ops := newGroup;
	       res(slv2u(endPtr)).special := newSpecialAction;  
	   end if;
	   
	   -- Update
	   for i in 0 to ROB_SIZE-1 loop
	       iv := i2slv(i, SMALL_NUMBER_SIZE);
	       
	       -- Update group!
	       -- NOTE: tag comparison for slot will be simplified because tag bits directly show ROB slot:
	       --          [ upper bits | ROB slot | index in group]
	       --            (getTagHighSN([tag]) and PTR_MASK) == iv ?
	       --             getTagLowSN == [group index] ? // this is within updateOpGroup  
	       for j in 0 to 3 loop
	           ptr1 := getTagHighSN(execInfo1(j).ins.tags.renameIndex) and PTR_MASK_SN;
	           if ptr1 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe	               
	           end if;
	           
               ptr2 := getTagHighSN(execInfo2(j).ins.tags.renameIndex) and PTR_MASK_SN;	           
	           if ptr2 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo2(j), 1, false);
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo2(j), 1, false);	               
	           end if;
	       end loop;

	   end loop;

       -- Clear unused fields for better synthesis
       if CLEAR_DEBUG_INFO then
           for j in 0 to ROB_SIZE-1 loop
               for i in 0 to PIPE_WIDTH-1 loop
                   -- controlInfo survives here, other useful data is stored in separate mem array because the rest is immutable
                   newInsState := res(j).ops(i).ins;
                   res(j).ops(i).ins := DEFAULT_INSTRUCTION_STATE;
                   res(j).ops(i).ins.controlInfo := newInsState.controlInfo;
                        -- CAREFUL: info aobut stores needed for StoreQueue
                        res(j).ops(i).ins.classInfo.secCluster := newInsState.classInfo.secCluster;
                        res(j).ops(i).ins.classInfo.useLQ := newInsState.classInfo.useLQ;
                        res(j).ops(i).ins.classInfo.branchIns := newInsState.classInfo.branchIns;
               end loop;

                   newInsState := res(j).special.ins;
                   res(j).special.ins.controlInfo := newInsState.controlInfo;
                   res(j).special.ins.specificOperation := newInsState.specificOperation;                          
           end loop;
       end if;
	   
	   return res;
	end function;
	
	function replaceConstantInformation(insVec: InstructionSlotArray; constInfo, constInfo2, constInfo3: Word) return InstructionSlotArray is
	   variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
	begin
	   if CLEAR_DEBUG_INFO then
           for i in 0 to PIPE_WIDTH-1 loop
	           res(i).ins := DEFAULT_INSTRUCTION_STATE;
	           res(i).ins.controlInfo := insVec(i).ins.controlInfo;
	               res(i).ins.classInfo.secCluster := insVec(i).ins.classInfo.secCluster;
	               res(i).ins.classInfo.useLQ := insVec(i).ins.classInfo.useLQ;
	               res(i).ins.classInfo.branchIns := insVec(i).ins.classInfo.branchIns;
	       end loop;       
	   end if;
	
	   for i in 0 to PIPE_WIDTH-1 loop
	       res(i).ins.physicalArgSpec.dest := constInfo(8*i + 7 downto 8*i);
	       res(i).ins.virtualArgSpec.dest := "000" & constInfo2(5*i + 4 downto 5*i);
	       
	       res(i).ins.virtualArgSpec.intDestSel := constInfo3(4 + i);
	       res(i).ins.virtualArgSpec.floatDestSel := constInfo3(0 + i);
	       
	       --res(i).ins.physicalArgSpec.intDestSel := constInfo3(4 + i);
	       --res(i).ins.physicalArgSpec.floatDestSel := constInfo3(0 + i);
	              
	   end loop;
	   
	   return res;
	end function;
	
	function replaceConstantInformationSpecial(special: InstructionSlot; constInfo: Word) return InstructionSlot is
	   variable res: InstructionSlot := special;
	   variable num: natural := 0;
	begin
	   num := slv2u(constInfo(SYS_OP_SIZE - 1 + 16 downto 16));	   
	   
	   res.ins.specificOperation.subpipe := None;
	   res.ins.specificOperation.system := SysOp'val(num); 
	   
	   if CLEAR_DEBUG_INFO then
	       res.ins := DEFAULT_INS_STATE;
	       res.ins.controlInfo := special.ins.controlInfo;
	       res.ins.specificOperation.subpipe := None;
	       res.ins.specificOperation.system := SysOp'val(num);
	           res.ins.specificOperation.bits := --sop(None, res.ins.specificOperation.system).bits;
	                                              constInfo(SYS_OP_SIZE - 1 + 16 downto 16); 
	   end if;
	   
	   return res;
	end function;

    
end package body;
