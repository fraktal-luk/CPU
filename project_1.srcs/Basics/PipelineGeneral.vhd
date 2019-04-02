----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 12.11.2018 20:20:51
-- Design Name: 
-- Module Name: PipelineGeneral - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;


package PipelineGeneral is


type ForwardingInfo is record
	tags0: PhysNameArray(0 to 2);
	tags1: PhysNameArray(0 to 2);
	values0: MwordArray(0 to 2);
	values1: MwordArray(0 to 2);	
	nextResultTags: PhysNameArray(0 to 2);
	nextTagsM2:	PhysNameArray(0 to 2);
end record;

type ForwardingMap is record
    maskRR: std_logic_vector(0 to 2);
    maskR1: std_logic_vector(0 to 2);
    maskR0: std_logic_vector(0 to 2);
    maskM1: std_logic_vector(0 to 2);
    maskM2: std_logic_vector(0 to 2);
end record;


constant DEFAULT_FORWARDING_INFO: ForwardingInfo := (
	--writtenTags => (others => (others => '0')),
		tags0 => (others => (others => '0')),
		tags1 => (others => (others => '0')),
		values0 => (others => (others => '0')),
		values1 => (others => (others => '0')),
		
	--resultTags => (others => (others => '0')),
	nextResultTags => (others => (others => '0')),
	nextTagsM2 => (others => (others => '0'))
	--resultValues => (others => (others => '0'))
);

constant DEFAULT_FORWARDING_MAP: ForwardingMap := (
    others => (others => '0')
);


function compactMask(vec: std_logic_vector) return std_logic_vector;
function getSelector(mr, mi: std_logic_vector(0 to 2)) return std_logic_vector;

function getNewElem(remv: std_logic_vector; newContent: InstructionSlotArray) return InstructionSlot;
function getNewElemSch(remv: std_logic_vector; newContent: SchedulerEntrySlotArray)
return SchedulerEntrySlot;


function setInstructionIP(ins: InstructionState; ip: Mword) return InstructionState;
function setInstructionTarget(ins: InstructionState; target: Mword) return InstructionState;
function setInstructionResult(ins: InstructionState; result: Mword) return InstructionState;

        function getStoredArg1(ins: InstructionState) return Mword;
        function getStoredArg2(ins: InstructionState) return Mword;
        function setStoredArg1(ins: InstructionState; val: Mword) return InstructionState;
        function setStoredArg2(ins: InstructionState; val: Mword) return InstructionState;

function getAddressIncrement(ins: InstructionState) return Mword;

function CMP_tagBefore(tagA, tagB: InsTag) return std_logic;
function CMP_tagAfter(tagA, tagB: InsTag) return std_logic;

function extractFullMask(queueContent: InstructionSlotArray) return std_logic_vector;

function extractFullMask(queueContent: SchedulerEntrySlotArray) return std_logic_vector;

function extractData(queueContent: InstructionSlotArray) return InstructionStateArray;

function extractData(queueContent: SchedulerEntrySlotArray) return InstructionStateArray;

function killByTag(before, ei, int: std_logic) return std_logic;

function getKillMask(content: InstructionStateArray; fullMask: std_logic_vector;
							causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector;

function stageArrayNext(livingContent, newContent: InstructionSlotArray; full, sending, receiving, kill: std_logic)
return InstructionSlotArray;


function getTagHigh(tag: std_logic_vector) return std_logic_vector;
function getTagLow(tag: std_logic_vector) return std_logic_vector;
function getTagHighSN(tag: InsTag) return SmallNumber;
function getTagLowSN(tag: InsTag) return SmallNumber;	
function clearTagLow(tag: std_logic_vector) return std_logic_vector;	
function clearTagHigh(tag: std_logic_vector) return std_logic_vector;	
function alignAddress(adr: std_logic_vector) return std_logic_vector;
function clearLowBits(vec: std_logic_vector; n: integer) return std_logic_vector;
function getLowBits(vec: std_logic_vector; n: integer) return std_logic_vector;

function getExceptionMask(insVec: InstructionSlotArray) return std_logic_vector;

function getSchedData(insArr: InstructionStateArray; fullMask: std_logic_vector) return SchedulerEntrySlotArray;

function getBranchMask(insVec: InstructionSlotArray) return std_logic_vector;
function getLoadMask(insVec: InstructionSlotArray) return std_logic_vector;
function getStoreMask(insVec: InstructionSlotArray) return std_logic_vector;
function getAluMask(insVec: InstructionSlotArray) return std_logic_vector;

function setFullMask(insVec: InstructionSlotArray; mask: std_logic_vector) return InstructionSlotArray;

function prepareForStoreValueIQ(insVec: InstructionStateArray) return InstructionStateArray;
--function prepareForStoreValueIntFloatIQ(insVecInt, insVecFloat: InstructionStateArray) return InstructionStateArray;
function prepareForStoreValueFloatIQ(insVecInt, insVecFloat: InstructionStateArray) return InstructionStateArray;


function removeArg2(insVec: InstructionStateArray) return InstructionStateArray;


        
        constant WAITING_FN_MAP: ForwardingMap := (
            maskRR => "110",   -- arg2 is unused   
            maskR1 => "000",  
            maskR0 => "000",
            maskM1 => "101",
            maskM2 => "010"
        );        

        constant ENQUEUE_FN_MAP: ForwardingMap := (
            maskRR => "000",      
            maskR1 => "111",  
            maskR0 => "111",
            maskM1 => "111",
            maskM2 => "010"
        );

        constant SELECTION_FN_MAP: ForwardingMap := (
            maskRR => "110",   -- arg2 is unused   
            maskR1 => "000",  
            maskR0 => "000",
            maskM1 => "101",
            maskM2 => "000"
        );
        
        constant ENQUEUE_FN_MAP_SV: ForwardingMap := (
            maskRR => "000",      
            maskR1 => "111",  
            maskR0 => "111",
            maskM1 => "000",
            maskM2 => "000"
        );

        constant WAITING_FN_MAP_SV: ForwardingMap := (
            maskRR => "100",
            maskR1 => "000",  
            maskR0 => "111",
            maskM1 => "000",
            maskM2 => "000"
        );


        -- FP store data
        constant ENQUEUE_FN_MAP_FLOAT_SV: ForwardingMap := (
            maskRR => "000",      
            maskR1 => "111",  
            maskR0 => "111",
            maskM1 => "000",
            maskM2 => "000"
        );
        
        constant WAITING_FN_MAP_FLOAT_SV: ForwardingMap := (
            maskRR => "100",   -- arg2 is unused   
            maskR1 => "000",  
            maskR0 => "111",
            maskM1 => "000",
            maskM2 => "000"
        );
  
end package;



package body PipelineGeneral is

function compactMask(vec: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to 3) := (others => '0');
    variable vec4: std_logic_vector(0 to 3) := vec;
begin
    case vec4 is
        when "0000" =>
            res := "0000";
        when "1111" =>
            res := "1111";
        when "1000" | "0100" | "0010" | "0001" =>
            res := "1000";
        when "0111" | "1011" | "1101" | "1110" =>
            res := "1110";
        when others =>
            res := "1100";
    end case;
    
    return res;
end function;

function getSelector(mr, mi: std_logic_vector(0 to 2)) return std_logic_vector is
    variable res: std_logic_vector(1 downto 0) := "00";
    variable m6: std_logic_vector(0 to 5) := mr & mi;
    variable n0: integer := 0;
begin
    n0 := 6 - countOnes(m6);
    case m6 is
        --when "111000" =>
        --    res := "11";
        when "111001" => 
            res := "10";
        when "111010" | "111011" => 
            res := "01";
        when "111100" | "111101" | "111110" | "111111" => 
            res := "00";
 
        --when "110000" => 
        --    res := "11";
        --when "110001" | "110010" => 
        --    res := "11";
        when "110011" | "110101" => 
            res := "10";
        when "110110" | "110111" =>
            res := "01";

        when "100111" => 
            res := "10";

        when others =>
            res := "11";
    end case;
    return res;
end function;

function getNewElem(remv: std_logic_vector; newContent: InstructionSlotArray) return InstructionSlot is
    variable res: InstructionSlot := newContent(0);
    variable inputMask: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
    variable sel: std_logic_vector(1 downto 0) := "00";
    variable remVec: std_logic_vector(0 to 2) := remv;               
begin
    inputMask := extractFullMask(newContent);
    sel := getSelector(remVec, inputMask(0 to 2));
    res := newContent(slv2u(sel));        
    return res;    
end function;

function getNewElemSch(remv: std_logic_vector; newContent: SchedulerEntrySlotArray)
return SchedulerEntrySlot is
    variable res: SchedulerEntrySlot := newContent(0);
    variable inputMask: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
    variable sel: std_logic_vector(1 downto 0) := "00";
    variable remVec: std_logic_vector(0 to 2) := remv;               
begin
    inputMask := extractFullMask(newContent);
    sel := getSelector(remVec, inputMask(0 to 2));
    res := newContent(slv2u(sel));        
    return res;    
end function;

function setInstructionIP(ins: InstructionState; ip: Mword) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.ip := ip;
	return res;
end function;

function setInstructionTarget(ins: InstructionState; target: Mword) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.target := target;
	return res;
end function;

function setInstructionResult(ins: InstructionState; result: Mword) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.result := result;
	return res;
end function;

function getAddressIncrement(ins: InstructionState) return Mword is
	variable res: Mword := (others => '0');
begin
	if ins.classInfo.short = '1' then
		res(1) := '1'; -- 2
	else
		res(2) := '1'; -- 4
	end if;
	return res;
end function;


function CMP_tagBefore(tagA, tagB: InsTag) return std_logic is
	variable wA, wB: word := (others => '0');
	variable wC: std_logic_vector(32 downto 0) := (others => '0');
begin
	wA(TAG_SIZE-1 downto 0) := tagA;
	wB(TAG_SIZE-1 downto 0) := tagB;
	wB := not wB;
	-- TODO: when going to 64 bit, this must be changed!
	wC := addMwordFasterExt(wA, wB, '1');
	wC(32 downto TAG_SIZE) := (others => '0');
	return wC(TAG_SIZE-1);
end function;

function CMP_tagAfter(tagA, tagB: InsTag) return std_logic is
	variable wA, wB, wC: word := (others => '0');
begin
	return CMP_tagBefore(tagB, tagA);
end function;

function extractFullMask(queueContent: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to queueContent'length-1) := (others => '0');
begin
	for i in res'range loop
		res(i) := queueContent(i).full;
	end loop;
	return res;
end function;

function extractFullMask(queueContent: SchedulerEntrySlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to queueContent'length-1) := (others => '0');
begin
	for i in res'range loop
		res(i) := queueContent(i).full;
	end loop;
	return res;
end function;

function extractData(queueContent: InstructionSlotArray) return InstructionStateArray is
	variable res: InstructionStateArray(0 to queueContent'length-1) := (others => defaultInstructionState);
begin
	for i in res'range loop
		res(i) := queueContent(i).ins;
	end loop;
	return res;
end function;


function extractData(queueContent: SchedulerEntrySlotArray) return InstructionStateArray is
	variable res: InstructionStateArray(0 to queueContent'length-1) := (others => defaultInstructionState);
begin
	for i in res'range loop
		res(i) := queueContent(i).ins;
	end loop;
	return res;
end function;

	function killByTag(before, ei, int: std_logic) return std_logic is
	begin
		return (before and ei) or int;
	end function;

function getKillMask(content: InstructionStateArray; fullMask: std_logic_vector;
							causing: InstructionState; execEventSig: std_logic; lateEventSig: std_logic)
return std_logic_vector is
	variable res: std_logic_vector(0 to fullMask'length-1);
	variable diff: SmallNumber := (others => '0');
begin
	for i in 0 to fullMask'length-1 loop
		res(i) := killByTag(CMP_tagBefore(causing.tags.renameIndex, content(i).tags.renameIndex),
									execEventSig, lateEventSig) and fullMask(i);
	end loop;
	return res;
end function;

function stageArrayNext(livingContent, newContent: InstructionSlotArray; full, sending, receiving, kill: std_logic)
return InstructionSlotArray is 
    constant LEN: natural := livingContent'length;
	variable res: InstructionSlotArray(0 to LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	--	constant CLEAR_VACATED_SLOTS_GENERAL: boolean := clearEmptySlots; 
begin
	res := livingContent;
	if kill = '1' then
		for i in 0 to LEN-1 loop
			res(i).full := '0';
		end loop;
	end if;
		
	if receiving = '1' then -- take full
		res := newContent;
	elsif sending = '1' or full = '0' then -- take empty
		-- CAREFUL: clearing result tags for empty slots
		for i in 0 to LEN-1 loop
			res(i).ins.physicalArgSpec.dest := (others => '0');
			res(i).ins.controlInfo.newEvent := '0';
		end loop;
		for i in 0 to LEN-1 loop
			res(i).full := '0';
		end loop;
	end if;			
			
	return res;
end function;


        function getStoredArg1(ins: InstructionState) return Mword is
        begin
            return ins.result;
        end function;
        
        function getStoredArg2(ins: InstructionState) return Mword is
        begin
            return ins.target;
        end function;
        
        function setStoredArg1(ins: InstructionState; val: Mword) return InstructionState is
            variable res: InstructionState := ins;
        begin
            res.result := val;
            return res;
        end function;
        
        function setStoredArg2(ins: InstructionState; val: Mword) return InstructionState is
            variable res: InstructionState := ins;
        begin
            res.target := val;
            return res;
        end function;
        
        
        

function getTagHigh(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(tag'high-LOG2_PIPE_WIDTH downto 0) := (others => '0');
begin
	res := tag(tag'high downto LOG2_PIPE_WIDTH);
	return res;
end function;

function getTagLow(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(LOG2_PIPE_WIDTH-1 downto 0) := (others => '0');
begin
	res := tag(LOG2_PIPE_WIDTH-1 downto 0);
	return res;
end function;

function getTagHighSN(tag: InsTag) return SmallNumber is
	variable res: SmallNumber := (others => '0');
begin
	res(TAG_SIZE-1-LOG2_PIPE_WIDTH downto 0) := tag(TAG_SIZE-1 downto LOG2_PIPE_WIDTH);
	return res;
end function;

function getTagLowSN(tag: InsTag) return SmallNumber is
	variable res: SmallNumber := (others => '0');
begin
	res(LOG2_PIPE_WIDTH-1 downto 0) := tag(LOG2_PIPE_WIDTH-1 downto 0);
	return res;
end function;


function clearTagLow(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(tag'high downto 0) := (others => '0');
begin
	res := tag;
	res(LOG2_PIPE_WIDTH-1 downto 0) := (others => '0');
	return res;
end function;	

function clearTagHigh(tag: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(tag'high downto 0) := (others => '0');
begin
	res := tag;
	res(tag'high downto LOG2_PIPE_WIDTH) := (others => '0');
	return res;
end function;

function alignAddress(adr: std_logic_vector) return std_logic_vector is
	variable res: std_logic_vector(adr'high downto 0) := (others => '0');
begin
	res := adr;
	res(ALIGN_BITS-1 downto 0) := (others => '0');
	return res;
end function;

function clearLowBits(vec: std_logic_vector; n: integer) return std_logic_vector is
	variable res: std_logic_vector(vec'high downto 0) := (others => '0');
begin
	res := vec;
	res(n-1 downto 0) := (others => '0');
	return res;
end function;

function getLowBits(vec: std_logic_vector; n: integer) return std_logic_vector is
	variable res: std_logic_vector(n-1 downto 0) := (others => '0');
begin
	res(n-1 downto 0) := vec(n-1 downto 0);
	return res;
end function;


function getExceptionMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(insVec'range) := (others=>'0');
begin
	for i in insVec'range loop
		res(i) := insVec(i).ins.controlInfo.hasException
		          or insVec(i).ins.controlInfo.specialAction; -- CAREFUL: what if special actions are allowed to write registers?
	end loop;			
	return res;
end function;

	function getSchedData(insArr: InstructionStateArray; fullMask: std_logic_vector) return SchedulerEntrySlotArray is
		variable res: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCH_ENTRY_SLOT);
	begin
		for i in 0 to PIPE_WIDTH-1 loop
			res(i).ins := insArr(i);
			res(i).full := fullMask(i);
            
            -- CAREFUL, UNNEEDED
            res(i).state.argValues.origSlot := i2slv(i, 2); -- So we know which 'readyRegs' slots to use in IQ!
            
            -- CAREFUL, TODO: define precisely what 'zero' designation means
			-- Set state markers: "zero" bit; only valid for Int args because FP doesn't use HW zero 
			res(i).state.argValues.zero(0) := (res(i).ins.physicalArgSpec.intArgSel(0) and not isNonzero(res(i).ins.virtualArgSpec.args(0)(4 downto 0)))
			                                     or (not res(i).ins.physicalArgSpec.intArgSel(0) and not res(i).ins.physicalArgSpec.floatArgSel(0));
			res(i).state.argValues.zero(1) := (res(i).ins.physicalArgSpec.intArgSel(1) and not isNonzero(res(i).ins.virtualArgSpec.args(1)(4 downto 0)))
			                                     or (not res(i).ins.physicalArgSpec.intArgSel(1) and not res(i).ins.physicalArgSpec.floatArgSel(1));
			res(i).state.argValues.zero(2) := (res(i).ins.physicalArgSpec.intArgSel(2) and not isNonzero(res(i).ins.virtualArgSpec.args(2)(4 downto 0)))
			                                     or (not res(i).ins.physicalArgSpec.intArgSel(2) and not res(i).ins.physicalArgSpec.floatArgSel(2));

			-- Set 'missing' flags for non-const arguments
			res(i).state.argValues.missing := (res(i).ins.physicalArgSpec.intArgSel and not res(i).state.argValues.zero)
			                               or (res(i).ins.physicalArgSpec.floatArgSel);
			
			-- Handle possible immediate arg
			if res(i).ins.constantArgs.immSel = '1' then
				res(i).state.argValues.missing(1) := '0';
				res(i).state.argValues.immediate := '1';
				res(i).state.argValues.zero(1) := '0';
			end if;

			res(i).ins.ip := (others => '0');			
			res(i).ins.target := (others => '0');
			res(i).ins.result := (others => '0');
			res(i).ins.bits := (others => '0');

		end loop;
		return res;
	end function;

function getBranchMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 		insVec(i).full = '1' and insVec(i).ins.controlInfo.skipped = '0'
			and 	insVec(i).ins.classInfo.branchIns = '1'
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;

function getLoadMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 		insVec(i).full = '1' and insVec(i).ins.controlInfo.skipped = '0'
			and (insVec(i).ins.operation = (Memory, load) or insVec(i).ins.operation = (System, sysMfc))
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;

function getStoreMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 		insVec(i).full = '1' and insVec(i).ins.controlInfo.skipped = '0'
			and (insVec(i).ins.operation = (Memory, store) or insVec(i).ins.operation = (System, sysMtc))
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;

function getAluMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 		insVec(i).full = '1' and insVec(i).ins.controlInfo.skipped = '0'
			and (insVec(i).ins.operation.unit = Alu or insVec(i).ins.operation.unit = Jump)
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;

function setFullMask(insVec: InstructionSlotArray; mask: std_logic_vector) return InstructionSlotArray is
    variable res: InstructionSlotArray(insVec'range) := insVec;
begin
    for i in res'range loop
        res(i).full := mask(i);
    end loop;
    
    return res;
end function;

        function prepareForStoreValueIQ(insVec: InstructionStateArray) return InstructionStateArray is
            variable res: InstructionStateArray(0 to PIPE_WIDTH-1) := insVec;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i).constantArgs.immSel := '0';
                
                res(i).virtualArgSpec.intDestSel := '0';
                    res(i).virtualArgSpec.floatDestSel := '0';                
                
                res(i).virtualArgSpec.intArgSel(0) := res(i).virtualArgSpec.intArgSel(2);
                res(i).virtualArgSpec.intArgSel(2) := '0';                
                    res(i).virtualArgSpec.floatArgSel(0) := '0';--res(i).virtualArgSpec.floatArgSel(2);
                    res(i).virtualArgSpec.floatArgSel(1) := '0';                                    
                    res(i).virtualArgSpec.floatArgSel(2) := '0';                
                
                res(i).virtualArgSpec.args(0) := res(i).virtualArgSpec.args(2);
                res(i).virtualArgSpec.args(2) := (others => '0');


                --res(i).constantArgs.immSel := '0';
                
                res(i).physicalArgSpec.intDestSel := '0';
                    res(i).physicalArgSpec.floatDestSel := '0';                
                
                res(i).physicalArgSpec.intArgSel(0) := res(i).physicalArgSpec.intArgSel(2);
                res(i).physicalArgSpec.intArgSel(2) := '0';                
                    res(i).physicalArgSpec.floatArgSel(0) := '0';--res(i).virtualArgSpec.floatArgSel(2);
                    res(i).physicalArgSpec.floatArgSel(1) := '0';                                    
                    res(i).physicalArgSpec.floatArgSel(2) := '0';                
                
                res(i).physicalArgSpec.args(0) := res(i).physicalArgSpec.args(2);
                res(i).physicalArgSpec.args(2) := (others => '0');                                              
            end loop;
            
            return res;
        end function;

--        function prepareForStoreValueIntFloatIQ(insVecInt, insVecFloat: InstructionStateArray) return InstructionStateArray is
--            variable res: InstructionStateArray(0 to PIPE_WIDTH-1) := insVecInt;
--        begin
--            for i in 0 to PIPE_WIDTH-1 loop
--                res(i).constantArgs.immSel := '0';
                
--                res(i).virtualArgSpec.intDestSel := '0';
--                    res(i).virtualArgSpec.floatDestSel := '0';                
                
--                res(i).virtualArgSpec.intArgSel(0) := res(i).virtualArgSpec.intArgSel(2);
--                res(i).virtualArgSpec.intArgSel(2) := '0';                
--                    res(i).virtualArgSpec.floatArgSel(0) := res(i).virtualArgSpec.floatArgSel(2);
--                    res(i).virtualArgSpec.floatArgSel(2) := '0';                
                
--                if res(i).virtualArgSpec.floatArgSel(0) = '1' then
--                    res(i).virtualArgSpec.args(0) := insVecFloat(i).virtualArgSpec.args(2);
--                else
--                    res(i).virtualArgSpec.args(0) := res(i).virtualArgSpec.args(2);
--                end if;
--                res(i).virtualArgSpec.args(2) := (others => '0');
                                              
--            end loop;
            
--            return res;
--        end function;

        
        function prepareForStoreValueFloatIQ(insVecInt, insVecFloat: InstructionStateArray) return InstructionStateArray is
            variable res: InstructionStateArray(0 to PIPE_WIDTH-1) := insVecInt;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i).constantArgs.immSel := '0';
                
                res(i).virtualArgSpec.intDestSel := '0';
                    res(i).virtualArgSpec.floatDestSel := '0';                
                
                res(i).virtualArgSpec.intArgSel(0) := '0';--res(i).virtualArgSpec.intArgSel(2);
                res(i).virtualArgSpec.intArgSel(1) := '0';                
                res(i).virtualArgSpec.intArgSel(2) := '0';                
                    res(i).virtualArgSpec.floatArgSel(0) := res(i).virtualArgSpec.floatArgSel(2);
                    res(i).virtualArgSpec.floatArgSel(2) := '0';                
                
                --if res(i).virtualArgSpec.floatArgSel(0) = '1' then
                    res(i).virtualArgSpec.args(0) := insVecFloat(i).virtualArgSpec.args(2);
                --else
                --    res(i).virtualArgSpec.args(0) := res(i).virtualArgSpec.args(2);
                --end if;
                res(i).virtualArgSpec.args(2) := (others => '0');


                res(i).physicalArgSpec.intDestSel := '0';
                    res(i).physicalArgSpec.floatDestSel := '0';                
                
                res(i).physicalArgSpec.intArgSel(0) := '0';--res(i).virtualArgSpec.intArgSel(2);
                res(i).physicalArgSpec.intArgSel(1) := '0';                
                res(i).physicalArgSpec.intArgSel(2) := '0';                
                    res(i).physicalArgSpec.floatArgSel(0) := res(i).physicalArgSpec.floatArgSel(2);
                    res(i).physicalArgSpec.floatArgSel(2) := '0';                
                
                --if res(i).virtualArgSpec.floatArgSel(0) = '1' then
                    res(i).physicalArgSpec.args(0) := insVecFloat(i).physicalArgSpec.args(2);
                --else
                --    res(i).virtualArgSpec.args(0) := res(i).virtualArgSpec.args(2);
                --end if;
                res(i).physicalArgSpec.args(2) := (others => '0');                                              
            end loop;
            
            return res;
        end function;


        function removeArg2(insVec: InstructionStateArray) return InstructionStateArray is
            variable res: InstructionStateArray(0 to PIPE_WIDTH-1) := insVec;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                res(i).virtualArgSpec.intArgSel(2) := '0';
                res(i).virtualArgSpec.args(2) := (others => '0');
                
                res(i).physicalArgSpec.intArgSel(2) := '0';
                res(i).physicalArgSpec.args(2) := (others => '0');                                                
            end loop;
            
            return res;
        end function;

end package body;
