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

function stageArrayNext(livingContent, newContent: InstructionSlotArray; full, sending, receiving, kill: std_logic)
return InstructionSlotArray;

-- Flow control: input structure
type FlowDriveSimple is record
	lockAccept: std_logic;
	lockSend: std_logic;
	kill: std_logic;
	prevSending: std_logic;
	nextAccepting: std_logic;	
end record;

-- Flow control: output structure
type FlowResponseSimple is record
	accepting: std_logic;
	sending: std_logic;
	isNew: std_logic;
	full: std_logic;
	living: std_logic;	
end record;

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

end package;



package body PipelineGeneral is

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
		res(i) := insVec(i).ins.controlInfo.hasException;
	end loop;			
	return res;
end function;


end package body;
