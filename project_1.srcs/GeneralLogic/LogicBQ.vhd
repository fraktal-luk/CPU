
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.CoreConfig.all;

use work.PipelineGeneral.all;


package LogicBQ is

type EarlyInfo is record
   full: std_logic_vector(0 to PIPE_WIDTH-1);
   frontBranch: std_logic_vector(0 to PIPE_WIDTH-1);
   confirmedBranch: std_logic_vector(0 to PIPE_WIDTH-1);

   ip: Mword;
   targets: MwordArray(0 to PIPE_WIDTH-1);
   links: MwordArray(0 to PIPE_WIDTH-1);
end record;

constant DEFAULT_EARLY_INFO: EarlyInfo := ( targets => (others => (others => '0')),
                                            links => (others => (others => '0')),
                                            others => (others => '0'));

type EarlyInfoArray is array (natural range <>) of EarlyInfo;

type LateInfo is record
   intPtr: SmallNumber;
   floatPtr: SmallNumber;
   sqPtr: SmallNumber;
   lqPtr: SmallNumber;
   usingInt: std_logic_vector(0 to PIPE_WIDTH-2);
   usingFloat: std_logic_vector(0 to PIPE_WIDTH-2);
   usingSQ: std_logic_vector(0 to PIPE_WIDTH-2);
   usingLQ: std_logic_vector(0 to PIPE_WIDTH-2);
end record;

constant DEFAULT_LATE_INFO: LateInfo := (others => (others => '0'));

constant EARLY_ELEM_SIZE: natural := 67;
constant EARLY_INFO_SIZE: natural := EARLY_ELEM_SIZE*PIPE_WIDTH + 32;
constant LATE_INFO_SIZE: natural := 4*8 + 4*(PIPE_WIDTH-1);

type LateInfoArray is array (natural range <>) of LateInfo;
type EarlyInfoSerialArray is array(0 to BQ_SIZE-1) of std_logic_vector(EARLY_INFO_SIZE-1 downto 0);
type LateInfoSerialArray is array(0 to BQ_SIZE-1) of std_logic_vector(LATE_INFO_SIZE-1 downto 0);

function getEarlyInfo(cpa: ControlPacketArray) return EarlyInfo;
function getLateInfo(insVec: InstructionSlotArray) return LateInfo;

function serializeEarlyInfo(info: EarlyInfo) return std_logic_vector;
function serializeLateInfo(info: LateInfo) return std_logic_vector;

function deserializeEarlyInfo(v: std_logic_vector) return EarlyInfo;
function deserializeLateInfo(v: std_logic_vector) return LateInfo;

function getMatchedSlot(full: std_logic; renameIndex: InsTag; earlySelected: EarlyInfo; lateSelected: LateInfo) return ControlPacket;

end package;


package body LogicBQ is

function getEarlyInfo(cpa: ControlPacketArray) return EarlyInfo is
    variable res: EarlyInfo;
begin
    for i in cpa'range loop
        res.full(i) := cpa(i).controlInfo.full;
        res.frontBranch(i) := cpa(i).controlInfo.frontBranch;
        res.confirmedBranch(i) := cpa(i).controlInfo.confirmedBranch;

        res.targets(i) := cpa(i).target;
        res.links(i) := cpa(i).nip;
    end loop;

    res.ip := cpa(0).ip;
    return res;
end function;

function getLateInfo(insVec: InstructionSlotArray) return LateInfo is
    variable res: LateInfo;
begin
    res.intPtr := insVec(0).ins.tags.intPointer;
    res.floatPtr := insVec(0).ins.tags.floatPointer;
    res.sqPtr := insVec(0).ins.tags.sqPointer;
    res.lqPtr := insVec(0).ins.tags.lqPointer;

    for i in 1 to insVec'length - 1 loop
        res.usingInt(i-1) := insVec(i).ins.tags.intPointer(0) xor insVec(i-1).ins.tags.intPointer(0);
        res.usingFloat(i-1) := insVec(i).ins.tags.floatPointer(0) xor insVec(i-1).ins.tags.floatPointer(0);
        res.usingSQ(i-1) := insVec(i).ins.tags.sqPointer(0) xor insVec(i-1).ins.tags.sqPointer(0);
        res.usingLQ(i-1) := insVec(i).ins.tags.lqPointer(0) xor insVec(i-1).ins.tags.lqPointer(0);
    end loop;
    return res;
end function;


function serializeEarlyInfo(info: EarlyInfo) return std_logic_vector is
    variable res: std_logic_vector(EARLY_INFO_SIZE-1 downto 0);
    variable tmp: std_logic_vector(EARLY_ELEM_SIZE-1 downto 0);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        tmp(31 downto 0) := info.targets(i);
        tmp(63 downto 32) := info.links(i);
        tmp(64) := info.full(i);
        tmp(65) := info.frontBranch(i);
        tmp(66) := info.confirmedBranch(i);
        
        res(67*i + 66 downto 67*i) := tmp(66 downto 0);
    end loop;

    res(EARLY_ELEM_SIZE*PIPE_WIDTH + 31 downto EARLY_ELEM_SIZE*PIPE_WIDTH) := info.ip;

    return res;
end function;

function serializeLateInfo(info: LateInfo) return std_logic_vector is
    variable res: std_logic_vector(LATE_INFO_SIZE-1 downto 0);
begin
    res(31 downto 0) := info.lqPtr & info.sqPtr & info.floatPtr & info.intPtr;    
    res(LATE_INFO_SIZE-1 downto 32) := info.usingLQ & info.usingSQ & info.usingFloat & info.usingInt;
    
    return res;
end function;

function deserializeEarlyInfo(v: std_logic_vector) return EarlyInfo is
    variable res: EarlyInfo;
    variable tmp: std_logic_vector(EARLY_ELEM_SIZE-1 downto 0);
begin
    for i in 0 to PIPE_WIDTH-1 loop
        tmp := v(67*i + 66 downto 67*i);

        res.targets(i) := tmp(31 downto 0);
        res.links(i) := tmp(63 downto 32);
        res.full(i) := tmp(64);
        res.frontBranch(i) := tmp(65);
        res.confirmedBranch(i) := tmp(66);    
    end loop;

    res.ip := v(EARLY_ELEM_SIZE*PIPE_WIDTH + 31 downto EARLY_ELEM_SIZE*PIPE_WIDTH);
    return res;
end function;

function deserializeLateInfo(v: std_logic_vector) return LateInfo is
    variable res: LateInfo;
begin
    res.lqPtr := v(31 downto 24);
    res.sqPtr := v(23 downto 16);
    res.floatPtr := v(15 downto 8);
    res.intPtr := v(7 downto 0);

    res.usingLQ :=    v(4*(PIPE_WIDTH-1)-1 + 32 downto 3*(PIPE_WIDTH-1) + 32);
    res.usingSQ :=    v(3*(PIPE_WIDTH-1)-1 + 32 downto 2*(PIPE_WIDTH-1) + 32);
    res.usingFloat := v(2*(PIPE_WIDTH-1)-1 + 32 downto 1*(PIPE_WIDTH-1) + 32);
    res.usingInt :=   v(  (PIPE_WIDTH-1)-1 + 32 downto                    32);

    return res;
end function;


function getMatchedSlot(full: std_logic; renameIndex: InsTag; earlySelected: EarlyInfo; lateSelected: LateInfo) return ControlPacket is
    variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
    constant ipBase: Mword := earlySelected.ip;
    constant trgs: MwordArray := earlySelected.targets;
    constant ress: MwordArray := earlySelected.links;
    variable intBase, floatBase, sqBase, lqBase: SmallNumber;
    variable useVecInt, useVecFloat, useVecSQ, useVecLQ: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
    variable lowPtr: natural := 0;
    variable resLow: Mword := (others => '0');
begin
    intBase := lateSelected.intPtr;
    floatBase := lateSelected.floatPtr;
    sqBase := lateSelected.sqPtr;
    lqBase := lateSelected.lqPtr;

    useVecInt(1 to PIPE_WIDTH-1) := lateSelected.usingInt;
    useVecFloat(1 to PIPE_WIDTH-1) := lateSelected.usingFloat;
    useVecSQ(1 to PIPE_WIDTH-1) := lateSelected.usingSQ;
    useVecLQ(1 to PIPE_WIDTH-1) := lateSelected.usingLQ;

    lowPtr := slv2u(getTagLow(renameIndex));

    res.controlInfo.frontBranch := earlySelected.frontBranch(lowPtr);
    res.controlInfo.confirmedBranch := earlySelected.confirmedBranch(lowPtr);
 
    res.controlInfo.full := full;

    res.nip := ress(lowPtr);
    res.target := trgs(lowPtr);

    if lowPtr = 0 then
        res.tags.intPointer := intBase;
        res.tags.floatPointer := floatBase;
        res.tags.sqPointer := sqBase;                 
        res.tags.lqPointer := lqBase;                 
    else
        res.tags.intPointer := addInt(intBase, countOnes(useVecInt(1 to lowPtr)));
        res.tags.floatPointer := addInt(floatBase, countOnes(useVecFloat(1 to lowPtr)));                                 
        res.tags.sqPointer := addInt(sqBase, countOnes(useVecSQ(1 to lowPtr)));                                 
        res.tags.lqPointer := addInt(lqBase, countOnes(useVecLQ(1 to lowPtr)));                                 
    end if;

   return res;
end function;

end package body; 
