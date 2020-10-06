
library IEEE;
use IEEE.STD_LOGIC_1164.all;

use std.textio.all;


use work.BasicTypes.all;
use work.ArchDefs.all;

use work.CoreConfig.all;

use work.Assembler.all;

use work.CpuText.all;

   
use work.InstructionState.all;


package Viewing is


type InsPrintFormat is (hex, disasm, physDisasm, tags, control, transfer, status, args);

constant INS_STR_SIZE: natural := 51;
subtype InsString is string(1 to INS_STR_SIZE);
type InsStringArray is array(integer range <>) of InsString;

subtype FetchStageView is InsStringArray(0 to FETCH_WIDTH-1);
subtype GenericStageView is InsStringArray(0 to PIPE_WIDTH-1);


function sprintHex(ins: InstructionState) return string;
function sprintDisasm(ins: InstructionState) return string;
function sprintPhysDisasm(ins: InstructionState) return string;
function sprintTags(ins: InstructionState) return string;
function sprintControl(ins: InstructionState) return string;
function sprintTransfer(ins: InstructionState) return string;
function sprintStatus(ins: InstructionState) return string;


function sprintHex(isl: InstructionSlot) return string;
function sprintDisasm(isl: InstructionSlot) return string;
function sprintPhysDisasm(isl: InstructionSlot) return string;
function sprintTags(isl: InstructionSlot) return string;
function sprintControl(isl: InstructionSlot) return string;
function sprintTransfer(isl: InstructionSlot) return string;
function sprintStatus(isl: InstructionSlot) return string;
function sprintArgs(sl: SchedulerEntrySlot) return string;    
function sprintRobRow(ia: InstructionSlotArray) return InsString;


function getInsString(isl: InstructionSlot) return InsString;
function getInsString(ssl: SchedulerEntrySlot) return InsString;

function getInsString(isl: InstructionSlot; fmt: InsPrintFormat) return InsString;
function getInsString(ssl: SchedulerEntrySlot; fmt: InsPrintFormat) return InsString;


function getInsStringArray(ia: InstructionSlotArray) return InsStringArray;
function getInsStringArray(sa: SchedulerEntrySlotArray) return InsStringArray;

function getInsStringArray(ia: InstructionSlotArray; fmt: InsPrintFormat) return InsStringArray;
function getInsStringArray(sa: SchedulerEntrySlotArray; fmt: InsPrintFormat) return InsStringArray;


--procedure printGroup(ia: InstructionSlotArray; filename: string);

procedure printGroup(ia: InstructionSlotArray; file f: text);
  
end package;


package body Viewing is


function getInsString(isl: InstructionSlot) return InsString is
begin
    return getInsString(isl, hex);
end function;

function getInsString(ssl: SchedulerEntrySlot) return InsString is
begin
    return getInsString(ssl, hex);
end function;

function getInsString(isl: InstructionSlot; fmt: InsPrintFormat) return InsString is
    variable res: InsString := (others => ' ');
begin
    case fmt is
        when hex =>
            res := padLeft(sprintHex(isl), INS_STR_SIZE);        
        when disasm =>
            res := padLeft(sprintDisasm(isl), INS_STR_SIZE);
        when physDisasm =>
            res := padLeft(sprintPhysDisasm(isl), INS_STR_SIZE);
        when tags =>
            res := padLeft(sprintDisasm(isl), INS_STR_SIZE);
        when control =>
            res := padLeft(sprintControl(isl), INS_STR_SIZE);
        when transfer =>
            res := padLeft(sprintTransfer(isl), INS_STR_SIZE);                                                        
        when status =>
            res := padLeft(sprintStatus(isl), INS_STR_SIZE);                                                        
                        
        when others =>
    end case;
    
    return res;
end function;

function getInsString(ssl: SchedulerEntrySlot; fmt: InsPrintFormat) return InsString is
    variable isl: InstructionSlot := (ssl.full, ssl.ins);
begin
    if fmt = args then
        return padLeft(sprintArgs(ssl), INS_STR_SIZE);
    else
        return getInsString(isl, fmt);
    end if;
end function;



function getInsStringArray(ia: InstructionSlotArray; fmt: InsPrintFormat) return InsStringArray is
    variable res: InsStringArray(0 to ia'length-1) := (others => (others => ' '));
begin
    for i in 0 to ia'length-1 loop
        res(i) := getInsString(ia(i), fmt);
    end loop;
    
    return res;
end function;


function getInsStringArray(sa: SchedulerEntrySlotArray; fmt: InsPrintFormat) return InsStringArray is
    variable res: InsStringArray(0 to sa'length-1) := (others => (others => ' '));
begin
    for i in 0 to sa'length-1 loop
        res(i) := getInsString(sa(i), fmt);       
    end loop;
    
    return res;
end function;


function getInsStringArray(ia: InstructionSlotArray) return InsStringArray is
begin  
    return getInsStringArray(ia, hex);
end function;

function getInsStringArray(sa: SchedulerEntrySlotArray) return InsStringArray is
begin
    return getInsStringArray(sa, hex);
end function;


function tag2hex(t: InsTag) return string is
begin
    return slv2hex(t);
end function;

function strExt(str: string; n: positive) return string is 
begin
    return padLeft(str, n);
end function;


function getDestSymbol(asp: InstructionArgSpec) return string is
    variable res: string(1 to 3) := "---";
begin
    if asp.intDestSel = '1' then
        res(1) := 'r';
    elsif asp.floatDestSel = '1' then
        res(1) := 'f';
    else
        res(1) := '*';
    end if;
    res(2 to 3) := slv2hex(asp.dest, 2);   
    return res;
end function;

function getSrcSymbol(asp: InstructionArgSpec; i: natural) return string is
    variable res: string(1 to 3) := "---";
begin
    if asp.intArgSel(i) = '1' then
        res(1) := 'r';
    elsif asp.floatArgSel(i) = '1' then
        res(1) := 'f';
    else
        res(1) := '*';
    end if;
    res(2 to 3) := slv2hex(asp.args(i), 2); 
    return res;
end function;

 
 
function sprintTags(ins: InstructionState) return string is
    variable ri: word := (others => '0');
begin
    ri(TAG_SIZE-1 downto 0) := ins.tags.renameIndex; 
    return w2hex(ri) & ",F:" & w2hex(ins.tags.fetchCtr) & ",D:" & w2hex(ins.tags.decodeCtr)& ",R:" & w2hex(ins.tags.renameCtr)& ",C:" & w2hex(ins.tags.commitCtr);    
end function;



function opName(op: SpecificOp) return string is
    variable res: string(1 to 10);
begin
    case op.subpipe is
        when ALU => 
            res(3 to 10) := strExt(ArithOp'image(op.arith), 8);
        when Mem => 
            res(3 to 10) := strExt(MemOp'image(op.memory), 8);
        when FP => 
            res(3 to 10) := strExt(FpOp'image(op.float), 8);
        when others => 
            res(3 to 10) := strExt(SysOp'image(op.system), 8);                                                              
    end case;
    res(1 to 3) := strExt(SubpipeType'image(op.subpipe), 3);
    res(4) := ':';    
    return res;
end function;



function destText(ins: InstructionState) return string is
begin
    if ins.virtualArgSpec.intDestSel = '1' then
        return "r" & slv2hex(ins.virtualArgSpec.dest, 2);
    elsif ins.virtualArgSpec.floatDestSel = '1' then
        return "v" & slv2hex(ins.virtualArgSpec.dest, 2);
    else
        return "---";
    end if;
end function;

function srcText(ins: InstructionState; i: natural) return string is
begin
    if i = 1 and ins.constantArgs.immSel = '1' then
        return "imm";
    end if;

    if ins.virtualArgSpec.intArgSel(i) = '1' then
        return "r" & slv2hex(ins.virtualArgSpec.args(i), 2);
    elsif ins.virtualArgSpec.floatArgSel(i) = '1' then
        return "v" & slv2hex(ins.virtualArgSpec.args(i), 2);
    else
        return "---";
    end if;
end function;


function sprintHex(ins: InstructionState) return string is
begin
   return disasmWithAddress(slv2u(ins.ip), ins.bits);
end function;


function sprintDisasm(ins: InstructionState) return string is
begin
   return opName(ins.specificOperation) & "  " & destText(ins) & ", " & srcText(ins, 0)& ", " & srcText(ins, 1) & ", " & srcText(ins, 2) & ", " & w2hex(ins.constantArgs.imm);
end function;


function physDestText(ins: InstructionState) return string is
begin
    if ins.physicalArgSpec.intDestSel = '1' then
        return "r" & slv2hex(ins.physicalArgSpec.dest, 2);
    elsif ins.physicalArgSpec.floatDestSel = '1' then
        return "v" & slv2hex(ins.physicalArgSpec.dest, 2);
    else
        return "---";
    end if;end function;

function physSrcText(ins: InstructionState; i: natural) return string is
begin
    if i = 1 and ins.constantArgs.immSel = '1' then
        return "imm";
    end if;
    
    if ins.physicalArgSpec.intArgSel(i) = '1' then
        return "r" & slv2hex(ins.physicalArgSpec.args(i), 2);
    elsif ins.physicalArgSpec.floatArgSel(i) = '1' then
        return "v" & slv2hex(ins.physicalArgSpec.args(i), 2);
    else
        return "---";
    end if;
end function;


function sprintPhysDisasm(ins: InstructionState) return string is
   variable res: string(1 to 5*(8) + (5-1)*2);    
begin
   return opName(ins.specificOperation) & "  " & physDestText(ins) & ", " & physSrcText(ins, 0)& ", " & physSrcText(ins, 1) & ", " & physSrcText(ins, 2) & ", " & w2hex(ins.constantArgs.imm);
end function;


function sprintControl(ins: InstructionState) return string is
    variable condStr: string(1 to 11);
    variable trg: string(1 to 8) := (others => '-');
    variable re:  string(1 to 2) := "  ";
begin
-- 
    --control: 		j nz(i65) -> (+0x10) = 0x477ad644 (T)
    --control:		j  z(i22) -> (i73)   = ?????????  (N)   -- register value not ready
    --control:      j         -> (-0x30) = 0x55409000 (T)
    --control:		none (+4) -								 -- adr increment
    --control:		exc Ov    -> 0x00000200
    --              0.......8 ->
    
    
    ---- for jumps the predicted dir can be also attached to mnemonic in disasm:
    --		jz+ r15, @L2
    --		jz+- r15, @L2   -- ? the Exec direction shown too?
    
    if ins.controlInfo.refetch = '1' then
        re(2) := 'R';
    end if;
    
    condStr := "none (+4)";
    if ins.controlInfo.hasException = '1' then
        condStr := "Exc        ";
        trg := w2hex(EXC_BASE); -- TODO: adjust when exceptions develop
    elsif ins.controlInfo.specialAction = '1' then
        condStr := "Spe        ";
        
    elsif ins.specificOperation.subpipe = ALU then
        trg := w2hex(ins.target);
        if ins.controlInfo.frontBranch = '1' then
            condStr(10) := '+';
        else
            condStr(10) := '-';
        end if;
        if ins.controlInfo.confirmedBranch = '1' then
            condStr(11) := '+';
        else
            condStr(11) := '-';
        end if;
        
        case ins.specificOperation.arith is 
            when opJ | opJl =>
                condStr := "j          ";              
            when opJz => 
                condStr := "j  z(" & srcText(ins, 0) & ")  ";
            when opJnz =>
                condStr := "j nz(" & srcText(ins, 0) & ")  ";                
            when others =>
                condStr(10 to 11) := "  ";
                trg := (others => '-');
        end case;
        
    end if;
    
    -- CAREFUL: not using the -+ signs because they'd need info on whether they are already evaluated
    -- TODO? use completed and completed2 as indicators of evaluated front and Exec branch (Debug only!)
    if true then
        return condStr(1 to 9) & " -> " & trg & re;
    end if;
end function;



function sprintTransfer(ins: InstructionState) return string is
    variable ires, itarg: string(1 to 8) := (others => ' ');
    variable mid: string(1 to 2) := "  ";
    variable name: string(1 to 4) := "none";
    variable c1, c2: string(1 to 2) := "  ";
    variable ctrl: string(1 to 5) := "     ";
begin
    if ins.specificOperation.subpipe = Mem then
        c1(1) := std_logic'image(ins.controlInfo.completed)(2);
        c2(1) := std_logic'image(ins.controlInfo.completed2)(2);
        c1(2) := ':';
        c2(2) := ':';

        if ins.controlInfo.sqMiss = '1' then
            ctrl(2) := 'F';
        end if;
        if ins.controlInfo.tlbMiss = '1' then
            ctrl(3) := 'T';
        end if;
        if ins.controlInfo.dataMiss = '1' then
            ctrl(4) := 'D';
        end if;
        if ins.controlInfo.orderViolation = '1' then
            ctrl(5) := 'O';
        end if;        
                
        case ins.specificOperation.memory is
            when opLoad =>
                name := "ldm ";
                ires := w2hex(ins.result);
                mid := " @";
                itarg := w2hex(ins.target);
            when opStore =>    
                name := "stm ";
                ires := w2hex(ins.result);
                mid := "->";
                itarg := w2hex(ins.target);
            when opLoadSys =>
                name := "lds ";
                ires := w2hex(ins.result);
                mid := " @";
                itarg := w2hex(ins.target);
            when opStoreSys =>
                name := "sts ";
                ires := w2hex(ins.result);
                mid := "->";
                itarg := w2hex(ins.target);
            when others =>
                name := "??? ";
                --return "??? " & w2hex(ins.result) & "  " & w2hex(ins.target);
        end case;
    end if;
    
    return name & c2 & ires & mid & c1 & itarg & ctrl;
end function;


function sprintStatus(ins: InstructionState) return string is
    variable comp: string(1 to 2) := "00";
    variable ctrl: string(1 to 3) := "   ";
begin
    if ins.controlInfo.completed = '1' then
        comp(1) := '1';
    end if;
    if ins.controlInfo.completed2 = '1' then
        comp(2) := '1';
    end if;
 
     if ins.controlInfo.frontBranch = '1' then
        ctrl(1) := 'F';
    end if;   
    if ins.controlInfo.confirmedBranch = '1' then
        ctrl(2) := 'C';
    end if;

    if ins.controlInfo.specialAction = '1' then
        ctrl(3) := 'S';
    end if;
    if ins.controlInfo.hasException = '1' then
        ctrl(4) := 'E';
    end if;

end function;



function eraseIfEmpty(full: std_logic; str: string) return string is
    variable res: string(str'range) := str;
begin
    if full /= '1' then
        res := (others => ' ');
    end if;
    return res;
end function;


function sprintHex(isl: InstructionSlot) return string is
begin
    return eraseIfEmpty(isl.full, sprintHex(isl.ins));
end function;

function sprintDisasm(isl: InstructionSlot) return string is
begin
    return eraseIfEmpty(isl.full, sprintDisasm(isl.ins));
end function;

function sprintPhysDisasm(isl: InstructionSlot) return string is
begin
    return eraseIfEmpty(isl.full, sprintPhysDisasm(isl.ins));    
end function;    

function sprintTags(isl: InstructionSlot) return string is
begin
    return eraseIfEmpty(isl.full, sprintTags(isl.ins));    
end function;    

function sprintControl(isl: InstructionSlot) return string is
begin
    return eraseIfEmpty(isl.full, sprintControl(isl.ins));    
end function;    

function sprintTransfer(isl: InstructionSlot) return string is
begin
    return eraseIfEmpty(isl.full, sprintTransfer(isl.ins));    
end function;    

function sprintStatus(isl: InstructionSlot) return string is
begin
    return eraseIfEmpty(isl.full, sprintStatus(isl.ins));    
end function;    


function argLocText(sl: SchedulerEntrySlot; i: natural) return string is
    variable sub, phase: string(1 to 2) := "  ";
begin
    if sl.state.missing(i) = '1' then
        return "    M";
    elsif sl.state.zero(i) = '1' then
        return "    Z";
    elsif i = 1 and sl.state.immediate = '1' then
        return "    I";
    else
        case sl.state.argLocsPipe(i)(1 downto 0) is
            when "11" => 
                sub := " 3";
            when "00" =>
                sub := " 0";
            when "01" =>
                sub := " 1";
            when "10" => 
                sub := " 2";
            when others =>
        
        end case;        
    
        case sl.state.argLocsPhase(i)(1 downto 0) is
            when "11" => 
                phase := "-1";
            when "00" =>
                phase := " 0";
            when "01" =>
                phase := " 1";
            when "10" => 
                phase := " 2";
            when others =>
            
        end case;
        
        return sub & ":" & phase; 
    end if;
    
    return "";        
end function;
     

function sprintArgsInternal(sl: SchedulerEntrySlot) return string is
begin
    return  destText(sl.ins) & ":" & physDestText(sl.ins) & " <- " &
            srcText(sl.ins, 0) & ":" & physSrcText(sl.ins, 0) & " [" & argLocText(sl, 0) & "], " &
            srcText(sl.ins, 1) & ":" & physSrcText(sl.ins, 1) & " [" & argLocText(sl, 1) & "], " &
            srcText(sl.ins, 2) & ":" & physSrcText(sl.ins, 2) & " [" & argLocText(sl, 2) & "]";
end function;


function sprintArgs(sl: SchedulerEntrySlot) return string is
begin
    return eraseIfEmpty(sl.full, sprintArgsInternal(sl));
end function;


function sprintRobRow(ia: InstructionSlotArray) return InsString is
    variable res: InsString := (others => ' ');
    variable tmpStr: string(1 to 8);
    variable ind: natural := 11;
    variable renameIndexW: Word := (others => '0');
begin
    renameIndexW(TAG_SIZE-1 downto 0) := ia(0).ins.tags.renameIndex;
    res(1 to 8) := w2hex(renameIndexW);
    res(9 to 10) := ": ";
    for i in 0 to PIPE_WIDTH-1 loop
        tmpStr := (others => ' ');
        if ia(i).full = '1' then
            tmpStr(1 to 2) := "00";
            if ia(i).ins.controlInfo.completed = '1' then
                tmpStr(1) := '1';
            end if;
            if ia(i).ins.controlInfo.completed2 = '1' then
                tmpStr(2) := '1';
            end if;
            if ia(i).ins.controlInfo.frontBranch = '1' then
                tmpStr(3) := 'F';
            end if;
            if ia(i).ins.controlInfo.confirmedBranch = '1' then
                tmpStr(4) := 'C';
            end if;                                 
            
            if ia(i).ins.controlInfo.hasException = '1' then
                tmpStr(5) := 'E';
            end if;
            
            if ia(i).ins.controlInfo.orderViolation = '1' then
                tmpStr(6) := 'V';
            end if;                
        end if;    

        res(ind to ind + tmpStr'length-1) := tmpStr;
        res(ind + tmpStr'length to ind + tmpStr'length+1) := ", ";
        ind := ind + tmpStr'length + 2;
    end loop;
    
    return res;
end function;


--procedure printGroup(ia: InstructionSlotArray; filename: string) is
--    file outFile: text open append_mode is filename;
--    variable outputLine: line;
--begin
--    for i in ia'range loop
--        if ia(i).full = '1' then
--            write(outputLine, disasmWithAddress(slv2u(ia(i).ins.ip), ia(i).ins.bits)); 
--            writeline(outFile, outputLine);
--        end if;
--    end loop;
    
--end procedure;

procedure printGroup(ia: InstructionSlotArray; file f: text) is
    variable outputLine: line;
begin
    for i in ia'range loop
        if ia(i).full = '1' then
            write(outputLine, disasmWithAddress(slv2u(ia(i).ins.ip), ia(i).ins.bits));
                if ia(i).ins.controlInfo.hasInterrupt = '1' then
                    write(outputLine, string'("  # Interrupt "));
                end if;
                if ia(i).ins.controlInfo.hasException = '1' then
                    write(outputLine, string'("  # Exception "));
                end if;                 
            
                if ia(i).ins.controlInfo.specialAction = '1' then
                    write(outputLine, string'("  # SpecialAction "));
                end if;
                
                if ia(i).ins.controlInfo.refetch = '1' then
                    write(outputLine, string'("(refetch)"));
                end if;                
            writeline(f, outputLine);
        end if;
    end loop;
    
end procedure;


end package body;

