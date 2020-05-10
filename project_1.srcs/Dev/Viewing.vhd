
library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.ArchDefs.all;

use work.CoreConfig.all;

use work.Assembler.all;
use std.textio.all;
   
use work.InstructionState.all;


package Viewing is

type InstructionText is record
    tagTxt: string(1 to 40);
    hexTxt: string(1 to 40);
    virtTxt: string(1 to 40);
    physTxt: string(1 to 40);
    controlTxt: string(1 to 40);       
end record;

type InstructionTextArray is array(integer range <>) of InstructionText;

subtype Str40 is string(1 to 40);
type AStr40 is array(0 to 2) of Str40;

type SchedEntryText is record
    stateTxt: string(1 to 40);
    args: AStr40;
end record;

type SchedEntryTextArray is array(integer range <>) of SchedEntryText;

function w2hex(w: Word) return string;



type StrArray is array(integer range <>) of string(1 to 51);
    
subtype FetchStageView is StrArray(0 to FETCH_WIDTH-1);
subtype GenericStageView is StrArray(0 to PIPE_WIDTH-1);


function createFetchStageView(stageOutputScalar: InstructionSlot; fetchLine: WordArray) return StrArray;
function createGenericStageView(stageOutput: InstructionSlotArray) return StrArray;
function createGenericStageView(stageOutput: SchedulerEntrySlotArray) return StrArray;



type InsPrintFormat is (none, disasm, physDisasm, tags, control, transfer, status, args);

constant INS_STR_SIZE: natural := 51;
subtype InsString is string(1 to INS_STR_SIZE);
type InsStringArray is array(integer range <>) of InsString;


    function sprintDisasm(ins: InstructionState) return string;

    function sprintPhysDisasm(ins: InstructionState) return string;

    function sprintTags(ins: InstructionState) return string;

    function sprintControl(ins: InstructionState) return string;

    function sprintTransfer(ins: InstructionState) return string;

    function sprintStatus(ins: InstructionState) return string;


    function sprintDisasm(isl: InstructionSlot) return string;

    function sprintPhysDisasm(isl: InstructionSlot) return string;

    function sprintTags(isl: InstructionSlot) return string;

    function sprintControl(isl: InstructionSlot) return string;

    function sprintTransfer(isl: InstructionSlot) return string;

    function sprintStatus(isl: InstructionSlot) return string;

    function sprintArgs(sl: SchedulerEntrySlot) return string;
    
    function sprintRobRow(ia: InstructionSlotArray) return InsString;

function getInsString(isl: InstructionSlot; fmt: InsPrintFormat) return InsString;

function getInsStringArray(ia: InstructionSlotArray; fmt: InsPrintFormat) return InsStringArray;

function getInsStringArray(sa: SchedulerEntrySlotArray; fmt: InsPrintFormat) return InsStringArray;

    
end package;


package body Viewing is



function getInsString(isl: InstructionSlot; fmt: InsPrintFormat) return InsString is
    variable res: InsString := (others => ' ');
begin
        case fmt is
            when disasm =>
                res := padTo(sprintDisasm(isl), INS_STR_SIZE);
            when physDisasm =>
                res := padTo(sprintPhysDisasm(isl), INS_STR_SIZE);
            when tags =>
                res := padTo(sprintDisasm(isl), INS_STR_SIZE);
            when control =>
                res := padTo(sprintControl(isl), INS_STR_SIZE);
            when transfer =>
                res := padTo(sprintTransfer(isl), INS_STR_SIZE);                                                        
            when status =>
                res := padTo(sprintStatus(isl), INS_STR_SIZE);                                                        
                            
            when others =>
        end case;
    
    return res;
end function;



function getInsStringArray(ia: InstructionSlotArray; fmt: InsPrintFormat) return InsStringArray is
    variable res: InsStringArray(0 to ia'length-1) := (others => (others => ' '));
begin
    for i in 0 to ia'length-1 loop
        case fmt is
            when disasm =>
                res(i) := padTo(sprintDisasm(ia(i)), INS_STR_SIZE);
            when physDisasm =>
                res(i) := padTo(sprintPhysDisasm(ia(i)), INS_STR_SIZE);
            when tags =>
                res(i) := padTo(sprintDisasm(ia(i)), INS_STR_SIZE);
            when control =>
                res(i) := padTo(sprintControl(ia(i)), INS_STR_SIZE);
            when transfer =>
                res(i) := padTo(sprintTransfer(ia(i)), INS_STR_SIZE);                                                        
            when status =>
                res(i) := padTo(sprintStatus(ia(i)), INS_STR_SIZE);                                                        
                            
            when others =>
        end case;
        
    end loop;
    
    return res;
end function;


function getInsStringArray(sa: SchedulerEntrySlotArray; fmt: InsPrintFormat) return InsStringArray is
    variable res: InsStringArray(0 to sa'length-1) := (others => (others => ' '));
begin
    for i in 0 to sa'length-1 loop
        case fmt is
            when args =>
                res(i) := padTo(sprintArgs(sa(i)), INS_STR_SIZE); 
                                            
            when others =>
        end case;
        
    end loop;
    
    return res;
end function;


function createFetchStageView(stageOutputScalar: InstructionSlot; fetchLine: WordArray) return StrArray is
    variable res: FetchStageView;
    variable adrHi, adrLo: Mword := stageOutputScalar.ins.ip; 
begin
    adrHi(ALIGN_BITS-1 downto 0) := (others => '0');
    adrLo(MWORD_SIZE-1 downto ALIGN_BITS) := (others => '0');    

    --res.full := stageOutputScalar.full;
    for i in 0 to fetchLine'length-1 loop
        if stageOutputScalar.full = '1' and slv2u(adrLo) <= 4*i then
            res(i) := disasmWithAddress(slv2u(adrHi) + 4*i, fetchLine(i));
        end if;
    end loop;
    
    return res;
end function;

function createGenericStageView(stageOutput: InstructionSlotArray) return StrArray is
    variable res: StrArray(stageOutput'range);
begin
    --res.full := stageOutputScalar.full;
    for i in 0 to stageOutput'length-1 loop
        if stageOutput(i).full = '1' then
            res(i) := disasmWithAddress(slv2u(stageOutput(i).ins.ip), stageOutput(i).ins.bits);
        end if;
    end loop;
    
    return res;
end function;

function createGenericStageView(stageOutput: SchedulerEntrySlotArray) return StrArray is
    variable res: StrArray(stageOutput'range);
begin
    --res.full := stageOutputScalar.full;
    for i in 0 to stageOutput'length-1 loop
        if stageOutput(i).full = '1' then
            res(i) := disasmWithAddress(slv2u(stageOutput(i).ins.ip), stageOutput(i).ins.bits);
        end if;
    end loop;
    
    return res;
end function;


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------


function reg2txt(reg: std_logic_vector) return string is
    variable res: string(1 to 2);
    constant letters: string(1 to 16) := "0123456789abcdef";
begin
    res(1) := letters(slv2u(reg(4 downto 4)) + 1);    
    res(2) := letters(slv2u(reg(3 downto 0)) + 1);
    return res;
end function;

function physReg2txt(reg: std_logic_vector) return string is
    variable res: string(1 to 2);
    constant letters: string(1 to 16) := "0123456789abcdef";
begin
    res(1) := letters(slv2u(reg(7 downto 4)) + 1);
    res(2) := letters(slv2u(reg(3 downto 0)) + 1);    
    return res;
end function;

function w2hex(w: Word) return string is
    constant letters: string(1 to 16) := "0123456789abcdef";
    variable res: string(1 to 8) := (others => '0');
begin
    for i in 7 downto 0 loop
        res(8-i) := letters(slv2u(w(4*i+3 downto 4*i)) + 1);
    end loop;
    return res;
end function;

function tag2hex(t: InsTag) return string is
    constant letters: string(1 to 16) := "0123456789abcdef";
    variable res: string(1 to 3) := (others => '0');
begin
    res(1) := letters(slv2u(t(8 downto 8)) + 1);
    res(2) := letters(slv2u(t(7 downto 4)) + 1);
    res(3) := letters(slv2u(t(3 downto 0)) + 1);    
    return res;
end function;

function strExt(str: string; n: positive) return string is 
    variable res: string(1 to n) := (others => ' ');
begin
    for i in 1 to str'length loop
        if i > n then
            exit;
        end if;
        res(i) := str(i);
    end loop;
    return res;
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
    res(2 to 3) := physReg2txt(asp.dest);  
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
    res(2 to 3) := physReg2txt(asp.args(i));
    return res;
end function;

 
 
function sprintTags(ins: InstructionState) return string is
    variable res: string(1 to 5*(8) + (5-1)*2);
    variable ri: word := (others => '0');
begin
    ri(TAG_SIZE-1 downto 0) := ins.tags.renameIndex; 
    return w2hex(ri) & ",F:" & w2hex(ins.tags.fetchCtr) & ",D:" & w2hex(ins.tags.decodeCtr)& ",R:" & w2hex(ins.tags.renameCtr)& ",C:" & w2hex(ins.tags.commitCtr);
    
    --return res;
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
        return "r" & reg2txt(ins.virtualArgSpec.dest);
    elsif ins.virtualArgSpec.floatDestSel = '1' then
        return "v" & reg2txt(ins.virtualArgSpec.dest);
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
        return "r" & reg2txt(ins.virtualArgSpec.args(i));
    elsif ins.virtualArgSpec.floatArgSel(i) = '1' then
        return "v" & reg2txt(ins.virtualArgSpec.args(i));
    else
        return "---";
    end if;
end function;


function sprintDisasm(ins: InstructionState) return string is
   variable res: string(1 to 5*(8) + (5-1)*2);    
begin
   return opName(ins.specificOperation) & "  " & destText(ins) & ", " & srcText(ins, 0)& ", " & srcText(ins, 1) & ", " & srcText(ins, 2) & ", " & w2hex(ins.constantArgs.imm);

   --return res; 
end function;


function physDestText(ins: InstructionState) return string is
begin
    if ins.physicalArgSpec.intDestSel = '1' then
        return "r" & reg2txt(ins.physicalArgSpec.dest);
    elsif ins.physicalArgSpec.floatDestSel = '1' then
        return "v" & reg2txt(ins.physicalArgSpec.dest);
    else
        return "---";
    end if;end function;

function physSrcText(ins: InstructionState; i: natural) return string is
begin
    if i = 1 and ins.constantArgs.immSel = '1' then
        return "imm";
    end if;
    
    if ins.physicalArgSpec.intArgSel(i) = '1' then
        return "r" & physReg2txt(ins.physicalArgSpec.args(i));
    elsif ins.physicalArgSpec.floatArgSel(i) = '1' then
        return "v" & physReg2txt(ins.physicalArgSpec.args(i));
    else
        return "---";
    end if;
end function;


function sprintPhysDisasm(ins: InstructionState) return string is
   variable res: string(1 to 5*(8) + (5-1)*2);    
begin
   return opName(ins.specificOperation) & "  " & physDestText(ins) & ", " & physSrcText(ins, 0)& ", " & physSrcText(ins, 1) & ", " & physSrcText(ins, 2) & ", " & w2hex(ins.constantArgs.imm);

   --return res; 
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
            if sl.state.argValues.missing(i) = '1' then
                return "    M";
            elsif sl.state.argValues.zero(i) = '1' then
                return "    Z";
            elsif i = 1 and sl.state.argValues.immediate = '1' then
                return "    I";
            else
                case sl.state.argValues.argLocsPipe(i)(1 downto 0) is
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
            
                case sl.state.argValues.argLocsPhase(i)(1 downto 0) is
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
        variable tmp: string(1 to 10);
    begin
        --if isl.ins.virtualArgSpec.intDestSel = '1' then
        return  destText(sl.ins) & ":" & physDestText(sl.ins) & " <- " &
        --elsif isl.ins.virtualArgSpec.floatDestSel = '1' then
                srcText(sl.ins, 0) & ":" & physSrcText(sl.ins, 0) & " [" & argLocText(sl, 0) & "], " &
                srcText(sl.ins, 1) & ":" & physSrcText(sl.ins, 1) & " [" & argLocText(sl, 1) & "], " &
                srcText(sl.ins, 2) & ":" & physSrcText(sl.ins, 2) & " [" & argLocText(sl, 2) & "]";
        --end if;
        
        --return "";
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





function insText(ins: InstructionState; isMem: std_logic) return InstructionText is
    variable dest, src0, src1, src2: string(1 to 3) := (others => '*');
    variable tagStr, hexStr, virtStr, physStr, controlStr, memStr: string(1 to 40) := (others => nul);
    variable res: InstructionText;
    variable hexTarget: string(1 to 8);
    variable destSymbol: string(1 to 1);
    variable srcSymbols: string(1 to 3);
begin
        
        -- tagStr := w2hex(ins.tags.fetchCtr)

    -- Tag text; fetchCtr/decodeCtr/renameCtr/renameIndex
    tagStr(1 to 8) := w2hex(ins.tags.fetchCtr);
    tagStr(9) := '/';
    tagStr(10 to 17) := w2hex(ins.tags.decodeCtr);
    tagStr(18) := '/';
    tagStr(19 to 26) := w2hex(ins.tags.renameCtr);
    tagStr(27) := '/';
    tagStr(28 to 30) := tag2hex(ins.tags.renameIndex);
    
    -- Hex text;   adr: bits
    hexStr(1 to 8) := w2hex(ins.ip); -- TODO: introduce 64b address when needed
    hexStr(9 to 10) := ": ";
    hexStr(11 to 18) := w2hex(ins.bits);
    
    -- Virtual txt;  
    dest(1 to 3) := getDestSymbol(ins.virtualArgSpec);
    src0(1 to 3) := getSrcSymbol(ins.virtualArgSpec, 0);
    src1(1 to 3) := getSrcSymbol(ins.virtualArgSpec, 1);    
    src2(1 to 3) := getSrcSymbol(ins.virtualArgSpec, 2);
    -- When imm value is used replace src1
    if ins.constantArgs.immSel = '1' then
        src1 := "imm";
    end if;   
    
    -- Characters 3,4 will be overwritten, removing op- part of each operation name
    case ins.specificOperation.subpipe is
        when ALU => 
            virtStr(3 to 11) := strExt(ArithOp'image(ins.specificOperation.arith), 9);
        when Mem => 
            virtStr(3 to 11) := strExt(MemOp'image(ins.specificOperation.memory), 9);
        when FP => 
            virtStr(3 to 11) := strExt(FpOp'image(ins.specificOperation.float), 9);
        when others => 
            virtStr(3 to 11) := strExt(SysOp'image(ins.specificOperation.system), 9);                                                              
    end case;
    virtStr(1 to 3) := strExt(SubpipeType'image(ins.specificOperation.subpipe), 3);
    virtStr(4) := ':';              
    --virtStr(10) := ' ';
    virtStr(13 to 15) := dest;
    virtStr(16 to 17) := ", ";
    virtStr(18 to 20) := src0;
    virtStr(21 to 22) := ", ";
    virtStr(23 to 25) := src1;
    virtStr(26 to 27) := ", ";
    virtStr(28 to 30) := src2;   

    --Physical txt
    dest(1 to 3) := getDestSymbol(ins.physicalArgSpec);
    src0(1 to 3) := getSrcSymbol(ins.physicalArgSpec, 0);
    src1(1 to 3) := getSrcSymbol(ins.physicalArgSpec, 1);    
    src2(1 to 3) := getSrcSymbol(ins.physicalArgSpec, 2);
    -- When imm value is used replace src1
    if ins.constantArgs.immSel = '1' then
        src1 := "imm";
    end if;   

    case ins.specificOperation.subpipe is
        when ALU => 
            physStr(3 to 11) := strExt(ArithOp'image(ins.specificOperation.arith), 9);
        when Mem => 
            physStr(3 to 11) := strExt(MemOp'image(ins.specificOperation.memory), 9);
        when FP => 
            physStr(3 to 11) := strExt(FpOp'image(ins.specificOperation.float), 9);
        when others => 
            physStr(3 to 11) := strExt(SysOp'image(ins.specificOperation.system), 9);                                                              
    end case;
    physStr(1 to 3) := strExt(SubpipeType'image(ins.specificOperation.subpipe), 3);
    physStr(4) := ':';              
    --virtStr(10) := ' ';
    physStr(13 to 15) := dest;
    physStr(16 to 17) := ", ";
    physStr(18 to 20) := src0;
    physStr(21 to 22) := ", ";
    physStr(23 to 25) := src1;
    physStr(26 to 27) := ", ";
    physStr(28 to 30) := src2;

    -- Control txt
    -- BrP: T/N/-, Ref: Y/N, BrC: T/N,-, Exc: Y/N, Sp: Y/N
    if ins.controlInfo.refetch = '1' then
       controlStr(1 to 7) := "Refetch";
    elsif ins.controlInfo.hasException = '1' then
       controlStr(1 to 3) := "Exc";
    elsif ins.controlInfo.orderViolation = '1' then
          controlStr(1 to 3) := "Ord";       
    elsif ins.controlInfo.specialAction = '1' then
       controlStr(1 to 7) := "Special";   
    elsif ins.classInfo.branchIns = '1' then
       hexTarget := w2hex(ins.target);
       controlStr(1 to 3) := "Bf:";
       controlStr(4) := '0';
       if ins.controlInfo.frontBranch = '1' then
           controlStr(4) := '1';
       end if;
       controlStr(5) := ',';
       controlStr(6 to 13) := hexTarget;
       controlStr(14) := ';';

       controlStr(15 to 17) := "Bc:";
       controlStr(18) := '0';
       if ins.controlInfo.confirmedBranch = '1' then
           controlStr(18) := '1';
       end if;
       controlStr(19) := ',';
       controlStr(20 to 27) := hexTarget;
    end if;
    
    -- Make structure
    res.tagTxt := tagStr;
    res.hexTxt := hexStr;
    res.virtTxt := virtStr;
    res.physTxt := physStr;
    res.controlTxt := controlStr;
    
    -- For mem ops:
    -- completed2: Value; completed: Addr
    memStr(1) := '@';
    memStr(10) := ':';
    memStr(2 to 9) := (others => '-');
    memStr(11 to 18) := (others => '-');        
    if ins.controlInfo.completed = '1' then
        memStr(2 to 9) := w2hex(ins.target);
    end if;
    
    if ins.controlInfo.completed2 = '1' then
        memStr(11 to 18) := w2hex(ins.result);            
    end if;

    memStr(19 to 21) := ";  ";
    if ins.controlInfo.orderViolation = '1' then
        memStr(21 to 23) := "Ord";
    end if;

    if isMem = '1' then
        res.controlTxt := memStr;
    end if;
   
    return res;
end function;



function getSchedStateText(se: SchedulerState; full: std_logic) return SchedEntryText is
    variable res: SchedEntryText; 
begin
    if full = '0' then
        return res;
    end if;

    if se.argValues.issued = '1' then
        res.stateTxt(1 to 6) := "Issue ";
    elsif se.argValues.missing = "000" then
        res.stateTxt(1 to 6) := "Ready ";
    else
        res.stateTxt(1 to 6) := "Waits ";
    end if;

    res.args(0)(1 to 3) := "0: ";
    res.args(0)(12 to 13) := ", ";
    res.args(0)(4 to 11) := (others => '-');
    if se.argValues.missing(0) = '1' then
        
        res.stateTxt(7) := '1';
    else
        if se.argValues.zero(0) = '1' then
            res.args(0)(14) := 'Z';
        else
            res.args(0)(14 to 18) := std_logic'image(se.argValues.argLocsPipe(0)(1))(2) & std_logic'image(se.argValues.argLocsPipe(0)(0))(2)
                               &  ':'
                               &  std_logic'image(se.argValues.argLocsPhase(0)(0))(2) & std_logic'image(se.argValues.argLocsPhase(0)(0))(2);
        end if;
        res.stateTxt(7) := '0';
    end if;

    res.args(1)(1 to 3) := "1: ";
    res.args(1)(12 to 13) := ", ";
    res.args(1)(4 to 11) := (others => '-');
    if se.argValues.missing(1) = '1' then
        res.stateTxt(8) := '1';
    else
        if se.argValues.zero(1) = '1' then
            res.args(1)(14) := 'Z';
        elsif se.argValues.immediate = '1' then
            res.args(1)(14) := 'I';
        else
            res.args(1)(14 to 18) := std_logic'image(se.argValues.argLocsPipe(1)(1))(2) & std_logic'image(se.argValues.argLocsPipe(1)(0))(2)
                           &  ':'
                           &  std_logic'image(se.argValues.argLocsPhase(1)(0))(2) & std_logic'image(se.argValues.argLocsPhase(1)(0))(2);        
        end if;
        res.stateTxt(8) := '0';
    end if;
        
        res.stateTxt(9) := '0';
        
    return res;
end function;


end package body;

