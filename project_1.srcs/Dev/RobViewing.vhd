--

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


use work.LogicROB.all;

use std.textio.all;
use work.CpuText.all;
use work.Assembler.all;


package RobViewing is
    procedure printContent(dynamicContent: DynamicOpInfoArray2D; startP, endP: SmallNumber);

    type ElementState is (ignored, empty, waiting, completed);
                        --(ignored, empty, waiting01, waiting0, waiting1, completed);
    type ElementState_N is (' ', '-', '0', '1');
    type ES2 is array(0 to 1) of ElementState_N;

    constant ces2: ES2 := "  ";

    -- write:   "  " -> "  " | "00" | "01" | "10" | "11"
    -- kill:     *   -> "  "
    -- abandon: 

    type StateRow is array(0 to PIPE_WIDTH-1) of ElementState;
    type StateTable is array(0 to ROB_SIZE-1) of StateRow;


    procedure DB_updateStatesMain(signal table: inout StateTable; execSigs: ExecPacketArray);
    procedure DB_writeStates(signal table: inout StateTable; input: InstructionSlotArray; ptr: SmallNumber);
    procedure DB_commitStates(signal table: inout StateTable; ptr: SmallNumber);
    procedure DB_eventStates(signal table: inout StateTable; events: EventState; startP, endP: SmallNumber);
    procedure DB_memEventStates(signal table: inout StateTable; ep: ExecPacket; memCt: ControlPacket; startP, endP: SmallNumber);

end package;


package body RobViewing is
    
    function getDynamicContentString(dyn: DynamicOpInfo) return string is
        variable res: line;
    begin
        -- pragma synthesis off

        if dyn.full = '1' then
            write(res, slv2hex(dyn.dbInfo.tag));
            write(res, string'(": "));
            write(res, std_logic'image(dyn.completed0));
            if dyn.secCluster = '1' then
                write(res, std_logic'image(dyn.completed1));
            else
                write(res, string'("'-'"));
            end if;
            write(res, string'(" "));
            if dyn.hasEvent = '1' then
                write(res, string'("E : "));
            else
                write(res, string'("  : "));
            end if;
            
            write(res, disasmWord(dyn.dbInfo.bits));

            return res.all;
        else
            return "-------------------------------------";
        end if;
        -- pragma synthesis on

    end function;

    procedure printContent(dynamicContent: DynamicOpInfoArray2D; startP, endP: SmallNumber) is
       file outFile: text open write_mode is "rob_content.txt";
       variable preRow, currentLine: line := null;
       constant LEN: natural := ROB_SIZE;
    begin
        report "ROB reporting ";

        for i in 0 to LEN-1 loop
            if p2i(startP, LEN) = i then
                preRow := new string'("start ");
            elsif p2i(endP, LEN) = i then
                preRow := new string'("end   ");
            else
                preRow := new string'("      ");
            end if;

            currentLine := null;
            write(currentLine, preRow.all & natural'image(i) & "  ");
            for j in 0 to PIPE_WIDTH-1 loop
                write(currentLine, getDynamicContentString(dynamicContent(i, j)) & ",   ");
            end loop;

            writeline(outFile, currentLine);
        end loop;
    end procedure;

    procedure fail(msg: string) is
    begin
        report msg severity failure;
    end procedure;

    procedure stateError(msg: string; oldVal: ElementState; row, col: natural) is
    begin
        report msg & "; at (" & natural'image(row) & ", " & natural'image(col) & ") = " &  ElementState'image(oldVal) severity failure;
    end procedure;


    procedure writeEntry(signal table: inout StateTable; row, col: natural; clusterSel: std_logic_vector(0 to 1)) is
        variable oldVal, newVal: ElementState := table(row)(col);
    begin
        case oldVal is
            when empty =>
                case clusterSel is
                    -- Careful: ignoring secCluster for now
                    when "00" => newVal := completed;
                    when "01" => newVal := completed;
                    when "10" => newVal := waiting;
                    when "11" => newVal := waiting;
                    when others => fail("Wrong cluster select");
                end case;
            when others => stateError("WRITE:", oldVal, row, col);
        end case;
        table(row)(col) <= newVal;
    end procedure;


    procedure updateEntry(signal table: inout StateTable; row, col: natural; valid: std_logic) is
        variable oldVal, newVal: ElementState := table(row)(col);
    begin
        case oldVal is
            when waiting | ignored =>
                if valid = '1' then
                    newVal := completed;
                end if;
            when others => stateError("UPDATE:", oldVal, row, col);
        end case;
        table(row)(col) <= newVal;
    end procedure;

    procedure commitEntry(signal table: inout StateTable; row, col: natural) is
        variable oldVal, newVal: ElementState := table(row)(col);
    begin
        case oldVal is
            when ignored | completed | empty => newVal := empty;
            when others => stateError("COMMIT:", oldVal, row, col);
        end case;
        table(row)(col) <= newVal;
    end procedure;

    procedure killEntry(signal table: inout StateTable; row, col: natural) is
        variable oldVal, newVal: ElementState := table(row)(col);
    begin
        newVal := empty;
        table(row)(col) <= newVal;
    end procedure;

    procedure ignoreEntry(signal table: inout StateTable; row, col: natural) is
        variable oldVal, newVal: ElementState := table(row)(col);
    begin
        case oldVal is
            -- No change empty->ignored or completed->ignored because trying to Update them should be an error
            -- However, trying to Update ignored entry is not wrong, just not required
            when ignored | waiting => newVal := ignored;
            when others => null;
        end case;
        table(row)(col) <= newVal;
    end procedure;


    procedure DB_updateStatesMain(signal table: inout StateTable; execSigs: ExecPacketArray) is
        variable execElem: ExecPacket := DEFAULT_EXEC_PACKET;
        variable row, col: natural := 0;
        variable tagHigh, tagHighTrunc: SmallNumber;
    begin

        for e in execSigs'range loop
            execElem := execSigs(e);
            tagHigh := getTagHighSN(execElem.tag);
            tagHighTrunc := tagHigh and PTR_MASK_SN;
            row := slv2u(tagHighTrunc);
            col := slv2u(getTagLow(execElem.tag));

            if (execElem.full and not execElem.killed) = '1' then
                updateEntry(table, row, col, '1');
            elsif execElem.fail = '1' then
                updateEntry(table, row, col, '0');
            end if;
        end loop;
    end procedure;

    procedure DB_writeStates(signal table: inout StateTable; input: InstructionSlotArray; ptr: SmallNumber) is
        variable execElem: InstructionSlot := DEFAULT_INS_SLOT;
        variable row, col: natural := 0;
        variable tagHigh, tagHighTrunc: SmallNumber;
        variable clusterSelect: std_logic_vector(0 to 1) := "00";
    begin
        tagHighTrunc := ptr and PTR_MASK_SN;
        row := slv2u(tagHighTrunc);
            
        for c in input'range loop
            execElem := input(c);
            
            if execElem.full /= '1' then
                next;
            end if;

            clusterSelect(0) := execElem.ins.typeInfo.mainCluster;
            clusterSelect(1) := execElem.ins.typeInfo.secCluster;
            
            writeEntry(table, row, c, clusterSelect);
        end loop;
    end procedure;

    procedure DB_commitStates(signal table: inout StateTable; ptr: SmallNumber) is
        variable execElem: InstructionSlot := DEFAULT_INS_SLOT;
        variable row, col: natural := 0;
        variable tagHigh, tagHighTrunc: SmallNumber;
    begin
        tagHighTrunc := ptr and PTR_MASK_SN;
        row := slv2u(tagHighTrunc);

        for c in 0 to PIPE_WIDTH-1 loop
            commitEntry(table, row, c);
        end loop;
    end procedure;

    procedure DB_eventStates(signal table: inout StateTable; events: EventState; startP, endP: SmallNumber) is
        variable execElem: InstructionSlot := DEFAULT_INS_SLOT;
        variable row, col: natural := 0;
        variable tag: InsTag := events.execCausing.tag;
        variable tagHigh, tagHighTrunc: SmallNumber;
    begin
        if events.lateCausing.full = '1' then
            for r in 0 to ROB_SIZE-1 loop
                for c in 0 to PIPE_WIDTH-1 loop
                    killEntry(table, r, c);
                end loop;
            end loop;
            return;
        end if;

        if events.execCausing.full /= '1' then
            return;
        end if;
        
        tagHigh := getTagHighSN(tag);
        tagHighTrunc := tagHigh and PTR_MASK_SN;
        row := slv2u(tagHighTrunc);
        col := slv2u(getTagLow(tag));
        
        for c in col+1 to PIPE_WIDTH-1 loop
            killEntry(table, row, c);
        end loop;

        loop -- must wrap so no for-range
            row := (row + 1) mod ROB_SIZE;
            if row = p2i(startP, ROB_SIZE) then exit; end if;
            for c in 0 to PIPE_WIDTH-1 loop
                killEntry(table, row, c);
            end loop;
        end loop;
    end procedure;


    procedure DB_memEventStates(signal table: inout StateTable; ep: ExecPacket; memCt: ControlPacket; startP, endP: SmallNumber) is
        variable execElem: InstructionSlot := DEFAULT_INS_SLOT;
        variable row, col: natural := 0;
        variable tag: InsTag := memCt.tags.renameIndex;
        variable tagHigh, tagHighTrunc: SmallNumber;
    begin
        if (ep.full and not ep.killed and memCt.controlInfo.specialAction) /= '1' then
            return;
        end if;
    
        tagHigh := getTagHighSN(tag);
        tagHighTrunc := tagHigh and PTR_MASK_SN;
        row := slv2u(tagHighTrunc);
        col := slv2u(getTagLow(tag));

        for c in col+1 to PIPE_WIDTH-1 loop
            ignoreEntry(table, row, c);
        end loop;
    end procedure;

end package body;