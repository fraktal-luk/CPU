--

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use std.textio.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.CpuText.all;
use work.Assembler.all;

use work.LogicQueues.all;

package MemQueueViewing is
    function getNamePrefix(constant LQ: boolean) return string;

    procedure printContent(content: QueueEntryArray; adrs: MwordArray; vals: MwordArray; startP, endP: SmallNumber; constant NAME_PREFIX: string);

    type QEntryState is (empty, waiting, adr, val, completed, committed);
    type EntryStateArray is array(natural range <>) of QEntryState;

    procedure stateError(msg: string; oldVal: QEntryState; ind: natural);
    procedure writeState(signal table: inout EntryStateArray; ind: natural; IS_LQ: boolean);
    procedure commitState(signal table: inout EntryStateArray; ind: natural; IS_LQ: boolean);
    procedure drainState(signal table: inout EntryStateArray; ind: natural);
    procedure updateState(signal table: inout EntryStateArray; ind: natural; a, v: std_logic; IS_LQ: boolean);
    procedure killState(signal table: inout EntryStateArray; ind: natural);
    procedure DB_writeStates(signal table: inout EntryStateArray; mask: std_logic_vector; endP: SmallNumber; IS_LQ: boolean);
    procedure DB_commitStates(signal table: inout EntryStateArray; mask: std_logic_vector; startP: SmallNumber;  IS_LQ: boolean);
    procedure DB_drainStates(signal table: inout EntryStateArray; drainP: SmallNumber);
    procedure DB_updateAddress(signal table: inout EntryStateArray; en: std_logic; er: ExecResult; ec: ControlPacket; IS_LQ: boolean);
    procedure DB_updateValue(signal table: inout EntryStateArray; en: std_logic; er: ExecResult);--; ec: ControlPacket; IS_LQ: boolean
    procedure DB_updateStates(signal table: inout EntryStateArray; enA, enV: std_logic; adrCt: ControlPacket; valRes: ExecResult; IS_LQ: boolean);
    procedure DB_eventStates(signal table: inout EntryStateArray; evt: EventState; startP, drainP: SmallNumber; IS_LQ: boolean);

end package;


package body MemQueueViewing is

    function getNamePrefix(constant LQ: boolean) return string is
    begin
        if LQ then
            return "l";
        else
            return "s";
        end if;
    end function;

    function getDynamicContentString(elem: QueueEntry; adr: Mword; value: Mword) return string is
        variable res: line;
    begin
        if true then --elem.full = '1' then
            write(res, string'(": "));
            write(res, std_logic'image(elem.completedA));
            write(res, std_logic'image(elem.completedV));
            write(res, string'(" @"));
            
            if elem.completedA = '1' then
                write(res, natural'image(slv2u(adr)));
            else
                write(res, string'("????????"));
            end if;
            write(res, string'(": "));
            
            if elem.completedV = '1' then
                write(res, natural'image(slv2u(value)));
            else
                write(res, string'("????????"));
            end if;                
            return res.all;
        else
            return "-------------------------------------";
        end if;
    end function;

    procedure printContent(content: QueueEntryArray; adrs: MwordArray; vals: MwordArray; startP, endP: SmallNumber; constant NAME_PREFIX: string) is
       file outFile: text open write_mode is NAME_PREFIX & "q_content.txt";
       constant LEN: natural := content'length;
       variable preRow, currentLine: line := null;
    begin
        for i in 0 to ROB_SIZE-1 loop
            if p2i(startP, LEN) = i then
                preRow := new string'("start ");
            elsif p2i(endP, LEN) = i then
                preRow := new string'("end   ");
            else
                preRow := new string'("      ");
            end if;

            currentLine := null;
            write(currentLine, preRow.all & natural'image(i) & "  ");
            write(currentLine, getDynamicContentString(content(i), adrs(i), vals(i)) & ",   ");

            writeline(outFile, currentLine);
        end loop;

    end procedure;


        procedure stateError(msg: string; oldVal: QEntryState; ind: natural) is
        begin
            report msg & "; at (" & natural'image(ind) & ") = " &  QEntryState'image(oldVal) severity failure;
        end procedure;

        procedure writeState(signal table: inout EntryStateArray; ind: natural; IS_LQ: boolean) is
            variable oldVal, newVal: QEntryState := table(ind);
        begin
            case oldVal is
                when empty =>
                    if IS_LQ then 
                        newVal := val;
                    else
                        newVal := waiting;
                    end if;
                when others => stateError("WRITE: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure commitState(signal table: inout EntryStateArray; ind: natural; IS_LQ: boolean) is
            variable oldVal, newVal: QEntryState := table(ind);
        begin
            case oldVal is
                when completed =>
                    if IS_LQ then 
                        newVal := empty;
                    else
                        newVal := committed;
                    end if;
                when others => stateError("COMMIT: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure drainState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: QEntryState := table(ind);
        begin
            case oldVal is
                when committed => newVal := empty;
                when others => stateError("DRAIN: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure updateState(signal table: inout EntryStateArray; ind: natural; a, v: std_logic; IS_LQ: boolean) is
            variable oldVal, newVal: QEntryState := table(ind);
        begin
            case oldVal is
                when waiting =>
                    if v = '1' then newVal := val; end if;
    
                    if a = '1' then
                        if newVal = val then
                            newVal := completed;
                        else
                            newVal := adr;
                        end if;
                    end if;

                when val =>
                    if v = '1' then stateError("UPDATE: ", oldVal, ind); end if;
                    if a = '1' then newVal := completed; end if;
                when adr =>
                    if v = '1' then newVal := completed; end if;
                    if a = '1' then stateError("UPDATE: ", oldVal, ind); end if; -- We allow multiple replays for loads
                when completed =>
                    if not IS_LQ then stateError("UPDATE: ", oldVal, ind); end if;
                when others => stateError("UPDATE: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure killState(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: QEntryState := table(ind);
        begin
            newVal := empty;
            table(ind) <= newVal;
        end procedure;


        procedure DB_writeStates(signal table: inout EntryStateArray; mask: std_logic_vector; endP: SmallNumber; IS_LQ: boolean) is
            constant n: natural := countOnes(mask);
            variable indSN: SmallNumber := endP;
            variable ind: natural := 0;
        begin
            for i in 0 to n-1 loop
                writeState(table, p2i(addInt(endP, i), table'length), IS_LQ);
            end loop;
        end procedure;

        procedure DB_commitStates(signal table: inout EntryStateArray; mask: std_logic_vector; startP: SmallNumber;  IS_LQ: boolean) is
            constant n: natural := countOnes(mask);
        begin
            for i in 0 to n-1 loop
                commitState(table, p2i(addInt(startP, i), table'length), IS_LQ);
            end loop;
        end procedure;

        procedure DB_drainStates(signal table: inout EntryStateArray; drainP: SmallNumber) is
            constant n: natural := 1;
        begin
            for i in 0 to n-1 loop
                drainState(table, p2i(drainP, table'length));
            end loop;
        end procedure;

        procedure DB_updateAddress(signal table: inout EntryStateArray; en: std_logic; er: ExecResult; ec: ControlPacket; IS_LQ: boolean) is
            constant n: natural := 1;
            variable dest: SmallNumber := sn(0);
        begin
            if en /= '1' then
                return;
            end if;
            
            if IS_LQ then
                dest := ec.tags.lqPointer;
            else
                dest := ec.tags.sqPointer;
            end if;

            case table(p2i(dest, table'length)) is
                when empty => report "Wrong: emp" severity error;
                when waiting => table(p2i(dest, table'length)) <= adr;
                when adr => 
                    if not IS_LQ then -- Repeated loads are possible. For stores, there should be just 1. 
                                      --It will change when proper misses are implemented and every adr operation may fail.
                            report "Wrong: adr" severity error;
                    end if;
                when val => table(p2i(dest, table'length)) <= completed;
                when completed => report "Wrong: comp" severity error;
                when committed => report "Wrong: comm" severity error;
            end case;
        end procedure;

        procedure DB_updateValue(signal table: inout EntryStateArray; en: std_logic; er: ExecResult--; ec: ControlPacket; IS_LQ: boolean
        ) is
            constant n: natural := 1;
            variable dest: SmallNumber := sn(0);
        begin
            if en /= '1' then
                return;
            end if;
            
            dest := er.dest;
            
            case table(p2i(dest, table'length)) is
                when empty => report "v Wrong: emp" severity error;
                when waiting => table(p2i(dest, table'length)) <= val;
                when adr => table(p2i(dest, table'length)) <= completed;
                when val => report "v Wrong: val" severity error;
                when completed => report "v Wrong: comp" severity error;
                when committed => report "v Wrong: comm" severity error;
            end case;
        end procedure;


        procedure DB_updateStates(signal table: inout EntryStateArray; enA, enV: std_logic; adrCt: ControlPacket; valRes: ExecResult; IS_LQ: boolean) is
            constant n: natural := 1;
            variable destA, destV: SmallNumber := sn(0);
        begin
            if IS_LQ then
                destA := adrCt.tags.lqPointer;
            else
                destA := adrCt.tags.sqPointer;
            end if;
            destV := valRes.dest;

            if (enA) = '1' then
                if enV = '1' and p2i(destA, table'length) = p2i(destV, table'length) then
                    updateState(table, p2i(destA, table'length), '1', '1', IS_LQ);
                    return;
                else
                    updateState(table, p2i(destA, table'length), '1', '0', IS_LQ);
                end if;
            end if;

            if (enV) = '1' then
                updateState(table, p2i(destV, table'length), '0', '1', IS_LQ);
            end if;
        end procedure;


        procedure DB_eventStates(signal table: inout EntryStateArray; evt: EventState; startP, drainP: SmallNumber; IS_LQ: boolean) is
            variable killInd: natural := 0;
        begin
            if evt.lateCausing.full = '1' then
                --table <= (others => empty);
                for i in 0 to table'length-1 loop
                    killState(table, i);
                end loop;
                return;
            end if;
            
            if evt.execCausing.full /= '1' then
                return;
            end if;
            
            if IS_LQ then
                killInd := slv2u(evt.execTags.lqPointer) mod table'length;
            else
                killInd := slv2u(evt.execTags.sqPointer) mod table'length;
            end if;
            
            -- TODO: handle the case of full queue - the below will probably kill nothing if exec event is outside the bounds of queue content
            loop
                if     (killInd = p2i(startP, table'length) and IS_LQ)
                    or (killInd = p2i(drainP, table'length) and not IS_LQ) then
                    exit;
                end if;
                --table(killInd) <= empty;
                killState(table, killInd);
                killInd := (killInd + 1) mod table'length;
            end loop;

        end procedure;


end package body;