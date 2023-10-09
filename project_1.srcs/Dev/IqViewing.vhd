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

use work.ForwardingNetwork.all;

use std.textio.all;
use work.CpuText.all;
use work.Assembler.all;


package IqViewing is
                procedure printContent(NAME: string; queueContent: SchedulerInfoArray);

                function TMP_lastEvent(current, prev: SchedulerInfo; killed: std_logic) return IqEvent;

        type EntryState is (empty, suspended, active, issued);
        type EntryStateArray is array(natural range <>) of EntryState;

        procedure stateError(msg: string; oldVal: EntryState; ind: natural);
        procedure writeEntry(signal table: inout EntryStateArray; ind: natural);
        procedure issueEntry(signal table: inout EntryStateArray; ind: natural);
        procedure pullbackEntry(signal table: inout EntryStateArray; ind: natural);
        procedure freeEntry(signal table: inout EntryStateArray; ind: natural);
        procedure killEntry(signal table: inout EntryStateArray; ind: natural);

        procedure DB_writeStates(signal table: inout EntryStateArray; locs: slv2D);
        procedure DB_issueStates(signal table: inout EntryStateArray; mask: std_logic_vector);
        procedure DB_freeStates(signal table: inout EntryStateArray; mask: std_logic_vector);
        procedure DB_retractStates(signal table: inout EntryStateArray; pullbackMask, retractMask0, retractMask1: std_logic_vector);
        procedure DB_killStates(signal table: inout EntryStateArray; lateEvent: std_logic; trialMask: std_logic_vector);

end package;


package body IqViewing is


        procedure stateError(msg: string; oldVal: EntryState; ind: natural) is
        begin
            report msg & "; at (" & natural'image(ind) & ") = " &  EntryState'image(oldVal) severity failure;
        end procedure;
       
        procedure writeEntry(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when empty => newVal := active; -- TODO: suspended if applicable
                when suspended | active | issued => stateError("WRITE: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure issueEntry(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when empty | suspended | issued => stateError("ISSUE: ", oldVal, ind);
                when active => newVal := issued;
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure pullbackEntry(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when issued => newVal := active; -- TODO: or suspended
                when others => stateError("PULLBACK: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure freeEntry(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when issued => newVal := empty; -- TODO: or suspended
                when others => stateError("FREE: ", oldVal, ind);
            end case;
            table(ind) <= newVal;
        end procedure;

        procedure killEntry(signal table: inout EntryStateArray; ind: natural) is
            variable oldVal, newVal: EntryState := table(ind);
        begin
            case oldVal is
                when others => newVal := empty;
            end case;
            table(ind) <= newVal;
        end procedure;


                function getArgString(argState: ArgumentState) return string is
                    variable immValue: Hword := argState.value;
                begin
                    if false then --argState.imm_T = '1' then
                        if IMM_AS_REG then
                            immValue(PhysName'length-2 downto 0) := argState.reg(6 downto 0);
                        end if;
                        return "{#" & integer'image(slv2u(immValue)) & "}";
                    elsif argState.zero_T = '1' then
                        return "{zero}";
                    else
                        if argState.waiting = '1' then
                            return "{" & natural'image(slv2u(argState.reg)) & " [0]}";
                        else
                            return "{" & natural'image(slv2u(argState.reg)) & " [1]}";                
                        end if;
                    end if;
    
                end function;
    
                
                function vec2digit(v: std_logic_vector(3 downto 0)) return character is
                    constant DIGITS: string := "0123456789abcdef";
                begin
                    return DIGITS(1 + slv2u(v));
                end function;

                -- CAREFUL: here we assume bit order is big endian
                function hexString(v: std_logic_vector) return string is
                    constant BIT_LEN: natural := v'length;
                    constant HEX_LEN: natural := (BIT_LEN+3)/4;        
                    variable res: string(1 to HEX_LEN) := (others => '0');
                    variable digit: character := ' ';
                    variable rightIndex: natural := 0;
                begin
                    for strIndex in 0 to HEX_LEN-1 loop
                        rightIndex := 4*strIndex;
                    
                        digit := vec2digit(v(rightIndex+3 downto rightIndex));
                        res(HEX_LEN-strIndex) := digit;
                    end loop;

                    rightIndex := 4*(HEX_LEN-1);
                    res(1) := vec2digit(v(v'left downto rightIndex));
                
                    return res;
                end function;

                procedure printContent(NAME: string; queueContent: SchedulerInfoArray) is
                   constant IQ_SIZE: natural := queueContent'length; 
                   file outFile: text open write_mode is "issue_queue" & NAME & ".txt";
                   variable preRow, currentLine: line := null;
                begin
                    -- pragma synthesis off
                
                    for i in 0 to IQ_SIZE-1 loop
                        currentLine := null;
                        write(currentLine, natural'image(i) & ": ");
                        if queueContent(i).dynamic.full /= '1' then
                            writeline(outFile, currentLine);
                            next;
                        end if;

                        write(currentLine, slv2hex(queueContent(i).static.dbInfo.seqNum));
                        write(currentLine, string'(": "));

                        write(currentLine, slv2hex(queueContent(i).dynamic.renameIndex));
                        write(currentLine, string'(", "));
                        write(currentLine, std_logic'image(queueContent(i).dynamic.status_N.issued0
                                                        or queueContent(i).dynamic.status_N.issued1
                                                        or queueContent(i).dynamic.status_N.issued2
                                                            ));
                        write(currentLine, string'(", "));
    
                        write(currentLine, getArgString(queueContent(i).dynamic.argStates(0)));
                        write(currentLine, string'(", "));
                        write(currentLine, getArgString(queueContent(i).dynamic.argStates(1)));
    
                        write(currentLine, string'(" // "));
    
                        write(currentLine, disasmWord(queueContent(i).static.dbInfo.bits));
                        writeline(outFile, currentLine);
                    end loop;
                    
                    -- pragma synthesis on
                end procedure;

                function TMP_lastEvent(current, prev: SchedulerInfo; killed: std_logic) return IqEvent is
                    constant ps: EntryStatus_N := prev.dynamic.status_N;
                    constant cs: EntryStatus_N := current.dynamic.status_N;
                begin
                    if killed = '1' and (ps.active or ps.suspended or ps.issued0 or ps.issued1 or ps.issued2) = '1' then
                        return kill;
                    elsif cs.issued0 = '1' then
                        return issue;
                    elsif (cs.suspended or cs.active) = '1' and (ps.issued0 or ps.issued1 or ps.issued2) = '1' then
                        return retract;
                    elsif (cs.suspended or cs.active) = '1' and ps.suspended = '0' and ps.active = '0' then
                        return insert;
                    elsif cs.active = '0' and cs.suspended = '0' and ps.issued2 = '1' then
                        return retire;
                    else
                        return none;
                    end if;
                    
                    return none;
                end function;

        procedure DB_writeStates(signal table: inout EntryStateArray; locs: slv2D) is
        begin
            for s in 0 to PIPE_WIDTH-1 loop
                for i in 0 to table'length-1 loop
                    if locs(i, s) = '1' then
                        writeEntry(table, i);
                    end if;
                end loop;
            end loop;
        end procedure;

        procedure DB_issueStates(signal table: inout EntryStateArray; mask: std_logic_vector) is
        begin
            for i in 0 to table'length-1 loop
                if mask(i) = '1' then
                    issueEntry(table, i);
                end if;
            end loop;
        end procedure;

        procedure DB_freeStates(signal table: inout EntryStateArray; mask: std_logic_vector) is
        begin
            for i in 0 to table'length-1 loop
                if mask(i) = '1' then
                    freeEntry(table, i);
                end if;
            end loop;
        end procedure;

        procedure DB_retractStates(signal table: inout EntryStateArray; pullbackMask, retractMask0, retractMask1: std_logic_vector) is
        begin
            for i in 0 to table'length-1 loop
                if pullbackMask(i) = '1' then
                    pullbackEntry(table, i);
                end if;
            end loop;
        end procedure;

        procedure DB_killStates(signal table: inout EntryStateArray; lateEvent: std_logic; trialMask: std_logic_vector) is
        begin
            if lateEvent = '1' then
                for i in 0 to table'length-1 loop
                    killEntry(table, i);
                end loop;
            end if;
            
            for i in 0 to table'length-1 loop
                if trialMask(i) = '1' then
                    killEntry(table, i);
                end if;
            end loop;
        end procedure;


end package body;