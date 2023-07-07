--

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.ForwardingNetwork.all;

use std.textio.all;
use work.CpuText.all;
use work.Assembler.all;


package IqViewing is
                procedure printContent(NAME: string; queueContent: SchedulerInfoArray);

                function TMP_lastEvent(current, prev: SchedulerInfo; killed: std_logic) return IqEvent;

end package;


package body IqViewing is


                function getArgString(argState: ArgumentState) return string is
                    variable immValue: Hword := argState.value;
                begin
                    if argState.imm = '1' then
                        if IMM_AS_REG then
                            immValue(PhysName'length-2 downto 0) := argState.reg(6 downto 0);
                        end if;
                        return "{#" & integer'image(slv2u(immValue)) & "}";
                    elsif argState.zero = '1' then
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
                        write(currentLine, std_logic'image(queueContent(i).dynamic.status.issued));
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
                begin
                    if killed = '1' and prev.dynamic.status.state /= empty then
                        return kill;
                    elsif current.dynamic.status.state = issued and prev.dynamic.status.state = active then
                        return issue;
                    elsif (current.dynamic.status.state = suspended or current.dynamic.status.state = active) and prev.dynamic.status.state = issued then
                        return retract;
                    elsif (current.dynamic.status.state = suspended or current.dynamic.status.state = active) and prev.dynamic.status.state = empty then
                        return insert;
                    elsif current.dynamic.status.state = empty and prev.dynamic.status.state = issued then
                        return retire;
                    else
                        return none;
                    end if;
                    
                    return none;
                end function;

end package body;