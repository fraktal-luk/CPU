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

--use work.ForwardingNetwork.all;

use work.LogicROB.all;

use std.textio.all;
use work.CpuText.all;
use work.Assembler.all;


package RobViewing is
    procedure printContent(dynamicContent: DynamicOpInfoArray2D; startP, endP: SmallNumber);
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
end package body;