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

end package body;