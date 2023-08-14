
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use std.textio.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.Arith.all;

use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package LogicLogging is

                -- Redundant? Event log feature already exists
                procedure DB_reportBranchEvent(cp: ControlPacket);
    procedure logEvent(file eventLog: text; lateEventSignal, execEventSignalE0, frontEventSignal, stallDetected: std_logic; cycleCount: natural);

end LogicLogging;



package body LogicLogging is

                -- Redundant? Event log feature already exists
                procedure DB_reportBranchEvent(cp: ControlPacket) is
                begin
                    -- pragma synthesis off
                    if DB_BRANCH_EXEC_TRACKING and cp.controlInfo.full = '1' and cp.controlInfo.newEvent = '1' then
                        report "";
                        report "DEBUG: branch redirect: " & natural'image(slv2u(cp.dbInfo.seqNum));
                        report "";
                        report "";
                    end if;
                    -- pragma synthesis on
                end procedure;
                
        procedure logEvent(file eventLog: text; lateEventSignal, execEventSignalE0, frontEventSignal, stallDetected: std_logic; cycleCount: natural) is
            variable currentLine: line := null;
        begin
            -- Event tracking
            if stallDetected = '1' then
                write(currentLine, natural'image(cycleCount));
                write(currentLine, string'(": Stall!"));                
            elsif lateEventSignal = '1' then
                write(currentLine, natural'image(cycleCount));
                write(currentLine, string'(": Late"));
            elsif execEventSignalE0 = '1' then
                write(currentLine, natural'image(cycleCount));
                write(currentLine, string'(": Exec"));
            elsif frontEventSignal = '1' then
                write(currentLine, natural'image(cycleCount));
                write(currentLine, string'(": Front"));
            end if;
            writeline(eventLog, currentLine);     
        end procedure;
end LogicLogging;
