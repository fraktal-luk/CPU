
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;


use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package ForwardingNetwork is

-- To:  From:
-- I0    I0    M0    F0
--      -2s   -3w    -   // number means source stage of notification, s/w - select(can issue immediately)/wait(can issue next cycle)

-- M0    I0    M0    F0
--      -2s   -3w    -

type ForwardingMode is record
    stage: integer;
    delayed: boolean; -- true - W, false - S  
end record;

type ForwardingModeArray is array (natural range <>) of ForwardingMode;

constant FORWARDING_MODES_INT: ForwardingModeArray(0 to 2) := (
    (-2, false), (-100, false), (-200, false)
);

constant FORWARDING_MODES_INT_D: ForwardingModeArray(0 to 2) := (
    (-2, false), (-100, false), (-3, false)
);


constant FORWARDING_MODES_FLOAT: ForwardingModeArray(0 to 2) := (
    (-200, false), (-100, false), (-100, false) 
);

constant FORWARDING_MODES_FLOAT_D: ForwardingModeArray(0 to 2) := (
    (-3, false), (-100, false), (-1, false) 
);


constant FORWARDING_MODES_SV_INT_D: ForwardingModeArray(0 to 2) := (
    (0, true), (-100, false), (0, true) 
);

constant FORWARDING_MODES_SV_FLOAT_D: ForwardingModeArray(0 to 2) := (
    (0, true), (-100, false), (0, true) 
);

end ForwardingNetwork;


package body  ForwardingNetwork is


end ForwardingNetwork;
