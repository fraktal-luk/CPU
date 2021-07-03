
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
    (-2, false), (-100, false), --(-3, true)
                                (-2, false)
);

constant FORWARDING_MODES_FLOAT: ForwardingModeArray(0 to 2) := (
    --(-3, true), (-100, false), (-3, true)
    (-2, false), (-100, false), (0, false) 
);

constant FORWARDING_MODES_SV_INT_D: ForwardingModeArray(0 to 2) := (
    (0, true), (-100, false), (0, true) 
);

constant FORWARDING_MODES_SV_FLOAT_D: ForwardingModeArray(0 to 2) := (
    (0, true), (-100, false), (0, true) 
);

constant ENQUEUE_FN_MAP: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "111",
    maskM2 => "110",
    maskM3 => "000"
);
     
constant WAITING_FN_MAP: ForwardingMap := (
    maskRR => "110",   -- arg2 is unused   
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "001",
    maskM2 => "110",
    maskM3 => "000"
    --    maskM1 => "000",
    --    maskM2 => "110"
);

constant SELECTION_FN_MAP: ForwardingMap := (
    maskRR => "110",   -- arg2 is unused   
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "001",
    maskM2 => "100",
    maskM3 => "000"
    --    maskM1 => "000",
    --    maskM2 => "110"
);

-------------

constant ENQUEUE_FN_MAP_SV: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000",
    maskM3 => "000"
);

constant WAITING_FN_MAP_SV: ForwardingMap := (
    maskRR => "100",
    maskR1 => "000",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000",
    maskM3 => "000"
);


-- FP store data
constant ENQUEUE_FN_MAP_FLOAT_SV: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000",
    maskM3 => "000"
);

constant WAITING_FN_MAP_FLOAT_SV: ForwardingMap := (
    maskRR => "100",   -- arg2 is unused   
    maskR1 => "000",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000",
    maskM3 => "000"
);

-- FP cluster        

constant ENQUEUE_FN_MAP_FLOAT: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "111",
    maskM2 => "111",
    maskM3 => "000"
);

constant WAITING_FN_MAP_FLOAT: ForwardingMap := (
    maskRR => "111",   
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "000",
    maskM2 => "111",
    maskM3 => "000"
);

constant SELECTION_FN_MAP_FLOAT: ForwardingMap := (
    maskRR => "111",
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "000",
    maskM2 => "000",
    maskM3 => "000"
);


end ForwardingNetwork;


package body  ForwardingNetwork is


end ForwardingNetwork;
