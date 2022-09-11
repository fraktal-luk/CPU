
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


---- 1 per arg. const if unused
--type WakeupModes is record
--    stage: IntArray(0 to 2);
--    fast:  std_logic_vector(0 to 2);
--    const: std_logic_vector(0 to 2);
--end record;

--type WakeupModesArray is array(0 to 2) of WakeupModes;

--constant DEFAULT_WAKEUP_MODES: WakeupModes := ((others => -5), (others => '0'), (others => '1'));


--constant WAKEUP_MODES_INT_FULL: WakeupModes :=(
--    (-2, -100, -3),
--    ('1', '0', '0'), -- fast on I0
--    ('0', '1', '0')  -- I1 unused
--);

--constant WAKEUP_MODES_ARRAY_I0: WakeupModesArray := (
--    0 => WAKEUP_MODES_INT_FULL,
--    1 => WAKEUP_MODES_INT_FULL,
--    2 => DEFAULT_WAKEUP_MODES
--);



constant FORWARDING_MODES_NONE: ForwardingModeArray(0 to 2) := (
    (-100, false), (-100, false), (-200, false)
);


-- Int
--
-- src:   I0   I1   M0
-- fast: -2    -    -
-- slow: -2    -   -3
-- 
-- stage: -2  -  -3 
-- fast:   1  -   0
-- const:  0  -   0

constant FORWARDING_MODES_INT: ForwardingModeArray(0 to 2) := (
    (-2, false), (-100, false), (-200, false)
);

constant FORWARDING_MODES_INT_D: ForwardingModeArray(0 to 2) := (
    (-2, false), (-2, false), (-3, false)
);


-- FP
--
-- src:   F0   F1   M0
-- fast:  -    -    -
-- slow: -3    -   -1
--
constant FORWARDING_MODES_FLOAT: ForwardingModeArray(0 to 2) := (
    (-200, false), (-100, false), (-100, false) 
);

constant FORWARDING_MODES_FLOAT_D: ForwardingModeArray(0 to 2) := (
    (-3, false), (-100, false), (-1, false) 
);


constant FORWARDING_MODES_SV_INT_D: ForwardingModeArray(0 to 2) := (
    (0, true), (0, false), (0, true) 
);

constant FORWARDING_MODES_SV_FLOAT_D: ForwardingModeArray(0 to 2) := (
    (0, true), (-100, false), (0, true) 
);

end ForwardingNetwork;


package body  ForwardingNetwork is


end ForwardingNetwork;
