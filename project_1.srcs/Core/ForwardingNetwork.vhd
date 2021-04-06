
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;


use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package ForwardingNetwork is

     
constant WAITING_FN_MAP: ForwardingMap := (
    maskRR => "110",   -- arg2 is unused   
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "101",
    maskM2 => "010"
);        

constant ENQUEUE_FN_MAP: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "111",
    maskM2 => "010"
);

constant SELECTION_FN_MAP: ForwardingMap := (
    maskRR => "110",   -- arg2 is unused   
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "101",
    maskM2 => "000"
);

constant ENQUEUE_FN_MAP_SV: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000"
);

constant WAITING_FN_MAP_SV: ForwardingMap := (
    maskRR => "100",
    maskR1 => "000",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000"
);


-- FP store data
constant ENQUEUE_FN_MAP_FLOAT_SV: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000"
);

constant WAITING_FN_MAP_FLOAT_SV: ForwardingMap := (
    maskRR => "100",   -- arg2 is unused   
    maskR1 => "000",  
    maskR0 => "111",
    maskM1 => "000",
    maskM2 => "000"
);

-- FP cluster
constant WAITING_FN_MAP_FLOAT: ForwardingMap := (
    maskRR => "111",   
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "000",
    maskM2 => "111"
);        

constant ENQUEUE_FN_MAP_FLOAT: ForwardingMap := (
    maskRR => "000",      
    maskR1 => "111",  
    maskR0 => "111",
    maskM1 => "111",
    maskM2 => "111"
);

constant SELECTION_FN_MAP_FLOAT: ForwardingMap := (
    maskRR => "111",
    maskR1 => "000",  
    maskR0 => "000",
    maskM1 => "000",
    maskM2 => "000"
);


end ForwardingNetwork;


package body  ForwardingNetwork is


end ForwardingNetwork;
