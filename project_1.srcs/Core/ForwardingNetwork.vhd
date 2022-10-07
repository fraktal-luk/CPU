
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


--            constant CFG_ALU: work.LogicIssue.SchedulerUpdateConfig := (true, false, false, false, FORWARDING_MODES_INT_D); (-2, -2, -3)
--            constant CFG_MUL: work.LogicIssue.SchedulerUpdateConfig := (true, false, false, false, FORWARDING_MODES_INT_D); (-2, -2, -3)
--            constant CFG_MEM: work.LogicIssue.SchedulerUpdateConfig := (true, false, true, false, FORWARDING_MODES_INT_D);  (-2, -2, -3)  [[-2,-1], [-2,-1], [-3,-1]]

--            constant CFG_SVI: work.LogicIssue.SchedulerUpdateConfig := (true, false, true, true, FORWARDING_MODES_SV_INT_D);   (0, 0, 0)     [[0, 1 ],  [0, 1], [0, 1]]
--            constant CFG_SVF: work.LogicIssue.SchedulerUpdateConfig := (true, false, true, true, FORWARDING_MODES_SV_FLOAT_D); (0, -, 0)     [[0, 1 ]   [-- ],  [0, 1]]
--            constant CFG_FP0: work.LogicIssue.SchedulerUpdateConfig := (true, false, false, false, FORWARDING_MODES_FLOAT_D);  (-3, --, -1)  [[-3, -1], [ --],  [-1 ] ]



type ForwardingInfo is record
	nextTagsM3:	PhysNameArray(0 to 2);
	nextTagsM2:	PhysNameArray(0 to 2);
	nextTagsM1: PhysNameArray(0 to 2);
	tags0: PhysNameArray(0 to 2);
	tags1: PhysNameArray(0 to 2);
	values0: MwordArray(0 to 2);
	values1: MwordArray(0 to 2);
	
        iqTagsM3:	PhysNameArray(0 to 2);
        iqTagsM2:    PhysNameArray(0 to 2);
        iqTagsM1: PhysNameArray(0 to 2);
        iqTags0: PhysNameArray(0 to 2);
        iqTags1: PhysNameArray(0 to 2);

	failedM2: std_logic_vector(0 to 2);	
	failedM1: std_logic_vector(0 to 2);	
	failed0: std_logic_vector(0 to 2);	
	failed1: std_logic_vector(0 to 2);	
end record;


type ForwardingComparisons is record
    cmpM3: std_logic_vector(0 to 2);
    cmpM2: std_logic_vector(0 to 2);
    cmpM1: std_logic_vector(0 to 2);
    cmp0: std_logic_vector(0 to 2);
    cmp1: std_logic_vector(0 to 2);
    reg:  std_logic;
end record;

constant DEFAULT_FORWARDING_COMPARISONS: ForwardingComparisons := (reg => '0', others => (others => '0'));

type ForwardingComparisonsArray is array(natural range <>) of ForwardingComparisons;

type ForwardingMatches is record
    -- src0
    a0cmpM3: std_logic_vector(0 to 2);
    a0cmpM2: std_logic_vector(0 to 2);
    a0cmpM1: std_logic_vector(0 to 2);
    a0cmp1: std_logic_vector(0 to 2);    
	a0cmp0: std_logic_vector(0 to 2);
    
    -- src1
	a1cmpM3: std_logic_vector(0 to 2);
	a1cmpM2: std_logic_vector(0 to 2);
	a1cmpM1: std_logic_vector(0 to 2);
	a1cmp1: std_logic_vector(0 to 2);	    
	a1cmp0: std_logic_vector(0 to 2);
	
	cmps: ForwardingComparisonsArray(0 to 1);
end record;

type ForwardingMatchesArray is array(integer range <>) of ForwardingMatches; 


constant DEFAULT_FORWARDING_INFO: ForwardingInfo := (
	nextTagsM3 => (others => (others => '0')),
	nextTagsM2 => (others => (others => '0')),
	nextTagsM1 => (others => (others => '0')),
    tags0 => (others => (others => '0')),
    tags1 => (others => (others => '0')),
    
        iqTagsM3 => (others => (others => '0')),
        iqTagsM2 => (others => (others => '0')),
        iqTagsM1 => (others => (others => '0')),
        iqTags0 => (others => (others => '0')),
        iqTags1 => (others => (others => '0')),

    values0 => (others => (others => '0')),
    values1 => (others => (others => '0')),
    failedM2 => (others => '0'),
    failedM1 => (others => '0'),
    failed0 => (others => '0'),
    failed1 => (others => '0')
);

constant DEFAULT_FORWARDING_MATCHES: ForwardingMatches := (
    cmps => (others => DEFAULT_FORWARDING_COMPARISONS),
    others => (others => '0')
);


function buildForwardingNetwork(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1: ExecResult_N;
                                s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo;

function buildForwardingNetworkFP(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                  s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                  s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo;


end ForwardingNetwork;


package body  ForwardingNetwork is


function buildForwardingNetwork(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1: ExecResult_N;
                                s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo is
    variable fni: ForwardingInfo := DEFAULT_FORWARDING_INFO;
begin
         -- Forwarding network
		 fni.nextTagsM3 := (0 => s0_M3.dest,                              1 => s1_M3.dest,                               2 => s2_M3.dest,                             others => (others => '0'));
		 fni.nextTagsM2 := (0 => s0_M2.dest,                              1 => s1_M2.dest,                               2 => s2_M2.dest,                             others => (others => '0'));
		 fni.nextTagsM1 := (0 => s0_M1.dest,                              1 => s1_M1.dest,                               2 => s2_M1.dest,                             others => (others => '0'));        
         fni.tags0 :=      (0 => s0_R0.dest,                              1 => s1_R0.dest,                               2 => s2_R0.dest,                             others => (others => '0')); 
         fni.tags1 :=      (0 => s0_R1.dest,                              1 => s1_R1.dest,                               2 => s2_R1.dest,                             others => (others => '0'));

		 fni.iqTagsM3 :=   (0 => s0_M3.iqTag,                                                                                                                          others => (others => '0'));
		 fni.iqTagsM2 :=   (0 => s0_M2.iqTag,                                                                                                                          others => (others => '0'));
		 fni.iqTagsM1 :=   (0 => s0_M1.iqTag,                                                                                                                          others => (others => '0'));        
         fni.iqTags0 :=    (0 => s0_R0.iqTag,                                                                                                                          others => (others => '0')); 
         fni.iqTags1 :=    (0 => s0_R1.iqTag,                                                                                                                          others => (others => '0'));

         fni.values0 :=    (0 => s0_R0.value,                             1 => s1_R0.value,                              2 => s2_R0.value,                            others => (others => '0'));
         fni.values1 :=    (0 => s0_R1.value,                             1 => s1_R1.value,                              2 => s2_R1.value,                            others => (others => '0'));                 
         fni.failedM2 :=   (0 => s0_M2.failed,                            1 => s1_M2.failed,                             2 => s2_M2.failed,                           others => '0');                 
         fni.failedM1 :=   (0 => s0_M1.failed,                            1 => s1_M1.failed,                             2 => s2_M1.failed,                           others => '0');                 
         fni.failed0  :=   (0 => s0_R0.failed,                            1 => s1_R0.failed,                             2 => s2_R0.failed,                           others => '0');                 
         fni.failed1  :=   (0 => s0_R1.failed,                            1 => s1_R1.failed,                             2 => s2_R1.failed,                           others => '0');                 

    return fni;
end function;

function buildForwardingNetworkFP(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                  s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                  s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult
) return ForwardingInfo is
    variable fni: ForwardingInfo := DEFAULT_FORWARDING_INFO;
begin
         -- Forwarding network
		 fni.nextTagsM3 := (0 => s0_M3.dest,                                                                             2 => s2_M3.dest,                             others => (others => '0'));
		 fni.nextTagsM2 := (0 => s0_M2.dest,                                                                             2 => s2_M2.dest,                             others => (others => '0'));
		 fni.nextTagsM1 := (0 => s0_M1.dest,                                                                             2 => s2_M1.dest,                             others => (others => '0'));        
         fni.tags0 :=      (0 => s0_R0.dest,                                                                             2 => s2_R0.dest,                             others => (others => '0')); 
         fni.tags1 :=      (0 => s0_R1.dest,                                                                             2 => s2_R1.dest,                             others => (others => '0'));
         fni.values0 :=    (0 => s0_R0.value,                                                                            2 => s2_R0.value,                            others => (others => '0'));
         fni.values1 :=    (0 => s0_R1.value,                                                                            2 => s2_R1.value,                            others => (others => '0'));                 
         fni.failedM2 :=   (0 => s0_M2.failed,                                                                           2 => s2_M2.failed,                           others => '0');                 
         fni.failedM1 :=   (0 => s0_M1.failed,                                                                           2 => s2_M1.failed,                           others => '0');                 
         fni.failed0  :=   (0 => s0_R0.failed,                                                                           2 => s2_R0.failed,                           others => '0');                 
         fni.failed1  :=   (0 => s0_R1.failed,                                                                           2 => s2_R1.failed,                           others => '0');                 

    return fni;
end function;


end ForwardingNetwork;
