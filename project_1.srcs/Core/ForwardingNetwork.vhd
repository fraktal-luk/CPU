
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;


use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package ForwardingNetwork is

-- Possible modes of wakeup:
--    none (no arg or always const)
--    fast (-2; also acting as slow with +1 cycle delay for ops that don't get selected immediately)
--    slow (-3)
--    reg (can read only from reg file, no bypass)

-- Modes differing by source-consumer combinations? including arg0, arg1, arg2 as different consumers?

-- IQ I0 sources: I0 fast; I1 slow; M0 slow + failable // (arg0 and arg1 alike)
-- IQ M0 sources: arg0: {I0 fast; I1 slow; M0 slow + failable}, arg1: {none (constant)}

-- Above is not enough for enqueueing ops; they may need info from multiple stages
-- Maybe additional modes are needed to represent it:
--    init_fast: effectively slow, using src stages {-2, -1} (can't use 1 cycle results immediately, needs +1 delay)
--    init_slow: effectively slow, using src stages {-3,-2,-1} (can use result immediately because of at least 3 cycle producer latency)
--    init_reg:  effectively reg, using src stages  {0, 1}

--  Mode array:
--     legend:    IQ-resident/enqueueing
--
--      \consumer:   I0.0         I0.1         I1.0       I1.1         M0.0        M0.1      SVI
-- producer:
--        I0         F/IF         F/IF         F/IF       F/IF         F/IF        N         R/IR
--        I1         S/IS         S/IS         S/IS       S/IS         S/IS        N         R/IR
--        M0         S/IS(f)      S/IS(f)      S/IS(f)    S/IS(f)      S/IS(f)     N         R/IR
--
--      \consumer:   F0.0         F0.1           SVF
-- producer:
--        F0         S(IS)        S(IS)          R/IR
--        M0*        S/IS(f)      S/IS(f)        R/IR
--  * results from M0 are available to FP part 1 cycle later than to Int part. Detection of fail is therefore 1c faster relative to speculative wakeup than it is at Int part


-- To:  From:
-- I0    I0    M0    F0
--      -2s   -3w    -   // number means source stage of notification, s/w - select(can issue immediately)/wait(can issue next cycle)

-- M0    I0    M0    F0
--      -2s   -3w    -


type WakeupMode is (NONE, CONST, FAST, SLOW, REG, INIT_FAST, INIT_SLOW, INIT_REG);

type WakeupModeArray is array (natural range <>) of WakeupMode;

type WakeupModeSet is array(0 to 1) of WakeupMode;
type IqWakeupModes is array(natural range <>) of WakeupModeSet;

constant DEFAULT_WAKEUP_MODES: IqWakeupModes(0 to 0) := (others => (NONE, NONE));

type WakeupSpec is array (natural range <>, natural range <>) of WakeupMode;

constant DEFAULT_WAKEUP_SPEC: WakeupSpec(0 to 0, 0 to 0) := (others => (others => NONE));

constant WAKEUP_SPEC_I0: WakeupSpec(0 to 1, 0 to 2) := ((FAST, SLOW, SLOW),
                                                        (FAST, SLOW, SLOW));
constant WAKEUP_SPEC_I1: WakeupSpec(0 to 1, 0 to 2) := WAKEUP_SPEC_I0;

constant WAKEUP_SPEC_M0: WakeupSpec(0 to 1, 0 to 2) := ((FAST, SLOW, SLOW),
                                                        (CONST,CONST,CONST)); 

constant WAKEUP_SPEC_SVI: WakeupSpec(0 to 1, 0 to 2) := ((REG, REG, REG),
                                                         (REG, REG, REG));

constant WAKEUP_SPEC_SVF: WakeupSpec(0 to 1, 0 to 1) := ((REG, REG),
                                                         (REG, REG));



-- Table below doesn't include enqueueing modes; but they always match IQ-resident modes with added INIT_ when applicable
--                                          source:     I0              I1           M0
constant WAKEUP_MODES_I0: IqWakeupModes(0 to 2) := ((FAST, FAST),  (SLOW, SLOW),  (SLOW, SLOW));  -- fail+2
constant WAKEUP_MODES_I1: IqWakeupModes(0 to 2) := ((FAST, FAST),  (SLOW, SLOW),  (SLOW, SLOW));  -- fail+2
constant WAKEUP_MODES_M0: IqWakeupModes(0 to 2) := ((FAST, CONST), (SLOW, CONST), (SLOW, CONST)); -- fail+2

constant WAKEUP_MODES_SVI: IqWakeupModes(0 to 2) := ((REG, NONE),  (REG, NONE),  (REG, NONE)); -- fail N/A

--                                          source:       F0           M0
constant WAKEUP_MODES_SVF: IqWakeupModes(0 to 1) := ((REG, NONE),  (REG, NONE));   -- fail N/A

constant WAKEUP_MODES_F0:  IqWakeupModes(0 to 1) := ((SLOW, SLOW), (SLOW, SLOW));  -- fail+1

-- By definition, memFail applies to M0 results. CAREFUL: in Int part, fail shows 2 cycles after slow wakeup; in FP part, 1 cycle after slow wakeup


type ForwardingMode is record
    stage: integer;
    delayed: boolean; -- true - W, false - S  
end record;

type ForwardingModeArray is array (natural range <>) of ForwardingMode;


---- 1 per arg. const if unused
type BypassState is record
    used:  std_logic_vector(0 to 2);
    usedFast:  std_logic_vector(0 to 2);
    obj:   ExecResultArray(0 to 2);
    objNext:   ExecResultArray(0 to 2);
    objNext2:   ExecResultArray(0 to 2);
    objTags: SmallNumberArray(0 to 2);
    stage: IntArray(0 to 2);
    phase: IntArray(0 to 2);
    memFail: std_logic;
end record;

constant DEFAULT_BYPASS_STATE: BypassState := (
    used => (others => '0'),
    usedFast => (others => '0'),
    obj => (others => DEFAULT_EXEC_RESULT),
    objNext => (others => DEFAULT_EXEC_RESULT),
    objNext2 => (others => DEFAULT_EXEC_RESULT),
    objTags => (others => sn(0)),
    stage => (others => -4),
    phase => (others => -4),
    memFail => '0'
);


constant FORWARDING_MODES_NONE: ForwardingModeArray(0 to 2) := (
    (-100, false), (-100, false), (-200, false)
);


-- Int
--
-- src:   I0   I1   M0
-- fast: -2    -    -
-- slow: -2   -2   -3
-- 
-- stage: -2  -2  -3 
-- fast:   1  -   0
-- const:  0  -   0

constant FORWARDING_MODES_INT: ForwardingModeArray(0 to 2) := (
    (-2, false), (-100, false), (-200, false)
);

constant FORWARDING_MODES_INT_D: ForwardingModeArray(0 to 2) := (
    (-2, false), (-2, false), (-3, false)
);
--  Slow wakeups for Int:
-- 
--     subpipeI0_Issue_N,   subpipeI1_E1_N,    subpipeM0_RegRead_N
--          -2 -> 0,          -2 -> 0,             -3 -> -1
--            true             true                  true


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
--            constant CFG_MEM: work.LogicIssue.SchedulerUpdateConfig := (true, false, false, false, FORWARDING_MODES_INT_D);  (-2, -2, -3)  [[-2,-1], [-2,-1], [-3,-1]]

--            constant CFG_SVI: work.LogicIssue.SchedulerUpdateConfig := (true, false, false, true, FORWARDING_MODES_SV_INT_D);   (0, 0, 0)     [[0, 1 ],  [0, 1], [0, 1]]
--            constant CFG_SVF: work.LogicIssue.SchedulerUpdateConfig := (true, false, true, true, FORWARDING_MODES_SV_FLOAT_D); (0, -, 0)     [[0, 1 ]   [-- ],  [0, 1]]
--            constant CFG_FP0: work.LogicIssue.SchedulerUpdateConfig := (true, false, true, false, FORWARDING_MODES_FLOAT_D);  (-3, --, -1)  [[-3, -1], [ --],  [-1 ] ]



type ForwardingInfo is record
	nextTagsM3:	PhysNameArray(0 to 2);
	nextTagsM2:	PhysNameArray(0 to 2);
	nextTagsM1: PhysNameArray(0 to 2);
	tags0: PhysNameArray(0 to 2);
	tags1: PhysNameArray(0 to 2);
	values0: MwordArray(0 to 2);
	values1: MwordArray(0 to 2);
	
        iqTagsM3: SmallNumberArray(0 to 2);
        iqTagsM2: SmallNumberArray(0 to 2);
        iqTagsM1: SmallNumberArray(0 to 2);
        iqTags0:  SmallNumberArray(0 to 2);
        iqTags1:  SmallNumberArray(0 to 2);

	failedM2: std_logic_vector(0 to 2);	
	failedM1: std_logic_vector(0 to 2);	
	failed0: std_logic_vector(0 to 2);	
	failed1: std_logic_vector(0 to 2);
	
	memFail: std_logic;
	memDepFail: std_logic;
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
    failed1 => (others => '0'),
    
    memFail => '0',
    memDepFail => '0'
);

constant DEFAULT_FORWARDING_MATCHES: ForwardingMatches := (
    cmps => (others => DEFAULT_FORWARDING_COMPARISONS)
--    others => (others => '0')
);


function buildForwardingNetwork(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1: ExecResult;
                                s1_M3, s1_M2, s1_M1, s1_R0, s1_R1: ExecResult;
                                s2_M3, s2_M2, s2_M1, s2_R0, s2_R1: ExecResult;
                                memFail, memDepFail: std_logic
         -- : ExecResult
) return ForwardingInfo;

function buildForwardingNetworkFP(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                  s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                  s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult;
                                  memFail, memDepFail: std_logic
) return ForwardingInfo;


end ForwardingNetwork;


package body  ForwardingNetwork is


function buildForwardingNetwork(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1: ExecResult;
                                s1_M3, s1_M2, s1_M1, s1_R0, s1_R1: ExecResult;
                                s2_M3, s2_M2, s2_M1, s2_R0, s2_R1: ExecResult;
                                memFail, memDepFail: std_logic
) return ForwardingInfo is
    variable fni: ForwardingInfo := DEFAULT_FORWARDING_INFO;
begin
    fni.nextTagsM3 := (0 => s0_M3.dest,                              1 => s1_M3.dest,                               2 => s2_M3.dest,                             others => (others => '0'));
    fni.nextTagsM2 := (0 => s0_M2.dest,                              1 => s1_M2.dest,                               2 => s2_M2.dest,                             others => (others => '0'));
    fni.nextTagsM1 := (0 => s0_M1.dest,                              1 => s1_M1.dest,                               2 => s2_M1.dest,                             others => (others => '0'));        
    fni.tags0 :=      (0 => s0_R0.dest,                              1 => s1_R0.dest,                               2 => s2_R0.dest,                             others => (others => '0')); 
    fni.tags1 :=      (0 => s0_R1.dest,                              1 => s1_R1.dest,                               2 => s2_R1.dest,                             others => (others => '0'));
    
--    fni.iqTagsM3 :=   (0 => s0_M3.iqTag,                             1 => s1_M3.iqTag,                              2 => s2_M3.iqTag,                            others => (others => '0'));
--    fni.iqTagsM2 :=   (0 => s0_M2.iqTag,                             1 => s1_M2.iqTag,                              2 => s2_M2.iqTag,                            others => (others => '0'));
--    fni.iqTagsM1 :=   (0 => s0_M1.iqTag,                             1 => s1_M1.iqTag,                              2 => s2_M1.iqTag,                            others => (others => '0'));        
--    fni.iqTags0 :=    (0 => s0_R0.iqTag,                             1 => s1_R0.iqTag,                              2 => s2_R0.iqTag,                            others => (others => '0')); 
--    fni.iqTags1 :=    (0 => s0_R1.iqTag,                             1 => s1_R1.iqTag,                              2 => s2_R1.iqTag,                            others => (others => '0'));
    
    fni.values0 :=    (0 => s0_R0.value,                             1 => s1_R0.value,                              2 => s2_R0.value,                            others => (others => '0'));
    fni.values1 :=    (0 => s0_R1.value,                             1 => s1_R1.value,                              2 => s2_R1.value,                            others => (others => '0'));                 
    
    fni.failedM2 :=   (0 => s0_M2.failed,                            1 => s1_M2.failed,                             2 => s2_M2.failed,                           others => '0');                 
    fni.failedM1 :=   (0 => s0_M1.failed,                            1 => s1_M1.failed,                             2 => s2_M1.failed,                           others => '0');                 
    fni.failed0  :=   (0 => s0_R0.failed,                            1 => s1_R0.failed,                             2 => s2_R0.failed,                           others => '0');                 
    fni.failed1  :=   (0 => s0_R1.failed,                            1 => s1_R1.failed,                             2 => s2_R1.failed,                           others => '0');                 

    fni.memFail := memFail;
    fni.memDepFail := memDepFail; 
    
    return fni;
end function;

function buildForwardingNetworkFP(s0_M3, s0_M2, s0_M1, s0_R0, s0_R1,
                                  s1_M3, s1_M2, s1_M1, s1_R0, s1_R1,
                                  s2_M3, s2_M2, s2_M1, s2_R0, s2_R1
          : ExecResult;
                                  memFail, memDepFail: std_logic

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

    fni.memFail := memFail;
    fni.memDepFail := memDepFail;

    return fni;
end function;


end ForwardingNetwork;
