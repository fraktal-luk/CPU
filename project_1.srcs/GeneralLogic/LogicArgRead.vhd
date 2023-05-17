
library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.ForwardingNetwork.all;


package LogicArgRead is

    function TMP_clearFull_DUMMY(ss: SchedulerState; evts: EventState) return SchedulerState;
--    function TMP_clearFull(ss: SchedulerState; evts: EventState) return SchedulerState;

    -- Issue stage
    ----------------------------------------
    ----------------------------------------
    function getDispatchArgValues_Is(input: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState;
        
    function updateDispatchArgs_Is_O(st: SchedulerState; ctSigs: IssueQueueSignals) return SchedulerState;
    function updateDispatchArgs_Is_N(st: SchedulerState; ctSigs: IssueQueueSignals) return SchedulerState;
    
    
    -- Reg read stage
    function getDispatchArgValues_RR_O(input: SchedulerState;
                                     prevSending: std_logic;
                                     events: EventState;
                                     vals0, vals1: MwordArray;
                                     USE_IMM: boolean; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false)
    return SchedulerState;
    
    function getDispatchArgValues_RR_N(input: SchedulerState;
                                     --prevSending: std_logic;
                                     events: EventState;
                                     vals0, vals1: MwordArray;
                                     USE_IMM: boolean; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false)
    return SchedulerState;
    
    function updateDispatchArgs_RR(st: SchedulerState; vals: MwordArray; regValues: MwordArray; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false) return SchedulerState;
    ----------------------------------------------
    ----------------------------------------------
end LogicArgRead;


package body LogicArgRead is


--        function TMP_clearFull(ss: SchedulerState; evts: EventState) return SchedulerState is
--            variable res: SchedulerState := ss;
--        begin
--            if evts.lateEvent = '1' then
--                res.full := '0';
--            end if;
            
--            return res;
--        end function;
        
        function TMP_clearFull_DUMMY(ss: SchedulerState; evts: EventState) return SchedulerState is
            variable res: SchedulerState := ss;
        begin            
            return res;
        end function;
    ------------------------------------------
    ------------------------------------------
    -- issue
    function TMP_clearDestIfEmpty(input: SchedulerState) return SchedulerState is
        variable res: SchedulerState := input;
    begin
        if res.full /= '1' then
           res.argSpec.dest := (others => '0');
           res.destTag := (others => '0');
        end if;

        return res;
    end function;


    function getDispatchArgValues_Is(input: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState is
        variable res: SchedulerState := input;
    begin
        res.full := ctSigs.sending;
        res := TMP_clearDestIfEmpty(res);
        
            if IMM_AS_REG then
              --  res.st.immValue(PhysName'length-2 downto 0) := res.argSpec.args(1)(6 downto 0);
            end if;
        
        --res := TMP_clearFull(res, events);
        if events.lateEvent = '1' then
            res.full := '0';
        end if;
        return res;
    end function;
    
    function updateDispatchArgs_Is_O(st: SchedulerState; ctSigs: IssueQueueSignals) return SchedulerState is
        variable res: SchedulerState := st;
    begin
        res.readNew(0) := bool2std(res.argSrc(0)(1 downto 0) = "11");
        res.readNew(1) := bool2std(res.argSrc(1)(1 downto 0) = "11");
    
        if res.argSrc(0)(1) /= '1' then
            res.argSpec.args(0) := (others => '0');
        end if;
    
        if res.argSrc(1)(1) /= '1' or res.st.zero(1) = '1' then
            res.argSpec.args(1) := (others => '0');
        end if;
        
        return res;
    
    end function;

        function TMP_applyKill(st: SchedulerState; ctSigs: IssueQueueSignals) return SchedulerState is
            variable res: SchedulerState := st;
        begin
            res.full := st.full and not (ctSigs.cancelled or ctSigs.killFollowerNext);
            return res;
        end function;

        function updateDispatchArgs_Is_N(st: SchedulerState; ctSigs: IssueQueueSignals) return SchedulerState is
            variable res: SchedulerState := st;
        begin
            res.readNew(0) := bool2std(res.argSrc(0)(1 downto 0) = "11");
            res.readNew(1) := bool2std(res.argSrc(1)(1 downto 0) = "11");
        
            if res.argSrc(0)(1) /= '1' then
                res.argSpec.args(0) := (others => '0');
            end if;
        
            if res.argSrc(1)(1) /= '1' or res.st.zero(1) = '1' then
                res.argSpec.args(1) := (others => '0');
            end if;

            res := TMP_applyKill(res, ctSigs);
            return res;
        end function;


    function getDispatchArgValues_RR(input: SchedulerState;
                                     prevSending: std_logic;
                                     vals0, vals1: MwordArray;
                                     USE_IMM: boolean; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false)
    return SchedulerState is
        variable res: SchedulerState := input;
    begin
        res.full := prevSending;
        res := TMP_clearDestIfEmpty(res);

        if REGS_ONLY then
            return res;    
        end if;

        if res.st.zero(0) = '1' then
            res.args(0) := (others => '0');
        elsif res.argSrc(0)(1 downto 0) = "00" then
            res.args(0) := vals0(slv2u(res.argLocsPipe(0)(1 downto 0)));
        elsif res.argSrc(0)(1 downto 0) = "01" then
            res.args(0) := vals1(slv2u(res.argLocsPipe(0)(1 downto 0)));
        else
            res.args(0) := (others => '0');           
        end if;
    
        if IMM_ONLY_1 or res.st.zero(1) = '1' then
            if USE_IMM then
                res.args(1)(31 downto 16) := (others => res.st.immValue(15));
                res.args(1)(15 downto 0) := res.st.immValue;
                
                if res.st.operation.arith = opAddH then
                    res.args(1)(31 downto 16) := res.st.immValue;
                    res.args(1)(15 downto 0) := (others => '0');
                end if;
            else
                res.args(1) := (others => '0');
            end if;
        elsif res.argSrc(1)(1 downto 0) = "00" then
            res.args(1) := vals0(slv2u(res.argLocsPipe(1)(1 downto 0)));
        elsif res.argSrc(1)(1 downto 0) = "01" then
            res.args(1) := vals1(slv2u(res.argLocsPipe(1)(1 downto 0)));
        else
            res.args(1) := (others => '0');
        end if;
        
        return res;
    end function;

        function getDispatchArgValues_RR_N(input: SchedulerState;
                                         --prevSending: std_logic;
                                         events: EventState;
                                         vals0, vals1: MwordArray;
                                         USE_IMM: boolean; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false)
        return SchedulerState is
            variable res: SchedulerState := input;
        begin
            res := getDispatchArgValues_RR(res, res.full, vals0, vals1, USE_IMM, REGS_ONLY, IMM_ONLY_1);
            --res := TMP_clearFull(res, events);
            if events.lateEvent = '1' then
                res.full := '0';
            end if;
            return res;
        end function;
    
    
        function getDispatchArgValues_RR_O(input: SchedulerState;
                                         prevSending: std_logic;
                                         events: EventState;
                                         vals0, vals1: MwordArray;
                                         USE_IMM: boolean; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false)
        return SchedulerState is
            variable res: SchedulerState := input;
        begin
            res := getDispatchArgValues_RR(res, prevSending, vals0, vals1, USE_IMM, REGS_ONLY, IMM_ONLY_1);
            --res := TMP_clearFull(res, events);
            if events.lateEvent = '1' then
                res.full := '0';
            end if;
            return res;
        end function;
    
    
    function updateDispatchArgs_RR(st: SchedulerState; vals: MwordArray; regValues: MwordArray; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false)
    return SchedulerState is
        variable res: SchedulerState := st;
    begin
        if REGS_ONLY then
            res.args(0) := regValues(0);
            res.args(1) := regValues(1);
            return res;
        end if;
    
        if res.readNew(0) = '1' then
            res.args(0) := vals(slv2u(res.argLocsPipe(0)(1 downto 0)));
        else
            res.args(0) := res.args(0) or regValues(0);
        end if;
    
        if IMM_ONLY_1 then
            null; -- don't read FN or registers
        elsif res.readNew(1) = '1' then
            res.args(1) := vals(slv2u(res.argLocsPipe(1)(1 downto 0)));
        else
            res.args(1) := res.args(1) or regValues(1);
        end if;

        return res;
    end function;
--------------------------------------------
--------------------------------------------


end LogicArgRead;
