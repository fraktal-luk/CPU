
library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.ForwardingNetwork.all;


package LogicArgRead is

    -- Issue stage
    function getIssueStage(input: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState;
    function updateIssueStage(ss: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState;

    function getArgValuesRR(ss: SchedulerState; vals0, vals1: MwordArray; USE_IMM: boolean; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false) return MwordArray;
    function advanceControlRR(input: SchedulerState; prevSending: std_logic; events: EventState) return SchedulerState;

    function updateArgsRR(ss: SchedulerState; argValues: MwordArray; vals: MwordArray; regValues: MwordArray; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false) return MwordArray;
    function updateControlRR(ss: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState;

end LogicArgRead;


package body LogicArgRead is

    function TMP_clearDestIfEmpty(input: SchedulerState) return SchedulerState is
        variable res: SchedulerState := input;
    begin
        if res.full /= '1' then
           res.dest := (others => '0');
           res.destTag := (others => '0');
        end if;

        return res;
    end function;


    function getIssueStage(input: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState is
        variable res: SchedulerState := input;
    begin
        res.full := ctSigs.sending;
        res := TMP_clearDestIfEmpty(res);

        res.poison := advancePoison(res.poison, events.memFail);

        if events.lateCausing.full = '1' then
            res.full := '0';
        end if;
        return res;
    end function;


    function updateIssueStage(ss: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState is
        variable res: SchedulerState := ss;
        variable squashPoisoned: std_logic := '0';
    begin
        squashPoisoned := events.memFail and resolving(ss.poison);

        res.st.operation := TMP_restoreOperation(res.st.operation);
    
        res.readNew(0) := bool2std(res.argSrc(0)(1 downto 0) = "11");
        res.readNew(1) := bool2std(res.argSrc(1)(1 downto 0) = "11");
    
        if res.argSrc(0)(1) /= '1' then
            res.args(0) := (others => '0');
        end if;
    
        if res.argSrc(1)(1) /= '1' or res.st.zero(1) = '1' then
            res.args(1) := (others => '0');
        end if;

            res.maybeFull := res.full;
        res.full := res.full and not (squashOnMemFail(events.memFail) or ctSigs.sentKilled or killFollower(ctSigs.trialPrev1, events)) and not squashPoisoned;
        return res;
    end function;


    function getArgValuesRR(ss: SchedulerState; vals0, vals1: MwordArray; USE_IMM: boolean; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false)
    return MwordArray is
        variable res: MwordArray(0 to 2) := ss.argValues;
    begin

        if REGS_ONLY then
            return res;
        end if;

        if ss.st.zero(0) = '1' then
            res(0) := (others => '0');
        elsif ss.argSrc(0)(1 downto 0) = "00" then
            res(0) := vals0(slv2u(ss.argLocsPipe(0)(1 downto 0)));
        elsif ss.argSrc(0)(1 downto 0) = "01" then
            res(0) := vals1(slv2u(ss.argLocsPipe(0)(1 downto 0)));
        else
            res(0) := (others => '0');           
        end if;

        if IMM_ONLY_1 or ss.st.zero(1) = '1' then
            if USE_IMM then
                res(1)(31 downto 16) := (others => ss.st.immValue(15));
                res(1)(15 downto 0) := ss.st.immValue;
                
                if ss.st.operation.arith = opAddH then
                    res(1)(31 downto 16) := ss.st.immValue;
                    res(1)(15 downto 0) := (others => '0');
                end if;
            else
                res(1) := (others => '0');
            end if;
        elsif ss.argSrc(1)(1 downto 0) = "00" then
            res(1) := vals0(slv2u(ss.argLocsPipe(1)(1 downto 0)));
        elsif ss.argSrc(1)(1 downto 0) = "01" then
            res(1) := vals1(slv2u(ss.argLocsPipe(1)(1 downto 0)));
        else
            res(1) := (others => '0');
        end if;

        return res;
    end function;

    function advanceControlRR(input: SchedulerState; prevSending: std_logic; events: EventState) return SchedulerState is
        variable res: SchedulerState := input;
    begin
        res.full := prevSending;
        res := TMP_clearDestIfEmpty(res);
        res.poison := advancePoison(res.poison, events.memFail);

        if events.lateCausing.full = '1' then
            res.full := '0';
        end if;
 
        return res;
    end function;


    function updateControlRR(ss: SchedulerState; ctSigs: IssueQueueSignals; events: EventState) return SchedulerState is
        variable res: SchedulerState := ss;
        variable squashPoisoned: std_logic := '0';
    begin
        squashPoisoned := events.memFail and resolving(ss.poison);

        res.full := res.full and not killFollower(ctSigs.trialPrev2, events) and not squashPoisoned;

        return res;
    end function;


    function updateArgsRR(ss: SchedulerState; argValues: MwordArray; vals: MwordArray; regValues: MwordArray; REGS_ONLY: boolean; IMM_ONLY_1: boolean := false) return MwordArray is
        variable res: MwordArray(0 to 2) := argValues;
    begin
        if REGS_ONLY then
            res(0) := regValues(0);
            res(1) := regValues(1);
            return res;
        end if;

        if ss.readNew(0) = '1' then
            res(0) := vals(slv2u(ss.argLocsPipe(0)(1 downto 0)));
        else
            res(0) := res(0) or regValues(0);
        end if;

        if IMM_ONLY_1 then
            null; -- don't read FN or registers
        elsif ss.readNew(1) = '1' then
            res(1) := vals(slv2u(ss.argLocsPipe(1)(1 downto 0)));
        else
            res(1) := res(1) or regValues(1);
        end if;

        return res;
    end function;

end LogicArgRead;
