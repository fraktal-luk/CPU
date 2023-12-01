
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.LogicExec.all;

use work.PipelineGeneral.all;


entity MultiplierDivider is
    Port (
        clk : in STD_LOGIC;

        prevSending: in std_logic;
        preInput: in SchedulerState;
        input: in SchedulerState;
        inputArgs: in MwordArray(0 to 2);
        inputEP: in ExecPacket;

        allowIssueI1: in std_logic;
        killFollowerNext: in std_logic;
        
        events: in EventState;
        
        lockIssueI1Out: out std_logic;        
        divUnlockOut: out std_logic;
        
        sending: out std_logic;

        outE0: out ExecPacket;
        outE1: out ExecPacket;
        outE2: out ExecPacket;

        outStage0: out ExecResult;
        outStage1: out ExecResult;

        output: out ExecResult;
        outputValue: out Mword
    );
end MultiplierDivider;



architecture Behavioral of MultiplierDivider is
    signal dataToMul --, divSlot
            : ExecResult := DEFAULT_EXEC_RESULT;
    signal dataToMul_C, dataMulE0_C, dataMulE1_C, dataMulE2_C, divSlot_C: ExecResult := DEFAULT_EXEC_RESULT;
    signal divAllowed, sendingDivIssued, divRR, --divFull,
            divAllowed_Pre, divRR_Pre, divPrepareSend_Pre,
            divUnlock,-- divUnlock_Alt,
            divUnlock_AltP,
            divPrepareSend, divResultSending, divResultSendingNK,
            divPrepareSend_N, divResultSending_N,
            divPrepare, divFullNext_T,
            divResultSent, divResultSent2, divReady, remReady, sendingDivRR: std_logic := '0';

    signal sg0, sg1, isLowE0, isLowE1: std_logic := '0';
    signal arg0, arg1, mulResult, divRes: Word := (others => '0');
    signal resLongE1: Dword := (others => '0');

    signal divQuot, divRem: Word := (others => '0');
    signal divQuot_Alt, divRem_Alt: Word := (others => '0');
    signal divQuot_New, divRem_New: Word := (others => '0');

    signal divEP, EP_I1_E0, EP_I1_E1, EP_I1_E2: ExecPacket := DEFAULT_EXEC_PACKET;
    signal stageDiv, stageE0, stageE1, stageE2: SchedulerState := DEFAULT_SCHED_STATE;

    alias argValues is inputArgs;
    alias divFull is stageDiv.full;

    signal ch0, ch1, ch2, ch3: std_logic := '0';
begin

    outE0 <= EP_I1_E0;
    outE1 <= EP_I1_E1;
    outE2 <= EP_I1_E2;

    sendingDivIssued <= preInput.full and usesDivider(preInput); -- Speculative because it doesn't take into account kill signals?
    sendingDivRR <= prevSending and usesDivider(input);

    -- This must mux issued multiply with div result
    --dataToMul <= divSlot when divResultSending = '1'
    dataToMul <= divSlot_C when divResultSending = '1'
            else prepareMultiply(prevSending, input);

        dataMulE0_C <= makeExecResult(stageE0);
        dataMulE1_C <= makeExecResult(stageE1);
        dataMulE2_C <= makeExecResult(stageE2);
        divSlot_C <= makeExecResult(stageDiv);

         --   ch1 <= bool2std(dataMulE1_C = dataMulE1);
         --   ch2 <= bool2std(dataMulE2_C = dataMulE2);
         --   ch3 <= bool2std(divSlot_C = divSlot);

    process (clk)
    begin
        if rising_edge(clk) then
          --  dataMulE0 <= dataToMul;
          --  dataMulE1 <= dataMulE0;
          ---  dataMulE2 <= dataMulE1;
            
                if divResultSending = '1' then
                    stageE0 <= stageDiv;
                elsif sendingDivRR /= '1' then
                    stageE0 <= work.LogicArgRead.advanceControlRR(input, input.full, events);
                else 
                    stageE0 <= DEFAULT_SCHED_STATE;
                end if;
                
                stageE1 <= work.LogicArgRead.advanceControlRR(stageE0, stageE0.full, events);
                stageE2 <= work.LogicArgRead.advanceControlRR(stageE1, stageE1.full, events);

            divResultSent <= divResultSending;
            divResultSent2 <= divResultSent;

            if (prevSending and not isDivOp(input.st.operation)) = '1' then
                EP_I1_E0 <= updateEP(inputEP, events);
            elsif divResultSending = '1' then
                EP_I1_E0 <= updateEP(divEP, events);
            else
                EP_I1_E0 <= DEFAULT_EXEC_PACKET; 
            end if;

            EP_I1_E1 <= updateEP(EP_I1_E0, events);
            EP_I1_E2 <= updateEP(EP_I1_E1, events);
        end if;
    end process;

    MAIN: process (clk)
    begin
        if rising_edge(clk) then
            -- stage E0
            isLowE0 <= bool2std(input.st.operation.arith = opMul);

            arg0 <= argValues(0);
            arg1 <= argValues(1);

            sg0 <= argValues(0)(31) and bool2std(input.st.operation.arith = opMulHS);
            sg1 <= argValues(1)(31) and bool2std(input.st.operation.arith = opMulHS);

            -- stage E1
            if divReady = '1' then
               divRes <= divQuot_New;
            elsif remReady = '1' then
               divRes <= divRem_New;
            end if;

            if not ENABLE_DIV then
                divRes <= (others => '0');
            end if;

            isLowE1 <= isLowE0;
            resLongE1 <= work.Arith.multiplyLong(arg0, arg1, sg0, sg1);

            -- stage E2
            if divResultSent2 = '1' then
                mulResult <= divRes;
            elsif isLowE1 /= '1' then
                mulResult <= resLongE1(63 downto 32);
            else
                mulResult <= resLongE1(31 downto 0);
            end if;
        end if;
    end process;

    --divUnlock <= divUnlock_Alt;
    divUnlock <= divUnlock_AltP and not preInput.maybeFull;

    DIVISION: block
        signal usingDiv, usingRem,-- trialled,
                trialled_C, kill, kill_C, opUnsigned: std_logic := '0';
        signal divTime: SmallNumber := sn(0);
    begin
    
                ch0 <= bool2std(kill_C = kill);
              --  ch3 <= '1';
    
            divAllowed_Pre <= divUnlock;
            divPrepareSend_Pre <= divFull and bool2std(slv2u(divTime) = 29);
            divRR_Pre <= sendingDivIssued and not killFollowerNext;

        divFullNext_T <= '0' when (divResultSending = '1' or kill = '1')
                    else '1' when sendingDivRR = '1'
                    else divFull;

        kill <= --(trialled and events.execCausing.full) or events.lateCausing.full; -- move to division
                kill_C;
        kill_C <= (trialled_C and events.execCausing.full) or events.lateCausing.full; -- move to division

        divPrepareSend <= divPrepareSend_N;
        divResultSending <= divResultSending_N;
        divResultSending_N <= divResultSendingNK and not kill;

        DIVIDER_STATE: process (clk)
        begin
            if rising_edge(clk) then
                divUnlock_AltP <= not (divAllowed_Pre and not divPrepareSend_Pre) and not divRR_Pre and not divFullNext_T;

            
                divPrepareSend_N <= divFull and bool2std(slv2u(divTime) = 29); -- TMP value
                divResultSendingNK <= divFull and bool2std(slv2u(divTime) = 31); -- TMP value

                divReady <= usingDiv and divResultSending;
                remReady <= usingRem and divResultSending;

                divPrepare <= divRR;

                divAllowed <= divUnlock;

                divRR <= sendingDivIssued and not killFollowerNext;

                --trialled <= compareTagBefore(events.preExecTags.renameIndex, divSlot.tag);
                trialled_C <= compareTagBefore(events.preExecTags.renameIndex, divSlot_C.tag) and divSlot_C.full;

                if divResultSending = '1' then
                    assert sendingDivIssued /= '1' report "Division overwrite!";
                    assert (divResultSending and input.full) /= '1' report "Div result collided with issue!";
                end if;

                if divResultSending = '1' or kill = '1' then
                    --divFull <= '0';
                    --divSlot.dbInfo <= DEFAULT_DEBUG_INFO;
                    divEP <= DEFAULT_EXEC_PACKET;
                        stageDiv <= DEFAULT_SCHED_STATE;
                        
                    usingDiv <= '0';
                    usingRem <= '0';
                elsif sendingDivRR = '1' then
                    --divSlot <= makeExecResult(input);
                    divEP <= updateEP(inputEP, events);
                        stageDiv <= work.LogicArgRead.advanceControlRR(input, input.full, events);

                    usingDiv <= bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opDivS);
                    usingRem <= bool2std(input.st.operation.arith = opRemU or input.st.operation.arith = opRemS);
                    --divFull <= '1';
                    divTime <= sn(0);
                else
                    divEP <= updateEP(divEP, events);
                    divTime <= addInt(divTime, 1);
                end if;

            end if;
        end process;


        DEV: block
            signal new00, new_T, isUnsigned, signSel0, signSel1: std_logic := '0';
            signal divisorS, divisor_TE, sum00, diff00, a0e, a1e, sum_TE, diff_TE, rem_TE: Dword := (others => '0');
            signal result00, result_T, sum_T, sum_L, sum_U, diff_T, diff_Sh, ma0, ma1: Word := (others => '0');
            signal quot00, rem00, quot_T, rem_T,  arg0, arg1, arg0t, arg1t: Word := (others => '0');
            
            function shiftRightU(w: Word) return Word is
                variable res: Word := (others => '0');
            begin
                res(30 downto 0) := w(31 downto 1);
                return res;
            end function;

            function shiftRightS(w: Word) return Word is
                variable res: Word := (others => '0');
            begin
                res(31) := w(31);
                res(30 downto 0) := w(31 downto 1);
                return res;
            end function;
        begin

            divQuot_New <= quot_T;
            divRem_New <=  minus(rem_T) when signSel1 = '1'
                    else  rem_T;

            divQuot_Alt <= quot00;
            divRem_Alt <=  minus(rem00) when signSel1 = '1'
                    else  rem00;

            diff00 <=    add(sum00, divisorS) when (isNonzero(divTime) /= '1') and isUnsigned /= '1'
                    else sub(sum00, divisorS);

            new00 <= cmpGeU(sum00, divisorS);

                diff_TE <= add(sum_TE, divisor_TE) when (isNonzero(divTime) /= '1') and isUnsigned /= '1'
                      else sub(sum_TE, divisor_TE);
                diff_T <= add(sum_T, arg1t) when (isNonzero(divTime) /= '1') and isUnsigned /= '1'
                     else sub(sum_T, arg1t);
                new_T <= cmpGeU(sum_TE, divisor_TE);

            a0e <= signExtend(argValues(0), 64);
            a1e <= '1' & argValues(1) & "000" & X"0000000";

            ma0 <= minus(argValues(0));
            ma1 <= minus(argValues(1));

            opUnsigned <= bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opRemU);

            DIVISION_SYNC: process (clk)
            begin
                if rising_edge(clk) then

                    if divResultSending = '1' then
                        quot_T <= result_T;
                        rem_T  <= sum_TE(32 downto 1);
                    end if;

                    if sendingDivRR = '1' then
                        arg0 <= argValues(0);
                        arg1 <= argValues(1);

                        isUnsigned <= opUnsigned;
                        signSel0 <= argValues(0)(31) and not opUnsigned;
                        signSel1 <= argValues(1)(31) and not opUnsigned;

                        result00 <= (others => '0');

                        arg0t <= argValues(0);
                        arg1t <= argValues(1);

                        if argValues(1)(31) = '1' and opUnsigned /= '1' then
                            sum00 <= minus(a0e);
                            divisorS <= minus(a1e);

                            arg0t <= ma0;
                            arg1t <= ma1;
                        elsif opUnsigned = '1' then
                            sum00 <= zeroExtend(argValues(0), 64);
                            divisorS <= '0' & argValues(1) & "000" & X"0000000";
                        else
                            sum00 <= signExtend(argValues(0), 64);
                            divisorS <= '0' & argValues(1) & "000" & X"0000000";
                        end if;
                    else
                        result00 <= result00(30 downto 0) & new00;
                        if new00 = '1' then
                            sum00 <= diff00;
                        else
                        end if;

                        divisorS <= (divisorS(63) and not isUnsigned) & divisorS(63 downto 1);
                    end if;


                    if sendingDivRR = '1' then
                        if argValues(1)(31) = '1' and opUnsigned /= '1' then
                            sum_TE <= (others => ma0(31));
                            sum_T <= (others => ma0(31));
                            sum_L <= ma0(30 downto 0) & '0';
                            divisor_TE <= zeroExtend(ma1, 64);
                        elsif opUnsigned = '1' then
                            sum_U <= (others => '0');
                            sum_TE <= (0 => argValues(0)(31), others => '0');
                            sum_T <= (0 => argValues(0)(31), others => '0');
                            sum_L <= argValues(0)(30 downto 0) & '0';
                            divisor_TE <= zeroExtend(argValues(1), 64);
                        else
                            sum_U <= (others => argValues(0)(31));
                            sum_TE <= (others => argValues(0)(31));
                            sum_T <= (others => argValues(0)(31));
                            sum_L <= argValues(0)(30 downto 0) & '0';
                            divisor_TE <= zeroExtend(argValues(1), 64);
                        end if;
                    else
                        result_T <= result_T(30 downto 0) & new_T; 
                    
                        if new_T = '1' then
                            sum_TE <= diff_TE(62 downto 0) & sum_L(31);
                            rem_TE <= diff_TE;
                        else
                            sum_TE <= sum_TE(62 downto 0) & sum_L(31);
                            rem_TE <= sum_TE;
                        end if;

                        sum_L <= sum_L(30 downto 0) & '0';
                    end if;

                end if;
            end process;
        end block;

    end block;


    lockIssueI1Out <= divPrepareSend;
    divUnlockOut <= divUnlock;

--    outStage0 <= dataMulE0;
--    outStage1 <= dataMulE1;  -- signals result tag
--    output <= setMemFail(dataMulE2, '0', mulResult);
    outStage0 <= dataMulE0_C;
    outStage1 <= dataMulE1_C;  -- signals result tag
    output <= setMemFail(dataMulE2_C, '0', mulResult);
        outputValue <= mulResult;

    sending <= divResultSending;

end Behavioral;
