
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionState.all;
use work.LogicExec.all;

use work.PipelineGeneral.all;


entity MultiplierDivider is
    Port (
        clk : in STD_LOGIC;

        prevSending: in std_logic;
        preInput: in SchedulerState;
        input: in SchedulerState;
        
        allowIssueI1: in std_logic;
        killFollowerNext: in std_logic;
        
        events: in EventState;
        
        lockIssueI1_Alt: out std_logic;
        
        sending: out std_logic;
        divUnlock_Alt: out std_logic;
        
        outStage0: out ExecResult;
        outStage1: out ExecResult;

        output: out ExecResult
    );
end MultiplierDivider;



architecture Behavioral of MultiplierDivider is
    signal dataToMul, dataMulE0, dataMulE1, dataMulE2, divSlot: ExecResult := DEFAULT_EXEC_RESULT;
    signal divAllowed, sendingDivIssued, divRR, divFull,
            divUnlock, 
            divPrepareSend, divResultSending, divPrepare,
            divResultSent, divResultSent2, divReady, remReady, sendingDivRR: std_logic := '0';

    signal sg0, sg1, isLowE0, isLowE1: std_logic := '0';
    signal arg0, arg1, mulResult, divRes: Word := (others => '0');
    signal resLongE1: Dword := (others => '0');

    signal divQuot, divRem: Word := (others => '0');
        signal divQuot_Alt, divRem_Alt: Word := (others => '0');
begin

    sendingDivIssued <= preInput.full and usesDivider(preInput); -- Speculative because it doesn't take into account kill signals?
    sendingDivRR <= prevSending and usesDivider(input);

    -- This must mux issued multiply with div result
    dataToMul <= divSlot when divResultSending = '1'
            else prepareMultiply(prevSending, input);

    process (clk)
    begin
        if rising_edge(clk) then
            dataMulE0 <= dataToMul;
            dataMulE1 <= dataMulE0;
            dataMulE2 <= dataMulE1;
            
            divResultSent <= divResultSending;
            divResultSent2 <= divResultSent;
        end if;
    end process;



    MAIN: process (clk)
    begin
        if rising_edge(clk) then
            -- stage E0
            isLowE0 <= bool2std(input.st.operation.arith = opMul);

            arg0 <= input.args(0);
            arg1 <= input.args(1);

            sg0 <= input.args(0)(31) and bool2std(input.st.operation.arith = opMulHS);
            sg1 <= input.args(1)(31) and bool2std(input.st.operation.arith = opMulHS);

            -- stage E1
            if divReady = '1' then
               divRes <= --divQuot;
                            divQuot_Alt;
            elsif remReady = '1' then
               divRes <= --divRem;
                            divRem_Alt;
            end if;

                if not TMP_ENABLE_DIV then
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


    divUnlock <= not (divAllowed and allowIssueI1) and not sendingDivIssued and not divRR -- and not divPrepare
                                                     and not divFull;


    DIVISION: block
        signal usingDiv, usingRem, trialled, kill, opUnsigned: std_logic := '0';
        signal divTime: SmallNumber := sn(0);
    begin
        kill <= (trialled and events.execEvent) or events.lateEvent; -- move to division

        divPrepareSend <= divFull and bool2std(slv2u(divTime) = 30); -- TMP value
        divResultSending <= divFull and bool2std(slv2u(divTime) = 32) and not kill; -- TMP value

        DIVIDER_STATE: process (clk)
        begin
            if rising_edge(clk) then
                divReady <= usingDiv and divResultSending;
                remReady <= usingRem and divResultSending;

                divPrepare <= divRR;

                divAllowed <= divUnlock;

                divRR <= sendingDivIssued and not killFollowerNext;

                trialled <= compareTagBefore(events.preExecTags.renameIndex, divSlot.tag);

                if divResultSending = '1' then
                    assert sendingDivIssued /= '1' report "Division overwrite!";
                    assert (divResultSending and input.full) /= '1' report "Div result collided with issue!";
                end if;

                if divResultSending = '1' or kill = '1' then
                    divFull <= '0';
                    divSlot.dbInfo <= DEFAULT_DEBUG_INFO;

                    usingDiv <= '0';
                    usingRem <= '0';
                elsif sendingDivRR = '1' then
                    divSlot <= makeExecResult(input);

                    usingDiv <= bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opDivS);
                    usingRem <= bool2std(input.st.operation.arith = opRemU or input.st.operation.arith = opRemS);
                --elsif divPrepare = '1' then
                    divFull <= '1';
                    divTime <= sn(0);
                else
                    divTime <= addInt(divTime, 1);
                end if;

            end if;
        end process;


        DEV: block
            signal new00, isUnsigned, signSel0, signSel1: std_logic := '0';
            signal divisorS, sum00, diff00, a0e, a1e: Dword := (others => '0');
            signal result00, sum_T, sum_L, diff_T, diff_Sh, rem_T, ma0, ma1: Word := (others => '0');
            signal quot00, rem00,   arg0, arg1, arg0t, arg1t: Word := (others => '0');
                signal dTime: natural := 0;
        begin
                dTime <= slv2u(divTime);

            divQuot_Alt <= quot00;

            divRem_Alt <=  minus(rem00) when signSel1 = '1'
                    else  rem00;

            diff00 <=    add(sum00, divisorS) when (isNonzero(divTime) /= '1') and isUnsigned /= '1'
                    else sub(sum00, divisorS);

            new00 <= cmpGeU(sum00, divisorS);

                diff_T <= add(sum_T, arg1t) when (isNonzero(divTime) /= '1') and isUnsigned /= '1'
                     else sub(sum_T, arg1t);

            a0e <= signExtend(input.args(0), 64);
            a1e <= '1' & input.args(1) & "000" & X"0000000";

            ma0 <= minus(input.args(0));
            ma1 <= minus(input.args(1));

                  opUnsigned <= bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opRemU);

            DIVISION_SYNC: process (clk)
            begin
                if rising_edge(clk) then

                    if divResultSending = '1' then
                        quot00 <= result00;
                        rem00  <= sum00(31 downto 0);
                    end if;

                    if sendingDivRR = '1' then
                        arg0 <= input.args(0);
                        arg1 <= input.args(1);

                        isUnsigned <= opUnsigned;
                        signSel0 <= input.args(0)(31) and not opUnsigned;
                        signSel1 <= input.args(1)(31) and not opUnsigned;

                        result00 <= (others => '0');

                        arg0t <= input.args(0);
                        arg1t <= input.args(1);

                        if input.args(1)(31) = '1' and opUnsigned /= '1' then
                            sum00 <= minus(a0e);
                            divisorS <= minus(a1e);

                            arg0t <= minus(input.args(0));
                            arg1t <= minus(input.args(1));

                            sum_T <= (others => ma0(31));
                            sum_L <= ma0(30 downto 0) & '0';
                        elsif opUnsigned = '1' then
                            sum00 <= zeroExtend(input.args(0), 64);
                            divisorS <= '0' & input.args(1) & "000" & X"0000000";

                            sum_T <= (0 => input.args(0)(31), others => '0');
                            sum_L <= input.args(0)(30 downto 0) & '0';
                        else
                            sum00 <= signExtend(input.args(0), 64);
                            divisorS <= '0' & input.args(1) & "000" & X"0000000";

                            sum_T <= (others => input.args(0)(31));
                            sum_L <= input.args(0)(30 downto 0) & '0';
                        end if;
                    else
                        result00 <= result00(30 downto 0) & new00;
                        if new00 = '1' then
                            sum00 <= diff00;

                            sum_T <= diff_T(30 downto 0) & sum_L(31);
                            rem_T <= diff_T;
                        else
                            sum_T <= sum_T(30 downto 0) & sum_L(31);
                            rem_T <= sum_T;
                        end if;

                        divisorS <= (divisorS(63) and not isUnsigned) & divisorS(63 downto 1);

                        sum_L <= sum_L(30 downto 0) & '0';
                    end if;

                            if dTime >= 31 then
                            --    sum_T <= (others => 'U');
                            --    sum_L <= (others => 'U');
                            end if;

                end if;
            end process;
        end block; -- DEV

    end block;


    lockIssueI1_Alt <= divPrepareSend;
    divUnlock_Alt <= divUnlock;

    outStage0 <= dataMulE0;
    outStage1 <= dataMulE1;  -- signals result tag
    output <= setMemFail(dataMulE2, '0', mulResult);

    sending <= divResultSending;

end Behavioral;
