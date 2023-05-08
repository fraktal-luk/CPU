
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
       signal divUnlock,
                 divResultSending, divResultSent, divResultSent2, divReady, remReady: std_logic := '0';
       signal mulResult, quot00, quot10, quot01, quot11, rem00, rem10, rem01, rem11: Word := (others => '0');

begin


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

                    -- Intfs?
                    outStage0 <= dataMulE0;
                    outStage1 <= dataMulE1;  -- signals result tag
                    --subpipeI1_E2 <= setMemFail(dataMulE2, '0', mulResult);


    EEEEEE: block
        signal isDivIssue, isDivRR: std_logic := '0';

        signal divFull, divSending, divPrepareSend, divAllowed, divMaybeIssued, divIssued, divRR, trialled, kill, usingDiv, usingRem, isUnsigned,
               new00, new10, new01, new11, sgA, sgB, isLow0, isLow1, signSel0, signSel1: std_logic := '0';
        signal divTime: SmallNumber := sn(0);
        signal resLong1, divisorS, sum00, sum10, sum01, sum11, diff00, diff10, diff01, diff11: Dword := (others => '0');
        signal result00, result10, result01, result11, res2, divRes_N, arg0, arg1: Word := (others => '0');
    begin
        isDivIssue <= usesDivider(preInput);
        isDivRR <= --slotRegReadI1.full and 
                    usesDivider(input);

        process (clk)
        begin
            if rising_edge(clk) then
                isLow0 <= bool2std(input.st.operation.arith = opMul);

                arg0 <= input.args(0);
                arg1 <= input.args(1);

                sgA <= input.args(0)(31) and bool2std(input.st.operation.arith = opMulHS);
                sgB <= input.args(1)(31) and bool2std(input.st.operation.arith = opMulHS);

                if (prevSending and isDivRR) = '1' then
                    signSel0 <= input.args(0)(31) and not bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opRemU);
                    signSel1 <= input.args(1)(31) and not bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opRemU);
                end if;

                if divReady = '1' then
                   if (signSel0 and signSel1) = '1' then
                       divRes_N <= quot11; 
                   elsif (signSel0 and not signSel1) = '1' then
                       divRes_N <= quot10; 
                   elsif (not signSel0 and signSel1) = '1' then
                       divRes_N <= quot01; 
                   else
                       divRes_N <= quot00; 
                   end if;

                elsif remReady = '1' then                                   
                   if (signSel0 and signSel1) = '1' then
                       divRes_N <= rem11; 
                   elsif (signSel0 and not signSel1) = '1' then
                       divRes_N <= rem10; 
                   elsif (not signSel0 and signSel1) = '1' then
                       divRes_N <= rem01; 
                   else
                       divRes_N <= rem00; 
                   end if;
                end if;

                isLow1 <= isLow0;
                resLong1 <= work.Arith.multiplyLong(arg0, arg1, sgA, sgB);

                if divResultSent2 = '1' then
                    res2 <= divRes_N;
                elsif isLow1 /= '1' then
                    res2 <= resLong1(63 downto 32);
                else
                    res2 <= resLong1(31 downto 0);
                end if;
            end if;
        end process;

        mulResult <= res2;

        -- Intf
        divResultSending <= divSending;

        -- Intf
        lockIssueI1_Alt <= divPrepareSend;


        divIssued <= preInput.full and isDivIssue; -- Speculative because it doesn't take into account kill signals?
        divSending <= divFull and bool2std(slv2u(divTime) = 32) and not kill; -- TMP value
        divPrepareSend <= divFull and bool2std(slv2u(divTime) = 30); -- TMP value

        -- Intf
        divUnlock_Alt <= divUnlock;
        divUnlock <= not (divAllowed and allowIssueI1) and not divIssued and not divRR and not divFull;

        -- src: events
        kill <= (trialled and events.execEvent) or events.lateEvent;

        process (clk)
        begin
            if rising_edge(clk) then
                divReady <= '0';
                remReady <= '0';

                divAllowed <= divUnlock;
                divMaybeIssued <= divAllowed and allowIssueI1;

                divRR <= divIssued and not killFollowerNext;

                trialled <= compareTagBefore(events.preExecTags.renameIndex, divSlot.tag);

                if divSending = '1' then
                    assert divIssued /= '1' report "Division overwrite!";
                    assert (divSending and input.full) /= '1' report "Div result collided with issue!";

                    divFull <= '0';
                        divSlot.dbInfo <= DEFAULT_DEBUG_INFO;

                    divReady <= usingDiv;
                    remReady <= usingRem;
                    usingDiv <= '0';
                    usingRem <= '0';
                elsif (prevSending and isDivRR) = '1' then
                    divFull <= '1';
                    divTime <= sn(0);
                    divSlot <= makeExecResult(input);

                    usingDiv <= bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opDivS);
                    usingRem <= bool2std(input.st.operation.arith = opRemU or input.st.operation.arith = opRemS);
                    isUnsigned <= bool2std(input.st.operation.arith = opDivU or input.st.operation.arith = opRemU);
                else
                    divTime <= addInt(divTime, 1);
                end if;

                if kill = '1' then
                    divFull <= '0';
                        divSlot.dbInfo <= DEFAULT_DEBUG_INFO;
                end if;

            end if;
        end process;

        
        diff00 <=    add(sum00, divisorS) when --isNonzero(divTime) /= '1'
                                               false
                else sub(sum00, divisorS);
        new00 <= --isNonzero(divTime) 
                    '1'
                 and ((cmpGeS(sum00, divisorS) and not isUnsigned) or (cmpGeU(sum00, divisorS) and isUnsigned));

        diff10 <=    add(sum10, divisorS) when isNonzero(divTime) /= '1'
                else sub(sum10, divisorS);
        new10 <= not isNonzero(divTime) or cmpGeS(sum10, divisorS);

        diff01 <=    add(sum01, divisorS) when isNonzero(divTime) /= '1'
                else sub(sum01, divisorS);
        new01 <= (not isNonzero(divTime) and isNonzero(sum01)) or 
                    cmpLeS(sum01, divisorS);
                                    
        diff11 <=    add(sum11, divisorS) when isNonzero(divTime) /= '1'
                else sub(sum11, divisorS);
        new11 <= isNonzero(divTime) and cmpLeS(sum11, divisorS);

        process (clk)
        begin
            if rising_edge(clk) then

                if divSending = '1' then
                    quot00 <= result00;
                    rem00  <= sum00(31 downto 0);

                    quot10 <= result10;
                    rem10  <= sum10(31 downto 0);
                    
                    quot01 <= result01;
                    rem01  <= sum01(31 downto 0);
                    
                    quot11 <= result11;
                    rem11  <= sum11(31 downto 0);
                elsif (prevSending and isDivRR) = '1' then
                    result00 <= (others => '0');
                    sum00 <= zeroExtend(input.args(0), 64);

                    result10 <= (others => '0');
                    sum10 <= signExtend(input.args(0), 64);

                    result01 <= (others => '0');
                    sum01 <= signExtend(input.args(0), 64);

                    result11 <= (others => '0');
                    sum11 <= signExtend(input.args(0), 64);

                    divisorS <= input.args(1)(31) & input.args(1) & "000" & X"0000000";

                else
                    result00 <= result00(30 downto 0) & new00;
                    if new00 = '1' then
                        sum00 <= diff00;
                    end if;
                    
                    result10 <= result10(30 downto 0) & new10;
                    if new10 = '1' then
                        sum10 <= diff10;
                    end if;
                    
                    result01 <= result01(30 downto 0) & new01;
                    if new01 = '1' then
                        sum01 <= diff01;
                    end if;
                    
                    result11 <= result11(30 downto 0) & new11;
                    if new11 = '1' then
                        sum11 <= diff11;
                    end if;

                    divisorS <= (divisorS(63) and not isUnsigned) & divisorS(63 downto 1);    
                end if;

            end if;
        end process;

    end block;

    output <= setMemFail(dataMulE2, '0', mulResult);

end Behavioral;
