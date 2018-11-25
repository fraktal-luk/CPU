----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 22.11.2018 00:15:07
-- Design Name: 
-- Module Name: UnitSequencer - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
--use work.TmpLogicFront.all;
use work.LogicSequence.all;


entity UnitSequencer is
	port(
    clk: in std_logic;
    reset: in std_logic;
    en: in std_logic;

    -- System reg interface
    sysRegReadSel: in slv5;
    sysRegReadValue: out Mword;

    -- Event/state interface                        
    intSignal: in std_logic;
    execEventSignal: in std_logic;
    execCausing: in InstructionState;        
    
    frontEventSignal: in std_logic;
    frontCausing: in InstructionState;
    
    execOrIntEventSignalOut: out std_logic;
    execOrIntCausingOut: out InstructionState;    
    lateEventOut: out std_logic;
    lateEventSetPC: out std_logic;
    lateCausing : out InstructionState;
    
    -- Interface PC <-> front pipe
    frontAccepting: in std_logic;
    pcSending: out std_logic;        
    pcDataLiving: out InstructionState;
    
    -- Interface Rename <-> Front     
    frontDataLastLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    frontLastSending: in std_logic;        
    renameAccepting: out std_logic;        
    
    -- Interface from Rename with IQ    
    iqAccepts: in std_logic;
    renamedDataLiving: out InstructionSlotArray(0 to PIPE_WIDTH-1);
    renamedSending: out std_logic;

    -- Interface with ROB
    commitAccepting: out std_logic;
    robDataLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    sendingFromROB: in std_logic;
    
    dataFromBQV: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    
    dataFromSB: in InstructionState;
    sbEmpty: in std_logic;
    sbSending: in std_logic;
    
    sysStoreAllow: in std_logic;
    sysStoreAddress: in slv5; 
    sysStoreValue: in Mword;        
    
    -- Counter outputs
    commitGroupCtrOut: out InsTag;        
    commitGroupCtrIncOut: out InsTag;
    
    committedSending: out std_logic;
    committedDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1);

    renameLockEndOut: out std_logic;

    newPhysDestsIn: in PhysNameArray(0 to PIPE_WIDTH-1);
    newPhysDestPointerIn: in SmallNumber;
    newPhysSourcesIn: in PhysNameArray(0 to 3*PIPE_WIDTH-1);
    
    intAllowOut: out std_logic;
    
    intAckOut: out std_logic;
    intRejOut: out std_logic
    
    --start: in std_logic    -- TODO: change to reset interrupt
);
end UnitSequencer;


architecture Behavioral of UnitSequencer is
	signal resetSig, enSig: std_logic := '0';							

        signal pcNext, savedPC, savedState: Mword := (others => '0');
        
        signal stageDataOutPC: InstructionState := DEFAULT_INSTRUCTION_STATE;
        signal sendingToPC, sendingOutPC, acceptingOutPC, sendingToLastEffective, running: std_logic := '0';
        
    signal stageDataLateCausingOut: InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
        
        signal excInfoUpdate, intInfoUpdate, execOrIntEventSignal: std_logic := '0';
        
        signal execOrIntCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;
        
        signal stageDataRenameIn: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        signal stageDataOutRename: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        signal sendingOutRename, acceptingOutRename, sendingToLateCausing, committingEvent: std_logic:= '0';     
        signal sendingToCommit, sendingOutCommit, acceptingOutCommit: std_logic := '0';
        signal stageDataToCommit, stageDataOutCommit: InstructionSlotArray(0 to PIPE_WIDTH-1)
                                                := (others => DEFAULT_INSTRUCTION_SLOT);                        
        
        signal renameCtr, renameCtrNext, commitCtr, commitCtrNext: InsTag := (others => '1');
        signal renameGroupCtr, renameGroupCtrNext, commitGroupCtr, commitGroupCtrNext: InsTag := INITIAL_GROUP_TAG;
        signal commitGroupCtrInc, commitGroupCtrIncNext: InsTag := INITIAL_GROUP_TAG_INC;--(others => '0');
        
        signal effectiveMask: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
        
        signal renameLockCommand, renameLockRelease, renameLockState, renameLockEnd: std_logic := '0';    

        signal lateCausingSig, newLateCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;                
                
    signal eventOccurred, killPC, ch0, ch1, eventCommitted, intCommitted, intSuppressed, 
                lateEventSending: std_logic := '0';
        
        signal intWaiting, addDbEvent, intAllow, intAck, dbtrapOn, restartPC: std_logic := '0';
        
        signal stageDataRenameInA, stageDataRenameOutA, stageDataCommitInA, stageDataCommitOutA:
                        InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        
    signal stageDataToPC, tmpPcOutA, stageDataLastEffectiveInA, stageDataLastEffectiveOutA, stageDataLateCausingIn:
                        InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
        
        constant HAS_RESET_SEQ: std_logic := '0';
        constant HAS_EN_SEQ: std_logic := '0';
        
        function getNewLateCausing(ins: InstructionState; int: std_logic;
                                   currentState,
                                   linkRegExc, linkRegInt,
                                   savedStateExc, savedStateInt: Mword 
        ) return InstructionState is
            variable res: InstructionState := ins;
                variable dummy: InstructionState := DEFAULT_INSTRUCTION_STATE;
        begin
            dummy := getLatePCData(ins, int,
                                                                currentState,
                                                                linkRegExc, linkRegInt,
                                                                savedStateExc, savedStateInt);
            res.controlInfo.hasInterrupt := int; 
            res.target := dummy.ip; -- jump address
            res.result := dummy.result; -- new 'currentState'
            return res;
        end function;
        
        function renameGroup(insVec: InstructionSlotArray;
                                    newPhysSources: PhysNameArray;
                                    newPhysDests: PhysNameArray;
                                    renameCtr: InsTag;
                                    renameGroupCtrNext: InsTag;
                                    newPhysDestPointer: SmallNumber;
                                    dbtrap: std_logic
                                    ) return InstructionSlotArray is
            variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
            variable reserveSelSig, takeVec: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0' );
            variable nToTake: integer := 0;
            variable newGprTags: SmallNumberArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));    
            variable newNumberTags: InsTagArray(0 to PIPE_WIDTH-1) := (others=>(others=>'0'));
        begin
            -- DUMMY!            
            
            return res;
        end function;
        
        
            signal sysRegArray: MwordArray(0 to 31) := (0 => (others => '1'), others => (others => '0'));    
    
        alias currentState is sysRegArray(1);
        alias linkRegExc is sysRegArray(2);
        alias linkRegInt is sysRegArray(3);
        alias savedStateExc is sysRegArray(4);
        alias savedStateInt is sysRegArray(5);
begin     
        resetSig <= reset and HAS_RESET_SEQ;
        enSig <= en or not HAS_EN_SEQ;
        
        EVENTS: block
        begin    
                eventOccurred <= lateEventSending or execEventSignal or frontEventSignal;
                killPC <= '0';
        
                lateEventOut <= lateEventSending;
                lateEventSetPC <= lateEventSending;
            execOrIntEventSignal <= lateEventSending or execEventSignal;
            execOrIntCausing <= stageDataLateCausingOut(0).ins when lateEventSending = '1' else execCausing;
            lateCausing <= stageDataLateCausingOut(0).ins;
        
            execOrIntEventSignalOut <= execOrIntEventSignal;    -- $MODULE_OUT
            execOrIntCausingOut <= execOrIntCausing; -- $MODULE_OUT
        end block;
        
        
        stageDataToPC(0).full <= sendingToPC;
        stageDataToPC(0).ins <= newPCData(lateEventSending, stageDataLateCausingOut(0).ins,
                                            execEventSignal, execCausing,
                                            frontEventSignal, frontCausing,
                                            pcNext);
                
       sendingToPC <= --(acceptingOutPC and 
                            running
                               or eventOccurred;      
                      --      or eventOccurred) and not killPC;
            process(clk)
            begin
                if rising_edge(clk) then
                    if (reset or restartPC) = '1' then
                        running <= '1';
                    elsif killPC = '1' then
                        running <= '0';
                    end if;
                end if;
            end process;
        
        
            SUBUNIT_PC: entity work.GenericStage(Behavioral) port map(
                clk => clk, reset => resetSig, en => enSig,
                        
                prevSending => sendingToPC,
        
                nextAccepting => running,
                                         -- In multithreaded implementation it should be '1' for selected thread 
                stageDataIn => stageDataToPC,
                
                acceptingOut => acceptingOutPC,
                sendingOut => sendingOutPC,
                stageDataOut => tmpPcOutA,
                
                execEventSignal => eventOccurred,
                lateEventSignal => lateEventSending,
                execCausing => DEFAULT_INSTRUCTION_STATE
            );            
        
        stageDataOutPC.ip <= tmpPcOutA(0).ins.ip;
        stageDataOutPC.target <= pcNext; -- CAREFUL: Attaching next address from line predictor. Correct?
        
        pcNext <= getNextPC(stageDataOutPC.ip, (others => '0'), '0');

        excInfoUpdate <= lateEventSending and not stageDataLateCausingOut(0).ins.controlInfo.hasInterrupt;
        intInfoUpdate <= lateEventSending and stageDataLateCausingOut(0).ins.controlInfo.hasInterrupt;     
        ----------------------------------------------------------------------
        
        SYS_REGS: block
        begin
            CLOCKED: process(clk)
            begin                    
                if rising_edge(clk) then
                    -- Reading sys regs
                    sysRegReadValue <= sysRegArray(slv2u(sysRegReadSel));            
        
                    -- Write from system write instruction
                    if sysStoreAllow = '1' then
                        sysRegArray(slv2u(sysStoreAddress)) <= sysStoreValue;
                    end if;
        
                    -- Writing specialized fields on events
                    if lateEventSending = '1' then
                        currentState <= stageDataLateCausingOut(0).ins.result;
                        currentState(15 downto 10) <= (others => '0');
                        currentState(7 downto 2) <= (others => '0');
                    end if;
                    
                    -- NOTE: writing to link registers after sys reg writing gives priority to the former,
                    --            but committing a sysMtc shouldn't happen in parallel with any control event
                    -- Writing exc status registers
                    if excInfoUpdate = '1' then
                        linkRegExc <= savedPC;
                        savedStateExc <= savedState;
                    end if;
                    
                    -- Writing int status registers
                    if intInfoUpdate = '1' then
                        linkRegInt <= savedPC;
                        savedStateInt <= savedState;
                    end if;
                    
                    -- Enforcing content of read-only registers
                    sysRegArray(0) <= (others => '1');--PROCESSOR_ID;
                    
                    -- Only some number of system regs exists        
                    for i in 6 to 31 loop
                        sysRegArray(i) <= (others => '0');
                    end loop;                
                end if;    
            end process;

            dbtrapOn <= currentState(25);
        end block;
        
        pcDataLiving <= stageDataOutPC;
        pcSending <= sendingOutPC;    
        
        -- Rename stage
        stageDataRenameInA <= renameGroup(frontDataLastLiving, newPhysSourcesIn, newPhysDestsIn, renameCtr,
                                                            renameGroupCtrNext, newPhysDestPointerIn, dbtrapOn);
        --    stageDataRenameInA <= makeSlotArray(stageDataRenameIn.data, stageDataRenameIn.fullMask);
        
        SUBUNIT_RENAME: entity work.GenericStage(Behavioral)--Renaming)
        generic map(
            USE_CLEAR => '0',
            WIDTH => PIPE_WIDTH
        )
        port map(
            clk => clk, reset => resetSig, en => enSig,
            
            -- Interface with front
            prevSending => frontLastSending,    
            stageDataIn => stageDataRenameInA,
            
            acceptingOut => acceptingOutRename,
            
            -- Interface with IQ
            nextAccepting => iqAccepts,
            sendingOut => sendingOutRename,
            stageDataOut => stageDataRenameOutA,
            
            -- Event interface
            execEventSignal => '0',
            lateEventSignal => '0',--evtPhase0 or execEventSignal, -- bcause Exec is always older than Rename     
            execCausing => execCausing
        );
        
        COMMON_STATE: block
        begin
            renameGroupCtrNext <= nextCtr(renameGroupCtr, execOrIntEventSignal,
                                                    execOrIntCausing.tags.renameIndex and i2slv(-PIPE_WIDTH, TAG_SIZE),
                                                    frontLastSending, ALL_FULL);
            renameCtrNext <= nextCtr(renameCtr, execOrIntEventSignal, execOrIntCausing.tags.renameSeq,
                                             frontLastSending, extractFullMask(frontDataLastLiving));
            commitGroupCtrNext <= --nextCtr(commitGroupCtr, '0', (others => '0'), sendingToCommit, ALL_FULL);
                                            commitGroupCtrInc when sendingToCommit = '1' else commitGroupCtr;
            commitCtrNext <= nextCtr(commitCtr, '0', (others => '0'), sendingToCommit, effectiveMask);
        
            commitGroupCtrInc <= i2slv(slv2u(commitGroupCtr) + PIPE_WIDTH, TAG_SIZE);
        
            commitGroupCtrIncNext <= nextCtr(commitGroupCtrInc, '0', (others => '0'), sendingToCommit, ALL_FULL);
        
            -- Re-allow renaming when everything from rename/exec is committed - reg map will be well defined now
            renameLockRelease <= '1' when commitGroupCtr = renameGroupCtr else '0';
                -- CAREFUL, CHECK: when the counters are equal, renaming can be resumed, but renameLockRelease
                --                      takes effect in next cycle, so before tha cycle renaming is still stopped.
                --                         Should compare to commitCtrNext instead?
                --                         But remember that rewinding GPR map needs a cycle, and before it happens,
                --                         renaming can't be done! So this delay may be caused by this problem.
        
            renameLockEnd <= renameLockState and renameLockRelease;
        
            effectiveMask <= getEffectiveMask(stageDataToCommit);
                
            COMMON_SYNCHRONOUS: process(clk)     
            begin
                if rising_edge(clk) then
                    renameCtr <= renameCtrNext;
                    commitCtr <= commitCtrNext;                    
                    renameGroupCtr <= renameGroupCtrNext;
                    commitGroupCtr <= commitGroupCtrNext;
                    commitGroupCtrInc <= commitGroupCtrIncNext;
        
                    -- Lock when exec part causes event
                    if execOrIntEventSignal = '1' then -- CAREFUL
                        renameLockState <= '1';    
                    elsif renameLockRelease = '1' then
                        renameLockState <= '0';
                    end if;                    
                end if;    
            end process;        
        end block;
        
        sendingToCommit <= sendingFromROB;
        
        -- Commit stage: in order again                
        SUBUNIT_COMMIT: entity work.GenericStage(Behavioral)
        generic map(
            WIDTH => PIPE_WIDTH
        )
        port map(
            clk => clk, reset => resetSig, en => enSig,
            
            -- Interface with CQ
            prevSending => sendingToCommit,
            stageDataIn => stageDataToCommit,
            acceptingOut => open, -- unused but don't remove
            
            -- Interface with hypothetical further stage
            nextAccepting => '1',
            sendingOut => sendingOutCommit,
            stageDataOut => stageDataCommitOutA,
            
            -- Event interface
            execEventSignal => '0', -- CAREFUL: committed cannot be killed!
            lateEventSignal => '0',    
            execCausing => execCausing
        );
        
            -- Tracking of target:
            --            'target' field of last effective will hold the address of next instruction
            --            to commit after lastEffective; it will be known with certainty because lastEffective is 
            --            already committed.
            --            When committing a taken branch -> fill with target from BQ output
            --            When committing normal op -> increment by length of the op
            --            
            --            The 'target' field will be used to update return address for exc/int
  stageDataToCommit <= recreateGroup(robDataLiving, dataFromBQV, stageDataLastEffectiveOutA(0).ins.target);
  stageDataLastEffectiveInA(0) <= getNewEffective(sendingToCommit, robDataLiving, dataFromBQV,
                                                                stageDataLastEffectiveOutA(0).ins, 
                                                                stageDataLateCausingOut(0).ins,
                                                                lateEventSending);
                                                                
    sendingToLastEffective <= sendingToCommit or lateEventSending;

    LAST_EFFECTIVE_SLOT: entity work.GenericStage(Behavioral)
    port map(
        clk => clk, reset => resetSig, en => enSig,
        
        -- Interface with CQ
        prevSending => sendingToLastEffective,
        stageDataIn => stageDataLastEffectiveInA,
        
        acceptingOut => open, -- unused but don't remove
        
        nextAccepting => '1',
        sendingOut => open,
        stageDataOut => stageDataLastEffectiveOutA,
        
        -- Event interface
        execEventSignal => '0', -- CAREFUL: committed cannot be killed!
        lateEventSignal => '0',    
        execCausing => DEFAULT_INSTRUCTION_STATE
    );

    committingEvent <= sendingToCommit and anyEvent(robDataLiving); -- ???

    EVENT_INCOMING: process(clk)
    begin
        if rising_edge(clk) then
            if sendingToLateCausing = '1' then
                eventCommitted <= '0';
                intCommitted <= '0';
                intSuppressed <= '0';
            end if;
            
            if committingEvent = '1' then
                eventCommitted <= '1';
            end if;
            if (intSignal and not committingEvent) = '1' then
                intCommitted <= '1';
            elsif (intSignal and committingEvent) = '1' then
                intSuppressed <= '1';
            end if;
        end if;
    end process;
    
    sendingToLateCausing <= (eventCommitted or intCommitted) and sbEmpty;
    
    newLateCausing <= getNewLateCausing(stageDataLastEffectiveOutA(0).ins, intCommitted,
                                        currentState, linkRegExc, linkRegInt, savedStateExc, savedStateInt);
    
    stageDataLateCausingIn(0) <= (sendingToLateCausing, newLateCausing);

    LATE_CAUSING_SLOT: entity work.GenericStage(Behavioral)
    port map(
        clk => clk, reset => resetSig, en => enSig,
        
        prevSending => sendingToLateCausing,
        stageDataIn => stageDataLateCausingIn,
        
        acceptingOut => open, -- unused but don't remove
        
        -- Interface with hypothetical further stage
        nextAccepting => '1',
        sendingOut => lateEventSending,
        stageDataOut => stageDataLateCausingOut,
        
        -- Event interface
        execEventSignal => '0', -- CAREFUL: committed cannot be killed!
        lateEventSignal => '0',    
        execCausing => DEFAULT_INSTRUCTION_STATE
    );
   
    EVENT_LINK_INFO: process(clk)
    begin
        if rising_edge(clk) then
            if sendingToLateCausing = '1' then
                savedPC <= stageDataLastEffectiveOutA(0).ins.target; -- TODO: PC rathe than target if not restartable
                savedState <= currentState;                
            end if;            
        end if;
    end process;
        
        intAllowOut <= not eventCommitted and not lateEventSending;
        intAckOut <= sendingToLateCausing and intCommitted;
        intRejOut <= sendingToLateCausing and intSuppressed;
        
        renameAccepting <= acceptingOutRename and not renameLockState;
        renamedDataLiving <= stageDataRenameOutA;
        renamedSending <= sendingOutRename;
        
        commitGroupCtrOut <= commitGroupCtr;
        commitGroupCtrIncOut <= commitGroupCtrInc;
        
        renameLockEndOut <= renameLockEnd;
        commitAccepting <= not eventCommitted and not lateEventSending; -- Blocked while procesing event
        committedSending <= sendingOutCommit;
        committedDataOut <= stageDataCommitOutA;
end Behavioral;
