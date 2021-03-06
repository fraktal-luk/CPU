----------------------------------------------------------------------------------

----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicSequence.all;
use work.LogicRenaming.all;


entity UnitSequencer is
    generic(
        DEBUG_FILE_PREFIX: string
    );
	port(
    clk: in std_logic;
    reset: in std_logic;
    en: in std_logic;

    -- System reg interface
    sysRegReadSel: in slv5;
    sysRegReadValue: out Mword;

    -- Event/state interface                        
    intSignal: in std_logic;
    intType: in std_logic_vector(0 to 1);
    
    execEventSignal: in std_logic;
    execCausing: in InstructionState;        
    
    frontEventSignal: in std_logic;
    frontCausing: in InstructionState;
  
    lateEventOut: out std_logic;
    lateEventSetPC: out std_logic;
    lateCausing: out InstructionState;
    
    -- Interface PC <-> front pipe
    frontAccepting: in std_logic;
    pcSending: out std_logic;        
    pcDataLiving: out InstructionState;
    
    -- Interface Rename <-> Front     
    frontDataLastLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    frontLastSending: in std_logic;

    -- Interface with ROB
    commitAccepting: out std_logic;
    robDataLiving: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    robSpecial: in InstructionSlot;
    sendingFromROB: in std_logic;
    
    dataFromBQV: in InstructionSlotArray(0 to PIPE_WIDTH-1);
    
    bqTargetData: in InstructionSlot;
    
    dataFromSB: in InstructionSlot;
    sbEmpty: in std_logic;
    sbSending: in std_logic;       
    
    -- Counter outputs
    commitGroupCtrOut: out InsTag;
    --commitGroupCtrIncOut: out InsTag;

    committedOut: out InstructionSlotArray(0 to PIPE_WIDTH-1);
    committedSending: out std_logic;
    
    lastEffectiveOut: out InstructionSlot;

    intAllowOut: out std_logic;
    
    intAckOut: out std_logic;
    intRejOut: out std_logic;
    
    doneSig: out std_logic; -- Debug outputs
    failSig: out std_logic
);
end UnitSequencer;


architecture Behavioral of UnitSequencer is
	signal resetSig, enSig: std_logic := '0';							

    signal pcNext: Mword := (others => '0');        
    signal stageDataOutPC, newLateCausing: InstructionState := DEFAULT_INSTRUCTION_STATE;
    signal sendingToPC, sendingOutPC, acceptingOutPC, sendingToLastEffective, running: std_logic := '0';
    signal eventOccurred, killPC, eventCommitted, intCommitted, intSuppressed, lateEventSending, dbtrapOn, restartPC: std_logic := '0';    
    signal sendingToLateCausing, committingEvent, sendingToCommit, sendingOutCommit, commitLocked: std_logic := '0';
    signal commitGroupCtr, commitGroupCtrNext: InsTag := INITIAL_GROUP_TAG;
    signal commitGroupCtrInc, commitGroupCtrIncNext: InsTag := INITIAL_GROUP_TAG_INC;

    signal stageDataLateCausingOut, stageDataToPC, tmpPcOutA, stageDataLastEffectiveInA, stageDataLastEffectiveOutA, stageDataLateCausingIn:
                        InstructionSlotArray(0 to 0) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal stageDataToCommit, stageDataCommitOutA: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);                              
    signal special: InstructionSlot := DEFAULT_INS_SLOT;

    signal intTypeCommitted: std_logic_vector(0 to 1) := (others => '0');
     
    signal commitCtr, commitCtrNext: Word := (others => '0');

    signal sysRegArray: MwordArray(0 to 31) := (0 => (others => '1'), others => (others => '0'));    

    alias currentState is sysRegArray(1);
    alias linkRegExc is sysRegArray(2);
    alias linkRegInt is sysRegArray(3);
    alias savedStateExc is sysRegArray(4);
    alias savedStateInt is sysRegArray(5);

    constant HAS_RESET_SEQ: std_logic := '0';
    constant HAS_EN_SEQ: std_logic := '0';

      signal  ch0, ch1: std_logic := '0';
begin
    resetSig <= reset and HAS_RESET_SEQ;
    enSig <= en or not HAS_EN_SEQ;

    eventOccurred <= lateEventSending or execEventSignal or frontEventSignal;
    killPC <= '0';

    lateEventOut <= lateEventSending;
    lateEventSetPC <= lateEventSending;
    lateCausing <= clearDbCausing(stageDataLateCausingOut(0).ins);
    stageDataToPC(0).full <= sendingToPC;
    stageDataToPC(0).ins <= newPCData(lateEventSending, stageDataLateCausingOut(0).ins,
                                        execEventSignal, execCausing,
                                        frontEventSignal, frontCausing,
                                        pcNext);
            
    sendingToPC <= running or eventOccurred;
 
    RUNNING_STATE: process(clk)
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

        nextAccepting => running, -- In multithreaded implementation it should be '1' for selected thread 
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

    ----------------------------------------------------------------------
    
    SYS_REGS: block
        signal sysStoreAllow: std_logic := '0';
        signal sysStoreAddress: slv5 := (others => '0'); 
        signal sysStoreValue: Mword := (others => '0'); 
        signal excInfoUpdate, intInfoUpdate: std_logic := '0';
    begin
        sysStoreAllow <= sbSending and dataFromSB.full and isStoreSysOp(dataFromSB.ins);
        sysStoreAddress <= dataFromSB.ins.target(4 downto 0);
        sysStoreValue <= dataFromSB.ins.result;
    
        excInfoUpdate <= lateEventSending       -- TODO: what about dbtrap?
                                        and (stageDataLateCausingOut(0).ins.controlInfo.hasException or (bool2std(special.ins.specificOperation.system = opCall)))
                                        and not stageDataLateCausingOut(0).ins.controlInfo.hasInterrupt;
        intInfoUpdate <= lateEventSending and stageDataLateCausingOut(0).ins.controlInfo.hasInterrupt;
    
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
                end if;
                
                -- NOTE: writing to link registers after sys reg writing gives priority to the former,
                --            but committing a sysMtc shouldn't happen in parallel with any control event
                if excInfoUpdate = '1' then
                    linkRegExc <= stageDataLateCausingOut(0).ins.ip;
                    savedStateExc <= currentState;
                end if;
                
                if intInfoUpdate = '1' then
                    linkRegInt <= stageDataLateCausingOut(0).ins.ip;
                    savedStateInt <= currentState;
                end if;
                
                -- Enforcing content of read-only registers
                sysRegArray(0) <= (others => '1');--PROCESSOR_ID;

                currentState(23 downto 16) <= (others => '0');
                currentState(15 downto 10) <= (others => '0'); -- bits of state reg always set to 0
                currentState(7 downto 2) <= (others => '0');               
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

    commitGroupCtrNext <= commitGroupCtrInc when sendingToCommit = '1' else commitGroupCtr;
    commitGroupCtrIncNext <= addInt(commitGroupCtrInc, PIPE_WIDTH) when sendingToCommit = '1' else commitGroupCtrInc;

    COMMON_SYNCHRONOUS: process(clk)     
    begin
        if rising_edge(clk) then
           commitGroupCtr <= commitGroupCtrNext;
           commitGroupCtrInc <= commitGroupCtrIncNext;
           commitCtr <= commitCtrNext;
                                  
           if sendingFromROB = '1' then
               special <= robSpecial;
           end if;           
        end if;
    end process;        
    
    sendingToCommit <= sendingFromROB;

    commitCtrNext <= addInt(commitCtr, countOnes(extractFullMask(robDataLiving))) when sendingToCommit = '1' else commitCtr;
    
    COMMIT_STAGE: if VIEW_ON generate
       use std.textio.all;
       use work.Viewing.all;
       file outFile: text open write_mode is DEBUG_FILE_PREFIX & "committed.txt"; 
    begin
        stageDataToCommit <= recreateGroup(robDataLiving, dataFromBQV, stageDataLastEffectiveOutA(0).ins.target, commitCtr);

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
        
        PRINTOUT: process (clk)
        begin
            if rising_edge(clk) then
                if DEBUG_LOG_COMMITTED then
                    if sendingToCommit = '1' then
                        printGroup(stageDataToCommit, outFile);
                    end if;
                end if;
            end if;
        end process;
        
    end generate;


    EVENT_HANDLING: block
    
    begin
        -- Tracking of target:
        --            'target' field of last effective will hold the address of next instruction
        --            to commit after lastEffective; it will be known with certainty because lastEffective is 
        --            already committed.
        --            When committing a taken branch -> fill with target from BQ output
        --            When committing normal op -> increment by length of the op
        --            The 'target' field will be used to update return address for exc/int                             
       stageDataLastEffectiveInA(0) <= getNewEffective(sendingToCommit, robDataLiving, bqTargetData,
                                                       stageDataLastEffectiveOutA(0).ins, stageDataLateCausingOut(0).ins, lateEventSending);                                                                                   
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
                    commitLocked <= '1';
                end if;

                if (intSignal and not committingEvent) = '1' then
                    intCommitted <= '1';
                    intTypeCommitted <= intType;
                    commitLocked <= '1';
                elsif (intSignal and committingEvent) = '1' then
                    intSuppressed <= '1';
                    commitLocked <= '1';
                end if;
                
                if lateEventSending = '1' then
                    commitLocked <= '0';
                end if;
            end if;
        end process;
        
        sendingToLateCausing <= (eventCommitted or intCommitted) and sbEmpty;
        
        newLateCausing <= getLatePCData(stageDataLastEffectiveOutA(0).ins, intCommitted, intTypeCommitted,
                                            currentState, linkRegExc, linkRegInt, savedStateExc, savedStateInt, special);   
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
    end block;

	COMMITTED_VIEW: if VIEW_ON generate
	   use work.Viewing.all;	        
        -- CAREFUL, TODO: include replaced intPointer and floatPointer in this view 
       signal committedText: GenericStageView;
       signal lastEffectiveText, lateCausingText: InsStringArray(0 to 0);
    begin
       committedText <= getInsStringArray(stageDataCommitOutA);
       lastEffectiveText <= getInsStringArray(stageDataLastEffectiveOutA);
       lateCausingText <= getInsStringArray(stageDataLateCausingOut);
    end generate;

    intAllowOut <= not eventCommitted and not lateEventSending;
    intAckOut <= sendingToLateCausing and intCommitted;
    intRejOut <= sendingToLateCausing and intSuppressed;
    
    commitGroupCtrOut <= commitGroupCtr;
    
    commitAccepting <= not commitLocked; -- Blocked while procesing event
                        
    doneSig <= eventCommitted and bool2std(special.ins.specificOperation.system = opSend);
    failSig <= eventCommitted and bool2std(special.ins.specificOperation.system = opError);
    
    OUTPUT_VIEWING: if VIEW_ON generate
        committedOut <= stageDataCommitOutA;
        committedSending <= sendingOutCommit;
        
        lastEffectiveOut <= stageDataLastEffectiveOutA(0);
    end generate;
    
--    NO_OUTPUT_VIEWING: if not VIEW_ON generate
--        committedOut <= (others => DEFAULT_INS_SLOT);
--        committedSending <= '0';
        
--        lastEffectiveOut <= DEFAULT_INS_SLOT;
--    end generate;    
            
end Behavioral;
