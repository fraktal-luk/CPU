----------------------------------------------------------------------------------

----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicSequence.all;
use work.LogicRenaming.all;


entity UnitSequencer is
    generic(
        DEBUG_FILE_PREFIX: string -- UNUSED
    );
	port(
    clk: in std_logic;
    reset: in std_logic;
    en: in std_logic;

    -- System reg interface
    sysRegReadIn: in ExecResult;
    sysRegReadOut: out ExecResult;

    -- Event/state interface
    intSignal: in std_logic;
    intType: in std_logic_vector(0 to 1);
    
    frontEvent: in ExecResult;
    execEvent: in ExecResult;
    lateEvent: out ExecResult;

    -- Interface PC <-> front pipe
    pcDataOut: out ControlPacket;
        
    -- Interface with ROB
    commitAccepting: out std_logic;
    
    robData: in ControlPacketArray(0 to PIPE_WIDTH-1);
    robSpecial: in SpecificOp;
    
    sendingFromROB: in std_logic;
    
    bqTargetData: in ExecResult;
    
    dataFromSB: in ExecResult;
    
    sbEmpty: in std_logic;
    sbSending: in std_logic;       
    
    -- Counter outputs
    commitGroupCtrOut: out InsTag;
    commitGroupCtrNextOut: out InsTag;

    intAllowOut: out std_logic;
    
    intAckOut: out std_logic;
    intRejOut: out std_logic;
    
    doneSig: out std_logic; -- Debug outputs
    failSig: out std_logic
);
end UnitSequencer;


architecture Behavioral of UnitSequencer is
    signal pcDataSig: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal pcNew, pcCurrent, pcInc: Mword := (others => '0');        
    signal sendingToPC, sendingOutPC, acceptingOutPC, sendingToLastEffective, running,
           eventOccurred, killPC, eventCommitted, intCommitted, intSuppressed, lateEventSending, dbtrapOn, restartPC, jumpWatchMatch,
           committingEvent, sendingOutCommit, commitLocked, fullPC, fullLateCausing, sysRegReadDone: std_logic := '0';

    signal commitGroupCtr, commitGroupCtrNext: InsTag := INITIAL_GROUP_TAG;

    signal lastEffectiveIn, lastEffectiveOut, lateCausingOut: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal sysRegValue: Mword := (others => '0');

    signal intTypeCommitted: std_logic_vector(0 to 1) := (others => '0');

    signal sysRegArray: MwordArray(0 to 31) := (0 => (others => '1'), others => (others => '0'));    
    signal specialOp: SpecificOp := DEFAULT_SPECIFIC_OP;

    alias currentState is sysRegArray(1);
    alias linkRegExc is sysRegArray(2);
    alias linkRegInt is sysRegArray(3);
    alias savedStateExc is sysRegArray(4);
    alias savedStateInt is sysRegArray(5);
    
    alias jumpWatchTarget is sysRegArray(30);
    alias jumpWatchAdr is sysRegArray(31);

    alias lateCausingState is lateCausingOut.nip;
    
    signal cycleCtr: Word := (others => '0'); -- DB
    signal pcDbInfo: InstructionDebugInfo := DEFAULT_DEBUG_INFO;

    signal  ch0, ch1, ch2, ch3, ch4, ch5: std_logic := '0';
begin
    eventOccurred <= lateEventSending or execEvent.full or frontEvent.full;
    killPC <= '0';

    pcNew <= newPCData(lateEventSending, lateCausingOut.target, 
                       execEvent.full, execEvent.value,
                       frontEvent.full, frontEvent.value,
                       pcInc);

    pcInc <= getNextPC(pcCurrent);

    sendingToPC <= running or eventOccurred;
 
    RUNNING_STATE: process(clk)
    begin
        if rising_edge(clk) then
            cycleCtr <= addInt(cycleCtr, 1);

            if (reset or restartPC) = '1' then
                running <= '1';
                pcDbInfo <= DB_addIndex(DEFAULT_DEBUG_INFO, (others => '0'));
            elsif killPC = '1' then
                running <= '0';
                pcDbInfo <= DEFAULT_DEBUG_INFO;
            end if;

            fullPC <= bool2std(sendingToPC = '1');

            if sendingToPC = '1' then
                pcCurrent <= pcNew;
                pcDbInfo <= DB_addCycle(DB_incIndex(pcDbInfo), cycleCtr);
            end if;
        end if;
    end process;

    sendingOutPC <= fullPC and not eventOccurred;

    SYS_REGS: block
        signal jumpDbMatch, jumpHwMatch, excInfoReady: std_logic := '0';
    begin
        excInfoReady <=
                        lateCausingOut.controlInfo.hasException
                        or (bool2std(specialOp.system = opCall)); -- TODO: dbTrap

        jumpHwMatch <= '0' and bool2std(lastEffectiveIn.target = jumpWatchTarget);
        jumpDbMatch <= bool2std(DB_ENABLE_JUMP_WATCH and lastEffectiveIn.target = DB_JUMP_WATCH_TARGET);

        CLOCKED: process(clk)
        begin
            if rising_edge(clk) then
                -- Reading sys regs
                sysRegValue <= sysRegArray(slv2u(sysRegReadIn.value(4 downto 0)));            
                sysRegReadDone <= sysRegReadIn.full;

                -- Write from system write instruction
                if dataFromSB.full = '1' then
                    sysRegArray(slv2u(dataFromSB.dest(4 downto 0))) <= dataFromSB.value;
                end if;

                -- Writing specialized fields on events
                if lateEventSending = '1' then
                    currentState <= lateCausingState;

                    -- NOTE: writing to link registers after sys reg writing gives priority to the former,
                    --            but committing a sysMtc shouldn't happen in parallel with any control event
                    if lateCausingOut.controlInfo.hasInterrupt = '1' then   
                        linkRegInt <= lateCausingOut.ip;
                        savedStateInt <= currentState;
                    elsif excInfoReady = '1' then
                        linkRegExc <= lateCausingOut.ip;
                        savedStateExc <= currentState;
                    end if;                
                end if;

                jumpWatchMatch <= '0'; -- Cleared by default, set for 1 cycle if happens
                if sendingToLastEffective = '1' then
                    if lastEffectiveIn.controlInfo.confirmedBranch = '1' and (jumpHwMatch = '1' or jumpDbMatch = '1')  then
                        jumpWatchAdr <= addInt(lastEffectiveOut.target, 4*countOnes(extractFullMask(robData))-4);
                        jumpWatchMatch <= '1';
                    end if;
                end if;

                -- Enforcing content of read-only registers
                sysRegArray(0) <= (others => '1');--PROCESSOR_ID;

                currentState(23 downto 16) <= (others => '0');
                currentState(15 downto 10) <= (others => '0'); -- bits of state reg always set to 0
                currentState(7 downto 2) <= (others => '0');               
                -- Only some number of system regs exists        
                for i in 6 to 29 loop
                    sysRegArray(i) <= (others => '0');
                end loop;                
            end if;    
        end process;

        dbtrapOn <= currentState(25);
    end block;


    EVENT_HANDLING: block
        signal lateCausingIn: ControlPacket := DEFAULT_CONTROL_PACKET;
        signal sendingToLateCausing: std_logic;
    begin
        -- Tracking of target:
        --            'target' field of last effective will hold the address of next instruction
        --            to commit after lastEffective; it will be known with certainty because lastEffective is 
        --            already committed.
        --            When committing a taken branch -> fill with target from BQ output
        --            When committing normal op -> increment by length of the op
        --            The 'target' field will be used to update return address for exc/int                             
        lastEffectiveIn <= getNewEffective(  sendingFromROB,
                                             robData,
                                             bqTargetData.full,
                                             bqTargetData.value,
                                             lastEffectiveOut.target,
                                             --lateCausingOut.controlInfo,
                                             --lateCausingOut.target,
                                             lateCausingOut,
                                             lateEventSending);

        sendingToLastEffective <= sendingFromROB or lateEventSending;
        committingEvent <= sendingFromROB and anyEvent(robData);

        EVENT_INCOMING: process(clk)
        begin
            if rising_edge(clk) then
                if sendingFromROB = '1' then
                   specialOp <= robSpecial;
                end if;
            
                if committingEvent = '1' then
                    eventCommitted <= '1';
                    commitLocked <= '1';
                end if;
 
                if intSignal = '1' then
                    commitLocked <= '1';
                    if committingEvent = '1' then
                        intSuppressed <= '1';
                    else
                        intCommitted <= '1';
                        intTypeCommitted <= intType;
                    end if;
                end if;

                if lateEventSending = '1' then
                    commitLocked <= '0';
                end if;

                if sendingToLastEffective = '1' then
                    lastEffectiveOut <= lastEffectiveIn;
                else
                    lastEffectiveOut.full <= '0';
                    lastEffectiveOut.controlInfo.c_full <= '0';
                    lastEffectiveOut.controlInfo.newEvent <= '0';
                end if;

                if sendingToLateCausing = '1' then
                    eventCommitted <= '0';
                    intCommitted <= '0';
                    intSuppressed <= '0';

                    fullLateCausing <= '1';

                    lateCausingOut <= lateCausingIn;
                else
                    fullLateCausing <= '0';
                    
                    lateCausingOut.full <= '0';
                    lateCausingOut.controlInfo.c_full <= '0';
                    lateCausingOut.controlInfo.newEvent <= '0';
                end if;
            end if;
        end process;

        lateEventSending <= fullLateCausing;

        sendingToLateCausing <= (eventCommitted or intCommitted) and sbEmpty;
                                -- (ec and sbe) or (ic and sbe)
        lateCausingIn <= getLatePCData( --lastEffectiveOut.controlInfo, lastEffectiveOut.target,
                                        lastEffectiveOut,
                                        intCommitted, intTypeCommitted, currentState,
                                        linkRegExc, linkRegInt, savedStateExc, savedStateInt,
                                        specialOp);
    end block;

    intAllowOut <= not eventCommitted and not lateEventSending;
    intAckOut <= --sendingToLateCausing and intCommitted;
                 intCommitted and sbEmpty;
    intRejOut <= --sendingToLateCausing and intSuppressed;
                 intSuppressed and sbEmpty;

    commitAccepting <= not commitLocked; -- Blocked while procesing event
    doneSig <= eventCommitted and bool2std(specialOp.system = opSend);
    failSig <= eventCommitted and bool2std(specialOp.system = opError); 

    -- OUTPUT
    pcDataSig.full <= sendingOutPC;
    pcDataSig.controlInfo.c_full <= sendingOutPC;
    pcDataSig.ip <= pcCurrent;
    pcDataSig.target <= pcInc;
    pcDataSig.dbInfo <= pcDbInfo;

    pcDataOut <= pcDataSig;


    sysRegReadOut.full <= sysRegReadDone;
    sysRegReadOut.value <= sysRegValue;

    lateEvent.full <= lateEventSending;
    lateEvent.value <= lateCausingOut.target;


    COMMIT_DB: block
        signal gapSig: std_logic := '0';
        signal commitCtr, commitCtrNext, lastSeqNumSig: Word := (others => '0');
        signal gapFirst, gapLast: Word := (others => 'U');
        signal robDataCommitted, robDataCommittedNext: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);
        signal commitGroupCtrInc, commitGroupCtrIncNext: InsTag := INITIAL_GROUP_TAG_INC;
    begin
        commitCtrNext <= addInt(commitCtr, countOnes(extractFullMask(robData))) when sendingFromROB = '1' else commitCtr;
        commitGroupCtrNext <= commitGroupCtrInc when sendingFromROB = '1' else commitGroupCtr; -- WTF?
        commitGroupCtrIncNext <= addInt(commitGroupCtrInc, PIPE_WIDTH) when sendingFromROB = '1' else commitGroupCtrInc; -- WTF?

        robDataCommittedNext <= assignCommitNumbers(robData, commitCtr);

        COMMON_SYNCHRONOUS: process(clk)

        begin
            if rising_edge(clk) then
               commitGroupCtr <= commitGroupCtrNext;
               commitGroupCtrInc <= commitGroupCtrIncNext;
               commitCtr <= commitCtrNext;

                   -- DEBUG
               if sendingFromROB = '1' then
                   robDataCommitted <= robDataCommittedNext;
                   DB_handleGroup(robDataCommittedNext, lastSeqNumSig, gapSig, gapFirst, gapLast);
                   DB_trackSeqNum(robDataCommittedNext);
               end if;        
            end if;
        end process;       

    end block;

    -- NOTE: RegisterManager has its own, this is for DB purpose
    commitGroupCtrOut <= commitGroupCtr;
    commitGroupCtrNextOut <= commitGroupCtrNext;

end Behavioral;
