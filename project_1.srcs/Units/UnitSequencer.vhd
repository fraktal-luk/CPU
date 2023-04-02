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
        DEBUG_FILE_PREFIX: string -- UNUSED
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
    
    frontEventSignal: in std_logic;
  
    lateEventOut: out std_logic;    
    lateEventSetPC: out std_logic;
    
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
	signal resetSig, enSig: std_logic := '0';							

    signal pcDataSig: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal pcNew, pcCurrent, pcNext: Mword := (others => '0');        
    signal sendingToPC, sendingOutPC, acceptingOutPC, sendingToLastEffective, running,
           eventOccurred, killPC, eventCommitted, intCommitted, intSuppressed, lateEventSending, dbtrapOn, restartPC, jumpWatchMatch,
           sendingToLateCausing, committingEvent, sendingToCommit, sendingOutCommit, commitLocked, fullPC, fullLateCausing: std_logic := '0';

    signal commitGroupCtr, commitGroupCtrNext: InsTag := INITIAL_GROUP_TAG;
    signal commitGroupCtrInc, commitGroupCtrIncNext: InsTag := INITIAL_GROUP_TAG_INC;
    signal commitCtr, commitCtrNext, cycleCtr, lastSeqNum: Word := (others => '0');

    signal stageDataLateCausingIn, stageDataLastEffectiveInA: ControlPacket := DEFAULT_CONTROL_PACKET;

    signal lastEffectiveCt, lateCausingCt: InstructionControlInfo := DEFAULT_CONTROL_INFO;
    signal lastEffectiveTarget, lateCausingIP, lateCausingResult, lateCausingTarget: Mword := (others => '0');

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
    
    signal pcDbInfo --, dbInfo_0, dbInfo_1, dbInfo_2
    : InstructionDebugInfo := DEFAULT_DEBUG_INFO;
    
    constant HAS_RESET_SEQ: std_logic := '0';
    constant HAS_EN_SEQ: std_logic := '0';

      signal robDataCommitted, robDataCommittedNext: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);

      signal  ch0, ch1, ch2, ch3, ch4, ch5: std_logic := '0';
begin
    resetSig <= reset and HAS_RESET_SEQ;
    enSig <= en or not HAS_EN_SEQ;

    eventOccurred <= lateEventSending or execEventSignal or frontEventSignal;
    killPC <= '0';

    lateEventOut <= lateEventSending;
    lateEventSetPC <= lateEventSending;
        
    lateEvent.full <= lateEventSending;
    lateEvent.value <= lateCausingTarget;
    
    pcNew <= newPCData( lateEventSending,    lateCausingTarget,
                        execEventSignal, execEvent.value,
                        frontEventSignal, frontEvent.value,
                        pcNext);

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

    sendingOutPC <= fullPC and not (eventOccurred or lateEventSending);   
    lateEventSending <= fullLateCausing;      
    
    pcNext <= getNextPC(pcCurrent, (others => '0'), '0');
    
    SYS_REGS: block
        signal sysStoreAllow, jumpDbMatch, jumpHwMatch: std_logic := '0';
        signal sysStoreAddress: slv5 := (others => '0');
        signal sysStoreValue: Mword := (others => '0');
        signal excInfoUpdate, intInfoUpdate: std_logic := '0';
    begin
        sysStoreAllow <= dataFromSB.full;
        sysStoreAddress <= dataFromSB.dest(4 downto 0);
        sysStoreValue <= datafromSB.value;
    
        excInfoUpdate <= lateEventSending       -- TODO: what about dbtrap?
                                        and (lateCausingCt.hasException or (bool2std(specialOp.system = opCall)))
                                        and not lateCausingCt.hasInterrupt;
        intInfoUpdate <= lateEventSending and lateCausingCt.hasInterrupt;
    
        jumpHwMatch <= '0' and bool2std(stageDataLastEffectiveInA.target = jumpWatchTarget);
        jumpDbMatch <= bool2std(DB_ENABLE_JUMP_WATCH and stageDataLastEffectiveInA.target = DB_JUMP_WATCH_TARGET);
    
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
                    currentState <= lateCausingResult;                  
                end if;
                
                -- NOTE: writing to link registers after sys reg writing gives priority to the former,
                --            but committing a sysMtc shouldn't happen in parallel with any control event
                if excInfoUpdate = '1' then
                    linkRegExc <= lateCausingIP;
                    savedStateExc <= currentState;
                end if;
                
                if intInfoUpdate = '1' then
                    linkRegInt <= lateCausingIP;
                    savedStateInt <= currentState;
                end if;

                jumpWatchMatch <= '0'; -- Cleared by default, set for 1 cycle if happens
                if sendingToLastEffective = '1' then
                    if stageDataLastEffectiveInA.controlInfo.confirmedBranch = '1' and (jumpHwMatch = '1' or jumpDbMatch = '1')  then
                        jumpWatchAdr <= addInt(lastEffectiveTarget, 4*countOnes(extractFullMask(robData))-4);
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
    
    -- OUTPUT
    pcDataSig.controlInfo.full <= sendingOutPC;
    pcDataSig.ip <= pcCurrent;
    pcDataSig.target <= pcNext;

    pcDataSig.dbInfo <= pcDbInfo;

    pcDataOut <= pcDataSig;
    ----------

    sendingToCommit <= sendingFromROB;


    commitCtrNext <= addInt(commitCtr, countOnes(extractFullMask(robData))) when sendingToCommit = '1' else commitCtr;
    commitGroupCtrNext <= commitGroupCtrInc when sendingToCommit = '1' else commitGroupCtr;
    commitGroupCtrIncNext <= addInt(commitGroupCtrInc, PIPE_WIDTH) when sendingToCommit = '1' else commitGroupCtrInc;

    robDataCommittedNext <= assignCommitNumbers(robData, commitCtr);


    COMMIT_DB: block
        signal gapSig: std_logic := '0';
        signal gapFirst, gapLast: Word := (others => 'U');
    begin
    
        COMMON_SYNCHRONOUS: process(clk)
            procedure DB_handleGroup(ia: ControlPacketArray; signal lastSeqNum: inout Word; signal gapSig: out std_logic) is--; signal gapSig: out std_logic) is
                variable any: boolean := false;
                variable firstNewSeqNum, lastNewSeqNum, gap: Word := (others => '0');
                
            begin
                -- pragma synthesis off
            
                for i in 0 to PIPE_WIDTH-1 loop
                    if ia(i).controlInfo.full = '1' then
                        if not any then
                            firstNewSeqNum := ia(i).dbInfo.seqNum;
                        end if;
                        lastNewSeqNum := ia(i).dbInfo.seqNum;
                        any := true;
                    end if;
                end loop;
                
                gap := sub(firstNewSeqNum, lastSeqNum);
                if slv2u(gap) /= 1 then
                    gapSig <= '1';
                    gapFirst <= addInt(lastSeqNum, 1);
                    gapLast <= addInt(firstNewSeqNum, -1);
                else
                    gapSig <= '0';
                    gapFirst <= (others => 'U');
                    gapLast <= (others => 'U');
                end if;
                
                lastSeqNum <= lastNewSeqNum;
                
                -- pragma synthesis on
            end procedure;
            
            procedure DB_trackSeqNum(ia: ControlPacketArray) is
            begin
                -- pragma synthesis off
                if DB_OP_TRACKING then
                    for i in ia'range loop
                        if ia(i).dbInfo.seqNum = DB_TRACKED_SEQ_NUM then
                            report "";
                            report "DEBUG: Tracked seqNum committed: " & integer'image(slv2u(DB_TRACKED_SEQ_NUM));
                            report "";
                            
                            return;
                        end if;
                    end loop;
                end if;
                -- pragma synthesis on
            end procedure;
        begin
            if rising_edge(clk) then
               commitGroupCtr <= commitGroupCtrNext;
               commitGroupCtrInc <= commitGroupCtrIncNext;
               commitCtr <= commitCtrNext;
    
               if sendingFromROB = '1' then
                   specialOp <= robSpecial;
                   
                   -- DEBUG
                   robDataCommitted <= robDataCommittedNext;
                   DB_handleGroup(robDataCommittedNext, lastSeqNum, gapSig);
                   DB_trackSeqNum(robDataCommittedNext);
               end if;        
            end if;
        end process;       
    
    end block;


    EVENT_HANDLING: block
    begin
        -- Tracking of target:
        --            'target' field of last effective will hold the address of next instruction
        --            to commit after lastEffective; it will be known with certainty because lastEffective is 
        --            already committed.
        --            When committing a taken branch -> fill with target from BQ output
        --            When committing normal op -> increment by length of the op
        --            The 'target' field will be used to update return address for exc/int                             
        stageDataLastEffectiveInA <= getNewEffective(sendingToCommit,
                                                     robData,
                                                     bqTargetData.full,
                                                     bqTargetData.value,
                                                     lastEffectiveTarget,
                                                     lateCausingCt, lateCausingTarget,
                                                     lateEventSending);

        sendingToLastEffective <= sendingToCommit or lateEventSending;
        committingEvent <= sendingToCommit and anyEvent(robData);
    
        EVENT_INCOMING: process(clk)
            -- TODO: now there's no ControlPacket to pass here, create it
            procedure DB_reportLateEvent(cp: ControlPacket) is
            begin
                -- pragma synthesis off
                if DB_BRANCH_EXEC_TRACKING and cp.controlInfo.full = '1' and cp.controlInfo.newEvent = '1' then
                    report "";
                    report "DEBUG: late event: " & natural'image(slv2u(cp.dbInfo.seqNum));
                    report "";
                    report "";
                end if;
                -- pragma synthesis on
            end procedure;
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

                if sendingToLastEffective = '1' then
                    lastEffectiveCt <= stageDataLastEffectiveInA.controlInfo;
                    lastEffectiveTarget <= stageDataLastEffectiveInA.target;
                else
                    lastEffectiveCt.full <= '0';
                    lastEffectiveCt.newEvent <= '0';
                end if;

                fullLateCausing <= sendingToLateCausing;
                if sendingToLateCausing = '1' then
                   lateCausingCt <= stageDataLateCausingIn.controlInfo;
                   lateCausingIP <= stageDataLateCausingIn.ip;
                   lateCausingResult <= stageDataLateCausingIn.nip;
                   lateCausingTarget <= stageDataLateCausingIn.target;
                else
                   lateCausingCt.full <= '0';
                   lateCausingCt.newEvent <= '0'; 
                end if;
            end if;
        end process;

        sendingToLateCausing <= (eventCommitted or intCommitted) and sbEmpty;

        stageDataLateCausingIn <= getLatePCData(lastEffectiveCt, lastEffectiveTarget,
                                                intCommitted, intTypeCommitted, currentState,
                                                linkRegExc, linkRegInt, savedStateExc, savedStateInt,
                                                specialOp);
    end block;

    intAllowOut <= not eventCommitted and not lateEventSending;
    intAckOut <= sendingToLateCausing and intCommitted;
    intRejOut <= sendingToLateCausing and intSuppressed;
    
    -- NOTE: RegisterManager has its own, this is for DB purpose
    commitGroupCtrOut <= commitGroupCtr;
    commitGroupCtrNextOut <= commitGroupCtrNext;
   
    commitAccepting <= not commitLocked; -- Blocked while procesing event
                        
    doneSig <= eventCommitted and bool2std(specialOp.system = opSend);
    failSig <= eventCommitted and bool2std(specialOp.system = opError); 
            
end Behavioral;
