----------------------------------------------------------------------------------

--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;

use work.LogicIssue.all;

use work.ForwardingNetwork.all;


entity IssueQueue is
	generic(
		IQ_SIZE: natural := 12;
		IS_FP: boolean := false;
		ALT_INPUT: boolean := false;
		DONT_MATCH1: boolean := false;
		FORWARDING: ForwardingModeArray := (0 => (-100, false));  -- Can be used immediately
		FORWARDING1: ForwardingModeArray := (0 => (-100, false));
		FORWARDING_D: ForwardingModeArray := (0 => (-100, false)); -- Can be used with 1 cycle delay
		NONSHIFT: boolean := true --false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSendingOK: in std_logic;
		newArr: in SchedulerInfoArray(0 to PIPE_WIDTH-1);
		
		--  newArr_Alt: in SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		--  newArrOut: out SchedulerEntrySlotArray(0 to PIPE_WIDTH-1);
		nextAccepting: in std_logic;

		events: in EventState;
		
		fni: in ForwardingInfo;
		readyRegFlags: in std_logic_vector(0 to 3*PIPE_WIDTH-1);

		acceptingMore: out std_logic;
		acceptingOut: out std_logic;
		
		schedulerOut: out SchedulerEntrySlot;
        outputSignals: out IssueQueueSignals		
	);
end IssueQueue;


architecture Behavioral of IssueQueue is
    constant QUEUE_SIZE_EXT: natural := IQ_SIZE;-- + PIPE_WIDTH;
    constant STATIC_ARRAY_SIZE: natural := 8;

    signal newArr_T: SchedulerInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_SCHEDULER_INFO);

    signal queueContent: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
        
        signal TMP_inputAdrs: SmallNumberArray(0 to 3) := (others => (others => '0'));  -- address where to write for each bank (first free slot)   
        signal TMP_inputSlots: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        signal TMP_bankCounts: SmallNumberArray(0 to 3) := (others => (others => '0'));

        signal TMP_inputDirs: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0'); 
        
        signal	   nFull_C,    nFull, nFullNext, nIn, nOut, recoveryCounter: SmallNumber := (others => '0');

        signal TMP_ageMatrix, TMP_ageMatrixNext, selMatrix: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
        signal TMP_insertionLocs, TMP_insertionLocs_Banked: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
        
        signal selBank, selBankAdr: natural := 0; 

            signal selectedBankValues: WordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
            signal selectedValue: Word := (others => '0');
            signal bank0, bank1, bank2, bank3: WordArray(0 to QUEUE_SIZE_EXT/4 - 1) := (others => (others => '0'));

            function TMP_setImmValue(si: SchedulerInfo; val: Word) return SchedulerInfo is
                variable res: SchedulerInfo := si;
            begin
                --res.static.immValue := val;
                res.static.bqPointer := val(7 downto 0);
                res.static.sqPointer := val(15 downto 8);
                res.static.lqPointer := val(23 downto 16);
                
                return res;
            end function;

        function TMP_getNewLocs(fullMask: std_logic_vector) return slv2D is
            variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
            variable cnt: natural := 0;
        begin
            for i in 0 to QUEUE_SIZE_EXT-1 loop
                if fullMask(i) /= '1' then
                    res(i, cnt) := '1';
                    cnt := cnt + 1;
                    if cnt = PIPE_WIDTH then
                        exit;
                    end if;
                end if;
            end loop;            
            return res;
        end function;

            function TMP_getNewLocs_Banked(fullMask: std_logic_vector) return slv2D is
                variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
                variable cnt: natural := 0;
                constant N_BANKS: natural := 4;
                constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
            begin
               for b in 0 to N_BANKS-1 loop
                   for i in 0 to BANK_SIZE-1 loop
                        if fullMask(i * N_BANKS + b) /= '1' then
                            res(i * N_BANKS + b, b) := '1';
                            exit;
                        end if;
                    end loop;
                end loop;
          
                return res;
            end function;

            function TMP_getNewAdrs_Banked(fullMask: std_logic_vector) return SmallNumberArray is
                variable res: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
                variable cnt: natural := 0;
                constant N_BANKS: natural := 4;
                constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
            begin
               for b in 0 to N_BANKS-1 loop
                   for i in 0 to BANK_SIZE-1 loop
                        if fullMask(i * N_BANKS + b) /= '1' then
                            res(b) := i2slv(i, SMALL_NUMBER_SIZE);
                            exit;
                        end if;
                    end loop;
                end loop;
          
                return res;
            end function;

            function TMP_getBankCounts(fullMask: std_logic_vector) return SmallNumberArray is
                variable res: SmallNumberArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
                variable cnt: natural := 0;
                constant N_BANKS: natural := 4;
                constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
            begin
               for i in 0 to BANK_SIZE-1 loop
                   for b in 0 to N_BANKS-1 loop
                        if fullMask(i * N_BANKS + b) = '1' then
                            res(b) := addInt(res(b), 1);
                        end if;
                    end loop;
                end loop;            
                return res;
            end function;

            function TMP_accepting_Banked(counts: SmallNumberArray) return std_logic is
                variable cnt: natural := 0;
                constant N_BANKS: natural := 4;
                constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
            begin
               for b in 0 to N_BANKS-1 loop
                    if slv2u(counts(b)) > BANK_SIZE - 1  then
                        return '0';
                    end if;
                end loop;
                return '1';
            end function;

            function TMP_acceptingMore_Banked(counts: SmallNumberArray) return std_logic is
                variable cnt: natural := 0;
                constant N_BANKS: natural := 4;
                constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
            begin
               for b in 0 to N_BANKS-1 loop
                    if slv2u(counts(b)) > BANK_SIZE - 2  then
                        return '0';
                    end if;
                end loop;
                return '1';
            end function;
    
            function TMP_getBank(selMask: std_logic_vector) return natural is
                constant N_BANKS: natural := 4;
                constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
            begin
                for i in 0 to N_BANKS-1 loop
                    for j in 0 to BANK_SIZE-1 loop
                        if selMask(i + N_BANKS*j) = '1' then
                            return i;
                        end if;
                    end loop;
                end loop;
                
                return 0;
            end function;

            function TMP_getBankAdr(selMask: std_logic_vector) return natural is
                constant N_BANKS: natural := 4;
                constant BANK_SIZE: natural := QUEUE_SIZE_EXT/N_BANKS;
            begin
                for i in 0 to N_BANKS-1 loop
                    for j in 0 to BANK_SIZE-1 loop
                        if selMask(i + N_BANKS*j) = '1' then
                            return j;
                        end if;
                    end loop;
                end loop;
                
                return 0;
            end function;

        function TMP_updateAgeMatrix(ageMatrix, insertionLocs: slv2D; fullMask: std_logic_vector) return slv2D is
            variable res: slv2D(0 to QUEUE_SIZE_EXT-1, 0 to QUEUE_SIZE_EXT-1) := ageMatrix;
        begin
            for i in 0 to PIPE_WIDTH-1 loop
                -- insert current fullMask into proper row    
                for j in 0 to QUEUE_SIZE_EXT-1 loop
                    if insertionLocs(j, i) = '1' then
                        -- Sorry, assigment doesn;t work for ranges in >= 2D
                        for k in 0 to QUEUE_SIZE_EXT-1 loop
                            res(j, k) := fullMask(k); -- Maybe setting to all ones would work too?
                        end loop;
                        
                        -- Preceding in this group are also masked!
                        for p in 0 to PIPE_WIDTH-1 loop
                            if p = i then
                                exit;
                            end if;
                            
                            for k in 0 to QUEUE_SIZE_EXT-1 loop
                                if insertionLocs(k, p) = '1' then
                                    res(j, k) := '1';
                                    exit;
                                end if;
                            end loop;
                        end loop;
                        
                        -- Clear all dependencies on this new op
                        for k in 0 to QUEUE_SIZE_EXT-1 loop
                            res(k, j) := '0';
                        end loop;
                        
                        exit;
                    end if;
                end loop;
            end loop;
            return res;
        end function;

    function TMP_getSelMask(readyMask: std_logic_vector; ageMatrix: slv2D) return std_logic_vector is
        variable res: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := --readyMask;--
                                                                 (others => '0');
        variable row: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            for j in 0 to QUEUE_SIZE_EXT-1 loop
                row(j) := readyMask(j) and ageMatrix(i, j);
            end loop;
            row(i) := '0';

            if isNonzero(row(0 to QUEUE_SIZE_EXT/2 - 1)) = '1' or isNonzero(row(QUEUE_SIZE_EXT/2 to QUEUE_SIZE_EXT-1)) = '1' then
                res(i) := '0';
            else
                res(i) := readyMask(i);
            end if;
        end loop;
             
        return res;
    end function;

    function TMP_getSelMatrix(readyMask: std_logic_vector; ageMatrix: slv2D) return slv2D is
        variable res: slv2D(0 to QUEUE_SIZE_EXT-1,0 to QUEUE_SIZE_EXT-1) := (others => (others => '0'));
        variable si: SchedulerInfo;
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            for j in 0 to QUEUE_SIZE_EXT-1 loop
                res(i, j) := readyMask(j) and ageMatrix(i, j);
            end loop;
        end loop;
             
        return res;
    end function;

        signal queueContent_NS, queueContentRR_NS, queueContentNext_NS, queueContentUpdated_NS, queueContentUpdatedSel_NS:
            SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);

        signal fma_NS: ForwardingMatchesArray(0 to QUEUE_SIZE_EXT - 1) := (others => DEFAULT_FORWARDING_MATCHES);

        signal fullMask_NS, trialMask_NS, readyMaskLive_NS, killMask_NS, readyMaskAll_NS, selMask_NS, selMask_NS1, selMask_NS2, selMask_NS3, selMask_NS4,
                            selMask_TrNS: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        signal controlSigs_NS: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    
        signal selectedSlot_NS, selectedSlot_NS_T: SchedulerInfo := DEFAULT_SCHEDULER_INFO;


    signal fullMask: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');

	signal anyReadyFull, anyReadyLive, sends, sendingKilled, isSent, isSent2, sentKilled, sentKilled_NS1, sentKilled_NS2, sentKilled_NS3, sentKilled_NS4, isEmpty: std_logic := '0';
    	signal anyReadyFull_NS, anyReadyLive_NS, sends_NS, sendingKilled_NS, isSent_NS, isSent2_NS, sentKilled_NS, isEmpty_NS: std_logic := '0';
    
    signal selectedSlot: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
	signal dispatchDataNew_T: SchedulerState := DEFAULT_SCHED_STATE;

    	signal dispatchDataNew_NS: SchedulerState := DEFAULT_SCHED_STATE;


    signal S_firstFree: natural := 0;       
    signal S_selectedStatic, S_selectedStaticRestored: StaticInfo := DEFAULT_STATIC_INFO;
                
        signal isFull, isAlmostFull: std_logic := '0';
                                  
        signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8: std_logic := '0';


    function updateRenameIndex(content: SchedulerInfoArray) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(content'range) := content;
        variable earlyStage: SchedulerInfoArray(0 to PIPE_WIDTH-1) := content(QUEUE_SIZE_EXT - PIPE_WIDTH to QUEUE_SIZE_EXT-1);
    begin
        earlyStage := restoreRenameIndex(earlyStage);

        res(QUEUE_SIZE_EXT - PIPE_WIDTH to QUEUE_SIZE_EXT-1) := earlyStage;
        return res;
    end function;
    
    function assignStaticPtr(arr: SchedulerInfoArray; p: natural) return SchedulerInfoArray is
        variable res: SchedulerInfoArray(arr'range) := arr;
    begin
        for i in res'range loop
            --res(i).dynamic.staticPtr := i2slv(p, SMALL_NUMBER_SIZE);
        end loop;
        return res;
    end function;
    
    function mergeWithStatic(si: SchedulerInfo; st: StaticInfo; tag: InsTag) return SchedulerInfo is
        variable res: SchedulerInfo := si;
        constant subIndV: std_logic_vector(1 downto 0) := si.dynamic.renameIndex(1 downto 0);
        constant subInd: natural := slv2u(subIndV);
        constant wordInd: natural := 8*(3-subInd); 
    begin
        res.static := st;
        res.static.bqPointerSeq := si.static.bqPointerSeq;
        res.dynamic.renameIndex(TAG_SIZE-1 downto LOG2_PIPE_WIDTH) := tag(TAG_SIZE-1 downto LOG2_PIPE_WIDTH);

        return res;
    end function;


    function TMP_trSelMask(selMask: std_logic_vector; queueContent, queueContent_NS: SchedulerInfoArray) return std_logic_vector is
        variable res: std_logic_vector(selMask'range) := (others => '0');
        variable si: SchedulerInfo;
    begin
        if isNonzero(selMask) = '0' then
            return res;
        end if;
        
        for i in selMask'range loop
            if selMask(i) = '1' then
                si := queueContent(i);
                exit;
            end if;
        end loop;

        for i in selMask'range loop
            if queueContent_NS(i).dynamic.renameIndex = si.dynamic.renameIndex and queueContent_NS(i).dynamic.full = si.dynamic.full then
                res(i) := '1';
                exit;
            end if;
        end loop;
       
        return res;
    end function;

    function queueSelect(inputElems: SchedulerInfoArray; selMask: std_logic_vector) return SchedulerInfo is
        variable res, a, b: SchedulerInfo := DEFAULT_SCHEDULER_INFO;
        variable maskedQueue: SchedulerInfoArray(inputElems'range) := (others => DEFAULT_SCHEDULER_INFO);
    begin
        for i in 0 to QUEUE_SIZE_EXT-1 loop
            if selMask(i) = '1' then
                maskedQueue(i) := inputElems(i);
            end if;
        end loop;
    
        for i in 0 to QUEUE_SIZE_EXT/2-1 loop
            a := orSchedEntrySlot(a, maskedQueue(i));
            b := orSchedEntrySlot(b, maskedQueue(i + QUEUE_SIZE_EXT/2));
        end loop;
        
        res := orSchedEntrySlot(a, b);
        return res;
    end function;
    
    function TMP_cmpSchedInfo(a, b: SchedulerInfo) return boolean is
        variable bNew: SchedulerInfo := b;
    begin
        bNew.static.operation := a.static.operation;
        return a = bNew;
    end function; 
begin
    newArr_T <= assignStaticPtr(newArr, S_firstFree);
    
        ch0 <= bool2std(selMask_TrNS = selMask_NS);
        ch1 <= bool2std(nFull = nFull_C);
        ch2 <= bool2std(dispatchDataNew_NS = dispatchDataNew_T);
    
        nFull_C <= i2slv(countOnes(fullMask_NS), SMALL_NUMBER_SIZE);
    
        nFullNext <=        sub(i2slv(countOnes(fullMask_NS), SMALL_NUMBER_SIZE), nOut) when slv2u(recoveryCounter) = 1
                      else  add(nFull, sub(nIn, nOut));

        nIn <= i2slv(countOnes(extractFullMask(newArr_T)), SMALL_NUMBER_SIZE) when prevSendingOK = '1' else (others => '0'); 
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when (isSent_NS and not sentKilled_NS) = '1' else (others => '0');
    
    MANAGEMENT: block
        signal trialMask, readyMaskLive, killMask, readyMaskAll, selMask: std_logic_vector(0 to QUEUE_SIZE_EXT-1) := (others => '0');
        signal queueContentRR, queueContentNext, queueContentUpdated, queueContentUpdatedSel: SchedulerInfoArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_SCHEDULER_INFO);
        signal fma: ForwardingMatchesArray(0 to QUEUE_SIZE_EXT-1) := (others => DEFAULT_FORWARDING_MATCHES);
        signal controlSigs: SlotControlArray(0 to QUEUE_SIZE_EXT-1);
    begin
    
           TMP_inputAdrs <= TMP_getNewAdrs_Banked(fullMask_NS);    
           TMP_bankCounts <= TMP_getBankCounts(fullMask_NS);
    
        QUEUE_SYNCHRONOUS: process(clk)
        begin
            if rising_edge(clk) then        
                if not NONSHIFT then
                    queueContent <= queueContentNext;
                end if;
                
                    queueContent_NS <= queueContentNext_NS;
                    TMP_ageMatrix <= TMP_ageMatrixNext;
                        
                sentKilled <= sendingKilled;            
                isSent <= sends;
                isSent2 <= isSent;

                    selMask_NS1 <= selMask_NS;
                    selMask_NS2 <= selMask_NS1;
                    selMask_NS3 <= selMask_NS2;
                    selMask_NS4 <= selMask_NS3;

                    sentKilled_NS <= sendingKilled_NS;
                        sentKilled_NS1 <= sendingKilled_NS;--isNonzero(killMask_NS and selMask_NS1);
                        sentKilled_NS2 <= isNonzero(killMask_NS and selMask_NS1);
                        sentKilled_NS3 <= isNonzero(killMask_NS and selMask_NS2);
                        sentKilled_NS4 <= isNonzero(killMask_NS and selMask_NS3);                        
                                
                    isSent_NS <= sends_NS;
                    isSent2_NS <= isSent_NS;
                
                nFull <= nFullNext;
                    
                if events.lateEvent = '1' or events.execEvent = '1' then
                    recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addInt(recoveryCounter, -1);
                end if;
                
                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here    
           
                isFull <= cmpGtU(nFullNext, QUEUE_SIZE_EXT - PIPE_WIDTH);
                isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE_EXT - 2*PIPE_WIDTH);
                 
                 
                     if prevSendingOK = '1' and newArr_T(0).dynamic.full = '1' then
                           bank0(slv2u(TMP_inputAdrs(0))) <= X"00" & newArr_T(0).static.lqPointer & newArr_T(0).static.sqPointer & newArr_T(0).static.bqPointer;
                     end if;
                     
                     if prevSendingOK = '1' and newArr_T(1).dynamic.full = '1' then
                           bank1(slv2u(TMP_inputAdrs(1))) <= --newArr_T(1).static.immValue;
                                                                 X"00" & newArr_T(1).static.lqPointer & newArr_T(1).static.sqPointer & newArr_T(1).static.bqPointer;
                     end if;
                     
                     if prevSendingOK = '1' and newArr_T(2).dynamic.full = '1' then
                           bank2(slv2u(TMP_inputAdrs(2))) <= --newArr_T(2).static.immValue;
                                                                 X"00" & newArr_T(2).static.lqPointer & newArr_T(2).static.sqPointer & newArr_T(2).static.bqPointer;
                     end if;
                     
                     if prevSendingOK = '1' and newArr_T(3).dynamic.full = '1' then
                           bank3(slv2u(TMP_inputAdrs(3))) <= --newArr_T(3).static.immValue;
                                                             X"00" & newArr_T(3).static.lqPointer & newArr_T(3).static.sqPointer & newArr_T(3).static.bqPointer;
                     end if;
            end if;
        end process;

                selectedBankValues <= (bank0(selBankAdr), bank1(selBankAdr), bank2(selBankAdr), bank3(selBankAdr));
                selectedValue <= selectedBankValues(selBank);

        fma <= findForwardingMatchesArray(queueContent, fni);
            fma_NS <= findForwardingMatchesArray(queueContent_NS, fni);
    
        controlSigs <= getControlSignals(queueContentUpdatedSel, events);               
    
        -- Vector signals
        killMask <= getKilledVec(controlSigs);
        trialMask <= getTrialVec(controlSigs);
        fullMask <= getFullVec(controlSigs);
        readyMaskAll <= getReadyVec(controlSigs);
        readyMaskLive <= getReadyLiveVec(controlSigs);        
        selMask <= getSelectedVec(controlSigs);
    
        -- Scalar signals
        anyReadyLive <= isNonzero(readyMaskLive);
        anyReadyFull <= isNonzero(readyMaskAll);
        sends <= anyReadyFull and nextAccepting;
        sendingKilled <= isNonzero(killMask and selMask);


            controlSigs_NS <= getControlSignals(queueContentUpdatedSel_NS, events);               
        
            -- Vector signals
            killMask_NS <= getKilledVec(controlSigs_NS);
            trialMask_NS <= getTrialVec(controlSigs_NS);
            fullMask_NS <= getFullVec(controlSigs_NS);
            readyMaskAll_NS <= getReadyVec(controlSigs_NS);
            readyMaskLive_NS <= getReadyLiveVec(controlSigs_NS);
            
            -- Scalar signals
            anyReadyLive_NS <= isNonzero(readyMaskLive_NS);
            anyReadyFull_NS <= isNonzero(readyMaskAll_NS);
            sends_NS <= anyReadyFull_NS and nextAccepting;
            sendingKilled_NS <= isNonzero(killMask_NS and selMask_NS);

            -- TMP: find by tag where the op is corresponding to selection from shifting queue      
            selMask_TrNS <= TMP_trSelMask(selMask, queueContent, queueContent_NS);
            selMask_NS <= TMP_getSelMask(readyMaskAll_NS, TMP_ageMatrix);
            
            selMatrix <= TMP_getSelMatrix(readyMaskAll_NS, TMP_ageMatrix);
            
        isEmpty <= not isNonzero(fullMask);
            isEmpty_NS <= not isNonzero(fullMask_NS);
    
        -- Content manipulation
        queueContentRR <= updateRenameIndex(queueContent);
        
        queueContentUpdated <= updateSchedulerArray(queueContentRR, fni, fma, false, false, DONT_MATCH1, FORWARDING_D, FORWARDING_D);
        queueContentUpdatedSel <= updateSchedulerArray(queueContentRR, fni, fma, false, true, DONT_MATCH1, FORWARDING, FORWARDING1);
    
        queueContentNext <= iqNext_N2(queueContentUpdated, newArr_T, prevSendingOK, sends, killMask, trialMask, selMask, readyRegFlags, 0);

            TMP_ageMatrixNext <= TMP_updateAgeMatrix(TMP_ageMatrix, TMP_insertionLocs, fullMask_NS);
            
            TMP_insertionLocs <= TMP_insertionLocs_Banked;
            TMP_insertionLocs_Banked <= TMP_getNewLocs_Banked(fullMask_NS);

            queueContentRR_NS <= (queueContent_NS);

            queueContentUpdated_NS <= updateSchedulerArray(queueContentRR_NS, fni, fma_NS, false, false, DONT_MATCH1, FORWARDING_D, FORWARDING_D);
            queueContentUpdatedSel_NS <= updateSchedulerArray(queueContentRR_NS, fni, fma_NS, false, true, DONT_MATCH1, FORWARDING, FORWARDING1);
    
            queueContentNext_NS <= iqNext_NS(queueContentUpdated_NS, newArr_T, prevSendingOK, sends_NS, killMask_NS, trialMask_NS, selMask_NS, readyRegFlags, 
                                            TMP_insertionLocs,
                                            0);

        selectedSlot <= prioSelect16(queueContentUpdatedSel, readyMaskAll);

            selectedSlot_NS_T <= prioSelect16(queueContentUpdatedSel_NS, selMask_NS);
            selectedSlot_NS <= queueSelect(queueContentUpdatedSel_NS, selMask_NS);
            
            selBank <= TMP_getBank(selMask_NS);
            selBankAdr <= TMP_getBankAdr(selMask_NS);
            
    end block;

    dispatchDataNew_T <= getSchedEntrySlot(selectedSlot);
    
    
        dispatchDataNew_NS <= getSchedEntrySlot(selectedSlot_NS);
                                                --TMP_setImmValue(selectedSlot_NS, selectedValue));


    WHEN_SH: if not NONSHIFT generate
        -- Output signals
        --schedulerOut <= TMP_restoreState(sends, dispatchDataNew_T);
            schedulerOut.full <= sends;
            schedulerOut.state <= dispatchDataNew_T;

        acceptingOut <= not isNonzero(fullMask(4 to 7));
        acceptingMore <= not isNonzero(fullMask(0 to 7));
    
        outputSignals <=   (sending => sends,
                            cancelled => sentKilled,
                            ready => anyReadyLive,
                            empty => isEmpty,
                            killSel => '0',
                            killSel1 => '0',
                            killSel2 => '0',
                            killSel3 => '0'
                            );    
    end generate;

    WHEN_NSH: if NONSHIFT generate
        -- Output signals
        --schedulerOut <= TMP_restoreState(sends_NS, dispatchDataNew_NS);
            schedulerOut.full <= sends_NS;
            schedulerOut.state <= dispatchDataNew_NS;
        
        acceptingOut <= --not isFull;
                        TMP_accepting_Banked(TMP_bankCounts);
        
        acceptingMore <= --not isAlmostFull;
                         TMP_acceptingMore_Banked(TMP_bankCounts);

        outputSignals <=   (sending => sends_NS,
                            cancelled => sentKilled_NS,
                            ready => anyReadyLive_NS,
                            empty => isEmpty_NS,
                            killSel => sendingKilled_NS,
                            killSel1 => sentKilled_NS1,
                            killSel2 => sentKilled_NS2,
                            killSel3 => sentKilled_NS3
                            );
    end generate;

end Behavioral;
