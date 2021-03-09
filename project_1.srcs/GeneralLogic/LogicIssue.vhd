--

--

library IEEE;
use IEEE.STD_LOGIC_1164.all;


use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;

use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package LogicIssue is

function TMP_restoreState(full: std_logic; ins: InstructionState; st: SchedulerState) return SchedulerEntrySlot;

function getDispatchArgValues(ins: InstructionState; st: SchedulerState; fni: ForwardingInfo;
											prevSending: std_logic;
											USE_IMM: boolean; REGS_ONLY, DELAY_ONLY: boolean)
return SchedulerEntrySlot;

function updateDispatchArgs(ins: InstructionState; st: SchedulerState; vals: MwordArray; regValues: MwordArray)
return SchedulerEntrySlot;


	function TMP_getIssuedMask(elems: SchedulerEntrySlotArray) return std_logic_vector;
	function TMP_setUntil(selVec: std_logic_vector; nextAccepting: std_logic) return std_logic_vector;

function iqContentNext(queueContent: SchedulerEntrySlotArray; inputDataS: SchedulerEntrySlotArray;
                                 killMask,
								 --remainMask,-- fullMask,-- livingMask,
								 selMask--, issuedMask
								 : std_logic_vector;
								 sends, sent, sentUnexpected, prevSending: std_logic)
return SchedulerEntrySlotArray;


function extractReadyMaskNew(entryVec: SchedulerEntrySlotArray) return std_logic_vector;

function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector;
function findLoc2b(cmp: std_logic_vector) return SmallNumber;

function updateSchedulerArray(insArray: SchedulerEntrySlotArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; progressLocs, dynamic: boolean)
return SchedulerEntrySlotArray;

function findForwardingMatchesArray(insArray: SchedulerEntrySlotArray; fni: ForwardingInfo) return ForwardingMatchesArray;

end LogicIssue;



package body LogicIssue is

function getWakeupPhase(fnm: ForwardingMap; ready1, ready0, readyM1, readyM2: std_logic_vector; progress: boolean) return SmallNumberArray is
    variable res: SmallNumberArray(0 to 2) := (others => "11111110"); -- -2 
begin
    for i in res'range loop
        if progress then
            if ready1(i) = '1' then
                res(i) := "00000010";        
            elsif ready0(i) = '1' then
                res(i) := "00000001";
            elsif readyM1(i) = '1' then
                res(i) := "00000000";
            else
                res(i) := "11111111";                       
            end if;    
        else
            if ready1(i) = '1' then
                res(i) := "00000001";        
            elsif ready0(i) = '1' then
                res(i) := "00000000";
            elsif readyM1(i) = '1' then
                res(i) := "11111111";
            else
                res(i) := "11111110";                       
            end if;
        end if;
        res(i)(7 downto 2) := (others => '0');
    end loop;

    return res;
end function;

function getWakeupReady(fnm: ForwardingMap; progress: boolean; wf: SmallNumberArray; ready1, ready0, readyM1, readyM2: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to 2) := (others => '0');
begin
    for i in res'range loop
        if true then
            if fnm.maskM2(i) = '1' then
                res(i) := readyM2(i);
            elsif fnm.maskM1(i) = '1' then
                res(i) := readyM1(i);        
            elsif fnm.maskR0(i) = '1' then
                res(i) := ready0(i);
            else
                res(i) := ready1(i);        
            end if;
        end if;
    end loop;

    return res;
end function;

function getWakeupVector(fnm: ForwardingMap; wf: SmallNumberArray; maskR1, maskR0, maskM1, maskM2, ready1, ready0, readyM1, readyM2: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to 2) := (others => '0');
begin
    for i in res'range loop
            if fnm.maskR1(i) = '1' then
                res(i) := ready1(i);
            elsif fnm.maskR0(i) = '1' then
                res(i) := ready0(i);
            elsif fnm.maskM1(i) = '1' then
                res(i) := readyM1(i);                       
            else
                res(i) := readyM2(i);  
            end if;
    end loop;

    return res;
end function;


function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector is
	variable res: std_logic_vector(list'range) := (others => '0');
begin
	for i in list'range loop
		if tag(PHYS_REG_BITS-1 downto 0) = list(i)(PHYS_REG_BITS-1 downto 0) then
			res(i) := '1';
		end if;
	end loop;
	return res;
end function;

function findLoc2b(cmp: std_logic_vector) return SmallNumber is
	variable res: SmallNumber := (others => '0');
begin
	-- TODO: if more than 3 source subpipes possible, need to update 
	if cmp(1) = '1' then
		res(1 downto 0) := "01";
	elsif cmp(2) = '1' then
		res(1 downto 0) := "10";
	end if;
	
	res(0) := cmp(1);
	res(1) := cmp(2);
	return res;
end function;


function updateArgLocs_Issue(ss: SchedulerState; readyBefore: std_logic_vector)
return SchedulerState is
	variable res: SchedulerState := ss;
begin
	for i in 0 to 1 loop
		if readyBefore(i) = '1' then
            case res.argLocsPhase(i)(1 downto 0) is
                when "11" =>
                    res.argLocsPhase(i) := "00000000";
                when "00" =>
                    res.argLocsPhase(i) := "00000001";				
                when others =>
                    res.argLocsPhase(i) := "00000010";
            end case;
		end if;

		res.argLocsPhase(i)(7 downto 2) := "000000";
		res.argLocsPipe(i)(7 downto 2) := "000000";
	end loop;

	return res;
end function;


function updateArgLocs(ss: SchedulerState;
                              readyBefore: std_logic_vector;
                              wakeupPhases0, wakeupPhases1: SmallNumberArray;
							  wakeupVec0, wakeupVec1: std_logic_vector;
							  progress: boolean)
return SchedulerState is
	variable res: SchedulerState := ss;
	variable wakeupVec: std_logic_vector(0 to 2) := (others => '0');
	variable wakeupPhases: SmallNumberArray(0 to 2) := (others => (others => '0'));  
begin
	for i in 0 to 1 loop
	   if i = 0 then
	       wakeupVec := wakeupVec0;
	       wakeupPhases := wakeupPhases0;
	   elsif i = 1 then
	       wakeupVec := wakeupVec1;
	       wakeupPhases := wakeupPhases1;
	   end if;
	
	   if progress then
            if readyBefore(i) = '1' then
                case res.argLocsPhase(i)(1 downto 0) is
                    when "11" =>
                        res.argLocsPhase(i) := "00000000";
                    when "00" =>
                        res.argLocsPhase(i) := "00000001";				
                    when others =>
                        res.argLocsPhase(i) := "00000010";
                end case;
            else
                for j in 0 to 2 loop
                    if wakeupVec(j) = '1' then
                        res.argLocsPipe(i) := i2slv(j, SMALL_NUMBER_SIZE);                    
                        res.argLocsPhase(i) := wakeupPhases(j);
                        exit;                    
                    end if;
                end loop;
            end if;
	   else -- not progress
            if readyBefore(i) = '1' then

            else
                for j in 0 to 2 loop
                    if wakeupVec(j) = '1' then
                        res.argLocsPipe(i) := i2slv(j, SMALL_NUMBER_SIZE);                    
                        res.argLocsPhase(i) := wakeupPhases(j);
                        exit;                    
                    end if;
                end loop;
            end if;       
        end if;
        
		res.argLocsPhase(i)(7 downto 2) := "000000";
		res.argLocsPipe(i)(7 downto 2) := "000000";
	end loop;

	return res;
end function;


function TMP_restoreState(full: std_logic; ins: InstructionState; st: SchedulerState) return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	variable v0, v1: std_logic_vector(1 downto 0) := "00";
	variable selected0, selected1: Mword := (others => '0');
	variable ready: std_logic_vector(0 to 2) := (others=>'0');
	variable locs: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	constant Z3: std_logic_vector(0 to 2) := (others => '0');
	constant ZZ3: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	variable imm: Word := (others => '0');
begin
    res.full := full;
	res.ins := ins;
	res.state := st;

    res.ins.tags.renameIndex := st.renameIndex;
    res.ins.tags.bqPointer := st.bqPointer;
    res.ins.tags.sqPointer := st.sqPointer;
    res.ins.tags.lqPointer := st.lqPointer;

    res.ins.specificOperation := st.operation;

    res.ins.physicalArgSpec.dest := res.state.argSpec.dest;
    res.ins.physicalArgSpec.intDestSel := res.state.argSpec.intDestSel;
    res.ins.physicalArgSpec.floatDestSel := res.state.argSpec.floatDestSel;
    
    res.ins.physicalArgSpec.intArgSel := (others => '0');
    res.ins.physicalArgSpec.floatArgSel := (others => '0');
        
    res.ins.physicalArgSpec.args := res.state.argSpec.args;    
	return res;
end function;


function getDispatchArgValues(ins: InstructionState; st: SchedulerState; fni: ForwardingInfo;
											prevSending: std_logic;
											USE_IMM: boolean; REGS_ONLY, DELAY_ONLY: boolean)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	variable v0, v1: std_logic_vector(1 downto 0) := "00";
	variable selected0, selected1: Mword := (others => '0');
	variable ready: std_logic_vector(0 to 2) := (others=>'0');
	variable locs: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	constant Z3: std_logic_vector(0 to 2) := (others => '0');
	constant ZZ3: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	variable imm: Word := (others => '0');
begin
	res.ins := ins;
	res.state := st;

    imm(15 downto 0) := res.state.immValue;
    imm(31 downto 16) := (others => res.state.immValue(15));
    
    if prevSending = '0' or
          (st.argSpec.intDestSel = '0' and st.argSpec.floatDestSel = '0') -- ???          
    then
        res.ins.physicalArgSpec.dest := (others => '0'); -- Don't allow false notifications of args
        res.state.argSpec.dest := (others => '0'); -- Don't allow false notifications of args
    end if;

		if res.state.zero(0) = '1' then
			res.state.args(0) := (others => '0');
			res.state.stored(0) := '1';
		elsif res.state.argLocsPhase(0)(1 downto 0) = "00" then
			res.state.args(0) := fni.values0(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
			res.state.stored(0) := '1';
		else --elsif res.state.argPhase(1 downto 0) := "01" then
			res.state.args(0) := fni.values1(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
			if res.state.argLocsPhase(0)(1 downto 0) = "01" then
				res.state.stored(0) := '1';
		    end if;
		end if;

	if false and res.state.immediate = '1' and USE_IMM then
		res.state.args(1) := imm;
		if IMM_AS_REG then
		    res.state.args(1)(PhysName'length-1 downto 0) := res.state.argSpec.args(1);
		end if;		
		res.state.stored(1) := '1';
	else
		if res.state.zero(1) = '1' then
		    if USE_IMM then
		        res.state.args(1)(31 downto 16) := (others => res.state.immValue(15));
		        res.state.args(1)(15 downto 0) := res.state.immValue;
		        if IMM_AS_REG then
                    res.state.args(1)(PhysName'length-1 downto 0) := res.state.argSpec.args(1);
                end if;                                                               
            else
                res.state.args(1) := (others => '0');
            end if;
			res.state.stored(1) := '1';
		elsif res.state.argLocsPhase(1)(1 downto 0) = "00" then
			res.state.args(1) := fni.values0(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
			res.state.stored(1) := '1';
		else --elsif res.state.argPhase(1 downto 0) := "01" then
			res.state.args(1) := fni.values1(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
			if res.state.argLocsPhase(1)(1 downto 0) = "01" then
                res.state.stored(1) := '1';
            end if;				
		end if;
	end if;

    if REGS_ONLY or DELAY_ONLY then
        res.state.stored := (others => '0');
    end if;

    ready := (others => '1');--not res.state.missing;
    res.state := updateArgLocs_Issue(res.state, ready);
    
    if CLEAR_DEBUG_INFO then
        res.ins := clearDbCounters(res.ins);
        res.ins := clearAbstractInfo(res.ins);
        
        res.ins.controlInfo := DEFAULT_CONTROL_INFO;

        res.ins.classInfo := DEFAULT_CLASS_INFO;
        res.ins.constantArgs.imm := (others => '0');

        res.ins.virtualArgSpec := DEFAULT_ARG_SPEC;
    end if;
    
	return res;
end function;


function updateDispatchArgs(ins: InstructionState; st: SchedulerState; vals: MwordArray; regValues: MwordArray)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	variable aa: MwordArray(0 to 5) := (others => (others => '0'));
	variable ind: integer := 0;
	variable selector: std_logic_vector(0 to 1) := "00";
	variable tbl: MwordArray(0 to 3) := (others => (others => '0'));
	variable carg0, carg1, carg2: Mword;
begin
	res.ins := ins;
	res.state := st;

	-- Clear 'missing' flag where readyNext indicates.

    if res.state.argLocsPhase(0)(1 downto 0) = "00" and res.state.stored(0) = '0' then
        res.state.args(0) := vals(slv2u(res.state.argLocsPipe(0)(1 downto 0)));
    elsif res.state.argLocsPhase(0)(1 downto 0) = "10" and res.state.stored(0) = '0' then
        res.state.args(0) := regValues(0);
    end if;

    if res.state.argLocsPhase(1)(1 downto 0) = "00" and res.state.stored(1) = '0' then
        res.state.args(1) := vals(slv2u(res.state.argLocsPipe(1)(1 downto 0)));
    elsif res.state.argLocsPhase(1)(1 downto 0) = "10" and res.state.stored(1) = '0' then -- and res.state.immediate = '0' then
        res.state.args(1) := regValues(1);
    end if;
	
	return res;
end function;


function extractReadyMaskNew(entryVec: SchedulerEntrySlotArray) return std_logic_vector is
	variable res: std_logic_vector(entryVec'range);
begin	
	for i in res'range loop
		res(i) := not isNonzero(entryVec(i).state.missing(0 to 1))      and not entryVec(i).state.issued;
	end loop;
	return res;
end function;


	function TMP_getIssuedMask(elems: SchedulerEntrySlotArray) return std_logic_vector is
        variable res: std_logic_vector(0 to elems'length-1) := (others => '0');
	begin
		for i in 0 to elems'length-1 loop
		    res(i) := elems(i).state.issued;
		end loop;
		return res;
    end function;

	function TMP_setUntil(selVec: std_logic_vector; nextAccepting: std_logic) return std_logic_vector is
		variable res: std_logic_vector(0 to selVec'length-1) := (others => '0');
	begin
		for i in res'range loop
			if (selVec(i) and nextAccepting) = '1' then
				exit;
			else
				res(i) := '1';
			end if;
		end loop;
		return res;
	end function;
	
function iqContentNext(queueContent: SchedulerEntrySlotArray; inputDataS: SchedulerEntrySlotArray;
                                 killMask,
								 --remainMask,-- fullMask,-- livingMask,
								 selMask--, issuedMask
								 : std_logic_vector;
								 sends, sent, sentUnexpected, prevSending: std_logic)
return SchedulerEntrySlotArray is
	constant QUEUE_SIZE: natural := queueContent'length;
	variable res: SchedulerEntrySlotArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_SCH_ENTRY_SLOT); 	
	variable newMask: std_logic_vector(0 to PIPE_WIDTH-1) := extractFullMask(inputDataS);--inputData.fullMask;--
	variable compMask: std_logic_vector(0 to PIPE_WIDTH-1) := compactMask(newMask);
	variable dataNewDataS: SchedulerEntrySlotArray(0 to PIPE_WIDTH-1) := inputDataS;
	
	variable iqDataNextS: SchedulerEntrySlotArray(0 to QUEUE_SIZE - 1) := (others => DEFAULT_SCH_ENTRY_SLOT);
	variable iqFullMaskNext: std_logic_vector(0 to QUEUE_SIZE - 1) :=	(others => '0');
    variable iqRemainingMaskSh: std_logic_vector(0 to QUEUE_SIZE + 4 - 1) := (others => '0');

	variable xVecS: SchedulerEntrySlotArray(0 to QUEUE_SIZE + PIPE_WIDTH - 1);
	constant fullMask: std_logic_vector(0 to QUEUE_SIZE-1) := extractFullMask(queueContent);
	constant issuedMask: std_logic_vector(0 to QUEUE_SIZE-1) := TMP_getIssuedMask(queueContent);
	constant remainMask: std_logic_vector(0 to QUEUE_SIZE-1) := TMP_setUntil(issuedMask, '1');
	
	variable fullMaskSh: std_logic_vector(0 to QUEUE_SIZE-1) := fullMask;
	constant livingMask: std_logic_vector(0 to QUEUE_SIZE-1) := fullMask and not killMask;	
	variable livingMaskSh: std_logic_vector(0 to QUEUE_SIZE-1) := livingMask;
	variable fillMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');	
begin
	-- Important, new instrucitons in queue must be marked!	
	for i in 0 to PIPE_WIDTH-1 loop
		dataNewDataS(i).state.newInQueue := '1';
	end loop;
  
	xVecS := queueContent & dataNewDataS;
	
	-- What is being issued now is marked
    for i in 0 to QUEUE_SIZE-1 loop
        if selMask(i) = '1' and sends = '1' then
            xVecS(i).state.issued := '1';
        end if;
        
        -- Retraction into IQ when sending turns out disallowed
        if issuedMask(i) = '1' and sentUnexpected = '1' then
        --    xVecS(i).state.issued := '0';
        end if;  
    end loop;	
	
	xVecS(QUEUE_SIZE) := xVecS(QUEUE_SIZE-1);
	for i in 0 to QUEUE_SIZE + PIPE_WIDTH - 1 loop
		xVecS(i).state.newInQueue := '0';
	end loop;

	for i in 0 to QUEUE_SIZE-2 loop
		livingMaskSh(i) := livingMask(i) and (livingMask(i+1) or not sent);
		fullMaskSh(i) := fullMask(i) and (fullMask(i+1) or not sent);			
	end loop;
	livingMaskSh(QUEUE_SIZE-1) := livingMask(QUEUE_SIZE-1) and ('0' or not sent);
	fullMaskSh(QUEUE_SIZE-1) := fullMask(QUEUE_SIZE-1) and ('0' or not sent);

	-- Now assign from x or y
	iqRemainingMaskSh(0 to 3) := (others => '1');
	iqRemainingMaskSh(4 to QUEUE_SIZE + 4 - 1) := fullMaskSh;
	iqDataNextS := queueContent;
	for i in 0 to QUEUE_SIZE-1 loop		    
        fillMask(i) := '0';
        for k in 0 to 3 loop -- Further beyond end requires more full inputs to be filled: !! equiv to remainingMask(-1-k), where '1' for k < 0
            fillMask(i) := fillMask(i) or (iqRemainingMaskSh(i + 3-k) and compMask(k));
        end loop;
	      
		iqFullMaskNext(i) := livingMaskSh(i) or (fillMask(i) and prevSending);
		if fullMaskSh(i) = '1' then -- From x	
			if remainMask(i) = '1' then
				iqDataNextS(i) := xVecS(i);
			else
				iqDataNextS(i) := xVecS(i + 1);
			end if;
		else -- From y
			iqDataNextS(i) := getNewElemSch(iqRemainingMaskSh(i+1 to i+3), dataNewDataS);
		end if;
	end loop;

	-- Fill output array
	for i in 0 to res'right loop
	   res(i).full := iqFullMaskNext(i);
	   if not CLEAR_DEBUG_INFO then
	       res(i).ins := iqDataNextS(i).ins;
	   else
	       res(i).ins := DEFAULT_INS_STATE;
       end if; 

	   res(i).state := iqDataNextS(i).state;	
	   res(i).state.stored := (others => '0');
	   res(i).state.args := (others => (others => '0'));
	end loop;

	return res;
end function;



function updateSchedulerState(ins: InstructionState; st: SchedulerState;
										fni: ForwardingInfo;
										fm: ForwardingMatches;
										fnm: ForwardingMap; progressLocs, dynamic: boolean)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCHEDULER_ENTRY_SLOT;
	variable tmp8: SmallNumber := (others => '0');
	variable cmp0toM2, cmp0toM1, cmp0toR0, cmp0toR1, cmp1toM2, cmp1toM1, cmp1toR0, cmp1toR1,
				rrf, readyR0, readyR1, nextReady, readyM2, readyBefore, wakeupVec0, wakeupVec1, readyNew: std_logic_vector(0 to 2) := (others=>'0');
	variable locs, locs0, locs1, nextLocs, locsM2, wakeupPhases, wakeupPhases0, wakeupPhases1: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	
begin
	res.ins := ins;	
	res.state := st;       
        		
    cmp0toR0 := fm.a0cmp0 and fnm.maskR0;
    cmp1toR0 := fm.a1cmp0 and fnm.maskR0;
    cmp0toR1 := fm.a0cmp1 and fnm.maskR1;
    cmp1toR1 := fm.a1cmp1 and fnm.maskR1;
    cmp0toM1 := fm.a0cmpM1 and fnm.maskM1;
    cmp1toM1 := fm.a1cmpM1 and fnm.maskM1;
    cmp0toM2 := fm.a0cmpM2 and fnm.maskM2;
    cmp1toM2 := fm.a1cmpM2 and fnm.maskM2;	
        		
    readyR0 := (isNonzero(cmp0toR0), isNonzero(cmp1toR0), '0');
    readyR1 := (isNonzero(cmp0toR1), isNonzero(cmp1toR1), '0');
    locs0 := (findLoc2b(cmp0toR0), findLoc2b(cmp1toR0), (others => '0'));
    locs1 := (findLoc2b(cmp0toR1), findLoc2b(cmp1toR1), (others => '0'));

    nextReady := (isNonzero(cmp0toM1), isNonzero(cmp1toM1), '0');
    nextLocs := (findLoc2b(cmp0toM1), findLoc2b(cmp1toM1), (others => '0'));
    readyM2 := (isNonzero(cmp0toM2), isNonzero(cmp1toM2), '0');
    locsM2 := (findLoc2b(cmp0toM2), findLoc2b(cmp1toM2), (others => '0'));

    if dynamic then
        wakeupPhases0 := getWakeupPhase(fnm, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, progressLocs);
        wakeupPhases1 := getWakeupPhase(fnm, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, progressLocs);

        wakeupVec0 := getWakeupVector(fnm, wakeupPhases, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2);
        wakeupVec1 := getWakeupVector(fnm, wakeupPhases, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2);
    else
        wakeupPhases0 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
        wakeupPhases1 := getWakeupPhase(fnm, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, progressLocs);
    
        wakeupVec0 := getWakeupVector(fnm, wakeupPhases, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, cmp0toR1, cmp0toR0, cmp0toM1, cmp0toM2);
        wakeupVec1 := getWakeupVector(fnm, wakeupPhases, fnm.maskR1, fnm.maskR0, fnm.maskM1, fnm.maskM2, cmp1toR1, cmp1toR0, cmp1toM1, cmp1toM2);
    end if;
    
	readyBefore := not res.state.missing;
    readyNew := (isNonzero(wakeupVec0), isNonzero(wakeupVec1), '0');

	-- Update arg tracking
	res.state := updateArgLocs(    res.state,
												readyBefore,
												wakeupPhases0,
												wakeupPhases1,
												wakeupVec0,
												wakeupVec1,
												progressLocs
												);
	-- tag broadcast stages for each Int Exec subpipe:
	--  pipe    I0  I1  M0
	--  phase   -1  --  -1
	-- 
	-- for FP:
	-- pipe    F0       M0
	-- phase   -2   --  -2 
	---
	-- getWakeupReady(map, readyR1, readyR0, nextReady, readyM2); -- combined readyX ( [readyM1(0), '0', readyM1(2)] or [i -> ready{wakeupPhase(i)}(i)]
	-- getWakeupPhase(map);  -- constant per IQ; where earliest bit is set in map 
	-- 
	-- 
															
	res.state.missing := res.state.missing and not readyNew;
	
	return res;
end function;


function updateSchedulerArray(insArray: SchedulerEntrySlotArray; fni: ForwardingInfo; fma: ForwardingMatchesArray; fnm: ForwardingMap; progressLocs, dynamic: boolean)
return SchedulerEntrySlotArray is
	variable res: SchedulerEntrySlotArray(0 to insArray'length-1);-- := insArray;
begin
	for i in insArray'range loop
		res(i) := updateSchedulerState(insArray(i).ins, insArray(i).state, fni, fma(i), fnm, progressLocs, dynamic);
	    res(i).full := (insArray(i).full);
	end loop;	
	return res;
end function;


function findForwardingMatches(ins: InstructionState; st: SchedulerState; fni: ForwardingInfo) return ForwardingMatches is
    variable res: ForwardingMatches := DEFAULT_FORWARDING_MATCHES;
begin
	res.a0cmp0 := findRegTag(st.argSpec.args(0), fni.tags0);
    res.a1cmp0 := findRegTag(st.argSpec.args(1), fni.tags0);
    res.a0cmp1 := findRegTag(st.argSpec.args(0), fni.tags1);
    res.a1cmp1 := findRegTag(st.argSpec.args(1), fni.tags1);
    res.a0cmpM1 := findRegTag(st.argSpec.args(0), fni.nextTagsM1);
    res.a1cmpM1 := findRegTag(st.argSpec.args(1), fni.nextTagsM1);
    res.a0cmpM2 := findRegTag(st.argSpec.args(0), fni.nextTagsM2);
    res.a1cmpM2 := findRegTag(st.argSpec.args(1), fni.nextTagsM2); 
    
    return res;
end function;

function findForwardingMatchesArray(insArray: SchedulerEntrySlotArray; fni: ForwardingInfo) return ForwardingMatchesArray is
    variable res: ForwardingMatchesArray(insArray'range) := (others => DEFAULT_FORWARDING_MATCHES);
begin
    for i in insArray'range loop
        res(i) := findForwardingMatches(insArray(i).ins, insArray(i).state, fni);
    end loop;
    return res;
end function;


end LogicIssue;
