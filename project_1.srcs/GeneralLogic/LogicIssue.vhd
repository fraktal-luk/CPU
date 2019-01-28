--
--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 
--
--   To use any of the example code shown below, uncomment the lines and modify as necessary
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

function getDispatchArgValues(ins: InstructionState; st: SchedulerState; fni: ForwardingInfo;
											prevSending: std_logic;
											USE_IMM: boolean)
return SchedulerEntrySlot;

function updateDispatchArgs(ins: InstructionState; st: SchedulerState; vals: MwordArray; regValues: MwordArray)
return SchedulerEntrySlot;


function iqContentNext(queueContent: SchedulerEntrySlotArray; inputDataS: SchedulerEntrySlotArray;
								 stayMask, fullMask, livingMask: std_logic_vector;
								 sendPossible, sends: std_logic;
								 prevSending: std_logic)
return SchedulerEntrySlotArray;

function extractReadyMaskNew(entryVec: SchedulerEntrySlotArray) return std_logic_vector;



function updateForWaitingFNI(ins: InstructionState; st: SchedulerState;
										readyRegFlags: std_logic_vector; fni: ForwardingInfo)--;
									--isNew: std_logic)
return SchedulerEntrySlot;

function updateForWaitingNewFNI(ins: InstructionState; st: SchedulerState;
										readyRegFlags: std_logic_vector; fni: ForwardingInfo)--;
									--isNew: std_logic)
return SchedulerEntrySlot;

function updateForSelectionFNI(ins: InstructionState; st: SchedulerState;
											readyRegFlags: std_logic_vector; fni: ForwardingInfo)
return SchedulerEntrySlot;


function updateForWaitingArrayFNI(insArray: SchedulerEntrySlotArray; readyRegFlags: std_logic_vector;
									fni: ForwardingInfo)--; isNew: std_logic)
return SchedulerEntrySlotArray;

function updateForWaitingArrayNewFNI(insArray: SchedulerEntrySlotArray; readyRegFlags: std_logic_vector;
									fni: ForwardingInfo)--; isNew: std_logic)
return SchedulerEntrySlotArray;


function updateForSelectionArrayFNI(insArray: SchedulerEntrySlotArray; readyRegFlags: std_logic_vector;
									fni: ForwardingInfo)
return SchedulerEntrySlotArray;


function findRegTag(tag: SmallNumber; list: PhysNameArray) return std_logic_vector;

function findLoc2b(cmp: std_logic_vector) return SmallNumber;

function updateArgLocs(argValues: InstructionArgValues;
							  readyBefore, readyReg, ready1, ready0, readyM1, readyM2: std_logic_vector; 
															 locs1, locs0, locsM1, locsM2: SmallNumberArray;
							  progress: boolean)
return InstructionArgValues;


end LogicIssue;



package body LogicIssue is


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
	if cmp(1) = '1' then
		res(1 downto 0) := "01";
	elsif cmp(2) = '1' then
		res(1 downto 0) := "10";
	end if;
	
		res(0) := cmp(1);
		res(1) := cmp(2);
	return res;
end function;

function updateArgLocs(argValues: InstructionArgValues;
							  readyBefore, readyReg, ready1, ready0, readyM1, readyM2: std_logic_vector; 
															 locs1, locs0, locsM1, locsM2: SmallNumberArray;
							  progress: boolean)
return InstructionArgValues is
	variable res: InstructionArgValues := argValues;
begin
	for i in 0 to 1 loop
		if readyBefore(i) = '1' then
			if progress then
			-- Progress the location for next cycle?
				case res.argLocsPhase(i)(1 downto 0) is
					when "11" =>
						res.argLocsPhase(i) := "00000000";
					when "00" =>
						res.argLocsPhase(i) := "00000001";
					--when "01" =>
					-- res.argLocsPhase(0) := "10";					
					when others =>
						res.argLocsPhase(i) := "00000010";
				end case;
			end if;
		elsif readyReg(i) = '1' then
			res.argLocsPhase(i) := "00000010";
		elsif ready1(i) = '1' then
			res.argLocsPipe(i) := locs1(i);
			if progress then
				res.argLocsPhase(i) := "00000010";		
			else
				res.argLocsPhase(i) := "00000001";				
			end if;
		elsif ready0(i) = '1' then
			res.argLocsPipe(i) := locs0(i);
			if progress then
				res.argLocsPhase(i) := "00000001";		
			else
				res.argLocsPhase(i) := "00000000";				
			end if;			
		elsif readyM1(i) = '1' then
		-- Store M1 loc
			res.argLocsPipe(i) := locsM1(i);
			if progress then
				res.argLocsPhase(i) := "00000000";		
			else
				res.argLocsPhase(i) := "00000011";				
			end if;
		elsif readyM2(i) = '1' then
		-- Store M2 loc
			res.argLocsPipe(i) := locsM2(i);
			if progress then
				res.argLocsPhase(i) := "00000011";		
			else
				report "Slow wakeup can be used only for waiting ops!" severity error;
				res.argLocsPhase(i) := "00000011";				
			end if;
		end if;
		
		res.argLocsPhase(i)(7 downto 2) := "000000";
		res.argLocsPipe(i)(7 downto 2) := "000000";		
	end loop;

	return res;
end function;

function selectUpdatedArg(avs: InstructionArgValues; ind: integer; immed: std_logic; def: Mword;
								  vals: MwordArray; regValues: MwordArray)
return Mword is
	variable res: Mword := def;
	variable selector: std_logic_vector(1 downto 0) := "00";
	variable tbl: MwordArray(0 to 3) := (others => (others => '0'));
begin

	if	(avs.readyNext(ind) and not avs.zero(ind) and not immed) = '1' then
		-- Use new value from Exec
		res := vals(slv2u(avs.nextLocs(ind)));		
		selector := avs.nextLocs(ind)(1 downto 0);
	elsif (avs.readyNow(ind) and not avs.zero(ind)) = '1' then
		res := def;
		selector := "10";
	else -- Use register value
		res := regValues(ind);
		selector := "11";
	end if;

	tbl(0) := vals(0);
	tbl(1) := vals(1);
	tbl(2) := def;
	tbl(3) := regValues(ind);

	case selector is
		when "00" => 
			res := tbl(0);
		when "01" => 
			res := tbl(1);
		when "10" => 
			res := tbl(2);
		when others => 
			res := tbl(3);
	end case;

	return res;
end function;



function getDispatchArgValues(ins: InstructionState; st: SchedulerState; fni: ForwardingInfo;
											prevSending: std_logic;
											USE_IMM: boolean)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCH_ENTRY_SLOT;
	variable v0, v1: std_logic_vector(1 downto 0) := "00";
	variable selected0, selected1: Mword := (others => '0');
	variable ready: std_logic_vector(0 to 2) := (others=>'0');
	variable locs: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
begin
	res.ins := ins;
	res.state := st;

    if prevSending = '0' or ins.physicalArgSpec.intDestSel = '0' then
        res.ins.physicalArgSpec.dest := (others => '0'); -- Don't allow false notifications of args
    end if;

	res.state.argValues.readyNow := ready;
	res.state.argValues.locs := locs;

		if res.state.argValues.zero(0) = '1' then
			res.state.argValues.arg0 := (others => '0');
		elsif res.state.argValues.argLocsPhase(0)(1 downto 0) = "00" then
			res.state.argValues.arg0 := fni.values0(slv2u(res.state.argValues.argLocsPipe(0)(1 downto 0)));
		else --elsif res.state.argValues.argPhase(1 downto 0) := "01" then
			res.state.argValues.arg0 := fni.values1(slv2u(res.state.argValues.argLocsPipe(0)(1 downto 0)));			
		end if;


	if res.state.argValues.immediate = '1' and USE_IMM then
		res.state.argValues.arg1 := res.ins.constantArgs.imm;
		res.state.argValues.arg1(31 downto 17) := (others => res.ins.constantArgs.imm(16)); -- 16b + addditional sign bit
	else
		if res.state.argValues.zero(1) = '1' then
			res.state.argValues.arg1 := (others => '0');
		elsif res.state.argValues.argLocsPhase(1)(1 downto 0) = "00" then
			res.state.argValues.arg1 := fni.values0(slv2u(res.state.argValues.argLocsPipe(1)(1 downto 0)));
		else --elsif res.state.argValues.argPhase(1 downto 0) := "01" then
			res.state.argValues.arg1 := fni.values1(slv2u(res.state.argValues.argLocsPipe(1)(1 downto 0)));			
		end if;
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
	res.state.argValues.missing := res.state.argValues.missing and not (res.state.argValues.readyNext and not res.state.argValues.zero);

		if res.state.argValues.argLocsPhase(0)(1 downto 0) = "11" then--and res.state.argValues.zero(0) = '0' then
			res.state.argValues.arg0 := vals(slv2u(res.state.argValues.argLocsPipe(0)(1 downto 0)));
		elsif res.state.argValues.argLocsPhase(0)(1 downto 0) = "10" then
			res.state.argValues.arg0 := regValues(0);
		end if;

		if res.state.argValues.argLocsPhase(1)(1 downto 0) = "11" then--and res.state.argValues.zero(0) = '0' then
			res.state.argValues.arg1 := vals(slv2u(res.state.argValues.argLocsPipe(1)(1 downto 0)));
		elsif res.state.argValues.argLocsPhase(1)(1 downto 0) = "10" and res.state.argValues.immediate = '0' then
			res.state.argValues.arg1 := regValues(1);
		end if;
	
	return res;
end function;


function extractReadyMaskNew(entryVec: SchedulerEntrySlotArray) return std_logic_vector is
	variable res: std_logic_vector(entryVec'range);
begin	
	for i in res'range loop
		res(i) := not isNonzero(entryVec(i).state.argValues.missing(0 to 1));
	end loop;
	return res;
end function;


function iqContentNext(queueContent: SchedulerEntrySlotArray; inputDataS: SchedulerEntrySlotArray;
								 stayMask, fullMask, livingMask: std_logic_vector;
								 sendPossible, sends: std_logic;
								 prevSending: std_logic)
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
	variable fullMaskSh: std_logic_vector(0 to QUEUE_SIZE-1) := fullMask;
	variable livingMaskSh: std_logic_vector(0 to QUEUE_SIZE-1) := livingMask;
	variable fillMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');	
	--variable remv: std_logic_vector(0 to 2) := "000";
	--variable sel: std_logic_vector(1 downto 0) := "00";		
begin
	-- Important, new instrucitons in queue must be marked!	
	for i in 0 to PIPE_WIDTH-1 loop
		dataNewDataS(i).state.argValues.newInQueue := '1';
	end loop;

	xVecS := queueContent & dataNewDataS;
	xVecS(QUEUE_SIZE) := xVecS(QUEUE_SIZE-1);
	for i in 0 to QUEUE_SIZE + PIPE_WIDTH - 1 loop
		xVecS(i).state.argValues.newInQueue := '0';
	end loop;

	for i in 0 to QUEUE_SIZE-2 loop
		livingMaskSh(i) := livingMask(i) and (livingMask(i+1) or not sends);
		fullMaskSh(i) := fullMask(i) and (fullMask(i+1) or not sendPossible);			
	end loop;
	livingMaskSh(QUEUE_SIZE-1) := livingMask(QUEUE_SIZE-1) and ('0' or not sends);
	fullMaskSh(QUEUE_SIZE-1) := fullMask(QUEUE_SIZE-1) and ('0' or not sendPossible);

	-- Now assign from x or y
	iqRemainingMaskSh(0 to 3) := (others => '1');
	iqRemainingMaskSh(4 to QUEUE_SIZE + 4 - 1) := fullMaskSh;
	iqDataNextS := queueContent;
	for i in 0 to QUEUE_SIZE-1 loop
		--remv := iqRemainingMaskSh(i+1 to i+3);	                   
	    
        fillMask(i) := '0';
        for k in 0 to 3 loop -- Further beyond end requires more ful inputs to be filled:
            --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
            fillMask(i) := fillMask(i) or (iqRemainingMaskSh(i + 3-k) and compMask(k));
        end loop;
	      
		iqFullMaskNext(i) := livingMaskSh(i) or (fillMask(i) and prevSending);
		if fullMaskSh(i) = '1' then -- From x	
			if stayMask(i) = '1' then
				iqDataNextS(i) := xVecS(i);
			else
				iqDataNextS(i) := xVecS(i + 1);
			end if;
		else -- From y
		    --sel := getSelector(remv, newMask(0 to 2));    
			iqDataNextS(i) := --dataNewDataS(slv2u(sel));  -- Not using get n;
			                  getNewElemSch(iqRemainingMaskSh(i+1 to i+3), dataNewDataS);
		end if;
	end loop;

	-- Fill output array
	for i in 0 to res'right loop
		res(i).full := iqFullMaskNext(i);
		res(i).ins := iqDataNextS(i).ins;
		res(i).state := iqDataNextS(i).state;
	end loop;

	return res;
end function;


function updateForWaitingFNI(ins: InstructionState; st: SchedulerState;
										readyRegFlags: std_logic_vector; fni: ForwardingInfo)--;
									--isNew: std_logic)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCHEDULER_ENTRY_SLOT;
	variable tmp8: SmallNumber := (others => '0');
	variable readyBefore, rrf: std_logic_vector(0 to 2) := (others => '0');

	variable cmp0toM1, cmp0toM2, cmp1toM1, cmp1toM2, stored, ready, nextReady, readyM2, written:
													std_logic_vector(0 to 2) := (others=>'0');
	variable locs, nextLocs, locsM1, locsM2: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	constant Z3: std_logic_vector(0 to 2) := (others=>'0');
	constant ZZ3: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
begin
	res.ins := ins;	
	res.state := st;

	cmp0toM1 := findRegTag(ins.physicalArgSpec.args(0), fni.nextResultTags); -- CAREFUL: (0)
	cmp1toM1 := findRegTag(ins.physicalArgSpec.args(1), fni.nextResultTags); --			 (0)
	cmp0toM2 := findRegTag(ins.physicalArgSpec.args(0), fni.nextTagsM2); --			 (1,2)
	cmp1toM2 := findRegTag(ins.physicalArgSpec.args(1), fni.nextTagsM2); -- 			 (1,2)
		
	--	cmp0toM1(1) := '0';
	--	cmp1toM1(1) := '0';
		
		nextReady := (isNonzero(cmp0toM1(0 to 2)), isNonzero(cmp1toM1(0 to 2)), '0');
		readyM2 := (isNonzero(cmp0toM2(1 to 1)), isNonzero(cmp1toM2(1 to 1)), '0');
	 locsM1 := (findLoc2b(cmp0toM1), findLoc2b(cmp1toM1), (others => '0'));
	 locsM2 := (findLoc2b(cmp0toM2), findLoc2b(cmp1toM2), (others => '0'));


	res.state.argValues.readyNext := nextReady;
	
	res.state.argValues.readyM2 := readyM2;
	res.state.argValues.locsM2 := locsM2;

	-- 
	if res.state.argValues.newInQueue = '1' then
		tmp8 := "000000" & res.state.argValues.origSlot;
		      -- getTagLowSN(res.ins.tags.renameIndex);-- and i2slv(PIPE_WIDTH-1, SMALL_NUMBER_SIZE);
		rrf := readyRegFlags(3*slv2u(tmp8) to 3*slv2u(tmp8) + 2);
	else
		rrf := (others => '0');
	end if;

		readyBefore := not res.state.argValues.missing;

		-- Update arg tracking
		res.state.argValues := updateArgLocs(res.state.argValues,
														readyBefore, rrf, Z3, Z3, nextReady, readyM2,
																				ZZ3, ZZ3, locsM1, locsM2,
														true);

	res.state.argValues.missing := res.state.argValues.missing and not rrf;

	res.state.argValues.missing := res.state.argValues.missing and not nextReady;	
	res.state.argValues.missing := res.state.argValues.missing and not readyM2;
	
	res.ins.ip := (others => '0');
	return res;
end function;


function updateForWaitingNewFNI(ins: InstructionState; st: SchedulerState;
										readyRegFlags: std_logic_vector; fni: ForwardingInfo)--;
									--isNew: std_logic)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCHEDULER_ENTRY_SLOT;
	variable tmp8: SmallNumber := (others => '0');
	variable rrf: std_logic_vector(0 to 2) := (others => '0');

	variable cmp0toM2, cmp0toM1, cmp0toR0, cmp0toR1, cmp1toM2, cmp1toM1, cmp1toR0, cmp1toR1,
				readyR0, readyR1,
				stored, ready, nextReady, readyM2, written, readyBefore: std_logic_vector(0 to 2) := (others=>'0');
	variable locs, locs0, locs1, nextLocs, locsM2: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
begin
	res.ins := ins;	
	res.state := st;		
	
		cmp0toR0 := findRegTag(ins.physicalArgSpec.args(0), fni.tags0);
		cmp1toR0 := findRegTag(ins.physicalArgSpec.args(1), fni.tags0);
		cmp0toR1 := findRegTag(ins.physicalArgSpec.args(0), fni.tags1);
		cmp1toR1 := findRegTag(ins.physicalArgSpec.args(1), fni.tags1);
		 readyR0 := (isNonzero(cmp0toR0), isNonzero(cmp1toR0), '0');
		 readyR1 := (isNonzero(cmp0toR1), isNonzero(cmp1toR1), '0');
		 locs0 := (findLoc2b(cmp0toR0), findLoc2b(cmp1toR0), (others => '0'));
		 locs1 := (findLoc2b(cmp0toR1), findLoc2b(cmp1toR1), (others => '0'));


		cmp0toM1 := findRegTag(ins.physicalArgSpec.args(0), fni.nextResultTags);
		cmp1toM1 := findRegTag(ins.physicalArgSpec.args(1), fni.nextResultTags);

		 nextReady := (isNonzero(cmp0toM1), isNonzero(cmp1toM1), '0');
		 nextLocs := (findLoc2b(cmp0toM1), findLoc2b(cmp1toM1), (others => '0'));


		cmp0toM2 := findRegTag(ins.physicalArgSpec.args(0), fni.nextTagsM2);
		cmp1toM2 := findRegTag(ins.physicalArgSpec.args(1), fni.nextTagsM2);

		 readyM2 := (isNonzero(cmp0toM2(1 to 1)), isNonzero(cmp1toM2(1 to 1)), '0');
		 locsM2 := (findLoc2b(cmp0toM2), findLoc2b(cmp1toM2), (others => '0'));

	res.state.argValues.readyNow := (others => '0'); 
	res.state.argValues.readyNext := (others => '0');
	
	res.state.argValues.readyM2 := readyM2;
	res.state.argValues.locsM2 := locsM2;

	-- 
	if res.state.argValues.newInQueue = '1' then
		tmp8 := "000000" & res.state.argValues.origSlot;
		          --getTagLowSN(res.ins.tags.renameIndex);-- and i2slv(PIPE_WIDTH-1, SMALL_NUMBER_SIZE);
		--rrf := readyRegFlags(3*slv2u(tmp8) to 3*slv2u(tmp8) + 2);
		--res.state.argValues.missing := res.state.argValues.missing and not rrf;
	end if;

		readyBefore := not res.state.argValues.missing;

		-- Update arg tracking
		res.state.argValues := updateArgLocs(res.state.argValues,
														readyBefore, rrf, readyR1, readyR0, nextReady, readyM2,
																				locs1, locs0, nextLocs, locsM2,
														true);
	
	--res.state.argValues.missing := res.state.argValues.missing and not ready;
	res.state.argValues.missing := res.state.argValues.missing and not readyR0;
	res.state.argValues.missing := res.state.argValues.missing and not readyR1;	
	
	res.state.argValues.missing := res.state.argValues.missing and not nextReady;	
	res.state.argValues.missing := res.state.argValues.missing and not readyM2;
	
	res.ins.ip := (others => '0');
	return res;
end function;


function updateForSelectionFNI(ins: InstructionState; st: SchedulerState;
				readyRegFlags: std_logic_vector; fni: ForwardingInfo)
return SchedulerEntrySlot is
	variable res: SchedulerEntrySlot := DEFAULT_SCHEDULER_ENTRY_SLOT;
	variable tmp8: SmallNumber := (others => '0');
	variable rrf: std_logic_vector(0 to 2) := (others => '0');

	variable cmp0toM1, cmp1toM1, stored, ready, nextReady, written, readyBefore: std_logic_vector(0 to 2) := (others=>'0');
	variable locs, nextLocs: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	constant Z3: std_logic_vector(0 to 2) := (others=>'0');
	constant ZZ3: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
begin
	res.ins := ins;
	res.state := st;
	
	nextLocs := st.argValues.locsM2; -- CAREFUL: a cycle has passed so those locs are now 1 cycle ahead
	nextReady := st.argValues.readyM2;

		cmp0toM1 := findRegTag(ins.physicalArgSpec.args(0), fni.nextResultTags);
		cmp1toM1 := findRegTag(ins.physicalArgSpec.args(1), fni.nextResultTags);
			cmp0toM1(1 to 2) := (others => '0');
			cmp1toM1(1 to 2) := (others => '0');
		 nextReady := (isNonzero(cmp0toM1(0 to 0)), isNonzero(cmp1toM1(0 to 0)), '0');
		 nextLocs := (findLoc2b(cmp0toM1), findLoc2b(cmp1toM1), (others => '0'));

	res.state.argValues.readyNow := (others => '0'); 
	res.state.argValues.readyNext := nextReady;--(others => '0');

	-- Checking reg ready flags (only for new ops in queue)
	-- CAREFUL! Which reg ready flags are for this instruction?
	--				Use groupTag, because it identifies the slot in previous superscalar stage
	if res.state.argValues.newInQueue = '1' then
		tmp8 := "000000" & res.state.argValues.origSlot;
		      --getTagLowSN(res.ins.tags.renameIndex);-- and i2slv(PIPE_WIDTH-1, SMALL_NUMBER_SIZE);
		rrf := readyRegFlags(3*slv2u(tmp8) to 3*slv2u(tmp8) + 2);
	else
		rrf := (others => '0');
	end if;


		readyBefore := not res.state.argValues.missing;

		-- Update arg tracking
		res.state.argValues := updateArgLocs(res.state.argValues,
														readyBefore, rrf, Z3, Z3, nextReady, Z3,
																				ZZ3, ZZ3, nextLocs, ZZ3,
														false);
														

	res.state.argValues.missing := res.state.argValues.missing and not rrf;

	
	res.state.argValues.missing := res.state.argValues.missing and not nextReady;
	
	res.state.argValues.locs := locs;	
	res.state.argValues.nextLocs := nextLocs;

	-- Clear unused fields
	res.ins.bits := (others => '0');
	res.ins.result := (others => '0');
	res.ins.target := (others => '0');		
--		
	res.ins.controlInfo.completed := '0';
	res.ins.controlInfo.completed2 := '0';
	res.ins.ip := (others => '0');

	res.ins.controlInfo.newEvent := '0';
	res.ins.controlInfo.hasInterrupt := '0';
	--res.ins.controlInfo.hasReturn := '0';		
	res.ins.controlInfo.exceptionCode := (others => '0');

	return res;
end function;



function updateForWaitingArrayFNI(insArray: SchedulerEntrySlotArray; readyRegFlags: std_logic_vector;
									fni: ForwardingInfo)--; isNew: std_logic)
return SchedulerEntrySlotArray is
	variable res: SchedulerEntrySlotArray(0 to insArray'length-1);-- := insArray;
begin
	for i in insArray'range loop	
		res(i) := updateForWaitingFNI(insArray(i).ins, insArray(i).state, readyRegFlags, fni);
	end loop;
	return res;
end function;


function updateForWaitingArrayNewFNI(insArray: SchedulerEntrySlotArray; readyRegFlags: std_logic_vector;
									fni: ForwardingInfo)--; isNew: std_logic)
return SchedulerEntrySlotArray is
	variable res: SchedulerEntrySlotArray(0 to insArray'length-1);-- := insArray;
begin
	for i in insArray'range loop	
		res(i) := updateForWaitingNewFNI(insArray(i).ins, insArray(i).state, readyRegFlags, fni);
			res(i).full := (insArray(i).full);
	end loop;
	return res;
end function;



function updateForSelectionArrayFNI(insArray: SchedulerEntrySlotArray; readyRegFlags: std_logic_vector;
									fni: ForwardingInfo)
return SchedulerEntrySlotArray is
	variable res: SchedulerEntrySlotArray(0 to insArray'length-1);-- := insArray;
begin
	for i in insArray'range loop
		res(i) := updateForSelectionFNI(insArray(i).ins, insArray(i).state, readyRegFlags, fni);
	end loop;	
	return res;
end function;

end LogicIssue;
