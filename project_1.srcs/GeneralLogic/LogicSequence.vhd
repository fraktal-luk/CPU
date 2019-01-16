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
use work.Arith.all;

use work.ArchDefs.all;
use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;


package LogicSequence is

	function getNextPC(pc: Mword; jumpPC: Mword; jump: std_logic) return Mword;

		-- group:  revTag = causing.groupTag and i2slv(-PIPE_WIDTH, SMALL_NUMBER_SIZE), mask = all ones
		-- sequential: revTag = causing.numberTag, mask = new group's fullMask		
		function nextCtr(ctr: InsTag; rewind: std_logic; revTag: InsTag;
									 allow: std_logic; mask: std_logic_vector) 
		return InsTag;
		
		constant ALL_FULL: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '1');

function getLinkInfo(ins: InstructionState; state: Mword) return InstructionState;


function getLatePCData(commitCausing: InstructionState; int: std_logic; intType: std_logic_vector;
								currentState, linkExc, linkInt, stateExc, stateInt: Mword)
return InstructionState;

function newPCData( commitEvent: std_logic; commitCausing: InstructionState;
						  execEvent: std_logic; execCausing: InstructionState;	
						  frontEvent: std_logic; frontCausing: InstructionState;
						  pcNext: Mword)
return InstructionState;

-- BACK ROUTING
-- Unifies content of ROB slot with BQ, others queues etc. to restore full state needed at Commit
function recreateGroup(insVec: InstructionSlotArray; bqGroup: InstructionSlotArray; prevTarget: Mword)
return InstructionSlotArray;


function isHalt(ins: InstructionState) return std_logic;

--function setInterrupt3(ins: InstructionState; intSignal, start: std_logic) return InstructionState;

function clearControlEvents(ins: InstructionState) return InstructionState;

function getNewEffective(sendingToCommit: std_logic; robDataLiving, dataFromBQV: InstructionSlotArray;
								 lastEffectiveIns, lateTargetIns: InstructionState;
								 --linkAddressInt, linkAddressExc: Mword;
								 --evtPhase1, intPhase1, start: std_logic)
								 evtPhase2: std_logic)

return InstructionSlot;

function getEffectiveMask(newContent: InstructionSlotArray) return std_logic_vector;

function anyEvent(insVec: InstructionSlotArray) return std_logic;

end LogicSequence;



package body LogicSequence is

function getNextPC(pc: Mword; jumpPC: Mword; jump: std_logic) return Mword is
	variable res, pcBase: Mword := (others => '0'); 
begin
	pcBase := pc and i2slv(-PIPE_WIDTH*4, MWORD_SIZE); -- Clearing low bits
	if jump = '1' then
		res := jumpPC;
	else
		res := addMwordBasic(pcBase, PC_INC);
	end if;
	return res;
end function;

-- group:  revTag = causing.groupTag and i2slv(-PIPE_WIDTH, SMALL_NUMBER_SIZE), mask = all ones
-- sequential: revTag = causing.numberTag, mask = new group's fullMask		
function nextCtr(ctr: InsTag; rewind: std_logic; revTag: InsTag;
							 allow: std_logic; mask: std_logic_vector) 
return InsTag is
begin
	if rewind = '1' then
		return revTag;
	elsif allow = '1' then
		return i2slv(slv2u(ctr) + countOnes(mask), TAG_SIZE);
	else
		return ctr;
	end if;
end function;


function getLinkInfo(ins: InstructionState; state: Mword) return InstructionState is
	variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
begin
	res.ip := ins.target;
	res.result := state;
	return res;
end function;


function getLatePCData(commitCausing: InstructionState; int: std_logic; intType: std_logic_vector;
								currentState, linkExc, linkInt, stateExc, stateInt: Mword)
return InstructionState is
	variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;-- content;
	variable newPC: Mword := (others=>'0');
	constant MINUS_4: Mword := i2slv(-4, MWORD_SIZE);
begin	
		if int = '1' then
		  if intType = "01" then
		        res.ip := X"00000280";
		  else
		        res.ip := INT_BASE; -- TEMP!
		  end if;
			res.result := currentState or X"00000001";
			res.result := res.result and X"fdffffff"; -- Clear dbtrap
		elsif commitCausing.controlInfo.hasException = '1'
			or commitCausing.controlInfo.dbtrap = '1' then
			-- TODO, FIX: exceptionCode sliced - shift left by ALIGN_BITS? or leave just base address
			res.ip := EXC_BASE(MWORD_SIZE-1 downto commitCausing.controlInfo.exceptionCode'length)
									& commitCausing.controlInfo.exceptionCode(
													commitCausing.controlInfo.exceptionCode'length-1 downto ALIGN_BITS)
									& EXC_BASE(ALIGN_BITS-1 downto 0);	
			-- TODO: if exception, it overrides dbtrap, but if only dbtrap, a specific vector address
				res.result := currentState or X"00000100";
				res.result := res.result and X"fdffffff";	-- Clear dbtrap
		elsif commitCausing.controlInfo.specialAction = '1' then
					res.result := currentState;
				if commitCausing.operation.func = sysSync 
				    or commitCausing.operation.func = sysSend then
					res.ip := commitCausing.target;
				elsif commitCausing.operation.func = sysReplay then
					res.ip := --commitCausing.ip;
							addMwordFaster(commitCausing.target, MINUS_4); -- CAREFUL: wouldn't work if branch or short
				elsif commitCausing.operation.func = sysHalt then
					res.ip := commitCausing.target; -- ???
				elsif commitCausing.operation.func = sysRetI then
					res.result := stateInt;
					res.ip := linkInt;
				elsif commitCausing.operation.func = sysRetE then
					res.result := stateExc;
					res.ip := linkExc;
				elsif commitCausing.operation.func = sysError then
				    res.ip := EXC_BASE;
			    elsif commitCausing.operation.func = sysCall then
                    res.ip := CALL_BASE; -- TEMP			    
				end if;
		end if;		
	
	return res;
end function;


function newPCData( commitEvent: std_logic; commitCausing: InstructionState;
						  execEvent: std_logic; execCausing: InstructionState;	
						  frontEvent: std_logic; frontCausing: InstructionState;
						  pcNext: Mword)
return InstructionState is
	variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;--content;
	variable newPC: Mword := (others=>'0');
begin
	if commitEvent = '1' then -- when from exec or front
		res.ip := commitCausing.target;
	elsif execEvent = '1' then		
		res.ip := execCausing.target;
	elsif frontEvent = '1' then
		--	report "front event!";
		res.ip := frontCausing.target;	
	else	-- Go to the next line
		res.ip := pcNext;
	end if;	

	return res;
end function;

-- Unifies content of ROB slot with BQ, others queues etc. to restore full state needed at Commit
function recreateGroup(insVec: InstructionSlotArray; bqGroup: InstructionSlotArray;
							  prevTarget: Mword--; tempValue: Mword; useTemp: std_logic
							  ) return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	variable targets: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	variable confBr: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable ind: integer := 0;
	variable prevTrg: Mword := (others => '0');
begin
	res := insVec;
	
	prevTrg := prevTarget;
	
	for i in 0 to PIPE_WIDTH-1 loop
		targets(i) := prevTrg;--bqGroup.data(i).target; -- Default to some input, not zeros 
	end loop;
	
	-- Take branch targets to correct places
	for i in 0 to PIPE_WIDTH-1 loop
		if bqGroup(i).full = '1' then
			ind := slv2u(getTagLow(bqGroup(i).ins.tags.renameIndex));
			targets(ind) := bqGroup(i).ins.target;
			confBr(ind) := '1';
		end if;
	end loop;

	for i in 0 to PIPE_WIDTH-1 loop
		if confBr(i) = '1' then--insVec(i).ins.controlInfo.confirmedBranch = '1' then
			null;
		else
			targets(i) := addMwordBasic(prevTrg, getAddressIncrement(insVec(i).ins));
		end if;
		res(i).ins.ip := prevTrg; -- ??
		prevTrg := targets(i);
		res(i).ins.target := targets(i);
	end loop;
	
	return res;
end function;


	function isHalt(ins: InstructionState) return std_logic is
	begin
		if ins.operation.func = sysHalt then
			return '1';
		else
			return '0';
		end if;
	end function;

--function setInterrupt3(ins: InstructionState; intSignal, start: std_logic) return InstructionState is
--	variable res: InstructionState := ins;
--begin
--	res.controlInfo.hasInterrupt := intSignal;-- or start;
--	res.controlInfo.hasReset := intSignal and start;
--	-- CAREFUL: needed because updating link info must have either interrupt or exception
--	if res.controlInfo.hasInterrupt = '1' then
--		res.controlInfo.hasException := '0';
--	end if;
--	return res;
--end function;

function clearControlEvents(ins: InstructionState) return InstructionState is
	variable res: InstructionState := ins;
begin
	res.controlInfo.newEvent := '0';
	res.controlInfo.hasInterrupt := '0';
	res.controlInfo.hasException := '0';	
	res.controlInfo.specialAction := '0';
	return res;
end function;


	function getLastEffective(newContent: InstructionSlotArray) return InstructionState is
		variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
		-- Seeking from right side cause we need the last one 
		for i in newContent'range loop
			-- Count only full instructions
			if newContent(i).full = '1' then
				res := newContent(i).ins;
			else
				exit;
			end if;
			
			-- If this one has an event, following ones don't count
			if 	newContent(i).ins.controlInfo.hasException = '1'
				or newContent(i).ins.controlInfo.specialAction = '1'	-- CAREFUL! This also breaks flow!
				or newContent(i).ins.controlInfo.dbtrap = '1'
			then
				res.controlInfo.newEvent := '1'; -- Announce that event is to happen now!
				exit;
			end if;
			
		end loop;
		
		return res;
	end function;


function getNewEffective(sendingToCommit: std_logic; robDataLiving, dataFromBQV: InstructionSlotArray;
								 lastEffectiveIns, lateTargetIns: InstructionState;
								 --linkAddressInt, linkAddressExc: Mword;
								 --evtPhase1, intPhase1, start: std_logic)
								 evtPhase2: std_logic)
return InstructionSlot is
	variable res: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	variable sdToCommit: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	variable insToLastEffective: InstructionState;
	
	variable effectiveVec, takenBranchVec, bqTakenBranchVec, differenceVec:
					std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable branchTarget: Mword := lastEffectiveIns.target;
	variable ind: std_logic_vector(LOG2_PIPE_WIDTH-1 downto 0) := (others => '0');
	variable targetInc: Mword := (others => '0');
begin
	sdToCommit := recreateGroup(robDataLiving, dataFromBQV, lastEffectiveIns.target);
	insToLastEffective := getLastEffective(sdToCommit);
	
--	lateTargetIns := getLatePCData(setInterrupt3(lastEffectiveIns, intPhase1, start),
--							(others => '0'), linkAddressExc, linkAddressInt, (others => '0'), (others => '0'));
	
	if evtPhase2 = '1' then
		res := ('1', lateTargetIns);--clearControlEvents(setInstructionTarget(lastEffectiveIns, lateTargetIns.ip)));
	else
	--if true then
		res := (sendingToCommit, insToLastEffective);

		-- Find taken jumps in ROB entry and last effective index
		for i in robDataLiving'range loop		
			if robDataLiving(i).full = '1' then
				effectiveVec(i) := '1';
				if robDataLiving(i).ins.controlInfo.confirmedBranch = '1' then
					takenBranchVec(i) := '1'; -- Taken branches don't need to finish the group, unlike special events! 
				end if;
			else
				exit;
			end if;
			
			-- If this one has an event, following ones don't count
			if 	robDataLiving(i).ins.controlInfo.hasException = '1'
				or robDataLiving(i).ins.controlInfo.specialAction = '1'	-- CAREFUL! This also breaks flow!
				or robDataLiving(i).ins.controlInfo.dbtrap = '1'
			then
				--res.controlInfo.newEvent := '1'; -- Announce that event is to happen now!
				exit;
			end if;
			
		end loop;		
		
		-- Find taken jumps in BQ group and select last as target
		branchTarget := lastEffectiveIns.target;
							--	getStoredArg1(dataFromBQV.data(0));
		for i in 0 to PIPE_WIDTH-1 loop
			ind := dataFromBQV(i).ins.tags.renameIndex(LOG2_PIPE_WIDTH-1 downto 0);
			-- Corresponding ROB entry musu be effective effective, otherwise branch doesn't happen!
			-- But if not effective, BQ entry would've be killed, so no need to check
			if dataFromBQV(i).full = '1' then -- 
														--and effectiveVec(slv2u(ind)) = '1' then
				if dataFromBQV(i).ins.controlInfo.confirmedBranch = '1' then
					bqTakenBranchVec(slv2u(ind)) := '1';
					branchTarget := dataFromBQV(i).ins.target;
				end if;				
			else
				exit;
			end if;
		end loop;		
		
		-- Find number of effective instructions after the jump
		for i in PIPE_WIDTH-1 downto 0 loop
			if takenBranchVec(i) = '1' then
				exit;
			elsif effectiveVec(i) = '1' then
				differenceVec(i) := '1';
			end if;
		end loop;
		-- CAREFUL: works only for 32b instructions
		targetInc(LOG2_PIPE_WIDTH + 2 downto 2) := i2slv(countOnes(differenceVec), LOG2_PIPE_WIDTH+1);
		res.ins.target := addMwordFaster(branchTarget, targetInc);
	end if; 
	
	return res;
end function;



	function getEffectiveMask(newContent: InstructionSlotArray) return std_logic_vector is
		variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	begin
		-- Seeking from right side cause we need the last one 
		for i in newContent'range loop
			if newContent(i).full = '1' then -- Count only full instructions
				res(i) := '1';
			else
				exit;
			end if;
			
			-- CAREFUL, TODO: what if there's a branch (or branch correction) and valid path after it??
			-- If this one has an event, following ones don't count
			if 	newContent(i).ins.controlInfo.hasException = '1'
				or newContent(i).ins.controlInfo.specialAction = '1'
				or newContent(i).ins.controlInfo.dbtrap = '1'
			then
				exit;
			end if;
			
		end loop;
		return res;
	end function;

    function anyEvent(insVec: InstructionSlotArray) return std_logic is
        variable effectiveMask: std_logic_vector(0 to PIPE_WIDTH-1) := getEffectiveMask(insVec);
    begin
        for i in 0 to PIPE_WIDTH-1 loop
            if (effectiveMask(i) = '1') and
              (     insVec(i).ins.controlInfo.hasException = '1'
				or insVec(i).ins.controlInfo.specialAction = '1'
                or insVec(i).ins.controlInfo.dbtrap = '1') then
                return '1';
            end if;            
        end loop;
        return '0'; 
    end function;

end LogicSequence;
