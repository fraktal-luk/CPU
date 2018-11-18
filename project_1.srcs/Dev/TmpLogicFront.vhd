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

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;

use work.PipelineGeneral.all;

use work.DecodingDev.all;


package TmpLogicFront is

function getInstructionClassInfo(ins: InstructionState) return InstructionClassInfo;

function decodeInstruction(inputState: InstructionState) return InstructionState;

function decodeMulti(sd: InstructionSlotArray) return InstructionSlotArray;

function fillTargetsAndLinks(insVec: InstructionSlotArray) return InstructionSlotArray;

function newFromHbufferW(content: InstructionStateArray; fullMask: std_logic_vector)
return InstructionSlotArray;

--function getFetchOffsetW(ip: Mword) return SmallNumber;

function getAnnotatedWords(fetchIns: InstructionState; fetchInsMulti: InstructionSlotArray;
									 fetchBlock: HwordArray)
return InstructionStateArray;

function getFrontEventMulti(predictedAddress: Mword; ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray;


--function getEarlyBranchMultiDataIn(predictedAddress: Mword;
--							  ins: InstructionState; receiving: std_logic; valid: std_logic;
--							  hbuffAccepting: std_logic; fetchBlock: HwordArray(0 to FETCH_BLOCK_SIZE-1))
--return InstructionSlotArray;

function countFullNonSkipped(insVec: InstructionSlotArray) return integer;

function findEarlyTakenJump(ins: InstructionState; insVec: InstructionSlotArray) return InstructionState;

function getBranchMask(insVec: InstructionSlotArray) return std_logic_vector;

function prepareForBQ(insVec: InstructionSlotArray; branchMask: std_logic_vector) return InstructionSlotArray;

end TmpLogicFront;



package body TmpLogicFront is

function getInstructionClassInfo(ins: InstructionState) return InstructionClassInfo is
	variable ci: InstructionClassInfo := defaultClassInfo;
begin
				-- Which clusters?
				-- CAREFUL, TODO: make it more regular and clear!
				ci.mainCluster := '1';
				if ins.operation = (Memory, store) then
					ci.store := '1';
					ci.secCluster := '1';
				end if;
				
				if ins.operation = (Memory, load) or ins.operation = (System, sysMFC) then
					ci.load := '1';
				end if;
				
				if ins.operation.unit = Jump then
					ci.branchIns := '1';
				
					ci.secCluster := '1';
					-- TODO: remove this distinction because no longer used!
					-- For branch with link main cluster for destination write
					--if isNonzero(ins.virtualArgSpec.dest(4 downto 0)) = '0' then						
					--	ci.mainCluster := '0';
					--end if;
				elsif ins.operation = (System, sysMtc) then
					ci.store := '1';
					ci.secCluster := '1';
				elsif	(ins.operation.unit = System and ins.operation.func /= sysMfc) then
					ci.mainCluster := '0';
					ci.secCluster := '1';
				end if;

			if ins.operation.func = sysUndef then
				ci.mainCluster := '0';
				ci.secCluster := '0';
			end if;

			ci.branchCond := '0';
			if 	ins.operation.func = jump then
				null;
			elsif ins.operation.func = jumpZ or ins.operation.func = jumpNZ then 
				ci.branchCond := '1';	
			end if;
			
		if ins.operation.unit = ALU or ins.operation.unit = Jump then
			ci.pipeA := '1';
		end if;
		
		if ins.operation.unit = MAC then
			ci.pipeB := '1';
		end if;
		
		ci.pipeC := ci.load or ci.store;
		
	return ci;
end function;

function decodeInstruction(inputState: InstructionState) return InstructionState is
	variable res: InstructionState := inputState;
    variable decodedIns: InstructionState := DEFAULT_INSTRUCTION_STATE;
	variable tmpVirtualArgs: InstructionVirtualArgs;
	variable tmpVirtualDestArgs: InstructionVirtualDestArgs;
begin
	decodedIns := decodeFromWord(inputState.bits);
	
	res.operation := decodedIns.operation;
	res.constantArgs := decodedIns.constantArgs;
	res.virtualArgSpec := decodedIns.virtualArgSpec;
	
	res.classInfo := getInstructionClassInfo(res);	

				if res.operation.unit = System and
						(	res.operation.func = sysRetI or res.operation.func = sysRetE
						or res.operation.func = sysSync or res.operation.func = sysReplay
						or res.operation.func = sysError
						or res.operation.func = sysHalt) then 		
					res.controlInfo.specialAction := '1';
					
						-- CAREFUL: Those ops don't get issued, they are handled at retirement
						res.classInfo.mainCluster := '0';
						res.classInfo.secCluster := '0';
				end if;	
	
		if res.operation.func = sysUndef then
			res.controlInfo.hasException := '1';
			res.controlInfo.exceptionCode := i2slv(ExceptionType'pos(undefinedInstruction), SMALL_NUMBER_SIZE);
		end if;
		
		if res.controlInfo.squashed = '1' then	-- CAREFUL: ivalid was '0'
			report "Trying to decode invalid location" severity error;
		end if;
		
		res.controlInfo.squashed := '0';
	return res;
end function;

 
function decodeMulti(sd: InstructionSlotArray) return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to sd'length-1) := sd;
begin
	for i in 0 to PIPE_WIDTH-1 loop
		res(i).ins := decodeInstruction(sd(i).ins);		
	end loop;
	return res;
end function;

function fillTargetsAndLinks(insVec: InstructionSlotArray) return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to insVec'length-1) := insVec;
	variable target, link: Mword := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		target := addMwordFaster(insVec(i).ins.ip, insVec(i).ins.target);
		link := addMwordBasic(insVec(i).ins.ip, getAddressIncrement(insVec(i).ins));
		res(i).ins.target := target;
		res(i).ins.result := link;
	end loop;
	return res;
end function;


function newFromHbufferW(content: InstructionStateArray; fullMask: std_logic_vector)
return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to content'length-1) := (others => DEFAULT_INSTRUCTION_SLOT);
begin
	for i in 0 to PIPE_WIDTH-1 loop
		res(i).full := fullMask(i); --'1';
		res(i).ins.bits := content(i).bits;--(15 downto 0) & content(2*i+1).bits(15 downto 0);
		res(i).ins.ip := content(i).ip;
		res(i).ins.controlInfo.squashed := content(i).controlInfo.squashed;
		res(i).ins.controlInfo.hasBranch := content(i).controlInfo.hasBranch;			
	end loop;

	return res;
end function;

--        -- UNUSED?
--		function getFetchOffsetW(ip: Mword) return SmallNumber is
--			variable res: SmallNumber := (others => '0');
--		begin
--			res(ALIGN_BITS-1 downto 0) := ip(ALIGN_BITS-1 downto 0);
--			-- Shift down by 2
--			res(SMALL_NUMBER_SIZE-3 downto 0) := res(SMALL_NUMBER_SIZE-1 downto 2);
--			return res;
--		end function;

function getAnnotatedWords(fetchIns: InstructionState; fetchInsMulti: InstructionSlotArray;
									 fetchBlock: HwordArray)
return InstructionStateArray is
	variable res: InstructionStateArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_STATE);
	variable	tempWord: word := (others => '0');
	variable wordIP: Mword := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		wordIP := fetchIns.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(4*i, ALIGN_BITS);
		tempWord(31 downto 16) := fetchBlock(2*i);
		tempWord(15 downto 0) := fetchBlock(2*i+1);

		res(i).bits := tempWord;
		res(i).ip := wordIP;
		res(i).classInfo.short := '0'; -- TEMP!
		res(i).controlInfo.squashed := fetchIns.controlInfo.squashed; -- CAREFUL: guarding from wrong reading 
	end loop;

	for i in 0 to PIPE_WIDTH-1 loop
		res(i).controlInfo.hasBranch := fetchInsMulti(i).ins.controlInfo.hasBranch;
		res(i).target := fetchInsMulti(i).ins.target;
	end loop;
	
	return res;
end function;

function getFrontEventMulti(predictedAddress: Mword;
							  ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	variable tempOffset, thisIP, tempTarget: Mword := (others => '0');
	variable targets: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	variable fullOut, full, branchIns, predictedTaken: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable nSkippedIns: integer := 0;
	variable regularJump, longJump, regJump: std_logic := '0';
begin
	-- receiving, valid, accepting	-> good
	-- receiving, valid, not accepting -> refetch
	-- receiving, invalid, accepting -> error, will cause exception, but handled later, from decode on
	-- receiving, invalid, not accepting -> refetch??
	
	-- CAREFUL: Only without hword instructions now!
	-- Find which are before the start of fetch address
	nSkippedIns := slv2u(predictedAddress(ALIGN_BITS-1 downto 0))/4;								
			
	for i in 0 to FETCH_WIDTH-1 loop
		full(i) := '1'; -- For skipping we use 'skipped' flag, not clearing 'full' 
		if i < nSkippedIns then
			res(i).ins.controlInfo.skipped := '1';
			 full(i) := '0'; -- CAREFUL: trying to dispose of 'skipped' flag
		end if;
	end loop;

	if true then --(receiving and valid and hbuffAccepting) = '1' then
		-- Calculate target for each instruction, even if it's to be skipped
		for i in 0 to FETCH_WIDTH-1 loop
			thisIP := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(i*4, ALIGN_BITS);
			
			regularJump := '0';
			longJump := '0';
			regJump := '0';
			
			if 	fetchLine(i)(31 downto 26) = opcode2slv(jl) 
				or fetchLine(i)(31 downto 26) = opcode2slv(jz) 
				or fetchLine(i)(31 downto 26) = opcode2slv(jnz)
			then
				regularJump := '1';				
				predictedTaken(i) := fetchLine(i)(20);		-- CAREFUL, TODO: temporary predicted taken iff backwards
			elsif fetchLine(i)(31 downto 26) = opcode2slv(j) -- Long jump instruction
			then
				longJump := '1';				
				predictedTaken(i) := '1'; -- Long jump is unconditional (no space for register encoding!)
			elsif  fetchLine(i)(31 downto 26) = opcode2slv(ext1) 
				and (fetchLine(i)(15 downto 10) = opcont2slv(ext1, jzR)
						or fetchLine(i)(15 downto 10) = opcont2slv(ext1, jnzR)) then
				regJump := '1';
				predictedTaken(i) := '0'; -- TEMP: register jumps predicted not taken
			end if;
			
			branchIns(i) := regularJump or longJump or regJump;
			
			if longJump = '1' then
				tempOffset := (others => fetchLine(i)(25));
				tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
			else --elsif regularJump = '1' then
				tempOffset := (others => fetchLine(i)(20));
				tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);				
			end if;

			targets(i) := addMwordFaster(thisIP, tempOffset);

			res(i).ins.classInfo.branchCond := branchIns(i); -- Mark as ins of type branch
			
			-- Now applying the skip!
			if res(i).ins.controlInfo.skipped = '1' then
				branchIns(i) := '0';
			end if;			
		end loop;
		
		-- Find if any branch predicted
		for i in 0 to FETCH_WIDTH-1 loop
			fullOut(i) := full(i);
			--res.data(i).bits := fetchBlock(2*i) & fetchBlock(2*i+1);
			if full(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
				-- Here check if the next line from line predictor agress with the target predicted now.
				--	If so, don't cause the event but set invalidation mask that next line will use.
				if targets(i)(MWORD_SIZE-1 downto ALIGN_BITS) = ins.target(MWORD_SIZE-1 downto ALIGN_BITS) then					
					-- CAREFUL: Remeber that it actually is treated as a branch, otherwise would be done 
					--				again at Exec!
					res(i).ins.controlInfo.hasBranch := '1';
				else
					-- Raise event
					res(i).ins.controlInfo.newEvent := '1';
					res(i).ins.controlInfo.hasBranch := '1';
					--res.data(i).target := targets(i);
				end if;
				
				-- CAREFUL: When not using line predictor, branches predicted taken must always be done here 
				if not USE_LINE_PREDICTOR then
					res(i).ins.controlInfo.newEvent := '1';
					res(i).ins.controlInfo.hasBranch := '1';
					--res.data(i).target := targets(i);
				end if;

				exit;
			end if;
		end loop;
		
		for i in 0 to FETCH_WIDTH-1 loop
			res(i).ins.bits := fetchLine(i);
			res(i).ins.target := targets(i);
			
			res(i).ins.result := ins.ip;
			res(i).ins.result(ALIGN_BITS-1 downto 0) := i2slv((i+1)*4, ALIGN_BITS); -- CAREFUL: not for short ins
		end loop;
	end if;
	res(PIPE_WIDTH-1).ins.result := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(0, ALIGN_BITS);
	res(PIPE_WIDTH-1).ins.result := addMwordBasic(res(PIPE_WIDTH-1).ins.result, PC_INC);
	
	for i in 0 to FETCH_WIDTH-1 loop
	   res(i).full := fullOut(i);
	end loop;
	return res;
end function;


--function getEarlyBranchMultiDataIn(predictedAddress: Mword;
--							  ins: InstructionState; receiving: std_logic; valid: std_logic;
--							  hbuffAccepting: std_logic; fetchBlock: HwordArray(0 to FETCH_BLOCK_SIZE-1))
--return InstructionSlotArray is
--	variable res: InstructionSlotArray := DEFAULT_STAGE_DATA_MULTI;
--begin
--	res := getFrontEventMulti(predictedAddress, ins, receiving, valid, hbuffAccepting, fetchBlock);
--	return res;
--end function;

function countFullNonSkipped(insVec: InstructionSlotArray) return integer is 
	variable res: integer := 0;
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if insVec(i).full = '1' and insVec(i).ins.controlInfo.skipped = '0' then
			res := res + 1;
		end if;
	end loop;
	return res;
end function;

function findEarlyTakenJump(ins: InstructionState; insVec: InstructionSlotArray) return InstructionState is
	variable res: InstructionState := ins;
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 		insVec(i).full = '1' and insVec(i).ins.controlInfo.skipped = '0'
			and 	insVec(i).ins.controlInfo.newEvent = '1'
		then
			res.controlInfo.newEvent := '1';
			res.controlInfo.hasBranch := '1';
			res.target  := insVec(i).ins.target;
			exit;
		end if;
	end loop;
	
	return res;
end function;

function getBranchMask(insVec: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if 		insVec(i).full = '1' and insVec(i).ins.controlInfo.skipped = '0'
			and 	insVec(i).ins.classInfo.branchCond = '1'
		then
			res(i) := '1';
		end if;
	end loop;
	
	return res;
end function;

function prepareForBQ(insVec: InstructionSlotArray; branchMask: std_logic_vector) return InstructionSlotArray is
	variable res: InstructionSlotArray(insVec'range) := insVec;
	variable result, target: Mword;
begin
	for i in insVec'range loop
		target := insVec(i).ins.target;
		result := insVec(i).ins.result;
		res(i).ins := setStoredArg1(res(i).ins, target);
		res(i).ins := setStoredArg2(res(i).ins, result);
		res(i).full := branchMask(i) and insVec(i).full and not insVec(i).ins.controlInfo.skipped; 
	end loop;
	
	return res;
end function;

end TmpLogicFront;
