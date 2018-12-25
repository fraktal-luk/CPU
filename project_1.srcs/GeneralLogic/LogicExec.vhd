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
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.Arith.all;


package LogicExec is

	-- DUMMY: This performs some simple operation to obtain a result
	function passArg0(ins: InstructionState) return InstructionState;
	function passArg1(ins: InstructionState) return InstructionState;
	function execLogicOr(ins: InstructionState) return InstructionState;
	function execLogicXor(ins: InstructionState) return InstructionState;

	-- set exception flags
	function raiseExecException(ins: InstructionState) return InstructionState;
	
	function resolveBranchCondition(av: InstructionArgValues; op: BinomialOp) return std_logic;

	function basicBranch(ins: InstructionState; st: SchedulerState; queueData: InstructionState; qs: std_logic)
	return InstructionState;

	function setExecState(ins: InstructionState;
								result: Mword; carry: std_logic; exc: std_logic_vector(3 downto 0))
	return InstructionState;

	function isBranch(ins: InstructionState) return std_logic;

	function executeAlu(ins: InstructionState; st: SchedulerState; queueData: InstructionState) return InstructionState;
	
end LogicExec;



package body LogicExec is

	function passArg0(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		--res.result := res.argValues.arg0;
		return res;
	end function;

	function passArg1(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		--res.result := res.argValues.arg1;
		return res;
	end function;

	function execLogicOr(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		--res.result := res.argValues.arg0 or res.argValues.arg1;
		return res;
	end function;	

	function execLogicXor(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		--res.result := res.argValues.arg0 xor res.argValues.arg1;
		return res;
	end function;	


	function raiseExecException(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		res.controlInfo.newEvent := '1';	
		--res.controlInfo.hasEvent := '1';	
		--res.controlInfo.newException := '1';
		res.controlInfo.hasException := '1';			
		return res;	
	end function;

	function resolveBranchCondition(av: InstructionArgValues; op: BinomialOp) return std_logic is
		variable isZero: std_logic;
	begin
		isZero := not isNonzero(av.arg0);
			
		if op.func = jump then
			return '1';
		elsif op.func = jump and isZero = '1' then
			return '1';
		elsif op.func = jumpNZ and isZero = '0' then
			return '1';
		else
			return '0';
		end if;	
		
	end function;


	function basicBranch(ins: InstructionState; st: SchedulerState; queueData: InstructionState; qs: std_logic
									) return InstructionState is
		variable res: InstructionState := ins;
		variable branchTaken: std_logic := '0';
		variable storedTarget, storedReturn, trueTarget: Mword := (others => '0');
		variable targetEqual: std_logic := '0';
	begin		
		res.operation := (General, Unknown);
	
		-- TODO: cases to handle
		-- jr taken		: if not taken goto return, if taken and not equal goto reg, if taken and equal ok 
		-- jr not taken: if not taken ok, if taken goto reg
		-- j taken		: if not taken goto return, if taken equal
		-- j not taken : if not taken ok, if taken goto dest
		
		-- Can keep dest and returnAdr from BQ in (target, result)?
		-- 	Then return := result, dest := target
		-- storedTarget := res.target; 
		-- storedReturn := res.result;
		-- targetEqual := [if storedTarget = reg then '1' else '0'];

		branchTaken := resolveBranchCondition(st.argValues, ins.operation);

		if res.controlInfo.hasBranch = '1' and branchTaken = '0' then
			res.controlInfo.hasBranch := '0';
			--res.controlInfo.newReturn := '1';
			res.controlInfo.hasReturn := '1';						
			res.controlInfo.newEvent := '1';
			--res.controlInfo.hasEvent := '1';
				trueTarget := getStoredArg2(queueData);
		elsif res.controlInfo.hasBranch = '0' and branchTaken = '1' then				
			res.controlInfo.hasReturn := '0';
			--res.controlInfo.newBranch := '1';
			res.controlInfo.hasBranch := '1';						
			res.controlInfo.newEvent := '1';
			--res.controlInfo.hasEvent := '1';
			if ins.constantArgs.immSel = '0' then -- if branch reg			
				trueTarget := st.argValues.arg1;
			else
				trueTarget := getStoredArg1(queueData);
			end if;
		elsif res.controlInfo.hasBranch = '0' and branchTaken = '0' then
			
			trueTarget := getStoredArg2(queueData);
		else -- taken -> taken
			if ins.constantArgs.immSel = '0' then -- if branch reg
				if getStoredArg1(queueData) /= st.argValues.arg1 then
					res.controlInfo.newEvent := '1';	-- Need to correct the target!				
				end if;
				trueTarget := st.argValues.arg1; -- reg destination
			else
				trueTarget := getStoredArg1(queueData);				
			end if;
		end if;

		res.target := --ins.argValues.arg1;
							trueTarget;
		-- Return address
		res.result := --linkAddress;
							getStoredArg2(queueData);
							
		return res;
	end function;


	function setExecState(ins: InstructionState;
								result: Mword; carry: std_logic; exc: std_logic_vector(3 downto 0))
	return InstructionState is
		variable res: InstructionState := ins;
	begin
		res.result := result;
		res.controlInfo.newEvent := isNonzero(exc);
		--res.controlInfo.hasEvent := res.controlInfo.newEvent;
		--res.controlInfo.newException := res.controlInfo.newEvent;
		res.controlInfo.hasException := res.controlInfo.newEvent;						
		res.controlInfo.exceptionCode := (others => '0');
		res.controlInfo.exceptionCode(3 downto 0) := exc;
		return res;
	end function;
	
	function isBranch(ins: InstructionState) return std_logic is
	begin
		if ins.operation = (Jump, jump) or ins.operation = (Jump, jumpZ) or ins.operation = (Jump, jumpNZ) then
			return '1';
		else
			return '0';
		end if;
	end function;
	
	
	function executeAlu(ins: InstructionState; st: SchedulerState; queueData: InstructionState)
	return InstructionState is
		variable res: InstructionState := ins;
		variable result, linkAdr: Mword := (others => '0');
		variable arg0, arg1, arg2: Mword := (others => '0');
			variable argAddSub: Mword := (others => '0');
			variable carryIn: std_logic := '0';
		variable c0, c1: slv5 := (others => '0');
		variable resultExt: std_logic_vector(MWORD_SIZE downto 0) := (others => '0');
		variable ov, carry: std_logic := '0';
		variable shH, shL: integer := 0;
		variable shNum, shTemp: SmallNumber := (others => '0');
			variable tempBits: std_logic_vector(95 downto 0) := (others => '0'); -- TEMP! for 32b only
			variable shiftedBytes: std_logic_vector(39 downto 0) := (others => '0');
	begin
		arg0 := st.argValues.arg0;
		arg1 := st.argValues.arg1;
		arg2 := st.argValues.arg2;
	
		--c0 := ins.constantArgs.c0;
		--c1 := ins.constantArgs.c1;	
	
	
		if ins.operation.func = arithSub then
			argAddSub := not arg1;
			carryIn := '1';
		else
			argAddSub := arg1;
			carryIn := '0';
		end if;
	
	
		--shTemp(4 downto 0) := c0; -- CAREFUL, TODO: handle the issue of 1-32 vs 0-31	
		shTemp(5 downto 0) := arg1(5 downto 0);
		if ins.operation.func = logicShl then
			shNum := subSN(shNum, shTemp);
		else
			shNum := shTemp;
		end if;
	
		shH := slv2s(shNum(5 downto 3));
				--0;
		shL := slv2u(shNum(2 downto 0));
	
		if ins.operation.func = arithSha then
			tempBits(95 downto 64) := (others => arg0(MWORD_SIZE-1));	
		end if;
		tempBits(63 downto 32) := arg0;
	
		shiftedBytes := tempBits(71 + 8*shH downto 32 + 8*shH);	
	
		-- Shifting: divide into byte part and intra-byte part
		--	shift left by 8*H + L
		-- must be universal: the H part also negative
		-- shift right by 3: 8*(-1) + 5
		--	Let's treat the number as 64 bit: [arg0 & 0x00000000] and mux relative to right bound.
		-- sh right 20 -> move window left by 2*8 + 4
		-- sh right 31 -> move window left by 3*8 + 7
		-- sh left   2 -> move window right by -1*8 + 6
		-- sh left  15 -> move window right by -2*8 + 1
		
		-- So, for shift left, number is negative, for right is positive
		-- Most negative byte count is -4, giving -4*8 + 0 = -32
		-- Most positive byte count is 3, giving 3*8 + 7 = 31
		
		resultExt := addMwordFasterExt(arg0, argAddSub, carryIn);	
		linkAdr := getStoredArg2(queueData);

--		if ins.operation.func = jump then
--			result := linkAdr;
--		else

		if (	(ins.operation.func = arithAdd 
			and arg0(MWORD_SIZE-1) = arg1(MWORD_SIZE-1)
			and arg0(MWORD_SIZE-1) /= resultExt(MWORD_SIZE-1)))
			or
			(	(ins.operation.func = arithSub 
			and arg0(MWORD_SIZE-1) /= arg1(MWORD_SIZE-1)
			and arg0(MWORD_SIZE-1) /= resultExt(MWORD_SIZE-1)))
		then 
			if false then --ENABLE_INT_OVERFLOW then
				ov := '1';
			end if;
		end if;

		if ins.operation.func = arithAdd or ins.operation.func = arithSub then
			carry := resultExt(MWORD_SIZE); -- CAREFUL, with subtraction carry is different, keep in mind
			result := resultExt(MWORD_SIZE-1 downto 0);					
		else
		
			case ins.operation.func is
				when logicAnd =>
					result := arg0 and arg1;				
				when logicOr =>
					result := arg0 or arg1;
				when jump => 
					result := linkAdr;
				when others => 
					result := shiftedBytes(31 + shL downto shL);
			end case;
		end if;
		
			res.controlInfo.newEvent := '0';
			res.controlInfo.hasException := '0';
			res.controlInfo.exceptionCode := (others => '0'); -- ???		
		
		if ov = '1' then
			res.controlInfo.newEvent := '1';
			res.controlInfo.hasException := '1';
			res.controlInfo.exceptionCode := (0 => '1', others => '0'); -- ???
		end if;
		--	res.controlInfo.exceptionCode := (0 => ov, others => '0'); -- ???
		
		res.result := result;
		
		return res;
	end function;

end LogicExec;
