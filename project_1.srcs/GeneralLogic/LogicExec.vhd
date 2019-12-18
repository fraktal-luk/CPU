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

	function basicBranch(ins: InstructionState; st: SchedulerState; queueData: InstructionState)--; qs: std_logic)
	return InstructionState;

	function setExecState(ins: InstructionState;
								result: Mword; carry: std_logic; exc: std_logic_vector(3 downto 0))
	return InstructionState;

	--function isBranch(ins: InstructionState) return std_logic;

	function executeAlu(ins: InstructionState; st: SchedulerState; queueData: InstructionState; branchIns: InstructionState) return InstructionState;

	function executeFpu(ins: InstructionState; st: SchedulerState) return InstructionState;

    function calcEffectiveAddress(ins: InstructionState; st: SchedulerState;
                                            fromDLQ: std_logic; dlqData: InstructionState)
    return InstructionState;        
    
    function setAddressCompleted(ins: InstructionState; state: std_logic) return InstructionState;
    
    function setDataCompleted(ins: InstructionState; state: std_logic) return InstructionState;
    
    function getLSResultData(ins: InstructionState;
                                      tlbReady: std_logic; tlbValue: Mword;
                                      memLoadReady: std_logic; memLoadValue: Mword;
                                      sysLoadReady: std_logic; sysLoadValue: Mword;
                                      storeForwardSending: std_logic; storeForwardIns: InstructionState;
                                      lqSelectedOutput: InstructionSlot
                                        ) return InstructionState;	
end LogicExec;



package body LogicExec is

	function passArg0(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		return res;
	end function;

	function passArg1(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		return res;
	end function;

	function execLogicOr(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		return res;
	end function;	

	function execLogicXor(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		return res;
	end function;	


	function raiseExecException(ins: InstructionState) return InstructionState is
		variable res: InstructionState := ins;
	begin
		res.controlInfo.newEvent := '1';
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
		elsif op.func = jumpZ and isZero = '1' then
		    return '1';
		else
			return '0';
		end if;	
		
	end function;


	function basicBranch(ins: InstructionState; st: SchedulerState; queueData: InstructionState --; qs: std_logic
									) return InstructionState is
		variable res: InstructionState := ins;
		variable branchTaken, targetMatch: std_logic := '0';
		variable storedTarget, storedReturn, trueTarget: Mword := (others => '0');
		variable targetEqual: std_logic := '0';
	begin			
		-- Cases to handle
		-- jr taken		: if not taken goto return, if taken and not equal goto reg, if taken and equal ok 
		-- jr not taken: if not taken ok, if taken goto reg
		-- j taken		: if not taken goto return, if taken equal
		-- j not taken : if not taken ok, if taken goto dest

        targetMatch := bool2std(queueData.target = st.argValues.arg1);
		branchTaken := resolveBranchCondition(st.argValues, ins.operation);

		if res.controlInfo.frontBranch = '1' and branchTaken = '0' then						
			res.controlInfo.newEvent := '1';
			trueTarget := queueData.result;
		elsif res.controlInfo.frontBranch = '0' and branchTaken = '1' then					
			res.controlInfo.newEvent := '1';
			res.controlInfo.confirmedBranch := '1';			
			if ins.constantArgs.immSel = '0' then -- if branch reg			
				trueTarget := st.argValues.arg1;
			else
				trueTarget := queueData.target;
			end if;
		elsif res.controlInfo.frontBranch = '0' and branchTaken = '0' then
			trueTarget := queueData.result;
		else -- taken -> taken
			if ins.constantArgs.immSel = '0' then -- if branch reg
				if targetMatch = '0' then
					res.controlInfo.newEvent := '1';	-- Need to correct the target!	
				end if;
				trueTarget := st.argValues.arg1; -- reg destination
			else
				trueTarget := queueData.target;			
			end if;
			res.controlInfo.confirmedBranch := '1';			
		end if;

		res.target := trueTarget;
		-- Return address
		res.result := queueData.result;
			res.tags.intPointer := queueData.tags.intPointer;
			res.tags.floatPointer := queueData.tags.floatPointer;
							
		return res;
	end function;

    -- UNUSED
	function setExecState(ins: InstructionState;
								result: Mword; carry: std_logic; exc: std_logic_vector(3 downto 0))
	return InstructionState is
		variable res: InstructionState := ins;
	begin
		res.result := result;
		res.controlInfo.newEvent := isNonzero(exc);
		res.controlInfo.hasException := res.controlInfo.newEvent;
		return res;
	end function;
	
	function executeAlu(ins: InstructionState; st: SchedulerState; queueData: InstructionState; branchIns: InstructionState)
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
		linkAdr := queueData.result;

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

			res.controlInfo.newEvent := '0';
			res.controlInfo.hasException := '0';
			--res.controlInfo.exceptionCode := (others => '0'); -- ???	

		if ins.operation.func = arithAdd or ins.operation.func = arithSub then
			carry := resultExt(MWORD_SIZE); -- CAREFUL, with subtraction carry is different, keep in mind
			result := resultExt(MWORD_SIZE-1 downto 0);					
		else
		
			case ins.operation.func is
				when logicAnd =>
					result := arg0 and arg1;				
				when logicOr =>
					result := arg0 or arg1;
				when jump | jumpZ | jumpNZ => 
					result := linkAdr;
					
					res.controlInfo.newEvent := branchIns.controlInfo.newEvent;
                    res.controlInfo.frontBranch := branchIns.controlInfo.frontBranch;
                    res.controlInfo.confirmedBranch := branchIns.controlInfo.confirmedBranch;

				when others => 
					result := shiftedBytes(31 + shL downto shL);
			end case;
		end if;

		if ov = '1' then
			res.controlInfo.newEvent := '1';
			res.controlInfo.hasException := '1';
			--res.controlInfo.exceptionCode := (0 => '1', others => '0'); -- ???
		end if;      
		
		res.result := result;
		
		return res;
	end function;


	
	function executeFpu(ins: InstructionState; st: SchedulerState) return InstructionState is
       variable res: InstructionState := ins;
	begin
        if ins.operation.func = fpuOr then
           res.result := st.argValues.arg0 or st.argValues.arg1;
        elsif ins.operation.func = fpuMov then
           res.result := st.argValues.arg0;
        else
           
		end if;
		return res;
	end function;

            
    function calcEffectiveAddress(ins: InstructionState; st: SchedulerState;
                                            fromDLQ: std_logic; dlqData: InstructionState)
    return InstructionState is
        variable res: InstructionState := ins;
    begin
        if fromDLQ = '1' then
            return dlqData;
        else
            res.result := addMwordFaster(st.argValues.arg0, st.argValues.arg1);
            return res;
        end if;
    end function;
    
    
    function setAddressCompleted(ins: InstructionState; state: std_logic) return InstructionState is
        variable res: InstructionState := ins;
    begin
        res.controlInfo.completed := state;
        return res;
    end function;
    
    function setDataCompleted(ins: InstructionState; state: std_logic) return InstructionState is
        variable res: InstructionState := ins;
    begin
        res.controlInfo.completed2 := state;
        return res;
    end function;
    
    function getLSResultData(ins: InstructionState;
                                      tlbReady: std_logic; tlbValue: Mword;	    
                                      memLoadReady: std_logic; memLoadValue: Mword;
                                      sysLoadReady: std_logic; sysLoadValue: Mword;
                                      storeForwardSending: std_logic; storeForwardIns: InstructionState;
                                      lqSelectedOutput: InstructionSlot                                         
                                        ) return InstructionState is
        variable res: InstructionState := ins;
    begin
        -- mfc/mtc?
        -- tlb/access error?
        -- tlb miss?
        -- data miss?            
        -- SQ forward miss?
        -- SQ forward hit?            
        -- else

        -- So far TLB and tag misses are not implemented
         if ins.operation = (System, sysMfc) or ins.operation = (System, sysMtc) then
             res.result := sysLoadValue;
         elsif false then
            -- TLB problems...
         elsif memLoadReady = '0' then
             res.controlInfo.dataMiss := '1';
         elsif storeForwardSending = '1' then
             res.result := storeForwardIns.result;
             if storeForwardIns.controlInfo.completed2 = '0' then
                 res.controlInfo.sqMiss := '1';
                      res.controlInfo.specialAction := '1';
             end if;
         else
            res.result := memLoadValue;
         end if;
       
         -- CAREFUL: store when newer load has been done - violation resolution when reissue is used
         if ins.operation = (Memory, store) and lqSelectedOutput.full = '1' then
            res.controlInfo.orderViolation := '1';
                res.controlInfo.specialAction := '1';
         end if;

        -- TODO: remember about miss/hit status and reason of miss if relevant!
        res := setAddressCompleted(res, '1'); -- TEMP
        return res;
    end function;
end LogicExec;
