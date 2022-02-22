--

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

    type AluControl is record
        adder: std_logic;
        sub: std_logic;
        bitop: std_logic_vector(3 downto 0);
        shifter: std_logic;
        shiftType: std_logic_vector(1 downto 0);
        jump: std_logic;
        jumpType: std_logic_vector(1 downto 0); -- [ab] -> a=0 conditional, a=1 unconditional, b=0 when zero, b=1 when nonzero
    end record;

    constant DEFAULT_ALU_CONTROL: AluControl := ('0', '0', "0000", '0', "00", '0', "00");

    function getAluControl(op: ArithOp) return AluControl;

	-- DUMMY: This performs some simple operation to obtain a result
	function passArg0(ins: InstructionState) return InstructionState;
	function passArg1(ins: InstructionState) return InstructionState;
	function execLogicOr(ins: InstructionState) return InstructionState;
	function execLogicXor(ins: InstructionState) return InstructionState;

	function basicBranch(sending: std_logic; ins: InstructionState; st: SchedulerState; queueData: InstructionState; ac: AluControl) return InstructionState;

	function executeAlu(ins: InstructionState; st: SchedulerState; queueData: InstructionState; branchIns: InstructionState; ac: AluControl) return InstructionState;

	function executeFpu(ins: InstructionState; st: SchedulerState) return InstructionState;

    function calcEffectiveAddress(ins: InstructionState; st: SchedulerState; fromDLQ: std_logic; dlqData: InstructionState)
    return InstructionState;        

    function getLSResultData(ins: InstructionState;
                              tlbReady: std_logic; tlbValue: Mword;
                              memLoadReady: std_logic; memLoadValue: Mword;
                              sysLoadReady: std_logic; sysLoadValue: Mword;
                              --storeForwardSending: std_logic; storeForwardIns: InstructionState;
                              storeForwardOutput: InstructionSlot;
                              lqSelectedOutput: InstructionSlot
                             ) return InstructionState;
                                        
    function getStoreDataOp(ss: SchedulerEntrySlot) return SchedulerEntrySlot;
                                               
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


	function resolveBranchCondition(ss: SchedulerState; op: ArithOp; ac: AluControl) return std_logic is
		variable isZero: std_logic;
	begin
		isZero := not isNonzero(ss.args(0));
			
		    return ac.jumpType(1) or (ac.jumpType(0) xor isZero); 
			
		if op = opJ then
			return '1';
		elsif op = opJnz and isZero = '0' then
			return '1';
		elsif op = opJz and isZero = '1' then
		    return '1';
		else
			return '0';
		end if;	
		
	end function;

	function basicBranch(sending: std_logic; ins: InstructionState; st: SchedulerState; queueData: InstructionState; ac: AluControl) return InstructionState is
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

        targetMatch := bool2std(queueData.target = st.args(1));
		branchTaken := resolveBranchCondition(st, --ins.specificOperation.arith, ac);
		                                          st.operation.arith, ac);

		if queueData.controlInfo.frontBranch = '1' and branchTaken = '0' then						
			res.controlInfo.newEvent := '1';
			trueTarget := queueData.result;
		elsif queueData.controlInfo.frontBranch = '0' and branchTaken = '1' then					
			res.controlInfo.newEvent := '1';
			res.controlInfo.confirmedBranch := '1';			
			if st.immediate = '0' then
				trueTarget := st.args(1);
			else
				trueTarget := queueData.target;
			end if;
		elsif queueData.controlInfo.frontBranch = '0' and branchTaken = '0' then
			trueTarget := queueData.result;
		else -- taken -> taken
			if st.immediate = '0' then
				if targetMatch = '0' then
					res.controlInfo.newEvent := '1';	-- Need to correct the target!	
				end if;
				trueTarget := st.args(1); -- reg destination
			else
				trueTarget := queueData.target;
			end if;
			res.controlInfo.confirmedBranch := '1';			
		end if;


                if branchTaken = '0' then
                    trueTarget := queueData.result;
                elsif st.immediate = '1' then
                    trueTarget := queueData.target;
                else
                    trueTarget := st.args(1);
                end if;
                
                if      (queueData.controlInfo.frontBranch xor branchTaken) = '1'
                    or  (queueData.controlInfo.frontBranch and branchTaken and not st.immediate and not targetMatch) = '1'
                then
                    res.controlInfo.newEvent := --'1';
                                                sending;
                else
                    res.controlInfo.newEvent := '0';
                end if;

                res.controlInfo.confirmedBranch := branchTaken;

		res.target := trueTarget;
		-- Return address
		res.result := queueData.result;
		res.tags.intPointer := queueData.tags.intPointer;
		res.tags.floatPointer := queueData.tags.floatPointer;
    	res.tags.sqPointer := queueData.tags.sqPointer;
	    res.tags.lqPointer := queueData.tags.lqPointer;
		
		if CLEAR_DEBUG_INFO then
		    res := clearDbCounters(res);
		    res := clearRawInfo(res);
		    res.constantArgs := DEFAULT_CONSTANT_ARGS;
		    res.classInfo := DEFAULT_CLASS_INFO;
		    
		    res.specificOperation := DEFAULT_SPECIFIC_OP;
		      
		    res.virtualArgSpec.intArgSel := (others => '0');
		    res.virtualArgSpec.floatArgSel := (others => '0');
		    res.virtualArgSpec.args := (others => (others => '0'));
		    
		    res.physicalArgSpec.intArgSel := (others => '0');
		    res.physicalArgSpec.floatArgSel := (others => '0');
		    res.physicalArgSpec.args := (others => (others => '0'));
		    
		    res.result := (others => '0');
		end if;		
		return res;
	end function;
	
	function executeAlu(ins: InstructionState; st: SchedulerState; queueData: InstructionState; branchIns: InstructionState; ac: AluControl)
	return InstructionState is
		variable res: InstructionState := ins;
		variable result, linkAdr: Mword := (others => '0');
		variable arg0, arg1, arg2: Mword := (others => '0');
		variable argAddSub: Mword := (others => '0');
		variable carryIn: std_logic := '0';
		variable resultExt: std_logic_vector(MWORD_SIZE downto 0) := (others => '0');
		variable resultExt0, resultExt1: Word := (others => '0');
		variable ov, carry, cl, cm0, cm1: std_logic := '0';
		variable shH, shL: integer := 0;
		variable shNum, shTemp: SmallNumber := (others => '0');
		variable tempBits: std_logic_vector(95 downto 0) := (others => '0'); -- TEMP! for 32b only
	    variable shiftedBytes: std_logic_vector(39 downto 0) := (others => '0');
	    variable shiftInput, rotated, shiftOutput: Dword := (others => '0');
	begin
		arg0 := st.args(0);
		arg1 := st.args(1);
		arg2 := st.args(2);

		if --ins.specificOperation.arith = opSub then
		   ac.sub = '1' then
			argAddSub := not arg1;
			carryIn := '1';
		else
			argAddSub := arg1;
			carryIn := '0';
		end if;
	
		shTemp(5 downto 0) := arg1(5 downto 0);
		if --ins.specificOperation.arith = opShl then
		      ac.shiftType = "00" then
			shNum := subSN(shNum, shTemp);
		else
			shNum := shTemp;
		end if;
	
		shH := slv2s(shNum(5 downto 3));
		shL := slv2u(shNum(2 downto 0));
	
		if --ins.specificOperation.arith = opSha then
		      ac.shiftType = "01" then
			tempBits(95 downto 64) := (others => arg0(MWORD_SIZE-1));	
		end if;
		tempBits(63 downto 32) := arg0;
	
		shiftedBytes := tempBits(71 + 8*shH downto 32 + 8*shH);	
	
	       shiftInput(31 downto 0) := arg0;
	       rotated := rotate64(shiftInput, arg1(5 downto 0));
	       shiftOutput := rotated; -- TEMP: works for 32b and 5-bit shift amount
--	       if arg1(9) = '0' then
--	           shiftOutput := clearRight64(rotated, arg1(5 downto 0));
--	       else
--	           shiftOutput := rotated; -- TEMP!
--	       end if;
	       
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
		
		resultExt(31 downto 0) := addExtNew(arg0, argAddSub, carryIn);	
		linkAdr := queueData.result;

--		if (	(ins.specificOperation.arith = opAdd
--			and arg0(MWORD_SIZE-1) = arg1(MWORD_SIZE-1)
--			and arg0(MWORD_SIZE-1) /= resultExt(MWORD_SIZE-1)))
--			or
--			(	(ins.specificOperation.arith = opSub 
--			and arg0(MWORD_SIZE-1) /= arg1(MWORD_SIZE-1)
--			and arg0(MWORD_SIZE-1) /= resultExt(MWORD_SIZE-1)))
--		then 
--			if false then --ENABLE_INT_OVERFLOW then
--				ov := '1';
--			end if;
--		end if;
	    
	    res.controlInfo.newEvent := '0';
	    res.controlInfo.hasException := '0';
			
			
			addExtNewP(arg0, argAddSub, carryIn, resultExt0, resultExt1, cl, cm0, cm1);

        if (ac.adder and ((cl and cm1) or cm0)) = '1' then
            result(31 downto 20) := resultExt1(31 downto 20);
            result(19 downto 0) := resultExt0(19 downto 0);
        elsif ac.adder = '1' then
            result := resultExt0;      

--		if --ins.specificOperation.arith = opAdd or ins.specificOperation.arith = opSub then
--		      ac.adder = '1' then
--			carry := resultExt(MWORD_SIZE); -- CAREFUL, with subtraction carry is different, keep in mind
--			result := resultExt(MWORD_SIZE-1 downto 0);					
		else
		      if ac.jump = '1' then
					result := linkAdr;
              
                  res.controlInfo.newEvent := branchIns.controlInfo.newEvent;
                  res.controlInfo.frontBranch := branchIns.controlInfo.frontBranch;
                  res.controlInfo.confirmedBranch := branchIns.controlInfo.confirmedBranch;		      
		      elsif ac.shifter = '1' then
		          result := --shiftedBytes(31 + shL downto shL);
		                      shiftOutput(31 downto 0);
		      else
		          case ac.bitop is
		              when "0000" =>
					      result := arg0 and arg1;		                  
		              when "0001" =>
		                  result := arg0 or arg1;
		              when others =>
		                  result := arg0 xor arg1;
		          end case;
		      end if; 
		
--			case ins.specificOperation.arith is
--				when opAnd =>
--					result := arg0 and arg1;				
--				when opOr =>
--					result := arg0 or arg1;
--				when opJ | opJz | opJnz => 
--					result := linkAdr;
					
--					res.controlInfo.newEvent := branchIns.controlInfo.newEvent;
--                    res.controlInfo.frontBranch := branchIns.controlInfo.frontBranch;
--                    res.controlInfo.confirmedBranch := branchIns.controlInfo.confirmedBranch;

--				when others => 
--					result := shiftedBytes(31 + shL downto shL);
--			end case;
		end if;

		if ov = '1' then
			res.controlInfo.newEvent := '1';
			res.controlInfo.hasException := '1';
		end if;
		res.result := result;

		if CLEAR_DEBUG_INFO then
		    res := clearDbCounters(res);
		    res := clearRawInfo(res);
		    res.constantArgs := DEFAULT_CONSTANT_ARGS;
		    res.classInfo := DEFAULT_CLASS_INFO;

            res.specificOperation := DEFAULT_SPECIFIC_OP;

		    res.virtualArgSpec.intArgSel := (others => '0');
		    res.virtualArgSpec.floatArgSel := (others => '0');
		    res.virtualArgSpec.args := (others => (others => '0'));
		    
		    res.physicalArgSpec.intArgSel := (others => '0');
		    res.physicalArgSpec.floatArgSel := (others => '0');
		    res.physicalArgSpec.args := (others => (others => '0'));
		    
		    res.target := (others => '0');
		end if;
		
		return res;
	end function;


        function getAluControl(op: ArithOp) return AluControl is
            variable ac: AluControl := DEFAULT_ALU_CONTROL; 
        begin
            case op is
                when opAdd =>
                    ac.adder := '1';
                when opSub =>
                    ac.adder := '1';
                    ac.sub := '1';
                when opAnd =>
                    
                when opOr => 
                    ac.bitop := "0001";
                when opXor =>
                    ac.bitop := "0010";
                when opShl =>
                    ac.shifter := '1';
                when opSha =>
                    ac.shifter := '1';
                    ac.shiftType := "01";
                when opRot => 
                    ac.shifter := '1';
                    ac.shiftType := "10";
                when opJz =>
                    ac.jump := '1';
                    ac.jumpType := "00";
                when opJnz =>
                    ac.jump := '1';
                    ac.jumpType := "01";
                when opJ | opJl =>
                    ac.jump := '1';
                    ac.jumpType := "10";
                -- opMul, opMulshs, opMulhu, opDiv
                when others =>
                    
            end case;
            
            return ac;
        end function;

	
	function executeFpu(ins: InstructionState; st: SchedulerState) return InstructionState is
       variable res: InstructionState := ins;
	begin
        if ins.specificOperation.float = opOr then
           res.result := st.args(0) or st.args(1);
        elsif ins.specificOperation.float = opMove then
           res.result := st.args(0);
        else
           
		end if;

		if CLEAR_DEBUG_INFO then
		    res := clearDbCounters(res);
		    res := clearRawInfo(res);
		    res.constantArgs := DEFAULT_CONSTANT_ARGS;
		    res.classInfo := DEFAULT_CLASS_INFO;
		    	    
		    res.virtualArgSpec.intArgSel := (others => '0');
		    res.virtualArgSpec.floatArgSel := (others => '0');
		    res.virtualArgSpec.args := (others => (others => '0'));
		    
		    res.physicalArgSpec.intArgSel := (others => '0');
		    res.physicalArgSpec.floatArgSel := (others => '0');
		    res.physicalArgSpec.args := (others => (others => '0'));
		    
		    res.target := (others => '0');
		end if;

		return res;
	end function;

            
    function calcEffectiveAddress(ins: InstructionState; st: SchedulerState; fromDLQ: std_logic; dlqData: InstructionState)
    return InstructionState is
        variable res: InstructionState := ins;
    begin
        if fromDLQ = '1' then
            res := dlqData;
        else
            res.result := add(st.args(0), st.args(1));
        end if;

		if CLEAR_DEBUG_INFO then
		    res := clearDbCounters(res);
		    res := clearRawInfo(res);
		    res.constantArgs := DEFAULT_CONSTANT_ARGS;
		    res.classInfo := DEFAULT_CLASS_INFO;
		    	    
		    res.virtualArgSpec.intArgSel := (others => '0');
		    res.virtualArgSpec.floatArgSel := (others => '0');
		    res.virtualArgSpec.args := (others => (others => '0'));
		    
		    res.physicalArgSpec.intArgSel := (others => '0');
		    res.physicalArgSpec.floatArgSel := (others => '0');
		    res.physicalArgSpec.args := (others => (others => '0'));
		    
		    res.target := (others => '0');
		end if;
        
        return res;
    end function;

    
    function getLSResultData(ins: InstructionState;
                              tlbReady: std_logic; tlbValue: Mword;	    
                              memLoadReady: std_logic; memLoadValue: Mword;
                              sysLoadReady: std_logic; sysLoadValue: Mword;
                              --storeForwardSending: std_logic; storeForwardIns: InstructionState;
                              storeForwardOutput: InstructionSlot;
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
         if isLoadSysOp(ins) = '1' or isStoreSysOp(ins) = '1' then
             res.result := sysLoadValue;
         elsif false then
            -- TLB problems...
         elsif memLoadReady = '0' then
             res.controlInfo.dataMiss := '1';
         elsif storeForwardOutput.full = '1' then
             res.result := storeForwardOutput.ins.result;
             if storeForwardOutput.ins.controlInfo.sqMiss = '1' then
                 res.controlInfo.sqMiss := '1';
                 res.controlInfo.specialAction := '1';
                 res.controlInfo.newEvent := '1';
             end if;
             
             if work.LogicQueues.addressHighMatching(ins.result, storeForwardOutput.ins.target) = '0' then
                -- TMP
                res.controlInfo.sqMiss := '1';
                res.controlInfo.specialAction := '1';
                res.controlInfo.newEvent := '1';
             end if;
         else
            res.result := memLoadValue;
         end if;
       
         -- CAREFUL: store when newer load has been done - violation resolution when reissue is used
         if isStoreMemOp(ins) = '1' and lqSelectedOutput.full = '1' then
            res.controlInfo.orderViolation := '1';
            res.controlInfo.specialAction := '1';
            res.controlInfo.newEvent := '1';
         end if;

        -- TODO: remember about miss/hit status and reason of miss if relevant!
        return res;
    end function;
    
    function getStoreDataOp(ss: SchedulerEntrySlot) return SchedulerEntrySlot is
        variable res: SchedulerEntrySlot := ss;
    begin
    
        if CLEAR_DEBUG_INFO then
		    res.ins := clearDbCounters(res.ins);
            res.ins := clearRawInfo(res.ins);
            
            res.ins.specificOperation := DEFAULT_SPECIFIC_OP;
            
            res.ins.constantArgs := DEFAULT_CONSTANT_ARGS;
            res.ins.classInfo := DEFAULT_CLASS_INFO;
                    
            res.ins.virtualArgSpec := DEFAULT_ARG_SPEC;
            res.ins.physicalArgSpec := DEFAULT_ARG_SPEC;
            
            res.ins.target := (others => '0');
            res.ins.result := (others => '0');
        end if;
        
        return res;
    end function;    
end LogicExec;
