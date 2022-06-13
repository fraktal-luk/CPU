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

	function basicBranch(sending: std_logic; st: SchedulerState;-- queueData: InstructionState;
	                                                           	tags: InstructionTags;
                                                                ctrl: InstructionControlInfo;
                                                                target, result: Mword;
	                                                           ac: AluControl) return InstructionState;

	function basicBranch_N(sending: std_logic; st: SchedulerState;-- queueData: InstructionState;
	                                                               tags: InstructionTags;
	                                                               ctrl: InstructionControlInfo;
	                                                               target, result: Mword;
	                                                               ac: AluControl) return ControlPacket;

	function executeAlu(st: SchedulerState;-- queueData: InstructionState; branchIns: InstructionState;
	                                           link: Mword; ctrl: InstructionControlInfo;
	                                           ac: AluControl)
	                                           --return InstructionState;
	                                           return ExecResult;

	function executeFpu(st: SchedulerState) return InstructionState;

    function calcEffectiveAddress(st: SchedulerState; fromDLQ: std_logic; dlqData: ExecResult)
    return InstructionState;        

    function getLSResultData(ins: InstructionState;
                              tlbReady: std_logic; tlbValue: Mword;
                              memLoadReady: std_logic; memLoadValue: Mword;
                              sysLoadReady: std_logic; sysLoadValue: Mword;
                              --storeForwardOutput: InstructionSlot;
                              --lqSelectedOutput: InstructionSlot;
                                ctSQ, ctLQ: ControlPacket
                             ) return InstructionState;
                                               
end LogicExec;



package body LogicExec is
function TMP_getIns(st: SchedulerState) return InstructionState is
	variable res: InstructionState := DEFAULT_INS_STATE;
	variable v0, v1: std_logic_vector(1 downto 0) := "00";
	variable selected0, selected1: Mword := (others => '0');
	variable ready: std_logic_vector(0 to 2) := (others=>'0');
	variable locs: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	constant Z3: std_logic_vector(0 to 2) := (others => '0');
	constant ZZ3: SmallNumberArray(0 to 2) := (others=>(others=>'0'));
	variable imm: Word := (others => '0');
begin
    res.tags.renameIndex := st.renameIndex;
    res.tags.bqPointer := st.bqPointer;
    res.tags.sqPointer := st.sqPointer;
    res.tags.lqPointer := st.lqPointer;
    res.tags.bqPointerSeq := st.bqPointerSeq;

    res.specificOperation := st.operation;

    res.physicalArgSpec.dest := st.argSpec.dest;
    res.physicalArgSpec.intDestSel := st.argSpec.intDestSel;
    res.physicalArgSpec.floatDestSel := st.argSpec.floatDestSel;
    
    res.physicalArgSpec.intArgSel := (others => '0');
    res.physicalArgSpec.floatArgSel := (others => '0');
        
    res.physicalArgSpec.args := st.argSpec.args;

    -- Clear unused fields       
    if CLEAR_DEBUG_INFO then
        res := clearAbstractInfo(res);
    end if;
    res.controlInfo.newEvent := '0';
    res.controlInfo.hasInterrupt := '0';
        
	return res;
end function;


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
	end function;

	function basicBranch(sending: std_logic; st: SchedulerState;-- queueData: InstructionState;
	                                                               tags: InstructionTags;
	                                                               ctrl: InstructionControlInfo;
	                                                               target, result: Mword;
	                                                               ac: AluControl) return InstructionState is
		variable res: InstructionState := DEFAULT_INS_STATE;
		variable branchTaken, targetMatch: std_logic := '0';
		variable storedTarget, storedReturn, trueTarget: Mword := (others => '0');
		variable targetEqual: std_logic := '0';
	begin
	           res.controlInfo := TMP_getIns(st).controlInfo;
	           res.tags := TMP_getIns(st).tags;

	       res.controlInfo.full := sending;
		-- Cases to handle
		-- jr taken		: if not taken goto return, if taken and not equal goto reg, if taken and equal ok 
		-- jr not taken: if not taken ok, if taken goto reg
		-- j taken		: if not taken goto return, if taken equal
		-- j not taken : if not taken ok, if taken goto dest

        targetMatch := --bool2std(queueData.target = st.args(1));
                        bool2std(target = st.args(1));
		branchTaken := resolveBranchCondition(st, st.operation.arith, ac);

        res.controlInfo.full := sending;

--		if queueData.controlInfo.frontBranch = '1' and branchTaken = '0' then
		if ctrl.frontBranch = '1' and branchTaken = '0' then
			res.controlInfo.newEvent := '1';
			trueTarget := --queueData.result;
			             result;
--		elsif queueData.controlInfo.frontBranch = '0' and branchTaken = '1' then
		elsif ctrl.frontBranch = '0' and branchTaken = '1' then
			res.controlInfo.newEvent := '1';
			res.controlInfo.confirmedBranch := '1';			
			if st.immediate = '0' then
				trueTarget := st.args(1);
			else
				trueTarget := --queueData.target;
				                target;
			end if;
--		elsif queueData.controlInfo.frontBranch = '0' and branchTaken = '0' then
		elsif ctrl.frontBranch = '0' and branchTaken = '0' then
			trueTarget := --queueData.result;
			                 result;
		else -- taken -> taken
			if st.immediate = '0' then
				if targetMatch = '0' then
					res.controlInfo.newEvent := '1';	-- Need to correct the target!	
				end if;
				trueTarget := st.args(1); -- reg destination
			else
				trueTarget := --queueData.target;
				                target;
			end if;
			res.controlInfo.confirmedBranch := '1';			
		end if;

        if branchTaken = '0' then
            trueTarget := --queueData.result;
                            result;
        elsif st.immediate = '1' then
            trueTarget := --queueData.target;
                            target;
        else
            trueTarget := st.args(1);
        end if;
        
--        if      (queueData.controlInfo.frontBranch xor branchTaken) = '1'
--            or  (queueData.controlInfo.frontBranch and branchTaken and not st.immediate and not targetMatch) = '1'
        if      (ctrl.frontBranch xor branchTaken) = '1'
                or  (ctrl.frontBranch and branchTaken and not st.immediate and not targetMatch) = '1'
        then
            res.controlInfo.newEvent := sending;
        else
            res.controlInfo.newEvent := '0';
        end if;

        res.controlInfo.confirmedBranch := branchTaken;

		res.target := trueTarget;
		-- Return address
		res.result := --queueData.result;
		              result;
		res.tags.intPointer := --queueData.tags.intPointer;
		                      tags.intPointer;
		res.tags.floatPointer := --queueData.tags.floatPointer;
		                          tags.floatPointer;
    	res.tags.sqPointer := --queueData.tags.sqPointer;
    	                       tags.sqPointer;
	    res.tags.lqPointer := --queueData.tags.lqPointer;
	                           tags.lqPointer;
		
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


	function basicBranch_N(sending: std_logic; st: SchedulerState;-- queueData: InstructionState;
	                                                               tags: InstructionTags;
	                                                               ctrl: InstructionControlInfo;
	                                                               target, result: Mword;
	                                                               ac: AluControl) return ControlPacket is
		variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
		variable branchTaken, targetMatch: std_logic := '0';
		variable storedTarget, storedReturn, trueTarget: Mword := (others => '0');
		variable targetEqual: std_logic := '0';
	begin
	       res.controlInfo := TMP_getIns(st).controlInfo;
           res.tags := TMP_getIns(st).tags;

	       res.controlInfo.full := sending;
		-- Cases to handle
		-- jr taken		: if not taken goto return, if taken and not equal goto reg, if taken and equal ok 
		-- jr not taken: if not taken ok, if taken goto reg
		-- j taken		: if not taken goto return, if taken equal
		-- j not taken : if not taken ok, if taken goto dest

        targetMatch := --bool2std(queueData.target = st.args(1));
                        bool2std(target = st.args(1));
		branchTaken := resolveBranchCondition(st, st.operation.arith, ac);

        res.controlInfo.full := sending;

--		if queueData.controlInfo.frontBranch = '1' and branchTaken = '0' then
		if ctrl.frontBranch = '1' and branchTaken = '0' then
			res.controlInfo.newEvent := '1';
			trueTarget := --queueData.result;
			             result;
--		elsif queueData.controlInfo.frontBranch = '0' and branchTaken = '1' then
		elsif ctrl.frontBranch = '0' and branchTaken = '1' then
			res.controlInfo.newEvent := '1';
			res.controlInfo.confirmedBranch := '1';			
			if st.immediate = '0' then
				trueTarget := st.args(1);
			else
				trueTarget := --queueData.target;
				                target;
			end if;
--		elsif queueData.controlInfo.frontBranch = '0' and branchTaken = '0' then
		elsif ctrl.frontBranch = '0' and branchTaken = '0' then
			trueTarget := --queueData.result;
			                 result;
		else -- taken -> taken
			if st.immediate = '0' then
				if targetMatch = '0' then
					res.controlInfo.newEvent := '1';	-- Need to correct the target!	
				end if;
				trueTarget := st.args(1); -- reg destination
			else
				trueTarget := --queueData.target;
				                target;
			end if;
			res.controlInfo.confirmedBranch := '1';			
		end if;

        if branchTaken = '0' then
            trueTarget := --queueData.result;
                            result;
        elsif st.immediate = '1' then
            trueTarget := --queueData.target;
                            target;
        else
            trueTarget := st.args(1);
        end if;
        
--        if      (queueData.controlInfo.frontBranch xor branchTaken) = '1'
--            or  (queueData.controlInfo.frontBranch and branchTaken and not st.immediate and not targetMatch) = '1'
        if      (ctrl.frontBranch xor branchTaken) = '1'
                or  (ctrl.frontBranch and branchTaken and not st.immediate and not targetMatch) = '1'
        then
            res.controlInfo.newEvent := sending;
        else
            res.controlInfo.newEvent := '0';
        end if;

        res.controlInfo.confirmedBranch := branchTaken;

		res.target := trueTarget;
		-- Return address
		res.nip := --queueData.result;
		              result;
		res.tags.intPointer := --queueData.tags.intPointer;
		                      tags.intPointer;
		res.tags.floatPointer := --queueData.tags.floatPointer;
		                          tags.floatPointer;
    	res.tags.sqPointer := --queueData.tags.sqPointer;
    	                       tags.sqPointer;
	    res.tags.lqPointer := --queueData.tags.lqPointer;
	                           tags.lqPointer;
		
		if CLEAR_DEBUG_INFO then
--		    res := clearDbCounters(res);
--		    res := clearRawInfo(res);
--		    res.constantArgs := DEFAULT_CONSTANT_ARGS;
--		    res.classInfo := DEFAULT_CLASS_INFO;
		    
--		    res.specificOperation := DEFAULT_SPECIFIC_OP;
		      
--		    res.virtualArgSpec.intArgSel := (others => '0');
--		    res.virtualArgSpec.floatArgSel := (others => '0');
--		    res.virtualArgSpec.args := (others => (others => '0'));
		    
--		    res.physicalArgSpec.intArgSel := (others => '0');
--		    res.physicalArgSpec.floatArgSel := (others => '0');
--		    res.physicalArgSpec.args := (others => (others => '0'));
		    
--		    res.result := (others => '0');
		end if;		
		return res;
	end function;

	function executeAlu(st: SchedulerState;-- queueData: InstructionState; branchIns: InstructionState;
	                                           link: Mword; ctrl: InstructionControlInfo;
	                                           ac: AluControl)
	--return InstructionState is
	return ExecResult is
		variable res: InstructionState := TMP_getIns(st);-- ins;
		variable res_N: ExecResult := DEFAULT_EXEC_RESULT;
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

		if ac.sub = '1' then
			argAddSub := not arg1;
			carryIn := '1';
		else
			argAddSub := arg1;
			carryIn := '0';
		end if;

		shTemp(5 downto 0) := arg1(5 downto 0);
		if ac.shiftType = "00" then
			shNum := subSN(shNum, shTemp);
		else
			shNum := shTemp;
		end if;
	
		shH := slv2s(shNum(5 downto 3));
		shL := slv2u(shNum(2 downto 0));
	
		if ac.shiftType = "01" then
			tempBits(95 downto 64) := (others => arg0(MWORD_SIZE-1));	
		end if;
		tempBits(63 downto 32) := arg0;

		shiftedBytes := tempBits(71 + 8*shH downto 32 + 8*shH);	

        shiftInput(31 downto 0) := arg0;
        rotated := rotate64(shiftInput, arg1(5 downto 0));
        shiftOutput := rotated; -- TEMP: works for 32b and 5-bit shift amount

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
		linkAdr := --queueData.result;
		              link;
	    
	    res.controlInfo.newEvent := '0';
	    res.controlInfo.hasException := '0';
		
		addExtNewP(arg0, argAddSub, carryIn, resultExt0, resultExt1, cl, cm0, cm1);

        if (ac.adder and ((cl and cm1) or cm0)) = '1' then
            result(31 downto 20) := resultExt1(31 downto 20);
            result(19 downto 0) := resultExt0(19 downto 0);
        elsif ac.adder = '1' then
            result := resultExt0;
		else
		      if ac.jump = '1' then
					result := linkAdr;
              
                  res.controlInfo.newEvent := --branchIns.controlInfo.newEvent;
                                               ctrl.newEvent;
                  res.controlInfo.frontBranch := --branchIns.controlInfo.frontBranch;
                                                  ctrl.frontBranch;
                  res.controlInfo.confirmedBranch := --branchIns.controlInfo.confirmedBranch;
                                                     ctrl.confirmedBranch;		      
		      elsif ac.shifter = '1' then
		          result := shiftOutput(31 downto 0);
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
		
		  res_N.full := st.full;
		  res_N.value := result;
		return res_N;
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

	
	function executeFpu(st: SchedulerState) return InstructionState is
       variable res: InstructionState := DEFAULT_INS_STATE;--ins;
	begin
        if st.operation.float = opOr then 
           res.result := st.args(0) or st.args(1);
        elsif st.operation.float = opMove then
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

            
    function calcEffectiveAddress(st: SchedulerState; fromDLQ: std_logic; dlqData: ExecResult)
    return InstructionState is
        variable res: InstructionState := DEFAULT_INS_STATE;
    begin
        res.specificOperation := st.operation;
    
        res.tags.renameIndex := st.renameIndex;
        res.tags.bqPointer := st.bqPointer;
        res.tags.bqPointerSeq := st.bqPointerSeq;
        res.tags.sqPointer := st.sqPointer;
        res.tags.lqPointer := st.lqPointer;
    
        if fromDLQ = '1' then
            --res := dlqData;
        elsif st.full = '1'then
            res.result := add(st.args(0), st.args(1));
        else
            res.result := (others => '0');
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
                              --storeForwardOutput: InstructionSlot;
                              --lqSelectedOutput: InstructionSlot;
                                ctSQ, ctLQ: ControlPacket
                             ) return InstructionState is
        variable res: InstructionState := DEFAULT_INS_STATE; --ins;
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
         elsif --storeForwardOutput.full = '1' then
                ctSQ.controlInfo.full = '1' then
             res.result := --storeForwardOutput.ins.result;
                             ctSQ.nip;
             if --storeForwardOutput.ins.controlInfo.sqMiss = '1' then
                ctSQ.controlInfo.sqMiss = '1' then
                 res.controlInfo.sqMiss := '1';
                 res.controlInfo.specialAction := '1';
                 res.controlInfo.newEvent := '1';
             end if;
             
             if work.LogicQueues.addressHighMatching(ins.result, --storeForwardOutput.ins.target) = '0' then
                                                                 ctSQ.target) = '0' then
                -- TMP
                res.controlInfo.sqMiss := '1';
                res.controlInfo.specialAction := '1';
                res.controlInfo.newEvent := '1';
             end if;
         else
            res.result := memLoadValue;
         end if;
       
         -- CAREFUL: store when newer load has been done - violation resolution when reissue is used
         if isStoreMemOp(ins) = '1' and --lqSelectedOutput.full = '1' then
                                        ctLQ.controlInfo.full = '1' then
            res.controlInfo.orderViolation := '1';
            res.controlInfo.specialAction := '1';
            res.controlInfo.newEvent := '1';
         end if;

        -- TODO: remember about miss/hit status and reason of miss if relevant!
        return res;
    end function;
   
end LogicExec;
