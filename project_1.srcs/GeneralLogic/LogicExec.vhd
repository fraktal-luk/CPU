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

	function basicBranch(sending: std_logic; st: SchedulerState; tags: InstructionTags;
                         ctrl: InstructionControlInfo;
                         target, result: Mword;
                         ac: AluControl) return ControlPacket;

	function executeAlu(full: std_logic; st: SchedulerState; link: Mword; ctrl: InstructionControlInfo;
	                                           ac: AluControl)
	                                           return ExecResult;

	function executeMulE0(full: std_logic; st: SchedulerState; link: Mword) return ExecResult;
	
	function executeFpu(st: SchedulerState) return Mword;

    function calcEffectiveAddress(st: SchedulerState; fromDLQ: std_logic; dlqData: ExecResult)
    return Mword;        

    function calcEffectiveAddress_2(full: std_logic; st: SchedulerState; fromDLQ: std_logic; dlqData: ExecResult)
    return ExecResult;

    function getLSResultData(op: SpecificOp;
                             result: Mword;
                             tlbReady: std_logic;
                             memLoadReady: std_logic;
                             sysLoadReady: std_logic;
                             ctSQ, ctLQ: ControlPacket
                             ) return InstructionControlInfo;


    function getLSResultData_result(op: SpecificOp;
                              memLoadReady: std_logic; memLoadValue: Mword;
                              sysLoadReady: std_logic; sysLoadValue: Mword;
                              ctSQ, ctLQ: ControlPacket
                             ) return ExecResult;                                               
end LogicExec;


package body LogicExec is

	function resolveBranchCondition(ss: SchedulerState; op: ArithOp; ac: AluControl) return std_logic is
		constant isZero: std_logic := not isNonzero(ss.args(0));
	begin			
		return ac.jumpType(1) or (ac.jumpType(0) xor isZero);
	end function;

	function basicBranch(sending: std_logic; st: SchedulerState; tags: InstructionTags;
	                     ctrl: InstructionControlInfo;
	                     target, result: Mword;
	                     ac: AluControl)
	return ControlPacket is
		variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
		variable branchTaken, targetMatch: std_logic := '0';
		variable storedTarget, storedReturn, trueTarget: Mword := (others => '0');
		variable targetEqual: std_logic := '0';
	begin
        res.tags := st.st.tags;

        res.controlInfo.full := sending;
		-- Cases to handle
		-- jr taken		: if not taken goto return, if taken and not equal goto reg, if taken and equal ok 
		-- jr not taken: if not taken ok, if taken goto reg
		-- j taken		: if not taken goto return, if taken equal
		-- j not taken : if not taken ok, if taken goto dest

        targetMatch := bool2std(target = st.args(1));
		branchTaken := resolveBranchCondition(st, st.st.operation.arith, ac);

        res.controlInfo.full := sending;

		if ctrl.frontBranch = '1' and branchTaken = '0' then
			res.controlInfo.newEvent := '1';
			trueTarget := result;
		elsif ctrl.frontBranch = '0' and branchTaken = '1' then
			res.controlInfo.newEvent := '1';
			res.controlInfo.confirmedBranch := '1';			
			if st.st.immediate = '0' then
				trueTarget := st.args(1);
			else
				trueTarget := target;
			end if;
		elsif ctrl.frontBranch = '0' and branchTaken = '0' then
			trueTarget := result;
		else -- taken -> taken
			if st.st.immediate = '0' then
				if targetMatch = '0' then
					res.controlInfo.newEvent := '1';	-- Need to correct the target!	
				end if;
				trueTarget := st.args(1); -- reg destination
			else
				trueTarget := target;
			end if;
			res.controlInfo.confirmedBranch := '1';			
		end if;

        if branchTaken = '0' then
            trueTarget := result;
        elsif st.st.immediate = '1' then
            trueTarget := target;
        else
            trueTarget := st.args(1);
        end if;
        
        if      (ctrl.frontBranch xor branchTaken) = '1'
                or  (ctrl.frontBranch and branchTaken and not st.st.immediate and not targetMatch) = '1'
        then
            res.controlInfo.newEvent := sending;
        else
            res.controlInfo.newEvent := '0';
        end if;

        res.controlInfo.confirmedBranch := branchTaken;

		res.target := trueTarget;
		-- Return address
		res.nip := result;
		res.tags.intPointer := tags.intPointer;
		res.tags.floatPointer := tags.floatPointer;
    	res.tags.sqPointer := tags.sqPointer;
	    res.tags.lqPointer := tags.lqPointer;		
		return res;
	end function;

	function executeAlu(full: std_logic; st: SchedulerState; link: Mword; ctrl: InstructionControlInfo; ac: AluControl)
	return ExecResult is
		variable res: ExecResult := DEFAULT_EXEC_RESULT;
		variable result: Mword := (others => '0');
		variable arg0, arg1, arg2: Mword := (others => '0');
		variable argAddSub: Mword := (others => '0');
		variable carryIn: std_logic := '0';
		variable resultExt: std_logic_vector(MWORD_SIZE downto 0) := (others => '0');
		variable resultExt0, resultExt1: Word := (others => '0');
		variable ov, carry, cl, cm0, cm1: std_logic := '0';
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

		addExtNewP(arg0, argAddSub, carryIn, resultExt0, resultExt1, cl, cm0, cm1);

        if (ac.adder and ((cl and cm1) or cm0)) = '1' then
            result(31 downto 20) := resultExt1(31 downto 20);
            result(19 downto 0) := resultExt0(19 downto 0);
        elsif ac.adder = '1' then
            result := resultExt0;
		else
            if ac.jump = '1' then
                result := link;	      
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
		
		res.full := full;
		res.tag := st.st.tags.renameIndex;
		res.dest := st.argSpec.dest;
		res.value := result;
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
            when opJ =>
                ac.jump := '1';
                ac.jumpType := "10";
            -- opMul, opMulshs, opMulhu, opDiv
            when others =>
                
        end case;
        
        return ac;
    end function;


	function executeMulE0(full: std_logic; st: SchedulerState; link: Mword) return ExecResult is
		variable res: ExecResult := DEFAULT_EXEC_RESULT;
		variable result: Mword := (others => '0');
		variable arg0, arg1, arg2: Mword := (others => '0');
		variable argAddSub: Mword := (others => '0');
		variable carryIn: std_logic := '0';
		variable resultExt: std_logic_vector(MWORD_SIZE downto 0) := (others => '0');
		variable resultExt0, resultExt1: Word := (others => '0');
		variable ov, carry, cl, cm0, cm1: std_logic := '0';
	    variable shiftInput, rotated, shiftOutput: Dword := (others => '0');
	begin
		arg0 := st.args(0);
		arg1 := st.args(1);
		arg2 := st.args(2);

        result := arg0 xor arg1;
		
		res.full := full;
		res.tag := st.st.tags.renameIndex;
		res.dest := st.argSpec.dest;
		res.value := result;
		return res;
	end function;

	
	function executeFpu(st: SchedulerState) return Mword is
       variable res: Mword := (others => '0');
	begin
        if st.st.operation.float = opOr then 
           res := st.args(0) or st.args(1);
        elsif st.st.operation.float = opMove then
           res := st.args(0);
        else
           
		end if;

		return res;
	end function;

  
    function calcEffectiveAddress(st: SchedulerState; fromDLQ: std_logic; dlqData: ExecResult)
    return Mword is
        variable res: Mword := (others => '0');
    begin
        if fromDLQ = '1' then
            res := st.args(1);
        elsif st.full = '1'then
            res := add(st.args(0), st.args(1));
        else
            res := (others => '0');
        end if;
        
        return res;
    end function;


    function calcEffectiveAddress_2(full: std_logic; st: SchedulerState; fromDLQ: std_logic; dlqData: ExecResult)
    return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
        variable adr: Mword := (others => '0'); 
    begin
        if fromDLQ = '1' then
            adr := st.args(1);
        elsif st.full = '1'then
            adr := add(st.args(0), st.args(1));
        else
            adr := (others => '0');
        end if;

        res.full := full;
        res.tag := st.st.tags.renameIndex;
        res.dest := st.argSpec.dest;        
        res.value := adr;
        
        return res;
    end function;
    
    function getLSResultData( op: SpecificOp;
                              result: Mword;
                              tlbReady: std_logic;  
                              memLoadReady: std_logic;
                              sysLoadReady: std_logic;
                              ctSQ, ctLQ: ControlPacket
                             ) return InstructionControlInfo is
        variable res: InstructionControlInfo := DEFAULT_CONTROL_INFO;
        constant sysOp: boolean := (isLoadSysOp(op) or isStoreSysOp(op)) = '1';
        constant memForwarded: boolean := std2bool(ctSQ.controlInfo.full);
        constant memFail: boolean := std2bool(not memLoadReady and isLoadMemOp(op));
    begin
        -- mfc/mtc?
        -- tlb/access error?
        -- tlb miss?
        -- data miss?            
        -- SQ forward miss?
        -- SQ forward hit?            
        -- else

        -- So far TLB and tag misses are not implemented
         if sysOp then

         elsif false then
            -- TLB problems...
         elsif memFail then
             res.dataMiss := '1';
         elsif memForwarded then
             if ctSQ.controlInfo.sqMiss = '1' then
                 res.sqMiss := '1';
                 res.specialAction := '1';
                 res.newEvent := '1';
             end if;
             
             if work.LogicQueues.addressHighMatching(result, ctSQ.target) = '0' then
                -- TMP
                res.sqMiss := '1';
                res.specialAction := '1';
                res.newEvent := '1';
             end if;
         else    -- successful mem load
         
         end if;
       
         -- CAREFUL: store when newer load has been done - violation resolution when reissue is used
         if isStoreMemOp(op) = '1' and ctLQ.controlInfo.full = '1' then
            res.orderViolation := '1';
            res.specialAction := '1';
            res.newEvent := '1';
         end if;

        return res;
    end function;

    function getLSResultData_result(op: SpecificOp;
                                  memLoadReady: std_logic; memLoadValue: Mword;
                                  sysLoadReady: std_logic; sysLoadValue: Mword;
                                  ctSQ, ctLQ: ControlPacket
                                 ) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
        constant sysOp: boolean := (isLoadSysOp(op) or isStoreSysOp(op)) = '1';
        constant memForwarded: boolean := std2bool(ctSQ.controlInfo.full);
        constant memFail: boolean := std2bool(not memLoadReady);
    begin
         if sysOp then
            res.value := sysLoadValue;
         elsif memForwarded then
            res.value := ctSQ.nip;
         else    -- successful mem load
            res.value := memLoadValue;
         end if;

        return res;
    end function;

end LogicExec;
