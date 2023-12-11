--

library IEEE;
use IEEE.STD_LOGIC_1164.all;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionStateBase.all;
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

	function basicBranch(sending: std_logic; ss_static: SchedulerState; args: MwordArray; bqControl: ControlPacket; ac: AluControl; lateEvent: ExecResult)
	return ControlPacket;

	function executeAlu(full: std_logic; ss_static: SchedulerState; args: MwordArray; link: Mword; ac: AluControl) return ExecResult;

	function prepareMultiply(full: std_logic; st: SchedulerState) return ExecResult;
	
	function executeFpu(st: SchedulerState; argValues: MwordArray(0 to 2)) return Mword;     
    function TMP_fp(full: std_logic; ss: SchedulerState; argValues: MwordArray(0 to 2)) return ExecResult;

    function mergeMemOp(stIQ, stMQ: SchedulerState; mqReady: std_logic) return SchedulerState;

    function calcEffectiveAddress(full: std_logic; st: SchedulerState; argValues: MwordArray(0 to 2); fromDLQ: std_logic) return ExecResult;

    function getLSResultData(op: SpecificOp; tlbReady: std_logic; memLoadReady: std_logic; ctSQ, ctLQ: ControlPacket) return InstructionControlInfo;
    function getLSResultData_result(op: SpecificOp; memLoadValue, sysLoadValue: Mword; ctSQ: ControlPacket) return ExecResult;
    function getBranchCompareEarly(ss: SchedulerState; full: std_logic) return ExecResult;
                                             
end LogicExec;


package body LogicExec is

	function resolveBranchCondition(arg: Mword; op: ArithOp; ac: AluControl) return std_logic is
		constant isZero: std_logic := not isNonzero(arg);
	begin
		return ac.jumpType(1) or (ac.jumpType(0) xor isZero);
	end function;

	function basicBranch(sending: std_logic; ss_static: SchedulerState; args: MwordArray; bqControl: ControlPacket; ac: AluControl; lateEvent: ExecResult)
	return ControlPacket is
		variable res: ControlPacket := DEFAULT_CONTROL_PACKET;
		variable branchTaken, targetMatch, targetEqual, newEvent: std_logic := '0';
		variable trueTarget: Mword := (others => '0');
		constant ctrl: InstructionControlInfo := bqControl.controlInfo;
		constant target: Mword := bqControl.target;
		constant result: Mword := bqControl.nip;
		constant argValues: MwordArray(0 to 2) := args;
		constant static: StaticInfo := ss_static.st;
	begin
		-- Cases to handle
		-- jr taken		: if not taken goto return, if taken and not equal goto reg, if taken and equal ok 
		-- jr not taken: if not taken ok, if taken goto reg
		-- j taken		: if not taken goto return, if taken equal
		-- j not taken : if not taken ok, if taken goto dest

        targetMatch := bool2std(target = argValues(1));
		branchTaken := resolveBranchCondition(argValues(0), static.operation.arith, ac);

        if false and lateEvent.full = '1' then
                trueTarget := lateEvent.value;
        elsif branchTaken = '0' then
            trueTarget := result;
        elsif static.immediate = '1' then
            trueTarget := target;         
        else
            trueTarget := argValues(1);
        end if;
        
        if false and lateEvent.full = '1' then
                newEvent := '1';
        elsif      (ctrl.frontBranch xor branchTaken) = '1'
               or  (branchTaken and not static.immediate and not targetMatch) = '1'
        then
            newEvent := sending;
        else
            newEvent := '0';
        end if;

        res.full := sending;
        res.controlInfo.c_full := sending;
        
	    res.dbInfo := static.dbInfo;

        res.controlInfo.newEvent := newEvent;
        res.controlInfo.confirmedBranch := branchTaken;
		res.target := trueTarget;

		res.nip := result;

        res.tags := static.tags;
		res.tags.intPointer := bqControl.tags.intPointer;
		res.tags.floatPointer := bqControl.tags.floatPointer;
    	res.tags.sqPointer := bqControl.tags.sqPointer;
	    res.tags.lqPointer := bqControl.tags.lqPointer;		
		return res;
	end function;


	function calculateAlu(args: MwordArray; link: Mword; ac: AluControl)
	return Mword is
		variable res: Mword := (others => '0');
		variable result: Mword := (others => '0');
		variable arg0, arg1, arg2: Mword := (others => '0');
		variable argAddSub: Mword := (others => '0');
		variable carryIn: std_logic := '0';

		variable resultExt0, resultExt1: Word := (others => '0');
		variable ov, carry, cl, cm0, cm1: std_logic := '0';
	    variable shiftInput, rotated, shiftOutput: Dword := (others => '0');
	begin

		arg0 := args(0);
		arg1 := args(1);
		arg2 := args(2);

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

		res := result;
		return res;
	end function;


	function executeAlu(full: std_logic; ss_static: SchedulerState; args: MwordArray; link: Mword; ac: AluControl) return ExecResult is
		variable res: ExecResult := DEFAULT_EXEC_RESULT;
	begin
		res.full := full;
	    res.dbInfo := ss_static.st.dbInfo;
	    res.poison := advancePoison(ss_static.poison);
		res.tag := ss_static.st.tags.renameIndex;
		res.dest := ss_static.dest;

		res.value := calculateAlu(args, link, ac);
		return res;
	end function;


    function getAluControl(op: ArithOp) return AluControl is
        variable ac: AluControl := DEFAULT_ALU_CONTROL; 
    begin
        case op is
            when opAdd | opAddH =>
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


	function prepareMultiply(full: std_logic; st: SchedulerState) return ExecResult is
		variable res: ExecResult := DEFAULT_EXEC_RESULT;
	begin 
        if (full and not isDivOp(st.st.operation)) = '1' then
            res.dbInfo := st.st.dbInfo; 
            res.full := full;

            res.poison := st.poison;

            res.tag := st.st.tags.renameIndex;
            res.dest := st.dest;
        end if;

		return res;
	end function;


	function executeFpu(st: SchedulerState; argValues: MwordArray(0 to 2)) return Mword is
       variable res: Mword := (others => '0');
	begin
        if st.st.operation.float = opOr then 
           res := argValues(0) or argValues(1);
        elsif st.st.operation.float = opMove then
           res := argValues(0);
		end if;

		return res;
	end function;


    function TMP_fp(full: std_logic; ss: SchedulerState; argValues: MwordArray(0 to 2)) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
    begin
        res.full := full;
        res.tag := ss.st.tags.renameIndex;
        res.dest := ss.dest;
        res.value := executeFpu(ss, argValues);
        return res;
    end function;


    function mergeMemOp(stIQ, stMQ: SchedulerState; mqReady: std_logic) return SchedulerState is
        variable res: SchedulerState := stIQ;
    begin
        if mqReady = '1' then
            res := stMQ;
        end if;
        
        return res;    
    end function;


    function calcEffectiveAddress(full: std_logic; st: SchedulerState; argValues: MwordArray(0 to 2); fromDLQ: std_logic) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
    begin
        res.full := full;
        
        res.poison := advancePoison(st.poison);
        res.dbInfo := st.st.dbInfo;
        res.tag := st.st.tags.renameIndex;
        res.dest := st.dest;        
        res.value := add(argValues(0), argValues(1));

        return res;
    end function;

    function getLSResultData(op: SpecificOp; tlbReady: std_logic; memLoadReady: std_logic; ctSQ, ctLQ: ControlPacket) return InstructionControlInfo is
        variable res: InstructionControlInfo := DEFAULT_CONTROL_INFO;
        constant sysOp: boolean := (isLoadSysOp(op) or isStoreSysOp(op)) = '1';
        constant memForwarded: boolean := std2bool(ctSQ.controlInfo.c_full);
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
             res.specialAction := '1';
             res.newEvent := '1';
         elsif memForwarded then
             if ctSQ.controlInfo.sqMiss = '1' then
                 res.sqMiss := '1';
                 res.specialAction := '1';
                 res.newEvent := '1';
             end if;
         else    -- successful mem load
            null;
         end if;

        res.dataMiss := bool2std(memFail);
        res.sqMiss := ctSQ.controlInfo.sqMiss;

         -- CAREFUL: store when newer load has been done - violation resolution when reissue is used
         if isStoreMemOp(op) = '1' and ctLQ.controlInfo.c_full = '1' then
            res.orderViolation := '1';
            res.specialAction := '1';
            res.newEvent := '1';
         end if;

        return res;
    end function;

    function getLSResultData_result(op: SpecificOp; memLoadValue, sysLoadValue: Mword; ctSQ: ControlPacket) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
        constant sysOp: boolean := (isLoadSysOp(op) or isStoreSysOp(op)) = '1';
        constant memForwarded: boolean := std2bool(ctSQ.controlInfo.c_full);
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

    function getBranchCompareEarly(ss: SchedulerState; full: std_logic) return ExecResult is
        variable res: ExecResult := DEFAULT_EXEC_RESULT;
    begin
        res.full := full and ss.st.branchIns;
        res.tag := ss.st.tags.renameIndex;
        res.dest := ss.st.tags.bqPointer;
        return res;
    end function; 

end LogicExec;
