--

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


package LogicFront is

function decodeInstructionNew(inputState: InstructionState) return InstructionState;

function getFrontEventMulti(predictedAddress, ip, target: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray;

function getEarlyEvent(earlyBranchMultiDataInA: InstructionSlotArray; target, predictedAddress: Mword; fetchStall: std_logic)
return InstructionState;
	
function prepareForBQ(ip: Mword; insVec: InstructionSlotArray) return InstructionSlotArray;

end LogicFront;



package body LogicFront is

function decodeInstructionNew(inputState: InstructionState) return InstructionState is
	variable res: InstructionState := inputState;
    variable decodedIns: InstructionState := DEFAULT_INSTRUCTION_STATE;
begin
  	decodedIns := decodeFromWordNew(inputState.bits);
	
	res.specificOperation := decodedIns.specificOperation;
	res.constantArgs := decodedIns.constantArgs;
	res.virtualArgSpec := decodedIns.virtualArgSpec;
	
    res.classInfo.fpRename := decodedIns.classInfo.fpRename;
    res.classInfo.branchIns := decodedIns.classInfo.branchIns;
    res.classInfo.mainCluster := decodedIns.classInfo.mainCluster;
    res.classInfo.secCluster := decodedIns.classInfo.secCluster;
    res.classInfo.useLQ := decodedIns.classInfo.useLQ;

     if res.specificOperation.subpipe = none then                 	
        res.controlInfo.specialAction := '1';
        -- CAREFUL: Those ops don't get issued, they are handled at retirement
        res.classInfo.mainCluster := '0';
        res.classInfo.secCluster := '0';
        
        if res.specificOperation.system = opUndef then
            res.controlInfo.hasException := '1';
        end if;        
    end if;
   
    res.controlInfo.specialAction := not (res.classInfo.mainCluster or res.classInfo.secCluster);

	return res;
end function;




function isJumpLink(w: Word) return std_logic is
begin
    if TMP_PARAM_NEW_DECODE then
        return bool2std(w(31 downto 26) = "001001");
    end if;

    return bool2std(w(31 downto 26) = opcode2slv(jl));
end function;

function isJumpCond(w: Word) return std_logic is
begin
    if TMP_PARAM_NEW_DECODE then
        return bool2std(w(31 downto 26) = "001010" or w(31 downto 26) = "001011");
    end if;

    return bool2std(w(31 downto 26) = opcode2slv(jz)) or bool2std(w(31 downto 26) = opcode2slv(jnz));
end function;

function isJumpLong(w: Word) return std_logic is
begin
    if TMP_PARAM_NEW_DECODE then
        return bool2std(w(31 downto 26) = "001000");
    end if;

    return bool2std(w(31 downto 26) = opcode2slv(j));
end function;

function isJumpReg(w: Word) return std_logic is
begin
    if TMP_PARAM_NEW_DECODE then
        return bool2std(w(31 downto 26) = "000000" and w(15 downto 10) = "000010" and (w(4 downto 0) = "00000" or w(4 downto 0) = "00001"));
    end if;
    
    return bool2std(w(31 downto 26) = opcode2slv(ext1)) and bool2std(w(15 downto 10) = opcont2slv(ext1, jzR) or w(15 downto 10) = opcont2slv(ext1, jzR));
end function;


function getFrontEventMulti(predictedAddress, ip, target: Mword; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	variable tempOffset, lastRes: Mword := (others => '0');
	variable targets: MwordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
	variable fullOut, full, branchIns, predictedTaken, uncondJump: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');
	variable nSkippedIns: integer := 0;
	variable regularJump, longJump, regJump: std_logic := '0';
begin
	-- CAREFUL: Only without hword instructions now!
	nSkippedIns := slv2u(predictedAddress(ALIGN_BITS-1 downto 2));	-- How many are before fetch address							
			
	for i in 0 to FETCH_WIDTH-1 loop
		full(i) := '1';
		if i < nSkippedIns then
			full(i) := '0';
		end if;

        res(i).ins.bits := fetchLine(i);
		res(i).ins := decodeInstructionNew(res(i).ins); -- Here decoding!

        res(i).ins.ip := ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(i*4, ALIGN_BITS);    -- !! Only for BQ, indirect   
        res(i).ins.result := ip;
        res(i).ins.result(ALIGN_BITS-1 downto 0) := i2slv((i+1)*4, ALIGN_BITS); -- !! Only for BQ/  CAREFUL: not for short ins
	end loop;
	lastRes := ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(0, ALIGN_BITS);
	res(FETCH_WIDTH-1).ins.result := add(lastRes, PC_INC); -- !! Only for BQ

    -- Calculate target for each instruction, even if it's to be skipped
    for i in 0 to FETCH_WIDTH-1 loop        
        regularJump := '0';
        longJump := '0';
        regJump := '0';

        -- jmp reg has src1
        -- jmp cnd has src0
        -- jmp lnk has dest
        -- jmp lng has none
    
        if isJumpLink(fetchLine(i)) = '1' then
            uncondJump(i) := '1';
            regularJump := '1';				
            predictedTaken(i) := '1';       -- jump link is unconditional
        elsif isJumpCond(fetchLine(i)) = '1'
        then
            regularJump := '1';				
            predictedTaken(i) := fetchLine(i)(20);		-- CAREFUL: temporary predicted taken iff backwards
        elsif isJumpLong(fetchLine(i)) = '1' then
            uncondJump(i) := '1';
            longJump := '1';
            predictedTaken(i) := '1'; -- Long jump is unconditional (no space for register encoding!)
        elsif isJumpReg(fetchLine(i)) = '1' then   
            regJump := '1';
            predictedTaken(i) := '0';               -- TEMP: register jumps predicted not taken
        end if;
        
        if longJump = '1' then
            tempOffset := (others => fetchLine(i)(25));
            tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
        else
            tempOffset := (others => fetchLine(i)(20));
            tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);
        end if;
    
        branchIns(i) := regularJump or longJump or regJump;
        res(i).ins.target := add(res(i).ins.ip, tempOffset);	-- !! Only for BQ
    end loop;
    
    -- Find if any branch predicted
    for i in 0 to FETCH_WIDTH-1 loop
        fullOut(i) := full(i);
        if full(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            if uncondJump(i) = '1' then -- CAREFUL: setting it here, so that if implementation treats is as NOP in Exec, it still gets this flag
                res(i).ins.controlInfo.confirmedBranch := '1';
            end if;

            res(i).ins.controlInfo.frontBranch := '1';					
            
            -- Here check if the next line from line predictor agrees with the target predicted now.
            --	If so, don't cause the event but set invalidation mask that next line will use.
            if res(i).ins.target(MWORD_SIZE-1 downto ALIGN_BITS) /= target(MWORD_SIZE-1 downto ALIGN_BITS) then
                res(i).ins.controlInfo.newEvent := '1';         -- !! Only for BQ
            end if;
            
            -- CAREFUL: When not using line predictor, branches predicted taken must always be done here 
            if not USE_LINE_PREDICTOR then
                res(i).ins.controlInfo.newEvent := '1';         -- !! Only for BQ
            end if;

            exit;
        end if;
    end loop;

	for i in 0 to FETCH_WIDTH-1 loop
	   res(i).full := fullOut(i);

        if res(i).full = '1' and res(i).ins.classInfo.branchIns = '1' then
            res(0).ins.controlInfo.firstBr := '1'; -- TMP, indicating that group has a branch
        end if;
	   
       if CLEAR_DEBUG_INFO then
          res(i).ins.specificOperation.arith := opAnd;
          res(i).ins.specificOperation.memory := opLoad;
          res(i).ins.specificOperation.float := opMove;
          res(i).ins.specificOperation.system := opNone;

          res(i) := clearRawInfo(res(i));          
          res(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;        
       end if;	   
	end loop;
	return res;
end function;


function findEarlyTakenJump(target: Mword; insVec: InstructionSlotArray) return InstructionState is
	variable res: InstructionState := DEFAULT_INS_STATE;
begin
    res.target := target;

	for i in 0 to PIPE_WIDTH-1 loop
		if insVec(i).full = '1' and insVec(i).ins.controlInfo.frontBranch = '1' then
		    if not CLEAR_DEBUG_INFO then
		       res := insVec(i).ins; 
		    end if;

		    res.controlInfo.newEvent := insVec(i).ins.controlInfo.newEvent; -- CAREFUL: event only if needs redirection, but break group at any taken jump 
            res.controlInfo.frontBranch := '1';
            res.target := insVec(i).ins.target; -- Correcting target within subsequent fetch line is still needed even if no redirection!
            exit;
		end if;
	end loop;
	
	return res;
end function;


function getEarlyEvent(earlyBranchMultiDataInA: InstructionSlotArray; target, predictedAddress: Mword; fetchStall: std_logic)
return InstructionState is
    variable res: InstructionState := DEFAULT_INS_STATE;
begin
    if fetchStall = '1' then -- Need refetching
        res.target := predictedAddress;
        res.controlInfo.newEvent := '1';
        res.controlInfo.refetch := '1';
    else
        res := findEarlyTakenJump(target, earlyBranchMultiDataInA);
    end if;
       
    return res;
end function;



--       earlyInputSending <= prevSendingBr and dataInBr(0).ins.controlInfo.firstBr;
-- ....
--    for i in insVec'range loop
--        res.full(i) := insVec(i).full;
--        res.frontBranch(i) := insVec(i).ins.controlInfo.frontBranch;
--        res.confirmedBranch(i) := insVec(i).ins.controlInfo.confirmedBranch;

--        res.targets(i) := insVec(i).ins.target;
--        res.links(i) := insVec(i).ins.result;
--    end loop;

--    res.ip := insVec(0).ins.ip;
    
function prepareForBQ(ip: Mword; insVec: InstructionSlotArray) return InstructionSlotArray is
	variable res, insVecSh: InstructionSlotArray(insVec'range) := insVec;
	variable result, target: Mword;
	variable branchMask: std_logic_vector(insVec'range) := (others => '0');
	variable nSh: natural := 0;
begin
    -- insVec: USES (controlInfo, target, full, classInfo.branchIns) [ result is overwritten]

    insVecSh := insVec;
    branchMask := getBranchMask(insVec);
    nSh := slv2u(ip(ALIGN_BITS-1 downto 2));

    for i in 0 to PIPE_WIDTH-1 loop
        if not TMP_PARAM_COMPRESS_RETURN then
            if i + nSh >= PIPE_WIDTH-1 then
                insVecSh(i).ins.result(MWORD_SIZE-1 downto ALIGN_BITS) := addInt(ip(MWORD_SIZE-1 downto ALIGN_BITS), 1);
                insVecSh(i).ins.result(ALIGN_BITS-1 downto 0) := (others => '0');
            else
                insVecSh(i).ins.result(MWORD_SIZE-1 downto ALIGN_BITS) := ip(MWORD_SIZE-1 downto ALIGN_BITS);      
                insVecSh(i).ins.result(ALIGN_BITS-1 downto 2) := i2slv(i + nSh + 1, ALIGN_BITS-2);                           
            end if;
        else    
            if i + nSh >= PIPE_WIDTH-1 then
                insVecSh(i).ins.result(MWORD_SIZE-1 downto ALIGN_BITS) := i2slv(1, MWORD_SIZE-ALIGN_BITS);
                insVecSh(i).ins.result(ALIGN_BITS-1 downto 0) := (others => '0');
            else
                insVecSh(i).ins.result(MWORD_SIZE-1 downto ALIGN_BITS) := (others => '0');
                insVecSh(i).ins.result(ALIGN_BITS-1 downto 2) := i2slv(i + nSh + 1, ALIGN_BITS-2);                           
            end if;
        end if;         
    end loop;

	for i in insVec'range loop
	   res(i).full := branchMask(i) and insVec(i).full; -- TODO: getBranchMask already check for 'full' - remove it here?   
       if CLEAR_DEBUG_INFO then -- Otherwise everything remains
           res(i).ins := DEFAULT_INS_STATE;
           res(i).ins.controlInfo := insVecSh(i).ins.controlInfo;
           res(i).ins.target := insVecSh(i).ins.target;
           res(i).ins.result := insVecSh(i).ins.result;
	   end if;
	end loop;

    -- TMP!
    res(0).ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) := ip(MWORD_SIZE-1 downto ALIGN_BITS);
    
	return res;
end function;

end LogicFront;
