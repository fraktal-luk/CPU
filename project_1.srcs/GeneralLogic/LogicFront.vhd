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

function decodeInstruction(inputState: InstructionState) return InstructionState;

function getFrontEventMulti(predictedAddress: Mword; ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray;

function getEarlyEvent(inputIns: Instructionstate; earlyBranchMultiDataInA: InstructionSlotArray;
                       predictedAddress: Mword; fetchStall: std_logic)
return InstructionState;
	
function prepareForBQ(insVec: InstructionSlotArray) return InstructionSlotArray;

end LogicFront;



package body LogicFront is

function getInstructionClassInfo(ins: InstructionState) return InstructionClassInfo is
	variable ci: InstructionClassInfo := defaultClassInfo;
begin 
    -- Which clusters?
    ci.mainCluster := '1';
    if ins.specificOperation.subpipe = Mem and (ins.specificOperation.memory = opStore or ins.specificOperation.memory = opStoreSys) then
        ci.secCluster := '1';
    end if;

    if ins.specificOperation.subpipe = Alu
        and (ins.specificOperation.arith = opJ or ins.specificOperation.arith = opJz or ins.specificOperation.arith = opJnz) then
        ci.branchIns := '1';
    end if;

    if ins.specificOperation.subpipe = none then  
        ci.mainCluster := '0';
        ci.secCluster := '0';
    end if;

	return ci;
end function;

function decodeInstruction(inputState: InstructionState) return InstructionState is
	variable res: InstructionState := inputState;
    variable decodedIns: InstructionState := DEFAULT_INSTRUCTION_STATE;
	variable tmpVirtualArgs: InstructionVirtualArgs;
	variable tmpVirtualDestArgs: InstructionVirtualDestArgs;
begin
	decodedIns := decodeFromWord(inputState.bits);
	
	res.specificOperation := decodedIns.specificOperation;
	res.constantArgs := decodedIns.constantArgs;
	res.virtualArgSpec := decodedIns.virtualArgSpec;
	
	res.classInfo := getInstructionClassInfo(res);	
    res.classInfo.fpRename := decodedIns.classInfo.fpRename;

     if res.specificOperation.subpipe = none then                 	
        res.controlInfo.specialAction := '1'; -- TODO: move this to classInfo?
        -- CAREFUL: Those ops don't get issued, they are handled at retirement
        res.classInfo.mainCluster := '0';
        res.classInfo.secCluster := '0';
        
        if res.specificOperation.system = opUndef then
            res.controlInfo.hasException := '1';
        end if;        
    end if;

	return res;
end function;


function isJumpLink(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = opcode2slv(jl));
           -- w(31);
end function;

function isJumpCond(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = opcode2slv(jz)) or bool2std(w(31 downto 26) = opcode2slv(jnz));
           -- w(30);
end function;

function isJumpLong(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = opcode2slv(j));
           -- w(29);
end function;

function isJumpReg(w: Word) return std_logic is
begin
    return bool2std(w(31 downto 26) = opcode2slv(ext1)) and bool2std(w(15 downto 10) = opcont2slv(ext1, jzR) or w(15 downto 10) = opcont2slv(ext1, jzR));
           -- w(28);
end function;


function getFrontEventMulti(predictedAddress: Mword; ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
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
		res(i).ins := decodeInstruction(res(i).ins); -- Here decoding!
		res(i).ins.tags.fetchCtr := ins.tags.fetchCtr(31 downto LOG2_PIPE_WIDTH) & i2slv(i, LOG2_PIPE_WIDTH);

        res(i).ins.ip := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(i*4, ALIGN_BITS);    -- !! Only for BQ, indirect   
        res(i).ins.result := ins.ip;
        res(i).ins.result(ALIGN_BITS-1 downto 0) := i2slv((i+1)*4, ALIGN_BITS); -- !! Only for BQ/  CAREFUL: not for short ins
	end loop;
	lastRes := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(0, ALIGN_BITS);
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
        --res(i).ins.classInfo.branchIns := branchIns(i);
        if full(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            if uncondJump(i) = '1' then -- CAREFUL: setting it here, so that if implementation treats is as NOP in Exec, it still gets this flag
                res(i).ins.controlInfo.confirmedBranch := '1';
            end if;

            res(i).ins.controlInfo.frontBranch := '1';					
            
            -- Here check if the next line from line predictor agrees with the target predicted now.
            --	If so, don't cause the event but set invalidation mask that next line will use.
            if res(i).ins.target(MWORD_SIZE-1 downto ALIGN_BITS) /= ins.target(MWORD_SIZE-1 downto ALIGN_BITS) then
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
	   
       if CLEAR_DEBUG_INFO then
          res(0).ins.specificOperation.arith := opAnd;
          res(0).ins.specificOperation.memory := opLoad;
          res(0).ins.specificOperation.float := opMove;
          res(0).ins.specificOperation.system := opNone;

          res(i).ins.ip := (others => '0');
          res(i).ins.bits := (others => '0');
          
          -- CAREFUL: BQ content depends on this
          --res(i).ins.target := (others => '0');
          --res(i).ins.result := (others => '0'); 
          
--          --res(i).ins.classInfo := DEFAULT_CLASS_INFO; 

--          --res(i).ins.constantArgs := DEFAULT_CONSTANT_ARGS;
--          --res(i).ins.virtualArgSpec := DEFAULT_ARG_SPEC;
--          --res(i).ins.physicalArgSpec := DEFAULT_ARG_SPEC;
        
--          --res(i).ins.specificOperation := DEFAULT_SPECIFIC_OP;
           
          res(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;        
       end if;	   
	end loop;
	return res;
end function;


function findEarlyTakenJump(ins: InstructionState; insVec: InstructionSlotArray) return InstructionState is
	variable res: InstructionState := ins;
begin
	for i in 0 to PIPE_WIDTH-1 loop
		if insVec(i).full = '1' and insVec(i).ins.controlInfo.frontBranch = '1' then
		    res.controlInfo.newEvent := insVec(i).ins.controlInfo.newEvent; -- CAREFUL: event only if needs redirection, but break group at any taken jump 
            res.controlInfo.frontBranch := '1';
            res.target := insVec(i).ins.target; -- Correcting target within subsequent fetch line is still needed even if no redirection!
            exit;
		end if;
	end loop;
	
	return res;
end function;


function getEarlyEvent(inputIns: Instructionstate; earlyBranchMultiDataInA: InstructionSlotArray;
                       predictedAddress: Mword; fetchStall: std_logic)
return InstructionState is
    variable res: InstructionState := inputIns;
begin
    if fetchStall = '1' then -- Need refetching
        res.target := predictedAddress;
        res.controlInfo.newEvent := '1';
        res.controlInfo.refetch := '1';
    else
        res := findEarlyTakenJump(inputIns, earlyBranchMultiDataInA);
        res.ip := predictedAddress;
    end if;
       
    return res;
end function;

function prepareForBQ(insVec: InstructionSlotArray) return InstructionSlotArray is
	variable res: InstructionSlotArray(insVec'range) := insVec;
	variable result, target: Mword;
	constant BRANCH_MASK: std_logic_vector(insVec'range) := getBranchMask(insVec);
begin
	for i in insVec'range loop
	   res(i).full := BRANCH_MASK(i) and insVec(i).full; -- TODO: getBranchMask already check for 'full' - remove it here?   
		
       if CLEAR_DEBUG_INFO then
           res(i).ins.ip := (others => '0');
           res(i).ins.bits := (others => '0');
           
           res(i).ins.classInfo := DEFAULT_CLASS_INFO; 
           
           -- target and result are stored in BQ

           res(i).ins.constantArgs := DEFAULT_CONSTANT_ARGS;
           res(i).ins.virtualArgSpec := DEFAULT_ARG_SPEC;
           res(i).ins.physicalArgSpec := DEFAULT_ARG_SPEC;
         
           res(i).ins.specificOperation := DEFAULT_SPECIFIC_OP;
            
           res(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;		
	   end if;
	end loop;
	
	return res;
end function;

end LogicFront;
