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


package LogicFront is

function getInstructionClassInfo(ins: InstructionState) return InstructionClassInfo;
function decodeInstruction(inputState: InstructionState) return InstructionState;
function decodeMulti(sd: InstructionSlotArray) return InstructionSlotArray;
function decodeLine(sd: WordArray) return InstructionSlotArray;

function fillTargetsAndLinks(insVec: InstructionSlotArray) return InstructionSlotArray;

function getFrontEventMulti(predictedAddress: Mword; ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray;

function getFrontEventMulti2(predictedAddress: Mword; ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray;

function findEarlyTakenJump(ins: InstructionState; insVec: InstructionSlotArray) return InstructionState;

function getEarlyEvent(inputIns: Instructionstate; earlyBranchMultiDataInA: InstructionSlotArray;
                       predictedAddress: Mword; fetchStall: std_logic)
return InstructionState;
	
function prepareForBQ(insVec: InstructionSlotArray; branchMask: std_logic_vector) return InstructionSlotArray;

end LogicFront;



package body LogicFront is

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
				   -- ins.classInfo.branchIns = '1' then
					ci.branchIns := '1';
					--ci.secCluster := '1';
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
    res.classInfo.fpRename := decodedIns.classInfo.fpRename;

    if res.operation.unit = System and
            (	res.operation.func = sysRetI or res.operation.func = sysRetE
            or res.operation.func = sysSync or res.operation.func = sysReplay
            or res.operation.func = sysError
            or res.operation.func = sysHalt
            or res.operation.func = sysCall
            or res.operation.func = sysSend ) then 		
        res.controlInfo.specialAction := '1'; -- TODO: move this to classInfo?
        
        -- CAREFUL: Those ops don't get issued, they are handled at retirement
        res.classInfo.mainCluster := '0';
        res.classInfo.secCluster := '0';
    end if;	
	
--	   if res.operation.unit = System then
--	       if    res.operation.func = sysRetI then
--	           res.classInfo.ret1 := '1';
--	           res.classInfo.sync := '1';	           
--	       elsif res.operation.func = sysRetE then
--	           res.classInfo.ret0 := '1';
--	           res.classInfo.sync := '1';	                          
--	       elsif res.operation.func = sysSync then
--	           res.classInfo.sync := '1';
--	       elsif res.operation.func = sysReplay then
--	           res.controlInfo.refetch := '1';
--	       elsif res.operation.func = sysHalt then
--	           res.classInfo.halt := '1';
--	           res.classInfo.sync := '1';         
--	       elsif res.operation.func = sysCall then
--	           --res.classInfo.ret1 := '1';	  -- ???? TODO
--	           res.classInfo.sync := '1';

	           	                
--	       elsif res.operation.func = sysSend then
--	           res.classInfo.isSend := '1';
--	       elsif res.operation.func = sysError then
--               res.classInfo.isError := '1';	               	       
--	       end if;
--	   end if;
	
	
    if res.operation.func = sysUndef then
        res.controlInfo.hasException := '1';
        --res.controlInfo.exceptionCode := i2slv(ExceptionType'pos(undefinedInstruction), SMALL_NUMBER_SIZE);
    end if;
    
    --if res.controlInfo.squashed = '1' then	-- CAREFUL: ivalid was '0'
    --    report "Trying to decode invalid location" severity error;
    --end if;
    
    --res.controlInfo.squashed := '0';
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

function decodeLine(sd: WordArray) return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to sd'length-1) := (others => DEFAULT_INS_SLOT);
	variable ins: InstructionState := DEFAULT_INSTRUCTION_STATE;
begin
	for i in 0 to PIPE_WIDTH-1 loop
	    ins.bits := sd(i);
		res(i).ins := decodeInstruction(ins);
		res(i).ins.bits := ins.bits;
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


function getFrontEventMulti(predictedAddress: Mword;
							  ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	variable tempOffset: Mword := (others => '0');
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
		
		res(i).ins.tags.fetchCtr := ins.tags.fetchCtr(31 downto LOG2_PIPE_WIDTH) & i2slv(i, LOG2_PIPE_WIDTH);

        res(i).ins.bits := fetchLine(i);
        res(i).ins.ip := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(i*4, ALIGN_BITS);        
        res(i).ins.result := ins.ip;
        res(i).ins.result(ALIGN_BITS-1 downto 0) := i2slv((i+1)*4, ALIGN_BITS); -- CAREFUL: not for short ins
	end loop;
	res(PIPE_WIDTH-1).ins.result := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(0, ALIGN_BITS);
	res(PIPE_WIDTH-1).ins.result := addMwordBasic(res(PIPE_WIDTH-1).ins.result, PC_INC);

    -- Calculate target for each instruction, even if it's to be skipped
    for i in 0 to FETCH_WIDTH-1 loop        
        regularJump := '0';
        longJump := '0';
        regJump := '0';
        
        if 	fetchLine(i)(31 downto 26) = opcode2slv(jl) then
            regularJump := '1';				
            predictedTaken(i) := '1';       -- CAREFUL, TODO: temporary predicted taken iff backwards
            uncondJump(i) := '1';		    
        elsif
             fetchLine(i)(31 downto 26) = opcode2slv(jz) 
            or fetchLine(i)(31 downto 26) = opcode2slv(jnz)
        then
            regularJump := '1';				
            predictedTaken(i) := fetchLine(i)(20);		-- CAREFUL, TODO: temporary predicted taken iff backwards
        elsif fetchLine(i)(31 downto 26) = opcode2slv(j) then -- Long jump instruction     
            uncondJump(i) := '1';
            longJump := '1';				
            predictedTaken(i) := '1'; -- Long jump is unconditional (no space for register encoding!)
        elsif  fetchLine(i)(31 downto 26) = opcode2slv(ext1) 
            and (fetchLine(i)(15 downto 10) = opcont2slv(ext1, jzR)
                 or fetchLine(i)(15 downto 10) = opcont2slv(ext1, jnzR)) then
            regJump := '1';
            predictedTaken(i) := '0'; -- TEMP: register jumps predicted not taken
        end if;
        
        if longJump = '1' then
            tempOffset := (others => fetchLine(i)(25));
            tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
        else
            tempOffset := (others => fetchLine(i)(20));
            tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);
        end if;

        branchIns(i) := regularJump or longJump or regJump;
        res(i).ins.target := addMwordFaster(res(i).ins.ip, tempOffset);			
    end loop;
    
    -- Find if any branch predicted
    for i in 0 to FETCH_WIDTH-1 loop
        fullOut(i) := full(i);
        res(i).ins.classInfo.branchIns := branchIns(i);
        if full(i) = '1' and branchIns(i) = '1' and predictedTaken(i) = '1' then
            if uncondJump(i) = '1' then -- CAREFUL: setting it here, so that if implementation treats is as NOP in Exec, it still gets this flag
                res(i).ins.controlInfo.confirmedBranch := '1';
            end if;

            res(i).ins.controlInfo.frontBranch := '1';					
            
            -- Here check if the next line from line predictor agrees with the target predicted now.
            --	If so, don't cause the event but set invalidation mask that next line will use.
            if res(i).ins.target(MWORD_SIZE-1 downto ALIGN_BITS) /= ins.target(MWORD_SIZE-1 downto ALIGN_BITS) then
                res(i).ins.controlInfo.newEvent := '1';
            end if;
            
            -- CAREFUL: When not using line predictor, branches predicted taken must always be done here 
            if not USE_LINE_PREDICTOR then
                res(i).ins.controlInfo.newEvent := '1';
            end if;

            exit;
        end if;
    end loop;

	for i in 0 to FETCH_WIDTH-1 loop
	   res(i).full := fullOut(i);
	end loop;
	return res;
end function;



function getFrontEventMulti2(predictedAddress: Mword;
							  ins: InstructionState; fetchLine: WordArray(0 to FETCH_WIDTH-1))
return InstructionSlotArray is
	variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	variable tempOffset: Mword := (others => '0');
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

        res(i).ins.ip := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(i*4, ALIGN_BITS);        
        res(i).ins.result := ins.ip;
        res(i).ins.result(ALIGN_BITS-1 downto 0) := i2slv((i+1)*4, ALIGN_BITS); -- CAREFUL: not for short ins
	end loop;
	res(PIPE_WIDTH-1).ins.result := ins.ip(MWORD_SIZE-1 downto ALIGN_BITS) & i2slv(0, ALIGN_BITS);
	res(PIPE_WIDTH-1).ins.result := addMwordBasic(res(PIPE_WIDTH-1).ins.result, PC_INC);

    -- Calculate target for each instruction, even if it's to be skipped
    for i in 0 to FETCH_WIDTH-1 loop        
        regularJump := '0';
        longJump := '0';
        regJump := '0';
        
            -- jmp reg has src1
            -- jmp cnd has src0
            -- jmp lnk has dest
            -- jmp lng has none
        
--        if res(i).ins.classInfo.branchIns = '1' then
--            if res(i).ins.virtualArgSpec.intArgSel(1) = '1' then
--                -- reg
--                regJump := '1';
--                predictedTaken(i) := '0'; -- TEMP: register jumps predicted not taken                
--            elsif res(i).ins.virtualArgSpec.intArgSel(0) = '1' then
--                -- cond
--                regularJump := '1';				
--                predictedTaken(i) := fetchLine(i)(20);        -- CAREFUL, TODO: temporary predicted taken iff backwards
--            elsif res(i).ins.virtualArgSpec.intDestSel = '1' then
--                -- link
--                regularJump := '1';				
--                predictedTaken(i) := '1';       -- CAREFUL, TODO: temporary predicted taken iff backwards
--                uncondJump(i) := '1';                
--            else
--               -- long
--               regularJump := '1';				
--               predictedTaken(i) := '1';       -- CAREFUL, TODO: temporary predicted taken iff backwards
--               uncondJump(i) := '1';
--            end if;
            
--        end if;

            if 	fetchLine(i)(31 downto 26) = opcode2slv(jl) then
                regularJump := '1';				
                predictedTaken(i) := '1';       -- CAREFUL, TODO: temporary predicted taken iff backwards
                uncondJump(i) := '1';		    
            elsif
                 fetchLine(i)(31 downto 26) = opcode2slv(jz) 
                or fetchLine(i)(31 downto 26) = opcode2slv(jnz)
            then
                regularJump := '1';				
                predictedTaken(i) := fetchLine(i)(20);		-- CAREFUL, TODO: temporary predicted taken iff backwards
            elsif fetchLine(i)(31 downto 26) = opcode2slv(j) then -- Long jump instruction     
                uncondJump(i) := '1';
                longJump := '1';				
                predictedTaken(i) := '1'; -- Long jump is unconditional (no space for register encoding!)
            elsif  fetchLine(i)(31 downto 26) = opcode2slv(ext1) 
                and (fetchLine(i)(15 downto 10) = opcont2slv(ext1, jzR)
                     or fetchLine(i)(15 downto 10) = opcont2slv(ext1, jnzR)) then
                regJump := '1';
                predictedTaken(i) := '0'; -- TEMP: register jumps predicted not taken
            end if;
            
            if longJump = '1' then
                tempOffset := (others => fetchLine(i)(25));
                tempOffset(25 downto 0) := fetchLine(i)(25 downto 0);
            else
                tempOffset := (others => fetchLine(i)(20));
                tempOffset(20 downto 0) := fetchLine(i)(20 downto 0);
            end if;
    
            branchIns(i) := regularJump or longJump or regJump;
            res(i).ins.target := addMwordFaster(res(i).ins.ip, tempOffset);			
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
                res(i).ins.controlInfo.newEvent := '1';
            end if;
            
            -- CAREFUL: When not using line predictor, branches predicted taken must always be done here 
            if not USE_LINE_PREDICTOR then
                res(i).ins.controlInfo.newEvent := '1';
            end if;

            exit;
        end if;
    end loop;

	for i in 0 to FETCH_WIDTH-1 loop
	   res(i).full := fullOut(i);
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

function prepareForBQ(insVec: InstructionSlotArray; branchMask: std_logic_vector) return InstructionSlotArray is
	variable res: InstructionSlotArray(insVec'range) := insVec;
	variable result, target: Mword;
begin
	for i in insVec'range loop
	   res(i).full := branchMask(i) and insVec(i).full;-- and not insVec(i).ins.controlInfo.skipped;
		
       -- This is like in BQ because this block of dta goes just there
       if CLEAR_DEBUG_INFO then         
           res(i).ins.ip := (others => '0');
           res(i).ins.bits := (others => '0');
        --    res(i).classInfo := DEFAULT_CLASS_INFO;              
                         
           --res(slv2u(endPtr)).ops(i).ins.result := (others => '0');
           --res(slv2u(endPtr)).ops(i).ins.target := (others => '0');
            
           res(i).ins.constantArgs := DEFAULT_CONSTANT_ARGS;
           res(i).ins.virtualArgSpec := DEFAULT_ARG_SPEC;
           res(i).ins.physicalArgSpec := DEFAULT_ARG_SPEC;
         
           res(i).ins.operation := (System, sysUndef);

           res(i).ins.tags.fetchCtr := (others => '0');
           res(i).ins.tags.decodeCtr := (others => '0');
           res(i).ins.tags.renameCtr := (others => '0');
           
           -- TODO: ptrs may be better here than go through IQ!
           res(i).ins.tags.intPointer := (others => '0');
           res(i).ins.tags.floatPointer := (others => '0');

           res(i).ins.tags.commitCtr := (others => '0');		
	   end if;
	end loop;
	
	return res;
end function;

end LogicFront;
