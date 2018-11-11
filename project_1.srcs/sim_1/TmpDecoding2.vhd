--
--	Package File Template
--

--

library IEEE;
use IEEE.STD_LOGIC_1164.all;
  
use work.BasicTypes.all;
use work.ArchDefs.all;

use work.InstructionState.all;

package TmpDecoding2 is
	
		type InsFieldNew is (opcode, opcont, none,  qa, qb, qc, qd, zero, one, 
									leftImm, imm10, imm16, imm21, imm26
									);		
	
		type InsFieldTableNew is array (InsFieldNew) of integer;
		type InsFieldTableNewW is array (InsFieldNew) of word;	
	
		subtype QuintetSrc is InsFieldNew range none to one; 	
	
		type ImmFormat is (imm26, imm21, imm16, imm10, none);
	
		-- TODO: Standardize jump conditions and handling them
		constant COND_NONE: slv5 := "11111";
		constant COND_Z: slv5 := "00000";
		constant COND_NZ: slv5 := "00001";


		type QuintetArgName is (d0, d1, s0, s1, s2, c0, c1);
		type QuintetSrcArray is array (QuintetArgName range d0 to c1) of QuintetSrc; 
		type QuintetValArray is array (QuintetArgName range d0 to c1) of slv5;
		type QuintetSelect is array (QuintetArgName range d0 to c1) of std_logic;


			type ExecDomain is (none, Int, Float);
			type ExecDomainArray is array (integer range <>) of ExecDomain;
	
			type RegisterSpec is record
				place:  QuintetSrc;
				domain: ExecDomain; 
			end record;

				type ArgFormatStruct is record
					quintets: 	QuintetSrcArray;
					immSize: 	ImmFormat;
					immSign: 	std_logic;
					hasLeftImm: std_logic;
					leftImmSign: std_logic;
						regDomains: ExecDomainArray(0 to 3); 				
				end record;
									
						type ArgFormatNew is record
							destSpec: RegisterSpec;
							src0Spec: RegisterSpec;
							src1Spec: RegisterSpec;
							src2Spec: RegisterSpec;
								c0Spec: RegisterSpec; -- TEMP!
								c1Spec: RegisterSpec; -- 	-||-
							immSel:		std_logic;
							immSign:		std_logic;
							immSize:		ImmFormat;
						end record;

		-- result would be like this:
		type OpFieldStruct is record
			opcode:			ProcOpcode;
			opcont: 			ProcOpcont;	
				unit:		ExecUnit;
				func:		ExecFunc;
			quintetSel:		QuintetSelect;
			quintetValues: QuintetValArray;
			hasLeftImm: 	std_logic;
			leftImm: 		word;
			hasImm: 			std_logic;
			imm: 				word;
			target: 			word;
		end record;		

				type OpFieldStructW is record
					opcode:			slv6;
					opcont: 			slv6;	
						unit:		ExecUnit;
						func:		ExecFunc;
					quintetSel:		QuintetSelect;
					quintetValues: QuintetValArray;
					hasLeftImm: 	std_logic;
					leftImm: 		word;
					hasImm: 			std_logic;
					imm: 				word;
					target:			word;
				end record;	


		type InsDefNew is record
			opcd: ProcOpcode;
			opct: ProcOpcont;
			unit: ExecUnit;
			func: ExecFunc;
			fmt:	ArgFormatStruct;	
		end record;
		
		type InsDefArrayNew is array (natural range <>) of InsDefNew;

		
		type InsDefNewW is record
			opcd: slv6;
			opct: slv6;
			unit: ExecUnit;
			func: ExecFunc;
			fmt:	ArgFormatStruct;	
		end record;
		
		type InsDefArrayNewW is array (natural range <>) of InsDefNewW;		
		
		function parseWordNewW(w: word) return InsFieldTableNewW;

			constant fmtUndef: ArgFormatStruct :=
					((others=>none), none, '0','0','0',
							(others => Int));

				-- TEMP: experimental new format
				constant formatImm: ArgFormatNew := 
					((qa, Int), 	(qb, Int), (qc, none), (qd, none),
						(none, none), (none, none),	'1', '0', imm16);

			constant fmtImm: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>none, s2=>none, c0=>none, c1=>none), imm16, '0', '0', '0',
							(others => Int));
			constant fmtReg2: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>qc, s2=>none, c0=>none, c1=>none), none, '0', '0', '0',
							(others => Int));
			constant fmtReg3: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>qc, s2=>qd, c0=>none, c1=>none), none, '0', '0', '0',
							(others => Int));					

			constant fmtRegFP2: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>qc, s2=>none, c0=>none, c1=>none), none, '0', '0', '0',
							(others => Float));
			constant fmtRegFP3: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>qc, s2=>qd, c0=>none, c1=>none), none, '0', '0', '0',
							(others => Float));	

			constant fmtLoadImm: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>none, s2=>none, c0=>none, c1=>none), imm10, '1', '0', '0',
							(others => Int));
			constant fmtStoreImm: ArgFormatStruct := 
					((d0=>zero, d1=>none, s0=>qb, s1=>none, s2=>qa, c0=>none, c1=>none), imm10, '1', '0', '0',
							(others => Int));					

			constant fmtLoadImmFP: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>none, s2=>none, c0=>none, c1=>none), imm10, '1', '0', '0',
							(Float, Int, Int, Int));
			constant fmtStoreImmFP: ArgFormatStruct := 
					((d0=>zero, d1=>none, s0=>qb, s1=>none, s2=>qa, c0=>none, c1=>none), imm10, '1', '0', '0',
							(Int, Int, Int, Float));

			-- Careful! In c1 we have to put condition! Temporary: zero Z, one NZ (but wont translate properly!)
				--								Let 'none' mean jumping always (to distinguish certain from conditional)
			constant fmtJumpLong: ArgFormatStruct := 
					((d0=>zero, d1=>none, s0=>zero, s1=>none, s2=>none, c0=>none, c1=> none), imm26, '1', '0', '0',
							(others => Int));					
			constant fmtJumpLink: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>zero, s1=>none, s2=>none, c0=>none, c1=> none), imm21, '1', '0', '0',
							(others => Int));					
			constant fmtJumpCondZ: ArgFormatStruct := 
					((d0=>zero, d1=>none, s0=>qa, s1=>none, s2=>none, c0=>none, c1=> zero), imm21, '1', '0', '0',
							(others => Int));					
			constant fmtJumpCondNZ: ArgFormatStruct := 
					((d0=>zero, d1=>none, s0=>qa, s1=>none, s2=>none, c0=>none, c1=> one), imm21, '1', '0', '0',
							(others => Int));					
			constant fmtJumpRZ: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>qc, s2=>none, c0=>none, c1=> zero), none, '0', '0', '0',
							(others => Int));					
			constant fmtJumpRNZ: ArgFormatStruct := 
					((d0=>qa, d1=>none, s0=>qb, s1=>qc, s2=>none, c0=>none, c1=> one), none, '0', '0', '0',
							(others => Int));	
				
			constant fmtShiftImm: ArgFormatStruct :=		
					((d0=>qa, d1=>none, s0=>qb, s1=>none, s2=>none, c0=>none, c1=>none), imm10, '0', '0', '0',
							(others => Int));
			
			constant fmtNoArgs: ArgFormatStruct :=		
					((others=>none), none, '0', '0', '0',
							(others => Int));			
			
				constant fmtMFC_TEMP: ArgFormatStruct := fmtLoadImm;
				constant fmtMTC_TEMP: ArgFormatStruct := fmtStoreImm;
		
		constant undefInsDef: InsDefNewW := (opcode2slv(ext2), opcont2slv(ext2, undef),
																				System, sysUndef, fmtUndef);
		
		constant decodeTableNew: InsDefArrayNew(0 to 31) := (
				0 => (andI, none, Alu, logicAnd, fmtImm),
				1 => (orI,  none, Alu, logicOr,  fmtImm),
				2 => (addI, none, Alu, arithAdd, fmtImm),
				3 => (subI, none, Alu, arithSub, fmtImm),
				
				4 => (ext1, load,	Memory,load,	fmtLoadImm),
				5 => (ext1, store,Memory,store,	fmtStoreImm),

				6 => (j, 	none, Jump, jump, fmtJumpLong),
				7 => (jl, 	none, Jump, jump,	fmtJumpLink),
				8 => (jz, 	none, Jump, jump, fmtJumpCondZ),
				9 => (jnz, 	none, Jump, jump, fmtJumpCondNZ),
				
				10=> (ext0, muls, Mac, mulS, fmtReg3),
				11=> (ext0, mulu, Mac, mulU, fmtReg3),

				12 => (ext0, shlC,  Alu,  logicShl,	fmtShiftImm),
				13 => (ext0, shrlC, Alu,  logicShrl,fmtShiftImm),
				14 => (ext0, shraC, Alu,  arithShra,fmtShiftImm), 

				15=> (ext2, mfc,	System, sysMFC, fmtMFC_TEMP),
				16=> (ext2, mtc, 	System, sysMTC, fmtMTC_TEMP),		
							
				17=> (ext0, addR, Alu, arithAdd, fmtReg2),
				18=> (ext0, subR, Alu, arithSub, fmtReg2),
				19=> (ext0, andR, Alu, logicAnd, fmtReg2),
				20=> (ext0, orR,  Alu, logicOr,  fmtReg2),
					
				21=> (ext1, jzR,  Jump, jump, fmtJumpRZ),
				22=> (ext1, jnzR, Jump, jump, fmtJumpRNZ),

					23 => (ext1, loadFP,		Memory,load,	fmtLoadImmFP),
					24 => (ext1, storeFP,	Memory,store,	fmtStoreImmFP),
				
					25 => (ext2, halt, System, sysHalt, fmtNoArgs),
					26 => (ext2, retI, System, sysRetI, fmtNoArgs),
					27 => (ext2, retE, System, sysRetE, fmtNoArgs),
					28 => (ext2, sync,	System, sysSync,	 fmtNoArgs),
					29 => (ext2, replay, System, sysReplay, fmtNoArgs),
					30 => (ext2, error,  System, sysError, fmtNoArgs),
				
				others => (ext2, undef, System, sysUndef, fmtUndef)
				);
		
			function toTableW(dt: InsDefArrayNew) return InsDefArrayNewW;		
			constant decodeTableNewW: InsDefArrayNewW(decodeTableNew'range) := toTableW(decodeTableNew);
		
		
		function TEMP_getLargeTable(decodeTableNewW: InsDefArrayNewW) return InsDefArrayNewW;
		
		constant TEMP_largeTable: InsDefArrayNewW(0 to 4095) := TEMP_getLargeTable(decodeTableNewW);
		
		function TEMP_find(opcd, opct: slv6) return InsDefNewW;
		
			function getOpFields(w: word) return OpFieldStruct;

			-- WARNING: classInfo may be actually created at later part, here being just a placeholder 
			procedure ofsInfo(ofs: in OpFieldStruct;
									op: out BinomialOp;
									ci: out InstructionClassInfo;
									ca: out InstructionConstantArgs;
									va: out InstructionVirtualArgs;
									vda: out InstructionVirtualDestArgs);

function decodeInstruction(inputState: InstructionState) return InstructionState;
		
end TmpDecoding2;


package body TmpDecoding2 is
	 
	procedure ofsInfo(ofs: in OpFieldStruct;
							op: out BinomialOp;
							ci: out InstructionClassInfo;
							ca: out InstructionConstantArgs;
							va: out InstructionVirtualArgs;
							vda: out InstructionVirtualDestArgs)
	is		
		variable num: integer;
	begin		
		vda.sel(0) := ofs.quintetSel(d0);	
		vda.d0 := ofs.quintetValues(d0);
		
		va.sel := ofs.quintetSel(s0) & ofs.quintetSel(s1) & ofs.quintetSel(s2);
		va.s0 := ofs.quintetValues(s0);
		va.s1 := ofs.quintetValues(s1);
		va.s2 := ofs.quintetValues(s2);
		
		ca.immSel := ofs.hasImm;
		ca.imm := ofs.imm;
		ca.c0 := ofs.quintetValues(c0);
		ca.c1 := ofs.quintetValues(c1);

		op := BinomialOp'(ofs.unit, ofs.func); 					

		-- WARNING: classInfo will be determined by HW-specific part.
	end procedure;

		------------------------------------------
		function e32(v: std_logic_vector) return word is
			variable res: word := (others=>'0');
			variable a, b: integer;
		begin
			a := v'high;
			b := v'low;
			res(a-b downto 0) := v;
			return res;
		end function;
		

		function parseWordNewW(w: word) return InsFieldTableNewW is
			variable res: InsFieldTableNewW := (others=>(others=>'0'));
		begin
			res := (none => (others=>'1'),
					  opcode => e32(w(31 downto 26)),
					  opcont => e32(w(15 downto 10)),
					  leftImm => e32(w(26 downto 16)),
					  --xr => slv2u(w(9 downto 0)),
					  qa => e32(w(25 downto 21)),
					  qb => e32(w(20 downto 16)),
					  qc => e32(w(9 downto 5)),
					  qd => e32(w(4 downto 0)),
					  imm26 => w,
					  imm21 => e32(w(20 downto 0)),
					  imm16 => e32(w(15 downto 0)),
					  imm10 => e32(w(9 downto 0)),
					  zero => (others=>'0'),
					  one => (0=>'1', others=>'0')
					  );
			return res;
		end function;
		


		function getOpFieldStructW(tab: InsFieldTableNewW;-- fmt: ArgFormatStruct; 
											idef: InsDefNewW)
		return OpFieldStruct is
			variable res: OpFieldStruct;
			variable fmt: ArgFormatStruct := idef.fmt;
			variable iw: word := tab(imm26);
		begin
			res.opcode := slv2opcode(tab(opcode)(5 downto 0));
			res.opcont := slv2opcont(tab(opcode)(5 downto 0), tab(opcont)(5 downto 0));

			-- Dispatch quintets
			for i in d0 to c1 loop
				if fmt.quintets(i) = none then
					res.quintetSel(i) := '0';
					res.quintetValues(i) := "00000";
				elsif fmt.quintets(i) = zero then
					res.quintetSel(i) := '1';
					res.quintetValues(i) := "00000";
				elsif fmt.quintets(i) = one then	
					res.quintetSel(i) := '1';
					res.quintetValues(i) := "00001";					
				else
					res.quintetSel(i) := '1';
					res.quintetValues(i) := tab(fmt.quintets(i))(4 downto 0);
				end if;	
			end loop;
			
			-- Handle imm value (and leftImm)		
			res.imm := iw;
			res.target := iw;
			
			if fmt.immSize = none then
				res.hasImm := '0';
			else
				res.hasImm := '1';
			end if;
			
			if fmt.immSize = imm10 then
				res.imm(31 downto 10) := (others => iw(9) and fmt.immSign);
			else
				res.imm(31 downto 16) := (others => iw(15) and fmt.immSign);
			end if;

			if fmt.immSize = imm26 then
				res.target(31 downto 26) := (others => iw(25) and fmt.immSign);
			else
				res.target(31 downto 21) := (others => iw(20) and fmt.immSign);
			end if;
			
--			case fmt.immSize is
--				when none =>
--					--res.hasImm := '0';
--					res.imm := (others => '0');
--				when imm10 =>
--					--res.hasImm := '1';
--					res.imm(31 downto 10) := (others => iw(9) and fmt.immSign);	
--				when imm16 =>
--					--res.hasImm := '1';		
--					res.imm(31 downto 16) := (others => iw(15) and fmt.immSign);
--				when imm21 =>
--					--res.hasImm := '1';	
--					res.imm(31 downto 21) := (others => iw(20) and fmt.immSign);
--				when imm26 =>
--					--res.hasImm := '1';	
--					res.imm(31 downto 26) := (others => iw(25) and fmt.immSign);					
--				when others =>
--					report "bad imm format specification" severity error;
--			end case;

			if fmt.hasLeftImm = '1' then
				res.hasLeftImm := '1';
				iw := tab(leftImm);
				res.leftImm := iw;
				res.leftImm(31 downto 10) := (others => iw(10) and fmt.leftImmSign);
			else
				res.hasLeftImm := '0';
				res.leftImm := (others => '0');
			end if;
			
			res.unit := idef.unit;
			res.func := idef.func;
			
			return res;
		end function;

		
		function findInstructionNewW(opcode, opcont: slv6) return integer is
			variable tempInt: integer;
		begin
			tempInt := decodeTableNewW'right;
			for i in decodeTableNewW'range loop
				if 		decodeTableNewW(i).opcd = opcode 
					and (not hasOpcont(slv2opcode(opcode)) or (decodeTableNewW(i).opct = opcont))
				then
					return i;
				end if;
			end loop;
			return tempInt;
		end function;

		function getOpFields(w: word) return OpFieldStruct is
			variable num: integer;
			variable parts: InsFieldTableNewW;
			variable ofs: OpFieldStruct;
			variable opcd: slv6; --ProcOpcode;
			variable opct: slv6; --ProcOpcont;
			variable match: InsDefNewW;			
		begin
			parts := parseWordNewW(w);
			opcd := parts(opcode)(5 downto 0);
			opct := parts(opcont)(5 downto 0);	
						
			if true then --
				--	false then
				-- This activates "large table" decoding
				match := TEMP_find(opcd, opct);
				ofs := getOpFieldStructW(parts, match);
			else
				num := findInstructionNewW(opcd, opct);
				match := decodeTableNewW(num);
				ofs := getOpFieldStructW(parts, decodeTableNewW(num));						
			end if;

			return ofs;
		end function;


		function toTableW(dt: InsDefArrayNew) return InsDefArrayNewW is
			variable res: InsDefArrayNewW(dt'range);
		begin
			for i in dt'range loop
				res(i) := (opcode2slv(dt(i).opcd), opcont2slv(dt(i).opcd, dt(i).opct),
								dt(i).unit, dt(i).func, dt(i).fmt);
			end loop;		
			return res;
		end function;


function TEMP_getLargeTable(decodeTableNewW: InsDefArrayNewW) return InsDefArrayNewW is
	variable res: InsDefArrayNewW(0 to 4095) := (others => undefInsDef);
	variable currentDef: InsDefNewW := undefInsDef;
	variable fullOpcode: std_logic_vector(0 to 11);
	variable index: natural := 0;
begin
	for i in decodeTableNewW'range loop
		currentDef := decodeTableNewW(i);
		fullOpcode(0 to 5) := currentDef.opcd;
		-- CAREFUL! If this opcode has no opcont, all possible bits on opcont position must lead to 
		--				the same definition!
		if hasOpcont(slv2opcode(currentDef.opcd)) then
			fullOpcode(6 to 11) := currentDef.opct;
			index := slv2u(fullOpcode);
			res(index) := currentDef;		
		else 
			for j in 0 to 63 loop
				fullOpcode(6 to 11) := i2slv(j, 6);
				index := slv2u(fullOpcode);
				res(index) := currentDef;				
			end loop;
		end if;
	end loop;
	
	return res;
end function;


function TEMP_find(opcd, opct: slv6) return InsDefNewW is
	variable fullOpcode: std_logic_vector(0 to 11);
	variable index: natural := 0;	
begin
	fullOpcode(0 to 5) := opcd;
	fullOpcode(6 to 11) := opct;
	index := slv2u(fullOpcode);
	return TEMP_largeTable(index);
end function;



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
			if 	 	(ins.operation.func = jump and ins.constantArgs.c1 = COND_NONE) then
				null;
			elsif (ins.operation.func = jump and ins.constantArgs.c1 /= COND_NONE) then 
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
	variable ofs: OpFieldStruct;
	variable tmpVirtualArgs: InstructionVirtualArgs;
	variable tmpVirtualDestArgs: InstructionVirtualDestArgs;
begin
	ofs := getOpFields(inputState.bits);
	ofsInfo(ofs,
					res.operation,
					res.classInfo,
					res.constantArgs,
					tmpVirtualArgs,
					tmpVirtualDestArgs);
	
		res.virtualArgSpec.intDestSel := tmpVirtualDestArgs.sel(0);
		res.virtualArgSpec.floatDestSel := '0';
		res.virtualArgSpec.dest := (others => '0');		
		res.virtualArgSpec.dest(4 downto 0) := tmpVirtualDestArgs.d0;
		res.virtualArgSpec.intArgSel := tmpVirtualArgs.sel;
		res.virtualArgSpec.floatArgSel := (others => '0');
		res.virtualArgSpec.args(0) := (others => '0');
		res.virtualArgSpec.args(0)(4 downto 0) := tmpVirtualArgs.s0;
		res.virtualArgSpec.args(1) := (others => '0');		
		res.virtualArgSpec.args(1)(4 downto 0) := tmpVirtualArgs.s1;
		res.virtualArgSpec.args(2) := (others => '0');
		res.virtualArgSpec.args(2)(4 downto 0) := tmpVirtualArgs.s2;
	
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
		res.target := ofs.target;
	return res;
end function;


end TmpDecoding2;
