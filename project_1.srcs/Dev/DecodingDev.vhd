----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.InstructionSet.all;

use work.InstructionState.all;


package DecodingDev is

procedure decodeFromWord(w: in Word; classInfo: out InstructionClassInfo; oOp: out SpecificOp; oConstantArgs: out InstructionConstantArgs; oArgSpec: out InstructionArgSpec);

end package;


package body DecodingDev is


function decodeOperation(op0, op1: slv6; op2: slv5) return SpecificOp is
    variable specificOperation: SpecificOp := DEFAULT_SPECIFIC_OP;
    
    variable isUndef: boolean := false;
    variable isAluOp, isFpOp, isMemOp, isSysOp: boolean := false;
    variable aluOp: ArithOp; 
    variable floatOp: FpOp; 
    variable memoryOp: MemOp; 
    variable systemOp: SysOp; 
begin

     isAluOp :=        op0 = "000000"
                or  op0 = "000101"     
                or  op0 = "001000"     
                or  op0 = "001001"
                or  op0 = "001010"
                or  op0 = "001011"
                or  op0 = "010000"
                or  op0 = "010001";
                
     isFpOp :=        op0 = "000001";
    
     isMemOp :=      op0 = "000010"
               or  op0 = "000011"     
               or  op0 = "000100"     
               or  op0 = "010100"
               or  op0 = "010101"
               or  op0 = "010110"
               or  op0 = "010111";    
    
     isSysOp :=     op0 = "000111";
     
     isUndef := not (isAluOp or isFpOp or isMemOp or isSysOp);

    -- 

    if op0 = "010000" then
        aluOp := opAdd;
    elsif op0 = "010001" then
        aluOp := opAddH;
    elsif op0 = "001000" then
        aluOp := opJ;
    elsif op0 = "001001" then
        aluOp := opJ;        
    elsif op0 = "001010" then
        aluOp := opJz;        
    elsif op0 = "001011" then
        aluOp := opJnz;        
    
    elsif op0 = "000000" then
        
        if op1 = "000000" then
            if op2 = "00000" then
                aluOp := opAnd;
            elsif op2 = "00001" then
                aluOp := opOr;
            else 
                isUndef := true;
            end if;
        elsif op1 = "000001" then
            if op2 = "00000" then
                aluOp := opAdd;
            elsif op2 = "00001" then
                aluOp := opSub;
            else 
                isUndef := true;
            end if;
        elsif op1 = "000010" then
            if op2 = "00000" then
                aluOp := opJz;
            elsif op2 = "00001" then
                aluOp := opJnz;
            else 
                isUndef := true;
            end if;
        elsif op1 = "000011" then
            if op2 = "00000" then
                aluOp := opMul;
            elsif op2 = "00001" then
                aluOp := opMulHU;
            elsif op2 = "00010" then
                aluOp := opMulHS;
            elsif op2 = "01000" then
                aluOp := opDivU;
            elsif op2 = "01001" then
                aluOp := opDivS;
            elsif op2 = "01010" then
                aluOp := opRemU;
            elsif op2 = "01011" then
                aluOp := opRemS;
            else 
                isUndef := true;
            end if;
         else
            isUndef := true;
        end if;

    elsif op0 = "000101" then
        
        if op1 = "000000" then
            aluOp := opShl;
        elsif op1 = "000001" then
            aluOp := opSha;
        elsif op1 = "000010" then
            aluOp := opRot;
        else
            isUndef := true;
        end if;
        
    end if;
    
    -- Mem
    if op0 = "010100" or op0 = "010110" then
        memoryOp := opLoad;    
    elsif op0 = "010101" or op0 = "010111" then
        memoryOp := opStore;
    elsif op0 = "000010" then 
        isUndef := true;
    elsif op0 = "000011" then 
        isUndef := true;
    elsif op0 = "000100" then 
        if op1 = "000000" then
            memoryOp := opLoadSys;
        elsif op1 = "100000" then
            memoryOp := opStoreSys;
        else
            isUndef := true; 
        end if;
    end if;
    
    -- Fp
    if op0 = "000001" then
        if op1 = "000000" and op2 = "00000" then
            floatOp := opMove;
        else
            isUndef := true;
        end if;
    end if;
    
    -- Sys
    if op0 = "000111" then
        if op1 = "000001" then
            systemOp := opError;        
        elsif op1 = "000010" then
            systemOp := opCall;        
        elsif op1 = "000011" then
            systemOp := opSync;        
        elsif op1 = "000100" then
            systemOp := opReplay;        
        elsif op1 = "000101" then
            systemOp := opHalt;        
        elsif op1 = "000110" then
            systemOp := opSend;        
        elsif op1 = "000111" then
            systemOp := opRetE;        
        elsif op1 = "001000" then
            systemOp := opRetI;
        else
            isUndef := true;      
        end if;
    end if;
    
    
    if isUndef then
        specificOperation := sop(None, opUndef);
    elsif isAluOp then
        specificOperation := sop(ALU, aluOp);
    elsif isFpOp then
        specificOperation := sop(FP, floatOp);
    elsif isMemOp then
        specificOperation := sop(Mem, memoryOp);
    else
        specificOperation := sop(None, systemOp);
    end if;
    
    return specificOperation;
end function;


function checkOp0(op0: slv6; table: Dword) return std_logic is
begin
    return table(slv2u(op0));
end function;

function checkOp1(op0, opRef0, op1: slv6; table: Dword) return std_logic is
begin
    return bool2std(op0 = opRef0) and table(slv2u(op1));
end function;


procedure decodeFromWord(w: in Word; classInfo: out InstructionClassInfo; oOp: out SpecificOp; oConstantArgs: out InstructionConstantArgs; oArgSpec: out InstructionArgSpec) is
    variable specificOperation: SpecificOp := DEFAULT_SPECIFIC_OP;
    variable constantArgs: InstructionConstantArgs := DEFAULT_CONSTANT_ARGS;

    variable op: SpecificOp := DEFAULT_SPECIFIC_OP;
    variable argSpec: InstructionArgSpec := DEFAULT_ARG_SPEC;

    constant op0: slv6 := w(31 downto 26);
    constant op1: slv6 := w(15 downto 10);
    constant op2: slv5 := w(4 downto 0);
    
    constant qa: slv5 := w(25 downto 21);
    constant qb: slv5 := w(20 downto 16);
    constant qc: slv5 := w(9 downto 5);
    constant qd: slv5 := w(4 downto 0);
    
    variable    hasOp1, hasOp2,
                isBranch, isStore, isLoad, hasImm26, hasImm21, hasImm16, hasImm10, hasFpDest, hasIntDest,
                src2a, src0a,
                intSrc0, intSrc1, intSrc2,
                fpSrc0, fpSrc1, fpSrc2: boolean := false;                
begin
    isBranch := std2bool(    checkOp0(op0, OP0_JUMP)
                          or checkOp1(op0, "000000", op1, OP1_INT_ARITH_BRANCH));
    hasImm26 := std2bool(checkOp0(op0, OP0_IMM26));
    
    hasImm21 := std2bool(checkOp0(op0, OP0_IMM21));

    hasImm16 := std2bool(checkOp0(op0, OP0_IMM16));
    
    hasImm10 := std2bool(checkOp0(op0, OP0_IMM10));

    hasFpDest := std2bool(checkOp0(op0, OP0_FLOAT_DEST) or checkOp1(op0, "000011", op1, OP1_FLOAT_MEM_LOAD));

    hasIntDest := std2bool(
                           checkOp0(op0, OP0_INT_DEST)
                        or checkOp1(op0, "000010", op1, OP1_INT_MEM_LOAD)
                        or checkOp1(op0, "000100", op1, OP1_SYS_MEM_LOAD)
                        );
    
    src2a := std2bool(checkOp0(op0, OP0_2A) or checkOp1(op0, "000100", op1, OP1_SYS_MEM_STORE));                
    
    src0a := std2bool(checkOp0(op0, OP0_0A));
    
    intSrc0 := std2bool(checkOp0(op0, OP0_INT_SRC0));
            
    intSrc1 := std2bool(checkOp0(op0, OP0_INT_SRC1));
    
    intSrc2 := std2bool(    checkOp0(op0, OP0_INT_SRC2)
                         or checkOp1(op0, "000010", op1, OP1_INT_MEM_STORE)
                         or checkOp1(op0, "000100", op1, OP1_SYS_MEM_STORE));

    fpSrc0  := std2bool(checkOp0(op0, OP0_FLOAT_ARITH));
    fpSrc1  := std2bool(checkOp1(op0, "000001", op1, OP1_FLOAT_ARITH_SRC1));
                      
    fpSrc2 := std2bool(    checkOp0(op0, OP0_FLOAT_SRC2)
                       or checkOp1(op0, "000001", op1, OP1_FLOAT_ARITH_SRC2));    

    isStore := std2bool(    checkOp0(op0, OP0_STORE)
                         or checkOp1(op0, "000010", op1, OP1_INT_MEM_STORE)
                         or checkOp1(op0, "000011", op1, OP1_FLOAT_MEM_STORE)
                         or checkOp1(op0, "000100", op1, OP1_SYS_MEM_STORE));

    isLoad := std2bool(    checkOp0(op0, OP0_LOAD)
                         or checkOp1(op0, "000010", op1, OP1_INT_MEM_LOAD)
                         or checkOp1(op0, "000011", op1, OP1_FLOAT_MEM_LOAD)
                         or checkOp1(op0, "000100", op1, OP1_SYS_MEM_LOAD));
     
    op := decodeOperation(op0, op1, op2);
    
    classInfo.branchIns := bool2std(isBranch);
    classInfo.mainCluster := bool2std(op0 /= "000111"); -- !!
    classInfo.secCluster := bool2std(isStore);
    classInfo.useLQ := bool2std(isLoad);
    classInfo.useSQ := bool2std(isStore);
    classInfo.useFP := bool2std(hasFpDest or fpSrc0 or fpSrc1 or fpSrc2);

    if op.subpipe = none then
        classInfo.mainCluster := '0';
        classInfo.secCluster := '0';
    end if;

    -- assign register definitions
    argSpec.dest := "000" & qa;
    if hasIntDest and isNonzero(argSpec.dest) = '1' then
        argSpec.intDestSel := '1';
    elsif hasFpDest then
        argSpec.floatDestSel := '1';
    else -- When none selected, set 0
        --res.virtualArgSpec.dest := (others => '0');
    end if;
    
    if src0a then
       argSpec.args(0) := "000" & qa;
    else
       argSpec.args(0) := "000" & qb;
    end if;

    argSpec.intArgSel := (bool2std(intSrc0), bool2std(intSrc1), bool2std(intSrc2));
    argSpec.floatArgSel := (bool2std(fpSrc0), bool2std(fpSrc1), bool2std(fpSrc2));

    if hasImm16 or hasImm10 or hasImm26 or hasImm21 then
        argSpec.args(1) := (others => '0');
    else
        argSpec.args(1) := "000" & qc;
    end if;

    if src2a then
       argSpec.args(2) := "000" & qa;
    else
       argSpec.args(2) := (others => '0');
    end if;


    -- process immediate
    constantArgs.immSel := bool2std(hasImm16 or hasImm10 or hasImm26 or hasImm21);    
    constantArgs.imm := w;
    
    if hasImm16 then
        constantArgs.imm(31 downto 16) := (others => w(15));
    else
        constantArgs.imm(31 downto 10) := (others => w(9));
    end if;

    --classInfo := res.classInfo;
    oOp := op;
    oConstantArgs := constantArgs;
    oArgSpec := argSpec;
end procedure;

end package body;