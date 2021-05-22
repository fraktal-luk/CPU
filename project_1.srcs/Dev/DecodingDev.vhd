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

-- dest: qa, none
-- src0: qb, qa, none
-- src1: qc, imm, none
-- src2: qd, qa, none
-- The above needs just 2 bits for quintet selection: (src0a, src2a)
-- Another bit will be for imm selection. Let it be always signed
-- Also must be selected the domain of args: I/F/none
-- (I|F|0)^4 -> or intSelect & fpSelect
-- > If src0 is qa, it will always be Int (branch cond)


function decodeFromWordNew(w: word) return InstructionState;

function decodeBranchInsNew(w: Word) return std_logic;
function decodeMainClusterNew(w: Word) return std_logic;
function decodeSecClusterNew(w: Word) return std_logic;
function decodeFpRenameNew(w: Word) return std_logic;


function decodeSrc2a(w: Word) return std_logic;
function decodeSrc0a(w: Word) return std_logic;

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
        aluOp := opAdd;
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


--    isBranch: op0 is branch or op1 is reg branch inside integer op0

--    hasImm26: op0
--    hasImm21: op0
--    hasImm16: op0
--    hasImm10: op0

--    hasFpDest: op0 is FP or op0 is FP-mem and operation is load
--                  or CVT int ot FP
    
--    hasNoIntDest:

--    hasIntDest: op0 or int mem load or CVT FP to int or Int(FP, FP) like FP compare 
    
--    src2a: op0 - is mem (no problem for loads because then src 2 is not activated
--    src0a: op0 - branch cond

--    intSrc0: op0

--     This is not set when immediate used
--    intSrc1: op0 (for Int 1 src we can have 2 sources with r0 as the second)

--    intSrc2: int store or int 3 src

--    fpSrc0: op0 ?

--      This should be not set when immediate used (SIMD shifts etc)
--    fpSrc1: op0 and not (FP 1 src or CVT to Int)
--    fpSrc2: FP store or FP 3 src



function checkOp0(op0: slv6; table: Dword) return std_logic is
begin
    return table(slv2u(op0));
end function;

function checkOp1(op0, opRef0, op1: slv6; table: Dword) return std_logic is
begin
    return bool2std(op0 = opRef0) and table(slv2u(op1));
end function;


function decodeFromWordNew(w: word) return InstructionState is
    variable res: InstructionState := DEFAULT_INSTRUCTION_STATE;
    variable specificOperation: SpecificOp := DEFAULT_SPECIFIC_OP;
    
    constant op0: slv6 := w(31 downto 26);
    constant op1: slv6 := w(15 downto 10);
    constant op2: slv5 := w(4 downto 0);
    
    constant qa: slv5 := w(25 downto 21);
    constant qb: slv5 := w(20 downto 16);
    constant qc: slv5 := w(9 downto 5);
    constant qd: slv5 := w(4 downto 0);
    
    variable    hasOp1, hasOp2,
                isBranch, hasImm26, hasImm21, hasImm16, hasImm10, hasFpDest, hasIntDest, hasNoIntDest,
                src2a, src0a,
                intSrc0, intSrc1, intSrc2,
                fpSrc0, fpSrc1, fpSrc2: boolean := false;                
begin
    isBranch := --    op0 = "001000" or op0 = "001001" or op0 = "001010"  or op0 = "001011"   or (op0 = "000000" and (op1 = "000010"));
                std2bool(    checkOp0(op0, OP0_JUMP)
                          or checkOp1(op0, "000000", op1, OP1_INT_ARITH_BRANCH));
    hasImm26 :=   --  op0 = "001000";
                 std2bool(checkOp0(op0, OP0_IMM26));
    
    hasImm21 :=  --   op0 = "001001"   or op0 = "001010"   or op0 = "001011";
                 std2bool(checkOp0(op0, OP0_IMM21));

    hasImm16 :=   --  op0(4) = '1';
                 std2bool(checkOp0(op0, OP0_IMM16));
    
    -- TODO: add shift instructions 
    hasImm10 :=  --   op0 = "000010"   or op0 = "000011"  or op0 = "000100";
                 std2bool(checkOp0(op0, OP0_IMM10));

    hasFpDest := --   op0 = "010110"
               -- or  op0 = "000001";
                std2bool(checkOp0(op0, OP0_FLOAT_DEST) or checkOp1(op0, "000011", op1, OP1_FLOAT_MEM_LOAD));
    
    hasNoIntDest :=   op0 = "000001"   or  op0 = "000011"  or  op0 = "000111"  or  op0 = "001000"    or  op0 = "001010"
                  or  op0 = "001011"   or  op0 = "010101"  or  op0 = "010110"  or  op0 = "010111"
                  or  (op0 = "000100" and op1 = "100000");

    hasIntDest := --not hasNoIntDest;
                  std2bool(
                           checkOp0(op0, OP0_INT_DEST)
                        or checkOp1(op0, "000010", op1, OP1_INT_MEM_LOAD)
                        or checkOp1(op0, "000100", op1, OP1_SYS_MEM_LOAD)
                        );
    
    src2a :=   --        op0 = "010101"  or  op0 = "010111"
               --    or  (op0 = "000100" and op1 = "100000");
                std2bool(checkOp0(op0, OP0_2A) or checkOp1(op0, "000100", op1, OP1_SYS_MEM_STORE));                
    
    src0a :=    --      op0 = "001010"  or  op0 = "001011";
                  std2bool(checkOp0(op0, OP0_0A));
    
    intSrc0 :=   --       op0 = "000000"    or  op0 = "000010"  or  op0 = "000011"  or  op0 = "001010"    or  op0 = "001011"
                 --   or  op0(4) = '1';
                std2bool(checkOp0(op0, OP0_INT_SRC0));
            
    intSrc1 :=  --    op0 = "000000";
                std2bool(checkOp0(op0, OP0_INT_SRC1));
    
    intSrc2 :=  --    op0 = "010101"
                --    or  (op0 = "000100" and op1 = "100000");      
               std2bool(    checkOp0(op0, OP0_INT_SRC2)
                         or checkOp1(op0, "000010", op1, OP1_INT_MEM_STORE)
                         or checkOp1(op0, "000100", op1, OP1_SYS_MEM_STORE));

    fpSrc0 :=  --     op0 = "000001"  and (op1 = "000000");
                std2bool(checkOp0(op0, OP0_FLOAT_ARITH));
    fpSrc1  :=   -- op0 = "000001" 
                 --     and (op1 = ... ); 
                std2bool(checkOp1(op0, "000001", op1, OP1_FLOAT_ARITH_SRC1));
                      
    fpSrc2 :=      --  op0 = "010111";
             std2bool(    checkOp0(op0, OP0_FLOAT_SRC2)
                       or checkOp1(op0, "000001", op1, OP1_FLOAT_ARITH_SRC2));    
     
    res.specificOperation := decodeOperation(op0, op1, op2);
    
    res.classInfo.fpRename := bool2std(hasFpDest or fpSrc0 or fpSrc1 or fpSrc2);
    
    -- assign register definitions
    res.virtualArgSpec.dest := "000" & qa;
    if hasIntDest and isNonzero(res.virtualArgSpec.dest) = '1' then
        res.virtualArgSpec.intDestSel := '1';
    elsif hasFpDest then
        res.virtualArgSpec.floatDestSel := '1';
    else -- When none selected, set 0
        --res.virtualArgSpec.dest := (others => '0');
    end if;
    
    if src0a then
       res.virtualArgSpec.args(0) := "000" & qa;
    else
       res.virtualArgSpec.args(0) := "000" & qb;
    end if;

    res.virtualArgSpec.intArgSel := (bool2std(intSrc0), bool2std(intSrc1), bool2std(intSrc2));
    res.virtualArgSpec.floatArgSel := (bool2std(fpSrc0), bool2std(fpSrc1), bool2std(fpSrc2));

    if hasImm16 or hasImm10 or hasImm26 or hasImm21 then
        res.virtualArgSpec.args(1) := (others => '0');
    else
        res.virtualArgSpec.args(1) := "000" & qc;
    end if;

    if src2a then
       res.virtualArgSpec.args(2) := "000" & qa;
    else
       res.virtualArgSpec.args(2) := (others => '0');
    end if;
    
    -- process immediate
    res.constantArgs.immSel := bool2std(hasImm16 or hasImm10 or hasImm26 or hasImm21);    
    res.constantArgs.imm := w;
    
    if hasImm16 then
        res.constantArgs.imm(31 downto 16) := (others => w(15));
    else
        res.constantArgs.imm(31 downto 10) := (others => w(9));
    end if;
    
    return res;
end function;



function decodeBranchInsNew(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res :=  bool2std(
                w(31 downto 26) = "001000"
             or  w(31 downto 26) = "001001"
             or  w(31 downto 26) = "001010"
             or  w(31 downto 26) = "001011"
             or  (w(31 downto 26) = "000000" and  w(15 downto 10) = "000010" and (w(4 downto 0) = "00000" or w(4 downto 0) = "00001"))
         );
    
    return res;
end function;


function decodeMainClusterNew(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res :=  bool2std(
                w(31 downto 26) /= "000111"
         );
    
    return res;
end function;

function decodeSecClusterNew(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res :=  bool2std(
            w(31 downto 26) = "010101"
         or  w(31 downto 26) = "010111"
         or  (w(31 downto 26) = "000100" and  w(15 downto 10) = "100000")
     );
    return res;
end function;

function decodeFpRenameNew(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res :=  bool2std(
           w(31 downto 26) = "000001"
        or w(31 downto 26) = "010110"
        or w(31 downto 26) = "010111"
       -- or  (w(31 downto 26) = "000100" and  w(15 downto 10) = "100000")
    );
    
    return res;
end function;







function decodeSrc2a(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res := bool2std(opcode = opcode2slv(st))
        or bool2std(opcode = opcode2slv(stf))
        or bool2std((opcode = opcode2slv(ext2)) and (opcont = opcont2slv(ext2, mtc)));
    
    return res;
end function;

function decodeSrc0a(w: Word) return std_logic is
    constant opcode: slv6 := w(31 downto 26);
    constant opcont: slv6 := w(15 downto 10);
    variable res: std_logic := '0';
begin
    res := bool2std(opcode = opcode2slv(jz))
        or bool2std(opcode = opcode2slv(jnz));
    
    return res;
end function;


end package body;