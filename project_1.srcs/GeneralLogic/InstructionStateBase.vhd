

library IEEE;
use IEEE.STD_LOGIC_1164.all;

use work.BasicTypes.all;
use work.ArchDefs.all;

use work.CoreConfig.all;


package InstructionStateBase is
	
subtype PhysName is SmallNumber;
type PhysNameArray is array(natural range <>) of PhysName;


type ExecUnit is (General, ALU, MAC, Divide, Jump, Memory, System, FPU );
type ExecFunc is (
                unknown,

                arithAdd, 
                    arithAddH,
                arithSub, arithSha,
                logicAnd, logicOr, logicShl,
                
                mulS, mulU, 
            
                divS, divU,
                
                load, store,
                
                jump,
                jumpZ,
                jumpNZ,
                
                sysRetI, sysRetE,
                sysHalt,
                sysSync, sysReplay,
                sysMTC, sysMFC, -- move to/from control
                sysError,
                sysCall,
                sysSend,
                
                fpuMov,
                fpuOr,
                
                sysUndef
);	


--------------
type SubpipeType is (None, ALU, Mem, FP);

type ArithOp is (opAnd, opOr, opXor, opAdd, opAddH, opSub, opShl, opSha, opRot, opJz, opJnz, opJ, opMul, opMulHS, opMulHU, opDivU, opDivS, opRemU, opRemS);

type MemOp is (opLoad, opStore, opLoadSys, opStoreSys);

type FpOp is (opMove, opOr);

type SysOp is (opNone, opUndef, opHalt, opSync, opReplay,  opRetI, opRetE, opCall, opError, opSend);

function getSpecificOpSize return natural;

function findLog2(n: positive) return natural;

end InstructionStateBase;



package body InstructionStateBase is


function findLog2(n: positive) return natural is
    variable i: natural := 0;
    variable pow2: positive := 1;
begin
    loop
        if pow2 >= n then
            return i;
        end if;
        i := i + 1;
        pow2 := 2*pow2;
    end loop;
    return 0;    
end function;

function getSpecificOpSize return positive is
    variable res: positive := 1;
    constant A: natural := ArithOp'pos(ArithOp'high) - ArithOp'pos(ArithOp'low) + 1;
    constant M: natural := MemOp'pos(MemOp'high) - MemOp'pos(MemOp'low) + 1;
    constant F: natural := FpOp'pos(FpOp'high) - FpOp'pos(FpOp'low) + 1;
    constant S: natural := SysOp'pos(SysOp'high) - SysOp'pos(SysOp'low) + 1;
    variable maxAM, maxAMF, maxAMFS, maxAll: natural := 0;
    variable i: positive := 1;
    variable pow2: positive := 2;
begin
    if A > M then
       maxAM := A;
    else
       maxAM := M;
    end if;
    
    if maxAM > F then
        maxAMF := maxAM;
    else
        maxAMF := F;
    end if;
    
    if maxAMF > S then
        maxAMFS := maxAMF;
    else
        maxAMFS := S;
    end if;
    maxAll := maxAMFS;
    
    return findLog2(maxAll);
end function;
	

end InstructionStateBase;
