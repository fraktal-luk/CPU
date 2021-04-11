

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.ArchDefs.all;


package InstructionSet is

-- Main table - level 0
-- 
--  000000: ext0
--  
--          addi
--          addh
-- 
-- 


type Opcode0 is (none, jumpLong, jumpLink, jumpZ, jumpNZ, intAlu, intMem, floatMem, addI, addH, sysMem, sysControl);

type Opcode1 is (none,
                 
                 -- intAlu
                 intLogic, intArith, jumpReg,
                 
                 
                 -- intMem
                 intLoadW, intStoreW,
                 
                 -- floatMem
                 floatLoadW, floatStoreW,
                 
                 -- sysMem
                 sysLoad, sysStore,

                 
                 -- sysControl                 
                 sysUndef,
                 sysError,
                 sysCall,
                 sysRetE,
                 sysRetI,
                 sysHalt,
                 sysSync,
                 sysReplay,
                 sysSend                 
                 );


type Opcode2 is (none, 

                intAnd, intOr, intXor,
                
                intAdd, intSub, 
                    
                    jumpRegZ, jumpRegNZ
                    
                
                );

type Format is (none, noRegs, jumpLong, jumpCond, jumpLink, intImm16, intImm10, intRR, 
                    intStore16, intStore10, floatLoad10, floatLoad16, floatStore10, floatStore16,
                    sysLoad, sysStore);

type Operation is (none,
                    
                    undef,
                    call,
                    sync,
                    retE,
                    retI,
                    replay,
                    halt,
                    send,
                    
                    
                    jump,
                    
                    intAnd, intOr, intXor,
                    
                    
                    intAdd, intSub,
                    
                    
                    intLoadW, intLoadD,
                    
                    intStoreW, intStoreD,
                    
                    floatLoadW, floatStoreW,
                    
                    sysLoad, sysStore
                    
                    );

type OpcodeDef0 is record
    --num: slv6;
    name: Opcode0;
    op: Operation;
    fmt: Format;
    mnem: ProcMnemonic;
end record;

type OpcodeTable0 is array(0 to 63) of OpcodeDef0;

type OpcodeDef1 is record
    --num: slv6;
    name: Opcode1;
    op: Operation;
    fmt: Format;
    mnem: ProcMnemonic;    
end record;

type OpcodeTable1 is array(0 to 63) of OpcodeDef1;

type OpcodeDef2 is record
    --num: slv5;
    name: Opcode2;
    op: Operation;
    mnem: ProcMnemonic;    
end record;

type OpcodeTable2 is array(0 to 31) of OpcodeDef2;


type TableArray0 is array(Opcode0) of OpcodeTable1; 
type TableArray1 is array(Opcode1) of OpcodeTable2; 


type Reference1 is record
    name: Opcode1;
    table: OpcodeTable2;
end record;


type InstructionDefinition is record
    opc0: Opcode0;
    opc1: Opcode1;
    opc2: Opcode2;
    op: Operation;    
    fmt: Format;    
end record;


type GeneralTable is array (ProcMnemonic) of InstructionDefinition;

function buildGeneralTable return GeneralTable;

constant TheTable: GeneralTable --(ProcMnemonic'left to ProcMnemonic'right)
                                := buildGeneralTable;


constant EmptyTable1: OpcodeTable1 := (others => (none, none, none, undef));
constant EmptyTable2: OpcodeTable2 := (others => (none, none, undef));


constant MainTable: OpcodeTable0 := (
    0 => (intAlu, none, none, undef), -- TableIntAlu
    
    1 => (intMem, none, none, undef), -- TableIntMem
    2 => (floatMem, none, none, undef), -- TableFloatMem
    
    4 => (sysMem, none, none, undef),  -- TableSysMem
    
    7 => (sysControl, none, none, undef),
    
    8 => (jumpLong, jump, jumpLong, ja),
    9 => (jumpLink, jump, jumpLink, jl),
    10 => (jumpZ, jump, jumpCond, jz_i),
    11 => (jumpNZ, jump, jumpCond, jnz_i),
    
    
    16 => (addI, intAdd, intImm16, add_i),
    17 => (addH, intAdd, intImm16, undef),
    
    others => (none, none, none, undef)
);

constant TableIntAlu: OpcodeTable1 := (
    0 => (intLogic, none, intRR, undef),
    1 => (intArith, none, intRR, undef),
    2 => (jumpReg, none, intRR, undef),
    
    others => (none, none, none, undef)
);

constant TableIntMem: OpcodeTable1 := (
    0 => (intLoadW, intLoadW, intImm10, undef),

    32 => (intStoreW, intStoreW, intStore10, undef),
    
    others => (none, none, none, undef)
);


constant TableFloatMem: OpcodeTable1 := (
    0 => (floatLoadW, floatLoadW, floatLoad10, undef),

    32 => (floatStoreW, floatStoreW, floatStore10, undef),
    
    others => (none, none, none, undef)
);


constant TableSysMem: OpcodeTable1 := (
    0 => (sysLoad, sysLoad, sysLoad, lds),

    32 => (sysStore, sysStore, sysStore, sts),
    
    others => (none, none, none, undef)
);

constant TableSysControl: OpcodeTable1 := (
    0 => (sysUndef, undef, noRegs, undef),
    1 => (sysError, undef, noRegs, undef),
    2 => (sysCall,  call, noRegs, sys),
    3 => (sysSync,  sync, noRegs, sys),
    4 => (sysReplay,  replay, noRegs, sys),
    5 => (sysHalt,  halt, noRegs, sys),
    6 => (sysSend,  send, noRegs, sys),
    7 => (sysRetE,  retE, noRegs, sys),
    8 => (sysRetI,  retI, noRegs, sys),
    
    others => (none, none, none, undef)
);



------------------

constant TableIntLogic: OpcodeTable2 := (
    0 => (intAnd, intAnd, and_r),
    1 => (intOr, intOr, or_r),
    others => (none, none, undef)
);

constant TableIntArith: OpcodeTable2 := (
    0 => (intAdd, intAdd, add_r),
    1 => (intSub, intSub, sub_r),
    others => (none, none, undef)
);


constant TableJumpReg: OpcodeTable2 := (
    0 => (jumpRegZ, jump, jz_r),
    1 => (jumpRegNZ, jump, jnz_r),
    others => (none, none, undef)
);

constant Tables1: TableArray0 := (
    intAlu => TableIntAlu,
    intMem => TableIntMem,
    floatMem => TableFloatMem,
    sysMem => TableSysMem,
    sysControl => TableSysControl,
    
    others => EmptyTable1
);

constant Tables2: TableArray1 := (
    intLogic => TableIntLogic,
    intArith => TableIntArith,
    jumpReg => TableJumpReg,
    
    others => EmptyTable2
);


-- Level 1
-- ext0 - ALU
-- 
-- 000000: logical op
-- ...
-- 
-- 
-- 
-- 
-- 



-- Level 2
-- ext0:logical
-- 
-- 00000: and
-- 
-- 
-- 
-- 
-- 



end InstructionSet;



package body InstructionSet is

function findMnemonic(mnem: ProcMnemonic) return boolean is
    variable op0, op1, op2: integer := -1;
    variable fmt: Format := none;
    variable tab1: OpcodeTable1;
begin
    -- Search in main table
    for i in 0 to 63 loop
        if MainTable(i).mnem = mnem and MainTable(i).op /= none then
            op0 := i;
            fmt := MainTable(i).fmt;
            
            return true;
        end if;
    end loop;
    
    -- Search in 1 level tables
    for t in Tables1'range loop
        
    end loop;
    
    return false;
end function;


function makeRow(i, j, k: integer) return InstructionDefinition is
    variable res: InstructionDefinition;
    variable op0: Opcode0 := none;
    variable op1: Opcode1 := none;
    variable op2: Opcode2 := none;
    variable op: Operation;
    variable fmt: Format;
begin
    if i /= -1 then
        op0 := MainTable(i).name;
        fmt := MainTable(i).fmt;
        op := MainTable(i).op;
        
        if j /= -1 then
            op1 := Tables1(op0)(j).name;
            fmt := Tables1(op0)(j).fmt;
            op := Tables1(op0)(j).op;
            
            if k /= -1 then
                op2 := Tables2(op1)(k).name;
                op := Tables2(op1)(j).op; 
            end if;
        end if;
    end if;
  
    return (op0, op1, op2, op, fmt);
end function;

function buildGeneralTable return GeneralTable is
    variable op0, op1, op2: integer := -1;
    variable fmt: Format := none;
    variable tab1: OpcodeTable1;
    variable tab2: OpcodeTable2;
    variable res: GeneralTable;
begin
    -- Search in main table
    for i in 0 to 63 loop
        if MainTable(i).op /= none then
            -- Add this to the table
            res(MainTable(i).mnem) := makeRow(i, -1, -1);
        else
            -- Enter level 1 table 
            tab1 := Tables1(MainTable(i).name);
                  
            for j in 0 to 63 loop
                if tab1(j).op /= none then
                    -- Add to table
                    res(tab1(j).mnem) := makeRow(i, j, -1);
                else
                    -- Enter level 2 table
                    tab2 := Tables2(tab1(i).name);
                
                    for k in 0 to 31 loop
                        if tab2(k).op /= none then
                            -- Add to table
                            res(tab2(k).mnem) := makeRow(i, j, k);
                        end if;
                        
                    end loop;
                end if;
            end loop;
        end if;
    end loop;
    
    return res;
end function;


end InstructionSet;
