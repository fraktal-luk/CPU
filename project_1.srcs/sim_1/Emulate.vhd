
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;
use work.ArchDefs.all;
use work.Assembler.all;


package Emulate is

type CoreState is record    
    nextIP: Mword;
    sysRegs: MwordArray(0 to 31);
    intRegs: MwordArray(0 to 31);
    floatRegs: MwordArray(0 to 31);
    
end record;


-- CAUTION: dynamic results can be implementation specific. Memory order fixes can't be predicted by non-exact simulation.
--          Phenomena that are not Visible can occur in hardware simulation without occuring in simple emulation.
type OperationResult is record    
    visible: std_logic; -- Takes architectural effect - normal result or exception. Refetches for memory order fix aren't Visible
    normal:    std_logic; -- Happened without special events - intentional control events are not Normal. Refetches for memory order are not Normal
    dynamic:   std_logic; -- Causes control events determined at execution level: exception, memory order fix,...
                            -- Exception return or system call are not dynamic. External interrupt DOES NOT imply dynamic.
                            -- Undefined instructions are not dynamic.
                            -- TODO: what about privilege violations? Probably dynamic exceptions, unless a special instruction for "illegal access" exists
    exception: std_logic;
    interrupt: std_logic;
    
    memRead: std_logic;
    sysRead: std_logic;
    memWrite: std_logic;
    sysWrite: std_logic;
    
    intWrite: std_logic;
    floatWrite: std_logic;

end record;


type InternalOperation is record    
    intSources: MwordArray(0 to 2);
    floatSources: MwordArray(0 to 2);
    hasIntDest: std_logic;
    intDest: Mword;
    hasFloatDest: std_logic;
    floatDest: Mword;
    isMemLoad: std_logic;
    isSysLoad: std_logic;
    isMemStore: std_logic;
    isSysStore: std_logic;
    affectsSys: std_logic;
    
    
end record;



end package;



package body Emulate is


function bin2opcode(v: std_logic_vector) return ProcOpcode is
begin
    case v(5 downto 0) is
        when "000000" => return andI;
        when "000001" => return orI;
        when "000010" => return addI;
        when "000011" => return subI;

        when "000100" => return jz;
        when "000101" => return jnz;
        when "000110" => return j;
        when "000111" => return jl;
            
        when "001000" => return ld;
        when "001001" => return st;
        when "001010" => return ldf;
        when "001011" => return stf;

        when "001100" => return ext0;
        when "001101" => return ext1;
        when "001110" => return ext2;
        when "001111" => return fop;
                  
        when others   => return undef;
    end case;    
    
end function;

function bin2opcont(opcode: ProcOpcode; v: std_logic_vector) return ProcOpcont is
begin
    case opcode is
        --        
        when ext0 =>
            case v(5 downto 0) is
                when "000000" => return andR;
                when "000001" => return orR;
                when "000010" => return shlC;
                when "000011" => return shaC;
        
                when "000100" => return addR;
                when "000101" => return subR;
                when "000110" => return muls;
                when "000111" => return mulu;
                    
                when "001000" => return divs;
                when "001001" => return divu;
          
                when others => return undef;
            end case;
        
        --
        when ext1 =>
            case v(5 downto 0) is
                when "000000" => return jzR;
                when "000001" => return jnzR;

                when others => return undef;
            end case;        
        
        --      
        when ext2 =>
            case v(5 downto 0) is
                when "000000" => return retE;
                when "000001" => return retI;
                when "000010" => return halt;
                when "000011" => return sync;
        
                when "000100" => return replay;
                when "000101" => return error;
                when "000110" => return call;
                when "000111" => return send;
                    
                when "001000" => return mfc;
                when "001001" => return mtc;                
                
                when others => return undef;
            end case;          
        
        --
        when fop =>
            case v(5 downto 0) is
                when "000000" => return fmov;
                when "000001" => return forr;
                
                when others => return undef;
            end case;
            
        when others => return none;
    end case;
    
end function;



function decode(w: Word) return InternalOperation is
    variable res: InternalOperation;
    constant opcode: ProcOpcode := bin2opcode(w(31 downto 26));
    constant opcont: ProcOpcont := bin2opcont(opcode, w(15 downto 10));
begin
    
    
    return res;
end function;


-- TEMP Maybe change to use abstract operation type, add side effects
procedure performOp(state: inout CoreState; memory: inout ByteArray; op: in InternalOperation) is
    
begin
    
    
end procedure;


end package body;
