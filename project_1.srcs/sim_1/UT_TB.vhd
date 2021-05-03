
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use std.textio.all;

use work.BasicTypes.all;	
use work.ArchDefs.all;	
use work.CoreConfig.all;

use work.Helpers.all;

use work.InstructionState.all;

use work.Assembler.all;
use work.Emulate.all;

use work.DecodingDev.all;


entity UT_TB is

end UT_TB;



architecture Behavioral of UT_TB is
    procedure testDecoder is
        variable w1, w2: Word;
        variable ins1, ins2: InstructionState;
    begin
        w1 := asm("ja 0");
        ins1 := decodeFromWord(w1);
        
        w2 := asmNew("ja 0");
        ins2 := decodeFromWordNew(w2);        
        
    end procedure;
    
    signal dummy: std_logic := '0';
    
    signal ins0, ins1: InstructionState; 
    signal ch0, ch1, ch2: std_logic := '0';
begin

        ins0 <= decodeFromWord(asm("ja 0"));
        ins1 <= decodeFromWordNew(asmNew("ja 0"));
        
        ch0 <= bool2std(ins0 = ins1);
    process 
    begin
        report "Start test";
        
        testDecoder;
        
        wait;
    end process;
    
end Behavioral;
