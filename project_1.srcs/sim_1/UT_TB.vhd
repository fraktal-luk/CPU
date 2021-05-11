
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
    function cmpEncodings(str: string) return std_logic is
        variable w1, w2: Word;
        variable ins1, ins2: InstructionState;
    begin
        --w1 := asm(str);
        --ins1 := decodeFromWord(w1);
        
        w2 := asmNew(str);
        ins2 := decodeFromWordNew(w2);
        
        return bool2std(ins1 = ins2);  
    end function;
    

    procedure testDecoder is
        variable w1, w2: Word;
        variable ins1, ins2: InstructionState;
    begin
        --w1 := asm("ja 0");
        --ins1 := decodeFromWord(w1);
        
        w2 := asmNew("ja 0");
        ins2 := decodeFromWordNew(w2);        
        
    end procedure;
    
    signal dummy: std_logic := '0';
    
    signal ins0, ins1: InstructionState; 

    signal ins2, ins3: InstructionState; 
    signal ch0, ch1, ch2: std_logic := '0';
begin

--        ins0 <= decodeFromWord(asm("ja 0"));
        ins1 <= decodeFromWordNew(asmNew("ja 0"));


--        ins2 <= decodeFromWord(asm("ldi_i r11, r15, 40"));
        ins3 <= decodeFromWordNew(asmNew("ldi_i r11, r15, 40"));
        

        
        ch0 <= bool2std(ins0 = ins1);
        
        ch1 <= cmpEncodings("and_i r3, r7, 20");
        ch2 <= cmpEncodings("ldi_i r11, r15, 40");

    process 
    begin
        report "Start test";
        
        testDecoder;
        
        wait;
    end process;
    
end Behavioral;
