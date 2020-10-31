----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07.10.2020 22:37:33
-- Design Name: 
-- Module Name: EmulationTB - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use std.textio.all;

use work.BasicTypes.all;	
use work.ArchDefs.all;	

use work.Helpers.all;

use work.Assembler.all;
use work.Emulate.all;

entity EmulationTB is
end EmulationTB;

architecture Behavioral of EmulationTB is
    constant TIME_STEP: time := 1 ns;
    
    signal programMemory: WordArray(0 to 1023);
    signal dataMemory: ByteArray(0 to 4095);
    
	signal prog: ProgramBuffer;
    signal machineCode: WordArray(0 to prog'length-1);
    
    signal currentTest, currentSuite: string(1 to 20);
    signal instructionWord: Mword;
    signal disasm: string(1 to 51);
    
    
    signal cpuState: CoreState := INIT_CORE_STATE;
    
    
        signal internalOp: InternalOperation;
        signal opFlags: std_logic_vector(0 to 2);
        
    signal okFlag, errorFlag: std_logic := '0';
begin
    okFlag <= bool2std(opFlags = "001");
    errorFlag <= bool2std(opFlags = "100");

    -- Stimulus process
    stim_proc: process
        variable progB: ProgramBuffer;
        variable testName, suiteName: line;
        variable insWordVar: Word;
        variable intOpVar: InternalOperation;
        file suiteFile: text open read_mode is "suite_names.txt";
        file testFile: text;
    begin
   
      wait for 10*TIME_STEP;
                
      loop
            suiteName := null;
            readline(suiteFile, suiteName);
            if suiteName = null then -- testName'length = 0 then
                exit;
            elsif suiteName(1) = ';' then
                next;
            end if;          
            
            report "Starting suite: " & suiteName.all;
            
            file_open(testFile, suiteName.all & ".txt", read_mode);
            loop
                testName := null;      
                readline(testFile, testName);
                if testName = null then -- testName'length = 0 then
                    exit;
                elsif testName(1) = ';' then
                    next;
                end if;
                
                report "Now to run: " & testName.all;
                progB := readSourceFile(testName.all & ".txt");
                machineCode <= processProgram(progB);

                    currentSuite <= (others => ' ');
                    currentTest <= (others => ' ');
                    currentSuite(1 to suiteName.all'length) <= suiteName.all;
                    currentTest(1 to testName.all'length) <= testName.all;
					
                        instructionWord <= (others => 'U');
                        internalOp <= DEFAULT_INTERNAL_OP;
                        disasm <= (others => ' ');
                        


                opFlags <= (others => '0');
                
                cpuState <= INIT_CORE_STATE;
                
                dataMemory <= (others => (others => '0'));
                
                wait for TIME_STEP;
                
                programMemory(0 to machineCode'length-1) <= machineCode(0 to machineCode'length-1);
                programMemory(512/4) <= ins6L(j, -512);-- TEMP! 
                programMemory(384/4) <= ins655655(ext2, 0, 0, send, 0, 0);
                programMemory(384/4 + 1) <= ins6L(j, 0); -- idle loop          
                

                
                wait for TIME_STEP;
                
--                disasmToFile(testName.all & "_disasm.txt", testProgram);
                
                -- Now doing the actual test 
                    while opFlags /= "100" and opFlags /= "001" loop -- ERROR or SEND (completed)
                        insWordVar := programMemory(slv2u(cpuState.nextIP)/4);
                        instructionWord <= insWordVar;
                        intOpVar := decode(cpuState.nextIP, insWordVar);
                        internalOp <= intOpVar;
                        disasm <= disasmWithAddress(slv2u(cpuState.nextIP), programMemory(slv2u(cpuState.nextIP)/4));
                        
                        performOp(cpuState, dataMemory, intOpVar, opFlags);
                                          
                        wait for TIME_STEP;
                    end loop;
                    

            end loop;
            report "All tests in suite done!";
        
        end loop;
            
        report "All suites done!";         
        
        wait;
    end process;
    
end Behavioral;
