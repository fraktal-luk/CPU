`timescale 1ns / 1ps

import InsDefs::*;
import Asm::*;

module ArchDesc0();

    Word w0 = 0;

    MnemonicClass::Mnemonic mnem;

    initial begin
        Section common;
        Section current;
        
        automatic squeue tests = readFile("tests_all.txt");

    
        automatic squeue lines = readFile(//"a_tmp_sf1.txt");
                                          //"branch_conditional.txt");
                                          "use_lib0.txt");
        $display("%p", tests);
        
        #1;

            common = processLines(readFile("common_asm.txt"));
            writeFile("common_disasm_SV.txt", disasmBlock(common.words));
            $display(common.exports);
        #1;
        
        foreach (tests[i]) begin
            automatic squeue lineParts = breakLine(tests[i]);
            $display("%s, %d", tests[i], lineParts.size());
            if (lineParts.size() > 1) $error("There shuould be 1 test per line");
            
            
            else if (lineParts.size() == 0);
            else begin
                automatic squeue fileLines = readFile({lineParts[0], ".txt"});
                current = processLines(fileLines);
                fillImports(current, 0, common, 0);
            end
            
            #1;
        end

        //#10 $display(lines.size());
        //processLines(lines);

    end



endmodule
