`timescale 1ns / 1ps



module ArchDesc0();

    import InsDefs::*;
    import Asm::*;
    import Emulation::*;

    Word w0 = 0;

    MnemonicClass::Mnemonic mnem;

    Emulator emulSig = new();


        Word wt0 = $signed(Word'(-20)) / $signed(Word'(3));
        Word wt1 =  $signed(Word'(-20)) % $signed(Word'(3));
        Word wt2 =  $signed(Word'(20)) / $signed(Word'(-3));
        Word wt3 =  $signed(Word'(20)) % $signed(Word'(-3));

        Word wt4 = $signed(Word'(-20)) / $signed(Word'(-3));
        Word wt5 =  $signed(Word'(-20)) % $signed(Word'(-3));
        Word wt6 =  $signed(Word'(20)) / $signed(Word'(3));
        Word wt7 =  $signed(Word'(20)) % $signed(Word'(3));

    initial begin
        Section common;
        Section current;
        Word progMem[];
        automatic logic[7:0] dataMem[] = new[4096];
        automatic Emulator emul = new();
        
        
        automatic squeue tests = readFile("tests_all.txt");

    
        automatic squeue lines = readFile(//"a_tmp_sf1.txt");
                                          //"branch_conditional.txt");
                                          "use_lib0.txt");
        $display("%p", tests);
        
                emul.reset();
                emulSig = emul;
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
                progMem = current.words;
            end
                
                emul.reset();
                emul.executeStep(progMem, dataMem);
                emul.executeStep(progMem, dataMem);
                emul.executeStep(progMem, dataMem);
                
                emulSig = emul;
            #1;
        end

        //#10 $display(lines.size());
        //processLines(lines);

    end



endmodule
