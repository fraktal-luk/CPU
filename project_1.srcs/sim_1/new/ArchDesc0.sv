`timescale 1ns / 1ps



module ArchDesc0();

    import InsDefs::*;
    import Asm::*;
    import Emulation::*;

    const Word COMMON_ADR = 1024;
    Word w0 = 0;
    string testName;

    MnemonicClass::Mnemonic mnem;

    Emulator emulSig = new();


        Word wt0 = (Dword'('h80000000)) >> 32;
        Word wt1 = (Dword'($unsigned('h80000000))) >> 32;
        Word wt2 = (Dword'($signed('h80000000))) >> 32;
        Word wt3 = Word'('h00000030) >> 6;

        Word wt4 = Word'('h00000030) << -1;
        Word wt5 = Word'('h00000030) << -2;
        Word wt6 = Word'('h00000030) << -3;
        Word wt7 = Word'('h00000030) << -4;

    initial begin
        Section common;
        Section current;
        Word progMem[4096];
        automatic logic[7:0] dataMem[] = new[4096]('{default: 0});
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
            //$display(common.exports);
        #1;
        
        foreach (tests[i]) begin
            automatic squeue lineParts = breakLine(tests[i]);
            $display("%s", tests[i]);
            testName = tests[i];
            
            if (lineParts.size() > 1) $error("There shuould be 1 test per line");
            
            
            else if (lineParts.size() == 0);
            else begin
                automatic squeue fileLines = readFile({lineParts[0], ".txt"});
                current = processLines(fileLines);
                current = fillImports(current, 0, common, COMMON_ADR);
                
                dataMem = '{default: 0};
                progMem = '{default: 'x};
                
                foreach (current.words[i])
                    progMem[i] = current.words[i];
                
                foreach (common.words[i])
                    progMem[COMMON_ADR/4 + i] = common.words[i];
                
//                                -- Error handler
//                setInstruction(programMemory2, EXC_BASE, "sys error");
//                setInstruction(programMemory2, addInt(EXC_BASE, 4), "ja 0");

//                -- Call handler
//                setInstruction(programMemory2, CALL_BASE, "sys send");
//                setInstruction(programMemory2, addInt(CALL_BASE, 4), "ja 0");
                
//                -- Common lib
//                setProgram(testProgram2, i2slv(4*1024, 32), commonCode2);
                
                progMem[Emulator::IP_RESET/4] = processLines({"ja -512"}).words[0];
                progMem[Emulator::IP_RESET/4 + 1] = processLines({"ja 0"}).words[0];
                
                progMem[Emulator::IP_ERROR/4] = processLines({"sys error"}).words[0];
                progMem[Emulator::IP_ERROR/4 + 1] = processLines({"ja 0"}).words[0];

                progMem[Emulator::IP_CALL/4] = processLines({"sys send"}).words[0];
                progMem[Emulator::IP_CALL/4 + 1] = processLines({"ja 0"}).words[0];
                
                emul.reset();
                #1;
                
                repeat (2000)
                begin
                    emul.executeStep(progMem, dataMem);
                    if (emul.status.error == 1) begin
                        $error(">>>> Emulation in error state");
                        break;
                    end
                    if (emul.status.send == 1) begin
                        $display("   Signal sent");
                        break;
                    end
                    if (emul.writeToDo.active == 1) begin
                        dataMem[emul.writeToDo.adr] = emul.writeToDo.value[31:24];
                        dataMem[emul.writeToDo.adr+1] = emul.writeToDo.value[23:16];
                        dataMem[emul.writeToDo.adr+2] = emul.writeToDo.value[15:8];
                        dataMem[emul.writeToDo.adr+3] = emul.writeToDo.value[7:0];
                    end
                    emul.drain();
                    
                    emulSig = emul;

                    #1;
                end
                
                testName = "";
                
            end
                

            #1;
        end

        //#10 $display(lines.size());
        //processLines(lines);

    end



endmodule
