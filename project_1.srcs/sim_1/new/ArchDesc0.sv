`timescale 1ns / 1ps


module ArchDesc0();

    import InsDefs::*;
    import Asm::*;
    import Emulation::*;

    const Word COMMON_ADR = 1024;
    Word w0 = 0;
    string testName;

    Emulator emulSig = new();


    initial begin
        Section common;
        Section current;
        Word progMem[4096];
        automatic logic[7:0] dataMem[] = new[4096]('{default: 0});
        automatic Emulator emul = new();
          
        automatic squeue tests = readFile("tests_all.txt");
        
        common = processLines(readFile("common_asm.txt"));
        
        emul.reset();
        emulSig = emul;
        #1;

        foreach (tests[i]) begin
            automatic squeue lineParts = breakLine(tests[i]);
            $write("%s", tests[i]);
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
                        $fatal(">>>> Emulation in error state\n");
                        break;
                    end
                    if (emul.status.send == 1) begin
                        $display("   Signal sent\n");
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

    end



endmodule
