`timescale 1ns / 1ps

import Base::*;
import InsDefs::*;
import Asm::*;
import Emulation::*;
import AbstractSim::*;
    
module ArchDesc0();

    const int ITERATION_LIMIT = 2000;

    const Word COMMON_ADR = 1024;
    Word w0 = 0;
    string testName;

    Emulator emulSig = new(), emulSig_C = new(), emulSig_A;

    Section common;
    Word progMem[4096];
    logic[7:0] dataMem[] = new[4096]('{default: 0});

        Word progMem_C[4096];
        logic[7:0] dataMem_C[] = new[4096]('{default: 0});
    

    localparam CYCLE = 10;

    logic clk = 1;
    
    always #(CYCLE/2) clk = ~clk; 


    initial begin
        automatic Emulator emul = new();
        automatic squeue tests = readFile("tests_all.txt");
        
        common = processLines(readFile("common_asm.txt"));
        
        emul.reset();
        emulSig = emul;
        #1;

        foreach (tests[i]) begin
            automatic squeue lineParts = breakLine(tests[i]);

            if (lineParts.size() > 1) $error("There should be 1 test per line");
            else if (lineParts.size() == 0);
            else begin            
                testName = lineParts[0];
                runTest({lineParts[0], ".txt"}, emul);
                testName = "";
            end
            #1;
        end

        testName = "err signal";
        testErrorSignal(emul);
        testName = "";
        #1;
        
        testName = "event";
        testEvent(emul);
        testName = "";
        #1;
        
        testName = "event2";
        testInterrupt(emul);
        testName = "";
        #1;
        
        //$display(">>> Tests done");
    end



                class EncapsulatedEmulation;
                    static Emulator emul = new();
                    static Word progMem[4096];
                    static logic[7:0] dataMem[] = new[4096]('{default: 0});
                
                endclass

                typedef EncapsulatedEmulation EncEmul;

//           initial begin
//                //automatic Emulator emul = new();
//                automatic squeue tests = readFile("tests_all.txt");
                
//                //common = processLines(readFile("common_asm.txt"));
                
//                EncEmul::emul.reset();
//                emulSig_C = EncEmul::emul;
//                #1;
        
//                foreach (tests[i]) begin
//                    automatic squeue lineParts = breakLine(tests[i]);
        
//                    if (lineParts.size() > 1) $error("There should be 1 test per line");
//                    else if (lineParts.size() == 0);
//                    else begin            
//                        //testName = lineParts[0];
//                        //runTest_C({lineParts[0], ".txt"}, EncEmul::emul, EncEmul::progMem, EncEmul::dataMem);
//                        //testName = "";
//                    end
//                    #1;
//                end
        
////                testName = "err signal";
////                testErrorSignal(emul);
////                testName = "";
////                #1;
                
////                testName = "event";
////                testEvent(emul);
////                testName = "";
////                #1;
                
////                testName = "event2";
////                testInterrupt(emul);
////                testName = "";
////                #1;
                
//                //$display(">>> Tests done");
//            end


    task automatic setBasicHandlers(ref Word progMem[4096]);
        progMem[IP_RESET/4] = processLines({"ja -512"}).words[0];
        progMem[IP_RESET/4 + 1] = processLines({"ja 0"}).words[0];
       
        progMem[IP_ERROR/4] = processLines({"sys error"}).words[0];
        progMem[IP_ERROR/4 + 1] = processLines({"ja 0"}).words[0];

        progMem[IP_CALL/4] = processLines({"sys send"}).words[0];
        progMem[IP_CALL/4 + 1] = processLines({"ja 0"}).words[0];   
    endtask


    task automatic runTest(input string name, ref Emulator emul);
        int i;
        EmulationWithMems ewm = new();
        squeue fileLines = readFile(name);
        Section testSection = processLines(fileLines);
    
        testSection = fillImports(testSection, 0, common, COMMON_ADR);

        dataMem = '{default: 0};
        progMem = '{default: 'x};
        
        foreach (testSection.words[i]) progMem[i] = testSection.words[i];
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers(progMem);
        
        emul.reset();
            ewm.prepareTest(name, COMMON_ADR);
        #1;
        
        for (i = 0; i < ITERATION_LIMIT; i++)
        begin
            emul.executeStep(progMem, dataMem);
            
                ewm.step();
            
            if (emul.status.error == 1) begin
                $fatal(">>>> Emulation in error state\n");
                break;
            end
            
            if (emul.status.send == 1) begin
                break;
            end
            
            if (emul.writeToDo.active == 1) begin
                dataMem[emul.writeToDo.adr] = emul.writeToDo.value[31:24];
                dataMem[emul.writeToDo.adr+1] = emul.writeToDo.value[23:16];
                dataMem[emul.writeToDo.adr+2] = emul.writeToDo.value[15:8];
                dataMem[emul.writeToDo.adr+3] = emul.writeToDo.value[7:0];
            end
            
            emul.drain();
                
                ewm.writeAndDrain();
                
            emulSig = emul;
                emulSig_A = ewm.emul;
            #1;
        end
        
        if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", name);
        
    endtask



//                task automatic runTest_C(input string name, ref Emulator emul, ref Word progMem[4096], ref logic[7:0] dataMem[]);
//                    int i;
//                    squeue fileLines = readFile(name);
//                    Section testSection = processLines(fileLines);
                
//                    testSection = fillImports(testSection, 0, common, COMMON_ADR);
            
//                    dataMem = '{default: 0};
//                    progMem = '{default: 'x};
                    
//                    foreach (testSection.words[i]) progMem[i] = testSection.words[i];
//                    foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
                    
//                    setBasicHandlers(progMem);
                    
//                    emul.reset();
//                    #1;
                    
//                    for (i = 0; i < ITERATION_LIMIT; i++)
//                    begin
//                        emul.executeStep(progMem, dataMem);
                        
//                        if (emul.status.error == 1) begin
//                            $fatal(">>>> ####### Emulation in error state\n");
//                            break;
//                        end
                        
//                        if (emul.status.send == 1) begin
//                            break;
//                        end
                        
//                        if (emul.writeToDo.active == 1) begin
//                            dataMem[emul.writeToDo.adr] = emul.writeToDo.value[31:24];
//                            dataMem[emul.writeToDo.adr+1] = emul.writeToDo.value[23:16];
//                            dataMem[emul.writeToDo.adr+2] = emul.writeToDo.value[15:8];
//                            dataMem[emul.writeToDo.adr+3] = emul.writeToDo.value[7:0];
//                        end
                        
//                        emul.drain();
            
//                        //emulSig_C = emul;
            
//                        #1;
//                    end
                    
//                    if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", name);
                    
//                endtask
            





    task automatic testErrorSignal(ref Emulator emul);
        int i;
                EmulationWithMems ewm = new();

        dataMem = '{default: 0};
        progMem = '{default: 'x};

        progMem[0] = processLines({"undef"}).words[0];
        progMem[1] = processLines({"ja 0"}).words[0];
        
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers(progMem);
        
        emul.reset();
                ewm.prepareErrorTest(COMMON_ADR);
        #1;
        
        for (i = 0; i < ITERATION_LIMIT; i++)
        begin
            emul.executeStep(progMem, dataMem);
                    ewm.step();
            
            if (emul.status.error == 1) break;
            
            if (emul.writeToDo.active == 1) begin
                dataMem[emul.writeToDo.adr] = emul.writeToDo.value[31:24];
                dataMem[emul.writeToDo.adr+1] = emul.writeToDo.value[23:16];
                dataMem[emul.writeToDo.adr+2] = emul.writeToDo.value[15:8];
                dataMem[emul.writeToDo.adr+3] = emul.writeToDo.value[7:0];
            end
            
            emul.drain();
                ewm.writeAndDrain();
            emulSig = emul;

            #1;
        end
        
        if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", "error sig");
        
    endtask


    task automatic testEvent(ref Emulator emul);
        int i;
                EmulationWithMems ewm = new();

        squeue fileLines = readFile("events.txt");
        Section testSection = processLines(fileLines);
    
        testSection = fillImports(testSection, 0, common, COMMON_ADR);

        dataMem = '{default: 0};
        progMem = '{default: 'x};
        
        foreach (testSection.words[i]) progMem[i] = testSection.words[i];
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers(progMem);
        
        // Special handler for call
        progMem[IP_CALL/4] = processLines({"add_i r20, r0, 55"}).words[0];
        progMem[IP_CALL/4 + 1] = processLines({"sys rete"}).words[0];
        progMem[IP_CALL/4 + 2] = processLines({"ja 0"}).words[0];
        
        emul.reset();
                ewm.prepareEventTest(COMMON_ADR);
        #1;
        
        
        for (i = 0; i < ITERATION_LIMIT; i++)
        begin
            emul.executeStep(progMem, dataMem);
                ewm.step();
            
            if (emul.status.error == 1) begin
                $fatal(">>>> Emulation in error state\n");
                break;
            end
            
            if (emul.status.send == 1) begin
                break;
            end
            
            if (emul.writeToDo.active == 1) begin
                dataMem[emul.writeToDo.adr] = emul.writeToDo.value[31:24];
                dataMem[emul.writeToDo.adr+1] = emul.writeToDo.value[23:16];
                dataMem[emul.writeToDo.adr+2] = emul.writeToDo.value[15:8];
                dataMem[emul.writeToDo.adr+3] = emul.writeToDo.value[7:0];
            end
            
            emul.drain();
                ewm.writeAndDrain();
                
            emulSig = emul;

            #1;
        end
        
        if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", "event");
        
    endtask



    task automatic testInterrupt(ref Emulator emul);
        int i;
                EmulationWithMems ewm = new();

        squeue fileLines = readFile("events2.txt");
        Section testSection = processLines(fileLines);
    
        testSection = fillImports(testSection, 0, common, COMMON_ADR);

        dataMem = '{default: 0};
        progMem = '{default: 'x};
        
        foreach (testSection.words[i]) progMem[i] = testSection.words[i];
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers(progMem);
        
        // Special handler for call
        progMem[IP_CALL/4] = processLines({"add_i r20, r0, 55"}).words[0];
        progMem[IP_CALL/4 + 1] = processLines({"sys rete"}).words[0];
        progMem[IP_CALL/4 + 2] = processLines({"ja 0"}).words[0];
        
        // Special handler for int 
        progMem[IP_INT/4] = processLines({"add_i r21, r0, 77"}).words[0];
        progMem[IP_INT/4 + 1] = processLines({"sys reti"}).words[0];
        progMem[IP_INT/4 + 2] = processLines({"ja 0"}).words[0];

        emul.reset();
            ewm.prepareIntTest(COMMON_ADR);
        #1;
        
        for (i = 0; i < ITERATION_LIMIT; i++)
        begin
            if (i == 3) begin 
                emul.interrupt;
                #1;
            end

            emul.executeStep(progMem, dataMem);
            
            if (emul.status.error == 1) begin
                $fatal(">>>> Emulation in error state\n");
                break;
            end
            
            if (emul.status.send == 1) begin
                break;
            end
            
            if (emul.writeToDo.active == 1) begin
                dataMem[emul.writeToDo.adr] = emul.writeToDo.value[31:24];
                dataMem[emul.writeToDo.adr+1] = emul.writeToDo.value[23:16];
                dataMem[emul.writeToDo.adr+2] = emul.writeToDo.value[15:8];
                dataMem[emul.writeToDo.adr+3] = emul.writeToDo.value[7:0];
            end
            
            emul.drain();
                ewm.writeAndDrain();
            emulSig = emul;

            #1;
        end
        
        if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", "event2");
        
    endtask



    generate
        typedef ProgramMemory#(4) ProgMem;
        typedef DataMemory#(4) DataMem;
    
        ProgMem programMem;
        DataMem dmem;
        ProgMem::Line icacheOut;
        
        Word fetchAdr;
        
        logic reset = 0, int0 = 0, done, wrong;
        
        logic readEns[4], writeEn;
        Word writeAdr, readAdrs[4], readValues[4], writeValue;
        
        task automatic runSim();
            squeue tests = readFile("tests_all.txt");
            
            foreach (tests[i]) begin
                squeue lineParts = breakLine(tests[i]);

                if (lineParts.size() > 1) $error("There shuould be 1 test per line");
                else if (lineParts.size() == 0) continue;
                
                TMP_setTest(lineParts[0]);
                    emulSig_C = TMP_getEmul();
                    progMem_C = TMP_getEmulWithMems().progMem;
                    dataMem_C = TMP_getEmulWithMems().dataMem;
                runTestSim(lineParts[0]);
                    emulSig_C = TMP_getEmul();
            end
                
                TMP_prepareErrorTest();
            runErrorTestSim();
                TMP_prepareEventTest();
            runEventTestSim();
                TMP_prepareIntTest();
            runIntTestSim();
            
            //$finish();
            $stop(1);
        endtask
        
        task automatic runTestSim(input string name);
            Section common, testProg;
            #CYCLE;
                $display("> RUN: %s", name);
                     //   $display("committed: %d", TMP_getCommit());

            programMem.clear();
 
            common = processLines(readFile({"common_asm", ".txt"}));
            programMem.setContentAt(common.words, COMMON_ADR);
            
            testProg = fillImports(processLines(readFile({name, ".txt"})), 0, common, COMMON_ADR);
            
            programMem.setContent(testProg.words);
            programMem.setBasicHandlers();

            reset <= 1;
            #CYCLE;
            reset <= 0;
            
            wait (done | wrong);
            
            if (wrong) $fatal("TEST FAILED: %s", name);
            
            #CYCLE;
            #CYCLE;
        endtask



        task automatic runErrorTestSim();
            Section common, testProg;
            #CYCLE;
                $display("> RUN: %s", "err");

            programMem.clear();

            common = processLines(readFile({"common_asm", ".txt"}));
            programMem.setContentAt(common.words, COMMON_ADR);
            
            testProg = processLines({"undef",
                                     "ja 0"});
            
            programMem.setContent(testProg.words);
            programMem.setBasicHandlers();

            reset <= 1;
            #CYCLE;
            reset <= 0;
            
            wait (wrong);
            #CYCLE;
            #CYCLE;
        endtask

        task automatic runEventTestSim();
            Section common, testProg;
            #CYCLE;
                $display("> RUN: %s", "event");

            programMem.clear();

            common = processLines(readFile({"common_asm", ".txt"}));
            programMem.setContentAt(common.words, COMMON_ADR);
            
            testProg = fillImports(processLines(readFile({"events", ".txt"})), 0, common, COMMON_ADR);
            
            programMem.setContent(testProg.words);
            programMem.setBasicHandlers();

            programMem.setContentAt(processLines({"add_i r20, r0, 55",
                                                  "sys rete",
                                                  "ja 0"
                                                  }).words,
                                                  IP_CALL);

            reset <= 1;
            #CYCLE;
            reset <= 0;
            
            wait (done | wrong);
            
            if (wrong) $fatal("TEST FAILED: %s", "events");
            
            #CYCLE;
            #CYCLE;
        endtask

        task automatic runIntTestSim();
            Section common, testProg;
            #CYCLE;
                $display("> RUN: %s", "int");
            
            programMem.clear();
            
            common = processLines(readFile({"common_asm", ".txt"}));
            programMem.setContentAt(common.words, COMMON_ADR);
            
            testProg = fillImports(processLines(readFile({"events2", ".txt"})), 0, common, COMMON_ADR);
            
            programMem.setContent(testProg.words);
            programMem.setBasicHandlers();

            programMem.setContentAt(processLines({"add_i r20, r0, 55",
                                                  "sys rete",
                                                  "ja 0"
                                                  }).words,
                                                  IP_CALL);

            programMem.setContentAt(processLines({"add_i r21, r0, 77",
                                                  "sys reti",
                                                  "ja 0"
                                                  }).words,
                                                  IP_INT);

            reset <= 1;
            #CYCLE;
            reset <= 0;
            #CYCLE;
            
            wait (fetchAdr == IP_CALL);
            #CYCLE;
            int0 <= 1;
            #CYCLE;
            int0 <= 0;
            
            wait (done | wrong);
            
            if (wrong) $fatal("TEST FAILED: %s", "int");
            #CYCLE;
            #CYCLE;
        endtask


        initial begin
            programMem = new();
            dmem = new();
            dmem.clear();
        end
        
        always_ff @(posedge clk) icacheOut <= programMem.read(fetchAdr);
        
        always @(posedge clk) begin
            if (readEns[0]) readValues[0] <= dmem.read(readAdrs[0]);
            if (writeEn) dmem.write(writeAdr, writeValue);
            if (reset) dmem.clear();
        end
        
        always runSim();
        
        AbstractCore core(
            .clk(clk),
            .insReq(), .insAdr(fetchAdr), .insIn(icacheOut),
            .readReq(readEns), .readAdr(readAdrs), .readIn(readValues),
            .writeReq(writeEn), .writeAdr(writeAdr), .writeOut(writeValue),
            
            .interrupt(int0),
            .reset(reset),
            .sig(done),
            .wrong(wrong)
        );

    endgenerate  

endmodule
