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

    Emulator emulSig = new();

    Section common;
    Word progMem[4096];
    logic[7:0] dataMem[] = new[4096]('{default: 0});

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
        
        $display(">>> Tests done");
    end

    task automatic setBasicHandlers();
        progMem[Emulator::IP_RESET/4] = processLines({"ja -512"}).words[0];
        progMem[Emulator::IP_RESET/4 + 1] = processLines({"ja 0"}).words[0];
        
        progMem[Emulator::IP_ERROR/4] = processLines({"sys error"}).words[0];
        progMem[Emulator::IP_ERROR/4 + 1] = processLines({"ja 0"}).words[0];

        progMem[Emulator::IP_CALL/4] = processLines({"sys send"}).words[0];
        progMem[Emulator::IP_CALL/4 + 1] = processLines({"ja 0"}).words[0];   
    endtask


    task automatic runTest(input string name, ref Emulator emul);
        int i;
        squeue fileLines = readFile(name);
        Section testSection = processLines(fileLines);
    
        testSection = fillImports(testSection, 0, common, COMMON_ADR);

        dataMem = '{default: 0};
        progMem = '{default: 'x};
        
        foreach (testSection.words[i]) progMem[i] = testSection.words[i];
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers();
        
        emul.reset();
        #1;
        
        for (i = 0; i < ITERATION_LIMIT; i++)
        begin
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

            emulSig = emul;

            #1;
        end
        
        if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", name);
        
    endtask


    task automatic testErrorSignal(ref Emulator emul);
        int i;
        dataMem = '{default: 0};
        progMem = '{default: 'x};

        progMem[0] = processLines({"undef"}).words[0];
        progMem[1] = processLines({"ja 0"}).words[0];
        
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers();
        
        emul.reset();
        #1;
        
        for (i = 0; i < ITERATION_LIMIT; i++)
        begin
            emul.executeStep(progMem, dataMem);
            
            if (emul.status.error == 1) break;
            
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
        
        if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", "error sig");
        
    endtask


    task automatic testEvent(ref Emulator emul);
        int i;
        squeue fileLines = readFile("events.txt");
        Section testSection = processLines(fileLines);
    
        testSection = fillImports(testSection, 0, common, COMMON_ADR);

        dataMem = '{default: 0};
        progMem = '{default: 'x};
        
        foreach (testSection.words[i]) progMem[i] = testSection.words[i];
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers();
        
        // Special handler for call
        progMem[Emulator::IP_CALL/4] = processLines({"add_i r20, r0, 55"}).words[0];
        progMem[Emulator::IP_CALL/4 + 1] = processLines({"sys rete"}).words[0];
        progMem[Emulator::IP_CALL/4 + 2] = processLines({"ja 0"}).words[0];
        
        emul.reset();
        #1;
        
        
        for (i = 0; i < ITERATION_LIMIT; i++)
        begin
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

            emulSig = emul;

            #1;
        end
        
        if (i >= ITERATION_LIMIT) $fatal("Exceeded max iterations in test %s", "event");
        
    endtask



    task automatic testInterrupt(ref Emulator emul);
        int i;
        squeue fileLines = readFile("events2.txt");
        Section testSection = processLines(fileLines);
    
        testSection = fillImports(testSection, 0, common, COMMON_ADR);

        dataMem = '{default: 0};
        progMem = '{default: 'x};
        
        foreach (testSection.words[i]) progMem[i] = testSection.words[i];
        foreach (common.words[i]) progMem[COMMON_ADR/4 + i] = common.words[i];
        
        setBasicHandlers();
        
        // Special handler for call
        progMem[Emulator::IP_CALL/4] = processLines({"add_i r20, r0, 55"}).words[0];
        progMem[Emulator::IP_CALL/4 + 1] = processLines({"sys rete"}).words[0];
        progMem[Emulator::IP_CALL/4 + 2] = processLines({"ja 0"}).words[0];
        
        // Special handler for int 
        progMem[Emulator::IP_INT/4] = processLines({"add_i r21, r0, 77"}).words[0];
        progMem[Emulator::IP_INT/4 + 1] = processLines({"sys reti"}).words[0];
        progMem[Emulator::IP_INT/4 + 2] = processLines({"ja 0"}).words[0];

        emul.reset();
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
                else runTestSim(lineParts[0]);
            end
            
            $finish();
        endtask
        
        task automatic runTestSim(input string name);
            Section common, testProg;
            #CYCLE;
                $display("> RUN: %s", name);
            
            common = processLines(readFile({"common_asm", ".txt"}));
            programMem.setContentAt(common.words, COMMON_ADR);
            
            testProg = fillImports(processLines(readFile({name, ".txt"})), 0, common, COMMON_ADR);
            
            programMem.setContent(testProg.words);
            programMem.setBasicHandlers();

            reset <= 1;
            #CYCLE;
            reset <= 0;
            
            wait (done);
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
