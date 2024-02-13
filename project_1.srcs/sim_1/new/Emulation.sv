`timescale 1ns / 1ps


package Emulation;
    import Base::*;
    import InsDefs::*;
    import Asm::*;


    typedef struct {
        Word intRegs[32], floatRegs[32], sysRegs[32];
        Word target;
    } CpuState;

     typedef struct {
        Word target;
        logic redirect;
     } ExecEvent;


    function automatic ExecEvent resolveBranch(input CpuState state, input AbstractInstruction abs, input Word adr);//OpSlot op);
        //AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(state.intRegs, state.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);

//        bit redirect = 0;
//        Word brTarget;

//        case (abs.mnemonic)
//            "ja", "jl": redirect = 1;
//            "jz_i": redirect = (args[0] == 0);
//            "jnz_i": redirect = (args[0] != 0);
//            "jz_r": redirect = (args[0] == 0);
//            "jnz_r": redirect = (args[0] != 0);
//            default: $fatal("Wrong kind of branch");
//        endcase

//        brTarget = (abs.mnemonic inside {"jz_r", "jnz_r"}) ? args[1] : adr + args[1];

//        return '{brTarget, redirect};
        
            return resolveBranch_Internal(abs, adr, args);
    endfunction

    function automatic ExecEvent resolveBranch_Internal(input AbstractInstruction abs, input Word adr, input Word3 vals);//OpSlot op);
        //AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = //getArgs(state.intRegs, state.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);
                        vals;
        bit redirect = 0;
        Word brTarget;

        case (abs.mnemonic)
            "ja", "jl": redirect = 1;
            "jz_i": redirect = (args[0] == 0);
            "jnz_i": redirect = (args[0] != 0);
            "jz_r": redirect = (args[0] == 0);
            "jnz_r": redirect = (args[0] != 0);
            default: ;//$fatal("Wrong kind of branch");
        endcase

        brTarget = (abs.mnemonic inside {"jz_r", "jnz_r"}) ? args[1] : adr + args[1];

        return '{brTarget, redirect};
    endfunction


    function automatic void writeIntReg(ref CpuState state, input Word regNum, input Word value);
        if (regNum == 0) return;
        state.intRegs[regNum] = value;
    endfunction

    function automatic void writeFloatReg(ref CpuState state, input Word regNum, input Word value);
        state.floatRegs[regNum] = value;
    endfunction

    function automatic void writeSysReg(ref CpuState state, input Word regNum, input Word value);
        state.sysRegs[regNum] = value;
    endfunction


    function automatic Word getArgValue(input Word intRegs[32], input Word floatRegs[32], input int src, input byte spec);
        case (spec)
           "i": return Word'(intRegs[src]);
           "f": return Word'(floatRegs[src]);
           "c": return Word'(src);
           "0": return 0;
           default: $fatal("Wrong arg spec");    
        endcase;    
    
    endfunction

    function automatic Word3 getArgs(input Word intRegs[32], input Word floatRegs[32], input int sources[3], input string typeSpec);
        Word3 res;        
        foreach (sources[i]) res[i] = getArgValue(intRegs, floatRegs, sources[i], typeSpec[i+2]);
        
        return res;
    endfunction


   function automatic Word calculateResult(input AbstractInstruction ins, input Word3 vals, input Word ip);
        Word result;
        case (ins.def.o)
            O_jump: result = ip + 4; // link adr
            
            O_intAnd:  result = vals[0] & vals[1];
            O_intOr:   result = vals[0] | vals[1];
            O_intXor:  result = vals[0] ^ vals[1];
            
            O_intAdd:  result = vals[0] + vals[1];
            O_intSub:  result = vals[0] - vals[1];
            O_intAddH: result = vals[0] + (vals[1] << 16);
            
            O_intMul:   result = vals[0] * vals[1];
            O_intMulHU: result = (Dword'($unsigned(vals[0])) * Dword'($unsigned(vals[1]))) >> 32;
            O_intMulHS: result = (Dword'($signed(vals[0])) * Dword'($signed(vals[1]))) >> 32;
            O_intDivU:  result = $unsigned(vals[0]) / $unsigned(vals[1]);
            O_intDivS:  result = divSigned(vals[0], vals[1]);
            O_intRemU:  result = $unsigned(vals[0]) % $unsigned(vals[1]);
            O_intRemS:  result = remSigned(vals[0], vals[1]);
            
            O_intShiftLogical: begin                
                if ($signed(vals[1]) >= 0) result = $unsigned(vals[0]) << vals[1];
                else                       result = $unsigned(vals[0]) >> -vals[1];
            end
            O_intShiftArith: begin                
                if ($signed(vals[1]) >= 0) result = $signed(vals[0]) << vals[1];
                else                       result = $signed(vals[0]) >> -vals[1];
            end
            O_intRotate: begin
                if ($signed(vals[1]) >= 0) result = {vals[0], vals[0]} << vals[1];
                else                       result = {vals[0], vals[0]} >> -vals[1];
            end
            
            O_floatMove: result = vals[0];

            default: ;
        endcase
        
        return result;
    endfunction


    typedef struct {
        int dummy;
        bit halted;
        bit error;
        bit send;
    } CoreStatus;


    class Emulator;

        const Word SYS_REGS_INITIAL[32] = '{
            0: -1,
            default: 0
        };
        
        Word ip;
        Word ipNext;
        
        string str;
        
        CoreStatus status;

        CpuState coreState;

        typedef struct {
            bit active;
            Word adr;
            Word value;
        } MemoryWrite;

        typedef struct {
            int error;            
            MemoryWrite memWrite;
        } ExecResult;

        MemoryWrite writeToDo;


        function automatic void reset();
            this.ip = IP_RESET;
            this.ipNext = 'x;
            
            this.status = '{default: 0};
            this.writeToDo = '{default: 0};

            this.coreState.intRegs = '{default: 0};
            this.coreState.floatRegs = '{default: 0};
            this.coreState.sysRegs = SYS_REGS_INITIAL;
        endfunction
        
        
        function automatic void executeStep(input Word progMem[], ref logic[7:0] dataMem[]);
            AbstractInstruction absIns = decodeAbstract(progMem[this.ip/4]);
            ExecResult execRes = processInstruction(this.ip, absIns, dataMem);            
        endfunction 
        
        
        function automatic CoreStatus checkStatus();
        
        endfunction 
        
        // Clear mem write and signals to send
        function automatic void drain();
            this.writeToDo = '{default: 0};
            this.status.send = 0;
        endfunction
              

        local function automatic ExecResult processInstruction(input Word adr, input AbstractInstruction ins, ref logic[7:0] dataMem[]);
            ExecResult res;
            FormatSpec fmtSpec = parsingMap[ins.fmt];
            Word3 args = getArgs(this.coreState.intRegs, this.coreState.floatRegs, ins.sources, fmtSpec.typeSpec);
                        
            this.str = disasm(ins.encoding);
           
            performCalculation(ins, args);
            performLoad(ins, args, dataMem);
            performBranch(ins, this.ip, args);
            this.writeToDo = getMemWrite(ins, args);
            performSys(ins, args);
            
            this.ip = this.ipNext;
            
            return res;
        endfunction

//////////////////////////
////  Move to InsDefs
            function automatic bit hasIntDest(input AbstractInstruction ins);
                return ins.def.o inside {
                    O_jump,
                    
                    O_intAnd,
                    O_intOr,
                    O_intXor,
                    
                    O_intAdd,
                    O_intSub,
                    O_intAddH,
                    
                    O_intMul,
                    O_intMulHU,
                    O_intMulHS,
                    O_intDivU,
                    O_intDivS,
                    O_intRemU,
                    O_intRemS,
                    
                    O_intShiftLogical,
                    O_intShiftArith,
                    O_intRotate,
                    
                    O_intLoadW,
                    O_intLoadD,
                    
                    O_sysLoad
                };
            endfunction

            function automatic bit hasFloatDest(input AbstractInstruction ins);
                return ins.def.o inside {
                    O_floatMove,
                    O_floatLoadW
                };
            endfunction

//////////////////////////////

        local function automatic void performCalculation(input AbstractInstruction ins, input Word3 vals);
            Word result = calculateResult(ins, vals, this.ip);

            if (hasFloatDest(ins)) writeFloatReg(this.coreState, ins.dest, result);
            if (hasIntDest(ins)) writeIntReg(this.coreState, ins.dest, result);
        endfunction
        
        local function automatic Word loadWord(input Word adr, input logic[7:0] mem[]);
            Word res;
            res[31:24] = mem[adr];
            res[23:16] = mem[adr+1];
            res[15:8] = mem[adr+2];
            res[7:0] = mem[adr+3];

            return res;
        endfunction
        
        local function automatic void performLoad(input AbstractInstruction ins, input Word3 vals, ref logic[7:0] mem[]);
            Word result;
            Word adr = vals[0] + vals[1];
            bit float = 0;
            
            case (ins.def.o)
                O_intLoadW: result = loadWord(adr, mem);
                O_intLoadD: result = loadWord(adr, mem); // TODO: actual Dword
                O_floatLoadW: begin
                    result = loadWord(adr, mem);
                    float = 1;
                end
                O_sysLoad: begin
                    result = this.coreState.sysRegs[vals[1]];
                end
                default: return;
            endcase

            if (hasFloatDest(ins)) writeFloatReg(this.coreState, ins.dest, result);
            if (hasIntDest(ins)) writeIntReg(this.coreState, ins.dest, result);
        endfunction
        
        local function automatic void performBranch(input AbstractInstruction ins, input Word ip, input Word3 vals);
            //Word trg = ip + 4;
                ExecEvent evt = resolveBranch_Internal(ins, ip, vals);
                Word trg_ = evt.redirect ? evt.target : ip + 4;
            
//            case (ins.mnemonic)
//                "ja", "jl": trg = ip + vals[1];
//                "jz_i": trg = (vals[0] == 0) ? ip + vals[1] : ip + 4;
//                "jnz_i": trg = (vals[0] != 0) ? ip + vals[1] : ip + 4;
//                "jz_r": trg = (vals[0] == 0) ? vals[1] : ip + 4;
//                "jnz_r": trg = (vals[0] != 0) ? vals[1] : ip + 4;
//                default: trg = ip + 4;
//            endcase  
            
//                assert (trg_ == trg) else $error("unmatched target! %d %d", trg_, trg);
            
            this.ipNext = trg_;
        endfunction 


        local function automatic void performSys(input AbstractInstruction ins, input Word3 vals);
            case (ins.def.o)
                O_sysStore: begin
                    writeSysReg(this.coreState, vals[1], vals[2]);
                end
                O_undef: begin
                    this.ipNext = IP_ERROR;
                    this.status.error = 1;

                    this.coreState.sysRegs[4] = this.coreState.sysRegs[1];
                    this.coreState.sysRegs[1] |= 1; // TODO: handle state register correctly
                    this.coreState.sysRegs[2] = this.ip + 4;
                end
                O_call: begin
                    this.ipNext = IP_CALL;

                    this.coreState.sysRegs[4] = this.coreState.sysRegs[1];
                    this.coreState.sysRegs[1] |= 1; // TODO: handle state register correctly
                    this.coreState.sysRegs[2] = this.ip + 4;
                end
                O_sync: ;
                O_retE: begin
                    this.ipNext = this.coreState.sysRegs[2];
                    
                    this.coreState.sysRegs[1] = this.coreState.sysRegs[4];
                end
                O_retI: begin
                    this.ipNext = this.coreState.sysRegs[3];
                    
                    this.coreState.sysRegs[1] = this.coreState.sysRegs[5];
                end
                O_replay: this.ipNext = this.ip;
                O_halt: begin
                    this.ipNext = this.ip;
                    this.status.halted = 1;
                end
                O_send: begin
                    this.status.send = 1;
                end
                default: return;
            endcase
        endfunction
        
        local function automatic MemoryWrite getMemWrite(input AbstractInstruction ins, input Word3 vals);
            MemoryWrite res = '{0, 0, 0};
            
            case (ins.def.o)
                O_intStoreW, O_intStoreD, O_floatStoreW:
                     res = '{1, vals[0] + vals[1], vals[2]};
                default: ; 
            endcase
            
            return res; 
        endfunction
        
        function automatic void interrupt();
            this.ipNext = IP_INT;

            this.coreState.sysRegs[5] = this.coreState.sysRegs[1];
            this.coreState.sysRegs[1] |= 2; // TODO: handle state register correctly
            this.coreState.sysRegs[3] = this.ip;
            
            this.ip = this.ipNext;
        endfunction
        
    endclass



    class EmulationWithMems;
        Emulator emul;
        Word progMem[4096];
        logic[7:0] dataMem[] = new[4096]('{default: 0});
 
        function new();
            this.emul = new();
            this.reset();
        endfunction


        function void reset();
            this.emul.reset();
            this.dataMem = '{default: 0};
            this.progMem = '{default: 'x}; 
        endfunction
        
        function void resetCpu();
            this.emul.reset();
        endfunction  
                
        function void writeProgram(input Word prog[], input int adr);
            foreach (prog[i]) this.progMem[adr/4 + i] = prog[i];
        endfunction
        
        
        function void writeData();
            
        endfunction
        

        function void setBasicHandlers();
            this.progMem[IP_RESET/4] = processLines({"ja -512"}).words[0];
            this.progMem[IP_RESET/4 + 1] = processLines({"ja 0"}).words[0];
           
            this.progMem[IP_ERROR/4] = processLines({"sys error"}).words[0];
            this.progMem[IP_ERROR/4 + 1] = processLines({"ja 0"}).words[0];
    
            this.progMem[IP_CALL/4] = processLines({"sys send"}).words[0];
            this.progMem[IP_CALL/4 + 1] = processLines({"ja 0"}).words[0];        
        endfunction
        
        function automatic void prepareTest(input string name, input int commonAdr);
            squeue fileLines = readFile(name);
            Section common = processLines(readFile("common_asm.txt"));
            Section testSection = processLines(fileLines);
            testSection = fillImports(testSection, 0, common, commonAdr);
            
            this.writeProgram(testSection.words, 0);
            this.writeProgram(common.words, commonAdr);
            
            this.setBasicHandlers();
            
            this.emul.reset();
        endfunction
 
         function automatic void prepareErrorTest(input int commonAdr);
            squeue fileLines = {"undef", "ja 0"};
            Section common = processLines(readFile("common_asm.txt"));
            Section testSection = processLines(fileLines);
            testSection = fillImports(testSection, 0, common, commonAdr);
            
            this.writeProgram(testSection.words, 0);
            this.writeProgram(common.words, commonAdr);
            
            this.setBasicHandlers();
            
            this.emul.reset();
        endfunction

         function automatic void prepareEventTest(input int commonAdr);
            squeue fileLines = readFile("events.txt");
            Section common = processLines(readFile("common_asm.txt"));
            Section testSection = processLines(fileLines);
            testSection = fillImports(testSection, 0, common, commonAdr);
            
            this.writeProgram(testSection.words, 0);
            this.writeProgram(common.words, commonAdr);
            
            this.setBasicHandlers();
            
            this.writeProgram(processLines({"add_i r20, r0, 55", "sys rete", "ja 0"}).words, IP_CALL);        

            this.emul.reset();
        endfunction

 
        function automatic void prepareIntTest(input int commonAdr);
            squeue fileLines = readFile("events2.txt");
            Section common = processLines(readFile("common_asm.txt"));
            Section testSection = processLines(fileLines);
            testSection = fillImports(testSection, 0, common, commonAdr);
            
            this.writeProgram(testSection.words, 0);
            this.writeProgram(common.words, commonAdr);
            
            this.setBasicHandlers();

            this.writeProgram(processLines({"add_i r20, r0, 55", "sys rete", "ja 0"}).words, IP_CALL);        
            this.writeProgram(processLines({"add_i r21, r0, 77", "sys reti", "ja 0"}).words, IP_INT);        
              
            this.emul.reset();
        endfunction
 
 
        function automatic void step();
            this.emul.executeStep(this.progMem, this.dataMem);
        endfunction
        
        function automatic void writeAndDrain();
            if (this.emul.writeToDo.active == 1) begin
                this.dataMem[emul.writeToDo.adr] = this.emul.writeToDo.value[31:24];
                this.dataMem[emul.writeToDo.adr+1] = this.emul.writeToDo.value[23:16];
                this.dataMem[emul.writeToDo.adr+2] = this.emul.writeToDo.value[15:8];
                this.dataMem[emul.writeToDo.adr+3] = this.emul.writeToDo.value[7:0];
            end
            
            this.emul.drain();
        endfunction 
        
        function automatic void interrupt();
            this.emul.interrupt();
        endfunction
        
    endclass    
    


endpackage



