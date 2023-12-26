`timescale 1ns / 1ps



package Emulation;
    import InsDefs::*;
    import Asm::*;

    typedef struct {
        int dummy;
        bit halted;
        bit error;
        bit send;
    } CoreStatus;

    class Emulator;
        const Word IP_RESET = 0;
        const Word IP_ERROR = 0;
        const Word IP_CALL = 0;
        const Word IP_INT = 0;
        
        const Word SYS_REGS_INITIAL[32] = '{
            0: -1,
            default: 0
        };
        
        Word ip;
        Word ipNext;
        
        CoreStatus status;
        
        Word intRegs[32];
        Word floatRegs[32];
        Word sysRegs[32];
        
       //     alias STATE_REG = sysRegs[1];
        
        
        typedef struct {
            bit active;
            Word adr;
            Word value;
        } MemoryWrite;
        
        typedef struct {
            int error;
            
            //int regWrite;
            
            MemoryWrite memWrite;
            
        } ExecResult;
        
        
        MemoryWrite writeToDo;
        
        
        function new();
        
        endfunction
        
        
        function automatic void reset();
            this.ip = IP_RESET;
            this.ipNext = 'x;
            
            this.status = '{default: 0};
            this.writeToDo = '{default: 0};
            
            this.intRegs = '{default: 0};
            this.floatRegs = '{default: 0};
            this.sysRegs = SYS_REGS_INITIAL;
        endfunction
        
        
        function automatic void executeStep(ref Word progMem[], ref logic[7:0] dataMem[]);
            AbstractInstruction absIns = decodeAbstract(progMem[this.ip/4]);
            ExecResult execRes = processInstruction(this.ip, absIns, dataMem);
            
            //dataMem[1] = 'h66;
            
        endfunction 
        
        
        function automatic CoreStatus checkStatus();
        
        endfunction 
        
        // Clear mem write and signals to send
        function automatic void drain();
            this.writeToDo = '{default: 0};
            this.status.send = 0;
        endfunction
              
        
        local function automatic Word getArgValue(input int src, input byte spec);
            case (spec)
               "i": return Word'(this.intRegs[src]);
               "f": return Word'(this.floatRegs[src]);
               "c": return Word'(src);
               "0": return 0;
               default: $fatal("Wrong arg spec");    
            endcase;    
        
        endfunction
        
        
        local function automatic Word3 getArgs(input int sources[3], input string typeSpec);
            Word3 res;
            
            foreach (sources[i]) begin
                res[i] = getArgValue(sources[i], typeSpec[i+2]);
            end
            
            return res;
        endfunction
        
        
        local function automatic ExecResult processInstruction(input Word adr, input AbstractInstruction ins, ref logic[7:0] dataMem[]);
            ExecResult res;
            string3 fmtSpec = parsingMap[ins.fmt];
            
            string typeSpec = fmtSpec[2];    
            string decoding = fmtSpec[1];
            string asmForm = fmtSpec[0];
            
            Word3 args = getArgs(ins.sources, typeSpec);
            
            //$display("%h: %h;  %s, %p: %p", this.ip, ins.encoding,  ins.mnemonic, ins.def, args);
            
            performCalculation(ins, args);
            performLoad(ins, args, dataMem);
            performBranch(ins, args);
            this.writeToDo = getMemWrite(ins, args);
            performSys(ins, args);
            
            this.ip = this.ipNext;
            
            return res;
        endfunction
        
        
        local static function automatic Word divSigned(input Word a, input Word b);
            int signed aInt = a;
            int signed bInt = b;
            int signed rInt = a/b;
            int signed rem = aInt - rInt * bInt;
            
            if (rem < 0 && bInt > 0) rInt--;
            if (rem > 0 && bInt < 0) rInt--;
            
            return rInt;
        endfunction
        
        local static function automatic Word remSigned(input Word a, input Word b);
            int signed aInt = a;
            int signed bInt = b;
            int signed rInt = a/b;
            int signed rem = aInt - rInt * bInt;
            
            if (rem < 0 && bInt > 0) rem += bInt;
            if (rem > 0 && bInt < 0) rem += bInt;
            
            return rem;
        endfunction
        
        
        local function automatic void performCalculation(input AbstractInstruction ins, input Word3 vals);
            Word result;
            bit float = 0;
            
            case (ins.def.o)
                O_jump: result = this.ip + 4; // link adr
                
                O_intAnd:  result = vals[0] & vals[1];
                O_intOr:   result = vals[0] | vals[1];
                O_intXor:  result = vals[0] ^ vals[1];
                
                O_intAdd:  result = vals[0] + vals[1];
                O_intSub:  result = vals[0] - vals[1];
                O_intAddH: result = vals[0] + (vals[1] << 16);
                
                O_intMul:   result = vals[0] * vals[1];
                O_intMulHU: result = ($unsigned(vals[0]) * $unsigned(vals[1])) >> 32;
                O_intMulHS: result = ($signed(vals[0]) * $signed(vals[1])) >> 32;
                O_intDivU:  result = $unsigned(vals[0]) / $unsigned(vals[1]);
                O_intDivS:  result = divSigned(vals[0], vals[1]);
                O_intRemU:  result = $unsigned(vals[0]) / $unsigned(vals[1]);
                O_intRemS:  result = remSigned(vals[0], vals[1]);
                
                O_intShiftLogical: result = $unsigned(vals[0]) << vals[1];
                O_intShiftArith:   result = $signed(vals[0]) << vals[1];
                O_intRotate:       result = {vals[0], vals[0]} << vals[1];
                
                O_floatMove: begin
                    result = vals[0];
                    float = 1;
                end
                
                default: return;
            endcase
            
            if (float)
                this.floatRegs[ins.dest] = result;
            else
                this.intRegs[ins.dest] = result;
            
            this.intRegs[0] = 0;
        endfunction
        
        local function automatic Word loadWord(input Word adr, ref logic[7:0] mem[]);
            return {mem[adr], mem[adr+1], mem[adr+2], mem[adr+3]};
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
                O_sysLoad: result = this.sysRegs[vals[1]];
                default: return;
            endcase

            if (float)
                this.floatRegs[ins.dest] = result;
            else
                this.intRegs[ins.dest] = result;
            
            this.intRegs[0] = 0;
        endfunction
        
        local function automatic void performBranch(input AbstractInstruction ins, input Word3 vals);
            
            case (ins.mnemonic)
                "ja", "jl": this.ipNext = this.ip + vals[1];
                "jz_i": this.ipNext = (vals[0] == 0) ? this.ip + vals[1] : this.ip + 4;
                "jnz_i": this.ipNext = (vals[0] != 0) ? this.ip + vals[1] : this.ip + 4;
                "jz_r": this.ipNext = (vals[0] == 0) ? this.ip + vals[1] : this.ip + 4;
                "jnz_r": this.ipNext = (vals[0] != 0) ? this.ip + vals[1] : this.ip + 4;
                default: this.ipNext = this.ip + 4;
            endcase
            
            
        endfunction 

        local function automatic void performSys(input AbstractInstruction ins, input Word3 vals);
            case (ins.def.o)
                O_sysStore:
                    this.sysRegs[vals[1]] = vals[2];
                O_undef: begin
                    this.ipNext = IP_ERROR;
                    this.status.error = 1;
                end
                O_call: begin
                    this.ipNext = IP_CALL;
                    this.sysRegs[3] = this.sysRegs[1];
                    this.sysRegs[1] |= 1; // TODO: handle state register correctly
                    this.sysRegs[2] = this.ip + 4;
                end
                O_sync: ;
                O_retE: begin
                    this.sysRegs[1] = this.sysRegs[3];
                    this.ipNext = this.sysRegs[2];
                end
                O_retI: begin
                    this.sysRegs[1] = this.sysRegs[5];
                    this.ipNext = this.sysRegs[4];
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
            this.sysRegs[5] = this.sysRegs[1];
            this.sysRegs[1] |= 2; // TODO: handle state register correctly
            this.sysRegs[4] = this.ip; // TODO: handle saving correct adr at interrupt
        endfunction
        
    endclass


endpackage
