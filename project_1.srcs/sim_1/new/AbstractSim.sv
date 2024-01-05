`timescale 1ns / 1ps

     import Base::*;
     import InsDefs::*;
     import Asm::*;
     import Emulation::*;

package AbstractSim;
    
     import Base::*;
     import InsDefs::*;
     import Asm::*;
     import Emulation::*;

     class ProgramMemory #(parameter WIDTH = 4);
        typedef Word Line[4];
        
        Word content[1024];
        
        function void setContent(Word arr[]);
            foreach (arr[i]) this.content[i] = arr[i];
            
            this.content[Emulator::IP_RESET/4] = processLines({"ja -512"}).words[0];
            this.content[Emulator::IP_RESET/4 + 1] = processLines({"ja 0"}).words[0];
            
            this.content[Emulator::IP_ERROR/4] = processLines({"sys error"}).words[0];
            this.content[Emulator::IP_ERROR/4 + 1] = processLines({"ja 0"}).words[0];
    
            this.content[Emulator::IP_CALL/4] = processLines({"sys send"}).words[0];
            this.content[Emulator::IP_CALL/4 + 1] = processLines({"ja 0"}).words[0];
        endfunction
        
        function Line read(input Word adr);
            Line res;
            Word truncatedAdr = adr & ~(4*WIDTH-1);
            
            foreach (res[i]) res[i] = content[truncatedAdr/4 + i];
            return res;
        endfunction
        
        
    endclass
    
     class DataMemory #(parameter WIDTH = 4);
        typedef logic[7:0] Line[4];
        
        logic[7:0] content[4096];
        
        function void setContent(Word arr[]);
            foreach (arr[i]) this.content[i] = arr[i];
        endfunction
        
        function void clear();
            this.content = '{default: 'x};
        endfunction;
        
        function automatic Word read(input Word adr);
            Word res = 0;
            
            for (int i = 0; i < 4; i++) 
                res = (res << 8) + content[adr + i];
            
            return res;
        endfunction

        function automatic void write(input Word adr, input Word value);
            Word data = value;
            
            for (int i = 0; i < 4; i++) begin
                content[adr + i] = data[31:24];
                data <<= 8;
            end        
        endfunction    
        
    endclass


endpackage


module AbstractCore
#(
    parameter FETCH_WIDTH = 4,
    parameter LOAD_WIDTH = FETCH_WIDTH
)
(
    input logic clk,
    output logic insReq, output Word insAdr, input Word insIn[FETCH_WIDTH],
    output logic readReq[LOAD_WIDTH], output Word readAdr[LOAD_WIDTH], input Word readIn[LOAD_WIDTH],
    output logic writeReq, output Word writeAdr, output Word writeOut,
    
    input logic interrupt,
    input logic reset,
    output logic sig,
    output logic wrong
);
    
    typedef struct {
        logic active;
        Word baseAdr;
        logic mask[FETCH_WIDTH];
        Word words[FETCH_WIDTH];
    } Stage;
    

    
    localparam Stage EMPTY_STAGE = '{'0, 'x, '{default: 0}, '{default: 'x}};

    typedef struct {
        logic active;
        Word adr;
        Word bits;
    } OpSlot;
    
    localparam OpSlot EMPTY_SLOT = '{'0, 'x, 'x};

    typedef Word FetchGroup[FETCH_WIDTH];

    int fqSize = 0, oqSize = 0;

    logic fetchAllow;// = 0;
    logic resetPrev, branchRedirect = 0;
    Word branchTarget = 'x;
    
    FetchGroup fetchedStage0;//, fetchedStage1;
    //FetchGroup decoded;
    
    Stage ipStage = EMPTY_STAGE, fetchStage0 = EMPTY_STAGE, fetchStage1 = EMPTY_STAGE;
    Stage fetchQueue[$:8];
    
    Stage nextStage = EMPTY_STAGE;
    OpSlot opQueue[$:24];
    OpSlot memOp = EMPTY_SLOT, memOpPrev = EMPTY_SLOT;
    
    Word intRegs[32], floatRegs[32];
    
    
    assign fetchedStage0 = insIn;
    
    always @(posedge clk) begin
        
        resetPrev <= reset;
        sig <= 0;

        readReq[0] = 0;
        readAdr[0] = 'x;
        writeReq = 0;
        writeAdr = 'x;
        writeOut = 'x;
        
        branchRedirect <= 0;
        branchTarget <= 'x;

        if (resetPrev | branchRedirect) begin            
            if (resetPrev)
                ipStage <= '{'1, 512, '{default: '0}, '{default: 'x}};
            else if (branchRedirect)
                ipStage <= '{'1, branchTarget, '{default: '0}, '{default: 'x}};
            else $fatal("Should never get here");

            fetchStage0 <= EMPTY_STAGE;
            fetchStage1 <= EMPTY_STAGE;
            fetchQueue.delete();
            
            nextStage <= EMPTY_STAGE;
            opQueue.delete();
            memOp <= EMPTY_SLOT;
            memOpPrev <= EMPTY_SLOT;
            
            intRegs = '{0: '0, default: 'x};
            floatRegs = '{default: 'x};
            
            fqSize <= fetchQueue.size();
            oqSize <= opQueue.size();
        end
        else begin
            if (fetchAllow) ipStage <= '{'1, (ipStage.baseAdr & ~(4*FETCH_WIDTH-1)) + 4*FETCH_WIDTH, '{default: '0}, '{default: 'x}};
            
            fetchStage0 <= setActive(ipStage, ipStage.active & fetchAllow);
            fetchStage1 <= setWords(fetchStage0, fetchedStage0);
            
            if (fetchStage1.active) fetchQueue.push_back(fetchStage1);

            if (fqSize > 0 && oqSize < 24-2*FETCH_WIDTH) nextStage <= fetchQueue.pop_front();
            else nextStage <= EMPTY_STAGE;
            
            if (nextStage.active) begin
                foreach (nextStage.words[i])
                    if (nextStage.mask[i]) opQueue.push_back('{'1, nextStage.baseAdr + 4*i, nextStage.words[i]});
            end
            
            
            
            memOp <= EMPTY_SLOT;
            
            memOpPrev <= memOp;
            
            // finish executing mem operation from prev cycle
            if (memOpPrev.active) begin
                // If load, write to register; if store, nothing to do
                automatic AbstractInstruction memAbs = decodeAbstract(memOpPrev.bits);
                if (memAbs.def.o inside {O_intLoadW, O_intLoadD}) begin
                    intRegs[memAbs.dest] = readIn[0];
                    intRegs[0] = 0;
                end
                else if (memAbs.def.o inside {O_floatLoadW}) begin
                    floatRegs[memAbs.dest] = readIn[0];
                end
            end
            
            if (!memOp.active) // If mem is being done, wait for result
                for (int i = 0; i < oqSize; i++) begin            
                    // scan until a mem operation
                    automatic OpSlot op = opQueue.pop_front();
                    automatic AbstractInstruction abs = decodeAbstract(op.bits);
                    
                    automatic Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap_[abs.fmt].typeSpec);
                    automatic Word result = calculateResult(abs, args, op.adr);
                    
                        $display("Exec %h: %s", op.adr, TMP_disasm(op.bits));
                    
                        if (op.adr == 16*11) sig <= 1;
                    
                    if (abs.def.o inside {
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
                        O_intRotate
                    }) intRegs[abs.dest] = result;
                    intRegs[0] = 0;
                    
                    if (abs.def.o inside {O_floatMove}) floatRegs[abs.dest] = result;
        
                    
                    // Branches
                    if (abs.def.o == O_jump) begin
                        // Resolve condition
                        automatic bit redirect = 0;
                        case (abs.mnemonic)
                            "ja", "jl": redirect = 1;
                            "jz_i": redirect = (args[0] == 0);
                            "jnz_i": redirect = (args[0] != 0);
                            "jz_r": redirect = (args[0] == 0);
                            "jnz_r": redirect = (args[0] != 0);
                            default: $fatal("Wrong kind of branch");
                        endcase
                        
                        branchTarget <= (abs.mnemonic inside {"jz_r", "jnz_r"}) ? args[1] : op.adr + args[1];
                        branchRedirect <= redirect;
                        
                        if (redirect) break;
                    end
                    
                    // Memory ops
                    if (abs.def.o inside {O_intLoadW, O_intLoadD, O_intStoreW, O_intStoreD, O_floatLoadW, O_floatStoreW}) begin
                        readReq[0] <= '1;
                        readAdr[0] <= args[0] + args[1];
                        memOp <= op;
                        
                        if (abs.def.o inside {O_intStoreW, O_intStoreD, O_floatStoreW}) begin
                            writeReq = 1;
                            writeAdr = args[0] + args[1];
                            writeOut = args[2];
                        end
                        
                        break;
                    end
                end
            
            
            fqSize <= fetchQueue.size();
            oqSize <= opQueue.size();
        end
        
    end
    
    function logic fetchQueueAccepts(input int k);
        return k <= 8 - 3 ? '1 : '0;
    endfunction
    
    function automatic Stage setActive(input Stage s, input logic on);
        Stage res = s;
        res.active = on;
        res.baseAdr = s.baseAdr & ~(4*FETCH_WIDTH-1);
        foreach (res.mask[i]) if ((s.baseAdr/4) % FETCH_WIDTH <= i) res.mask[i] = '1;
        return res;
    endfunction

    function automatic Stage setWords(input Stage s, input FetchGroup fg);
        Stage res = s;
        res.words = fg;
        return res;
    endfunction

    assign fetchAllow = fetchQueueAccepts(fqSize);    
    //assign fqSize = fetchQueue.size();

    assign insAdr = ipStage.baseAdr;

endmodule
