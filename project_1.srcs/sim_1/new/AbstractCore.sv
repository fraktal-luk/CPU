
import Base::*;
import InsDefs::*;
import Asm::*;
import Emulation::*;

import AbstractSim::*;


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

    localparam int FETCH_QUEUE_SIZE = 8;
    localparam int OP_QUEUE_SIZE = 24;


    localparam OpSlot EMPTY_SLOT = '{'0, 'x, 'x};
    

    typedef Word FetchGroup[FETCH_WIDTH];

    int fqSize = 0, oqSize = 0;

    logic fetchAllow;
    logic resetPrev = 0, intPrev = 0, branchRedirect = 0, eventRedirect = 0;
    Word branchTarget = 'x, eventTarget = 'x, storedTarget = 'x;
    
    Stage ipStage = EMPTY_STAGE, fetchStage0 = EMPTY_STAGE, fetchStage1 = EMPTY_STAGE;
    Stage fetchQueue[$:FETCH_QUEUE_SIZE];
    
    Stage nextStage = EMPTY_STAGE;
    OpSlot opQueue[$:OP_QUEUE_SIZE];
    OpSlot memOp = EMPTY_SLOT, memOpPrev = EMPTY_SLOT, lastCommitted = EMPTY_SLOT;
    OpSlot committedGroup[4] = {default: EMPTY_SLOT};
    
    Word intRegs[32], floatRegs[32], sysRegs[32], sysRegs_A[32];
    
    int lastPerfCount = 0;
    
    always @(posedge clk) begin
        
        resetPrev <= reset;
        intPrev <= interrupt;
        sig <= 0;
        wrong <= 0;

        readReq[0] = 0;
        readAdr[0] = 'x;
        writeReq = 0;
        writeAdr = 'x;
        writeOut = 'x;
        
        branchRedirect <= 0;
        branchTarget <= 'x;

        eventRedirect <= 0;
        eventTarget <= 'x;

        lastPerfCount <= 0; 

        if (resetPrev | intPrev | branchRedirect | eventRedirect) begin            
            if (resetPrev)
                ipStage <= '{'1, 512, '{default: '0}, '{default: 'x}};
            else if (intPrev)
                ipStage <= '{'1, eventTarget, '{default: '0}, '{default: 'x}};
            else if (eventRedirect)
                ipStage <= '{'1, eventTarget, '{default: '0}, '{default: 'x}};
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
            
            if (resetPrev) begin
                intRegs = '{0: '0, default: '0};
                floatRegs = '{default: '0};
                sysRegs = '{0: -1, 1: 0, default: '0};
                sysRegs_A = '{0: -1, 1: 0, default: '0};
            end

        end
        else begin
            if (fetchAllow) ipStage <= '{'1, (ipStage.baseAdr & ~(4*FETCH_WIDTH-1)) + 4*FETCH_WIDTH, '{default: '0}, '{default: 'x}};
            
            fetchStage0 <= setActive(ipStage, ipStage.active & fetchAllow);
            fetchStage1 <= setWords(fetchStage0, insIn);
            
            if (fetchStage1.active) fetchQueue.push_back(fetchStage1);

            if (fqSize > 0 && oqSize < OP_QUEUE_SIZE - 2*FETCH_WIDTH) nextStage <= fetchQueue.pop_front();
            else nextStage <= EMPTY_STAGE;
            
            if (nextStage.active) begin
                foreach (nextStage.words[i])
                    if (nextStage.mask[i]) opQueue.push_back('{'1, nextStage.baseAdr + 4*i, nextStage.words[i]});
            end
            
            memOp <= EMPTY_SLOT;
            memOpPrev <= memOp;
            
            if (interrupt) begin
                eventTarget <= IP_INT;
                storedTarget <= IP_INT;
                
                sysRegs[5] = sysRegs[1];
                sysRegs[1] |= 1; // TODO: handle state register correctly
                sysRegs[3] = storedTarget;
                
                sysRegs_A[5] = sysRegs_A[1];
                sysRegs_A[1] |= 1; // TODO: handle state register correctly
                sysRegs_A[3] = storedTarget;
            end
            else begin

                if (memOpPrev.active) begin // Finish executing mem operation from prev cycle
                    // If load, write to register; if store, nothing to do
                    automatic AbstractInstruction memAbs = decodeAbstract(memOpPrev.bits);
                    if (memAbs.def.o inside {O_intLoadW, O_intLoadD}) begin
                        intRegs[memAbs.dest] = readIn[0];
                        intRegs[0] = 0;
                    end
                    else if (memAbs.def.o inside {O_floatLoadW}) begin
                        floatRegs[memAbs.dest] = readIn[0];
                    end
                    
                    lastCommitted <= memOpPrev;
                end
                
                if (!memOp.active) begin// If mem is being done, wait for result
                    automatic int performedCount = 0;
                    automatic OpSlot performedNow[4] = {default: EMPTY_SLOT};
                    for (int i = 0; i < oqSize; i++) begin            
                        // scan until a mem, taken branch or system operation
                        automatic OpSlot op = opQueue.pop_front();
                        automatic AbstractInstruction abs = decodeAbstract(op.bits);
                        
                        //automatic OpSlot opSys = getSystemOp(op);
                        automatic AbstractInstruction absSys;// = decodeAbstract(op.bits);
                        
                        automatic Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap[abs.fmt].typeSpec);
                        automatic Word result = calculateResult(abs, args, op.adr);
                        automatic LateEvent lateEvt;
                        
                        performedNow[performedCount] = op;
                        performedCount++;

                        
                        if (abs.def.o == O_sysLoad) result = sysRegs[args[1]];

                        //    $display("Exec %h: %s", op.adr, TMP_disasm(op.bits));
                                                
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
                            O_intRotate,
                            
                            O_sysLoad
                        }) intRegs[abs.dest] = result;
                        
                        intRegs[0] = 0;
                        
                        if (abs.def.o inside {O_floatMove}) floatRegs[abs.dest] = result;
            
                        lastCommitted <= op;
                        storedTarget <= op.adr + 4;
                        
                        
                        
                        // Branches
                        if (abs.def.o == O_jump) begin
                            // Resolve condition
                            automatic bit redirect = 0;
                            automatic Word brTarget;
                            case (abs.mnemonic)
                                "ja", "jl": redirect = 1;
                                "jz_i": redirect = (args[0] == 0);
                                "jnz_i": redirect = (args[0] != 0);
                                "jz_r": redirect = (args[0] == 0);
                                "jnz_r": redirect = (args[0] != 0);
                                default: $fatal("Wrong kind of branch");
                            endcase
                            
                            brTarget = (abs.mnemonic inside {"jz_r", "jnz_r"}) ? args[1] : op.adr + args[1];
                            
                            branchTarget <= brTarget;
                            branchRedirect <= redirect;
                            
                            if (redirect) begin
                                storedTarget <= brTarget;
                                break;
                            end
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
                        
                        
                        
                        
                        // System ops
                        absSys = abs;
                        
                        lateEvt = getLateEvent(op, abs, sysRegs[2], sysRegs[3]);

                        
                        case (abs.def.o)
                            O_sysStore: begin
                                sysRegs[args[1]] = args[2];
                            end
                        
                            O_halt: begin
                                $error("halt not implemented");
                            end

                            default: ;                            
                        endcase
                        
                        modifySysRegs(sysRegs, op, abs);
                        //    modifySysRegs(sysRegs_A, op, abs);
                        
                        
                        eventTarget <= lateEvt.target;
                        storedTarget <= lateEvt.target;
                        eventRedirect <= lateEvt.redirect;
                    
                        sig <= lateEvt.sig;
                        wrong <= lateEvt.wrong;
                        
                        lastCommitted <= op;
                        
                        //if (abs.def.o inside {O_undef, O_call, O_sync, O_retE, O_retI, O_replay, O_halt, O_send})
                        if (isSystemOp(abs)) break;
                        
                        if (performedCount == 4) break; // Limits Exec group size to 4 ops
                    end
                    lastPerfCount <= performedCount;
                end
                
            end


        end
        
        fqSize <= fetchQueue.size();
        oqSize <= opQueue.size();
    end
    
    function logic fetchQueueAccepts(input int k);
        return k <= FETCH_QUEUE_SIZE - 3 ? '1 : '0;
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
    assign insAdr = ipStage.baseAdr;


//    function automatic logic getSystemOp(input OpSlot op);
//        return abs.def.o inside {O_undef, O_call, O_sync, O_retE, O_retI, O_replay, O_halt, O_send};
//    endfunction


endmodule
