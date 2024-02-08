
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
        int ctr;
        Word baseAdr;
        logic mask[FETCH_WIDTH];
        Word words[FETCH_WIDTH];
    } Stage;

    localparam Stage EMPTY_STAGE = '{'0, -1, 'x, '{default: 0}, '{default: 'x}};

    localparam int FETCH_QUEUE_SIZE = 8;
    localparam int OP_QUEUE_SIZE = 24;
    localparam int OOO_QUEUE_SIZE = 240;

    localparam OpSlot EMPTY_SLOT = '{'0, -1, 'x, 'x}; // TODO: move as const to package
    

    typedef Word FetchGroup[FETCH_WIDTH];
    
    int fetchCtr = 0;
    int fqSize = 0, oqSize = 0, oooqSize;

    logic fetchAllow;
    logic resetPrev = 0, intPrev = 0, branchRedirect = 0, eventRedirect = 0;
    Word branchTarget = 'x, eventTarget = 'x, storedTarget = 'x;
    
    Stage ipStage = EMPTY_STAGE, fetchStage0 = EMPTY_STAGE, fetchStage1 = EMPTY_STAGE;
    Stage fetchQueue[$:FETCH_QUEUE_SIZE];
    
    Stage nextStage = EMPTY_STAGE;
    OpSlot opQueue[$:OP_QUEUE_SIZE];
    OpSlot memOp = EMPTY_SLOT, memOpPrev = EMPTY_SLOT, sysOp = EMPTY_SLOT, sysOpPrev = EMPTY_SLOT, lastCommitted = EMPTY_SLOT;
    OpSlot committedGroup[4] = '{default: EMPTY_SLOT};
    
    
        typedef struct {
            OpSlot op;
            logic done;
        }
        OpSlotExt;

    typedef struct {
        int id;
        logic done;
    }
    OpStatus;

    OpStatus oooQueue[$:OOO_QUEUE_SIZE];
    
    
    Word intRegs[32], floatRegs[32], sysRegs[32];
    
    int lastPerfCount = 0;
    string lastCommittedStr, oooqStr;
    logic cmp0, cmp1, dummy = '1;



    assign lastCommittedStr = TMP_disasm(lastCommitted.bits);

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

        advanceOOOQ();

        if (resetPrev | intPrev | branchRedirect | eventRedirect) begin
            performRedirect();
        end
        else begin
            fetchAndEnqueue();

            writeToOpQ(nextStage);
            writeToOOOQ(nextStage);

            memOp <= EMPTY_SLOT;
            memOpPrev <= memOp;

            sysOp <= EMPTY_SLOT;
            if (!sysOpPrev.active) sysOpPrev <= sysOp;

            if (interrupt) begin
                performInterrupt();
            end
            else begin
                if (memOpPrev.active) begin // Finish executing mem operation from prev cycle
                    performMemLater(memOpPrev);
                end
                if (sysOpPrev.active) begin // Finish executing sys operation from prev cycle
                    performSysLater(sysOpPrev);
                    sysOpPrev <= EMPTY_SLOT;
                end
                else if (memOp.active) begin
                end
                else if (sysOp.active) begin
                end
                else begin
                    automatic int performedCount = 0;
                    for (int i = 0; i < oqSize; i++) begin            
                        // scan until a mem, taken branch or system operation
                        automatic OpSlot op = opQueue.pop_front();
                        performedCount++;

                        if (isBranchOp(op)) begin
                            performBranch(op);
                            break;
                        end
                        if (isMemOp(op)) begin
                            performMemFirst(op);
                            break;
                        end
                        if (isSysOp(op)) begin
                            performSysFirst(op); 
                            break;
                        end
                        performRegularOp(op);
                        
                        if (performedCount == 4) break; // Limits Exec group size to 4 ops
                    end
                    lastPerfCount <= performedCount;
                end
                
            end

        end
        
        fqSize <= fetchQueue.size();
        oqSize <= opQueue.size();
        oooqSize <= oooQueue.size();
        
            //oooqStr <= $
            $swrite(oooqStr, "%p", oooQueue);
    end


    assign fetchAllow = fetchQueueAccepts(fqSize);
    assign insAdr = ipStage.baseAdr;


    function logic fetchQueueAccepts(input int k);
        return k <= FETCH_QUEUE_SIZE - 3 ? '1 : '0;
    endfunction
    
    function automatic Stage setActive(input Stage s, input logic on, input int ctr);
        Stage res = s;
        res.active = on;
        res.ctr = ctr;
        res.baseAdr = s.baseAdr & ~(4*FETCH_WIDTH-1);
        foreach (res.mask[i]) if ((s.baseAdr/4) % FETCH_WIDTH <= i) res.mask[i] = '1;
        return res;
    endfunction

    function automatic Stage setWords(input Stage s, input FetchGroup fg);
        Stage res = s;
        res.words = fg;
        return res;
    endfunction

    function automatic logic isBranchOp(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        return abs.def.o inside {O_jump};
    endfunction
    
    function automatic logic isMemOp(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        return abs.def.o inside {O_intLoadW, O_intLoadD, O_intStoreW, O_intStoreD, O_floatLoadW, O_floatStoreW};
    endfunction
    
    function automatic logic isSysOp(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        return abs.def.o inside {O_undef, O_call, O_sync, O_retE, O_retI, O_replay, O_halt, O_send,  O_sysStore};
    endfunction


    task automatic commitOp(input OpSlot op, input Word trg);
        lastCommitted <= op;
        storedTarget <= trg;
            
        updateOOOQ(op);
        TMP_commit(op);
    endtask
    
    task automatic performRedirect();
        if (resetPrev) begin
            ipStage <= '{'1, -1, 512, '{default: '0}, '{default: 'x}};
            TMP_reset();
        end
        else if (intPrev) begin
            ipStage <= '{'1, -1, eventTarget, '{default: '0}, '{default: 'x}};
            TMP_interrupt();
        end
        else if (eventRedirect)
            ipStage <= '{'1, -1, eventTarget, '{default: '0}, '{default: 'x}};
        else if (branchRedirect)
            ipStage <= '{'1, -1, branchTarget, '{default: '0}, '{default: 'x}};
        else $fatal("Should never get here");

        fetchStage0 <= EMPTY_STAGE;
        fetchStage1 <= EMPTY_STAGE;
        fetchQueue.delete();
        
        nextStage <= EMPTY_STAGE;
        opQueue.delete();
        oooQueue.delete();
        memOp <= EMPTY_SLOT;
        memOpPrev <= EMPTY_SLOT;
        sysOp <= EMPTY_SLOT;
        sysOpPrev <= EMPTY_SLOT;
          
        if (resetPrev) begin
            intRegs = '{0: '0, default: '0};
            floatRegs = '{default: '0};
            sysRegs = '{0: -1, 1: 0, default: '0};
        end
    endtask

    task automatic fetchAndEnqueue();
        if (fetchAllow) begin
            ipStage <= '{'1, -1, (ipStage.baseAdr & ~(4*FETCH_WIDTH-1)) + 4*FETCH_WIDTH, '{default: '0}, '{default: 'x}};
            fetchCtr <= fetchCtr + FETCH_WIDTH;
        end
        
        fetchStage0 <= setActive(ipStage, ipStage.active & fetchAllow, fetchCtr);
        fetchStage1 <= setWords(fetchStage0, insIn);
        
        if (fetchStage1.active) fetchQueue.push_back(fetchStage1);

        if (fqSize > 0 && oqSize < OP_QUEUE_SIZE - 2*FETCH_WIDTH) nextStage <= fetchQueue.pop_front();
        else nextStage <= EMPTY_STAGE;
        
    endtask

    task automatic performInterrupt();
            $display(">> Interrupt !!!");
    
        eventTarget <= IP_INT;
        storedTarget <= IP_INT;
        
        sysRegs[5] = sysRegs[1];
        sysRegs[1] |= 1; // TODO: handle state register correctly
        sysRegs[3] = storedTarget;
    endtask

    task automatic performBranch(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap[abs.fmt].typeSpec);

        // Resolve condition
        bit redirect = 0;
        Word brTarget;
        Word trg;

        Word result = calculateResult(abs, args, op.adr);

        case (abs.mnemonic)
            "ja", "jl": redirect = 1;
            "jz_i": redirect = (args[0] == 0);
            "jnz_i": redirect = (args[0] != 0);
            "jz_r": redirect = (args[0] == 0);
            "jnz_r": redirect = (args[0] != 0);
            default: $fatal("Wrong kind of branch");
        endcase

        intRegs[abs.dest] = result;
        intRegs[0] = 0;

        brTarget = (abs.mnemonic inside {"jz_r", "jnz_r"}) ? args[1] : op.adr + args[1];
        
        branchTarget <= brTarget;
        branchRedirect <= redirect;
        
        trg = redirect ? brTarget : op.adr + 4;
        
        commitOp(op, trg);
    endtask
    
    task automatic performMemFirst(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap[abs.fmt].typeSpec);

        readReq[0] <= '1;
        readAdr[0] <= args[0] + args[1];
        memOp <= op;
        
        if (abs.def.o inside {O_intStoreW, O_intStoreD, O_floatStoreW}) begin
            writeReq = 1;
            writeAdr = args[0] + args[1];
            writeOut = args[2];
        end
    endtask

    task automatic performSysFirst(input OpSlot op);
        sysOp <= op;
    endtask

    task automatic performMemLater(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        // If load, write to register; if store, nothing to do
        if (abs.def.o inside {O_intLoadW, O_intLoadD}) begin
            intRegs[abs.dest] = readIn[0];
            intRegs[0] = 0;
        end
        else if (abs.def.o inside {O_floatLoadW}) begin
            floatRegs[abs.dest] = readIn[0];
        end

        commitOp(op, op.adr + 4);
    endtask

        task automatic performSysLater(input OpSlot op);
            performSys(op);
        endtask


    task automatic performSys(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap[abs.fmt].typeSpec);
    
        LateEvent lateEvt = getLateEvent(op, abs, sysRegs[2], sysRegs[3]);
        Word trg = lateEvt.redirect ? lateEvt.target : op.adr + 4;

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
        
        eventTarget <= lateEvt.target;
        eventRedirect <= lateEvt.redirect;
    
        commitOp(op, trg);
    
        sig <= lateEvt.sig;
        wrong <= lateEvt.wrong;
    endtask


    task automatic performRegularOp(input OpSlot op);//, input AbstractInstruction abs, input Word3 args);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap[abs.fmt].typeSpec);

        Word result = calculateResult(abs, args, op.adr);
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
        
         commitOp(op, op.adr + 4);
    endtask        

        task automatic writeToOpQ(input Stage st);
            if (st.active) begin
                foreach (st.words[i])
                    if (st.mask[i]) opQueue.push_back('{'1, st.ctr + i, st.baseAdr + 4*i, st.words[i]});
            end
        endtask

        task automatic writeToOOOQ(input Stage st);
            if (st.active) begin
                foreach (st.words[i])
                    if (st.mask[i]) oooQueue.push_back('{st.ctr + i, '0})
                    ;
            end
        endtask

        task automatic updateOOOQ(input OpSlot op);
            const int ind[$] = oooQueue.find_index with (item.id == op.id);
            assert (ind.size() > 0)
                oooQueue[ind[0]].done = '1;
            else
                $error("No such id in OOOQ: %d", op.id);
            
        endtask

        task automatic advanceOOOQ();
            while (oooQueue.size() > 0 && oooQueue[0].done == 1) begin
                oooQueue.pop_front();
            end
        endtask

endmodule
