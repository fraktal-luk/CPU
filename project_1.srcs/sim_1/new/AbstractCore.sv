
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
    
    logic dummy = '1;

    typedef int InsId;

    typedef struct {
        int id;
        Word adr;
        Word bits;
        Word target;
    } InstructionInfo;
    
    function automatic InstructionInfo makeInsInfo(input OpSlot op);
        InstructionInfo res;
        res.id = op.id;
        res.adr = op.adr;
        res.bits = op.bits;
        
        return res;
    endfunction
    
    
    InstructionInfo insMap[int]; // structure holding all instructions in flight (beginning at Fetch), and possibly some more, as a database
    int insMapSize = 0, nCommitted = 0, nRetired = 0;
    
    InsId intWritersR[32] = '{default: -1}, floatWritersR[32] = '{default: -1};
    InsId intWritersC[32] = '{default: -1}, floatWritersC[32] = '{default: -1};
    
//    InsId reg1r, reg1c;
    
//        assign re1r = intWritersR[1];
//        assign re1c = intWritersC[1];
    
    
    function automatic void setTarget(input int id, input Word trg);
        insMap[id].target = trg;
    endfunction


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
    
    typedef OpSlot OpSlot4[4];

    typedef struct {
        int num;
        OpSlot regular[4];
        OpSlot branch;
        OpSlot mem;
        OpSlot sys;
    } IssueGroup;
    
    const IssueGroup DEFAULT_ISSUE_GROUP = '{num: 0, regular: '{default: EMPTY_SLOT}, branch: EMPTY_SLOT, mem: EMPTY_SLOT, sys: EMPTY_SLOT};

    typedef Word FetchGroup[FETCH_WIDTH];
    
    int fetchCtr = 0;
    int fqSize = 0, oqSize = 0, oooqSize = 0, committedNum = 0, frontCompleted = 0;

    logic fetchAllow;
    logic resetPrev = 0, intPrev = 0, branchRedirect = 0, eventRedirect = 0;
    Word branchTarget = 'x, eventTarget = 'x, storedTarget = 'x, committedTarget = 'x, retiredTarget = 'x;
    
    Stage ipStage = EMPTY_STAGE, fetchStage0 = EMPTY_STAGE, fetchStage1 = EMPTY_STAGE;
    Stage fetchQueue[$:FETCH_QUEUE_SIZE];
    
    Stage nextStage = EMPTY_STAGE;
    OpSlot opQueue[$:OP_QUEUE_SIZE];
    OpSlot memOp = EMPTY_SLOT, memOpPrev = EMPTY_SLOT, sysOp = EMPTY_SLOT, sysOpPrev = EMPTY_SLOT, lastCommitted = EMPTY_SLOT, lastRetired = EMPTY_SLOT;
    IssueGroup issuedSt0 = DEFAULT_ISSUE_GROUP, issuedSt0_C = DEFAULT_ISSUE_GROUP, issuedSt1 = DEFAULT_ISSUE_GROUP, issuedSt1_C = DEFAULT_ISSUE_GROUP;
    
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
    
    string lastCommittedStr, lastRetiredStr, oooqStr;
    logic cmp0, cmp1;

        assign cmp0 = (intWritersR[1] == intWritersC[1]);

    assign lastCommittedStr = TMP_disasm(lastCommitted.bits);
    assign lastRetiredStr = TMP_disasm(lastRetired.bits);

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

        advanceOOOQ();
                
            issuedSt0 <= DEFAULT_ISSUE_GROUP;
            issuedSt1 <= issuedSt0;

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
                automatic IssueGroup igIssue = DEFAULT_ISSUE_GROUP, igExec = DEFAULT_ISSUE_GROUP;// = issuedSt0;
            
                if (memOpPrev.active) begin // Finish executing mem operation from prev cycle
                    performMemLater(memOpPrev);
                end
                else if (sysOpPrev.active) begin // Finish executing sys operation from prev cycle
                    performSysLater(sysOpPrev);
                    sysOpPrev <= EMPTY_SLOT;
                end
                else if (memOp.active || issuedSt0.mem.active || issuedSt1.mem.active
                        ) begin
                end
                else if (sysOp.active || issuedSt0.sys.active || issuedSt1.sys.active
                        ) begin
                end
                else begin
                    igIssue = issueFromOpQ(opQueue, oqSize);
                    igExec = igIssue;
                end
                        igExec = issuedSt1;
                    issuedSt0 <= igIssue;
 
                    foreach (igExec.regular[i]) begin
                        if (igExec.regular[i].active) performRegularOp(igExec.regular[i]);
                    end
                
                    if (igExec.branch.active) performBranch(igExec.branch);
                    else if (igExec.mem.active) performMemFirst(igExec.mem);
                    else if (igExec.sys.active) performSysFirst(igExec.sys);
                //end
                
            end

        end
        
        fqSize <= fetchQueue.size();
        oqSize <= opQueue.size();
        oooqSize <= oooQueue.size();
        frontCompleted <= countFrontCompleted();
        
        begin
            automatic OpStatus oooqDone[$] = (oooQueue.find with (item.done == 1));
            committedNum <= oooqDone.size();
            
                assert (oooqDone.size() <= 4) else $error("How 5?");
        end
        
        insMapSize = insMap.size();
        
            $swrite(oooqStr, "%p", oooQueue);
    end


    assign fetchAllow = fetchQueueAccepts(fqSize);
    assign insAdr = ipStage.baseAdr;


    function logic fetchQueueAccepts(input int k);
        return k <= FETCH_QUEUE_SIZE - 3 ? '1 : '0;
    endfunction


    task automatic addToInsBase(input Stage s, input logic on, input int ctr);
        Stage st = setActive(s, on, ctr);
        
        if (st.active) begin
            foreach (st.words[i])
                if (st.mask[i]) begin
                    insMap[st.ctr + i] = makeInsInfo('{'1, st.ctr + i, st.baseAdr + 4*i, st.words[i]});
                end
        end
    endtask

    task automatic updateInsEncodings(input Stage s);
        Stage st = s;
        
        if (st.active) begin
            foreach (st.words[i])
                if (st.mask[i]) begin
                    insMap[st.ctr + i].bits = s.words[i];
                end
        end
    endtask


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


    task automatic commitOp(input OpSlot op, input Word trg);
        lastCommitted <= op;
        storedTarget <= trg;
        committedTarget <= trg;
            
        updateOOOQ(op);
          //TMP_commit(op); // 
        nCommitted++;
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
        
            issuedSt0 <= DEFAULT_ISSUE_GROUP;
            issuedSt1 <= DEFAULT_ISSUE_GROUP;
        
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
        
        addToInsBase(ipStage, ipStage.active & fetchAllow, fetchCtr);
        fetchStage0 <= setActive(ipStage, ipStage.active & fetchAllow, fetchCtr);
        
        updateInsEncodings(setWords(fetchStage0, insIn));
        fetchStage1 <= setWords(fetchStage0, insIn);

        if (fetchStage1.active) fetchQueue.push_back(fetchStage1);

        if (fqSize > 0 && oqSize < OP_QUEUE_SIZE - 2*FETCH_WIDTH) begin
            Stage toRename = fetchQueue.pop_front();
            mapStageAtRename(toRename);
            nextStage <= toRename;
        end
        else nextStage <= EMPTY_STAGE;
        
    endtask


    task automatic mapOpAtRename(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        if (writesIntReg(op)) intWritersR[abs.dest] = op.id;
        if (writesFloatReg(op)) floatWritersR[abs.dest] = op.id;
        intWritersR[0] = -1;            
    endtask

    task automatic mapOpAtCommit(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        if (writesIntReg(op)) intWritersC[abs.dest] = op.id;
        if (writesFloatReg(op)) floatWritersC[abs.dest] = op.id;
        intWritersC[0] = -1;            
    endtask

    task automatic mapStageAtRename(input Stage st);
        foreach (st.mask[i])
            if (st.mask[i]) begin
                OpSlot op = '{'1, st.ctr + i, st.baseAdr + 4*i, st.words[i]};
                mapOpAtRename(op);
            end 
    endtask


    task automatic restoreWriters();
        intWritersR = intWritersC;
        floatWritersR = floatWritersC;
    endtask


    task automatic performInterrupt();
            $display(">> Interrupt !!!");
    
        eventTarget <= IP_INT;
        storedTarget <= IP_INT;
        committedTarget <= IP_INT;
        
        sysRegs[5] = sysRegs[1];
        sysRegs[1] |= 1; // TODO: handle state register correctly
        sysRegs[3] = committedTarget;
    endtask

    task automatic performBranch(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap[abs.fmt].typeSpec);

        bit redirect = 0;
        Word brTarget;
        Word trg;

        Word result = calculateResult(abs, args, op.adr);
        
        // Resolve condition
        case (abs.mnemonic)
            "ja", "jl": redirect = 1;
            "jz_i": redirect = (args[0] == 0);
            "jnz_i": redirect = (args[0] != 0);
            "jz_r": redirect = (args[0] == 0);
            "jnz_r": redirect = (args[0] != 0);
            default: $fatal("Wrong kind of branch");
        endcase

        writeIntReg(abs.dest, result);
//        intRegs[abs.dest] = result;
//        intRegs[0] = 0;

        brTarget = (abs.mnemonic inside {"jz_r", "jnz_r"}) ? args[1] : op.adr + args[1];
        
        branchTarget <= brTarget;
        branchRedirect <= redirect;
        
        trg = redirect ? brTarget : op.adr + 4;
        
        setTarget(op.id, trg);
        commitOp(op, trg);
    endtask
    
    
    task automatic performMemFirst(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, '{default: 'x}, abs.sources, parsingMap[abs.fmt].typeSpec);

        readReq[0] <= '1;
        readAdr[0] <= args[0] + args[1];
        memOp <= op;
        
        if (isStoreNonSys(op)) begin
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

        if (abs.def.o inside {O_intLoadW, O_intLoadD})
            writeIntReg(abs.dest, readIn[0]);
        else if (abs.def.o inside {O_floatLoadW})
            writeFloatReg(abs.dest, readIn[0]);

        setTarget(op.id, op.adr + 4);
        commitOp(op, op.adr + 4);
    endtask

    task automatic performSysLater(input OpSlot op);
        performSys(op);
    endtask

    task automatic writeIntReg(input Word regNum, input Word value);
        if (regNum == 0) return;
        intRegs[regNum] = value;
    endtask

    task automatic writeFloatReg(input Word regNum, input Word value);
        floatRegs[regNum] = value;
    endtask

    task automatic writeSysReg(input Word regNum, input Word value);
        sysRegs[regNum] = value;
    endtask

    task automatic setLateEvent(input LateEvent evt);
        eventTarget <= evt.target;
        eventRedirect <= evt.redirect;
        sig <= evt.sig;
        wrong <= evt.wrong;
    endtask

    task automatic performSys(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);
    
        LateEvent lateEvt = getLateEvent(op, abs, sysRegs[2], sysRegs[3]);
        Word trg = lateEvt.redirect ? lateEvt.target : op.adr + 4;

        case (abs.def.o)
            O_sysStore: writeSysReg(args[1], args[2]);
            O_halt: $error("halt not implemented");
            default: ;                            
        endcase

        setTarget(op.id, trg);
        commitOp(op, trg);

        modifySysRegs(sysRegs, op, abs);
        setLateEvent(lateEvt);
    endtask


    task automatic performRegularOp(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(intRegs, floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);

        Word result = calculateResult(abs, args, op.adr);
        if (abs.def.o == O_sysLoad) result = sysRegs[args[1]];

        if (writesIntReg(op)) writeIntReg(abs.dest, result);
        if (writesFloatReg(op)) writeFloatReg(abs.dest, result);
        
        setTarget(op.id, op.adr + 4);
        commitOp(op, op.adr + 4);
    endtask        

    task automatic writeToOpQ(input Stage st);
        if (st.active) begin
            foreach (st.words[i]) if (st.mask[i]) opQueue.push_back('{'1, st.ctr + i, st.baseAdr + 4*i, st.words[i]});
        end
    endtask

    task automatic writeToOOOQ(input Stage st);
        if (st.active) begin
            foreach (st.words[i]) if (st.mask[i]) oooQueue.push_back('{st.ctr + i, '0});
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
            OpStatus opSt = oooQueue.pop_front();
            InstructionInfo insInfo = insMap[opSt.id];
            OpSlot op = '{1, insInfo.id, insInfo.adr, insInfo.bits};
            assert (op.id == opSt.id) //$display("%p", op); 
                                else $error("wrong retirement: %p / %p", opSt, op);
            mapOpAtCommit(op);
                TMP_commit(op);
        
            lastRetired <= op;
            retiredTarget <= insInfo.target; 
            nRetired++;
        end
    endtask


    function automatic IssueGroup issueFromOpQ(ref OpSlot queue[$:OP_QUEUE_SIZE], input int size);
        OpSlot q[$:OP_QUEUE_SIZE] = queue;
        int remainingSize = size;
    
        IssueGroup res = DEFAULT_ISSUE_GROUP;
        for (int i = 0; i < 4; i++) begin
            if (remainingSize > 0) begin
                OpSlot op = queue.pop_front();
                remainingSize--;
                res.num++;
                
                if (isBranchOp(op)) begin
                    res.branch = op;
                    break;
                end
                else if (isMemOp(op)) begin
                    res.mem = op;
                    break;
                end
                else if (isSysOp(op)) begin
                    res.sys = op;
                    break;
                end
                
                res.regular[i] = op;
            end
        end
        
        return res;
    endfunction
    
        // How many in front are ready to commit
        function automatic int countFrontCompleted();
            //int cnt = 0;
            foreach (oooQueue[i]) begin
                if (!oooQueue[i].done) return i;
            end
            return oooQueue.size();
        endfunction
    
endmodule
