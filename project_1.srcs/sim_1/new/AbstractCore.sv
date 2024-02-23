
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
    
    logic dummy = '0;

    typedef int InsId;

    typedef struct {
        int id;
        Word adr;
        Word bits;
        Word target;
        Word result;
    } InstructionInfo;
    
    function automatic InstructionInfo makeInsInfo(input OpSlot op);
        InstructionInfo res;
        res.id = op.id;
        res.adr = op.adr;
        res.bits = op.bits;
        
        return res;
    endfunction
    
    
    InstructionInfo insMap[int]; // structure holding all instructions in flight (beginning at Fetch), and possibly some more, as a database
    int insMapSize = 0, nRenamed = 0, nCommitted = 0, nRetired = 0;
    

    function automatic void setTarget(input int id, input Word trg);
        insMap[id].target = trg;
    endfunction

    function automatic void setResult(input int id, input Word res);
        insMap[id].result = res;
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
    
    typedef OpSlot OpSlotA[FETCH_WIDTH];


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
    Word branchTarget = 'x, eventTarget = 'x;
    
    Stage ipStage = EMPTY_STAGE, fetchStage0 = EMPTY_STAGE, fetchStage1 = EMPTY_STAGE;
    Stage fetchQueue[$:FETCH_QUEUE_SIZE];
    
    Stage nextStage = EMPTY_STAGE;
    OpSlotA nextStageA = '{default: EMPTY_SLOT};
    OpSlot opQueue[$:OP_QUEUE_SIZE];
    OpSlot memOp = EMPTY_SLOT, memOpPrev = EMPTY_SLOT, sysOp = EMPTY_SLOT, sysOpPrev = EMPTY_SLOT, lastRenamed = EMPTY_SLOT, lastCommitted = EMPTY_SLOT, lastRetired = EMPTY_SLOT;
    IssueGroup issuedSt0 = DEFAULT_ISSUE_GROUP, issuedSt0_C = DEFAULT_ISSUE_GROUP, issuedSt1 = DEFAULT_ISSUE_GROUP, issuedSt1_C = DEFAULT_ISSUE_GROUP;
    
    OpSlot committedGroup[4] = '{default: EMPTY_SLOT};


    typedef struct {
        int id;
        logic done;
    }
    OpStatus;

    OpStatus oooQueue[$:OOO_QUEUE_SIZE];
    
    CpuState renamedState, execState, retiredState;
    SimpleMem renamedMem = new(), execMem = new(), retiredMem = new();
    
    InsId intWritersR[32] = '{default: -1}, floatWritersR[32] = '{default: -1};
    InsId intWritersC[32] = '{default: -1}, floatWritersC[32] = '{default: -1};
    
    
    string lastRenamedStr, lastCommittedStr, lastRetiredStr, oooqStr;
    logic cmp0, cmp1;
    Word cmpw0, cmpw1, cmpw2, cmpw3;
        
            always @(posedge clk) cmp0 = (retiredState.target == branchTarget);
            always @(posedge clk) cmp1 = (retiredState.target == eventTarget);
        
        always @(posedge clk) cmpw1 <= execMem.loadW(32);
        always @(posedge clk) cmpw2 <= retiredMem.loadW(32);
        always @(posedge clk) cmpw3 <= execMem.loadW(0);



    assign lastRenamedStr = disasm(lastRenamed.bits);
    assign lastCommittedStr = disasm(lastCommitted.bits);
    assign lastRetiredStr = disasm(lastRetired.bits);

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

            writeToOpQ(nextStage, nextStageA);
            writeToOOOQ(nextStage);

            memOp <= EMPTY_SLOT;
            memOpPrev <= memOp;

            sysOp <= EMPTY_SLOT;
            if (!sysOpPrev.active) sysOpPrev <= sysOp;

            if (reset) begin
                execReset();
            end
            else if (interrupt) begin
                execInterrupt();
            end
            else begin
                automatic IssueGroup igIssue = DEFAULT_ISSUE_GROUP, igExec = DEFAULT_ISSUE_GROUP;// = issuedSt0;
            
                if (memOpPrev.active) begin // Finish executing mem operation from prev cycle
                    execMemLater(memOpPrev);
                end
                else if (sysOpPrev.active) begin // Finish executing sys operation from prev cycle
                    execSysLater(sysOpPrev);
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
                    if (igExec.regular[i].active) execRegular(igExec.regular[i]);
                end
            
                if (igExec.branch.active)execBranch(igExec.branch);
                else if (igExec.mem.active) execMemFirst(igExec.mem);
                else if (igExec.sys.active) execSysFirst(igExec.sys);
            
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
        
            cmpw0 <= cmpRegs(execState.intRegs, retiredState.intRegs);
    end

            
        function automatic Word cmpRegs(input Word arr0[32], input Word arr1[32]);
            Word res;
            foreach (arr0[i])
                res = (res << 1) | (arr0[31-i] == arr1[31-i]);
            return res; 
        endfunction
            

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
        updateOOOQ(op);
        nCommitted++;
    endtask
    
    task automatic performRedirect();
        if (resetPrev) begin
            ipStage <= '{'1, -1, eventTarget, '{default: '0}, '{default: 'x}};
            TMP_reset();
        end
        else if (intPrev) begin
            ipStage <= '{'1, -1, eventTarget, '{default: '0}, '{default: 'x}};
            TMP_interrupt();
        end
        else if (eventRedirect) begin
            ipStage <= '{'1, -1, eventTarget, '{default: '0}, '{default: 'x}};
        end
        else if (branchRedirect) begin
            ipStage <= '{'1, -1, branchTarget, '{default: '0}, '{default: 'x}};
        end
        else $fatal("Should never get here");
            
        if (eventRedirect || intPrev || resetPrev) begin
            renamedState = retiredState;
            execState = retiredState;
            execMem = retiredMem;
        end

        fetchStage0 <= EMPTY_STAGE;
        fetchStage1 <= EMPTY_STAGE;
        fetchQueue.delete();
        
        nextStage <= EMPTY_STAGE;
        nextStageA <= '{default: EMPTY_SLOT};
        opQueue.delete();
        oooQueue.delete();
        
        issuedSt0 <= DEFAULT_ISSUE_GROUP;
        issuedSt1 <= DEFAULT_ISSUE_GROUP;
    
        memOp <= EMPTY_SLOT;
        memOpPrev <= EMPTY_SLOT;
        sysOp <= EMPTY_SLOT;
        sysOpPrev <= EMPTY_SLOT;

        if (resetPrev) begin
            renamedState = initialState(IP_RESET);
            renamedMem.reset();

            execState = initialState(IP_RESET);
            execMem.reset();

            retiredState = initialState(IP_RESET);
            retiredMem.reset();
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
            
            foreach (toRename.words[i])
                if (toRename.mask[i]) begin
                    OpSlot currentOp = makeOp(toRename, i);
                    performAt(renamedState, renamedMem, currentOp);
                    lastRenamed <= currentOp;
                    nRenamed++;
                end
                
            nextStage <= toRename;
            nextStageA <= makeOpA(toRename);
        end
        else begin
            nextStage <= EMPTY_STAGE;
            nextStageA <= '{default: EMPTY_SLOT};
        end
        
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



    task automatic execReset();    
        eventTarget <= IP_RESET;

        performAsyncEvent(renamedState, IP_RESET);
        performAsyncEvent(execState, IP_RESET);
        performAsyncEvent(retiredState, IP_RESET);  
    endtask

    task automatic execInterrupt();
        $display(">> Interrupt !!!");
        eventTarget <= IP_INT;

        performAsyncEvent(renamedState, IP_INT);
        performAsyncEvent(execState, IP_INT);
        performAsyncEvent(retiredState, IP_INT);
    endtask

    task automatic performLink(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word result = op.adr + 4;
        writeIntReg(state, abs.dest, result);
    endtask

    task automatic setBranch(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        ExecEvent evt = resolveBranch(state, abs, op.adr);
        
        state.target = evt.redirect ? evt.target : op.adr + 4;
    endtask

    // TODO: accept Event as arg?
    task automatic setExecEvent(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        ExecEvent evt = resolveBranch(state, abs, op.adr);
        
        branchTarget <= evt.target;
        branchRedirect <= evt.redirect;
    endtask

    task automatic performBranch(ref CpuState state, input OpSlot op);
        setBranch(state, op);
        performLink(state, op);
    endtask


    task automatic performRegularOp(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(state.intRegs, state.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);

        Word result = calculateResult(abs, args, op.adr);
        if (abs.def.o == O_sysLoad) result = state.sysRegs[args[1]];

        if (writesIntReg(op)) writeIntReg(state, abs.dest, result);
        if (writesFloatReg(op)) writeFloatReg(state, abs.dest, result);
        
        state.target = op.adr + 4;
    endtask    

    
    task automatic performMemFirst(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(state.intRegs, state.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);

        // TODO: make struct, unpack a assigment to ports
        readReq[0] <= '1;
        readAdr[0] <= args[0] + args[1];
        memOp <= op;
        
        if (isStoreMemOp(op)) begin
            // TODO: make struct, unpack a assigment to ports 
            writeReq = 1;
            writeAdr = args[0] + args[1];
            writeOut = args[2];
        end
    endtask

    task automatic performMemLater(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);

        if (writesIntReg(op)) writeIntReg(state, abs.dest, readIn[0]);
        if (writesFloatReg(op)) writeFloatReg(state, abs.dest, readIn[0]);
        
        state.target = op.adr + 4;
    endtask

        task automatic performMemTMP_SIM(ref CpuState state, input OpSlot op, ref SimpleMem mem);
            AbstractInstruction abs = decodeAbstract(op.bits);
            Word3 args = getArgs(state.intRegs, state.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);

            Word adr = calculateEffectiveAddress(abs, args);
            Word result = getLoadValue(abs, adr, mem, state);
            
            if (isLoadOp(op))
                assert (result == readIn[0]) else $error("Load diff: %d %d // adr %d, in: %p", result, readIn[0], adr, abs);
            
            if (isStoreMemOp(op)) mem.storeW(adr, args[2]);
                
            //if (writesIntReg(op)) writeIntReg(state, abs.dest, result);
            //if (writesFloatReg(op)) writeFloatReg(state, abs.dest, result);
            
            state.target = op.adr + 4;
        endtask

        task automatic performMemAll__(ref CpuState state, input OpSlot op, ref SimpleMem mem);
            AbstractInstruction abs = decodeAbstract(op.bits);
            Word3 args = getArgs(state.intRegs, state.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);

            Word adr = calculateEffectiveAddress(abs, args);
            Word result = getLoadValue(abs, adr, mem, state);
            
            if (isStoreMemOp(op)) mem.storeW(adr, args[2]);
                
            if (writesIntReg(op)) writeIntReg(state, abs.dest, result);
            if (writesFloatReg(op)) writeFloatReg(state, abs.dest, result);
            
            state.target = op.adr + 4;
        endtask



    task automatic performSysStore(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(state.intRegs, state.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);

        writeSysReg(state, args[1], args[2]);
        state.target = op.adr + 4;
    endtask

    task automatic performSys(ref CpuState state, input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);

        case (abs.def.o)
            O_sysStore: performSysStore(state, op);
            O_halt: $error("halt not implemented");
            default: ;                            
        endcase

        modifySysRegs(state, op.adr, abs);
    endtask


    task automatic execBranch(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word result = op.adr + 4;
        ExecEvent evt = resolveBranch(execState, abs, op.adr);
        Word trg = evt.redirect ? evt.target : op.adr + 4;

        setExecEvent(execState, op);
        performBranch(execState, op);

        setResult(op.id, result);
        setTarget(op.id, trg);
        commitOp(op, trg);
    endtask


    task automatic execMemFirst(input OpSlot op);
        performMemFirst(execState, op);
    endtask

    task automatic execMemLater(input OpSlot op);
        performMemLater(execState, op);
        
        performMemTMP_SIM(execState, op, execMem);

        setResult(op.id, readIn[0]);
        setTarget(op.id, op.adr + 4);
        commitOp(op, op.adr + 4);
    endtask


    task automatic execSysFirst(input OpSlot op);
        sysOp <= op;
    endtask

    task automatic execSysLater(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        LateEvent lateEvt = getLateEvent(op, abs, execState.sysRegs[2], execState.sysRegs[3]);
        Word trg = lateEvt.redirect ? lateEvt.target : op.adr + 4;
        
        setTarget(op.id, trg);
        commitOp(op, trg);
    endtask


    task automatic execRegular(input OpSlot op);
        AbstractInstruction abs = decodeAbstract(op.bits);
        Word3 args = getArgs(execState.intRegs, execState.floatRegs, abs.sources, parsingMap[abs.fmt].typeSpec);
        Word result = calculateResult(abs, args, op.adr);
        if (abs.def.o == O_sysLoad) result = execState.sysRegs[args[1]];
        performRegularOp(execState, op);
        
        setResult(op.id, result);
        setTarget(op.id, op.adr + 4);
        commitOp(op, op.adr + 4);
    endtask



    task automatic performAt(ref CpuState state, ref SimpleMem mem, input OpSlot op);
        if (isBranchOp(op)) begin
            performBranch(state, op);
        end
        else if (isMemOp(op)) begin
            performMemAll__(state, op, mem);
        end
        else if (isSysOp(op)) begin
            performSys(state, op);
        end
        else
            performRegularOp(state, op);
    endtask


        function automatic OpSlot makeOp(input Stage st, input int i);
            if (!st.active) return EMPTY_SLOT;
        
            return '{1, st.ctr + i, st.baseAdr + 4*i, st.words[i]};
        endfunction

        function automatic OpSlotA makeOpA(input Stage st);
            OpSlotA res = '{default: EMPTY_SLOT};
            if (!st.active) return res;

            foreach (st.words[i])
                if (st.mask[i])
                    res[i] = makeOp(st, i);
            return res;
        endfunction

    task automatic writeToOpQ(input Stage st, input OpSlotA sa);
        if (st.active)
            foreach (st.words[i])
                if (st.mask[i]) begin
                    assert (makeOp(st, i) === sa[i]) else $error("quuuu! %p %p", makeOp(st, i), sa[i]);
                    opQueue.push_back(makeOp(st, i));
                end
        return;
        
            //if (st.active) begin
                foreach (sa[i]) if (sa[i].active) opQueue.push_back(sa[i]);
            //end
        
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
        if (eventRedirect || interrupt || reset) return;
    
        while (oooQueue.size() > 0 && oooQueue[0].done == 1) begin
            OpStatus opSt = oooQueue.pop_front();
            InstructionInfo insInfo = insMap[opSt.id];
            OpSlot op = '{1, insInfo.id, insInfo.adr, insInfo.bits};
            AbstractInstruction abs = decodeAbstract(op.bits);
            assert (op.id == opSt.id) else 
                    $error("wrong retirement: %p / %p", opSt, op);

            mapOpAtCommit(op);
            TMP_commit(op);

            if (isSysOp(op)) begin
                 setLateEvent__(execState, op);
                 performSys(execState, op);
            end
            
            performAt(retiredState, retiredMem, op);
        
            lastRetired <= op;
            nRetired++;
            
            if (isSysOp(op)) break;
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
        foreach (oooQueue[i]) begin
            if (!oooQueue[i].done) return i;
        end
        return oooQueue.size();
    endfunction

    task automatic setLateEvent(ref CpuState state, input LateEvent evt);    
        eventTarget <= evt.target;
        eventRedirect <= evt.redirect;
        sig <= evt.sig;
        wrong <= evt.wrong;
    endtask

    task automatic setLateEvent__(ref CpuState state, input OpSlot op);    
        AbstractInstruction abs = decodeAbstract(op.bits);
        LateEvent lateEvt = getLateEvent(op, abs, state.sysRegs[2], state.sysRegs[3]);

        setLateEvent(state, lateEvt);
    endtask    

endmodule
