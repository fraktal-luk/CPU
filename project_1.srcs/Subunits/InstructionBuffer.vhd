----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;

use work.CoreConfig.all;

use work.PipelineGeneral.all;

use work.LogicFront.all;


entity InstructionBuffer is
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSending: in std_logic;
		nextAccepting: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;

		stageDataIn: in InstructionSlotArray(0 to FETCH_WIDTH-1);
		acceptingOut: out std_logic;
		sendingOut: out std_logic;
		stageDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1)
	);
end InstructionBuffer;



architecture Implem of InstructionBuffer is
	
	signal queueData, queueDataNext: InstructionSlotArray(0 to IBUFFER_SIZE-1)
								:= (others => DEFAULT_INSTRUCTION_SLOT);
    signal sending: std_logic := '0';
    
    signal fullMask: std_logic_vector(0 to IBUFFER_SIZE-1) := (others => '0');
    
    function bufferDataNext(content: InstructionSlotArray; newContent: InstructionSlotArray;
                            nextAccepting, prevSending, kill: std_logic)
    return InstructionSlotArray is
        constant LEN: positive := content'length;
        variable res: InstructionSlotArray(0 to LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        variable fullMask, fillMask, remainingMask, nextMask: std_logic_vector(0 to LEN-1) := (others => '0');
        variable remainingMaskExt: std_logic_vector(0 to LEN + FETCH_WIDTH - 1) := (others => '0');
        variable inputMask, inputMaskComp: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
    begin
        fullMask := extractFullMask(content);
        inputMask := extractFullMask(newContent);
        
        
        inputMaskComp := compactMask(inputMask);
        
        if nextAccepting = '1' then -- sending; shift mask by 4
            remainingMaskExt(0 to LEN - 1) := fullMask;
            remainingMaskExt(0 to 3) := (others => '1');
        else
            remainingMaskExt(4 to LEN + 3) := fullMask;
            remainingMaskExt(0 to 3) := (others => '1');
        end if;
        
        for i in 0 to LEN-1 loop       
            if remainingMaskExt(i + 4) = '1' then  -- !! equivalent to remainingMask(i), where '1' for i < 0    
                if nextAccepting = '1' and i + 4 < LEN then
                    res(i).ins := content(i+4).ins;
                else
                    res(i).ins := content(i).ins;
                end if;
            else
                res(i) := getNewElem(remainingMaskExt(i+1 to i+3), newContent);
            end if;
            
            res(i).ins.controlInfo.newEvent := '0'; -- Separating front events from exec events
                                         --  Meanwhile, branch taken/not taken state must be retained         
            fillMask(i) := '0';
            for k in 0 to 3 loop -- Further beyond end requires more ful inputs to be filled:
                --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
                fillMask(i) := fillMask(i) or (remainingMaskExt(i + 3-k) and inputMaskComp(k));
            end loop;
            
            res(i).full := (remainingMaskExt(i + 4) or (fillMask(i) and prevSending)) and not kill;
        end loop;
        
        if CLEAR_DEBUG_INFO then    
            for i in 0 to IBUFFER_SIZE-1 loop
                res(i).ins := clearAbstractInfo(res(i).ins);
                res(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;
            end loop;
        end if;

        return res;
    end function;

    -- Not compacting, just treating each input group as 4-wide, only adjusting to left within fetch group
    function bufferDataNext_New(content: InstructionSlotArray; newContent: InstructionSlotArray;
                            nextAccepting, prevSending, kill: std_logic)
    return InstructionSlotArray is
        constant LEN: positive := content'length;
        variable res: InstructionSlotArray(0 to LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        variable fullMask, fillMask, remainingMask, nextMask: std_logic_vector(0 to LEN-1) := (others => '0');
        variable remainingMaskExt: std_logic_vector(0 to LEN + 3) := (others => '0');
        variable inputMask, inputMaskComp, inputMaskAdj: std_logic_vector(0 to FETCH_WIDTH-1) := (others => '0');
        variable inputMaskTmp: std_logic_vector(0to 2*FETCH_WIDTH-1) := (others => '0');
        variable newContentAdj: InstructionSlotArray(0 to FETCH_WIDTH-1) := (others => DEFAULT_INS_SLOT);
        variable newContentTmp: InstructionSlotArray(0 to 2*FETCH_WIDTH-1) := newContent & newContent;
        variable nShift, j: integer := 0;
        variable fillHere: boolean := false;
    begin
    
        fullMask := extractFullMask(content);
        inputMask := extractFullMask(newContent);

        remainingMaskExt(0 to LEN-1) := fullMask;
        
        inputMaskTmp(0 to FETCH_WIDTH-1) := inputMask;
    
        inputMaskComp := compactMask(inputMask);        
    
        nShift := getFirstOnePosition(inputMask);
        if isNonzero(inputMask) = '0' then
            nShift := 0;
        end if; 
        
        for i in 0 to FETCH_WIDTH-1 loop
            newContentAdj(i) := newContentTmp(nShift + i);
            inputMaskAdj(i) := inputMaskTmp(nShift + i);
        end loop;

        
--        if nextAccepting = '1' then -- sending; shift mask by 4
--            remainingMaskExt(0 to LEN - 1) := fullMask;
--            remainingMaskExt(0 to 3) := (others => '1');
--        else
--            remainingMaskExt(4 to LEN + 3) := fullMask;
--            remainingMaskExt(0 to 3) := (others => '1');
--        end if;
        
        for i in 0 to LEN/FETCH_WIDTH - 1 loop
            fillHere := false;
            j := i*FETCH_WIDTH;
            if nextAccepting = '1' then
                remainingMask(j) := remainingMaskExt(j + FETCH_WIDTH);
            else
                remainingMask(j) := remainingMaskExt(j);
            end if;
            
            if remainingMask(j) = '0' then
                if prevSending = '1' and j = 0 then
                    fillHere := true; 
                elsif prevSending = '1' and j > 0 and remainingMask(j - FETCH_WIDTH) = '1' then
                    fillHere := true;
                end if;
            end if;
  
            if fillHere then
                for k in 0 to FETCH_WIDTH-1 loop
                    res(j + k) := newContentAdj(k);
                    res(j + k).ins.controlInfo.newEvent := '0';                  
                end loop;
            elsif nextAccepting = '1' and j + FETCH_WIDTH < LEN then
                for k in 0 to FETCH_WIDTH-1 loop
                    res(j + k) := content(j + k + FETCH_WIDTH);                   
                end loop;
            elsif nextAccepting = '1' and j + FETCH_WIDTH >= LEN then
                for k in 0 to FETCH_WIDTH-1 loop
                    res(j + k).full := '0';                   
                end loop;                  
            else
                for k in 0 to FETCH_WIDTH-1 loop
                    res(j + k) := content(j + k);                   
                end loop;                    
            end if;
            --end if;
            
        end loop;

        for i in 0 to LEN-1 loop
            if kill = '1' then
                res(i).full := '0';
            end if;
        end loop;

        
--            for i in 0 to LEN-1 loop      
--                if remainingMaskExt(i + 4) = '1' then  -- !! equivalent to remainingMask(i), where '1' for i < 0    
--                    if nextAccepting = '1' and i + 4 < LEN then
--                        res(i).ins := content(i+4).ins;
--                    else
--                        res(i).ins := content(i).ins;
--                    end if;
--                else
--                    res(i) := getNewElem(remainingMaskExt(i+1 to i+3), newContentAdj);
--                end if;
                
--                res(i).ins.controlInfo.newEvent := '0'; -- Separating front events from exec events
--                                             --  Meanwhile, branch taken/not taken state must be retained         
--                fillMask(i) := '0';
--                for k in 0 to 3 loop -- Further beyond end requires more ful inputs to be filled:
--                    --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
--                    fillMask(i) := fillMask(i) or (remainingMaskExt(i + 3-k) and inputMaskComp(k));
--                end loop;
                
--                res(i).full := (remainingMaskExt(i + 4) or (fillMask(i) and prevSending)) and not kill;
--            end loop;
        
        if CLEAR_DEBUG_INFO then    
            for i in 0 to IBUFFER_SIZE-1 loop
                res(i).ins := clearAbstractInfo(res(i).ins);
                res(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;
            end loop;
        end if;

        return res;
    end function;


    function adjustStage(content: InstructionSlotArray)
    return InstructionSlotArray is
        constant LEN: positive := content'length;
        variable res: InstructionSlotArray(0 to LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        variable contentExt: InstructionSlotArray(0 to 2*LEN-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        variable fullMask: std_logic_vector(0 to LEN-1) := (others => '0');
        variable nShift, j: integer := 0;
    begin
        contentExt(0 to LEN-1) := content;
        fullMask := extractFullMask(content);
        nShift := getFirstOnePosition(fullMask);
        if isNonzero(fullMask) = '0' then
            nShift := 0;
        end if; 
        
        for i in 0 to LEN-1 loop
            res(i) := contentExt(nShift + i);
        end loop;
        
        return res;    
    end function;
    
    subtype PipeStage is InstructionSlotArray(0 to PIPE_WIDTH-1);
    type PipeStageArray is array(natural range <>) of PipeStage;


begin
    fullMask <= extractFullMask(queueData);
    
    queueDataNext <= bufferDataNext_New(queueData, stageDataIn, nextAccepting, prevSending, execEventSignal);
    
    
    sending <= nextAccepting and queueData(0).full and not execEventSignal; -- Send if nonempty & not killed

    X_YES: if false generate
        acceptingOut <= not queueData(IBUFFER_SIZE-4).full;
        stageDataOut <= queueData(0 to 3);
        sendingOut <= sending;
    end generate;
    
	BUFF_CLOCKED: process(clk)
	begin					
		if rising_edge(clk) then
            queueData <= queueDataNext;
		end if;
	end process;
	
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
	   signal queueTxt: InsStringArray(0 to IBUFFER_SIZE-1);
	begin
	   queueTxt <= getInsStringArray(queueData);
	end generate;	

    NEW_IMPL: block
        signal mem: PipeStageArray(0 to 3) := (others => (others => DEFAULT_INS_SLOT));
            signal memImm: DwordArray(0 to 3) := (others => (others => '0'));
            signal immRead: Dword := (others => '0');
            
            signal memArgs0: DwordArray(0 to 3) := (others => (others => '0'));
            signal memArgs1: DwordArray(0 to 3) := (others => (others => '0'));
            signal argsRead, groupArgs: WordArray(0 to 3) := (others => (others => '0'));
            
            signal memInfo: WordArray(0 to 3) := (others => (others => '0'));
            
        signal full: std_logic_vector(0 to 3) := (others => '0');
        signal pStart, pEnd, nFullGroups: SmallNumber := (others => '0');
        signal dataOutFull, dataOutFilling, dataOutStalled, isSending, isReading, memWriting, memReading, memBypassing, memDraining, isAccepting: std_logic := '0';
        signal memEmpty: std_logic := '1';
        
        signal stageDataInFormatted, dataOut, dataMemRead: PipeStage := (others => DEFAULT_INS_SLOT);
        
            signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9, cha: std_logic := '0';
        
        function formatInput(insVec: PipeStage) return PipeStage is
            variable res: PipeStage := insVec;
        begin
            for i in res'range loop
                res(i).ins.controlInfo.newEvent := '0';
                
                if CLEAR_DEBUG_INFO then    
                    res(i).ins := clearAbstractInfo(res(i).ins);
                    res(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;
                end if;   
            end loop;
            
            res := adjustStage(res);
            
            return res;
        end function;
        
        function TEST_memRead(insVec: PipeStage; d: Dword) return PipeStage is
            variable res: PipeStage := insVec;
        begin
            for i in 0 to 3 loop
                res(i).ins.constantArgs.imm(15 downto 0) := d(15 + 16*(3-i) downto 16*(3-i));
            end loop;
            return res;
        end function;
        
        
--            controlInfo:
--                frontBranch
--                confirmedBranch
--                specialAction
            
--            classInfo:
--                mainCluster
--                secCluster
--                fpRename
--                branchIns
                
--            virtualArgSpec:
--                intDestSel, floatDestSel, dest
--                intArgSel, floatArgSel, args

--            constantArgs:
--                immSel
--                imm
                
--            specificOperation:
--                ...

        

        -- TODO: this can also hold 'full' flag of the slot!
        function packInfo(ins: InstructionState) return Byte is
            variable res: Byte := (others => '0');
            variable b: Byte := (others => '0');
        begin
            res(7 downto 0) := ins.controlInfo.frontBranch & ins.controlInfo.confirmedBranch & ins.controlInfo.specialAction & ins.constantArgs.immSel
                             & ins.classInfo.mainCluster  & ins.classInfo.secCluster  & ins.classInfo.fpRename  & ins.classInfo.branchIns;           
            return res;
        end function;
        
        function unpackInfo(w: Byte; ins: InstructionState) return InstructionState is
            variable res: InstructionState := ins;
            variable b: Byte := (others => '0');
        begin
            
            res.controlInfo.frontBranch := w(7);
            res.controlInfo.confirmedBranch := w(6);
            res.controlInfo.specialAction := w(5);
            
            res.constantArgs.immSel := w(4);
            
            res.classInfo.mainCluster := w(3);
            res.classInfo.secCluster := w(2);
            res.classInfo.fpRename := w(1);
            res.classInfo.branchIns := w(0);
                                
            return res;
        end function;

        
        -- CAREFUL: only virtual because 5 bits per reg!
        function packArgSpec(argSpec: InstructionArgSpec) return Word is
            variable res: Word := (others => '0');
            variable b: Byte := (others => '0');
        begin
            b := argSpec.intDestSel & argSpec.floatDestSel & '0' & argSpec.dest(4 downto 0);
            res(31 downto 24) := b;
            
            for i in 0 to 2 loop
                b := argSpec.intArgSel(i) & argSpec.floatArgSel(i) & '0' & argSpec.args(i)(4 downto 0);
                res(23 - i*8 downto 16 - i*8) := b;
            end loop;
            
            return res;
        end function;
        
        function unpackArgSpec(w: Word) return InstructionArgSpec is
            variable res: InstructionArgSpec := DEFAULT_ARG_SPEC;
            variable b: Byte := (others => '0');
        begin
            b := w(31 downto 24);
            res.intDestSel := b(7);
            res.floatDestSel := b(6);
            res.dest := "000" & b(4 downto 0);
            
            for i in 0 to 2 loop
                b := w(23 - i*8 downto 16 - i*8);
                res.intArgSel(i) := b(7);
                res.floatArgSel(i) := b(6);
                res.args(i) := "000" & b(4 downto 0);
            end loop;
            
            return res;
        end function;
        
        
        
        
        function packGroupInfo(insVec: PipeStage) return Word is
           variable res: Word := (others => '0');
        begin
            for i in 0 to 3 loop
                res(31 - 8*i downto 24 - 8*i) := packInfo(insVec(i).ins);
            end loop;
            
            return res;
        end function;        

        function unpackGroupInfo(insVec: PipeStage; wa: Word) return PipeStage is
            variable res: PipeStage := insVec;
        begin
            for i in 0 to 3 loop
                res(i).ins := unpackInfo(wa(31 - 8*i downto 24 - 8*i), insVec(i).ins);
            end loop;
                        
            return res;
        end function;  


        function packGroupArgs(insVec: PipeStage) return WordArray is
           variable res: WordArray(0 to 3) := (others => (others => '0'));
        begin
            for i in 0 to 3 loop
                res(i) := packArgSpec(insVec(i).ins.virtualArgSpec);
            end loop;
             
            return res;
        end function;        

        function unpackGroupArgs(insVec: PipeStage; wa: WordArray) return PipeStage is
            variable res: PipeStage := insVec;
        begin
            for i in 0 to 3 loop
                 --   assert res(i).ins.virtualArgSpec = unpackArgSpec(packArgSpec(res(i).ins.virtualArgSpec)) report "YYYYYY! " & integer'image(i);
                res(i).ins.virtualArgSpec := unpackArgSpec(wa(i));
            end loop;
                        
            return res;
        end function; 
    
        
        type WAA is array(0 to 3) of WordArray(0 to 3);
        
        signal argMem,infoMem: WAA := (others => (others => (others => '0')));
        --signal infoMem: WordArray(0 to 3) := (others => (others => '0')); 
            
        attribute ram_style: string;
        attribute ram_style of mem: signal is "distributed";    
        
        attribute ram_style of infoMem: signal is "distributed";    
        attribute ram_style of argMem: signal is "distributed";    
        attribute ram_style of memImm: signal is "distributed";    

        attribute ram_style of memArgs0: signal is "distributed";    
        attribute ram_style of memArgs1: signal is "distributed";    
        attribute ram_style of memInfo: signal is "distributed";    
                    
            signal chWord0, chWord1: Word := (others => '0');   
    begin
                ch0 <= bool2std(dataOut(0) = queueData(0)) or not queueData(0).full;
                ch1 <= bool2std(dataOut(1) = queueData(1)) or not queueData(1).full;
                ch2 <= bool2std(dataOut(2) = queueData(2)) or not queueData(2).full;
                ch3 <= bool2std(dataOut(3) = queueData(3)) or not queueData(3).full;
    
            dataOutStalled <= dataOutFull and not isSending;
            
            -- This means writing and keeping it there for later, not writing and using at the same time.
            -- Actual storage of value can happen also when bypassing - but is not used later.
            memWriting <= prevSending and (dataOutStalled or not memEmpty); -- Writing to emty mem: when dataOut stalled
                                                                            -- Writing to non empty mem: when prevSending and mem already full            
            memBypassing <= prevSending and not memWriting
                                                        and not execEventSignal;

            memReading <= (isSending or not dataOutFull) and not memEmpty
                                                                     and not execEventSignal;
            
            memDraining <= memReading and not memWriting and bool2std(addIntTrunc(pStart, 1, 2) = pEnd);
            
                isAccepting <= bool2std(pStart /= pEnd) or memEmpty;
            
        --isReading <= --(isSending and not memEmpty) or (not dataOutFull and prevSending);
        --              (isSending or not dataOutFull) and (not memEmpty or prevSending);
        isSending <= dataOutFull and nextAccepting and not execEventSignal;
        
            nFullGroups <=      i2slv(4, SMALL_NUMBER_SIZE) when memEmpty = '0' and pEnd = pStart
                        else subTruncZ(pEnd, pStart, 2); -- range 0:4
        
        
                dataMemRead <= --mem(slv2u(pStart));
                                 unpackGroupInfo(  
                                      unpackGroupArgs( 
                                           TEST_memRead(mem(slv2u(pStart)), immRead),
                                           argsRead), --argMem(slv2u(pStart))) ,
                                      memInfo(slv2u(pStart)));
                                --TEST_memRead(mem(slv2u(pStart)), immRead);
                immRead <= memImm(slv2u(pStart));
           
                        chWord0 <= packArgSpec(dataMemRead(0).ins.virtualArgSpec);
                        chWord1 <= argMem(slv2u(pStart))(0);
           
           
                                groupArgs <= packGroupArgs(stageDataInFormatted);
                                   argsRead(0) <= memArgs0(slv2u(pStart))(63 downto 32);
                                   argsRead(1) <= memArgs0(slv2u(pStart))(31 downto 0);
                                   argsRead(2) <= memArgs1(slv2u(pStart))(63 downto 32);
                                   argsRead(3) <= memArgs1(slv2u(pStart))(31 downto 0);
           
           stageDataInFormatted <= formatInput(stageDataIn);     
        CLOCKED: process (clk)
        begin
        
            if rising_edge(clk) then
                dataOutFull <= (memReading or memBypassing or dataOutStalled);
            
                if memWriting = '1' then
                    memEmpty <= '0';
                elsif memDraining = '1' then    
                    memEmpty <= '1';
                end if;
            
                if prevSending = '1' then
                    mem(slv2u(pEnd)) <= stageDataInFormatted;
                        memImm(slv2u(pEnd)) <= stageDataInFormatted(0).ins.constantArgs.imm(15 downto 0) & stageDataInFormatted(1).ins.constantArgs.imm(15 downto 0)
                                             & stageDataInFormatted(2).ins.constantArgs.imm(15 downto 0) & stageDataInFormatted(3).ins.constantArgs.imm(15 downto 0);
                        --argMem(slv2u(pEnd)) <= packGroupArgs(stageDataInFormatted);
                        memInfo(slv2u(pEnd)) <= packGroupInfo(stageDataInFormatted);
                        
                        memArgs0(slv2u(pEnd)) <= groupArgs(0) & groupArgs(1); 
                        memArgs1(slv2u(pEnd)) <= groupArgs(2) & groupArgs(3); 
                        
                        
                    pEnd <= addIntTrunc(pEnd, 1, 2); -- TMP: 3 bits                
                end if;
                
                --dataOutFull <= '0';
                if memReading = '1' or memBypassing = '1' then
                    if --pStart = pEnd then -- memEmpty, bypassing
                            memBypassing = '1' then  -- CAREFUL: this correct a serious error
                        dataOut <= stageDataInFormatted;
                    else
                        dataOut <= dataMemRead;                                     
                    end if;
                    pStart <= addIntTrunc(pStart, 1, 2); -- TMP: 2 bits                    
                end if;
                
                if execEventSignal = '1' then
                    dataOutFull <= '0';
                    memEmpty <= '1';
                    pStart <= pStart;
                    pEnd <= pStart;
                    
                    --nFullGroups <= (others => '0');                    
                end if;
                
                --pStart(SMALL_NUMBER_SIZE-1 downto 2) <= (others => '0');
                --pEnd(SMALL_NUMBER_SIZE-1 downto 2) <= (others => '0');
            end if;
        end process;

        Y_YES: if true generate
            acceptingOut <= isAccepting;
            stageDataOut <= dataOut;
            sendingOut <= isSending;
        end generate;
        
            ch4 <= bool2std(isAccepting = not queueData(IBUFFER_SIZE-4).full);
            ch5 <= bool2std(isSending = sending);
        
    end block;

end Implem;
