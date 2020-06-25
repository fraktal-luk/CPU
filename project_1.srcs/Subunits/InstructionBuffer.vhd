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
    
    acceptingOut <= not queueData(IBUFFER_SIZE-4).full;
    
    sending <= nextAccepting and queueData(0).full and not execEventSignal; -- Send if nonempty & not killed
    stageDataOut <= queueData(0 to 3);
    
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
        signal full: std_logic_vector(0 to 3) := (others => '0');
        signal pStart, pEnd, nFullGroups: SmallNumber := (others => '0');
        signal dataOutFull, dataOutFilling, dataOutStalled, isSending, isReading, memWriting, memReading, memBypassing, memDraining, isAccepting: std_logic := '0';
        signal memEmpty: std_logic := '1';
        
        signal dataOut: PipeStage := (others => DEFAULT_INS_SLOT);
        
            signal ch0, ch1: std_logic := '0';
        
        -- TODO! Left adjusting!
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
    begin
                ch0 <= bool2std(dataOut(0) = queueData(0));
                ch1 <= bool2std(dataOut(1) = queueData(1));
    
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
                    mem(slv2u(pEnd)) <= formatInput(stageDataIn);
                    pEnd <= addIntTrunc(pEnd, 1, 2); -- TMP: 3 bits                
                end if;
                
                --dataOutFull <= '0';
                if memReading = '1' or memBypassing = '1' then
                    if pStart = pEnd then -- memEmpty, bypassing
                        dataOut <= formatInput(stageDataIn);
                    else
                        dataOut <= mem(slv2u(pStart));                                       
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
            end if;
        end process;
        
    end block;

    sendingOut <= sending;

end Implem;
