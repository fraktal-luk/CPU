----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;

use work.LogicROB.all;


entity ReorderBuffer is
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		lateEventSignal: in std_logic;

		execSigsMain: in ExecResultArray(0 to 3);
		execSigsSec: in ExecResultArray(0 to 3);
		
		branchControl: in InstructionControlInfo;
		memoryControl: in InstructionControlInfo;
		
		specialOp: in SpecificOp;
		
		inputData: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		prevSending: in std_logic;
		prevSendingRe: in std_logic;

		acceptingOut: out std_logic;
		acceptingMore: out std_logic;
        acceptAlloc: out std_logic;

		nextAccepting: in std_logic;
		sendingOut: out std_logic; 

        robOut: out ControlPacketArray(0 to PIPE_WIDTH-1);

		outputArgInfoI: out RenameInfoArray(0 to PIPE_WIDTH-1);
		outputArgInfoF: out RenameInfoArray(0 to PIPE_WIDTH-1);
		outputSpecial: out SpecificOp;
		
	    dbState: in DbCoreState
	);	
end ReorderBuffer;


architecture Behavioral of ReorderBuffer is
    signal inputSpecial: InstructionSlot := DEFAULT_INS_SLOT;
        
	signal isSending, outputCompleted, outputCompleted_Pre, outputEmpty, execEvent, allowAlloc: std_logic := '0';	
	signal startPtr, startPtrNext, endPtr, endPtrNext, renamedPtr, renamedPtrNext, causingPtr: SmallNumber := (others => '0');	

    -- Static content arrays - not changing throughout lifetime
    signal staticContent: StaticOpInfoArray2D := (others => (others => DEFAULT_STATIC_OP_INFO));
    signal staticGroupContent: StaticGroupInfoArray := (others => DEFAULT_STATIC_GROUP_INFO);
    signal serialMemContent: SerialMem := (others => (others => '0'));                         

    -- Dynamic content arrays - may change anytime
    signal dynamicContent: DynamicOpInfoArray2D := (others => (others => DEFAULT_DYNAMIC_OP_INFO));
    signal dynamicGroupContent: DynamicGroupInfoArray := (others => DEFAULT_DYNAMIC_GROUP_INFO);
    -- 
    signal robOut_N: ControlPacketArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_CONTROL_PACKET);


    signal ch0, ch1, ch2, ch3: std_logic := '0';


    -- TMP: to remove
    function getRenameInfoSC(sa: StaticOpInfoArray; da: DynamicOpInfoArray; constant IS_FP: boolean := false)
    return RenameInfoArray is
        variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
        variable cancelDest: std_logic := '0';
    begin

        for i in 0 to PIPE_WIDTH-1 loop
            cancelDest := not da(i).full or da(i).hasException or da(i).specialAction;

            res(i).dbInfo := da(i).dbInfo;

            if IS_FP then
                res(i).destSel := sa(i).virtualFloatDestSel;
                res(i).destSelFP := sa(i).virtualFloatDestSel;
                
                res(i).psel := sa(i).virtualFloatDestSel;
            else
                res(i).destSel := sa(i).virtualIntDestSel;
                res(i).psel := sa(i).virtualIntDestSel;
            end if;
            
            if da(i).full /= '1' then
                res(i).destSel := '0';
                res(i).destSelFP := '0';
            end if;
        
            if cancelDest = '1' then
                res(i).psel := '0';
            end if;
            
            res(i).virtualDest := sa(i).virtualDest;
            res(i).physicalDest := sa(i).physicalDest;
        end loop;
        return res;
    end function;  


    function getOutputControlArray(sa: StaticOpInfoArray; da: DynamicOpInfoArray; sgi: StaticGroupInfo; dgi: DynamicGroupInfo) return ControlPacketArray is
        variable res: ControlPacketArray(sa'range) := (others => DEFAULT_CONTROL_PACKET);
    begin
        for i in res'range loop
            res(i).dbInfo := da(i).dbInfo;
            
            res(i).controlInfo.full := da(i).full;
            res(i).controlInfo.killed := da(i).killed;
            res(i).controlInfo.causing := da(i).causing;

            res(i).controlInfo.newEvent := da(i).hasEvent;
            res(i).controlInfo.hasException := da(i).hasException;
            res(i).controlInfo.confirmedBranch := da(i).confirmedBranch;
            res(i).controlInfo.specialAction := da(i).specialAction;
            res(i).controlInfo.refetch := da(i).refetch;

            res(i).classInfo.branchIns := sa(i).useBQ;      
            res(i).classInfo.secCluster := sa(i).useSQ;      
            res(i).classInfo.useLQ := sa(i).useLQ;  
        end loop;

        res(0).controlInfo.firstBr := sgi.useBQ;

        return res;
    end function;


begin
    inputSpecial.ins.specificOperation <= specialOp;

	execEvent <= branchControl.full and branchControl.newEvent;
	
	causingPtr <= getTagHighSN(execSigsMain(0).tag) and PTR_MASK_SN_LONG;
	
    NEW_DEV: block
        signal staticInput, staticOutput, staticOutputDB, staticOutput_Pre, staticOutput_PreDB: StaticOpInfoArray;
        signal dynamicInput, dynamicOutput, dynamicOutput_Pre: DynamicOpInfoArray;

        signal staticGroupInput, staticGroupOutput, staticGroupOutput_Pre: StaticGroupInfo;
        signal dynamicGroupInput, dynamicGroupOutput, dynamicGroupOutput_Pre: DynamicGroupInfo;
        
        signal serialInput, serialOutput: std_logic_vector(TMP_SERIAL_MEM_WIDTH-1 downto 0) := (others=> '0');
    begin

        -- Inputs
        serialInput <= serializeStatic(staticInput, staticGroupInput);
           
        staticInput <= getStaticOpInfoA(inputData);
        dynamicInput <= getDynamicOpInfoA(inputData);
        
        staticGroupInput <= getStaticGroupInfo(inputData, inputSpecial);
        dynamicGroupInput <= getDynamicGroupInfo(inputData, inputSpecial);

        -- Outputs
        outputSpecial <= getSpecialOperation(staticGroupOutput, dynamicGroupOutput);

    	outputCompleted_Pre <= groupCompleted(dynamicOutput_Pre);

        dynamicOutput_Pre <= readDynamicOutput(dynamicContent, startPtrNext);
        dynamicGroupOutput_Pre <= readDynamicGroupOutput(dynamicGroupContent, startPtrNext);
        
        staticOutput_Pre <= deserializeStaticInfoA(serialMemContent(p2i(startPtrNext, ROB_SIZE)));                    
        staticGroupOutput_Pre <= deserializeStaticGroupInfo(serialMemContent(p2i(startPtrNext, ROB_SIZE)));

        staticOutput_PreDB <= readStaticOutput(staticContent, startPtrNext);


        SYNCH: process (clk)
        begin
            if rising_edge(clk) then
                -- Update content
                updateDynamicContent(dynamicContent, execSigsMain, 0);
                updateDynamicContent(dynamicContent, execSigsSec, 1);

                updateDynamicContentBranch(dynamicContent, branchControl.full, branchControl, execSigsMain(0).tag);
                updateDynamicContentMemEvent(dynamicContent, execSigsMain(2).full, memoryControl, execSigsMain(2).tag);

                --DB_trackUpdateSeqNum(execSigsMain);
                --DB_trackUpdateSeqNum(execSigsSec);

                if lateEventSignal = '1' then
                    flushDynamicContent(dynamicContent);
                    --DB_trackCompleteFlush(dynamicContent);
                end if;

                -- Write inputs
                if prevSending = '1' then                    
                    writeStaticInput(staticContent, staticInput, endPtr);
                    writeStaticGroupInput(staticGroupContent, staticGroupInput, endPtr);

                    writeDynamicInput(dynamicContent, dynamicInput, endPtr);
                    writeDynamicGroupInput(dynamicGroupContent, dynamicGroupInput, endPtr);
                    
                    --DB_trackInputSeqNum(dynamicInput);
                    
                    serialMemContent(p2i(endPtr, ROB_SIZE)) <= serialInput;
                end if;

                -- Read output
                serialOutput <= serialMemContent(p2i(startPtrNext, ROB_SIZE)); -- DB

                staticOutput <= staticOutput_Pre; -- DB
                staticGroupOutput <= staticGroupOutput_Pre;

                dynamicOutput <= dynamicOutput_Pre;
                dynamicGroupOutput <= dynamicGroupOutput_Pre;
                
                staticOutputDB <= staticOutput_PreDB;
 
                outputCompleted <= outputCompleted_Pre;
                
                if isSending = '1' then
                    removeGroup(dynamicContent, startPtr);
                    --DB_trackInputSeqNum(dynamicOutput);
                end if;             
            end if;
        end process;

        robOut_N <= getOutputControlArray(staticOutput, dynamicOutput, staticGroupOutput, dynamicGroupOutput);

        outputArgInfoI <= getRenameInfoSC(staticOutput, dynamicOutput, false);
        outputArgInfoF <= getRenameInfoSC(staticOutput, dynamicOutput, true);
	end block;

    CTR_MANAGEMENT: block
        signal recoveryCounter, nAlloc, nAllocNext: SmallNumber := (others => '0');
    begin    
        startPtrNext <= addIntTrunc(startPtr, 1, ROB_PTR_SIZE+1) when isSending = '1'
                   else startPtr;

        endPtrNext <= startPtrNext when lateEventSignal = '1'
                    else  addIntTrunc(causingPtr, 1, ROB_PTR_SIZE+1) when execEvent = '1'
                    else  addIntTrunc(endPtr, 1, ROB_PTR_SIZE+1) when prevSending = '1'
                    else  endPtr;

        renamedPtrNext <= startPtrNext when lateEventSignal = '1'
                    else  addIntTrunc(causingPtr, 1, ROB_PTR_SIZE+1) when execEvent = '1'
                    else  addIntTrunc(renamedPtr, 1, ROB_PTR_SIZE+1) when prevSendingRe = '1'
                    else  renamedPtr;

        nAllocNext <= getNumFull(startPtrNext, renamedPtrNext, ROB_PTR_SIZE);

        MANAGEMENT: process (clk)
        begin
            if rising_edge(clk) then
                startPtr <= startPtrNext;
                endPtr <= endPtrNext;
                renamedPtr <= renamedPtrNext;

                nAlloc <= nAllocNext;

            	allowAlloc <= not cmpGtU(nAllocNext, ROB_SIZE-1);

                outputEmpty <= bool2std(startPtrNext = endPtr) or lateEventSignal;

                if lateEventSignal = '1' or execEvent = '1' then
                    recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addInt(recoveryCounter, -1);
                end if;

                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here    
            end if;		
        end process;

    end block;

    robOut <= robOut_N;

	acceptAlloc <= allowAlloc;

    isSending <= outputCompleted and nextAccepting and not outputEmpty;  
	sendingOut <= isSending;

    -- pragma synthesis off
    DEBUG_HANDLING: if DB_ENABLE generate
        
        process (clk)
            use std.textio.all;
            use work.Assembler.all;
    
            function getDynamicContentString(dyn: DynamicOpInfo) return string is
                variable res: line;
            begin
                if dyn.full = '1' then
                    write(res, natural'image(slv2u(dyn.dbInfo.tag)));
                    write(res, string'(": "));
                    write(res, std_logic'image(dyn.completed0));
                    if dyn.secCluster = '1' then
                        write(res, std_logic'image(dyn.completed1));
                    else
                        write(res, string'("'-'"));
                    end if;
                    write(res, string'(" "));
                    if dyn.hasEvent = '1' then
                        write(res, string'("E : "));
                    else
                        write(res, string'("  : "));
                    end if;
                    
                    write(res, disasmWord(dyn.dbInfo.bits));
                    
                    return res.all;
                else
                    return "-------------------------------------";
                end if;
            end function;
        
            procedure printContent is
               file outFile: text open write_mode is "rob_content.txt";
               variable preRow, currentLine: line := null;
            begin
                for i in 0 to ROB_SIZE-1 loop
                    if p2i(startPtr, ROB_SIZE) = i then
                        preRow := "start ";
                    elsif p2i(endPtr, ROB_SIZE) = i then
                        preRow := "end   ";
                    else
                        preRow := "      ";
                    end if;
                    
                    currentLine := null;
                    write(currentLine, preRow.all & natural'image(i) & "  ");
                    for j in 0 to PIPE_WIDTH-1 loop
                        write(currentLine, getDynamicContentString(dynamicContent(i, j)) & ",   ");
                    end loop;
    
                    writeline(outFile, currentLine);
                end loop;
            end procedure;
            
        begin
            
            if rising_edge(clk) then
                if DB_LOG_EVENTS then
                    if dbState.dbSignal = '1' then
                        report "ROB reporting ";
                        printContent;
                    end if;
                end if;
            end if;
        end process;
        
    end generate;
    -- pragma synthesis on

end Behavioral;
