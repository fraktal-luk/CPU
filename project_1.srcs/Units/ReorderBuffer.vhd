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
		acceptingOut: out std_logic;
		acceptingMore: out std_logic;
		
		nextAccepting: in std_logic;
		sendingOut: out std_logic; 
		
		--outputData: out InstructionSlotArray(0 to PIPE_WIDTH-1);
        robOut: out ControlPacketArray(0 to PIPE_WIDTH-1);

		outputArgInfoI: out RenameInfoArray(0 to PIPE_WIDTH-1);
		outputArgInfoF: out RenameInfoArray(0 to PIPE_WIDTH-1);
		outputSpecial: out SpecificOp
	);	
end ReorderBuffer;


architecture Behavioral of ReorderBuffer is
    signal outputDataSig, outputDataSig_Pre: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
    signal outputSpecialSig, inputSpecial: InstructionSlot := DEFAULT_INS_SLOT;
        
	signal isSending, isEmpty, outputCompleted, outputCompleted_Pre, outputEmpty, execEvent, isFull, isAlmostFull: std_logic := '0';	
	signal startPtr, startPtrNext, endPtr, endPtrNext, causingPtr: SmallNumber := (others => '0');	

    signal ch0, ch1, ch2, ch3: std_logic := '0';
    
    
    -- TMP: to remove
    function getRenameInfoSC(insVec: InstructionSlotArray; constant IS_FP: boolean := false)
    return RenameInfoArray is
        variable res: RenameInfoArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_RENAME_INFO);
        variable va, pa: InstructionArgSpec := DEFAULT_ARG_SPEC;
        variable ca: InstructionConstantArgs := DEFAULT_CONSTANT_ARGS;
        variable depVec: DependencyVec;
    begin

        for i in 0 to PIPE_WIDTH-1 loop
            va := insVec(i).ins.virtualArgSpec;
            pa := insVec(i).ins.physicalArgSpec;
            ca := insVec(i).ins.constantArgs;
        
            if IS_FP then        
                res(i).destSel := va.floatDestSel;
                res(i).destSelFP := va.floatDestSel;
                
                res(i).psel := pa.floatDestSel;
            else
                res(i).destSel := va.intDestSel;
                res(i).psel := pa.intDestSel;
            end if;
            
            
            res(i).virtualDest := va.dest(4 downto 0);
            res(i).physicalDest := pa.dest;

            if IS_FP then
                res(i).sourceSel := va.floatArgSel;
            else
                res(i).sourceSel := va.intArgSel;
            end if;

            for j in 0 to 2 loop
                res(i).sourceConst(j) :=   (va.intArgSel(j) and not isNonzero(va.args(j)(4 downto 0))) -- int r0
                                        or (not va.intArgSel(j) and not va.floatArgSel(j))             -- not used
                                        or (bool2std(j = 1) and ca.immSel);                            -- imm
            end loop;

            res(i).deps := depVec(i);
            res(i).physicalSourcesNew := res(i).physicalSources;
                                
            for j in 0 to 2 loop
                res(i).sourcesNew(j) := isNonzero(res(i).deps(j));
                for k in PIPE_WIDTH-1 downto 0 loop
                    if res(i).deps(j)(k) = '1' then
                        exit;
                    end if;
                end loop;
            end loop;
            
            res(i).sourcesReady := (others => '0');
        end loop;
        return res;
    end function;
begin
    inputSpecial.ins.specificOperation <= specialOp;

	execEvent <= branchControl.full and branchControl.newEvent;
	
	causingPtr <= getTagHighSN(execSigsMain(0).tag) and PTR_MASK_SN_LONG;
	
    NEW_DEV: block
        signal staticInput, staticOutput, staticOutput_Pre: StaticOpInfoArray;
        signal dynamicInput, dynamicOutput, dynamicOutput_Pre: DynamicOpInfoArray;

        signal staticGroupInput, staticGroupOutput, staticGroupOutput_Pre: StaticGroupInfo;
        signal dynamicGroupInput, dynamicGroupOutput, dynamicGroupOutput_Pre: DynamicGroupInfo;
        
        signal staticContent: StaticOpInfoArray2D := (others => (others => DEFAULT_STATIC_OP_INFO));
        signal dynamicContent: DynamicOpInfoArray2D := (others => (others => DEFAULT_DYNAMIC_OP_INFO));

        signal staticGroupContent: StaticGroupInfoArray := (others => DEFAULT_STATIC_GROUP_INFO);
        signal dynamicGroupContent: DynamicGroupInfoArray := (others => DEFAULT_DYNAMIC_GROUP_INFO);
        
        signal serialInput, serialOutput: std_logic_vector(TMP_SERIAL_MEM_WIDTH-1 downto 0) := (others=> '0');
        signal serialMemContent: SerialMem := (others => (others => '0'));                         
    begin
    
        -- Inputs
        serialInput <= serializeStatic(staticInput, staticGroupInput);
           
        staticInput <= getStaticOpInfoA(inputData);
        dynamicInput <= getDynamicOpInfoA(inputData);
        
        staticGroupInput <= getStaticGroupInfo(inputData, inputSpecial);
        dynamicGroupInput <= getDynamicGroupInfo(inputData, inputSpecial);

        -- Outputs
        outputDataSig_Pre <= getInstructionSlotArray(staticOutput_Pre, dynamicOutput_Pre, staticGroupOutput_Pre, dynamicGroupOutput_Pre);
        outputSpecialSig <= getSpecialSlot(staticGroupOutput, dynamicGroupOutput);

    	outputCompleted_Pre <= groupCompleted(outputDataSig_Pre, dynamicOutput_Pre);

        dynamicOutput_Pre <= readDynamicOutput(dynamicContent, startPtrNext);
        dynamicGroupOutput_Pre <= readDynamicGroupOutput(dynamicGroupContent, startPtrNext);
        
        staticOutput_Pre <= deserializeStaticInfoA(serialMemContent(p2i(startPtrNext, ROB_SIZE)));                    
        staticGroupOutput_Pre <= deserializeStaticGroupInfo(serialMemContent(p2i(startPtrNext, ROB_SIZE)));


        SYNCH: process (clk)
        begin
            if rising_edge(clk) then
                -- Update content
                updateDynamicContent(dynamicContent, execSigsMain, 0);
                updateDynamicContent(dynamicContent, execSigsSec, 1);

                updateDynamicContentBranch(dynamicContent, branchControl.full, branchControl, execSigsMain(0).tag);
                updateDynamicContentMemEvent(dynamicContent, execSigsMain(2).full, memoryControl, execSigsMain(2).tag);

                -- Write inputs
                if prevSending = '1' then                    
                    writeStaticInput(staticContent, staticInput, endPtr);
                    writeStaticGroupInput(staticGroupContent, staticGroupInput, endPtr);

                    writeDynamicInput(dynamicContent, dynamicInput, endPtr);
                    writeDynamicGroupInput(dynamicGroupContent, dynamicGroupInput, endPtr);
                    
                    serialMemContent(p2i(endPtr, ROB_SIZE)) <= serialInput;
                end if;
                   
                -- Read output
                staticOutput <= readStaticOutput(staticContent, startPtrNext);

                dynamicOutput <= dynamicOutput_Pre;
                dynamicGroupOutput <= dynamicGroupOutput_Pre;
                
                serialOutput <= serialMemContent(p2i(startPtrNext, ROB_SIZE));
                
                staticGroupOutput <= staticGroupOutput_Pre;
                  
                outputDataSig <= outputDataSig_Pre;
                outputCompleted <= outputCompleted_Pre;             
            end if;
        end process;
	end block;


    CTR_MANAGEMENT: block
        signal recoveryCounter, nFull, nFullNext, nIn, nOut: SmallNumber := (others => '0');
    begin    
        startPtrNext <= startPtr when isSending = '0' else addIntTrunc(startPtr, 1, ROB_PTR_SIZE+1);
        
        endPtrNext <= startPtrNext when lateEventSignal = '1'
                    else  addIntTrunc(causingPtr, 1, ROB_PTR_SIZE+1) when execEvent = '1'
                    else  addIntTrunc(endPtr, 1, ROB_PTR_SIZE+1) when prevSending = '1'
                    else  endPtr;
        
        isEmpty <= getQueueEmpty(startPtr, endPtr, ROB_PTR_SIZE+1);
        -- nFull logic
        nIn <= i2slv(1, SMALL_NUMBER_SIZE) when prevSending = '1' else (others => '0');
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');

        nFullNext <= getNumFull(startPtrNext, endPtrNext, ROB_PTR_SIZE);
        
        MANAGEMENT: process (clk)
        begin
            if rising_edge(clk) then
                startPtr <= startPtrNext;
                endPtr <= endPtrNext;

                nFull <= nFullNext;
    
                isFull <= cmpGtU(nFullNext, ROB_SIZE-1);
                isAlmostFull <= cmpGtU(nFullNext, ROB_SIZE-2);
            	
            	-- TODO: check
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

--    outputData <= setDestFlags(outputDataSig);
    
    robOut <= convertROBData(outputDataSig);
    
    outputArgInfoI <= getRenameInfoSC(setDestFlags(outputDataSig), false);
    outputArgInfoF <= getRenameInfoSC(setDestFlags(outputDataSig), true);                
    outputSpecial <= outputSpecialSig.ins.specificOperation;

	acceptingOut <= not isFull;
    acceptingMore <= not isAlmostFull;
	
    isSending <= outputCompleted and nextAccepting and not outputEmpty;  
	sendingOut <= isSending;

end Behavioral;
