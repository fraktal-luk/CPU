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
		
		outputData: out InstructionSlotArray(0 to PIPE_WIDTH-1);
		outputSpecial_N: out SpecificOp
	);	
end ReorderBuffer;


architecture Behavioral of ReorderBuffer is

    signal outputDataSig, outputDataSig_Pre: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
    signal outputSpecialSig, inputSpecial_N: InstructionSlot := DEFAULT_INS_SLOT;
        
	signal isSending, isEmpty, outputCompleted, outputCompleted_Pre, outputEmpty, execEvent, isFull, isAlmostFull: std_logic := '0';
	--signal --startPtr,
	  --      startPtrNext--, 
	        --endPtr, endPtrNext--, causingPtr
	    --       : SmallNumber := (others => '0');	
	signal startPtrLong, startPtrLongNext, endPtrLong, endPtrLongNext, causingPtrLong: SmallNumber := (others => '0');	

    signal ch0, ch1, ch2, ch3: std_logic := '0';
begin
    inputSpecial_N.ins.specificOperation <= specialOp;

	execEvent <= branchControl.full and branchControl.newEvent;
	
	--causingPtr <= getTagHighSN(execSigsMain(0).tag) and PTR_MASK_SN;
	causingPtrLong <= getTagHighSN(execSigsMain(0).tag) and PTR_MASK_SN_LONG;
	
    NEW_DEV: block
        signal staticInput, staticOutput, staticOutput_D_Pre: StaticOpInfoArray;
        signal dynamicInput, dynamicOutput, dynamicOutput_Pre: DynamicOpInfoArray;

        signal staticGroupInput, staticGroupOutput_D, staticGroupOutput_D_Pre: StaticGroupInfo;
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
        
        staticGroupInput <= getStaticGroupInfo(inputData, inputSpecial_N);
        dynamicGroupInput <= getDynamicGroupInfo(inputData, inputSpecial_N);

        -- Outputs
        outputDataSig_Pre <= getInstructionSlotArray_T(staticOutput_D_Pre, dynamicOutput_Pre, staticGroupOutput_D_Pre, dynamicGroupOutput_Pre);
        outputSpecialSig <= getSpecialSlot_T(staticGroupOutput_D, dynamicGroupOutput);

    	outputCompleted_Pre <= groupCompleted(outputDataSig_Pre, dynamicOutput_Pre);

        dynamicOutput_Pre <= readDynamicOutput(dynamicContent, startPtrLongNext);
        dynamicGroupOutput_Pre <= readDynamicGroupOutput(dynamicGroupContent, startPtrLongNext);
        
        staticOutput_D_Pre <= deserializeStaticInfoA(serialMemContent(p2i(startPtrLongNext, ROB_SIZE)));                    
        staticGroupOutput_D_Pre <= deserializeStaticGroupInfo(serialMemContent(p2i(startPtrLongNext, ROB_SIZE)));


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
                    writeStaticInput(staticContent, staticInput, endPtrLong);
                    writeStaticGroupInput(staticGroupContent, staticGroupInput, endPtrLong);

                    writeDynamicInput(dynamicContent, dynamicInput, endPtrLong);
                    writeDynamicGroupInput(dynamicGroupContent, dynamicGroupInput, endPtrLong);
                    
                    serialMemContent(p2i(endPtrLong, ROB_SIZE)) <= serialInput;
                end if;
                   
                -- Read output
                staticOutput <= readStaticOutput(staticContent, startPtrLongNext);

                dynamicOutput <= dynamicOutput_Pre;
                dynamicGroupOutput <= dynamicGroupOutput_Pre;
                
                serialOutput <= serialMemContent(p2i(startPtrLongNext, ROB_SIZE));
                
                staticGroupOutput_D <= staticGroupOutput_D_Pre;
                  
                outputDataSig <= outputDataSig_Pre;
                outputCompleted <= outputCompleted_Pre;             
            end if;
        end process;
	end block;


    CTR_MANAGEMENT: block
        signal recoveryCounter, nFull, nFullNext, nIn, nOut: SmallNumber := (others => '0');
    begin    
        startPtrLongNext <= startPtrLong when isSending = '0' else addIntTrunc(startPtrLong, 1, ROB_PTR_SIZE+1);
        
        endPtrLongNext <= startPtrLongNext when lateEventSignal = '1'
                    else  addIntTrunc(causingPtrLong, 1, ROB_PTR_SIZE+1) when execEvent = '1'
                    else  addIntTrunc(endPtrLong, 1, ROB_PTR_SIZE+1) when prevSending = '1'
                    else  endPtrLong;
        
        isEmpty <= getQueueEmpty(startPtrLong, endPtrLong, ROB_PTR_SIZE+1);
        -- nFull logic
        nIn <= i2slv(1, SMALL_NUMBER_SIZE) when prevSending = '1' else (others => '0');
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');

        nFullNext <= getNumFull(startPtrLongNext, endPtrLongNext, ROB_PTR_SIZE);     
       
--        startPtr <= startPtrLong and PTR_MASK_SN;
--        endPtr <= endPtrLong and PTR_MASK_SN;
--        startPtrNext <= startPtrLongNext and PTR_MASK_SN;
--        endPtrNext <= endPtrLongNext and PTR_MASK_SN;
        
        MANAGEMENT: process (clk)
        begin
            if rising_edge(clk) then
                startPtrLong <= startPtrLongNext;
                endPtrLong <= endPtrLongNext;

                nFull <= nFullNext;
    
                isFull <= cmpGtU(nFullNext, ROB_SIZE-1);
                isAlmostFull <= cmpGtU(nFullNext, ROB_SIZE-2);
            	
            	-- TODO: check
                outputEmpty <= bool2std(startPtrLongNext = endPtrLong) or lateEventSignal;
                
                if lateEventSignal = '1' or execEvent = '1' then
                    recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addInt(recoveryCounter, -1);
                end if;
                
                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here    
            end if;		
        end process;

    end block;

    outputData <= outputDataSig;
        outputSpecial_N <= outputSpecialSig.ins.specificOperation;

	acceptingOut <= not isFull;
    acceptingMore <= not isAlmostFull;
	
    isSending <= outputCompleted and nextAccepting and not outputEmpty;  
	sendingOut <= isSending;

end Behavioral;
