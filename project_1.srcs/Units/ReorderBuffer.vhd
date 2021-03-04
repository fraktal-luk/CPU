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
		commitGroupCtr: in InsTag;

		execEndSigs1: in InstructionSlotArray(0 to 3);
		execEndSigs2: in InstructionSlotArray(0 to 3);
		
		inputSpecial: in InstructionSlot;
		inputData: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		prevSending: in std_logic;
		acceptingOut: out std_logic;
		acceptingMore: out std_logic;
		
		nextAccepting: in std_logic;
		sendingOut: out std_logic; 
		
		outputData: out InstructionSlotArray(0 to PIPE_WIDTH-1);
		outputSpecial: out InstructionSlot
	);	
end ReorderBuffer;


architecture Behavioral of ReorderBuffer is

    signal outputDataSig: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INS_SLOT);
    signal outputSpecialSig: InstructionSlot := DEFAULT_INS_SLOT;
        
	signal isSending, isEmpty, outputCompleted, outputEmpty, execEvent, isFull, isAlmostFull: std_logic := '0';
	signal startPtr, startPtrNext, endPtr, endPtrNext, causingPtr: SmallNumber := (others => '0');	
	signal startPtrLong, startPtrLongNext, endPtrLong, endPtrLongNext, causingPtrLong: SmallNumber := (others => '0');	

        signal ch0, ch1, ch2, ch3: std_logic := '0';
        
    -- TODO: duplicated from SQ, unduplicate  
    function getQueueEmpty(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return std_logic is
            constant xored: SmallNumber := pStart xor pEnd;
            constant template: SmallNumber := (others => '0');
        begin
            return bool2std(xored(QUEUE_PTR_SIZE downto 0) = template(QUEUE_PTR_SIZE downto 0));
        end function;
    
    
        function getNumFull(pStart, pEnd: SmallNumber; constant QUEUE_PTR_SIZE: natural) return SmallNumber is
            constant diff: SmallNumber := subTruncZ(pEnd, pStart, QUEUE_PTR_SIZE);
            constant xored: SmallNumber := pStart xor pEnd;        
            variable result: SmallNumber := diff;
        begin
            result(QUEUE_PTR_SIZE) := xored(QUEUE_PTR_SIZE) and not isNonzero(xored(QUEUE_PTR_SIZE-1 downto 0));
            return result;      
        end function;        
begin
	execEvent <= execEndSigs1(0).full and execEndSigs1(0).ins.controlInfo.newEvent;
	causingPtr <= getTagHighSN(execEndSigs1(0).ins.tags.renameIndex) and PTR_MASK_SN; -- TEMP!
	causingPtrLong <= getTagHighSN(execEndSigs1(0).ins.tags.renameIndex) and PTR_MASK_SN_LONG; -- TEMP!
	
    NEW_DEV: block
        signal staticInput, staticOutput, staticOutput_D: StaticOpInfoArray;
        signal dynamicInput, dynamicOutput: DynamicOpInfoArray;

        signal staticGroupInput, staticGroupOutput, staticGroupOutput_D: StaticGroupInfo;
        signal dynamicGroupInput, dynamicGroupOutput: DynamicGroupInfo;
        
        signal staticContent: StaticOpInfoArray2D := (others => (others => DEFAULT_STATIC_OP_INFO));
        signal dynamicContent: DynamicOpInfoArray2D := (others => (others => DEFAULT_DYNAMIC_OP_INFO));

        signal staticGroupContent: StaticGroupInfoArray := (others => DEFAULT_STATIC_GROUP_INFO);
        signal dynamicGroupContent: DynamicGroupInfoArray := (others => DEFAULT_DYNAMIC_GROUP_INFO);
        
            signal serialInput, serialOutput: std_logic_vector(TMP_SERIAL_MEM_WIDTH-1 downto 0) := (others=> '0');
            signal serialMemContent: SerialMem := (others => (others => '0'));                         
    begin
                serialInput <= serializeStatic(staticInput, staticGroupInput);
                
                --staticOutput_D <= deserializeStaticInfoA(serialOutput);
                --staticGroupOutput_D <= deserializeStaticGroupInfo(serialOutput);
                
        staticInput <= getStaticOpInfoA(inputData);
        dynamicInput <= getDynamicOpInfoA(inputData);
        
        staticGroupInput <= getStaticGroupInfo(inputData, inputSpecial);
        dynamicGroupInput <= getDynamicGroupInfo(inputData, inputSpecial);

        outputDataSig <= getInstructionSlotArray_T(staticOutput_D, dynamicOutput, staticGroupOutput_D, dynamicGroupOutput);
        outputSpecialSig <= getSpecialSlot_T(staticGroupOutput_D, dynamicGroupOutput);

    	outputCompleted <= groupCompleted(outputDataSig);
    
        SYNCH: process (clk)
        begin
            if rising_edge(clk) then
                -- Update content
                updateDynamicContent(dynamicContent, execEndSigs1, 0);
                updateDynamicContent(dynamicContent, execEndSigs2, 1);

                updateDynamicContentBranch(dynamicContent, execEndSigs1(0));
                updateDynamicContentMemEvent(dynamicContent, execEndSigs1(2));

                -- Write inputs
                if prevSending = '1' then                    
                    writeStaticInput(staticContent, staticInput, endPtr);
                    writeStaticGroupInput(staticGroupContent, staticGroupInput, endPtr);

                    writeDynamicInput(dynamicContent, dynamicInput, endPtr);
                    writeDynamicGroupInput(dynamicGroupContent, dynamicGroupInput, endPtr);
                    
                        serialMemContent(slv2u(endPtr)) <= serialInput;
                 end if;
                   
                -- Read output                    
                staticOutput <= readStaticOutput(staticContent, startPtrNext);
                staticGroupOutput <= readStaticGroupOutput(staticGroupContent, startPtrNext);

                dynamicOutput <= readDynamicOutput(dynamicContent, startPtrNext);
                dynamicGroupOutput <= readDynamicGroupOutput(dynamicGroupContent, startPtrNext);
                
                    serialOutput <= serialMemContent(slv2u(startPtrNext));
                    staticOutput_D <= deserializeStaticInfoA(serialMemContent(slv2u(startPtrNext)));                    
                    staticGroupOutput_D <= deserializeStaticGroupInfo(serialMemContent(slv2u(startPtrNext)));
                                    
            end if;
        end process;
        
            
            ch0 <= --bool2std(staticOutput_D = staticOutput);
                    getQueueEmpty(startPtrLong, endPtrLong, ROB_PTR_SIZE+1);
            ch1 <= not isEmpty xor ch0;

            ch2 <= bool2std(staticOutput_D(2) = staticOutput(2));
            ch3 <= bool2std(staticOutput_D(3) = staticOutput(3));
	end block;


    CTR_MANAGEMENT: block
        signal recoveryCounter,
               nFull, nFullNext, nFullRestored, nIn, nOut,
               ptrDiff, flowDiff: SmallNumber := (others => '0');
    begin
--        startPtrNext <= startPtr when isSending = '0' else addIntTrunc(startPtr, 1, ROB_PTR_SIZE);
        
--        endPtrNext <= startPtrNext when lateEventSignal = '1'
--                else  addIntTrunc(causingPtr, 1, ROB_PTR_SIZE) when execEvent = '1'
--                else  addIntTrunc(endPtr, 1, ROB_PTR_SIZE) when prevSending = '1'
--                else  endPtr;

            startPtrLongNext <= startPtrLong when isSending = '0' else addIntTrunc(startPtrLong, 1, ROB_PTR_SIZE+1);
            
            endPtrLongNext <= startPtrLongNext when lateEventSignal = '1'
                    else  addIntTrunc(causingPtrLong, 1, ROB_PTR_SIZE+1) when execEvent = '1'
                    else  addIntTrunc(endPtrLong, 1, ROB_PTR_SIZE+1) when prevSending = '1'
                    else  endPtrLong;
        

        isEmpty <= getQueueEmpty(startPtrLong, endPtrLong, ROB_PTR_SIZE+1);
        -- nFull logic
        nIn <= i2slv(1, SMALL_NUMBER_SIZE) when prevSending = '1' else (others => '0');
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');
   
        ptrDiff <= subTruncZ(endPtr, startPtrNext, ROB_PTR_SIZE);
        nFullRestored <= i2slv(ROB_SIZE, SMALL_NUMBER_SIZE) when startPtr = endPtr and isEmpty = '0'   
                        else subTruncZ(endPtr, startPtrNext, ROB_PTR_SIZE);

--            nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pTagged and memEmpty = '0'
--                               else subTruncZ(pTagged, pStartNext, QUEUE_PTR_SIZE);
          
        flowDiff <= subSN(addSN(nFull, nIn), nOut);
        nFullNext <=     nFullRestored when cmpEqU(recoveryCounter, 1) = '1'
                    else flowDiff and PTR_MASK_SN;
 
            
            startPtr <= startPtrLong and PTR_MASK_SN;
            endPtr <= endPtrLong and PTR_MASK_SN;
            startPtrNext <= startPtrLongNext and PTR_MASK_SN;
            endPtrNext <= endPtrLongNext and PTR_MASK_SN;
            
 
        MANAAGEMENT: process (clk)
        begin
            if rising_edge(clk) then
                --startPtr <= startPtrNext;
                --endPtr <= endPtrNext;        

                        startPtrLong <= startPtrLongNext;
                        endPtrLong <= endPtrLongNext;
            	            
                outputEmpty <= bool2std(startPtrNext = endPtr) or lateEventSignal;                
                
                if lateEventSignal = '1' or execEvent = '1' then
                    recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addInt(recoveryCounter, -1);
                end if;
                
                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
                
                nFull <= nFullNext;
    
                isFull <= cmpGtU(nFullNext, ROB_SIZE-1-1 + 1);
                isAlmostFull <= cmpGtU(nFullNext, ROB_SIZE-1-2 + 1);
    
            end if;		
        end process;
                            
    end block;

    outputData <= outputDataSig;
    outputSpecial <= outputSpecialSig;

	acceptingOut <= not isFull;
    acceptingMore <= not isAlmostFull;
	
    isSending <= outputCompleted and nextAccepting and not outputEmpty;  
	sendingOut <= isSending;

	
	VIEW: if VIEW_ON generate
	   use work.Viewing.all;
	
	   type StageTextArray is array (integer range <>) of InsStringArray(0 to PIPE_WIDTH-1);
	   
	   signal robView: StageTextArray(0 to ROB_SIZE-1);	   
	   subtype RobSlotText is string(1 to 80);
	   type RobSlotArray is array(integer range <>) of RobSlotText;
	   
--	   function createRobView(content: ReorderBufferArray) return StageTextArray is
--	       variable res: StageTextArray(0 to ROB_SIZE-1);
--	   begin
--	       for i in 0 to ROB_SIZE-1 loop
--	           if content(i).full = '1' then
--	               res(i) := getInsStringArray(content(i).ops);    
--	           end if;
--	       end loop;
	       
--	       -- TODO: special actions!
	       
--	       return res;
--	   end function;
	   
	   signal robText: InsStringArray(0 to ROB_SIZE-1) := (others => (others => ' '));
	begin	   
	    --robView <= createRobView(content);
        
        ROB_TEXT: for i in 0 to ROB_SIZE-1 generate
        --    robText(i) <= sprintRobRow(content(i).ops);
        end generate;
        
        process(clk)
        begin
            if rising_edge(clk) then 

            end if;
        end process;
        
    end generate;
end Behavioral;
