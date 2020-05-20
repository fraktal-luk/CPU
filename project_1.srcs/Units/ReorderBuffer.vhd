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
    signal outputDataReg: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal outputSpecialReg: InstructionSlot := DEFAULT_INS_SLOT;
	signal fullMask, completedMask, completedMaskNext: std_logic_vector(0 to ROB_SIZE-1) := (others => '0');

    signal content, contentNext: ReorderBufferArray := DEFAULT_ROB_ARRAY;

	signal isSending, isEmpty: std_logic := '0';
	signal execEvent: std_logic := '0'; -- depends on input in slot referring to branch ops

    constant ROB_HAS_RESET: std_logic := '0';
    constant ROB_HAS_EN: std_logic := '0';

	constant PTR_MASK_TAG: InsTag := i2slv(ROB_SIZE-1, TAG_SIZE);
	constant PTR_MASK_SN: SmallNumber := i2slv(ROB_SIZE-1, SMALL_NUMBER_SIZE);
	constant ROB_PTR_SIZE: natural := countOnes(PTR_MASK_SN);	
	
	signal recoveryCounter: SmallNumber := (others => '0');
	signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0'); 
	
	signal isFull, isAlmostFull: std_logic := '0'; 	
	
	function getNextRobContent(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           newSpecialAction: InstructionSlot;
	                           execInfo1, execInfo2: InstructionSlotArray;
	                           sends, receiving, execEvent, lateEventSignal: std_logic;
	                           startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable killMask: std_logic_vector(0 to ROB_SIZE-1) := (others => '0');
	   variable ptr1, ptr2,  iv, tagHiPart, tagLoPart: SmallNumber := (others => '0');
	   variable newInsState: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
	   if sends = '1' then
	       res(slv2u(startPtr)).full := '0'; -- CAREFUL: don't get index out of bounds
	   end if;
	   
	   if receiving = '1' then
	       res(slv2u(endPtr)).full := '1'; -- CAREFUL: don't get index out of bounds
	       res(slv2u(endPtr)).ops := newGroup;
	       res(slv2u(endPtr)).special := newSpecialAction;  
	   end if;
	
	   killMask := getMaskBetween(ROB_SIZE, causingPtr, endPtr, '0'); -- This has '1' also at 'equal' position!
	   killMask(slv2u(causingPtr)) := '0'; -- CAREFUL: don't get index out of bounds
	   
	   for i in 0 to ROB_SIZE-1 loop
	       iv := i2slv(i, SMALL_NUMBER_SIZE);
	       
	       -- Update group!
	       -- NOTE: tag comparison for slot will be simplified because tag bits directly show ROB slot:
	       --          [ upper bits | ROB slot | index in group]
	       --            (getTagHighSN([tag]) and PTR_MASK) == iv ?
	       --             getTagLowSN == [group index] ? // this is within updateOpGroup  
	       for j in 0 to 3 loop
	           ptr1 := getTagHighSN(execInfo1(j).ins.tags.renameIndex) and PTR_MASK_SN;
               ptr2 := getTagHighSN(execInfo2(j).ins.tags.renameIndex) and PTR_MASK_SN;
	           if ptr1 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo1(j), 0);
	           end if;
	           if ptr2 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo2(j), 1);
	           end if;
	       end loop;
	       
	       if ((execEvent and killMask(i)) or lateEventSignal) = '1' then
	           res(i).full := '0';
	       end if;
	   end loop;

       -- Clear unused fields for better synthesis
       if CLEAR_DEBUG_INFO then
           for j in 0 to ROB_SIZE-1 loop
               for i in 0 to PIPE_WIDTH-1 loop
                   -- Only controlInfo survives here! Other useful data is stored in separate mem array because the rest is immutable
                   newInsState := res(j).ops(i).ins;
                   res(j).ops(i).ins := DEFAULT_INSTRUCTION_STATE;
                   res(j).ops(i).ins.controlInfo := newInsState.controlInfo;
               end loop;

                   newInsState := res(j).special.ins;
                   res(j).special.ins.controlInfo := newInsState.controlInfo;
                   res(j).special.ins.specificOperation := newInsState.specificOperation;                          
           end loop;
       end if;
	   
	   return res;
	end function;
	
	function replaceConstantInformation(insVec: InstructionSlotArray; constInfo, constInfo2, constInfo3: Word) return InstructionSlotArray is
	   variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
	begin
	   for i in 0 to PIPE_WIDTH-1 loop
	       res(i).ins.physicalArgSpec.dest := constInfo(8*i + 7 downto 8*i);
	       res(i).ins.virtualArgSpec.dest := "000" & constInfo2(5*i + 4 downto 5*i);
	       
	       res(i).ins.virtualArgSpec.intDestSel := constInfo3(0 + 2*i);
	       res(i).ins.virtualArgSpec.floatDestSel := constInfo3(8 + 2*i);
	       
	       res(i).ins.physicalArgSpec.intDestSel := constInfo3(1 + 2*i);
	       res(i).ins.physicalArgSpec.floatDestSel := constInfo3(9 + 2*i);
	   end loop;
	   
	   return res;
	end function;
	
	function replaceConstantInformationSpecial(special: InstructionSlot; constInfo: Word) return InstructionSlot is
	   variable res: InstructionSlot := special;
	   variable num: natural := 0;
	begin
	   num := slv2u(constInfo(SYS_OP_SIZE - 1 + 16 downto 16));	   
	   
	   res.ins.specificOperation.subpipe := None;
	   res.ins.specificOperation.system := SysOp'val(num);	   
	   
	   return res;
	end function;

	signal startPtr, startPtrNext, endPtr, acmPtr, causingPtr: SmallNumber := (others => '0');
	
	signal constantBuf, constantBuf2, constantBuf3: WordArray(0 to ROB_SIZE-1) := (others => (others => '0'));
	signal inputConstant, inputConstant2, inputConstant3, constantFromBuf, constantFromBuf2, constantFromBuf3: Word := (others => '0');
	
    attribute ram_style: string;
    --attribute ram_style of constantBuf, constantBuf2, constantBuf3: signal is "block";	
begin

	execEvent <= execEndSigs1(0).full and execEndSigs1(0).ins.controlInfo.newEvent;
	causingPtr <= getTagHighSN(execEndSigs1(0).ins.tags.renameIndex) and PTR_MASK_SN; -- TEMP!
	
	contentNext <= getNextRobContent(content, inputData,
	                                 inputSpecial,
	                                 execEndSigs1, execEndSigs2,
	                                 isSending, prevSending,
	                                 execEvent, lateEventSignal,
	                                 startPtr, endPtr, causingPtr);

    inputConstant <= inputData(3).ins.physicalArgSpec.dest & inputData(2).ins.physicalArgSpec.dest & inputData(1).ins.physicalArgSpec.dest & inputData(0).ins.physicalArgSpec.dest;
    inputConstant2(19 downto 0) <= inputData(3).ins.virtualArgSpec.dest(4 downto 0) & inputData(2).ins.virtualArgSpec.dest(4 downto 0)
                    & inputData(1).ins.virtualArgSpec.dest(4 downto 0) & inputData(0).ins.virtualArgSpec.dest(4 downto 0);
    
    inputConstant3(7 downto 0) <=  inputData(3).ins.physicalArgSpec.intDestSel & inputData(3).ins.virtualArgSpec.intDestSel
                                 & inputData(2).ins.physicalArgSpec.intDestSel & inputData(2).ins.virtualArgSpec.intDestSel
                                 & inputData(1).ins.physicalArgSpec.intDestSel & inputData(1).ins.virtualArgSpec.intDestSel
                                 & inputData(0).ins.physicalArgSpec.intDestSel & inputData(0).ins.virtualArgSpec.intDestSel;
    
    inputConstant3(15 downto 8) <=  inputData(3).ins.physicalArgSpec.floatDestSel & inputData(3).ins.virtualArgSpec.floatDestSel
                                  & inputData(2).ins.physicalArgSpec.floatDestSel & inputData(2).ins.virtualArgSpec.floatDestSel
                                  & inputData(1).ins.physicalArgSpec.floatDestSel & inputData(1).ins.virtualArgSpec.floatDestSel
                                  & inputData(0).ins.physicalArgSpec.floatDestSel & inputData(0).ins.virtualArgSpec.floatDestSel;

    inputConstant3(SYS_OP_SIZE - 1 + 16 downto 16) <= i2slv(SysOp'pos(inputSpecial.ins.specificOperation.system), SYS_OP_SIZE);

    CONSTANT_MEM: process (clk)
    begin
        if rising_edge(clk) then
            if prevSending = '1' then
                constantBuf(slv2u(endPtr)) <= inputConstant;
                constantBuf2(slv2u(endPtr)) <= inputConstant2;
                constantBuf3(slv2u(endPtr)) <= inputConstant3;
            end if;
            
            constantFromBuf <= constantBuf(slv2u(startPtrNext));
            constantFromBuf2 <= constantBuf2(slv2u(startPtrNext));            
            constantFromBuf3 <= constantBuf3(slv2u(startPtrNext));            
        end if;
    end process;


    startPtrNext <= startPtr when isSending = '0' else addIntTrunc(startPtr, 1, ROB_PTR_SIZE);
							
	isEmpty <= bool2std(startPtr = endPtr); -- CAREFUL: elsewhere it MUST be assured that ROB never gets full because this would become incorrect. 'isFull' must mean 1 free slot
							
	SYNCHRONOUS: process (clk)
	begin
		if rising_edge(clk) then		  
		    -- Regular content
            content <= contentNext;

            startPtr <= startPtrNext;
            
            if lateEventSignal = '1' then
                endPtr <= startPtrNext;
            elsif execEvent = '1' then
                endPtr <= addIntTrunc(causingPtr, 1, ROB_PTR_SIZE);                            
            elsif prevSending = '1' then
                endPtr <= addIntTrunc(endPtr, 1, ROB_PTR_SIZE);
            end if;
                   
            if lateEventSignal = '1' or execEvent = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
            nFull <= nFullNext;
            
            -- CAREFUL: here we assure that the buffer is never full (so isFull incidcates 1 free slot). So startPtr = endPtr always indicates emptiness
            if cmpGtU(nFullNext, ROB_SIZE-1-1) = '1' then
                isFull <= '1';
                isAlmostFull <= '1';
            elsif cmpGtU(nFullNext, ROB_SIZE-2-1) = '1' then
                isFull <= '0';
                isAlmostFull <= '1';
            else
                isFull <= '0';
                isAlmostFull <= '0';
            end if;
            
            completedMask <= completedMaskNext;
            outputDataReg <= content(slv2u(startPtrNext)).ops;
            outputSpecialReg <= content(slv2u(startPtrNext)).special;
		end if;		
	end process;
 
    nIn <= i2slv(1, SMALL_NUMBER_SIZE) when prevSending = '1' else (others => '0');
    nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');
       
    CTR_MANAGEMENT: block
        signal ptrDiff, flowDiff: SmallNumber := (others => '0');
    begin
        nFullRestored <= ptrDiff and PTR_MASK_SN;
        ptrDiff <= subSN(endPtr, startPtrNext);
          
        flowDiff <= subSN(addSN(nFull, nIn), nOut);
        nFullNext <=     nFullRestored when cmpEqU(recoveryCounter, 1) = '1'
                    else flowDiff and PTR_MASK_SN;
    end block;
	   
	FULL_MASK: for i in 0 to ROB_SIZE-1 generate
	   fullMask(i) <= content(i).full;
       completedMaskNext(i) <= groupCompleted(content(i).ops) and fullMask(i) and not isEmpty and not lateEventSignal;
	end generate;
	
    isSending <= completedMask(slv2u(startPtr)) and nextAccepting and not isEmpty;

	acceptingOut <= not isFull;
	
    acmPtr <= addIntTrunc(endPtr, 1, ROB_PTR_SIZE);
    acceptingMore <= not isAlmostFull;
	outputData <= replaceConstantInformation(outputDataReg, constantFromBuf, constantFromBuf2, constantFromBuf3);

	sendingOut <= isSending;
	outputSpecial <= replaceConstantInformationSpecial(outputSpecialReg, constantFromBuf3);
	
	VIEW: if VIEW_ON generate
	   use work.Viewing.all;
	
	   type StageTextArray is array (integer range <>) of InsStringArray(0 to PIPE_WIDTH-1);
	   
	   signal robView: StageTextArray(0 to ROB_SIZE-1);	   
	   subtype RobSlotText is string(1 to 80);
	   type RobSlotArray is array(integer range <>) of RobSlotText;
	   
	   function createRobView(content: ReorderBufferArray) return StageTextArray is
	       variable res: StageTextArray(0 to ROB_SIZE-1);
	   begin
	       for i in 0 to ROB_SIZE-1 loop
	           if content(i).full = '1' then
	               res(i) := getInsStringArray(content(i).ops);    
	           end if;
	       end loop;
	       
	       -- TODO: special actions!
	       
	       return res;
	   end function;
	   
	   signal robText: InsStringArray(0 to ROB_SIZE-1) := (others => (others => ' '));
	begin	   
	    robView <= createRobView(content);
        
        ROB_TEXT: for i in 0 to ROB_SIZE-1 generate
            robText(i) <= sprintRobRow(content(i).ops);
        end generate;
        
        process(clk)
        begin
            if rising_edge(clk) then 

            end if;
        end process;
        
    end generate;
end Behavioral;
