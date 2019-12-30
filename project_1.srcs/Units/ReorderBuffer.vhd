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

	signal fullMask, completedMask, completedMaskNext, completedMask_T,  completedMaskNext_T: std_logic_vector(0 to ROB_SIZE-1) := (others => '0');

    signal content, contentNext: ReorderBufferArray := DEFAULT_ROB_ARRAY;

	signal isSending, isSending_T, isEmpty, ch0, ch1: std_logic := '0';
	signal execEvent: std_logic := '0'; -- depends on input in slot referring to branch ops

    constant ROB_HAS_RESET: std_logic := '0';
    constant ROB_HAS_EN: std_logic := '0';

	constant PTR_MASK_TAG: InsTag := i2slv(ROB_SIZE-1, TAG_SIZE);
	constant PTR_MASK_SN: SmallNumber := i2slv(ROB_SIZE-1, SMALL_NUMBER_SIZE);
	constant ROB_PTR_SIZE: natural := countOnes(PTR_MASK_SN);	
	
	signal recoveryCounter: SmallNumber := (others => '0');
	signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0'); 
	
	signal isFull, isAlmostFull: std_logic := '0'; 	

    --signal specialActionSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT; -- There needs to be only 1 such slot because everything younger is to be killed on commit
	
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
	begin
	   if sends = '1' then
	       res(slv2u(startPtr)).full := '0'; -- CAREFUL: don't get index out of bounds
	   end if;
	   
	   if receiving = '1' then
	       res(slv2u(endPtr)).full := '1'; -- CAREFUL: don't get index out of bounds
	       res(slv2u(endPtr)).ops := newGroup;
	       
	           res(slv2u(endPtr)).special := --getSpecialActionSlot(newGroup);
	                                         newSpecialAction;  
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
                   res(j).ops(i).ins.ip := (others => '0');
                   res(j).ops(i).ins.bits := (others => '0');              
                   res(j).ops(i).ins.result := (others => '0');
                   res(j).ops(i).ins.target := (others => '0');
                   
                   res(j).ops(i).ins.constantArgs := DEFAULT_CONSTANT_ARGS;
                
                   res(j).ops(i).ins.operation := (System, sysUndef);
                   
                        res(j).ops(i).ins.virtualArgSpec.intArgSel := (others => '0');
                        res(j).ops(i).ins.virtualArgSpec.floatArgSel := (others => '0');
                        res(j).ops(i).ins.virtualArgSpec.args := (others => (others => '0'));

                        res(j).ops(i).ins.physicalArgSpec.intArgSel := (others => '0');
                        res(j).ops(i).ins.physicalArgSpec.floatArgSel := (others => '0');
                        res(j).ops(i).ins.physicalArgSpec.args := (others => (others => '0'));
                   
                        res(j).ops(i).ins.virtualArgSpec.dest := (others => '0');  -- separate RAM
                        res(j).ops(i).ins.physicalArgSpec.dest := (others => '0'); -- separate RAM
                   
                        
                       -- res(j).ops(i).ins.virtualArgSpec.intDestSel := '0';
                       -- res(j).ops(i).ins.physicalArgSpec.intDestSel := '0';
                   
                   --res(slv2u(j)).ops(i).ins.tags := DEFAULT_INSTRUCTION_TAGS;
                   res(j).ops(i).ins.tags.fetchCtr := (others => '0');
                   res(j).ops(i).ins.tags.decodeCtr := (others => '0');
                   res(j).ops(i).ins.tags.renameCtr := (others => '0');
                            res(j).ops(i).ins.tags.renameIndex := (others => '0');
                   res(j).ops(i).ins.tags.commitCtr := (others => '0');
                   
                        res(j).ops(i).ins.tags.intPointer := (others => '0');
                        res(j).ops(i).ins.tags.floatPointer := (others => '0');
               end loop;
               
               res(j).special.ins.ip := (others => '0');
               res(j).special.ins.bits := (others => '0');              
               res(j).special.ins.result := (others => '0');
               res(j).special.ins.target := (others => '0');
               
               res(j).special.ins.constantArgs := DEFAULT_CONSTANT_ARGS;
            
               res(j).special.ins.operation := (System, sysUndef);
               
               res(j).special.ins.virtualArgSpec := DEFAULT_ARG_SPEC; 
               res(j).special.ins.physicalArgSpec := DEFAULT_ARG_SPEC;
               
               res(j).special.ins.tags := DEFAULT_INSTRUCTION_TAGS;                            
           end loop;
       end if;
	   
	   return res;
	end function;
	
	function replaceConstantInformation(insVec: InstructionSlotArray; constInfo, constInfo2: Word) return InstructionSlotArray is
	   variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := insVec;
	begin
	   for i in 0 to PIPE_WIDTH-1 loop
	       res(i).ins.physicalArgSpec.dest := constInfo(8*i + 7 downto 8*i);
	       res(i).ins.virtualArgSpec.dest := "000" & constInfo2(5*i + 4 downto 5*i);
	       
	       --        res(i).ins.virtualArgSpec.intDestSel := constInfo2(20 + 2*i + 0);
	       --        res(i).ins.physicalArgSpec.intDestSel := constInfo2(20 + 2*i + 0);
	   end loop;
	   
	   return res;
	end function;
	

	signal startPtr, startPtrNext, endPtr, acmPtr, causingPtr: SmallNumber := (others => '0');
	
	signal constantBuf, constantBuf2: WordArray(0 to ROB_SIZE-1) := (others => (others => '0'));
	signal inputConstant, inputConstant2, constantFromBuf, constantFromBuf2: Word := (others => '0');
	
    attribute ram_style: string;
    --attribute ram_style of constantBuf, constantBuf2: signal is "block";	
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
    
--        inputConstant2(27 downto 20) <=  inputData(3).ins.physicalArgSpec.intDestSel & inputData(3).ins.virtualArgSpec.intDestSel
--                                       & inputData(2).ins.physicalArgSpec.intDestSel & inputData(2).ins.virtualArgSpec.intDestSel
--                                       & inputData(1).ins.physicalArgSpec.intDestSel & inputData(1).ins.virtualArgSpec.intDestSel
--                                       & inputData(0).ins.physicalArgSpec.intDestSel & inputData(0).ins.virtualArgSpec.intDestSel;
                                       
--        inputConstant2(31 downto 28) <=  inputData(3).ins.physicalArgSpec.floatDestSel
--                                                                      & inputData(2).ins.physicalArgSpec.floatDestSel
--                                                                      & inputData(1).ins.physicalArgSpec.floatDestSel
--                                                                      & inputData(0).ins.physicalArgSpec.floatDestSel;
                                       
    
    CONSTANT_MEM: process (clk)
    begin
        if rising_edge(clk) then
            if prevSending = '1' then
                constantBuf(slv2u(endPtr)) <= inputConstant;
                constantBuf2(slv2u(endPtr)) <= inputConstant2;
            end if;
            
            constantFromBuf <= constantBuf(slv2u(startPtrNext));
            constantFromBuf2 <= constantBuf2(slv2u(startPtrNext));            
        end if;
    end process;


    startPtrNext <= startPtr when isSending = '0' else --addSN(startPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
                                                        addIntTrunc(startPtr, 1, ROB_PTR_SIZE);
							
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
                endPtr <= --addSN(causingPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
                            addIntTrunc(causingPtr, 1, ROB_PTR_SIZE);                            
            elsif prevSending = '1' then
                endPtr <= --addSN(endPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
                            addIntTrunc(endPtr, 1, ROB_PTR_SIZE);
            end if;
                   
            if lateEventSignal = '1' or execEvent = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= --subSN(recoveryCounter, i2slv(1, SMALL_NUMBER_SIZE));
                                    addInt(recoveryCounter, -1);
            end if;
            
            nFull <= nFullNext;
            
            -- CAREFUL: here we assure that the buffer is never full (so isFull incidcates 1 free slot). So startPtr = endPtr always indicates emptiness
            if --nFullNext > ROB_SIZE-1 then
                --cmpGreaterUnsignedSN(nFullNext, i2slv(ROB_SIZE-1-1, SMALL_NUMBER_SIZE)) = '1' then
                cmpGtU(nFullNext, ROB_SIZE-1-1) = '1' then
                isFull <= '1';
                isAlmostFull <= '1';
            elsif --nFullNext > ROB_SIZE-2 then
                --cmpGreaterUnsignedSN(nFullNext, i2slv(ROB_SIZE-2-1, SMALL_NUMBER_SIZE)) = '1' then
                cmpGtU(nFullNext, ROB_SIZE-2-1) = '1' then
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
        nFullNext <=     nFullRestored when --recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
                                            cmpEqU(recoveryCounter, 1) = '1'
                    else flowDiff and PTR_MASK_SN;
    end block;
	   
	FULL_MASK: for i in 0 to ROB_SIZE-1 generate
	   fullMask(i) <= content(i).full;
       completedMaskNext(i) <= groupCompleted(content(i).ops) and fullMask(i) and not isEmpty and not lateEventSignal;
	end generate;
	
    isSending <= completedMask(slv2u(startPtr)) and nextAccepting and not isEmpty;

	acceptingOut <= not isFull;
	
    acmPtr <= --addSN(endPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
                addIntTrunc(endPtr, 1, ROB_PTR_SIZE);
    acceptingMore <= not isAlmostFull;		
	outputData <= --outputDataReg;
	               replaceConstantInformation(outputDataReg, constantFromBuf, constantFromBuf2);

	sendingOut <= isSending;
	
	   outputSpecial <= outputSpecialReg;
	
	VIEW: block
	   signal robTxt: RobText;
	begin
	   robTxt <= getRobView(content);
	end block;

end Behavioral;
