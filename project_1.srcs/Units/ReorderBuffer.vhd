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


------
    

	type BitArray2D is array(integer range<>, integer range<>) of std_logic;
	
	-- Just for view?
	--signal fullMask:        std_logic_vector(0 to ROB_SIZE-1) := (others => '0');
	
	--signal completedArray0: BitArray2D(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
	--signal completedArray1: BitArray2D(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
	
	-- ??
	--signal killedArray1:    BitArray2D(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));

    -- TRIAL: marking which ops have some events whose content would be stored in a different queue
	--signal eventArray:      BitArray2D(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) := (others => (others => '0'));
	
	constant ROB_STATIC_DATA_SIZE: natural := 128;
    
    -- change to array of bit vectors
    signal staticData:      BitArray2D(0 to ROB_SIZE-1, 0 to ROB_STATIC_DATA_SIZE-1) := (others => (others => '0'));
    

	
	--   .ins.physicalArgSpec.dest, .ins.virtualArgSpec.dest, .ins.physicalArgSpec.intDestSel, .ins.physicalArgSpec.floatDestSel,
    --	                                                      .ins.virtualArgSpec.intDestSel,  .ins.virtualArgSpec.floatDestSel,
    --    COMMON: i2slv(SysOp'pos(inputSpecial.ins.specificOperation.system), SYS_OP_SIZE);
    --      
    
    -- TMP!
    
    type StaticGroupInfo is record
        specialOp: std_logic_vector(3 downto 0); -- TMP
        useBQ:      std_logic; -- Maybe only here?
    end record;
    
    type StaticOpInfo is record
        virtualIntDestSel:     std_logic;
        virtualFloatDestSel:   std_logic;
        physicalIntDestSel:     std_logic;
        physicalFloatDestSel:   std_logic;
        virtualDest:    RegName;    
        physicalDest:   PhysName;
        
        operation:  std_logic_vector(7 downto 0); -- TMP
        mainCluster: std_logic;
        secCluster: std_logic;
        useSQ:      std_logic;
        useLQ:      std_logic;
        useBQ:      std_logic;  -- May be better a Group param?
    end record;


    
    type DynamicGroupInfo is record
        full: std_logic;
    end record;
    
    type DynamicOpInfo is record
        full:       std_logic;
        killed:     std_logic;
        causing:    std_logic;
        completed0: std_logic;
        completed1: std_logic;
        
        hasEvent:     std_logic;
        hasException: std_logic;
        confirmedBranch: std_logic;
        specialAction: std_logic;
        refetch: std_logic;
    end record;
    
    type StaticOpInfoArray is array(0 to PIPE_WIDTH-1) of StaticOpInfo;
    type StaticOpInfoArray2D is array(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) of StaticOpInfo;
 
    type DynamicOpInfoArray is array(0 to PIPE_WIDTH-1) of DynamicOpInfo;
    type DynamicOpInfoArray2D is array(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) of DynamicOpInfo;
    

    
    
    function getStaticOpInfo(isl: InstructionSlot) return StaticOpInfo is
        variable res: StaticOpInfo;
    begin
        res.virtualIntDestSel := isl.ins.virtualArgSpec.intDestSel;
        res.virtualFloatDestSel := isl.ins.virtualArgSpec.floatDestSel;
        res.physicalIntDestSel := isl.ins.physicalArgSpec.intDestSel;
        res.physicalFloatDestSel := isl.ins.physicalArgSpec.floatDestSel;        
        res.virtualDest := isl.ins.virtualArgSpec.dest(4 downto 0);    
        res.physicalDest := isl.ins.physicalArgSpec.dest;
        
        res.operation := (others => 'U');
        res.mainCluster := isl.ins.classInfo.mainCluster;
        res.secCluster := isl.ins.classInfo.secCluster;
        res.useSQ := 'U';
        res.useLQ := isl.ins.classInfo.useLQ;
        res.useBQ := isl.ins.classInfo.branchIns;
        
        return res;
    end function;

    function getStaticOpInfoA(isa: InstructionSlotArray) return StaticOpInfoArray is
        variable res: StaticOpInfoArray;
    begin
        for i in isa'range loop
            res(i) := getStaticOpInfo(isa(i));
        end loop; 
        
        return res;
    end function;


    function getDynamicOpInfo(isl: InstructionSlot) return DynamicOpInfo is
        variable res: DynamicOpInfo;
    begin
        res.full := isl.full;
        res.killed := '0';
        res.causing := '0';
        res.completed0 := not isl.ins.classInfo.mainCluster; 
        res.completed1 := not isl.ins.classInfo.secCluster;
        
        res.hasEvent := '0';
        res.hasException := '0';
        res.confirmedBranch := isl.ins.controlInfo.confirmedBranch;
        res.hasException := '0';
        res.specialAction := isl.ins.controlInfo.specialAction; -- ???
        res.refetch := '0'; --isl.ins.controlInfo.refetch;
        
        return res;
    end function;   

    function getDynamicOpInfoA(isa: InstructionSlotArray) return DynamicOpInfoArray is
        variable res: DynamicOpInfoArray;
    begin
        for i in isa'range loop
            res(i) := getDynamicOpInfo(isa(i));
        end loop; 
        
        return res;
    end function;
    
    
    --subtype BitVector is std_logic_vector(natural range 0 to natural'high);


    function serializeOp(isl: InstructionSlot) return std_logic_vector is
    begin
    
    end function;
    
    function deserializeOp(isl: InstructionSlot; serialData: std_logic_vector) return InstructionSlotArray is
    begin
    
    end function; 

    
    function serializeSpecialAction(isl: InstructionSlot) return std_logic_vector is
    begin
    
    end function;
    
    function deserializeSpecialAction(isl: InstructionSlot; serialData: std_logic_vector) return InstructionSlotArray is
    begin
    
    end function; 
    

    function serializeOpGroup(insVec: InstructionSlotArray) return std_logic_vector is
    begin
    
    end function;
    
    function deserializeOpGroup(insVec: InstructionSlotArray; serialData: std_logic_vector) return InstructionSlotArray is
    begin
    
    end function; 
 
-------



    signal outputDataReg: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
    signal outputSpecialReg: InstructionSlot := DEFAULT_INS_SLOT;
	--signal fullMask_O, completedMask, completedMaskNext: std_logic_vector(0 to ROB_SIZE-1) := (others => '0');

    signal content, contentNext: ReorderBufferArray := DEFAULT_ROB_ARRAY;

	signal isSending, isEmpty, outputCompleted, outputEmpty: std_logic := '0';
	signal execEvent: std_logic := '0'; -- depends on input in slot referring to branch ops

	constant PTR_MASK_TAG: InsTag := i2slv(ROB_SIZE-1, TAG_SIZE);
	constant PTR_MASK_SN: SmallNumber := i2slv(ROB_SIZE-1, SMALL_NUMBER_SIZE);
	constant ROB_PTR_SIZE: natural := countOnes(PTR_MASK_SN);	





	signal recoveryCounter: SmallNumber := (others => '0');
	signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0'); 
	
	signal isFull, isAlmostFull: std_logic := '0'; 	


	function getNextRobContent_Input(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           newSpecialAction: InstructionSlot;
	                           receiving: std_logic;
	                           endPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	begin
	   
	   -- Input
	   if receiving = '1' then
	       res(slv2u(endPtr)).ops := newGroup;
	       res(slv2u(endPtr)).special := newSpecialAction;  
	   end if;
	   
	   return res;
	end function; 
	
	function getNextRobContent_Update(content: ReorderBufferArray;
	                           execInfo1, execInfo2: InstructionSlotArray
	                           )
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable ptr1, ptr2,  iv: SmallNumber := (others => '0');
	   variable newInsState: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
	   
	   -- Update
	   for i in 0 to ROB_SIZE-1 loop
	       iv := i2slv(i, SMALL_NUMBER_SIZE);
	       
	       -- Update group!
	       -- NOTE: tag comparison for slot will be simplified because tag bits directly show ROB slot:
	       --          [ upper bits | ROB slot | index in group]
	       --            (getTagHighSN([tag]) and PTR_MASK) == iv ?
	       --             getTagLowSN == [group index] ? // this is within updateOpGroup  
	       for j in 0 to 3 loop
	           ptr1 := getTagHighSN(execInfo1(j).ins.tags.renameIndex) and PTR_MASK_SN;
	           if ptr1 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe	               
	           end if;
	           
               ptr2 := getTagHighSN(execInfo2(j).ins.tags.renameIndex) and PTR_MASK_SN;	           
	           if ptr2 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo2(j), 1, false);
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo2(j), 1, false);	               
	           end if;
	       end loop;

	   end loop;
        
       return res;
    end function;

	function getNextRobContent_ClearDB(content: ReorderBufferArray) --;
	                           --execInfo1, execInfo2: InstructionSlotArray;
	                           --execEvent, lateEventSignal: std_logic;
	                           --startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable ptr1, ptr2,  iv: SmallNumber := (others => '0');
	   variable newInsState: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
       -- Clear unused fields for better synthesis
       if CLEAR_DEBUG_INFO then
           for j in 0 to ROB_SIZE-1 loop
               for i in 0 to PIPE_WIDTH-1 loop
                   -- controlInfo survives here, other useful data is stored in separate mem array because the rest is immutable
                   newInsState := res(j).ops(i).ins;
                   res(j).ops(i).ins := DEFAULT_INSTRUCTION_STATE;
                   res(j).ops(i).ins.controlInfo := newInsState.controlInfo;
                        -- CAREFUL: info aobut stores needed for StoreQueue
                        res(j).ops(i).ins.classInfo.secCluster := newInsState.classInfo.secCluster;
                        res(j).ops(i).ins.classInfo.useLQ := newInsState.classInfo.useLQ;
               end loop;

                   newInsState := res(j).special.ins;
                   res(j).special.ins.controlInfo := newInsState.controlInfo;
                   res(j).special.ins.specificOperation := newInsState.specificOperation;                          
           end loop;
       end if;
	   
	   return res;
	end function;
	
	function getNextRobContent(content: ReorderBufferArray;
	                           newGroup: InstructionSlotArray;
	                           newSpecialAction: InstructionSlot;
	                           execInfo1, execInfo2: InstructionSlotArray;
	                           sends, receiving, execEvent, lateEventSignal: std_logic;
	                           startPtr, endPtr, causingPtr: SmallNumber)
	return ReorderBufferArray is
	   variable res: ReorderBufferArray := content;
	   variable ptr1, ptr2,  iv: SmallNumber := (others => '0');
	   variable newInsState: InstructionState := DEFAULT_INSTRUCTION_STATE;
	begin
	   
	   -- Input
	   if receiving = '1' then
	       res(slv2u(endPtr)).ops := newGroup;
	       res(slv2u(endPtr)).special := newSpecialAction;  
	   end if;
	   
	   -- Update
	   for i in 0 to ROB_SIZE-1 loop
	       iv := i2slv(i, SMALL_NUMBER_SIZE);
	       
	       -- Update group!
	       -- NOTE: tag comparison for slot will be simplified because tag bits directly show ROB slot:
	       --          [ upper bits | ROB slot | index in group]
	       --            (getTagHighSN([tag]) and PTR_MASK) == iv ?
	       --             getTagLowSN == [group index] ? // this is within updateOpGroup  
	       for j in 0 to 3 loop
	           ptr1 := getTagHighSN(execInfo1(j).ins.tags.renameIndex) and PTR_MASK_SN;
	           if ptr1 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo1(j), 0, j = 2); -- [2] is Mem subpipe	               
	           end if;
	           
               ptr2 := getTagHighSN(execInfo2(j).ins.tags.renameIndex) and PTR_MASK_SN;	           
	           if ptr2 = iv then
	               res(i).ops := updateOpGroup(res(i).ops, execInfo2(j), 1, false);
	               res(i).ops := updateOpGroupCompleted(res(i).ops, execInfo2(j), 1, false);	               
	           end if;
	       end loop;

	   end loop;

       -- Clear unused fields for better synthesis
       if CLEAR_DEBUG_INFO then
           for j in 0 to ROB_SIZE-1 loop
               for i in 0 to PIPE_WIDTH-1 loop
                   -- controlInfo survives here, other useful data is stored in separate mem array because the rest is immutable
                   newInsState := res(j).ops(i).ins;
                   res(j).ops(i).ins := DEFAULT_INSTRUCTION_STATE;
                   res(j).ops(i).ins.controlInfo := newInsState.controlInfo;
                        -- CAREFUL: info aobut stores needed for StoreQueue
                        res(j).ops(i).ins.classInfo.secCluster := newInsState.classInfo.secCluster;
                        res(j).ops(i).ins.classInfo.useLQ := newInsState.classInfo.useLQ;
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
	   if CLEAR_DEBUG_INFO then
           for i in 0 to PIPE_WIDTH-1 loop
	           res(i).ins := DEFAULT_INSTRUCTION_STATE;
	           res(i).ins.controlInfo := insVec(i).ins.controlInfo;
	               res(i).ins.classInfo.secCluster := insVec(i).ins.classInfo.secCluster;
	               res(i).ins.classInfo.useLQ := insVec(i).ins.classInfo.useLQ;
	       end loop;       
	   end if;
	
	   for i in 0 to PIPE_WIDTH-1 loop
	       res(i).ins.physicalArgSpec.dest := constInfo(8*i + 7 downto 8*i);
	       res(i).ins.virtualArgSpec.dest := "000" & constInfo2(5*i + 4 downto 5*i);
	       
	       res(i).ins.virtualArgSpec.intDestSel := constInfo3(4 + i);
	       res(i).ins.virtualArgSpec.floatDestSel := constInfo3(0 + i);
	       
	       --res(i).ins.physicalArgSpec.intDestSel := constInfo3(4 + i);
	       --res(i).ins.physicalArgSpec.floatDestSel := constInfo3(0 + i);
	              
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
	   
	   if CLEAR_DEBUG_INFO then
	       res.ins := DEFAULT_INS_STATE;
	       res.ins.controlInfo := special.ins.controlInfo;
	       res.ins.specificOperation.subpipe := None;
	       res.ins.specificOperation.system := SysOp'val(num);
	   end if;
	   
	   return res;
	end function;

	signal startPtr, startPtrNext, endPtr, causingPtr: SmallNumber := (others => '0');
	
	signal constantBuf, constantBuf2, constantBuf3, mem0, mem1: WordArray(0 to ROB_SIZE-1) := (others => (others => '0'));
	signal inputConstant, inputConstant2, inputConstant3, constantFromBuf, constantFromBuf2, constantFromBuf3, iw0, iw1, ow0, ow1: Word := (others => '0');
	
	   signal ch0, ch1, ch2, ch3: std_logic := '0';
	
    attribute ram_style: string;
    --attribute ram_style of constantBuf, constantBuf2, constantBuf3: signal is "block";	
    --attribute ram_style of mem0, mem1: signal is "block";	
begin

	execEvent <= execEndSigs1(0).full and execEndSigs1(0).ins.controlInfo.newEvent;
	causingPtr <= getTagHighSN(execEndSigs1(0).ins.tags.renameIndex) and PTR_MASK_SN; -- TEMP!
	
    NEW_DEV: block
        signal inputStaticOpInfoA: StaticOpInfoArray;
        signal inputDynamicOpInfoA: DynamicOpInfoArray;    
    begin
        inputStaticOpInfoA <= getStaticOpInfoA(inputData);
        inputDynamicOpInfoA <= getDynamicOpInfoA(inputData);
	end block;
	

    -- Serialize some input
    inputConstant <= inputData(3).ins.physicalArgSpec.dest & inputData(2).ins.physicalArgSpec.dest & inputData(1).ins.physicalArgSpec.dest & inputData(0).ins.physicalArgSpec.dest;
    inputConstant2(19 downto 0) <= inputData(3).ins.virtualArgSpec.dest(4 downto 0) & inputData(2).ins.virtualArgSpec.dest(4 downto 0)
                    & inputData(1).ins.virtualArgSpec.dest(4 downto 0) & inputData(0).ins.virtualArgSpec.dest(4 downto 0);
    
    inputConstant3(15 downto 8) <=  inputData(3).ins.physicalArgSpec.intDestSel --& inputData(3).ins.physicalArgSpec.floatDestSel
                                 & inputData(2).ins.physicalArgSpec.intDestSel --& inputData(2).ins.physicalArgSpec.floatDestSel
                                 & inputData(1).ins.physicalArgSpec.intDestSel --& inputData(1).ins.physicalArgSpec.floatDestSel
                                 & inputData(0).ins.physicalArgSpec.intDestSel --& inputData(0).ins.physicalArgSpec.floatDestSel;

                                 & inputData(3).ins.physicalArgSpec.floatDestSel
                                 & inputData(2).ins.physicalArgSpec.floatDestSel
                                 & inputData(1).ins.physicalArgSpec.floatDestSel
                                 & inputData(0).ins.physicalArgSpec.floatDestSel;
    
    inputConstant3(7 downto 0) <=  inputData(3).ins.virtualArgSpec.intDestSel --& inputData(3).ins.virtualArgSpec.floatDestSel
                                  & inputData(2).ins.virtualArgSpec.intDestSel --& inputData(2).ins.virtualArgSpec.floatDestSel
                                  & inputData(1).ins.virtualArgSpec.intDestSel --& inputData(1).ins.virtualArgSpec.floatDestSel
                                  & inputData(0).ins.virtualArgSpec.intDestSel --& inputData(0).ins.virtualArgSpec.floatDestSel;

                                  & inputData(3).ins.virtualArgSpec.floatDestSel
                                  & inputData(2).ins.virtualArgSpec.floatDestSel
                                  & inputData(1).ins.virtualArgSpec.floatDestSel
                                  & inputData(0).ins.virtualArgSpec.floatDestSel;

    inputConstant3(SYS_OP_SIZE - 1 + 16 downto 16) <= i2slv(SysOp'pos(inputSpecial.ins.specificOperation.system), SYS_OP_SIZE);

        iw0 <= inputConstant;
        iw1 <= inputConstant3(SYS_OP_SIZE-1+16 downto 16) & inputConstant3(7 downto 0) & inputConstant2(19 downto 0);

        
        -- Deserialize
        constantFromBuf <= ow0;
        constantFromBuf2(19 downto 0) <= ow1(19 downto 0);
        constantFromBuf3(7 downto 0) <= ow1(27 downto 20);
        constantFromBuf3(SYS_OP_SIZE-1+16 downto 16) <= ow1(31 downto 28);

    CONSTANT_MEM: process (clk)
    begin
        if rising_edge(clk) then
            if prevSending = '1' then
                constantBuf(slv2u(endPtr)) <= inputConstant;
                constantBuf2(slv2u(endPtr)) <= inputConstant2;
                constantBuf3(slv2u(endPtr)) <= inputConstant3;
                
                mem0(slv2u(endPtr)) <= iw0;
                mem1(slv2u(endPtr)) <= iw1;           
            end if;
            
            ow0 <= mem0(slv2u(startPtrNext));
            ow1 <= mem1(slv2u(startPtrNext));                        
        end if;
    end process;



	
	
	-- Main NEXT function
	contentNext <= getNextRobContent(content, inputData,
	                                 inputSpecial,
	                                 execEndSigs1, execEndSigs2,
	                                 isSending, prevSending,
	                                 execEvent, lateEventSignal,
	                                 startPtr, endPtr, causingPtr);

    startPtrNext <= startPtr when isSending = '0' else addIntTrunc(startPtr, 1, ROB_PTR_SIZE);
	
	-- UNUSED						
	isEmpty <= bool2std(startPtr = endPtr); -- CAREFUL: elsewhere it MUST be assured that ROB never gets full because this would become incorrect. 'isFull' must mean 1 free slot
							
	SYNCHRONOUS: process (clk)
	begin
		if rising_edge(clk) then		  
		    -- Regular content
            content <= contentNext;

            startPtr <= startPtrNext;
            
            -- endPtr logic
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
            
                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
            
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
            
            --completedMask <= completedMaskNext;
            outputDataReg <= content(slv2u(startPtrNext)).ops;
            outputSpecialReg <= content(slv2u(startPtrNext)).special;
            
            outputEmpty <= bool2std(startPtrNext = endPtr) or lateEventSignal;
		end if;		
	end process;
    
    
    -- nFull logic
    nIn <= i2slv(1, SMALL_NUMBER_SIZE) when prevSending = '1' else (others => '0');
    nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');
       
    CTR_MANAGEMENT: block
        signal ptrDiff, flowDiff: SmallNumber := (others => '0');
    begin
        ptrDiff <= subSN(endPtr, startPtrNext);
        nFullRestored <= ptrDiff and PTR_MASK_SN;
          
        flowDiff <= subSN(addSN(nFull, nIn), nOut);
        nFullNext <=     nFullRestored when cmpEqU(recoveryCounter, 1) = '1'
                    else flowDiff and PTR_MASK_SN;
    end block;
	   
--	FULL_MASK: for i in 0 to ROB_SIZE-1 generate
--	   --fullMask_O(i) <= content(i).full;
--       --completedMaskNext(i) <= groupCompleted(content(i).ops) and fullMask_O(i)
--       --                             and not isEmpty and not lateEventSignal;
--	end generate;
	

	acceptingOut <= not isFull;
    acceptingMore <= not isAlmostFull;
    


	outputCompleted <= groupCompleted(outputDataReg);
    isSending <= outputCompleted and nextAccepting and not outputEmpty;
    
	sendingOut <= isSending;
	-- incorporating deserialized info
	outputData <= ( replaceConstantInformation(outputDataReg, constantFromBuf, constantFromBuf2, constantFromBuf3));
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
