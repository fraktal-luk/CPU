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

	--type BitArray2D is array(integer range<>, integer range<>) of std_logic;
	
	constant ROB_STATIC_DATA_SIZE: natural := 128;

		signal outputData_T: InstructionSlotArray(0 to PIPE_WIDTH-1);
		signal outputSpecial_T: InstructionSlot;

    
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
        
        --operation:  std_logic_vector(7 downto 0); -- TMP
        mainCluster: std_logic;
        secCluster: std_logic;
        useSQ:      std_logic;
        useLQ:      std_logic;
        useBQ:      std_logic;  -- May be better a Group param?
    end record;

    constant DEFAULT_STATIC_GROUP_INFO: StaticGroupInfo := (
        specialOp => (others => '0'),
        others => '0'
    );
    
    constant DEFAULT_STATIC_OP_INFO: StaticOpInfo := (
        virtualDest => (others => '0'),
        physicalDest => (others => '0'),
        others => '0'
    );

    
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

    constant DEFAULT_DYNAMIC_GROUP_INFO: DynamicGroupInfo := (
        others => '0'
    );
    
    constant DEFAULT_DYNAMIC_OP_INFO: DynamicOpInfo := (
        others => '0'
    );

    
    type StaticOpInfoArray is array(0 to PIPE_WIDTH-1) of StaticOpInfo;
    type StaticOpInfoArray2D is array(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) of StaticOpInfo;

    type StaticGroupInfoArray is array(0 to ROB_SIZE-1) of StaticGroupInfo;

 
    type DynamicOpInfoArray is array(0 to PIPE_WIDTH-1) of DynamicOpInfo;
    type DynamicOpInfoArray2D is array(0 to ROB_SIZE-1, 0 to PIPE_WIDTH-1) of DynamicOpInfo;

    type DynamicGroupInfoArray is array(0 to ROB_SIZE-1) of DynamicGroupInfo;
    

    function getStaticGroupInfo(isa: InstructionSlotArray; ssl: InstructionSlot) return StaticGroupInfo is
        variable res: StaticGroupInfo;
    begin
        res.specialOp := --ssl.ins.specificOperation.bits;
                            sop(None, ssl.ins.specificOperation.system).bits;
        
        res.useBQ := isa(0).ins.controlInfo.firstBr;
        return res;
    end function;
        

    function getDynamicGroupInfo(isa: InstructionSlotArray; ssl: InstructionSlot) return DynamicGroupInfo is
        variable res: DynamicGroupInfo;
    begin
            res.full := '0';--'1';
                
        return res;
    end function;
    

    
    function getStaticOpInfo(isl: InstructionSlot) return StaticOpInfo is
        variable res: StaticOpInfo;
    begin
        res.virtualIntDestSel := isl.ins.virtualArgSpec.intDestSel;
        res.virtualFloatDestSel := isl.ins.virtualArgSpec.floatDestSel;
        
        -- phys dest sel delds UNUSED?
        res.physicalIntDestSel := --isl.ins.physicalArgSpec.intDestSel;
                                  '0';
        res.physicalFloatDestSel := --isl.ins.physicalArgSpec.floatDestSel;
                                  '0';        
        res.virtualDest := isl.ins.virtualArgSpec.dest(4 downto 0);    
        res.physicalDest := isl.ins.physicalArgSpec.dest;
        
        -- cluster-fields UNUSED?
        res.mainCluster := --isl.ins.classInfo.mainCluster;
                            '0';
        res.secCluster := --isl.ins.classInfo.secCluster;
                            '0';
        res.useSQ := --'U';
                        isl.ins.classInfo.secCluster; -- ??
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
    


        function getDynamicOpInfo_T(isl: InstructionSlot) return DynamicOpInfo is
            variable res: DynamicOpInfo;
        begin
            res.full := isl.full;
            res.killed := isl.ins.controlInfo.killed;
            res.causing := isl.ins.controlInfo.causing;
            res.completed0 := isl.ins.controlInfo.completed; 
            res.completed1 := isl.ins.controlInfo.completed2;
            
            res.hasEvent := isl.ins.controlInfo.newEvent;
            res.hasException := isl.ins.controlInfo.hasException;
            res.confirmedBranch := isl.ins.controlInfo.confirmedBranch;
            res.specialAction := isl.ins.controlInfo.specialAction; -- ???
            res.refetch := isl.ins.controlInfo.refetch; --isl.ins.controlInfo.refetch;
            
            return res;
        end function;   
    
        function getDynamicOpInfoA_T(isa: InstructionSlotArray) return DynamicOpInfoArray is
            variable res: DynamicOpInfoArray;
        begin
            for i in isa'range loop
                res(i) := getDynamicOpInfo_T(isa(i));
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

    signal content, contentNext: ReorderBufferArray := DEFAULT_ROB_ARRAY;

	signal isSending, isEmpty, outputCompleted, outputEmpty: std_logic := '0';
	signal execEvent: std_logic := '0'; -- depends on input in slot referring to branch ops
	signal isFull, isAlmostFull: std_logic := '0';
	signal startPtr, startPtrNext, endPtr, endPtrNext, causingPtr: SmallNumber := (others => '0');
    signal constantFromBuf, constantFromBuf2, constantFromBuf3: Word := (others => '0');	
	signal ch0, ch1, ch2, ch3: std_logic := '0';
	
    attribute ram_style: string;
    --attribute ram_style of constantBuf, constantBuf2, constantBuf3: signal is "block";	
    --attribute ram_style of mem0, mem1: signal is "block";	
begin

	execEvent <= execEndSigs1(0).full and execEndSigs1(0).ins.controlInfo.newEvent;
	causingPtr <= getTagHighSN(execEndSigs1(0).ins.tags.renameIndex) and PTR_MASK_SN; -- TEMP!
	
    NEW_DEV: block
        signal staticInput, staticOutput, staticOutput_T: StaticOpInfoArray;
        signal dynamicInput, dynamicOutput, dynamicOutput_T: DynamicOpInfoArray;

        signal staticGroupInput, staticGroupOutput, staticGroupOutput_T: StaticGroupInfo;
        signal dynamicGroupInput, dynamicGroupOutput, dynamicGroupOutput_T: DynamicGroupInfo;
        
        signal staticContent: StaticOpInfoArray2D := (others => (others => DEFAULT_STATIC_OP_INFO));
        signal dynamicContent: DynamicOpInfoArray2D := (others => (others => DEFAULT_DYNAMIC_OP_INFO));

        signal staticGroupContent: StaticGroupInfoArray := (others => DEFAULT_STATIC_GROUP_INFO);
        signal dynamicGroupContent: DynamicGroupInfoArray := (others => DEFAULT_DYNAMIC_GROUP_INFO);
        
--        function writeStaticInput(content: StaticOpInfoArray2D; input: StaticOpInfoArray; ptr: SmallNumber) return StaticOpInfoArray2D is
--            variable res: StaticOpInfoArray2D := content;
--        begin
--            for i in input'range loop
--                res(slv2u(ptr), i) := input(i);
--            end loop;
--            return res;
--        end function;
        
        procedure writeStaticInput(signal content: inout StaticOpInfoArray2D; input: StaticOpInfoArray; ptr: SmallNumber) is
        begin
            for i in input'range loop
                content(slv2u(ptr), i) <= input(i);
            end loop;
        end procedure;
        
        procedure writeDynamicInput(signal content: inout DynamicOpInfoArray2D; input: DynamicOpInfoArray; ptr: SmallNumber) is
        begin
            for i in input'range loop
                content(slv2u(ptr), i) <= input(i);
            end loop;
        end procedure;
        
        procedure writeStaticGroupInput(signal content: inout StaticGroupInfoArray; input: StaticGroupInfo; ptr: SmallNumber) is
        begin
            --for i in input'range loop
                content(slv2u(ptr)) <= input;
            --end loop;
        end procedure;
        
        procedure writeDynamicGroupInput(signal content: inout DynamicGroupInfoArray; input: DynamicGroupInfo; ptr: SmallNumber) is
        begin
            --for i in input'range loop
                content(slv2u(ptr)) <= input;
            --end loop;
        end procedure;


        function readStaticOutput(content: StaticOpInfoArray2D; ptr: SmallNumber) return StaticOpInfoArray is
            variable res: StaticOpInfoArray;
        begin
            for i in res'range loop
                res(i):= content(slv2u(ptr), i);
            end loop;
            return res;
        end function;

        function readDynamicOutput(content: DynamicOpInfoArray2D; ptr: SmallNumber) return DynamicOpInfoArray is
            variable res: DynamicOpInfoArray;
        begin
            for i in res'range loop
                res(i):= content(slv2u(ptr), i);
            end loop;
            return res;
        end function;
        
        function readStaticGroupOutput(content: StaticGroupInfoArray; ptr: SmallNumber) return StaticGroupInfo is
        begin
            --for i in input'range loop
            return content(slv2u(ptr));
            --end loop;
        end function;

        function readDynamicGroupOutput(content: DynamicGroupInfoArray; ptr: SmallNumber) return DynamicGroupInfo is
        begin
            --for i in input'range loop
            return content(slv2u(ptr));
            --end loop;
        end function;

        
        procedure updateDynamicContent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlotArray; cluster: natural) is
            variable groupInd, opInd: natural;
            variable tagHigh,tagHighTrunc: SmallNumber;
        begin
            for i in execInfo'range loop
                tagHigh := getTagHighSN(execInfo(i).ins.tags.renameIndex);
                tagHighTrunc := tagHigh and PTR_MASK_SN;
                groupInd := slv2u(tagHighTrunc);
                opInd := slv2u(getTagLow(execInfo(i).ins.tags.renameIndex));
            
                --updateCompleted(content);
                if execInfo(i).full = '1' then
                    if cluster = 0 then
                        content(groupInd, opInd).completed0 <= '1';
                    elsif cluster = 1 then
                        content(groupInd, opInd).completed1 <= '1';
                    else
                        report "Cluster must be 0 or 1" severity failure;                    
                    end if;
                end if;
            end loop;
            
        end procedure;

        procedure updateDynamicGroupBranch(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot; ind: natural) is
            variable groupInd, opInd: natural;
            variable tagHigh,tagHighTrunc: SmallNumber;
            variable eventFound: boolean := false;
        begin
            tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
            tagHighTrunc := tagHigh and PTR_MASK_SN;
            groupInd := slv2u(tagHighTrunc);
            opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));

            for i in 0 to PIPE_WIDTH-1 loop
                if eventFound then
                    content(groupInd, i).full <= '0';
                    content(groupInd, i).killed <= '1';
                    
                elsif opInd = i then
                    if execInfo.ins.controlInfo.confirmedBranch = '1' then
                        content(groupInd, i).confirmedBranch <= '1';                    
                    end if;                
                
                    if execInfo.ins.controlInfo.newEvent = '1' then
                        content(groupInd, i).causing <= '1';                    
                    
                        eventFound:= true;
                    end if;
                end if;
                
            end loop;
        end procedure;

         
        procedure updateDynamicContentBranch(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot) is
            variable groupInd, opInd: natural;
            variable tagHigh,tagHighTrunc: SmallNumber;
        begin
                tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
                tagHighTrunc := tagHigh and PTR_MASK_SN;
                groupInd := slv2u(tagHighTrunc);
                opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));
            
                if execInfo.full = '1' then
                    updateDynamicGroupBranch(content, execInfo, groupInd);
                end if;
            
        end procedure;         



        procedure updateDynamicGroupMemEvent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot; ind: natural) is
            variable groupInd, opInd: natural;
            variable tagHigh,tagHighTrunc: SmallNumber;
            variable eventFound: boolean := false;
        begin
            tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
            tagHighTrunc := tagHigh and PTR_MASK_SN;
            groupInd := slv2u(tagHighTrunc);
            opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));

            for i in 0 to PIPE_WIDTH-1 loop
                if eventFound then
                    content(groupInd, i).full <= '0';
                    content(groupInd, i).killed <= '1';
                    
                elsif opInd = i then
                    if execInfo.ins.controlInfo.specialAction = '1' then
                        content(groupInd, i).specialAction <= '1';   
                        content(groupInd, i).refetch <= '1';                    
                        content(groupInd, i).causing <= '1';                    
                        eventFound:= true;
                    end if;
                end if;
                
            end loop;
        end procedure;

         
        procedure updateDynamicContentMemEvent(signal content: inout DynamicOpInfoArray2D; execInfo: InstructionSlot) is
            variable groupInd, opInd: natural;
            variable tagHigh,tagHighTrunc: SmallNumber;
        begin
                tagHigh := getTagHighSN(execInfo.ins.tags.renameIndex);
                tagHighTrunc := tagHigh and PTR_MASK_SN;
                groupInd := slv2u(tagHighTrunc);
                opInd := slv2u(getTagLow(execInfo.ins.tags.renameIndex));
            
                if execInfo.full = '1' then
                    updateDynamicGroupMemEvent(content, execInfo, groupInd);
                end if;
            
        end procedure;         
                       

             signal ch0, ch1, ch2, ch3: std_logic := '0';                     
    begin
        staticInput <= getStaticOpInfoA(inputData);
        dynamicInput <= getDynamicOpInfoA(inputData);
        
        staticGroupInput <= getStaticGroupInfo(inputData, inputSpecial);
        dynamicGroupInput <= getDynamicGroupInfo(inputData, inputSpecial);

            staticOutput_T <= getStaticOpInfoA(outputData_T);
            dynamicOutput_T <= getDynamicOpInfoA_T(outputData_T);

            staticGroupOutput_T <= getStaticGroupInfo(outputData_T, outputSpecial_T);
            dynamicGroupOutput_T <= getDynamicGroupInfo(outputData_T, outputSpecial_T);
        
            ch0 <= bool2std(staticOutput = staticOutput_T);
            ch1 <= bool2std(dynamicOutput = dynamicOutput_T);
            ch2 <= bool2std(staticGroupOutput = staticGroupOutput_T);
            ch3 <= bool2std(dynamicGroupOutput = dynamicGroupOutput_T);


--            ch0 <= bool2std(dynamicOutput(0) = dynamicOutput_T(0));
--            ch1 <= bool2std(dynamicOutput(1) = dynamicOutput_T(1));
--            ch2 <= bool2std(dynamicOutput(2) = dynamicOutput_T(2));
--            ch3 <= bool2std(dynamicOutput(3) = dynamicOutput_T(3));
        
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
                 end if;
                   
                -- Read output                    
                staticOutput <= readStaticOutput(staticContent, startPtrNext);
                staticGroupOutput <= readStaticGroupOutput(staticGroupContent, startPtrNext);

                dynamicOutput <= readDynamicOutput(dynamicContent, startPtrNext);
                dynamicGroupOutput <= readDynamicGroupOutput(dynamicGroupContent, startPtrNext);                    
            end if;
        end process;
	end block;
	
    
    SERIAL_INFO: block
        signal mem0, mem1: WordArray(0 to ROB_SIZE-1) := (others => (others => '0'));
        signal inputConstant, inputConstant2, inputConstant3, iw0, iw1, ow0, ow1: Word := (others => '0');    
    begin
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
                    mem0(slv2u(endPtr)) <= iw0;
                    mem1(slv2u(endPtr)) <= iw1;           
                end if;
                
                ow0 <= mem0(slv2u(startPtrNext));
                ow1 <= mem1(slv2u(startPtrNext));                        
            end if;
        end process;

    end block;

	
	
	-- Main NEXT function
	contentNext <= getNextRobContent(content, inputData,
	                                 inputSpecial,
	                                 execEndSigs1, execEndSigs2,
	                                 isSending, prevSending,
	                                 execEvent, lateEventSignal,
	                                 startPtr, endPtr, causingPtr);
			
	SYNCHRONOUS: process (clk)
	begin
		if rising_edge(clk) then		  
		    -- Regular content
            content <= contentNext;
            
            --completedMask <= completedMaskNext;
            outputDataReg <= content(slv2u(startPtrNext)).ops;
            outputSpecialReg <= content(slv2u(startPtrNext)).special;
         

        end if;        
    end process;         


    

    CTR_MANAGEMENT: block
        signal recoveryCounter,
               nFull, nFullNext, nFullRestored, nIn, nOut,
               ptrDiff, flowDiff: SmallNumber := (others => '0');
    begin
        startPtrNext <= startPtr when isSending = '0' else addIntTrunc(startPtr, 1, ROB_PTR_SIZE);
        
        endPtrNext <= startPtrNext when lateEventSignal = '1'
                else  addIntTrunc(causingPtr, 1, ROB_PTR_SIZE) when execEvent = '1'
                else  addIntTrunc(endPtr, 1, ROB_PTR_SIZE) when prevSending = '1'
                else  endPtr;
        
        -- UNUSED                        
        isEmpty <= bool2std(startPtr = endPtr); -- CAREFUL: elsewhere it MUST be assured that ROB never gets full because this would become incorrect. 'isFull' must mean 1 free slot

        -- nFull logic
        nIn <= i2slv(1, SMALL_NUMBER_SIZE) when prevSending = '1' else (others => '0');
        nOut <= i2slv(1, SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');
   
        ptrDiff <= subSN(endPtr, startPtrNext);
        nFullRestored <= ptrDiff and PTR_MASK_SN;
          
        flowDiff <= subSN(addSN(nFull, nIn), nOut);
        nFullNext <=     nFullRestored when cmpEqU(recoveryCounter, 1) = '1'
                    else flowDiff and PTR_MASK_SN;
 
        MANAAGEMENT: process (clk)
        begin
            if rising_edge(clk) then
                startPtr <= startPtrNext;
                endPtr <= endPtrNext;        
            	            
                outputEmpty <= bool2std(startPtrNext = endPtr) or lateEventSignal;                
                
                if lateEventSignal = '1' or execEvent = '1' then
                    recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
                elsif isNonzero(recoveryCounter) = '1' then
                    recoveryCounter <= addInt(recoveryCounter, -1);
                end if;
                
                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
                
                nFull <= nFullNext;
    
                isFull <= cmpGtU(nFullNext, ROB_SIZE-1-1);
                isAlmostFull <= cmpGtU(nFullNext, ROB_SIZE-1-2);
    
            end if;		
        end process;
                            
    end block;

	acceptingOut <= not isFull;
    acceptingMore <= not isAlmostFull;

	outputCompleted <= groupCompleted(outputDataReg);
    isSending <= outputCompleted and nextAccepting and not outputEmpty;
    
	sendingOut <= isSending;
	-- incorporating deserialized info
	outputData <= ( replaceConstantInformation(outputDataReg, constantFromBuf, constantFromBuf2, constantFromBuf3));
	outputSpecial <= replaceConstantInformationSpecial(outputSpecialReg, constantFromBuf3);



	outputData_T <= ( replaceConstantInformation(outputDataReg, constantFromBuf, constantFromBuf2, constantFromBuf3));
	outputSpecial_T <= replaceConstantInformationSpecial(outputSpecialReg, constantFromBuf3);


	
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
