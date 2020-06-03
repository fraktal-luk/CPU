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
use work.LogicQueues.all;


entity BranchQueue is
	generic(
		QUEUE_SIZE: integer := 8
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		acceptingOut: out std_logic;
		almostFull: out std_logic;
		
		acceptingBr: out std_logic;
		
		prevSending: in std_logic;
			prevSendingBr: in std_logic;
		dataIn: in InstructionSlotArray(0 to PIPE_WIDTH-1);
			dataInBr: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		storeValueInput: in InstructionSlot;
		compareAddressInput: in InstructionSlot;

		selectedDataOutput: out InstructionSlot;

		committing: in std_logic;
		robData: in InstructionSlotArray(0 to PIPE_WIDTH-1);
		groupCtrInc: in InsTag;

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;
		
		nextAccepting: in std_logic;		
		sendingSQOut: out std_logic;
		dataOutV: out InstructionSlotArray(0 to PIPE_WIDTH-1)
	);
end BranchQueue;


architecture Behavioral of BranchQueue is    
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

	signal isSending: std_logic := '0';

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, taggedMask, killMask, livingMask, frontMask, sendingMask, inputMask, inputMaskBr,
			 fullMaskNext, taggedMaskNext, taggedLivingMask, matchMask, matchMaskUpdate: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot, selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	
	signal dataOutSig, dataOutSigNext, dataOutSigFinal: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

	signal pStart, pStartNext, pTagged, pAll, causingPtr: SmallNumber := (others => '0');
    signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0');
    signal recoveryCounter: SmallNumber := (others => '0');
    signal isFull, isAlmostFull: std_logic := '0';
	

    function getNewContentBr(content: InstructionStateArray; dataIn, dataInBr: InstructionSlotArray;
                                taggedMask, fullMask: std_logic_vector;
				                prevSending, prevSendingBr: std_logic;
				                inputMask, inputMaskBr: std_logic_vector;
				                pTagged, pAll: SmallNumber;
				                matchMaskUpdate: std_logic_vector;
				                storeValueInput: InstructionSlot
				                )
    return InstructionStateArray is
        variable res: InstructionStateArray(0 to QUEUE_SIZE-1) := content;
        variable slot: InstructionSlot;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    variable im: std_logic_vector(0 to QUEUE_SIZE-1) := inputMask;
	    variable imBr: std_logic_vector(0 to QUEUE_SIZE-1) := inputMaskBr;	    
    begin

        -- TODO, CHECK: clear newEvent if unneeded? Other controlInfo except frontBranch, confirmedBranch, firstBr?
        -- TODO: add 'completed' flag for debug?  

        for i in 0 to QUEUE_SIZE-1 loop
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pTagged) and PTR_MASK_SN;
           
           case diff(1 downto 0) is
               when "00" =>
                    remv := "111";
               when "01" =>
                    remv := "110";
               when "10" =>
                    remv := "100";
               when others =>
                    remv := "000";                                                                                       
           end case;
           
           if im(i) = '1' then
               slot := getNewElem(remv, dataIn); 
                       --dataIn(slv2u(diff(1 downto 0)));         
               res(i).tags := slot.ins.tags;               
               res(i).controlInfo.firstBr := '0'; -- This is '1' only for the first branch in group!               
           end if;
        end loop;

        -- Set firstBr bit for the first entry in new group
        if prevSending = '1' then
            res(slv2u(pTagged)).controlInfo.firstBr := '1';
        end if;

        for i in 0 to QUEUE_SIZE-1 loop
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pAll) and PTR_MASK_SN;
           
           case diff(1 downto 0) is
               when "00" =>
                    remv := "111";
               when "01" =>
                    remv := "110";
               when "10" =>
                    remv := "100";
               when others =>
                    remv := "000";                                                                                       
           end case;
           
           if imBr(i) = '1' then
              slot := getNewElem(remv, dataInBr);
                     --dataInBr(slv2u(diff(1 downto 0)));  
              res(i) := slot.ins;                       
           end if;
        end loop;

        -- Update target after branch execution
        for i in 0 to QUEUE_SIZE-1 loop
           if taggedMask(i) = '1' -- !! Prevent instruction with renmeIndex. = 0 from updating untagged entries! 
               and matchMaskUpdate(i) = '1'
               and storeValueInput.full = '1'
           then
               res(i).target := storeValueInput.ins.target;
               res(i).controlInfo.confirmedBranch := storeValueInput.ins.controlInfo.confirmedBranch;
               res(i).controlInfo.newEvent := storeValueInput.ins.controlInfo.newEvent;
           end if;
           
           if CLEAR_DEBUG_INFO then
              res(i) := clearRawInfo(res(i));
              res(i) := clearDbCounters(res(i));
              
              res(i).specificOperation := DEFAULT_SPECIFIC_OP;

              res(i).constantArgs := DEFAULT_CONSTANT_ARGS;
              res(i).virtualArgSpec := DEFAULT_ARG_SPEC;
              res(i).physicalArgSpec := DEFAULT_ARG_SPEC;
             end if;                    
        end loop;

        return res;
    end function;

begin
    causingPtr <= getCausingPtr(content, execCausing);
    
	-- in shifting queue this would be shfited by nSend
	frontMask <= getSendingMask(content, taggedLivingMask, groupCtrInc);
	sendingMask <= frontMask when committing = '1' else (others => '0');

	killMask <= getKillMask(content, taggedMask, execCausing, execEventSignal, lateEventSignal);
	livingMask <= fullMask when (lateEventSignal = '0' and execEventSignal = '0') 
	         else taggedLivingMask;	
    taggedLivingMask <= taggedMask and not killMask;
				
    inputMask <= getInputMask(taggedMask, extractFullMask(dataIn), prevSending, pTagged, PTR_MASK_SN);				
    inputMaskBr <= getInputMask(fullMask, extractFullMask(dataInBr), prevSendingBr, pAll, PTR_MASK_SN);
				
	fullMaskNext <= (livingMask and not sendingMask) or inputMaskBr;
	taggedMaskNext <= (taggedLivingMask and not sendingMask) or inputMask;
	
	contentNext <=
				getNewContentBr(content, dataIn, dataInBr,
				                taggedMask, fullMask,
				                prevSending, prevSendingBr,
				                inputMask, inputMaskBr,				             
				                pTagged, pAll,
				                matchMaskUpdate,
				                storeValueInput);
				                
	dataOutSigNext <= getWindow(content, taggedMask, pStartNext, PIPE_WIDTH);
	selectedDataSlot <= selectBranchDataSlot(content, taggedMask, matchMask, compareAddressInput);
	matchMask <= getMatchingTags(content, compareAddressInput.ins.tags.renameIndex);
	
    pStartNext <= addIntTrunc(pStart, getNumberToSend(dataOutSig, groupCtrInc, committing), QUEUE_PTR_SIZE);
	
	process (clk)
	begin
		if rising_edge(clk) then	
            fullMask <= fullMaskNext;
            taggedMask <= taggedMaskNext;
			content <= contentNext;
            pStart <= pStartNext;
			
			selectedDataOutputSig <= selectedDataSlot;
            dataOutSig <= dataOutSigNext;
            
            matchMaskUpdate <= matchMask;
            
            if lateEventSignal = '1' then
                pTagged <= pStartNext;
            elsif execEventSignal = '1' then
                pTagged <= addIntTrunc(causingPtr, 1, QUEUE_PTR_SIZE);
            elsif prevSending = '1' then
                pTagged <= addIntTrunc(pTagged, countOnes(inputMask), QUEUE_PTR_SIZE);                
            end if;

            if lateEventSignal = '1' then
                pAll <= pStartNext;
            elsif execEventSignal = '1' then
                pAll <= addIntTrunc(causingPtr, 1, QUEUE_PTR_SIZE);       
            elsif prevSendingBr = '1' then
                pAll <= addIntTrunc(pAll, countOnes(inputMaskBr), QUEUE_PTR_SIZE);                
            end if;
               
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif isNonzero(recoveryCounter) = '1' then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            
                recoveryCounter(7 downto 1) <= (others => '0'); -- Only 1 bit needed here
            
            isFull <=       cmpGtU(nFullNext, QUEUE_SIZE-4);
            isAlmostFull <= cmpGtU(nFullNext, QUEUE_SIZE-8);
            nFull <= nFullNext;                     
		end if;
	end process;

    nFullNext <= nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
                else subTruncZ(add(nFull, nIn), nOut, QUEUE_CAP_SIZE);

    nIn <= i2slv( countOnes(extractFullMask(dataInBr)), SMALL_NUMBER_SIZE ) when prevSendingBr = '1' else (others => '0');

    nOut <= i2slv(countOnes(extractFullMask(dataOutSigFinal)), SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');              
    nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pAll and fullMask(0) = '1'
                else subTruncZ(pAll, pStartNext, QUEUE_PTR_SIZE);  -- CAREFUL: nFullRestored can be outside PTR range but it's handled in the other branch
    
    dataOutSigFinal <= getSendingArray(dataOutSig, groupCtrInc, committing);

    isSending <= dataOutSigFinal(0).full;
	acceptingOut <= '1';
	acceptingBr <= not isAlmostFull;     
 
	dataOutV <= dataOutSigFinal;                   
	sendingSQOut <= isSending;

	selectedDataOutput <= selectedDataSlot;
	almostFull <= '0'; -- TODO: is it deprecated?
	
	VIEW: if VIEW_ON generate
       use work.Viewing.all;
      
       signal queueText: InsStringArray(0 to QUEUE_SIZE-1);
    begin       
       queueText <= getInsStringArray(makeSlotArray(content, fullMask), control);       
    end generate;

end Behavioral;
