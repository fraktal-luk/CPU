----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:57:56 12/11/2016 
-- Design Name: 
-- Module Name:    MemoryUnit - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
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

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, taggedMask, killMask, livingMask, frontMask, sendingMask, inputMask, inputMaskBr,
			 fullMaskNext, taggedMaskNext, taggedLivingMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot, selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;	
	signal dataOutSig, dataOutSigOld, dataOutSigNext, dataOutSigFinal: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);

	signal pStart, pStartNext, pTagged, pAll, causingPtr, pAcc: SmallNumber := (others => '0');
	   signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0');
	   signal recoveryCounter: SmallNumber := (others => '0');
	   signal isSending, isFull, isAlmostFull: std_logic := '0'; 	
	

    function getNewContentBr(content: InstructionStateArray; dataIn, dataInBr: InstructionSlotArray;
                                taggedMask, fullMask: std_logic_vector;
				                prevSending, prevSendingBr: std_logic;
				                inputMask, inputMaskBr: std_logic_vector;
				                pTagged, pAll: SmallNumber;
				                storeValueInput: InstructionSlot
				                )
    return InstructionStateArray is
        variable res: InstructionStateArray(0 to QUEUE_SIZE-1) := content;
        variable slot: InstructionSlot;
        --variable sel: natural range 0 to 3 := 0;
        variable diff: SmallNumber := (others => '0');
	    variable remv: std_logic_vector(0 to 2) := "000";
	    --variable compMask, compMaskBr: std_logic_vector(0 to 3) := "0000";
	    variable im: std_logic_vector(0 to QUEUE_SIZE-1) := inputMask;
	    variable imBr: std_logic_vector(0 to QUEUE_SIZE-1) := inputMaskBr;	    
    begin
        --compMask := compactMask(extractFullMask(dataIn));
        --compMaskBr := compactMask(extractFullMask(dataInBr));

        for i in 0 to QUEUE_SIZE-1 loop
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pTagged) and PTR_MASK_SN;
           --sel := slv2u(diff(1 downto 0));
           
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
           
           --sel := slv2u(getSelector(remv, extractFullMask(dataIn)(0 to 2)));
           if im(i) = '1' then
               --res(i).tags := dataIn(sel).ins.tags;
               --res(i).operation := dataIn(sel).ins.operation;
                    slot := getNewElem(remv, dataIn);          
                    res(i).tags := slot.ins.tags;
                    res(i).operation := slot.ins.operation;
                        res(i).controlInfo.firstBr := '0'; -- This is '1' only for the first branch in group!               
           end if;
        end loop;

        -- Set firstBr bit for the first entry in new group
        if prevSending = '1' then
            res(slv2u(pTagged)).controlInfo.firstBr := '1';
        end if;

        for i in 0 to QUEUE_SIZE-1 loop
           diff := subSN( i2slv(i, SMALL_NUMBER_SIZE), pAll) and PTR_MASK_SN;
           --sel := slv2u(diff(1 downto 0));
           
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
           
           --sel := slv2u(getSelector(remv, extractFullMask(dataInBr)(0 to 2))); 
           if imBr(i) = '1' then
               --res(i) := dataInBr(sel).ins;
                    slot := getNewElem(remv, dataInBr);
                    res(i) := slot.ins;
           end if;
        end loop;

        -- Update target after branch execution
        for i in 0 to QUEUE_SIZE-1 loop
           if taggedMask(i) = '1' -- !! Prevent instruction with r.i. = 0 form updating untagged entries! 
               and content(i).tags.renameIndex = storeValueInput.ins.tags.renameIndex
               and storeValueInput.full = '1'
           then
               res(i).target := storeValueInput.ins.target;
               res(i).controlInfo.confirmedBranch := storeValueInput.ins.controlInfo.confirmedBranch;
                   res(i).controlInfo.newEvent := storeValueInput.ins.controlInfo.newEvent;
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
				                storeValueInput);
				                
	dataOutSigNext <= getWindow(content, taggedMask, pStartNext, PIPE_WIDTH);
	selectedDataSlot <= selectDataSlot(content, taggedMask, compareAddressInput);
	
    pStartNext <= addSN(pStart, i2slv(getNumberToSend(dataOutSig, groupCtrInc, committing), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
	
	process (clk)
	begin
		if rising_edge(clk) then	
--			TMP_mask <= TMP_maskNext;
            fullMask <= fullMaskNext;
            taggedMask <= taggedMaskNext;
			content <= contentNext;
            pStart <= pStartNext;
			
			selectedDataOutputSig <= selectedDataSlot;
            dataOutSig <= dataOutSigNext;
            
            if lateEventSignal = '1' then
                pTagged <= pStartNext;
            elsif execEventSignal = '1' then
                pTagged <= addSN(causingPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            elsif prevSending = '1' then -- + N
                pTagged <= addSN(pTagged, i2slv(countOnes(inputMask), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;

            if lateEventSignal = '1' then
                pAll <= pStartNext;
            elsif execEventSignal = '1' then
                pAll <= addSN(causingPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;     -- increment(causingPtr, N_BITS_PTR);        
            elsif prevSendingBr = '1' then -- + N
                pAll <= addSN(pAll, i2slv(countOnes(inputMaskBr), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;
               
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif recoveryCounter /= i2slv(0, SMALL_NUMBER_SIZE) then
                recoveryCounter <= subSN(recoveryCounter, i2slv(1, SMALL_NUMBER_SIZE));
            end if;
               
            isFull <= cmpGreaterUnsignedSN(nFullNext, i2slv(QUEUE_SIZE-4, SMALL_NUMBER_SIZE)); -- queueCmpGt(nFullNext, QUEUE_SIZE-4, N_BITS_FULL) = '1' then
            isAlmostFull <= cmpGreaterUnsignedSN(nFullNext, i2slv(QUEUE_SIZE-8, SMALL_NUMBER_SIZE));
                
            nFull <= nFullNext;    
                        
		end if;
	end process;

    nFullNext <=  nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
            else  subSN(addSN(nFull, nIn), nOut);

    nIn <= i2slv( countOnes(extractFullMask(dataInBr)), SMALL_NUMBER_SIZE ) when prevSendingBr = '1' else (others => '0');
        
    QUEUE_MANAGEMENT: block
        constant TAG_DIFF_SIZE_MASK: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
        signal tagDiff: SmallNumber := (others => '0');
    begin
       nOut <= i2slv(countOnes(extractFullMask(dataOutSigFinal)), SMALL_NUMBER_SIZE) when isSending = '1' else (others => '0');              
       nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStartNext = pAll and fullMask(0) = '1'
                       else tagDiff and TAG_DIFF_SIZE_MASK;
       tagDiff <= subSN(pAll, pStartNext);       
    end block;
    
    dataOutSigFinal <= getSendingArray(dataOutSig, groupCtrInc, committing);

    isSending <= dataOutSigFinal(0).full;
	acceptingOut <= '1';

    -- Accept when 4 free slot exist
    pAcc <= subSN(pStart, i2slv(4, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
	acceptingBr <= not isAlmostFull;     
 
	dataOutV <= dataOutSigFinal;                   
	sendingSQOut <= isSending;

	selectedDataOutput <= selectedDataSlot;
	almostFull <= '0';
	
	
    VIEW: block
       signal queueTxt: InstructionTextArray(0 to QUEUE_SIZE-1);
    begin
       queueTxt <= insStateArrayText(content, fullMask, '0');
    end block;

end Behavioral;
