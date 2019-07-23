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
	signal isSending: std_logic := '0';							

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1)
															:= (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, taggedMask, killMask, livingMask, frontMask, sendingMask, inputMask, inputMaskBr,
			 fullMaskNext, taggedMaskNext: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
	
	signal taggedLivingMask: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	signal selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;

	signal pStart, pTagged, pAll, causingPtr, pAcc: SmallNumber := (others => '0');
	
	signal dataOutSig: InstructionSlotArray(0 to PIPE_WIDTH-1) := (others => DEFAULT_INSTRUCTION_SLOT);
	
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);

	   signal nFull, nFullNext, nFullRestored, nIn, nOut: SmallNumber := (others => '0');
	   signal recoveryCounter: SmallNumber := (others => '0');
	   signal isFull, isAlmostFull: std_logic := '0'; 	
	
	function getCausingPtr(content: InstructionStateArray; causing: InstructionState) return SmallNumber is
	   variable res: SmallNumber := (others => '0');
	begin
	   for i in content'range loop
	       if content(i).tags.renameIndex = causing.tags.renameIndex then
	           res := i2slv(i, SMALL_NUMBER_SIZE);
	           exit;
	       end if;
	   end loop;
	   
	   return res;
	end function;
	
	function getInputMask(mask, newMask: std_logic_vector; prevSending: std_logic; ptr: SmallNumber)
	return std_logic_vector is
	   constant LEN: natural := mask'length;
	   variable newMaskComp: std_logic_vector(0 to newMask'length-1) := compactMask(newMask);
	   variable res: std_logic_vector(0 to LEN-1) := (others => '0');
	   variable remainingMaskExt: std_logic_vector(0 to LEN + 4 - 1) := (others => '0');
	   variable remv: std_logic_vector(0 to 3) := "0000";
	   variable pLoc: natural := slv2u(ptr);
	   variable diff: SmallNumber := (others => '0');
	begin
	   
       remainingMaskExt(4 to LEN + 3) := mask;
       remainingMaskExt(0 to 3) := (others => '1');
       
       if prevSending = '0' then
           return res; -- Stays empty
       end if;
       
       for i in 0 to LEN-1 loop
           diff := subSN(i2slv(i, SMALL_NUMBER_SIZE), ptr) and PTR_MASK_SN;
           if slv2u(diff) >= 4 then
               remv := (others => '0');
           else
               case diff(1 downto 0) is
                   when "00" =>
                        remv := "1111";
                   when "01" =>
                        remv := "1110";
                   when "10" =>
                        remv := "1100";
                   when others =>
                        remv := "1000";                                                                                       
               end case;
           end if;
           --             report integer'image(slv2u(remv));
           --remv := remainingMaskExt(i+1 to i+3);
           
           res(i) := '0';
           for k in 0 to 3 loop -- Further beyond end requires more ful inputs to be filled:
               --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
               res(i) := res(i) or (remv(3-k) and newMaskComp(k));
           end loop;           	   
       end loop;
       
	   return res;
	end function;
	
	function getSendingMask(content: InstructionStateArray; mask: std_logic_vector;
	                tag: InsTag) return std_logic_vector is
	   variable res: std_logic_vector(0 to content'length-1) := (others => '0');
	begin
	   for i in 0 to QUEUE_SIZE-1 loop
           if getTagHighSN(content(i).tags.renameIndex) = getTagHighSN(tag) then
             res(i) := mask(i);
           end if; 
       end loop;
	   return res;
	end function;

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
           end if;
        end loop;

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
           end if;            
        end loop;

        return res;
    end function;
    
    function getWindow(content: InstructionStateArray; mask: std_logic_vector;
                        p: SmallNumber; n: natural)
    return InstructionSlotArray is
        variable res: InstructionSlotArray(0 to n-1) := (others => DEFAULT_INSTRUCTION_SLOT);
        variable pContent, pWindow: natural := 0;
    begin
        pContent := slv2u(p);
        pWindow := 0;
        
        for i in 0 to n-1 loop
            pContent := slv2u(p) + i;
            if pContent >= content'length then
                pContent := pContent - content'length;
            end if;
            res(i).ins := content(pContent);
            res(i).full := mask(pContent);
        end loop;
        
        return res;
    end function;
    
    function selectDataSlot(content: InstructionStateArray; taggedMask: std_logic_vector;
                            compareAddressInput: InstructionSlot)
    return InstructionSlot is 
        variable res: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;    
    begin
        for i in 0 to QUEUE_SIZE-1 loop 
            res.ins := content(i);
            if content(i).tags.renameIndex = compareAddressInput.ins.tags.renameIndex
                and compareAddressInput.full = '1' and taggedMask(i) = '1'
            then
                res.full := '1';
                exit;
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
				
    inputMask <= getInputMask(taggedMask, extractFullMask(dataIn), prevSending, pTagged);				
    inputMaskBr <= getInputMask(fullMask, extractFullMask(dataInBr), prevSendingBr, pAll);
				
	fullMaskNext <= (livingMask and not sendingMask) or inputMaskBr;
	taggedMaskNext <= (taggedLivingMask and not sendingMask) or inputMask;
	
	contentNext <=
				getNewContentBr(content, dataIn, dataInBr,
				                taggedMask, fullMask,
				                prevSending, prevSendingBr,
				                inputMask, inputMaskBr,				             
				                pTagged, pAll,
				                storeValueInput);
				                
	dataOutSig <= getWindow(content, frontMask, pStart, PIPE_WIDTH);
	selectedDataSlot <= selectDataSlot(content, taggedMask, compareAddressInput);
	
	process (clk)
	begin
		if rising_edge(clk) then	
--			TMP_mask <= TMP_maskNext;
            fullMask <= fullMaskNext;
            taggedMask <= taggedMaskNext;
			content <= contentNext;
			
			selectedDataOutputSig <= selectedDataSlot;--(selectedSendingSig, selectedData);

            if isSending = '1' then
                pStart <= addSN(pStart,
                             i2slv(countOnes(extractFullMask(dataOutSig)), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;
            
            if lateEventSignal = '1' then
                pTagged <= pStart;
            elsif execEventSignal = '1' then
                pTagged <= addSN(causingPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            elsif prevSending = '1' then -- + N
                pTagged <= addSN(pTagged, i2slv(countOnes(inputMask), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;

            if lateEventSignal = '1' then
                pAll <= pStart;
            elsif execEventSignal = '1' then
                pAll <= addSN(causingPtr, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;             
            elsif prevSendingBr = '1' then -- + N
                pAll <= addSN(pAll, i2slv(countOnes(inputMaskBr), SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
            end if;
            



            
            if lateEventSignal = '1' or execEventSignal = '1' then
                recoveryCounter <= i2slv(1, SMALL_NUMBER_SIZE);
            elsif recoveryCounter /= i2slv(0, SMALL_NUMBER_SIZE) then
                recoveryCounter <= subSN(recoveryCounter, i2slv(1, SMALL_NUMBER_SIZE));
            end if;
                            
            if --nFullNext > QUEUE_SIZE-4 then
                cmpGreaterUnsignedSN(nFullNext, i2slv(QUEUE_SIZE-4, SMALL_NUMBER_SIZE)) = '1' then
                isFull <= '1';
                isAlmostFull <= '1';
            elsif --nFullNext > QUEUE_SIZE-8 then
                cmpGreaterUnsignedSN(nFullNext, i2slv(QUEUE_SIZE-8, SMALL_NUMBER_SIZE)) = '1' then
                isFull <= '0';
                isAlmostFull <= '1';
            else
                isFull <= '0';
                isAlmostFull <= '0';
            end if;     
                
                
                        
		end if;
	end process;



            nFullNext <=  nFullRestored when recoveryCounter = i2slv(1, SMALL_NUMBER_SIZE)
                    else  --nFull + nIn - nOut;
                          subSN(addSN(nFull, nIn), nOut);

	    nIn <= i2slv( countOnes(extractFullMask(dataInBr)), SMALL_NUMBER_SIZE ) when prevSendingBr = '1' else (others => '0');
	        
        QUEUE_MANAGEMENT: block
            constant QUEUE_SIZE_MASK: SmallNumber := i2slv(2*QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
            signal tagDiff: SmallNumber := (others => '0');
        begin
           nOut <= i2slv(countOnes(extractFullMask(dataOutSig)), SMALL_NUMBER_SIZE) when isSending = '1'
          else (others => '0');        
        
           nFullRestored <= i2slv(QUEUE_SIZE, SMALL_NUMBER_SIZE) when pStart = pAll and fullMask(0) = '1'
                           else tagDiff and QUEUE_SIZE_MASK;
                           tagDiff <= subSN(pAll, pStart); -- TODO: modulo to make it positive           
        end block;



	isSending <= committing and dataOutSig(0).full;
	dataOutV <= dataOutSig;

	acceptingOut <= '1';

    -- Accept when 4 free slot exist
    pAcc <= subSN(pStart, i2slv(4, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
	acceptingBr <= --not fullMask(slv2u(pAcc));
                   --not isFull;
                   not isAlmostFull;     
                    
	sendingSQOut <= isSending;

	selectedDataOutput <= selectedDataSlot;
	almostFull <= '0';
end Behavioral;
