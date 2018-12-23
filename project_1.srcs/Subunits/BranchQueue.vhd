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

		--storeAddressInput: in InstructionSlot;
		--storeValueInput: in InstructionSlot;
		--compareAddressInput: in InstructionSlot;

		selectedDataOutput: out InstructionSlot;

		committing: in std_logic;
		groupCtrInc: in InsTag;

		lateEventSignal: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;
		
		nextAccepting: in std_logic;		
		sendingSQOut: out std_logic;
		dataOutV: out InstructionSlotArray(0 to PIPE_WIDTH-1);
		
			committedOutput: out InstructionSlot;
			committedEmpty: out std_logic
	);
end BranchQueue;


architecture Behavioral of BranchQueue is
	signal isSending: std_logic := '0';							

	signal content, contentNext: InstructionStateArray(0 to QUEUE_SIZE-1)
															:= (others => DEFAULT_INSTRUCTION_STATE);
	signal fullMask, taggedMask, killMask, livingMask, sendingMask, inputMask, inputMaskBr,
			 fullMaskNext, taggedMaskNext: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0'); 


	signal contentView, contentNextView:
					InstructionStateArray(0 to QUEUE_SIZE-1) := (others => DEFAULT_INSTRUCTION_STATE);
	signal maskView, liveMaskView, maskNextView: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
	
	signal cmpMask, taggedLivingMask, matchedSlot: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

	signal selectedDataSlot: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;
	signal selectedDataOutputSig: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;

	signal compareAddressIns: InstructionState := DEFAULT_INSTRUCTION_STATE;
	
	signal numPrevBr: SmallNumber := (others => '0');
	signal pStart, pTagged, pAll, causingPtr: SmallNumber := (others => '0');
	
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
	
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
                        report integer'image(slv2u(remv));
           --remv := remainingMaskExt(i+1 to i+3);
           
           res(i) := '0';
           for k in 0 to 3 loop -- Further beyond end requires more ful inputs to be filled:
               --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
               res(i) := res(i) or (remv(3-k) and newMaskComp(k));
           end loop;           	   
       end loop;
       
	   return res;
	end function;
	
begin
	--	compareAddressIns <= storeAddressInput.ins;


    causingPtr <= getCausingPtr(content, execCausing);
    
	-- in shifting queue this would be shfited by nSend
--	sendingMask <= getQueueSendingMask(qs0, QUEUE_SIZE, bufferDrive.nextAccepting);
	killMask <= getKillMask(content, fullMask, execCausing, execEventSignal, lateEventSignal);
	livingMask <= fullMask and not killMask;			
    taggedLivingMask <= taggedMask and not killMask;
				
    inputMask <= getInputMask(taggedMask, extractFullMask(dataIn), prevSending, pTagged);				
    inputMaskBr <= getInputMask(fullMask, extractFullMask(dataInBr), prevSendingBr, pAll);
				
	fullMaskNext <= (livingMask and not sendingMask) or inputMaskBr;
	taggedMaskNext <= (taggedLivingMask and not sendingMask) or inputMask;
	
--	contentNext <=
--				getNewContentBr(content, dataIn, dataInBr,
--				                prevSending, prevSendingBr,
--				                fullMask, taggedMask,
--											storeAddressInput.full, storeValueInput.full,
--											storeAddressInput.ins, storeValueInput.ins);

	
--	sqOutData <= TMP_sendingData;

--		cmpMask <=	compareAddress(TMP_content, fullOrCommMask, compareAddressIns) when MODE = store
--				else	compareAddress(TMP_content, TMP_mask, compareAddressIns);
--		-- TEMP selection of hit checking mechanism 
--		matchedSlot <= findMatching(makeSlotArray(content, mask), compareAddressInput.ins);

--	selectedDataSlot <= (isNonzero(matchedSlot) and compareAddressInput.full, chooseIns(TMP_content, matchedSlot));
	
	process (clk)
	begin
		if rising_edge(clk) then	
--			TMP_mask <= TMP_maskNext;
            fullMask <= fullMaskNext;
            taggedMask <= taggedMaskNext;
			content <= contentNext;
			
			selectedDataOutputSig <= selectedDataSlot;--(selectedSendingSig, selectedData);

            if isSending = '1' then  -- TODO: + nSending
                pStart <= addSN(pStart, i2slv(1, SMALL_NUMBER_SIZE)) and PTR_MASK_SN;
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
		end if;
	end process;


	isSending <= '0';
	--dataOutV <= sqOutData;
	--	dataOutV.data <= TMP_content(0 to 3);
	--	dataOutV.fullMask <= TMP_mask(0 to 3);
	
	acceptingOut <= '1'; -- TMP

	acceptingBr <=  '1'; -- TMP

	
	sendingSQOut <= isSending;

	selectedDataOutput <= selectedDataSlot;
	almostFull <= '0';
end Behavioral;
