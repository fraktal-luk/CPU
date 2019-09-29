----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 30.08.2019 21:44:04
-- Design Name: 
-- Module Name: LogicQueues - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
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

use work.Arith.all;

use work.CoreConfig.all;
use work.InstructionState.all;
use work.PipelineGeneral.all;


package LogicQueues is


	function getCausingPtr(content: InstructionStateArray; causing: InstructionState) return SmallNumber;
	
	function getInputMask(mask, newMask: std_logic_vector; prevSending: std_logic; ptr: SmallNumber; ptrMask: SmallNumber)
	return std_logic_vector;

	function getSendingMask(content: InstructionStateArray; mask: std_logic_vector;
	                tag: InsTag) return std_logic_vector;

	function getCancelMask(content: InstructionStateArray; mask: std_logic_vector;
	                tag: InsTag; robData: InstructionSlotArray) return std_logic_vector;
    
    function getWindow(content: InstructionStateArray; mask: std_logic_vector;
                        p: SmallNumber; n: natural)
    return InstructionSlotArray;
    
    function selectBranchDataSlot(content: InstructionStateArray; taggedMask: std_logic_vector;
                            compareAddressInput: InstructionSlot)
    return InstructionSlot;
    
    function getNumberToSend(dataOutSig: InstructionSlotArray(0 to PIPE_WIDTH-1); nextCommitTag: InsTag; committing: std_logic) return integer;

    function getSendingArray(dataOutSig: InstructionSlotArray(0 to PIPE_WIDTH-1); nextCommitTag: InsTag; committing: std_logic) return InstructionSlotArray;

	
end package;


package body LogicQueues is


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
	
	function getInputMask(mask, newMask: std_logic_vector; prevSending: std_logic; ptr: SmallNumber; ptrMask: SmallNumber)
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
           diff := subSN(i2slv(i, SMALL_NUMBER_SIZE), ptr) and ptrMask;
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
           
           res(i) := '0';
           for k in 0 to 3 loop -- Further beyond end requires more full inputs to be filled:
               --                            !! equiv to remainingMask(-1-k), where '1' for k < 0
               res(i) := res(i) or (remv(3-k) and newMaskComp(k));
           end loop;           	   
       end loop;
       
	   return res;
	end function;

	function getSendingMask(content: InstructionStateArray; mask: std_logic_vector;
	                tag: InsTag) return std_logic_vector is
	   constant LEN: integer := content'length;
	   variable res: std_logic_vector(0 to content'length-1) := (others => '0');
	begin
	   for i in 0 to LEN-1 loop
           if getTagHighSN(content(i).tags.renameIndex) = getTagHighSN(tag) then
             res(i) := mask(i);
           end if; 
       end loop;
	   return res;
	end function;


	function getCancelMask(content: InstructionStateArray; mask: std_logic_vector;
	                tag: InsTag; robData: InstructionSlotArray) return std_logic_vector is
	   constant LEN: integer := content'length;
	   variable res: std_logic_vector(0 to content'length-1) := (others => '0');
	begin
	   for i in 0 to LEN-1 loop   
           if    getTagLow(content(i).tags.renameIndex) = "10" and
                 (not robData(3).full or robData(3).ins.controlInfo.hasException or robData(3).ins.controlInfo.specialAction or robData(3).ins.controlInfo.dbtrap) = '1' then
               res(i) := '1';           
           elsif getTagLow(content(i).tags.renameIndex) = "10" and
                 (not robData(2).full or robData(2).ins.controlInfo.hasException or robData(2).ins.controlInfo.specialAction or robData(2).ins.controlInfo.dbtrap) = '1' then
               res(i) := '1';           
           elsif getTagLow(content(i).tags.renameIndex) = "01" and
                 (not robData(1).full or robData(1).ins.controlInfo.hasException or robData(1).ins.controlInfo.specialAction or robData(1).ins.controlInfo.dbtrap) = '1' then
               res(i) := '1';
           elsif 
                 (not robData(0).full or robData(0).ins.controlInfo.hasException or robData(0).ins.controlInfo.specialAction or robData(0).ins.controlInfo.dbtrap) = '1' then
               res(i) := '1';
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
    
    function selectBranchDataSlot(content: InstructionStateArray; taggedMask: std_logic_vector;
                            compareAddressInput: InstructionSlot)
    return InstructionSlot is
        constant LEN: integer := content'length;
        variable res: InstructionSlot := DEFAULT_INSTRUCTION_SLOT;    
    begin
        for i in 0 to LEN-1 loop 
            res.ins := content(i);
            if content(i).tags.renameIndex = compareAddressInput.ins.tags.renameIndex and taggedMask(i) = '1'
            then
                res.full := compareAddressInput.full;
                exit;
            end if;
        end loop;
        return res;
    end function;
    
    function getNumberToSend(dataOutSig: InstructionSlotArray(0 to PIPE_WIDTH-1); nextCommitTag: InsTag; committing: std_logic) return integer is
       variable res: integer := 0;
    begin
       if getTagHighSN(dataOutSig(0).ins.tags.renameIndex) /= getTagHighSN(nextCommitTag) or committing = '0' then
           return 0;
       end if;        
       
       -- So there's a matching tag. Count full slots up to a 'redirect' mark or stop when new 'start' mark is met
       -- (the first 'start' mark on elem 0 is there always and we ignore it!
       for i in 0 to PIPE_WIDTH-1 loop
           if dataOutSig(i).full = '0' then
               exit;
           end if;
           
           if dataOutSig(i).ins.controlInfo.firstBr = '1' and i /= 0 then
               exit;
           end if;           
           
           res := res + 1;
           
           if dataOutSig(i).ins.controlInfo.newEvent = '1' then
               exit;
           end if;  
       end loop;
       
       return res;
    end function;

    function getSendingArray(dataOutSig: InstructionSlotArray(0 to PIPE_WIDTH-1); nextCommitTag: InsTag; committing: std_logic) return InstructionSlotArray is
       variable res: InstructionSlotArray(0 to PIPE_WIDTH-1) := dataOutSig;
    begin
       for i in 0 to PIPE_WIDTH-1 loop
           res(i).full := '0';
       end loop;
    
       if getTagHighSN(dataOutSig(0).ins.tags.renameIndex) /= getTagHighSN(nextCommitTag) or committing = '0' then
           return res;
       end if;        
       
       -- So there's a matching tag. Count full slots up to a 'redirect' mark or stop when new 'start' mark is met
       -- (the first 'start' mark on elem 0 is there always and we ignore it!
       for i in 0 to PIPE_WIDTH-1 loop
           if dataOutSig(i).full = '0' then
               exit;
           end if;
           
           if dataOutSig(i).ins.controlInfo.firstBr = '1' and i /= 0 then
               exit;
           end if;           
           
           res(i).full := '1';
           
           if dataOutSig(i).ins.controlInfo.newEvent = '1' then
               exit;
           end if;  
       end loop;
       
       return res;
    end function;



end package body;