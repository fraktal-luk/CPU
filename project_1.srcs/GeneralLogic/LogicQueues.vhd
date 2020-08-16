
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

    function getMatchingTags(content: InstructionStateArray; tag: InsTag) return std_logic_vector; -- UNUSED  
    function getMatchingIndex(content: InstructionStateArray; tag: InsTag) return natural;  -- UNUSED
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

    
    function getMatchingTags(content: InstructionStateArray; tag: InsTag) return std_logic_vector is
        variable res: std_logic_vector(content'range) := (others => '0');
    begin
        for i in content'range loop
            res(i) := bool2std(content(i).tags.renameIndex = tag);
        end loop;
        return res;
    end function;
    
    -- NOTE, TODO: can start loop from 1 and return 0 by default because every branch writing to the mem must already have a slot allocated!
    function getMatchingIndex(content: InstructionStateArray; tag: InsTag) return natural is
    begin
        for i in content'range loop
            if content(i).tags.renameIndex = tag then
                return i;
            end if;
        end loop;
        return 0;
    end function;

end package body;