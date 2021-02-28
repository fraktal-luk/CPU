

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;

use work.CoreConfig.all;

use work.PipelineGeneral.all;

--use work.LogicFront.all;


package LogicBQ is

constant PTR_MASK_SN: SmallNumber := i2slv(BQ_SIZE-1, SMALL_NUMBER_SIZE);
constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;


end package;


package body LogicBQ is

end package body; 


