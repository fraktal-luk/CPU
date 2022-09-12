
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicRenaming.all;



entity QueueAllocator is

    port(
        clk: in std_logic;

        inReady: in std_logic;
        inGroup: in BufferEntryArray;
        
        outReady: out std_logic;
        outGroup: out BufferEntryArray;
        
        evt: EventState
    );

end QueueAllocator;


architecture Behavioral of QueueAllocator is

begin


end Behavioral;
