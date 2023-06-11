----------------------------------------------------------------------------------

----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;

use work.CoreConfig.all;

use work.PipelineGeneral.all;

use work.LogicFront.all;

use work.LogicIbuffer.all;

entity InstructionBuffer is
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;
		
		prevSending: in std_logic;
		stageDataIn: in BufferEntryArray;
		nextAccepting: in std_logic;
		execEventSignal: in std_logic;

		acceptingOut: out std_logic;
		sendingOut: out std_logic;
		stageDataOut: out BufferEntryArray
	);
end InstructionBuffer;



architecture Implem of InstructionBuffer is
    constant QUEUE_SIZE: natural := IBUFFER_SIZE;
	constant PTR_MASK_SN: SmallNumber := i2slv(QUEUE_SIZE-1, SMALL_NUMBER_SIZE);
    constant QUEUE_PTR_SIZE: natural := countOnes(PTR_MASK_SN);
    constant QUEUE_CAP_SIZE: natural := QUEUE_PTR_SIZE + 1;

    signal content: BufferEntryArray2D := (others => (others => DEFAULT_BUFFER_ENTRY));
    signal dataOut: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
    
    signal serialMemInput, serialMemOutput: std_logic_vector(MEM_WIDTH-1 downto 0) := (others => '0');
    signal serialMem: SerialMemory := (others=> (others => '0'));
    
    signal pStart, pStartNext, pEnd, pEndNext, nFull, nFullNext: SmallNumber := (others => '0'); -- nFullGroups - DB ony
    signal dataOutFull, dataOutStalled, isSending, memWriting, memReading, memBypassing, canAccept: std_logic := '0';
    signal memEmpty: std_logic := '1';

       signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9, cha: std_logic := '0';
begin

    -- This means writing and keeping it there for later, not writing and using at the same time.
    -- Actual storage of value can happen also when bypassing - but is not used later.
    -- Writing to emty mem: when dataOut stalled
    -- Writing to non empty mem: when prevSending and mem already full
    memWriting <= prevSending and (dataOutStalled or not memEmpty);          
    memBypassing <= prevSending and not memWriting and not execEventSignal;
    memReading <= (isSending or not dataOutFull) and not memEmpty and not execEventSignal;    

    dataOutStalled <= dataOutFull and not isSending;
    isSending <= dataOutFull and nextAccepting;

    serialMemInput <= serializeEntryArray(stageDataIn);

    CLOCKED: process (clk)
    begin
    
        if rising_edge(clk) then
            dataOutFull <= (memReading or memBypassing or dataOutStalled) and not execEventSignal;

            if prevSending = '1' then
                 serialMem(p2i(pEnd, IBUFFER_SIZE)) <= serialMemInput;
                 
                 for i in 0 to stageDataIn'length-1 loop
                    content(p2i(pEnd, IBUFFER_SIZE), i) <= stageDataIn(i);
                 end loop;                    
            end if;
            
            if memBypassing = '1' then
                dataOut <= stageDataIn;
            elsif memReading = '1' then                        
                dataOut <= deserializeEntryArray(serialMem(p2i(pStart, IBUFFER_SIZE)));
                
                for i in 0 to stageDataIn'length-1 loop
                    dataOut(i).dbInfo <= content(p2i(pStart, IBUFFER_SIZE), i).dbInfo;
                end loop;
            end if;

            pStart <= pStartNext;
            pEnd <= pEndNext;
            
            nFull <= nFullNext;
            canAccept <= not cmpGtU(nFullNext, QUEUE_SIZE-1);
            
            memEmpty <= getQueueEmpty(pStartNext, pEndNext, QUEUE_PTR_SIZE);
        end if;
    end process;

    pStartNext <= addIntTrunc(pStart, 1, QUEUE_PTR_SIZE+1) when (memReading or memBypassing) = '1'
            else  pStart;

    pEndNext <= pStart when execEventSignal = '1'
        else    addIntTrunc(pEnd, 1, QUEUE_PTR_SIZE+1) when prevSending = '1'
        else    pEnd;

    nFullNext <= getNumFull(pStartNext, pEndNext, QUEUE_PTR_SIZE);

    -- Outputs
    acceptingOut <= canAccept;

    stageDataOut <= dataOut;
    sendingOut <= isSending;

end Implem;
