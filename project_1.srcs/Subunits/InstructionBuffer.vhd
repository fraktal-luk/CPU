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
		nextAccepting: in std_logic;
		execEventSignal: in std_logic;
		execCausing: in InstructionState;

		stageDataIn: in InstructionSlotArray(0 to FETCH_WIDTH-1);
		acceptingOut: out std_logic;
		sendingOut: out std_logic;
		stageDataOut: out InstructionSlotArray(0 to PIPE_WIDTH-1)
	);
end InstructionBuffer;



architecture Implem of InstructionBuffer is
   signal input: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
   signal content: BufferEntryArray2D := (others => (others => DEFAULT_BUFFER_ENTRY));
   signal output, output_D: BufferEntryArray := (others => DEFAULT_BUFFER_ENTRY);
   
   signal serialMemInput, serialMemOutput: std_logic_vector(MEM_WIDTH-1 downto 0) := (others => '0');
   signal serialMem: SerialMemory := (others=> (others => '0'));
        
   signal full: std_logic_vector(0 to 3) := (others => '0');
   signal pStart, pEnd, pStartNext, pEndNext, pStartLong, pStartLongNext, pEndLong, pEndLongNext, nFullGroups: SmallNumber := (others => '0');
   signal dataOutFull, dataOutFilling, dataOutStalled, isSending, isReading, memWriting, memReading, memBypassing, memDraining, isAccepting: std_logic := '0';
   signal memEmpty: std_logic := '1';
    
   signal stageDataInFormatted, dataOut: PipeStage := (others => DEFAULT_INS_SLOT);
    
       signal ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9, cha: std_logic := '0';
begin
    dataOutStalled <= dataOutFull and not isSending;
    
    -- This means writing and keeping it there for later, not writing and using at the same time.
    -- Actual storage of value can happen also when bypassing - but is not used later.
    memWriting <= prevSending and (dataOutStalled or not memEmpty); -- Writing to emty mem: when dataOut stalled
                                                                    -- Writing to non empty mem: when prevSending and mem already full            
    memBypassing <= prevSending and not memWriting and not execEventSignal;

    memReading <= (isSending or not dataOutFull) and not memEmpty and not execEventSignal;
    
    memDraining <= memReading and not memWriting and bool2std(addIntTrunc(pStart, 1, 2) = pEnd);
    
    isAccepting <= bool2std(pStart /= pEnd) or memEmpty;
    
    isSending <= dataOutFull and nextAccepting  ;-- and not execEventSignal;
    
    nFullGroups <=  getNumFull(pStartLong, pEndLong, QUEUE_PTR_SIZE+1);
       
   stageDataInFormatted <= formatInput(stageDataIn);           
   input <= getEntryArray(stageDataInFormatted);
   serialMemInput <= serializeEntryArray(input);
          
    CLOCKED: process (clk)
    begin
    
        if rising_edge(clk) then
            dataOutFull <= (memReading or memBypassing or dataOutStalled) and not execEventSignal;

            if prevSending = '1' then
                 --updateQueue(content, pEndLong, input);
                 serialMem(slv2u(pEnd)) <= serialMemInput;                     
            end if;
            
            if memBypassing = '1' then
                dataOut <= stageDataInFormatted;
            elsif memReading = '1' then                        
                dataOut <= getInsSlotArray(deserializeEntryArray(serialMem(slv2u(pStart))));
            end if;

            pStartLong <= pStartLongNext;
            pEndLong <= pEndLongNext;
            
            memEmpty <= getQueueEmpty(pStartLongNext, pEndLongNext, QUEUE_PTR_SIZE);
        end if;
    end process;

    pStart <= pStartLong and PTR_MASK_SN;
    pEnd <= pEndLong and PTR_MASK_SN;

    pStartLongNext <= addIntTrunc(pStartLong, 1, QUEUE_PTR_SIZE+1) when (memReading or memBypassing) = '1'
            else  pStartLong;
            
    pEndLongNext <= pStartLong when execEventSignal = '1'
        else    addIntTrunc(pEndLong, 1, QUEUE_PTR_SIZE+1) when prevSending = '1'
        else    pEndLong;

    acceptingOut <= isAccepting;
    stageDataOut <= dataOut;
    sendingOut <= isSending;

	
--	VIEW: if VIEW_ON generate
--       --use work.Viewing.all;
--	   --signal queueTxt: InsStringArray(0 to IBUFFER_SIZE-1);
--	begin
--	   --queueTxt <= getInsStringArray(queueData);
--	end generate;	

end Implem;
