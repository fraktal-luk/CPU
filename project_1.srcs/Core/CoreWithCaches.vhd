
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.ArchDefs.all;	

use work.CoreConfig.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

 
entity CoreWithCaches is
    port (
        clk: in std_logic;
        reset: in std_logic;
        en: in std_logic;
        
        ints: in std_logic_vector(0 to 7);
        intAcks: out std_logic_vector(0 to 7);
        
        dataInWrite: in std_logic;
        dataInAdr: in std_logic_vector(9 downto 0);
        dataInValue: in Mword;
        
        dataOutRead: in std_logic;
        dataOutAdr: in std_logic_vector(9 downto 0);
        dataOutValue: out Mword;
        
        dataFw: out std_logic;
        dataFwAdr: out std_logic_vector(9 downto 0);
        dataFwValue: out Mword;

        insInWrite: in std_logic;
        insInAdr: in std_logic_vector(9 downto 0);
        insInValue: in Mword;
        
        eventsOut: out std_logic_vector(0 to 7)
    );
end CoreWithCaches;


architecture Behavioral of CoreWithCaches is
    signal loadTestProgram : std_logic := '0';


        signal insInWriteD: std_logic := '0';
        signal insInAdrD: std_logic_vector(9 downto 0) := (others => '0');
        signal insInValueD: Mword := (others => '0');

--        signal dataInWriteD: std_logic := '0';
--        signal dataInAdrD: std_logic_vector(9 downto 0) := (others => '0');
--        signal dataInValueD: Mword := (others => '0');
        
        signal dataOutReadD: std_logic := '0';
        signal dataOutAdrD: std_logic_vector(9 downto 0) := (others => '0');
        signal dataOutValueP: Mword := (others => '0');

        signal intAckD, int0d, int1d: std_logic := '0';

    signal resetDataMem: std_logic := '0';
    
    signal dataWrite, dataWriteD: std_logic := '0';
    signal dataAdr, dataAdrD: std_logic_vector(9 downto 0) := (others => '0');
    signal dataValue: Mword := (others => '0');
    signal dataValueD: Mword := (others => '0');
    
    -- Inputs
    signal ivalid : std_logic := '0';
    signal iin : WordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal intallow, intack: std_logic := '0';
    signal int0 : std_logic := '0';
    signal int1 : std_logic := '0';
    signal iaux : std_logic_vector(31 downto 0) := (others => '0');

    signal dread: std_logic;
    signal dwrite: std_logic;
    signal dadr : Mword;
    signal doutadr: Mword;
    signal dvalid: std_logic;
    signal din :  Mword;
    signal dout : Mword;

    signal filladr: Mword := (others => '0');
    signal fillready: std_logic := '0';

     --Outputs
    signal iadrvalid : std_logic;
    signal iadr : std_logic_vector(31 downto 0);
    signal oaux : std_logic_vector(31 downto 0);
begin

        SIGNAL_DELAY: process (clk)
        begin
            if rising_edge(clk) then
                int0 <= ints(0);
                int1 <= ints(1);
        
                intAcks(0) <= intack;   
            end if;
        end process;
       
        CPU_CORE: entity work.Core port map (
            clk => clk,
            reset => reset,
            en => '1',
            iadrvalid => iadrvalid,
            iadr => iadr,
            ivalid => ivalid,
            iin => iin,
               
            dread => dread,
            dwrite => dwrite,
            dadr => dadr,
            doutadr => doutadr,
            dvalid => dvalid,
            din => din,
            dout => dout,             
               
            intallow => intallow,
            intack => intack,
            int0 => int0,
            int1 => int1,
               
            filladr => filladr,
            fillready => fillready,
               
            iaux => iaux,
            oaux => oaux
        );


        PROGRAM_MEM: block
            use work.Assembler.all;

            procedure loadProgramFromFileWithImports(filename: in string; libExports: XrefArray; libStart: Mword; signal testProgram: out WordArray) is        
                constant prog: ProgramBuffer := readSourceFile(filename);
                variable machineCode: WordArray(0 to prog'length-1);
                variable imp, exp: XrefArray(0 to 100);
            begin
                processProgram(prog, machineCode, imp, exp);
                machineCode := fillXrefs(machineCode, imp, matchXrefs(imp, libExports), 0, slv2u(libStart));
        
                testProgram <= (others => (others => 'U'));
                testProgram(0 to machineCode'length-1) <= machineCode(0 to machineCode'length-1);        
            end procedure;

            procedure setTestProgram(signal output: out WordArray) is
                constant prog: ProgramBuffer := readSourceFile("primes.txt");
                variable machineCode: WordArray(0 to prog'length-1);
                variable imp, exp: XrefArray(0 to 100);
                --variable res: WordArray(0 to 1023) := (others => (others => '0'));
                constant offset: natural := 512/4;
                --constant bound: natural := 
            begin
                processProgram(prog, machineCode, imp, exp);
                machineCode := fillXrefs(machineCode, imp, matchXrefs(imp, exp), 0, 0);
                
                output(offset to output'length-1) <= machineCode(0 to output'length-1 - offset);
                --return res;
            end procedure;
            
            
            signal programMem: WordArray(0 to 1023) := (others => (others => '0'));
            
            constant TEST_MODE: boolean := true;
        begin
            MEM_INIT: process
            begin
                wait for 5 ns;
                loadTestProgram <= '1';
                wait for 10 ns;
                loadTestProgram <= '0';
                wait for 10 ns;

                wait;
            end process;
        
            SYNCH: process (clk)
                variable baseIP: Mword := (others => '0');
            begin
                
                if rising_edge(clk) then					
                    -- CAREFUL! don't fetch if adr not valid, cause it may ovewrite previous, valid fetch block.
                    --				If fetch block is valid but cannot be sent further (pipe stall etc.),
                    --				it must remain in fetch buffer until it can be sent.
                    --				So we can't get new instruction bits when Fetch stalls, cause they'd destroy
                    --				stalled content in fetch buffer!
                    baseIP := iadr and i2slv(-PIPE_WIDTH*4, MWORD_SIZE); -- Clearing low bits
                    for i in 0 to PIPE_WIDTH-1 loop
                        iin(i) <= programMem(slv2u(baseIP(12 downto 2)) + i); -- CAREFUL! 2 low bits unused (32b memory) 									
                    end loop;
                    
                    ivalid <= iadrvalid and not isNonzero(iadr(iadr'high downto 12));
                    
                            insInWriteD <= insInWrite;
                            insInAdrD <= insInAdr;
                            insInValueD <= insInValue;
                    if loadTestProgram = '1' then
                        setTestProgram(programMem);
                    elsif insInWriteD = '1' then
                        programMem(slv2u(insInAdrD)) <= insInValueD;
                    end if;
                end if;	
            end process;
        end block;
        
        dataWrite <= dataInWrite or dwrite;
        dataAdr <= dataInAdr when dataInWrite = '1' else doutadr(11 downto 2);
        dataValue <= dataInValue when dataInWrite = '1' else dout;
        
        DATA_MEM: block
            signal memReadDone, memReadDonePrev, memWriteDone: std_logic := '0';
            signal memReadValue, memReadValuePrev, memWriteAddress, memWriteValue: Mword := (others => '0');
            signal dataMem: WordArray(0 to 1023) := (others => (others => '0'));
        begin
            SYNCH: process (clk)
            
            begin
                if rising_edge(clk) then
                
                                dataWriteD <= dataWrite;
                                dataAdrD <= dataAdr;
                                dataValueD <= dataValue;
                
--                    if resetDataMem = '1' then
--                        dataMem <= (others => (others => '0'));
--                    else		
                        -- TODO: define effective address exact size
                    
                        -- Reading
                        memReadDone <= dread;
                        memReadDonePrev <= memReadDone;
                        memReadValue <= dataMem(slv2u(dadr(MWORD_SIZE-1 downto 2))) ;-- CAREFUL: pseudo-byte addressing 
                        memReadValuePrev <= memReadValue;	
                        
                        -- Writing
                        memWriteDone <= dwrite;
                        memWriteValue <= dout;
                        memWriteAddress <= doutadr;

--                        -- CAREFUL: this gives priority to write from outside and write by CPU is ignored if concurrent.
--                        --          TODO: enable arbitration
--                        if dataInWrite = '1' then
--                            dataMem(slv2u(dataInAdr)) <= dataInValue;
--                        elsif dwrite = '1' then
--                            dataMem(slv2u(doutadr(MWORD_SIZE-1 downto 2))) <= dout; -- CAREFUL: pseudo-byte addressing  
--                        end if;
                        
                        if dataWriteD = '1' then
                            dataMem(slv2u(dataAdrD)) <= dataValueD;
                        end if;
                        
                            dataOutReadD <= dataOutRead;
                            dataOutAdrD <= dataOutAdr;
                            dataOutValue <= dataOutValueP;
                            
                        if dataOutReadD = '1' then
                            dataOutValueP <= dataMem(slv2u(dataOutAdrD));
                        end if;
                    --end if;
                end if;	
            end process;
        
            din <= memReadValue;
            dvalid <= memReadDone;
        end block;
        
        
        
end Behavioral;
