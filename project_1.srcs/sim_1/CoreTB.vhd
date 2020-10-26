----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.11.2018 22:40:42
-- Design Name: 
-- Module Name: CoreTB - Behavioral
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
use std.textio.all;

use work.BasicTypes.all;	
use work.ArchDefs.all;	
use work.CoreConfig.all;

use work.Helpers.all;

use work.Assembler.all;
use work.Emulate.all;

ENTITY CoreTB IS
END CoreTB;
 
ARCHITECTURE Behavior OF CoreTB IS
    
    constant EMULATION: boolean := true;
    constant LOG_EMULATION_TRACE: boolean := true;
    constant CORE_SIMULATION: boolean := true;
    
    -- Component Declaration for the Unit Under Test (UUT)

    COMPONENT Core -- FrontPipe0
    PORT(
        clk : IN  std_logic;
        reset : IN  std_logic;
        en : IN  std_logic;
        iadrvalid : OUT  std_logic;
        iadr : OUT  std_logic_vector(31 downto 0);
        ivalid : IN  std_logic;
        iin : IN  WordArray(0 to PIPE_WIDTH-1);
			
		dread: out std_logic;
		dwrite: out std_logic;
        dadr : out  Mword;
		doutadr: out Mword;
		dvalid: in std_logic;
        din : in  Mword;
        dout : out  Mword;			
			
		intallow: out std_logic;
		intack: out std_logic;
        int0 : IN  std_logic;
        int1 : IN  std_logic;
        filladr: in Mword;
        fillready: in std_logic;
        iaux : IN  std_logic_vector(31 downto 0);
        oaux : OUT  std_logic_vector(31 downto 0)
    );
    END COMPONENT;
    

    --Inputs
    signal clk : std_logic := '0';
    signal reset : std_logic := '0';
    signal en : std_logic := '0';
    
    
    -- CPU ports
    -- Inputs
    signal ivalid : std_logic := '0';
    signal iin : WordArray(0 to PIPE_WIDTH-1) := (others => (others => '0'));
    signal intallow, intack: std_logic := '0';
    signal int0 : std_logic := '0';
    signal int1 : std_logic := '0';
    signal int0a, int0b: std_logic := '0';
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
	-- end CP ports
	
	

    signal resetDataMem: std_logic := '0';
    
    -- Clock period definitions
    constant clk_period : time := 10 ns;

    constant TIME_STEP: time := 1 ns; -- for 1 instruction in emulation

 
	signal prog: ProgramBuffer;
	signal machineCode: WordArray(0 to prog'length-1);
	
    signal testProgram: WordArray(0 to 1023);
    signal testToDo, testDone, testFail: std_logic := '0';
    
    signal currentTest, currentSuite: string(1 to 20);

    
    signal programMemory: WordArray(0 to 1023);
    signal dataMemory: ByteArray(0 to 4095);
    
    alias cpuEndFlag is oaux(0);
    alias cpuErrorFlag is oaux(1);
    
    --signal instructionWord: Mword;
    --signal disasm: string(1 to 51);

    type Instruction is record
        address: Mword;
        bits: Word;
        disasm: string(1 to 51);
    end record;
    
    signal currentInstruction: Instruction;
    
    
    signal cpuState: CoreState := INIT_CORE_STATE;
    
    
        signal internalOp: InternalOperation;
        signal opFlags: std_logic_vector(0 to 2);
        
    signal okFlag, errorFlag: std_logic := '0';
        signal TEST_word, TP, TQ: Word;
        signal TEST_gb: GroupBuffer;
        
        
        signal TMP_str0: string(1 to 10) := ('1', '1', cr, cr, cr, cr, cr, cr, cr, cr); 
        signal TMP_str1: string(1 to 10) := ('1', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '); 
        
        signal num0, num1: integer;
        
        file traceFile: text open write_mode is "emulation_trace.txt";
    
    
    
    function compareTraceLines(sa, sb: string) return boolean is
    begin
        if sa(1 to 8) /= sb(1 to 8) then 
            return false;
        elsif sa(11 to 18) /= sb(11 to 18) then    
            return false;
        end if;
        return true;
    end function;
    
    function skipLine(s: string) return boolean is
    begin
        --        return false;
    
        for i in s'range loop
            if s(i) = '#' and i < s'length then
                return s(i+1) = 'R';
            end if;
        end loop;
        return false;
    end function;
    

    procedure compareTraceFiles(a, b: in string; match: out boolean) is
        file fa: text open read_mode is a;
        file fb: text open read_mode is b;
        variable la, lb: line;
        variable ia, ib: natural := 0;
    begin
        match := true;
           --     report "Comparng" severity note;
    
        loop
           --         report "iter " & natural'image(ia);
        
                    --if ia > 20 or ib > 20 then return; end if;
            la := null;
            lb := null;
            ia := ia + 1;
            ib := ib + 1;
            readline(fa, la);
            readline(fb, lb);
            
            while la /= null and skipLine(la.all) loop la := null; readline(fa, la); ia := ia + 1; end loop;
            while lb /= null and skipLine(lb.all) loop lb := null; readline(fb, lb); ib := ib + 1; end loop;
            
--                    report "What now?";
            
--                        if la = null then
--                            report "la ended " & natural'image(ia);
--                        else
--                            report la.all;     
--                        end if;
--                        if lb = null then
--                            report "lb ended " & natural'image(ib)
--                            ;
--                        else
--                            report lb.all;     
--                        end if;
            
            if la = null and lb = null then
                --return true;
                --            report "Both files end";
                return;
            end if;
            
            if (la = null) /= (lb = null) then
--                    report "EOF!";
                    --        report "One file ends";
                match := false;
                return;
            end if; 
            
            if not compareTraceLines(la.all, lb.all) then
                --    report "Different lines!";
                match := false;
                return;
            end if;
            
            --    report "go on";
        end loop;
        
        --    report natural'image(ia) & " <-> " &natural'image(ib);
        
        --return true;
    end procedure;
        
BEGIN

            assert ins655H(addI, r20, r0, 55) = asm("add_i r20, r0, 55") severity failure;
            assert ins6L(j, -512) = asm("ja -512") severity failure;
            --    TP <= ins6L(j, -512);
             --   TQ <= asm("ja -512");
            assert ins655655(ext2, 0, 0, retE, 0, 0) = asm("sys rete") severity failure;
      

            num0 <= integer'value(TMP_str0(2 to 10));
            num1 <= integer'value(TMP_str1);

                            --TEST_word <=  asm("add_i r0, r0, 0");
                            TEST_word <=  asm("ldf_i f7, r0, 20");
                            TEST_gb <= parseInstructionString("ldf_i f7, r0, 20" & cr);
                            

    okFlag <= bool2std(opFlags = "001");
    errorFlag <= bool2std(opFlags = "100");

   -- Instantiate the Unit Under Test (UUT)
   uut: Core PORT MAP (
      clk => clk,
      reset => reset,
      en => en,
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

   -- Clock process definitions
   clk_process :process
   begin
		clk <= '1';
		wait for clk_period/2;
		clk <= '0';
		wait for clk_period/2;
   end process;
 	
	reset <= '1' after 105 ns, '0' after 115 ns;
	en <= '1' after 105 ns;
	
	int0 <= int0a or int0b;
	
    --testDone <= oaux(0);
    --testFail <= oaux(1);
	
   -- Stimulus process
   stim_proc: process
       variable progB: ProgramBuffer;
       variable testName, suiteName, disasmText: line;
       file suiteFile: text open read_mode is "suite_names.txt";
       file testFile: text;
	   
        variable insWordVar: Word;
        variable intOpVar: InternalOperation;
        variable opResultVar: OperationResult;
         
      variable match: boolean := true;     
   begin
	
	  wait for 110 ns;
      
      loop
          suiteName := null;
          readline(suiteFile, suiteName);
          if suiteName = null then -- testName'length = 0 then
              exit;
          elsif suiteName(1) = ';' then
              next;
          end if;          
          
          report "Starting suite: " & suiteName.all;
          
          file_open(testFile, suiteName.all & ".txt", read_mode);
          loop
              testName := null;	  
              readline(testFile, testName);
              if testName = null then -- testName'length = 0 then
                  exit;
              elsif testName(1) = ';' then
                  next;
              end if;
    
              report "Now to run: " & testName.all;
              progB := readSourceFile(testName.all & ".txt");
              machineCode <= processProgram(progB);

                    currentSuite <= (others => ' ');
                    currentTest <= (others => ' ');
                    currentSuite(1 to suiteName.all'length) <= suiteName.all;
                    currentTest(1 to testName.all'length) <= testName.all;

                        --instructionWord <= (others => 'U');
                        internalOp <= DEFAULT_INTERNAL_OP;
                        --disasm <= (others => ' ');
                        currentInstruction <= ((others => 'U'), (others => 'U'), (others => ' '));
                        
						opFlags <= (others => '0');
						cpuState <= INIT_CORE_STATE;
						dataMemory <= (others => (others => '0'));
               
                resetDataMem <= '1';
               
              wait until rising_edge(clk);
                
                resetDataMem <= '0';
                
              --  dataMem <= (others => (others => '0'));
              
              testProgram(0 to machineCode'length-1) <= machineCode(0 to machineCode'length-1);
              testProgram(512/4) <= ins6L(j, -512);-- TEMP! 
              testProgram(384/4) <= ins655655(ext2, 0, 0, send, 0, 0);
              testProgram(384/4 + 1) <= ins6L(j, 0); -- idle loop          

                programMemory(0 to machineCode'length-1) <= machineCode(0 to machineCode'length-1);
                programMemory(512/4) <= ins6L(j, -512);-- TEMP! 
                programMemory(384/4) <= ins655655(ext2, 0, 0, send, 0, 0);
                programMemory(384/4 + 1) <= ins6L(j, 0); -- idle loop   

                if CORE_SIMULATION then
                      testToDo <= '1';
                      int0b <= '1';                      
                      wait until rising_edge(clk);
                      testToDo <= '0';
                      int0b <= '0';
                end if;
                
                if EMULATION then
                    wait for TIME_STEP;
                end if;
            
              disasmToFile(testName.all & "_disasm.txt", testProgram);
                    
              report "Waiting for completion...";

                
                if EMULATION then    
                    -- Now doing the actual test 
                    while opFlags /= "100" and opFlags /= "001" loop -- ERROR or SEND (completed)
                        insWordVar := programMemory(slv2u(cpuState.nextIP)/4);
                        --instructionWord <= insWordVar;
                        intOpVar := decode(cpuState.nextIP, insWordVar);
                        internalOp <= intOpVar;
                        --disasm <= disasmWithAddress(slv2u(cpuState.nextIP), programMemory(slv2u(cpuState.nextIP)/4));
                        
                            currentInstruction <= (cpuState.nextIP, insWordVar,  disasmWithAddress(slv2u(cpuState.nextIP), programMemory(slv2u(cpuState.nextIP)/4)));   
                        performOp(cpuState, dataMemory, intOpVar, opFlags, opResultVar);
                        
                        if LOG_EMULATION_TRACE then
                               -- log trace
                               write(disasmText, disasmWithAddress(slv2u(cpuState.nextIP), insWordVar));
                               writeline(traceFile, disasmText);
                        end if;
                        
                        wait for TIME_STEP;
                    end loop;
                end if;

                if CORE_SIMULATION then
                      loop
                          wait until rising_edge(clk);
        
                          if cpuEndFlag = '1' then
                              report "Test done";
                              testDone <= '1';
                              exit;
                          end if;
                          
                          if cpuErrorFlag = '1' then
                              report "TEST FAIL: " & testName.all;
                              testFail <= '1';
                              wait;                     
                          end if;                  
                      end loop;
                end if;
                
              wait until rising_edge(clk);
                        testDone <= '0';
                        testFail <= '0';              
          end loop;
          
          report "All tests in suite done!";
          
          wait until rising_edge(clk);
                    
      
              compareTraceFiles("emulation_trace.txt", "CoreDB_committed.txt", match);
                    report "Traces match: " & boolean'image(match);
                assert match report "Traces are divergent!" severity error;
                
      
           --         wait;
      end loop;
          
      report "All suites done!";
      currentSuite <= (others => ' ');
      currentTest <= (others => ' ');

        internalOp <= DEFAULT_INTERNAL_OP;
        --disasm <= (others => ' ');
        currentInstruction <= ((others => 'U'), (others => 'U'), (others => ' '));
        
        opFlags <= (others => '0');
        cpuState <= INIT_CORE_STATE;
        dataMemory <= (others => (others => '0'));
      
      wait until rising_edge(clk);
      
      report "Run exception tests";
      testProgram(0) <= ins655655(ext2, 0, 0, error, 0, 0);
      testProgram(1) <= ins6L(j, 0);
      
      wait until rising_edge(clk);

      testToDo <= '1';
      int0b <= '1';
      wait until rising_edge(clk);
      testToDo <= '0';
      int0b <= '0';
      
      disasmToFile("error_disasm.txt", testProgram);
      
      report "Waiting for completion...";
 
      wait until rising_edge(clk);

      loop
          wait until rising_edge(clk);
              if cpuEndFlag = '1' then
                  report "Success signal when error expected!";
                  testFail <= '1';
                  wait;
              end if;
              
              if cpuErrorFlag = '1' then
                  report "Error signal confirmed correctly";
                  testDone <= '1';
                  exit;                     
              end if;                  
      end loop;     

      wait until rising_edge(clk);
            testDone <= '0';
            testFail <= '0';
            
      report "Now test exception return";

	  progB := readSourceFile("events.txt" );
      machineCode <= processProgram(progB);
      wait until rising_edge(clk);

        
      testProgram(0 to machineCode'length-1) <= machineCode(0 to machineCode'length-1);
      testProgram(512/4) <= ins6L(j, -512);-- TEMP!        

      testProgram(384/4) <= ins655H(addI, r20, r0, 55);
      testProgram(384/4 + 1) <= ins655655(ext2, 0, 0, retE, 0, 0);
      
      --wait until rising_edge(clk);         
      testToDo <= '1';
      int0b <= '1';
      wait until rising_edge(clk);
      testToDo <= '0';
      int0b <= '0';
      
      disasmToFile("events_disasm.txt", testProgram);      
      report "Waiting for completion...";

     wait until rising_edge(clk);
 
     loop
        wait until rising_edge(clk);
          if cpuEndFlag = '1' then
              report "Test done";
              testDone <= '1';
              exit;
          end if;
          
          if cpuErrorFlag = '1' then
              report "TEST FAIL: " & "events";
              testFail <= '1';            
              wait;                     
          end if;                  
      end loop;      

      report "Now test interrupts";

	  progB := readSourceFile( "events2.txt");
      machineCode <= processProgram(progB);
      wait until rising_edge(clk);
                        testDone <= '0';
                        testFail <= '0';
          
      testProgram(0 to machineCode'length-1) <= machineCode(0 to machineCode'length-1);
      testProgram(512/4) <= ins6L(j, -512);-- TEMP!        
      
      testProgram(384/4) <= ins655H(addI, r20, r0, 55);
      testProgram(384/4 + 1) <= ins655655(ext2, 0, 0, retE, 0, 0);
      
      testProgram(640/4) <= ins655H(addI, r0, r0, 0); -- NOP
      testProgram(640/4 + 1) <= ins655655(ext2, 0, 0, retI, 0, 0);          
      
      --wait until rising_edge(clk);         
      testToDo <= '1';
      int0b <= '1';
      wait until rising_edge(clk);
      testToDo <= '0';
      int0b <= '0';
      
      disasmToFile("events2_disasm.txt", testProgram);      
      report "Waiting for completion...";


      wait until rising_edge(clk);
        -- After x cycles send interrupt
      wait for 22 * 10 ns;
      wait until rising_edge(clk);        
      int1 <= '1';
      wait until rising_edge(clk);
      int1 <= '0';      
 
      loop
          wait until rising_edge(clk);
          
          if cpuEndFlag = '1' then
              report "Test done";
              testDone <= '1';
              
              exit;
          end if;
          
          if cpuErrorFlag = '1' then
              report "TEST FAIL: " & "events2";
              testFail <= '1';            
              wait;                     
          end if;                  
      end loop;      

              report "All test runs have been completed successfully";

        wait until rising_edge(clk);
            testDone <= '0';
            testFail <= '0';

      wait;
   end process;


	PROGRAM_MEM: process (clk)
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
                iin(i) <= testProgram(slv2u(baseIP(10 downto 2)) + i); -- CAREFUL! 2 low bits unused (32b memory) 									
            end loop;
					
            if iadrvalid = '1' and countOnes(iadr(iadr'high downto 12)) = 0 then
                ivalid <= '1';					
            else
                ivalid <= '0';	
            end if;			
		end if;	
	end process;	


    DATA_MEM: block
        signal memReadDone, memReadDonePrev, memWriteDone: std_logic := '0';
        signal memReadValue, memReadValuePrev, memWriteAddress, memWriteValue: Mword := (others => '0');
        signal dataMem: WordArray(0 to 255) := (others => (others => '0'));
    begin
        SYNCH: process (clk)
        
        begin
            if rising_edge(clk) then
                if resetDataMem = '1' then
                    dataMem <= (others => (others => '0'));
                elsif en = '1' then			
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
                    if dwrite = '1' then
                        dataMem(slv2u(doutadr(MWORD_SIZE-1 downto 2))) <= dout; -- CAREFUL: pseudo-byte addressing		
                    end if;
                    
                end if;
            end if;	
        end process;
    
        din <= memReadValue;
        dvalid <= memReadDone;
	end block;
END;
