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

    signal currentSuite, currentTest: string(1 to 30);    
    signal testToDo, testDone, testFail: std_logic := '0';
    
    
    signal simDone, emulDone: std_logic := '1';
        
    signal emulReady: std_logic := '0';
    
    
    constant EMULATION: boolean := true;
    constant LOG_EMULATION_TRACE: boolean := true;
    constant CORE_SIMULATION: boolean := true;
    

    --Inputs
    signal clk : std_logic := '0';
    signal reset : std_logic := '0';
    signal en : std_logic := '0';
    

    signal int0a, int0b, int1sig: std_logic := '0';

    signal resetDataMem: std_logic := '0';
    
    -- Clock period definitions
    constant clk_period : time := 10 ns;
	
    signal testProgram2: WordArray(0 to 2047);
    
    alias programMemory2 is testProgram2;
    
    signal cpuEndFlag: std_logic := '0';
    signal cpuErrorFlag: std_logic := '0';

    type Instruction is record
        address: Mword;
        bits: Word;
        disasm: string(1 to 51);
        internalOp: InternalOperation;
    end record;
        
    signal opFlags: std_logic_vector(0 to 2);
    signal okFlag, errorFlag: std_logic := '0';

    signal commonCode2: WordArray(0 to PROGRAM_BUFFER_SIZE-1);

    signal ch0, ch1, ch2, ch3,  ch4, ch5, ch6, ch7: std_logic := '0';
    
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
        loop
            la := null;
            lb := null;
            ia := ia + 1;
            ib := ib + 1;
            readline(fa, la);
            readline(fb, lb);
            
            while la /= null and skipLine(la.all) loop la := null; readline(fa, la); ia := ia + 1; end loop;
            while lb /= null and skipLine(lb.all) loop lb := null; readline(fb, lb); ib := ib + 1; end loop;

            if la = null and lb = null then
                return;
            end if;
            
            if (la = null) /= (lb = null) then
                match := false;
                return;
            end if; 
            
            if not compareTraceLines(la.all, lb.all) then
                match := false;
                return;
            end if;            
        end loop;
    end procedure;
              
    procedure cycle(signal clk: in std_logic) is
    begin
        wait until rising_edge(clk);
    end procedure;

    procedure cycle is
    begin
        wait until rising_edge(clk);
    end procedure;

        
    procedure checkTestResult(variable testName: in line; signal testDone, testFail: out std_logic) is
    begin
        loop
          cycle(clk);
    
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
            
        cycle(clk);
        testDone <= '0';
        testFail <= '0';  
    end procedure;

    procedure checkTestResult(testName: in string; signal testDone, testFail: out std_logic) is
    begin
        loop
          cycle(clk);
    
          if cpuEndFlag = '1' then
              report "Test done";
              testDone <= '1';
              exit;
          end if;
          
          if cpuErrorFlag = '1' then
              report "TEST FAIL: " & testName;
              testFail <= '1';
              wait;                     
          end if;                  
        end loop;
            
        cycle(clk);
        testDone <= '0';
        testFail <= '0';  
    end procedure;

    procedure checkErrorTestResult(testName: in string; signal testDone, testFail: out std_logic) is
    begin
        loop
          cycle(clk);
            
          -- CAREFUL: This is reversed because we expect error signal
          if cpuErrorFlag = '1' then
              report "Test done";
              testDone <= '1';
              exit;
          end if;
          
          if cpuEndFlag = '1' then
              report "TEST FAIL: " & testName;
              testFail <= '1';
              wait;                     
          end if;                  
        end loop;
            
        cycle(clk);
        testDone <= '0';
        testFail <= '0';  
    end procedure;

    procedure announceTest(signal nameOut, suiteOut: out string; name, suite: string) is
    begin
        stringAssign(nameOut, name);
        stringAssign(suiteOut, suite);
        
        report "Test to run: " & name;
    end procedure;
    
    procedure startTest(signal testToDo, int0b: out std_logic) is
    begin
      cycle(clk);
      testToDo <= '1';
      int0b <= '1';
      cycle(clk);                     
      testToDo <= '0';
      int0b <= '0';
    end procedure;
    
    procedure setForOneCycle(signal s: out std_logic; signal clk: std_logic) is
    begin
        s <= '1';
        cycle(clk);
        s <= '0';
        cycle(clk);
    end procedure;
    
    procedure putInstructionSequence(signal programMem: inout WordArray; address: in Mword; insSeq: WordArray) is
        constant startAdr: natural := slv2u(address)/4;
        constant LEN: natural := insSeq'length;
    begin
        assert isNonzero(address(1 downto 0)) = '0' report "Unaligned instruction address!" severity error;
    
        programMem(startAdr to startAdr + LEN - 1) <= insSeq;    
    end procedure;

    procedure loadProgramFromFileWithImports(filename: in string; libExports: XrefArray; libStart: Mword; signal testProgram: out WordArray) is        
	    --constant prog: ProgramBuffer := readSourceFile(filename).words; -- range: 0 to 999
	    constant code: CodeBuffer := readSourceFile(filename);
	    variable wordBuf: WordBuffer;
        variable machineCode: WordArray(0 to PROGRAM_BUFFER_SIZE-1);
        variable imp, exp: XrefArray(0 to 100);
    begin
        processProgram(code, wordBuf, imp, exp);
        machineCode := wordBuf.words(0 to PROGRAM_BUFFER_SIZE-1);
        machineCode := fillXrefs(machineCode, imp, matchXrefs(imp, libExports), 0, slv2u(libStart));

        testProgram <= (others => (others => 'U'));
        testProgram(0 to machineCode'length-1) <= machineCode(0 to machineCode'length-1);        
    end procedure;


    procedure setInstruction(signal testProgram: inout WordArray; offset: Mword; ins: string) is
        constant offsetInt: natural := slv2u(offset)/4; 
    begin
        testProgram(offsetInt) <= asm(ins);
    end procedure;

    procedure setProgram(signal testProgram: inout WordArray; offset: Mword; program: WordArray) is
        constant offsetInt: natural := slv2u(offset)/4; 
    begin
        testProgram(offsetInt to offsetInt + program'length-1) <= program;
    end procedure;

    -- Differs from simple ln.all in that it's written to a string of predefined length
    procedure fillStringFromLine(signal s: out string; variable ln: in line) is
    begin
        s <= (others => ' ');
        s(1 to ln.all'length) <= ln.all;
    end procedure;
    
    
    function getInstruction(signal cpuState: in CoreState; signal programMemory: in WordArray) return Instruction is
        variable res: Instruction;
        variable insWordVar: Word;
        variable intOpVar: InternalOperation;
    begin
        insWordVar := programMemory(slv2u(cpuState.nextIP)/4);
        intOpVar := decodeAbstract(cpuState.nextIP, insWordVar);
        res := (cpuState.nextIP, insWordVar, work.InstructionSet.TMP_disasm(insWordVar), intOpVar);
        return res;
    end function;

BEGIN
   okFlag <= bool2std(opFlags = "001");
   errorFlag <= bool2std(opFlags = "100");

   -- Clock process definitions
   clk_process: process
   begin
		clk <= '1';
		wait for clk_period/2;
		clk <= '0';
		wait for clk_period/2;
   end process;
 	
   reset <= '1' after 105 ns, '0' after 115 ns;
   en <= '1' after 105 ns;
	
   -- Stimulus process
   stim_proc: process
       variable testName, suiteName, disasmText: line;
       file suiteFile: text open read_mode is "suite_names.txt";
       file testFile: text;

       variable machineCodeVar2: WordArray(0 to PROGRAM_BUFFER_SIZE-1);
       variable machineCodeBuf: WordBuffer;

       variable exp, imp: XrefArray(0 to 100);
       
       variable match: boolean := true;
   begin
              processProgram(readSourceFile("common_asm.txt"), machineCodeBuf, imp, exp);
              machineCodeVar2 := machineCodeBuf.words(0 to PROGRAM_BUFFER_SIZE-1);
              commonCode2 <= machineCodeVar2;	           

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
              if testName = null then
                  exit;
              elsif testName(1) = ';' then
                  next;
              end if;

              announceTest(currentTest, currentSuite, testName.all, suiteName.all);    


              loadProgramFromFileWithImports(testName.all & ".txt", exp, i2slv(4*1024, MWORD_SIZE), programMemory2);

                -- Reset handler
                setInstruction(programMemory2, RESET_BASE, "ja -512");

                -- Error handler
                setInstruction(programMemory2, EXC_BASE, "sys error");
                setInstruction(programMemory2, addInt(EXC_BASE, 4), "ja 0");

                -- Call handler
                setInstruction(programMemory2, CALL_BASE, "sys send");
                setInstruction(programMemory2, addInt(CALL_BASE, 4), "ja 0");
                
                -- Common lib
                setProgram(testProgram2, i2slv(4*1024, 32), commonCode2);	           


              setForOneCycle(resetDataMem, clk);
              disasmToFile(testName.all & "_disasm.txt", testProgram2);

              if CORE_SIMULATION then
                  startTest(testToDo, int0b);
                  report "Waiting for completion...";
                  cycle;
                  checkTestResult(testName, testDone, testFail);
              end if;
            
              -- Wait for emulation to end 
              while emulReady /= '1' loop           
                  cycle;
              end loop;
            
          end loop;
          
          report "All tests in suite done!";          
          cycle;
      end loop;
        
      if EMULATION and LOG_EMULATION_TRACE and CORE_SIMULATION then -- TODO: scenario where emulation happens along with Core sim?
          --compareTraceFiles("emulation_trace.txt", "CoreDB_committed.txt", match);
          --report "Traces match: " & boolean'image(match);
          --assert match report "Traces are divergent!" severity error;
      end if;
      
      report "All suites done!";        
      cycle;
      
      -----------------------------------------------
      -----------------------------------------------

      -- Test error signal  
      announceTest(currentTest, currentSuite, "err signal", "");      

          --testProgram2(0) <= asm("sys error");
          --testProgram2(1) <= asm("ja 0");
	  	  
	  	  setInstruction(programMemory2, i2slv(0, MWORD_SIZE), "sys error");
	  	  setInstruction(programMemory2, i2slv(4, MWORD_SIZE), "ja 0");
	  	  
	  	  
    	  setProgram(testProgram2, i2slv(4*1024, 32), commonCode2);	           


      setForOneCycle(resetDataMem, clk);
      
      disasmToFile("error_disasm.txt", testProgram2);

      if CORE_SIMULATION then
          startTest(testToDo, int0b);  
          report "Waiting for completion...";
          checkErrorTestResult("check_error", testDone, testFail);  
      end if;
      
        -- Wait for emulation to end 
        while emulReady /= '1' loop           
            cycle;
        end loop;

      cycle;
      
      -------------
      -------------
      announceTest(currentTest, currentSuite, "exc return", "");
      
      loadProgramFromFileWithImports("events.txt", exp, i2slv(4*1024, MWORD_SIZE), programMemory2);

              -- Reset handler      
              --testProgram2(slv2u(RESET_BASE)/4) <=     asm("ja -512");       
              setInstruction(programMemory2, RESET_BASE, "ja -512");

              -- Error handler
	  	      setInstruction(programMemory2, EXC_BASE, "sys error");
	  	      setInstruction(programMemory2, addInt(EXC_BASE, 4), "ja 0");
	  	        
              -- Call handler - special
              --testProgram2(slv2u(CALL_BASE)/4) <=     asm("add_i r20, r0, 55");  
              --testProgram2(slv2u(CALL_BASE)/4 + 1) <= asm("sys rete");
              setInstruction(programMemory2, CALL_BASE, "add_i r20, r0, 55");
	  	      setInstruction(programMemory2, addInt(CALL_BASE, 4), "sys rete");

              
              -- Common lib
              setProgram(testProgram2, i2slv(4*1024, 32), commonCode2); 


      setForOneCycle(resetDataMem, clk); 

      disasmToFile("events_disasm.txt", testProgram2);
      
      if CORE_SIMULATION then
          startTest(testToDo, int0b);      
          
          report "Waiting for completion...";
    
          checkTestResult("events", testDone, testFail);   
       end if;
       
        -- Wait for emulation to end 
        while emulReady /= '1' loop           
            cycle;
        end loop;
      
      -------
      -------
      announceTest(currentTest, currentSuite, "interrupt", "");      

      loadProgramFromFileWithImports("events2.txt", exp, i2slv(4*1024, MWORD_SIZE), programMemory2);
              -- Reset handler
              --testProgram2(slv2u(RESET_BASE)/4) <=     asm("ja -512");
              setInstruction(programMemory2, RESET_BASE, "ja -512");

              -- Error handler
	  	      setInstruction(programMemory2, EXC_BASE, "sys error");
	  	      setInstruction(programMemory2, addInt(EXC_BASE, 4), "ja 0");

              -- Call handler - special
              --testProgram2(slv2u(CALL_BASE)/4) <=     asm("add_i r20, r0, 55");
              --testProgram2(slv2u(CALL_BASE)/4 + 1) <= asm("sys rete");
              setInstruction(programMemory2, CALL_BASE, "add_i r20, r0, 55");
              setInstruction(programMemory2, addInt(CALL_BASE, 4), "sys rete");
              
              -- Int handler - special
              --testProgram2(slv2u(INT_BASE)/4) <=     asm("add_i r0, r0, 0"); -- NOP
              --testProgram2(slv2u(INT_BASE)/4 + 1) <= asm("sys reti");
              setInstruction(programMemory2, INT_BASE, "add_i r0, r0, 0"); -- NOP
              setInstruction(programMemory2, addInt(INT_BASE, 4), "sys reti");
                
              -- Common lib
              setProgram(testProgram2, i2slv(4*1024, 32), commonCode2);  
              
      
      setForOneCycle(resetDataMem, clk);
      
      disasmToFile("events2_disasm.txt", testProgram2);
      
      if CORE_SIMULATION then
          startTest(testToDo, int0b);
               
          report "Waiting for completion...";
    
          cycle;
            -- After x cycles send interrupt
          wait for 22 * 10 ns;
          cycle;
    
          setForOneCycle(int1sig, clk);
    
          checkTestResult("events2", testDone, testFail);  
      end if;
      
      -- Wait for emulation to end 
      while emulReady /= '1' loop           
          cycle;
      end loop;      
      
        
      report "All test runs have been completed successfully";
      cycle;

      wait;
   end process;

    
  TMP_EMULATION: block
      signal cpuState2: CoreState := INIT_CORE_STATE;
      signal dataMemory2: ByteArray(0 to 4095);
      signal currentInstruction2: Instruction;
  begin
        TMP_EMUL: process (clk)
            type EmulState is (ready, prepare, running);
            variable state: EmulState := ready;
            variable currentInstructionVar2: Instruction;
            variable opResultVar2: OperationResult;
            variable disasmText: line;
        begin
            if rising_edge(clk) then
                case state is
                    when ready =>
                        if resetDataMem = '1' then
                            emulDone <= '0';
                            state := prepare;
                          
                            currentInstruction2 <= ((others => 'U'), (others => 'U'), (others => ' '), DEFAULT_INTERNAL_OP);
                              
                            opFlags <= (others => '0');                          
                            cpuState2 <= INIT_CORE_STATE;
                            dataMemory2 <= (others => (others => '0'));                                                       
                        end if;
                    
                    when prepare =>
                        if testToDo = '1' then
                            state := running;
                        end if;
                        
                    when running =>
                        if EMULATION then
                            -- Now doing the actual test 
                            if opFlags /= "100" and opFlags /= "001" then -- ERROR or SEND (completed)
                                currentInstructionVar2 := getInstruction(cpuState2, programMemory2);
                                currentInstruction2 <= currentInstructionVar2;
                                performOp(cpuState2, dataMemory2, currentInstructionVar2.internalOp, opFlags, opResultVar2);
                            
                                if LOG_EMULATION_TRACE then
                                   write(disasmText, disasmWithAddress(slv2u(cpuState2.nextIP), currentInstructionVar2.bits));
                                   writeline(traceFile, disasmText);                                    
                                end if;
                            else
                                state := ready;
                            end if;
                        else
                            state := ready;
                        end if;
                                          
                    when others =>
                end case;            
                
                emulReady <= bool2std(state = ready);                
            end if;
        end process;

    end block;


    SIMULATION: block
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
              
        -- CPU ports
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
        -- end CP ports    
    begin

        int0 <= int0a or int0b;
        int1 <= int1sig;

        cpuEndFlag <= oaux(0);
        cpuErrorFlag <= oaux(1);    
    
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
                    iin(i) <= testProgram2(slv2u(baseIP(12 downto 2)) + i); -- CAREFUL! 2 low bits unused (32b memory) 									
                end loop;
                
                ivalid <= iadrvalid and not isNonzero(iadr(iadr'high downto 12));
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
                        if isNonzero(dadr(MWORD_SIZE-1 downto 10)) /= '1' then
                            memReadValue <= dataMem(slv2u(dadr(MWORD_SIZE-1 downto 2))) ;-- CAREFUL: pseudo-byte addressing 
                        end if;
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
        
    end block;
END;
