//`timescale 1ns / 1ps

 import Base::*;
 import InsDefs::*;
 import Asm::*;
 import Emulation::*;

package AbstractSim;
    
     import Base::*;
     import InsDefs::*;
     import Asm::*;
     import Emulation::*;

     class ProgramMemory #(parameter WIDTH = 4);
        typedef Word Line[4];
        
        Word content[1024];
        
        function void setBasicHandlers();
            this.content[IP_RESET/4] = processLines({"ja -512"}).words[0];
            this.content[IP_RESET/4 + 1] = processLines({"ja 0"}).words[0];
            
            this.content[IP_ERROR/4] = processLines({"sys error"}).words[0];
            this.content[IP_ERROR/4 + 1] = processLines({"ja 0"}).words[0];
    
            this.content[IP_CALL/4] = processLines({"sys send"}).words[0];
            this.content[IP_CALL/4 + 1] = processLines({"ja 0"}).words[0];
        endfunction
        
        function void clear();
            this.content = '{default: 'x};
        endfunction
        
        function void setContent(input Word arr[]);
            foreach (arr[i]) this.content[i] = arr[i];
        endfunction

        function void setContentAt(input Word arr[], input Word adr);
            assert((adr % 4) == 0) else $fatal("Unaligned instruction address not allowed");
            foreach (arr[i]) this.content[adr/4 + i] = arr[i];
        endfunction
        
        function Line read(input Word adr);
            Line res;
            Word truncatedAdr = adr & ~(4*WIDTH-1);
            
            foreach (res[i]) res[i] = content[truncatedAdr/4 + i];
            return res;
        endfunction
        
        
    endclass
    
     class DataMemory #(parameter WIDTH = 4);
        typedef logic[7:0] Line[4];
        
        logic[7:0] content[4096];
        
        function void setContent(Word arr[]);
            foreach (arr[i]) this.content[i] = arr[i];
        endfunction
        
        function void clear();
            this.content = '{default: 'x};
        endfunction;
        
        function automatic Word read(input Word adr);
            Word res = 0;
            
            for (int i = 0; i < 4; i++) 
                res = (res << 8) + content[adr + i];
            
            return res;
        endfunction

        function automatic void write(input Word adr, input Word value);
            Word data = value;
            
            for (int i = 0; i < 4; i++) begin
                content[adr + i] = data[31:24];
                data <<= 8;
            end        
        endfunction    
        
    endclass


endpackage
