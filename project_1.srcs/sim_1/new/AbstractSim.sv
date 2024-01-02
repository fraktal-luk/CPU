`timescale 1ns / 1ps

     import Base::*;
     import InsDefs::*;
     import Asm::*;

package AbstractSim;
    
     import Base::*;
     import InsDefs::*;
     import Asm::*;

     class ProgramMemory #(parameter WIDTH = 4);
        typedef Word Line[4];
        
        Word content[1024];
        
        function void setContent(Word arr[]);
            foreach (arr[i]) this.content[i] = arr[i];
        endfunction
        
        function Line read(input Word adr);
            Line res;
            Word truncatedAdr = (adr/4);
            
            foreach (res[i]) res[i] = content[truncatedAdr + i];
            return res;
        endfunction
        
        
    endclass
    



endpackage


module AbstractCore
#(
    parameter FETCH_WIDTH = 4
)
(
    input logic clk,
    output logic insReq, output Word insAdr, input Word insIn[FETCH_WIDTH],
    output logic readReq, output Word readAdr, input Word readIn,
    output logic writeReq, output Word writeAdr, output Word writeOut,
    
    input logic interrupt,
    input logic reset,
    output logic sig
);
    
    
    typedef Word FetchGroup[FETCH_WIDTH];

    logic resetPrev = 0;
    Word ip = 0;
    
    
    FetchGroup fetchedStage0, fetchedStage1;
    
    assign fetchedStage0 = insIn;
    
    always @(posedge clk) begin
        
        resetPrev = reset;
        
        if (resetPrev) begin
            ip <= 0;
        end
        else begin
            ip <= ip + 4*FETCH_WIDTH;
            fetchedStage1 <= fetchedStage0;
        end
        //fetchedStage1 <= fetchedStage0;
        
    end
    
    assign insAdr = ip;
    

endmodule
