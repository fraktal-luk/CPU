`timescale 1ns / 1ps

import InsDefs::*;
import Asm::*;

module ArchDesc0();

    Word w0 = 0;

    MnemonicClass::Mnemonic mnem;

    initial begin
        automatic squeue lines = readFile("a_tmp_sf1.txt");        
        #10 $display(lines.size());
        //$display("0: %p", breakLine({lines[0], 8'h0}));
        //$display("1: %p", breakLine({lines[23], 8'h0}));
        //$display("2: %p", breakLine({lines[24], 8'h0}));
        
        processLines(lines);

    end



endmodule
