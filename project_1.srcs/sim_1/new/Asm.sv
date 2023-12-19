
package Asm;

    import InsDefs::*;

    typedef string squeue[$];

    function automatic squeue readFile(input string name);
        int file = $fopen(name, "r");
        string line;
        string lines[$];

        while (!$feof(file)) begin
            $fgets(line, file);
            lines.push_back(line);
        end
        
        $fclose(file);
                
        return lines;
    endfunction

    function bit isLetter(input logic[7:0] char);
        return (char inside {["A":"Z"], ["A":"z"]});            
    endfunction
    
    function bit isDigit(input logic[7:0] char);
        return (char inside {["0":"9"]});            
    endfunction

    function bit isAlpha(input logic[7:0] char);
        return (char inside {["A":"Z"], ["A":"z"], ["0":"9"], "_"});            
    endfunction

    function bit isWhite(input logic[7:0] char);
        return (char inside {" ", "\t"});            
    endfunction

    function automatic squeue breakLine(input string line);
        squeue elems;

        for (int i = 0; i < line.len(); ) begin
            
            if (line[i] inside {0, ";", "\n"}) begin
                break;
            end
            else if (isWhite(line[i])) begin
                while (isWhite(line[i])) i++; // Skip spaces
            end
            else if (line[i] == "$") begin
                int iStart = i;
                i++;
                while (isAlpha(line[i])) begin
                    i++;
                end
                
                //$display(line.substr(iStart, i-1));
                elems.push_back(line.substr(iStart, i-1));
            end
            else if (isAlpha(line[i]) || line[i] == "-") begin
                int iStart = i;
                i++;
                while (isAlpha(line[i])) begin
                    i++;
                end
                
                //$display(line.substr(iStart, i-1));
                elems.push_back(line.substr(iStart, i-1));
            end
            else if (line[i] == ",") begin
                i++;
            end
            else begin
                i++;
                $error("char %s at %d not recognized", line[i-1], i-1);
            end
            
        end
        
        return elems;
    endfunction

    function automatic void processLines(input squeue lines);
    
        squeue labels = '{};
        squeue errors = '{};
    
        int nInstructionLines = 0;
    
        foreach (lines[i]) begin
            squeue parts = breakLine({lines[i], 8'h0});
            if (parts.size() == 0)
                continue;
            else if (parts[0][0] == "$") begin
                labels.push_back(parts[0]);
                errors.push_back($sformatf("%d: Something after label", i));
            end
            else begin
                analyzeCodeLine(nInstructionLines, parts);
                nInstructionLines++;
            end
        end
        
    endfunction

    typedef enum {
        NONE, SOME
    } ParseError;

    typedef struct {
        int line;
        squeue parts;
        ParseError error = SOME;
        string label;
    } CodeLine;

    function automatic CodeLine analyzeCodeLine(input int line, input squeue parts);
        CodeLine res;
        string mnemonic = parts[0];
 
        res.line = line;
        res.parts = parts;
        res.error = NONE;
        
        $display("Code line: %s", parts[0]);
        
        if (!isLetter(mnemonic[0])) begin
            res.error = SOME;
            return res;
        end
        
        // TODO: get rid of this hack, define sys instructions like sys_call etc?
        if (mnemonic == "sys")
            mnemonic = {mnemonic,"_", parts[1]};

          //  $display(getFormatName(mnemonic));
          getParsing(mnemonic);
            
        // get expected format 
        
        // check args conformance to format
        
        // if constant arg is a label, set it
        
        return res;
    endfunction

endpackage
