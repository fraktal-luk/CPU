
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
            else if (line[i] inside {"$", "@"}) begin
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


    typedef enum {
        NONE, SOME
    } ParseError;


    typedef struct {
        int line;
        int codeLine;
        squeue parts;
        ParseError error = SOME;
        Word ins;
        CodeRef codeRef;
    } CodeLine;

    typedef struct {
        int line;
        int codeLine;
        squeue parts;
        ParseError error = SOME;
        //Word ins;
        //CodeRef codeRef;
        string label;
    } DirectiveLine;

    typedef struct {
        int codeLine; string label; int size;
    } ImportRef;

    typedef struct {
        int codeLine; string label;
    } ExportRef;

    typedef struct {
        string desc;
        Word words[];
        ImportRef imports[];
        ExportRef exports[];
    } Section;

    function automatic Section processLines(input squeue lines);
        Section res;
        squeue labels = '{};
        int labelMap[string];
        ImportRef importMap[string];
        ImportRef imports[$];
        ExportRef exports[$];
        squeue errors = '{};
        CodeLine instructions[$];
        Word code[];
    
        int nInstructionLines = 0;
    
        foreach (lines[i]) begin
            squeue parts = breakLine({lines[i], 8'h0});
            if (parts.size() == 0)
                continue;
            else if (parts[0][0] == "$") begin
                labels.push_back(parts[0]);
                labelMap[parts[0]] = nInstructionLines + 1;
                errors.push_back($sformatf("%d: Something after label", i));
            end
            else if (parts[0][0] == "@") begin
                DirectiveLine dl = analyzeDirective(i, nInstructionLines, parts);
                if (dl.label.len() != 0)
                    exports.push_back('{nInstructionLines, dl.label});
            end
            else begin
                instructions.push_back(analyzeCodeLine(i, nInstructionLines, parts));
                nInstructionLines++;
            end
        end
        
        code = new[nInstructionLines];
        
        // Resolve labels
        foreach(instructions[i]) begin
            CodeLine ins = instructions[i];
            if (ins.codeRef.label.len() != 0) begin
                if (labelMap.exists(ins.codeRef.label)) begin
                    int cline = labelMap[ins.codeRef.label];
                    int targetAdr = 4*cline;
                    int usingAdr = 4*ins.codeLine;
                    Word newWord = ins.ins;
                    $display("Resolving ref: %s -> ins %d", ins.codeRef.label, cline);
                    
                    if (ins.codeRef.ref26 == 1) newWord[25:0] = (targetAdr - usingAdr);
                    else if (ins.codeRef.ref21 == 1) newWord[20:0] = (targetAdr - usingAdr);
                    
                    $display("%h", newWord);
                    instructions[i].ins = newWord;
                end
                else begin
                    int size = ins.codeRef.ref26 == 1 ? 26 : 21;
                    $display("Unresolved reference: %d: %s", ins.line, ins.codeRef.label);
                    
                    imports.push_back('{ins.codeLine, ins.codeRef.label, size});
                    //importMap[ins.codeRef.label] = '{ins.codeLine, ins.codeRef.label, size};
                end             
            end
            code[i] = instructions[i].ins;
        end
        
        res.words = code;
        
        begin
            int nImports = imports.size();
            res.imports = new[nImports](imports[0:$]);
        //for (importMap[s])
        //    res.imports = '{};
        end
        
        $display("%p", exports);
        
            foreach (res.words[i]) begin
            //    TMP_disasm(res.words[i]);
            end
        
        return res;
    endfunction


    function automatic CodeLine analyzeCodeLine(input int line, input int codeLine, input squeue parts);
        CodeLine res;
        string mnemonic = parts[0];
        string partsExt[4];
 
        res.line = line + 1;
        res.codeLine = codeLine + 1;
        res.parts = parts;
        res.error = NONE;
        
        //$display("Code line: %s", parts[0]);
        
        if (!isLetter(mnemonic[0])) begin
            res.error = SOME;
            return res;
        end

      foreach(partsExt[i])
        if (i < parts.size())
            partsExt[i] = parts[i];

        // TODO: get rid of this hack, define sys instructions like sys_call etc?
        if (mnemonic == "sys") begin
            mnemonic = {mnemonic,"_", parts[1]};
            partsExt[0] = mnemonic;
            partsExt[1] = "";
        end
            
        //  TMP_showArgs(partsExt);
        
        res.ins = TMP_getIns(partsExt);
        
        res.codeRef = TMP_getCodeRef(partsExt);
        //$display("%p", res);
        //$display("%h", res.ins);
        // get expected format 
        
        // check args conformance to format
        
        // if constant arg is a label, set it
        
        return res;
    endfunction

    function automatic DirectiveLine analyzeDirective(input int line, input int codeLine, input squeue parts);
        DirectiveLine res;
        
        res.line = line + 1;
        res.codeLine = codeLine + 1;
        res.parts = parts;
        res.error = NONE;
        
        if (parts[0] == "@proc") begin
            res.label = {"$", parts[1]};
            if (parts.size() > 2) begin
                $error("Too many arguments: %d", line + 1);
                res.error = SOME;
            end
        end
        else if (parts[0] == "@end") begin
            if (parts.size() > 1) begin
                $error("Too many arguments: %d", line + 1);
                res.error = SOME;
            end
        end
        else begin
            $error("Unknown directive: %d", line + 1);
            res.error = SOME;
        end

        return res;
    endfunction

endpackage
