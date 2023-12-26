

package InsDefs;
    typedef logic[31:0] Word;

    class MnemonicClass;
        typedef 
        enum {
            // set, mov, clr, nop, -- pseudoinstructions
        

            and_r,
            or_r,
            xor_r,
            
            add_i,
            add_h,
            add_r,
            sub_r,
            
            shl_i, shl_r, //-- direction defined by shift value, not opcode 
            sha_i, sha_r, //--   
            rot_i, rot_r,
            
            mult, 
            mulh_s, mulh_u,
            div_s, div_u,
            rem_s, rem_u,
            
            mov_f, or_f,   // -- Float operations
            
            ldi_i, ldi_r, //-- int
            sti_i, sti_r,
            
            ldf_i, ldf_r, //-- float
            stf_i, stf_r, 
            
            lds, //-- load sys
            
            sts, //-- store sys
            
            jz_i, jz_r, jnz_i, jnz_r,
            ja, jl, //-- jump always, jump link
            
            sys, //-- system operation
            
            sys_retE,
            sys_retI,
            sys_halt,
            sys_sync,
            sys_replay,
            sys_error,
            sys_call,
            sys_send,
            
            undef
        } Mnemonic;
    endclass;


    typedef enum {
        none,
        noRegs,
            jumpLong, jumpLink, jumpCond,
            intImm16, intImm10,
        intStore16, intStore10, floatLoad10, floatLoad16, floatStore10, floatStore16,
        sysLoad, sysStore,
            int1R, int2R, int3R, 
            float1R, float2R, float3R,
            floatToInt, intToFloat
    } InstructionFormat;


    typedef enum {
        P_intAlu = 0,
        P_floatOp = 1,
        P_intMem = 2,
        P_floatMem = 3,
        P_sysMem = 4,
        P_intAluImm = 5,
        
        P_sysControl = 7,

        P_ja = 8,
        P_jl = 9,
        P_jz = 10,
        P_jnz = 11,
 
        P_addI = 16,
        P_addH = 17, 
        
        P_intLoadW16 = 20,
        P_intStoreW16 = 21,
        P_floatLoadW16 = 22,
        P_floatStoreW16 = 23,
        
        

        P_none = -1
    } Primary;



    typedef enum {
        
        // P_intAlu          
        S_intLogic = 0 + 64*P_intAlu,
        S_intArith = 1 + 64*P_intAlu,
        S_jumpReg = 2 + 64*P_intAlu,
        S_intMul = 3 + 64*P_intAlu,
         
        // P_intAluImm
        S_intShiftLogical = 0 + 64*P_intAluImm,
        S_intShiftArith   = 1 + 64*P_intAluImm,
        S_intRotate       = 2 + 64*P_intAluImm, 
         
        // P_floatOp
        S_floatMove  = 0 + 64*P_floatOp,
         
        // P_intMem
        //S_intLoadW,
        //S_intStoreW,
         
        // P_floatMem
        //S_floatLoadW,
        //S_floatStoreW,
         
        // P_sysMem
        S_sysLoad   = 0 + 64*P_sysMem,
        S_sysStore  = 32+ 64*P_sysMem,
         
        // P_sysControl                 
        S_sysUndef   = 0 + 64*P_sysControl,
        S_sysError = 1 + 64*P_sysControl,
        S_sysCall = 2 + 64*P_sysControl,
        S_sysSync = 3 + 64*P_sysControl,
        S_sysReplay = 4 + 64*P_sysControl,
        
        S_sysHalt  = 5 + 64*P_sysControl,
        S_sysSend = 6 + 64*P_sysControl,
        S_sysRetE = 7 + 64*P_sysControl,
        S_sysRetI = 8 + 64*P_sysControl,
           
        S_none = -1               
     } Secondary;



    typedef enum {
        T_intAnd = 0 + 32*S_intLogic,
        T_intOr  = 1 + 32*S_intLogic,
        T_intXor = 2 + 32*S_intLogic,
    
        T_intAdd = 0 + 32*S_intArith,
        T_intSub = 1 + 32*S_intArith,  
    
        T_intMul = 0 + 32*S_intMul,
        T_intMulHU = 1 + 32*S_intMul,
        T_intMulHS = 2 + 32*S_intMul,
        T_intDivU = 8 + 32*S_intMul,
        T_intDivS = 9 + 32*S_intMul,
        T_intRemU = 10 + 32*S_intMul,
        T_intRemS = 11 + 32*S_intMul,
    
        T_floatMove = 0  + 32*S_floatMove,
      
        T_jumpRegZ = 0  + 32*S_jumpReg,
        T_jumpRegNZ = 1  + 32*S_jumpReg,
        
        T_none = -1

    } Ternary;

    typedef enum {
        O_undef,
        O_call,
        O_sync,
        O_retE,
        O_retI,
        O_replay,
        O_halt,
        O_send,
        
        O_jump,
        
        O_intAnd, O_intOr, O_intXor,
        O_intAdd, O_intSub,
        O_intAddH,
        O_intMul, O_intMulHU, O_intMulHS,
        O_intDivU, O_intDivS,
        O_intRemU, O_intRemS,
        
        O_intShiftLogical, O_intShiftArith, O_intRotate,
        
        O_floatMove,
        
        O_intLoadW, O_intLoadD,
        O_intStoreW, O_intStoreD,
        O_floatLoadW, O_floatStoreW,
        O_sysLoad, O_sysStore
    } Operation;


    typedef struct {
        
        Primary p;
        Secondary s;
        Ternary t;
        Operation o;
    } InstructionDef;


    typedef InstructionFormat FormatMap[string];
    
    const FormatMap formatMap = '{
        "undef": none,
    
        "and_r": int2R,
        
        "or_r": int2R,
        "xor_r": int2R,
        
        "add_i": intImm16,
        "add_h": intImm16,
        "add_r": int2R,
        "sub_r": int2R,
        
        "shl_i": intImm10, 
        
        "sha_i": intImm10,
        
        "rot_i": intImm10, 
        "rot_r": int2R,
        
        "mult": int2R, 
        "mulh_s": int2R,
        "mulh_u": int2R,
        "div_s": int2R,
        "div_u": int2R,
        "rem_s": int2R,
        "rem_u": int2R,
        
        "mov_f": float1R,
        "or_f": float2R,   // -- Float operations
        
        "ldi_i": intImm16,
        "sti_i": intStore16,
        
        "ldf_i": floatLoad16,
        "stf_i": floatStore16,

        "lds": sysLoad, //-- load sys
        "sts": sysStore, //-- store sys
        
        "jz_i": jumpCond,
        "jz_r": int2R,
        "jnz_i": jumpCond,
        "jnz_r": int2R,
        "ja": jumpLong,
        "jl": jumpLink, //-- jump always, jump link        
        
        "sys_retE": noRegs,
        "sys_retI": noRegs,
        "sys_halt": noRegs,
        "sys_sync": noRegs,
        "sys_replay": noRegs,
        "sys_error": noRegs,
        "sys_call": noRegs,
        "sys_send": noRegs 
    };


    function automatic InstructionFormat getFormat(input string s);
        typedef MnemonicClass::Mnemonic Mnem;
        Mnem m;
        for (Mnem mi = m.first(); 1; mi = mi.next()) begin
            if (s == mi.name()) return formatMap[s];
            if (mi == mi.last()) return none;
        end  
    endfunction


    typedef string string3[3];

    const string3 parsingMap[InstructionFormat] = '{
        none: '{"    ", "0,000", "0,000"},
    
        noRegs :       '{"    ", "0,000", "0,000"},
    
        jumpLong :     '{"1   ", "0,0L0", "i,ic0"},
        jumpLink :     '{"d1  ", "a,0J0", "i,ic0"},
        jumpCond :     '{"01  ", "0,aJ0", "i,ic0"},
        
        intImm16 :     '{"d01 ", "a,bH0", "i,ic0"},
        intImm10 :     '{"d01 ", "a,bX0", "i,ic0"},
        
        intStore16 :   '{"201 ", "0,bHa", "i,ici"},
        intStore10 :   '{"201 ", "0,bXa", "i,ici"},
        
        floatLoad16 :  '{"d01 ", "a,bH0", "f,ic0"},
        floatLoad10 :  '{"d01 ", "a,bX0", "f,ic0"},
        
        floatStore16 : '{"201 ", "0,bHa", "i,icf"},
        floatStore10 : '{"201 ", "0,bXa", "i,icf"},
    
        sysLoad :      '{"d01 ", "a,bX0", "i,ic0"},

        sysStore :     '{"201 ", "0,bXa", "0,ici"},
    
        int3R :        '{"d012", "a,bcd", "i,iii"},
        int2R :        '{"d01 ", "a,bc0", "i,ii0"},
        int1R :        '{"d0  ", "a,b00", "i,i00"},
    
        float3R :      '{"d012", "a,bcd", "f,fff"},
        float2R :      '{"d01 ", "a,bc0", "f,ff0"},
        float1R :      '{"d0  ", "a,b00", "f,f00"},
        
        floatToInt :   '{"d0  ", "a,b00", "i,f00"},
        intToFloat :   '{"d0  ", "a,b00", "f,i00"}
    };

    //typedef string string3[3];
    typedef string string4[4];
    typedef Word Word3[3];
    typedef Word Word4[4];

    typedef struct {
        bit ref21 = 0;
        bit ref26 = 0;
        string label = "";
    } CodeRef;

    function automatic void TMP_showArgs(input string parts[]);
        InstructionFormat fmt = getFormat(parts[0]);
        InstructionDef def = getDef(parts[0]);
        
        string out[] = orderArgs(parts[1:3], parsingMap[fmt]);
        Word4 vals = parseArgs(out[0:3]);
        Word res = fillArgs(vals, parsingMap[fmt], 0);
        res = fillOp(res, def);
    endfunction;

    function automatic Word TMP_getIns(input string parts[]);
        InstructionFormat fmt = getFormat(parts[0]);
        InstructionDef def = getDef(parts[0]);
        
        string args[] = orderArgs(parts[1:3], parsingMap[fmt]);
        Word4 vals;
        Word res;

        if ( checkArgs(args[0:3], parsingMap[fmt]) != 1) $error("Incorrect args");
                    
        vals = parseArgs(args[0:3]);
       
        res = fillArgs(vals, parsingMap[fmt], 0);            
        res = fillOp(res, def);
        
        return res;
    endfunction;

    function automatic CodeRef TMP_getCodeRef(input string parts[]);
        InstructionFormat fmt = getFormat(parts[0]);
        InstructionDef def = getDef(parts[0]);
        
        string args[] = orderArgs(parts[1:3], parsingMap[fmt]);
        Word4 vals;
        CodeRef res;

        res.label = parseLabel(args[0:3], parsingMap[fmt][1]);
        if (res.label.len() != 0)
            case (def.p)
                P_ja: res.ref26 = 1;
                P_jl, P_jz, P_jnz: res.ref21 = 1;
                default: ;
            endcase            
        return res; 
    endfunction;


    typedef InstructionDef DefMap[string];
    
    const DefMap defMap = '{
        "undef": '{P_none, S_none, T_none, O_undef},
    
        "and_r":  '{P_intAlu, S_intLogic, T_intAnd, O_intAnd}, //int2R,
        "or_r":   '{P_intAlu, S_intLogic, T_intOr, O_intOr}, //int2R,
        "xor_r":  '{P_intAlu, S_intLogic, T_intXor, O_intXor}, //int2R,
        
        "add_i": '{P_addI, S_none, T_none, O_intAdd},//intImm16,
        "add_h": '{P_addH, S_none, T_none, O_intAddH},//intImm16,
        "add_r": '{P_intAlu, S_intArith, T_intAdd, O_intAdd},//int2R,
        "sub_r": '{P_intAlu, S_intArith, T_intSub, O_intSub},//int2R,
        
        "shl_i": '{P_intAluImm, S_intShiftLogical, T_none, O_intShiftLogical},//intImm10, 
        "sha_i": '{P_intAluImm, S_intShiftArith, T_none, O_intShiftArith},//intImm10, 
        "rot_i": '{P_intAluImm, S_intRotate, T_none, O_intRotate},//intImm10, 
        
        "mult":   '{P_intAlu, S_intMul, T_intMul, O_intMul},//int2R, 
        "mulh_s": '{P_intAlu, S_intMul, T_intMulHU, O_intMulHU},//int2R, 
        "mulh_u": '{P_intAlu, S_intMul, T_intMulHS, O_intMulHS},//int2R, 
        "div_s":  '{P_intAlu, S_intMul, T_intDivS, O_intDivS},//int2R, 
        "div_u":  '{P_intAlu, S_intMul, T_intDivU, O_intDivU},//int2R, 
        "rem_s":  '{P_intAlu, S_intMul, T_intRemS, O_intRemS},//int2R, 
        "rem_u":  '{P_intAlu, S_intMul, T_intRemU, O_intRemU},//int2R, 
        
        "mov_f":  '{P_floatOp, S_floatMove, T_floatMove, O_floatMove},//float1R,
//            "or_f": float2R,   // -- Float operations
        
        "ldi_i": '{P_intLoadW16,  S_none, T_none, O_intLoadW},//intImm16,
        "sti_i": '{P_intStoreW16, S_none, T_none, O_intStoreW},//intStore16,
        
        "ldf_i": '{P_floatLoadW16,  S_none, T_none, O_floatLoadW},//floatLoad16,
        "stf_i": '{P_floatStoreW16,  S_none, T_none, O_floatStoreW},//floatStore16,
//            //stf_r, 
        
        "lds": '{P_sysMem,  S_sysLoad, T_none, O_sysLoad},//sysLoad, //-- load sys
        "sts": '{P_sysMem,  S_sysStore, T_none, O_sysStore},//sysStore, //-- store sys
        
        "jz_i": '{P_jz, S_none, T_none, O_jump},//jumpCond,
        "jz_r": '{P_intAlu, S_jumpReg, T_jumpRegZ, O_jump},//int2R,
        "jnz_i": '{P_jnz, S_none, T_none, O_jump},//jumpCond,
        "jnz_r": '{P_intAlu, S_jumpReg, T_jumpRegNZ, O_jump},//int2R,
        "ja": '{P_ja, S_none, T_none, O_jump},//,//jumpLong,
        "jl": '{P_jl, S_none, T_none, O_jump},//jumpLink, //-- jump always, jump link
        
        "sys_retE": '{P_sysControl, S_sysRetE, T_none, O_retE},
        "sys_retI": '{P_sysControl, S_sysRetI, T_none, O_retI},
        "sys_halt": '{P_sysControl, S_sysHalt, T_none, O_halt},
        "sys_sync": '{P_sysControl, S_sysSync, T_none, O_sync},
        "sys_replay": '{P_sysControl, S_sysReplay, T_none, O_replay},
        "sys_error": '{P_sysControl, S_sysError, T_none, O_undef},
        "sys_call": '{P_sysControl, S_sysCall, T_none, O_call},
        "sys_send": '{P_sysControl, S_sysSend, T_none, O_send}
        
    }; 


    function automatic InstructionDef getDef(input string s);
        typedef MnemonicClass::Mnemonic Mnem;
        Mnem m;
        for (Mnem mi = m.first(); 1; mi = mi.next()) begin
            if (s == mi.name()) return defMap[s];
            if (mi == mi.last()) return '{P_none, S_none, T_none, O_undef};
        end  
    endfunction


    function automatic string4 orderArgs(input string args[], input string3 parsingMap);
        string4 out;
        int index;
        string spec = parsingMap[0];
        
        foreach (spec[i]) begin
            case (spec[i])
                "d": index = 0;
                "0": index = 1;
                "1": index = 2;
                "2": index = 3;
                " ": continue;
                default: $fatal("Wrong format definition");
            endcase
            out[index] = args[i];
        end
        
        return out;
    endfunction

    function automatic int checkArgs(input string4 args, input string3 parsingMap);
        string typeSpec = parsingMap[2];    
        string decoding = parsingMap[1];
        
        case (typeSpec[0])
            "i": if (args[0][0] != "r" && decoding[0] != "0") return 0;
            "f": if (args[0][0] != "f" && decoding[0] != "0") return 0;
            default: if (args[0][0] != "") return 0;
        endcase

        for (int i = 1; i <= 3; i++) begin
            case (typeSpec[i+1])
                "i": if (args[i][0] != "r" && decoding[i+1] != "0") return 0;
                "f": if (args[i][0] != "f" && decoding[i+1] != "0") return 0;
                "c": ;
                "0": if (args[i].len() != 0) return 0;
                default:   
                   begin
                       $error("arg spec: [%s]", typeSpec[i+1]);
                       if (args[i][0] != " ") return 0;
                   end
            endcase
        end
        
        return 1;
    endfunction


    function automatic Word4 parseArgs(input string4 args);
        Word4 res;
        integer value = 'x;

        foreach(args[i]) begin
            if (args[i].len() == 0) begin
               res[i] = 'x;
               continue;
            end
        
            case (args[i][0])
                "$", "@": value = 'x;
                "f":      value = args[i].substr(1, args[i].len()-1).atoi();
                "r":      value = args[i].substr(1, args[i].len()-1).atoi();
                "-":      value = args[i].substr(0, args[i].len()-1).atoi();
                "0", "1", "2", "3", "4", "5", "6", "7", "8", "9": 
                          value = args[i].atoi();
                default: $fatal("Wrong arg");
            endcase
            
            res[i] = value;
        end
        
        return res;
    endfunction


    function automatic string parseLabel(input string4 args, input string decoding);
        for (int i = 1; i < 4; i++) begin
            if (decoding[i+1] inside {"L", "J"} && args[i][0] == "$") return args[i];
        end
        
        return "";
    endfunction

    
    function automatic Word fillField(input Word w, input logic[7:0] field, input Word value);
        Word res = w;
        case (field)
            "a": res[25:21] = value[4:0];
            "b": res[20:16] = value[4:0];
            "c": res[9:5] = value[4:0];
            "d": res[4:0] = value[4:0];
            "X": res[4:0] = value[4:0];
            "H": res[15:0] = value[15:0];
            "J": res[20:0] = value[20:0];
            "L": res[25:0] = value[25:0];
            " ", "0": ;
            default: $fatal("Invalid field: %s", field);
        endcase
        
        return res;
    endfunction


    function automatic Word fillArgs(input Word4 args, input string3 parsingMap, input bit unknownOffset);
        string decoding = parsingMap[1];
        Word res = '0;

        res = fillField(res, decoding[0], args[0]);
       
        res = fillField(res, decoding[2], args[1]);
        res = fillField(res, decoding[3], args[2]);
        res = fillField(res, decoding[4], args[3]);

        return res;
    endfunction
    
    function automatic Word fillOp(input Word w, input InstructionDef def);
        Word res = w;
        res[31:26] = def.p;
        if (def.s != S_none) res[15:10] = def.s;
        if (def.t != T_none) res[4:0] = def.t;
        return res;
    endfunction;

    function automatic Primary toPrimary(input int n);
        Primary p;
        p = p.first();
        
        forever begin
            if (p == n) return p; 
            
            if (p == p.last()) break;
            p = p.next();
        end
        
        return P_none;
    endfunction;

    function automatic Secondary toSecondary(input int n, input Primary p);
        Secondary s;
        s = s.first();
        
        forever begin
            if (s == 64*p + n) return s; 
            
            if (s == s.last()) break;
            s = s.next();
        end
        
        return S_none;
     endfunction;
    
    function automatic Ternary toTernary(input int n, input Primary p, input Secondary s);
        Ternary t;
        t = t.first();
        
        forever begin
            if (t == 64*32*p + 32*s + n) return t;
            
            if (t == t.last()) break;
            t = t.next();
        end
        
        return T_none;
    endfunction;

    function automatic matchDefinition(input InstructionDef pattern, candidate);
        return (candidate.p == pattern.p) && (candidate.s inside {S_none, pattern.s}) && (candidate.t inside {T_none, pattern.t});
    endfunction

    function automatic string decodeMnem(input Word w);
        Primary p = toPrimary(w[31:26]);
        Secondary s = toSecondary(w[15:10], p);
        Ternary t = toTernary(w[4:0], p, s);
        
        InstructionDef def = '{p, s, t, O_undef};

        string found[$] = defMap.find_index with(matchDefinition(def, item));
        string name;
        
        if (found.size() == 0) return "undef";        
        if (found.size() != 1) $error("No single definition, %d", found.size());
        name = found[0];

        return name;               
    endfunction

    typedef struct {
        string mnemonic;
        Word encoding;
        InstructionFormat fmt;
        InstructionDef def;
        int dest;
        int sources[3];
    } AbstractInstruction;


    function automatic AbstractInstruction decodeAbstract(input Word w);
        string s = decodeMnem(w);
        AbstractInstruction res;
        InstructionFormat f = getFormat(s);
        InstructionDef d = getDef(s);
        
        string3 fmtSpec = parsingMap[f];
        
        string typeSpec = fmtSpec[2];    
        string decoding = fmtSpec[1];
        string asmForm = fmtSpec[0];

        int qa = w[25:21];        
        int qb = w[20:16];        
        int qc = w[9:5];        
        int qd = w[4:0];        

        int dest;
        int sources[3];

        case (decoding[0])
            "a": dest = qa;
            "b": dest = qb;
            "c": dest = qc;
            "d": dest = qd;
            "0", " ": ;
            default: $fatal("Wrong dest specifier");
        endcase

        foreach(sources[i])
            case (decoding[i+2])
                "a": sources[i] = qa;
                "b": sources[i] = qb;
                "c": sources[i] = qc;
                "d": sources[i] = qd;
                "X": sources[i] = $signed(w[9:0]);
                "H": sources[i] = $signed(w[15:0]);
                "J": sources[i] = $signed(w[20:0]);
                "L": sources[i] = $signed(w[25:0]);
                "0", " ": ;
                default: $fatal("Wrong source specifier");
            endcase
        
        res.mnemonic = s;
        res.encoding = w;
        res.fmt = f;
        res.def = d;
        res.dest = dest;
        res.sources = sources;
        
        return res; 
    endfunction

    
    function automatic string ins2str(input AbstractInstruction ins);
        string s;
        int dest;
        int sources[3];
        string destStr;
        string sourcesStr[3];

        string3 fmtSpec = parsingMap[ins.fmt];
        
        string typeSpec = fmtSpec[2];    
        string decoding = fmtSpec[1];
        string asmForm = fmtSpec[0];

        dest = ins.dest;
        sources = ins.sources;
               
        case (typeSpec[0])
            "i": $swrite(destStr, "r%0d", dest);
            "f": $swrite(destStr, "f%0d", dest);
            "0": destStr = "";
            default: $fatal("Wrong dest specifier");
        endcase

        foreach(sources[i])
            case (typeSpec[i+2])
                "i": $swrite(sourcesStr[i], "r%0d", sources[i]);
                "f": $swrite(sourcesStr[i], "f%0d", sources[i]);
                "c": $swrite(sourcesStr[i], "%0d", sources[i]);
                "0": sourcesStr[i] = "";
                default: $fatal("Wrong source specifier");
            endcase

        s = {ins.mnemonic, "          "};
        s = s.substr(0,9);

        foreach (asmForm[i]) begin
            case (asmForm[i])
                "d": s = {s, " ", destStr};
                "0": s = {s, " ", sourcesStr[0]};
                "1": s = {s, " ", sourcesStr[1]};
                "2": s = {s, " ", sourcesStr[2]};
                " ": ;
                default: $fatal("Wrong asm syntax description");
            endcase;
          
            if (i == 3 || asmForm[i+1] == " ") break;

            s = {s, ","};
        end
        
        return s;
    endfunction

    function automatic string TMP_disasm(input Word w);
        AbstractInstruction absIns = decodeAbstract(w);        
        return ins2str(absIns);
    endfunction;

endpackage
