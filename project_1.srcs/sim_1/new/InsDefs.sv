

package InsDefs;
    typedef logic[31:0] Word;


/*
type ProcMnemonic is ( -- one word instruction names, distinguishing different arg combinations
*/
    class MnemonicClass;
        typedef 
        enum {
            // set, mov, clr, nop, -- pseudoinstructions
            undef,
        

            //and_i,
            and_r,
            //or_i, 
            or_r,
            //xor_i, 
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
            sys_send 
            
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
            P_none,
            P_ja,
            P_jl,
            P_jz,
            P_jnz,
            P_intAlu,
            P_floatOp,
            P_intMem,
            P_floatMem,
            P_intAluImm,
            P_addI,
            P_addH, 
            
            P_intLoadW16,
            P_intStoreW16,
            P_floatLoadW16,
            P_floatStoreW16,
            
            P_sysMem,
            
            P_sysControl
        } Primary;
    
        typedef enum {
            S_none,
            
            // P_intAlu          
            S_intLogic, S_intArith, S_jumpReg, S_intMul,
             
            // P_intAluImm
            S_intShiftLogical, S_intShiftArith, S_intRotate,
             
            // P_floatOp
            S_floatMove,
             
            // P_intMem
            S_intLoadW, S_intStoreW,
             
            // P_floatMem
            S_floatLoadW, S_floatStoreW,
             
            // P_sysMem
            S_sysLoad, S_sysStore,
             
            // P_sysControl                 
            S_sysUndef,
            S_sysError,
            S_sysCall,
            S_sysRetE,
            S_sysRetI,
            S_sysHalt,
            S_sysSync,
            S_sysReplay,
            S_sysSend                 
         } Secondary;

        typedef enum {
            T_none, 

            T_intAnd, T_intOr, T_intXor,
        
            T_intAdd, T_intSub,  
        
            T_intMul, T_intMulHU, T_intMulHS,
            T_intDivU, T_intDivS,
            T_intRemU, T_intRemS,
        
            T_floatMove,
          
            T_jumpRegZ, T_jumpRegNZ
        } Ternary;

    //endclass;
    
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
        
            //and_i: ,
            "and_r": int2R,
            
            
            //or_i,
            "or_r": int2R,
            //xor_i,
            "xor_r": int2R,
            
            "add_i": intImm16,
            "add_h": intImm16,
            "add_r": int2R,
            "sub_r": int2R,
            
            
            "shl_i": intImm10, 
            
            //"shl_r": int2R, //-- direction defined by shift value, not opcode 
            
            "sha_i": intImm10,
            //"sha_r": int2R, //--   
            
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
            //ldi_r, //-- int
            "sti_i": intStore16,
            //sti_r,
            
            "ldf_i": floatLoad16,
            //ldf_r, //-- float
            "stf_i": floatStore16,
            //stf_r, 
            
            
            "lds": sysLoad, //-- load sys
            
            "sts": sysStore, //-- store sys
            
            "jz_i": jumpCond,
            "jz_r": int2R,
            "jnz_i": jumpCond,
            "jnz_r": int2R,
            "ja": jumpLong,
            "jl": jumpLink, //-- jump always, jump link
            /**/
            //"sys": noRegs //-- system operation
            
            
            "sys_retE": noRegs,
            "sys_retI": noRegs,
            "sys_halt": noRegs,
            "sys_sync": noRegs,
            "sys_replay": noRegs,
            "sys_error": noRegs,
            "sys_call": noRegs,
            "sys_send": noRegs
            
        };

//        function automatic string getFormatName(input string s);
//            typedef MnemonicClass::Mnemonic Mnem;
//            Mnem m;
//            for (Mnem mi = m.first(); 1; mi = mi.next()) begin
                
//                if (s == mi.name()) return formatMap[s].name();
                
//                if (mi == mi.last()) return "";
//            end  
//        endfunction

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
    
    
    typedef string string4[4];
    typedef Word Word4[4];
    
    function automatic void getParsing(input string s);
        InstructionFormat fmt = getFormat(s);
        $display("%s: %s ->  '%s', '%s', '%s'", s, fmt.name(), parsingMap[fmt][0], parsingMap[fmt][1], parsingMap[fmt][2]);

    endfunction;

    function automatic void TMP_showArgs(input string parts[]);
        InstructionFormat fmt = getFormat(parts[0]);
        InstructionDef def = getDef(parts[0]);
        
        string out[] = orderArgs(parts[1:3], parsingMap[fmt]);
        Word4 vals;
        Word res;
        
        $display("%s %s %s %s", out[0], out[1], out[2], out[3]);
        
        $display("%d", checkArgs(out[0:3], parsingMap[fmt]));// $display("Args error!");
        
        vals = parseArgs(out[0:3]);
        
        $display("%p", vals);
        
        res = fillArgs(vals, parsingMap[fmt], 0);
        $write("%b ", res);
        
        res = fillOp(res, def);
        $display("%b", res);
    endfunction;

   // class Opcodes;


        typedef InstructionDef DefMap[string];
        
        const DefMap defMap = '{
            "undef": '{P_none, S_none, T_none, O_undef},
        
            //and_i: ,
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
            
            
//                        P_intLoadW16,
//            P_intStoreW16,
//            P_floatLoadW16,
//            P_floatStoreW16,
            
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

//            //"sys": noRegs //-- system operation
            
            "sys_retE": '{P_sysControl, S_sysRetE, T_none, O_send},
            "sys_retI": '{P_sysControl, S_sysRetI, T_none, O_send},
            "sys_halt": '{P_sysControl, S_sysHalt, T_none, O_send},
            "sys_sync": '{P_sysControl, S_sysSync, T_none, O_send},
            "sys_replay": '{P_sysControl, S_sysReplay, T_none, O_send},
            "sys_error": '{P_sysControl, S_sysError, T_none, O_send},
            "sys_call": '{P_sysControl, S_sysCall, T_none, O_send},
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
        // d 0 1 2   a0 a1 a2 a3
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

    function automatic bit checkArgs(input string4 args, input string3 parsingMap);
        
//    asmForm: string(1 to 4);
//    decoding: string(1 to 5);
//    typeSpec: string(1 to 5);
        string typeSpec = parsingMap[2];    
        string decoding = parsingMap[1];
        
        case (typeSpec[0])
            "i": if (args[0][0] != "r" && decoding[0] != "0") return 2;
            "f": if (args[0][0] != "f" && decoding[0] != "0") return 3;
            default: if (args[0][0] != " ") return 4
                        ;
        endcase

        for (int i = 0; i <= 3; i++) begin
            case (typeSpec[i+1])
                "i": if (args[i][0] != "r" && decoding[i+1] != "0") return 5 + 10*i;
                "f": if (args[i][0] != "f" && decoding[i+1] != "0") return 6 + 10*i;
                "c": ;
                default: if (args[i][0] != " ") return 7 + 10*i;
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
                "-":      value = args[i].substr(1, args[i].len()-1).atoi();
                "0", "1", "2", "3", "4", "5", "6", "7", "8", "9": 
                          value = args[i].atoi();
                default: $fatal("Wrong arg");
            endcase
            
            res[i] = value;
        end
        
        return res;
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


//-- Insert args into word
//function TMP_fillArgs(args: IntArray; desc: NEW_FormatDescription; undefOffset: boolean) return Word is
    function automatic Word fillArgs(input Word4 args, input string3 parsingMap, input bit unknownOffset);
        string decoding = parsingMap[1];
        Word res = '0;
//    variable res: Word := (others => '0');
//begin
//    case desc.decoding(1) is
//        when 'a' =>
//            res(25 downto 21) := i2slv(args(0), 5);
//        when 'b' =>
//            res(20 downto 16) := i2slv(args(0), 5);
//        when 'c' =>
//            res(9 downto 5) := i2slv(args(0), 5);
//        when 'd' =>
//            res(4 downto 0) := i2slv(args(0), 5);                                                
//        when others =>
//    end case;
       res = fillField(res, decoding[0], args[0]);
       
       res = fillField(res, decoding[2], args[1]);
       res = fillField(res, decoding[3], args[2]);
       res = fillField(res, decoding[4], args[3]);
        
//    case desc.decoding(3) is -- Skip the comma!
//        when 'a' =>
//            res(25 downto 21) := i2slv(args(1), 5);
//        when 'b' =>
//            res(20 downto 16) := i2slv(args(1), 5);
//        when 'c' =>
//            res(9 downto 5) := i2slv(args(1), 5);
//        when 'd' =>
//            res(4 downto 0) := i2slv(args(1), 5);
//        when 'X' =>
//            res(9 downto 0) := i2slv(args(1), 10);
//        when 'H' =>
//            res(15 downto 0) := i2slv(args(1), 16);
//        when 'J' =>
//            res(20 downto 0) := i2slv(args(1), 21);
//        when 'L' =>
//            res(25 downto 0) := i2slv(args(1), 26);                                                            
//        when others =>
//    end case;
    
//    case desc.decoding(4) is
//        when 'a' =>
//            res(25 downto 21) := i2slv(args(2), 5);
//        when 'b' =>
//            res(20 downto 16) := i2slv(args(2), 5);
//        when 'c' =>
//            res(9 downto 5) := i2slv(args(2), 5);
//        when 'd' =>
//            res(4 downto 0) := i2slv(args(2), 5);
//        when 'X' =>     
//            res(9 downto 0) := i2slv(args(2), 10);
//            if undefOffset then
//                res(9 downto 0) := (others => 'U');
//            end if;
//        when 'H' =>
//            res(15 downto 0) := i2slv(args(2), 16);
//            if undefOffset then
//                res(15 downto 0) := (others => 'U');
//            end if;
//        when 'J' =>
//            res(20 downto 0) := i2slv(args(2), 21);
//            if undefOffset then
//                res(20 downto 0) := (others => 'U');
//            end if;
//        when 'L' =>
//            res(25 downto 0) := i2slv(args(2), 26);
//            if undefOffset then
//                res(25 downto 0) := (others => 'U');
//            end if;                                                            
//        when others =>
//    end case;
    
//    case desc.decoding(5) is
//        when 'a' =>
//            res(25 downto 21) := i2slv(args(3), 5);
//        when 'b' =>
//            res(20 downto 16) := i2slv(args(3), 5);
//        when 'c' =>
//            res(9 downto 5) := i2slv(args(3), 5);
//        when 'd' =>
//            res(4 downto 0) := i2slv(args(3), 5);
//        when 'X' =>
//            res(9 downto 0) := i2slv(args(3), 10);
//        when 'H' =>
//            res(15 downto 0) := i2slv(args(3), 16);
//        when 'J' =>
//            res(20 downto 0) := i2slv(args(3), 21);
//        when 'L' =>
//            res(25 downto 0) := i2slv(args(3), 26);                                                            
//        when others =>
//    end case;   

//    return res;
//end function;
        return res;
    endfunction
    
//function TMP_fillOp(w: Word; insDef: InstructionDefinition) return Word is
    function automatic Word fillOp(input Word w, input InstructionDef def);
        Word res = w;
        
        res[31:26] = def.p.num();
        if (def.s != S_none) res[15:10] = def.s.num();
        if (def.t != T_none) res[4:0] = def.t.num();
//    variable res: Word := w;
//begin
//    res(31 downto 26) := i2slv(insDef.i, 6);
    
//    if insDef.j = -1 then return res; end if;
    
//    res(15 downto 10) := i2slv(insDef.j, 6);

//    if insDef.k = -1 then return res; end if;

//    res(4 downto 0) := i2slv(insDef.k, 5);
    
//    return res;
//end function;
        return res;
    endfunction;



endpackage
