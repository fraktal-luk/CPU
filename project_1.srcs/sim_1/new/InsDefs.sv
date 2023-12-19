

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
        

            and_i, and_r,
            or_i, or_r,
            xor_i, xor_r,
            
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
    
    class OpcodeClass;
        typedef enum {
            andI,
            orI,
            
            addI,
            subI,
            
            jz,
            jnz,
            
            j,
            jl,
            
            ld,
            st,
            
            ldf,
            stf,
            
            ext0, //-- E format: opcont is present and defines exact operation  
            ext1,
            ext2,
            fop, //-- Float operations
            
            undef
	   } Opcode;
	endclass

    class OpcontClass;
        typedef enum {
            none,
            
            //-- ext0:
            andR,
            orR,
            
            shlC,
            //--shrlC,
            shaC,
            
            addR,
            subR,
            
            muls,
            mulu,
            
            divs,
            divu,
            
            //-- ext1:
            //--mem
            store,
            load,
                
            storeFP,
            loadFP,
            //--
            jzR, //-- jumping with adr in register
            jnzR,	
            
            //-- ext2: 
            //-- system jumps
            retE,
            retI,
            halt,
            sync,
            replay,
            error,
            call,
            send,
            
            mfc,
            mtc,
            
            fmov,
            forr,
            
            undef
	   } Opcont;						

    endclass

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
            
            /*
            shl_i, shl_r, //-- direction defined by shift value, not opcode 
            sha_i, sha_r, //--   
            rot_i, rot_r,
            
            mult, 
            mulh_s, mulh_u,
            div_s, div_u,
            rem_s, rem_u,
            
            mov_f, or_f,   // -- Float operations
            */
            "ldi_i": intImm16,
            //ldi_r, //-- int
            "sti_i": intStore16,
            //sti_r,
            
            "ldf_i": floatLoad16,
            //ldf_r, //-- float
            "stf_i": floatStore16,
            //stf_r, 
            
            /*
            lds, //-- load sys
            
            sts, //-- store sys
            */
            "jz_i": jumpCond,
            "jz_r": int2R,
            "jnz_i": jumpCond,
            "jnz_r": int2R,
            "ja": jumpLong,
            "jl": jumpLink, //-- jump always, jump link
            /**/
            "sys": noRegs //-- system operation
            
            /*
            sys_retE,
            sys_retI,
            sys_halt,
            sys_sync,
            sys_replay,
            sys_error,
            sys_call,
            sys_send
            */
        };

        function automatic string getFormatName(input string s);
            typedef MnemonicClass::Mnemonic Mnem;
            Mnem m;
            for (Mnem mi = m.first(); 1; mi = mi.next()) begin
                
                if (s == mi.name()) return formatMap[s].name();
                
                if (mi == mi.last()) return "";
            end  
        endfunction

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
    
    
    function automatic void getParsing(input string s);
        InstructionFormat fmt = getFormat(s);
        $display("%s: %s ->  '%s', '%s', '%s'", s, fmt.name(), parsingMap[fmt][0], parsingMap[fmt][1], parsingMap[fmt][2]);
    endfunction;
    
endpackage
