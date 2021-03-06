
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.ArchDefs.all;

package CoreConfig is

--function checkSynthesis return boolean;

constant CLEAR_DEBUG_INFO: boolean := true;
constant VIEW_ON: boolean := false;

   constant DEBUG_LOG_COMMITTED: boolean := false;

--constant SYNTH_CHECK: boolean := checkSynthesis;

constant LOG2_PIPE_WIDTH: natural := 0 + 2;
constant PIPE_WIDTH: positive := 2**LOG2_PIPE_WIDTH;
constant ALIGN_BITS: natural := LOG2_PIPE_WIDTH + 2;
constant PC_INC: Mword := (ALIGN_BITS => '1', others => '0');    

constant FETCH_WIDTH: positive := PIPE_WIDTH; 

constant IBUFFER_SIZE: positive := 4;
constant ROB_SIZE: positive := 8; 

constant USE_LINE_PREDICTOR: boolean := true;

constant CLEAR_DEST_SEL_ON_EMPTY: boolean := false; -- When op gets empty, sets *DestSel to 0

constant PHYS_REG_BITS: natural := 6 + LOG2_PIPE_WIDTH;

-- CAREFUL, TODO: compute it by log2 from number of phys regs
constant PHYS_REG_BITS_EFFECTIVE: natural := PHYS_REG_BITS - 1;

constant N_PHYSICAL_REGS: natural := 128;
constant N_PHYS: natural := N_PHYSICAL_REGS;
	
constant FREE_LIST_SIZE: positive := 256;

-- Optimize immediate field by keeping part of it in physical register field
constant IMM_AS_REG: boolean := true;


constant IQ_SIZE_I0: natural := 8;
constant IQ_SIZE_M0: natural := 8;
constant IQ_SIZE_F0: natural := 8;
constant IQ_SIZE_INT_SV: natural := 8;
constant IQ_SIZE_FLOAT_SV: natural := 8;

constant BQ_SIZE: natural := 8;
constant SQ_SIZE: natural := 8;
constant LQ_SIZE: natural := 8;

constant BQ_PTR_MASK: SmallNumber := i2slv(SQ_SIZE-1, SMALL_NUMBER_SIZE);
constant SQ_PTR_MASK: SmallNumber := i2slv(SQ_SIZE-1, SMALL_NUMBER_SIZE);
constant LQ_PTR_MASK: SmallNumber := i2slv(SQ_SIZE-1, SMALL_NUMBER_SIZE);

constant BQ_PTR_SIZE: natural := countOnes(BQ_PTR_MASK);
constant SQ_PTR_SIZE: natural := countOnes(SQ_PTR_MASK);
constant LQ_PTR_SIZE: natural := countOnes(LQ_PTR_MASK);

    constant TMP_PARAM_COMPRESS_RETURN: boolean := false;
    
    constant TMP_PARAM_NEW_DECODE: boolean := true;
    
end CoreConfig;



package body CoreConfig is

--function checkSynthesis return boolean is
--begin
--    -- synthesis translate_off
--    return true;
--    -- synthesis translate_on

--    assert checkSynthesis report "Synthesis started with incorrect settings" severity failure;
--    return false;
--end function;

end CoreConfig;
