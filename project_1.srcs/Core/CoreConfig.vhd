
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.ArchDefs.all;

package CoreConfig is

constant CLEAR_DEBUG_INFO: boolean := true;
constant VIEW_ON: boolean := false;

constant LOG2_PIPE_WIDTH: natural := 0 + 2;
constant PIPE_WIDTH: positive := 2**LOG2_PIPE_WIDTH;
constant ALIGN_BITS: natural := LOG2_PIPE_WIDTH + 2;
constant PC_INC: Mword := (ALIGN_BITS => '1', others => '0');    

constant FETCH_WIDTH: positive := PIPE_WIDTH; 

constant IBUFFER_SIZE: positive := 5*FETCH_WIDTH;
constant ROB_SIZE: positive := 8; 

constant USE_LINE_PREDICTOR: boolean := true;

constant CLEAR_DEST_SEL_ON_EMPTY: boolean := false; -- When op gets empty, sets *DestSel to 0

constant PHYS_REG_BITS: natural := 6 + LOG2_PIPE_WIDTH;

-- CAREFUL, TODO: compute it by log2 from number of phys regs
constant PHYS_REG_BITS_EFFECTIVE: natural := PHYS_REG_BITS - 1;

constant N_PHYSICAL_REGS: natural := --64 * PIPE_WIDTH;
                                        128;
constant N_PHYS: natural := N_PHYSICAL_REGS;
	
constant FREE_LIST_SIZE: positive := --N_PHYSICAL_REGS;
                                        256;

-- Optimize immediate field by keeping part of it in physical register field
constant IMM_AS_REG: boolean := true;

    constant TMP_PARAM_COMPRESS_RETURN: boolean := false;
    constant TMP_PARAM_COMPRESS_PTRS: boolean := false;
    constant TMP_PARAM_SIMPLIFY_ISSUE: boolean := true;
end CoreConfig;



package body CoreConfig is


end CoreConfig;
