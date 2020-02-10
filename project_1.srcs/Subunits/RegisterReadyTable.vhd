
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;

use work.PipelineGeneral.all;
use work.LogicRenaming.all;


entity RegisterReadyTable is
	generic(
		WRITE_WIDTH: integer := 1;
		IS_FP: boolean := false
	);
	port(
		clk: in std_logic;
		reset: in std_logic;
		en: in std_logic;

		sendingToReserve: in std_logic;
		stageDataToReserve: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		newPhysDests: in PhysNameArray(0 to PIPE_WIDTH-1);
	
		stageDataReserved: in InstructionSlotArray(0 to PIPE_WIDTH-1);

		writingMask: in std_logic_vector(0 to WRITE_WIDTH-1);
		writingData: in InstructionStateArray(0 to WRITE_WIDTH-1);
		
		readyRegFlagsNext: out std_logic_vector(0 to 3*PIPE_WIDTH-1)
	);
end RegisterReadyTable;



architecture Behavioral of RegisterReadyTable is
		constant WIDTH: natural := WRITE_WIDTH;

		signal readyTableClearAllow: std_logic := '0';
		signal readyTableClearSel: std_logic_vector(0 to PIPE_WIDTH-1) := (others => '0');

		--signal readyRegsSig: std_logic_vector(0 to N_PHYSICAL_REGS-1) := (0 to 31 => '1', others=>'0');
			signal altMask: std_logic_vector(0 to WRITE_WIDTH-1) := (others => '0');
			signal altDests: PhysNameArray(0 to WRITE_WIDTH-1) := (others => (others => '0'));
			
function extractReadyRegBits(bits: std_logic_vector; data: InstructionSlotArray) return std_logic_vector is
	variable res: std_logic_vector(0 to 3*data'length-1) := (others => '0'); -- 31) := (others=>'0');
begin
	for i in 0 to data'length-1 loop
		res(3*i + 0) := bits(slv2u(data(i).ins.physicalArgSpec.args(0)(PHYS_REG_BITS-1 downto 0)));
		res(3*i + 1) := bits(slv2u(data(i).ins.physicalArgSpec.args(1)(PHYS_REG_BITS-1 downto 0)));
		res(3*i + 2) := bits(slv2u(data(i).ins.physicalArgSpec.args(2)(PHYS_REG_BITS-1 downto 0)));
	end loop;		
	return res;
end function;	

function initList(IS_FP: boolean) return std_logic_vector is
    variable res: std_logic_vector(0 to N_PHYSICAL_REGS-1) := (others => '0');
begin
    if IS_FP then
        res := (0 to 32 => '1', others => '0');        
    else
        res := (0 to 31 => '1', others => '0'); 
    end if;
    return res;
end function;


    subtype MatchVec is std_logic_vector(0 to 15);
    type MatchMat is array(0 to 3) of matchVec;
    type MaskLH is array(0 to 1) of std_logic_vector(0 to N_PHYSICAL_REGS-1);
   

function matchInputL(adr: PhysName; en0, en1: std_logic) return MatchVec is
        variable matchL, matchH: MatchVec := (others=> '0');
    begin
    
        -- Cut input addresses to 2x4 bit parts
        for i in 0 to 15 loop            
            --for j in 0 to 3 loop
                matchL(i) := bool2std((adr(3 downto 0) & en0 & en1) = (i2slv(i, 4) & "11"));
                matchH(i) := bool2std(adr(7 downto 4) = i2slv(i, 4));                
            --end loop;
        end loop;
    
    
    return matchL;
end function; 

function matchInputH(adr: PhysName) return MatchVec is
    variable matchL, matchH: MatchVec := (others=> '0');
begin

    -- Cut input addresses to 2x4 bit parts
    for i in 0 to 15 loop            
        --for j in 0 to 3 loop
            matchL(i) := bool2std(adr(3 downto 0) = i2slv(i, 4));
            matchH(i) := bool2std(adr(7 downto 4) = i2slv(i, 4));                
        --end loop;
    end loop;
    
    
    return matchH;
end function;


function getSelectionMaskL(matchL, matchH: MatchVec) return std_logic_vector is
    variable res: std_logic_vector(0 to N_PHYSICAL_REGS-1) := (others => '0');
    variable ind: natural := 0;
begin
    for ih in 0 to 15 loop
        for il in 0 to 15 loop
            ind := 16*ih + il;
            if ind >= N_PHYSICAL_REGS then
                next;
            end if;
            res(ind) := matchL(il);
            
        end loop;
    end loop;
    
    return res;
end function;

function getSelectionMaskH(matchL, matchH: MatchVec) return std_logic_vector is
    variable res: std_logic_vector(0 to N_PHYSICAL_REGS-1) := (others => '0');
    variable ind: natural := 0;
begin
    for ih in 0 to 15 loop
        for il in 0 to 15 loop
            ind := 16*ih + il;
            if ind >= N_PHYSICAL_REGS then
                next;
            end if;
            res(ind) := matchH(ih);
            
        end loop;
    end loop;
    
    return res;
end function;


function getSelMask2(adr: PhysName; en0, en1: std_logic) return MaskLH is
    variable res: MaskLH := (others => (others => '0'));
    variable matchL, matchH: MatchVec := (others => '0');
begin
    matchL := matchInputL(adr, en0, en1);
    matchH := matchInputH(adr);
    res(0) := getSelectionMaskL(matchL, matchH);
    res(1) := getSelectionMaskH(matchL, matchH);
    return res;
end function;



signal matchL, matchH: MatchVec := (others => '0');
signal selMask, selMask0, selMask1, selMask2, selMask3, selMaskS: MaskLH := (others => (others => '0'));

function nextReadyMask(sm0, sm1, sm2, sm3, sms: MaskLH; state: std_logic_vector) return std_logic_vector is
    variable res: std_logic_vector(0 to N_PHYSICAL_REGS-1) := (others => '0');
    variable tmp, tmp2, tmp3: std_logic := '0';
begin
    for i in 0 to N_PHYSICAL_REGS-1 loop
        tmp :=  (sm0(0)(i) and sm0(1)(i))
            or  (sm1(0)(i) and sm1(1)(i))
            or  (sm2(0)(i) and sm2(1)(i));
        
        tmp2 := tmp or (sm3(0)(i) and sm3(1)(i));
        tmp3 := sms(0)(i) and sms(1)(i);
        
        res(i) := tmp3 or (state(i) and not tmp2);
        
    end loop;

    return res;
end function;

begin

--    matchL <= matchInputL(newPhysDests(0));
--    matchH <= matchInputH(newPhysDests(0));
    selMask0 <= getSelMask2(newPhysDests(0), readyTableClearSel(0), readyTableClearAllow);
    selMask1 <= getSelMask2(newPhysDests(1), readyTableClearSel(1), readyTableClearAllow);
    selMask2 <= getSelMask2(newPhysDests(2), readyTableClearSel(2), readyTableClearAllow);
    selMask3 <= getSelMask2(newPhysDests(3), readyTableClearSel(3), readyTableClearAllow);
    selMaskS <= getSelMask2(altDests(0), altMask(0), '1');


		readyTableClearAllow <= sendingToReserve; -- for ready table
		readyTableClearSel <= (others => '1'); -- No need to prevent free yet-unallocated regs from clearing!	
		
		altMask(0) <= writingMask(0) and ((writingData(0).physicalArgSpec.floatDestSel and bool2std(IS_FP)) or ( writingData(0).physicalArgSpec.intDestSel and not bool2std(IS_FP)));
		altDests(0) <= writingData(0).physicalArgSpec.dest;-- <= getArrayPhysicalDests(writingData);
		
		      --newPhysDests <= getPhysicalDests(stageDataReserved);
		


        

		
		
		IMPL: block
			signal content, content2: std_logic_vector(0 to N_PHYSICAL_REGS-1) := initList(IS_FP);--(0 to 31 => '1', others => '0');
		begin
				SYNCHRONOUS: process(clk)
				begin
					if rising_edge(clk) then							
						if readyTableClearAllow = '1' then							
							for i in 0 to PIPE_WIDTH-1 loop								
								if readyTableClearSel(i) = '1' then
									-- clear
									content(slv2u(newPhysDests(i))) <= '0';						
								end if;
							end loop;
						end if;
						
						for i in 0 to altMask'length-1 loop
                            if altMask(i) = '1' then
                                -- set 
                                content(slv2u(altDests(i))) <= '1';
                            end if;
                        end loop;
                        
                            content2 <= nextReadyMask(selMask0, selMask1, selMask2, selMask3, selMaskS, content2);
                            content2(0) <= '1'; 								
					end if;
				end process;
				
			readyRegFlagsNext <= extractReadyRegBits(content2, stageDataReserved);				
		end block;
end Behavioral;
