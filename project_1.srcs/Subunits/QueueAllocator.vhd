
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use work.BasicTypes.all;
use work.Helpers.all;

use work.ArchDefs.all;
use work.InstructionState.all;
use work.CoreConfig.all;
use work.PipelineGeneral.all;
use work.LogicRenaming.all;



entity QueueAllocator is

    generic(
        QUEUE_SIZE: natural;
        BANK_SIZE: natural
    );

    port(
        clk: in std_logic;

        inReady: in std_logic;
        inMask: std_logic_vector;
        inGroup: in BufferEntryArray;

        outReady: out std_logic;
        outGroup: out InstructionSlotArray(0 to RENAME_W-1);

        iqUsed: in std_logic_vector(0 to QUEUE_SIZE-1);
        iqFreed: in std_logic_vector(0 to QUEUE_SIZE-1);

        evt: in EventState
    );

end QueueAllocator;


architecture Behavioral of QueueAllocator is
    alias N_BANKS is RENAME_W;
    type TableType is array(0 to N_BANKS-1, 0 to BANK_SIZE-1) of std_logic;
    signal usedTable: TableType := (others => (others => '0'));

    signal newInds: SmallNumberArray(0 to RENAME_W-1) := (others => (others => '0'));
    signal lanesReady: std_logic_vector(0 to RENAME_W-1) := (others => '0');

        signal TMP_execEventD: std_logic := '0'; -- Must wait for flushing actual IQ

    function getFreeIndices(table: TableType) return SmallNumberArray is
        variable res: SmallNumberArray(0 to RENAME_W-1) := (others => (others => '0'));
    begin
        for bank in 0 to N_BANKS-1 loop
            for slot in 0 to BANK_SIZE-1 loop
                if table(bank, slot) /= '1' then
                    res(bank) := sn(slot);
                    exit;
                end if;
            end loop;
        end loop;

        return res;
    end function;

    function hasFreeIndices(table: TableType) return std_logic_vector is
        variable res: std_logic_vector(0 to RENAME_W-1) := (others => '0');
    begin
        for bank in 0 to N_BANKS-1 loop
            for slot in 0 to BANK_SIZE-1 loop
                if table(bank, slot) /= '1' then
                    res(bank) := '1';
                end if;
            end loop;
        end loop;

        return res;
    end function;


    procedure reserve(signal table: inout TableType; usedLanes: std_logic_vector; newInds: SmallNumberArray) is
    begin
        for bank in 0 to N_BANKS-1 loop
            if usedLanes(bank) = '1' then
                table(bank, slv2u(newInds(bank))) <= '1';
            end if;
        end loop;        
    end procedure;

    procedure reclaim(signal table: inout TableType; freed: std_logic_vector) is
    begin
        for bank in 0 to N_BANKS-1 loop
            for slot in 0 to BANK_SIZE-1 loop
                if freed(bank*BANK_SIZE + slot) = '1' then
                    table(bank, slot) <= '0';
                end if;
            end loop;
        end loop;
    end procedure;

    procedure flush(signal table: inout TableType; iqUsed: std_logic_vector; lateEvent: std_logic) is
    begin
        if lateEvent = '1' then
            table <= (others => (others => '0'));
            return;
        end if;
    
        for bank in 0 to N_BANKS-1 loop
            for slot in 0 to BANK_SIZE-1 loop
                if iqUsed(bank*BANK_SIZE + slot) /= '1' then
                    table(bank, slot) <= '0';
                end if;
            end loop;
        end loop;
    end procedure;
begin

    newInds <= getFreeIndices(usedTable);
    lanesReady <= hasFreeIndices(usedTable);

    process (clk)
    begin
        if rising_edge(clk) then
                TMP_execEventD <= evt.execEvent;
        
            reclaim(usedTable, iqFreed);
            
            if (TMP_execEventD or evt.lateEvent) = '1' then
                flush(usedTable, iqUsed, evt.lateEvent);
            elsif inReady = '1' then
                reserve(usedTable, inMask, newInds);
            end if;

        end if;
    end process;

end Behavioral;
