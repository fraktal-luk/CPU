
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
            TMP_outTagsPre: out SmallNumberArray(0 to RENAME_W-1);
            TMP_outTags: out SmallNumberArray(0 to RENAME_W-1);

        accept: out std_logic;

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

    signal recoveryCounter: SmallNumber := sn(0);

        signal TMP_execEventD: std_logic := '0'; -- Must wait for flushing actual IQ

        signal fullMaskMirror: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');

    function getMirror(table: TableType) return std_logic_vector is
        variable res: std_logic_vector(0 to QUEUE_SIZE-1) := (others => '0');
    begin
        
        for bank in 0 to N_BANKS-1 loop
            for slot in 0 to BANK_SIZE-1 loop
               res(slot*N_BANKS + bank) := table(bank, slot);
            end loop;
        end loop;

        return res;
    end function;

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
                if freed(slot*N_BANKS + bank) = '1' then
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
                if iqUsed(slot*N_BANKS + bank) /= '1' then
                    table(bank, slot) <= '0';
                end if;
            end loop;
        end loop;
    end procedure;


    function allOf(v: std_logic_vector) return std_logic is
    begin
        for i in v'range loop
            if v(i) /= '1' then
                return '0';
            end if;
        end loop;
        return '1';
    end function;

    function anyOf(v: std_logic_vector) return std_logic is
    begin
        for i in v'range loop
            if v(i) = '1' then
                return '1';
            end if;
        end loop;
        return '0';
    end function;

begin

        fullMaskMirror <= getMirror(usedTable); -- for Viewing

    newInds <= getFreeIndices(usedTable);
    lanesReady <= hasFreeIndices(usedTable);

    process (clk)
    begin
        if rising_edge(clk) then
                TMP_execEventD <= evt.execEvent;

            reclaim(usedTable, iqFreed);

--            if (TMP_execEventD or evt.lateEvent) = '1' then
--                flush(usedTable, iqUsed, evt.lateEvent);
            if recoveryCounter = sn(1) then
                flush(usedTable, iqUsed, '0');
            elsif inReady = '1' then
                reserve(usedTable, inMask, newInds);
                    TMP_outTags <= newInds;
            end if;


            if (evt.execEvent or evt.lateEvent) = '1' then
                recoveryCounter <= sn(3);
            elsif recoveryCounter /= sn(0) then
                recoveryCounter <= addInt(recoveryCounter, -1);
            end if;
            recoveryCounter(SMALL_NUMBER_SIZE-1 downto 2) <= (others => '0');
        end if;
    end process;

        accept <= allOf(lanesReady);

        TMP_outTagsPre <= newInds;

end Behavioral;
