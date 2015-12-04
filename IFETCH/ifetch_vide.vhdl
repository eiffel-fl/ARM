library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity IFetch is
	port(
	-- Icache interface
			if_adr			: out Std_Logic_Vector(31 downto 0) ; --
			if_adr_valid	: out Std_Logic; --

			ic_inst			: in Std_Logic_Vector(31 downto 0) ; --
			ic_stall			: in Std_Logic; --

	-- Decode interface
			if_adr_next		: out Std_Logic_Vector(31 downto 0) ; --
			if_ir				: out Std_Logic_Vector(31 downto 0) ; --
			if_ir_valid		: out Std_Logic; --

			dec_fetch_inst	: in Std_Logic; --
			dec_bpc			: in Std_Logic_Vector(31 downto 0) ; --
			dec_bpc_valid	: in Std_Logic; --

	-- global interface
			ck					: in Std_Logic;
			reset_n			: in Std_Logic;
			vdd				: in Std_Logic;
			vss				: in Std_Logic);
end IFetch;

----------------------------------------------------------------------

architecture Behavior of IFetch is

signal pc : unsigned(31 downto 0);
signal next_pc		: unsigned(31 downto 0);
signal fetch_en	: std_logic;


begin
-- Gestion du PC
	process (ck)
	begin
		if rising_edge(ck) then
			if ic_stall /= '1' then
				if reset_n = '0' then
					pc <= to_unsigned(0, 1);
				elsif dec_fetch_inst = '1' then
					if_adr_valid <= dec_fetch_inst;
					if dec_bpc_valid = '1' then
						pc <= unsigned(dec_bpc);
					else
						pc <= pc + 4;
					end if;
					if_adr <= Std_Logic_Vector(pc);
				end if;
			--else
				--cas de gel
				pc <= pc;
			end if;
		end if;
	end process;

-- Gestion de IR
	process (ck)
	begin
		if rising_edge(ck) then
			if ic_stall /= '1' then
				if_adr_next <= Std_Logic_Vector(pc);
				if_ir <= ic_inst;
			--else
				--cas de gel
			end if;
		end if;
	end process;
end Behavior;