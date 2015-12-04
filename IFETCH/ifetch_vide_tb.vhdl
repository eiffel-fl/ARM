library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Ifetch_tb IS
end Ifetch_tb;

architecture behavior OF Ifetch_tb IS
	component fetch is
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
	end component;

signal pc : unsigned(31 downto 0);
signal next_pc		: unsigned(31 downto 0);
signal fetch_en	: std_logic;

----------------------------------------------------------------------

begin
	testb : fetch
	port map(
		-- Icache interface
		if_adr => if_adr,
		if_adr_valid => if_adr_valid,

		ic_inst => ic_inst,
		ic_stall => ic_stall,

		-- Decode interface
		if_adr_next => if_adr_next,
		if_ir => if_ir,
		if_ir_valid => if_ir_valid,

		dec_fetch_inst => dec_fetch_inst,
		dec_bpc => dec_bpc,
		dec_bpc_valid => dec_bpc_valid,

-- global interface
		ck => ck,
		reset_n => reset_n,
		vdd => vdd,
		vss => vss
	);

	testBench : process
	--VARIABLE i : INTEGER := 0;
	begin
-- 		a <= X"A";
-- 		b <= X"B";
-- 		sel <= '0';
-- 		ck <= '0';
-- 		WAIT FOR 2 ns;
-- 		ck <= '1';
-- 		WAIT FOR 2 ns;
-- 		ck <= '0';
-- 		WAIT;
	end process;
end behavior;