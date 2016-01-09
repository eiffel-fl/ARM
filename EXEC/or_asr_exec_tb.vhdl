library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity or_asr_exec_tb IS --nom de l'entity utilisé pour ghdl -e
end or_asr_exec_tb;

architecture behavior OF or_asr_exec_tb IS
	component exec is
	port(
		--! Decode interface
		dec_op1 : in Std_Logic_Vector(31 downto 0); -- first alu input
		dec_op2 : in Std_Logic_Vector(31 downto 0); -- shifter input
		--dec_op3 : in Std_Logic_Vector(31 downto 0); -- data to MEM or MUL accu

		--! Decode to mem interface
		dec_mem_data : in Std_Logic_Vector(31 downto 0); -- data to MEM
		dec_mem_dest : in Std_Logic_Vector(5 downto 0);

		dec_mem_lw : in Std_Logic;
		dec_mem_lb : in Std_Logic;
		dec_mem_sw : in Std_Logic;
		dec_mem_sb : in Std_Logic;

		--! Shifter command
		dec_shift_lsl : in Std_Logic;
		dec_shift_lsr : in Std_Logic;
		dec_shift_asr : in Std_Logic;
		dec_shift_ror : in Std_Logic;
		dec_shift_rrx : in Std_Logic;
		dec_shift_val : in Std_Logic_Vector(4 downto 0);
		dec_cy : in Std_Logic; --carry d'entrée du shifter

		--! Alu operand selection
		dec_comp_op1 : in Std_Logic;
		dec_comp_op2 : in Std_Logic;
		dec_alu_cy : in Std_Logic;
		dec_zero_op1 : in Std_Logic; -- For mul if multiplier(0) = '0'

		--! Alu command
		dec_alu_add : in Std_Logic;
		dec_alu_and : in Std_Logic;
		dec_alu_or : in Std_Logic;
		dec_alu_xor : in Std_Logic;

		exe_res : out Std_Logic_Vector(31 downto 0);
		--exe_res_valid	: out Std_Logic;

		--! Exe bypass to decod
		exe_alu_res : out Std_Logic_Vector(31 downto 0);

		--! Flags
		exe_c : out Std_Logic;
		exe_v : out Std_Logic;
		exe_n : out Std_Logic;
		exe_z : out Std_Logic;

		--! Mem interface
		exe_mem_adr : out Std_Logic_Vector(31 downto 0); -- Alu res register
		exe_mem_data : out Std_Logic_Vector(31 downto 0);
		exe_mem_dest : out Std_Logic_Vector(5 downto 0);

		exe_mem_lw : out Std_Logic;
		exe_mem_lb : out Std_Logic;
		exe_mem_sw : out Std_Logic;
		exe_mem_sb : out Std_Logic;

		--! Global interface
		ck : in Std_Logic;
		reset_n : in Std_Logic;
		vss : in Std_Logic;
		vdd : in Std_Logic
	);
	end component;

	signal dec_op1 : Std_Logic_Vector(31 downto 0);
	signal dec_op2 : Std_Logic_Vector(31 downto 0);

	signal dec_mem_data : Std_Logic_Vector(31 downto 0);
	signal dec_mem_dest : Std_Logic_Vector(5 downto 0);

	signal dec_mem_lw : Std_Logic;
	signal dec_mem_lb : Std_Logic;
	signal dec_mem_sw : Std_Logic;
	signal dec_mem_sb : Std_Logic;

	signal dec_shift_lsl : Std_Logic;
	signal dec_shift_lsr : Std_Logic;
	signal dec_shift_asr : Std_Logic;
	signal dec_shift_ror : Std_Logic;
	signal dec_shift_rrx : Std_Logic;
	signal dec_shift_val : Std_Logic_Vector(4 downto 0);
	signal dec_cy : Std_Logic;

	signal dec_comp_op1 : Std_Logic;
	signal dec_comp_op2 : Std_Logic;
	signal dec_alu_cy : Std_Logic;
	signal dec_zero_op1 : Std_Logic;

	signal dec_alu_add : Std_Logic;
	signal dec_alu_and : Std_Logic;
	signal dec_alu_or :  Std_Logic;
	signal dec_alu_xor : Std_Logic;

	signal exe_res : Std_Logic_Vector(31 downto 0);

	signal exe_alu_res : Std_Logic_Vector(31 downto 0);

	signal exe_c : Std_Logic;
	signal exe_v : Std_Logic;
	signal exe_n : Std_Logic;
	signal exe_z : Std_Logic;

	signal exe_mem_adr :  Std_Logic_Vector(31 downto 0);
	signal exe_mem_data : Std_Logic_Vector(31 downto 0);
	signal exe_mem_dest : Std_Logic_Vector(5 downto 0);

	signal exe_mem_lw : Std_Logic;
	signal exe_mem_lb : Std_Logic;
	signal exe_mem_sw : Std_Logic;
	signal exe_mem_sb : Std_Logic;

	signal ck :      Std_Logic;
	signal reset_n : Std_Logic;
	signal vss :     Std_Logic;
	signal vdd :     Std_Logic;


----------------------------------------------------------------------
begin
	testb : exec
	port map(
		--! Decode interface
		dec_op1 => dec_op1,
		dec_op2 => dec_op2,
		--dec_op3 => --dec_op3,

		--! Decode to mem interface
		dec_mem_data => dec_mem_data,
		dec_mem_dest => dec_mem_dest,

		dec_mem_lw => dec_mem_lw,
		dec_mem_lb => dec_mem_lb,
		dec_mem_sw => dec_mem_sw,
		dec_mem_sb => dec_mem_sb,

		--! Shifter command
		dec_shift_lsl => dec_shift_lsl,
		dec_shift_lsr => dec_shift_lsr,
		dec_shift_asr => dec_shift_asr,
		dec_shift_ror => dec_shift_ror,
		dec_shift_rrx => dec_shift_rrx,
		dec_shift_val => dec_shift_val,
		dec_cy => dec_cy,

		--! Alu operand selection
		dec_comp_op1 => dec_comp_op1,
		dec_comp_op2 => dec_comp_op2,
		dec_alu_cy => dec_alu_cy,
		dec_zero_op1 => dec_zero_op1,

		--! Alu command
		dec_alu_add => dec_alu_add,
		dec_alu_and => dec_alu_and,
		dec_alu_or => dec_alu_or,
		dec_alu_xor => dec_alu_xor,

		exe_res => exe_res,
		--exe_res_valid => --exe_res_valid,

		--! Exe bypass to decod
		exe_alu_res => exe_alu_res,

		--! Flags
		exe_c => exe_c,
		exe_v => exe_v,
		exe_n => exe_n,
		exe_z => exe_z,

		--! Mem interface
		exe_mem_adr => exe_mem_adr,
		exe_mem_data => exe_mem_data,
		exe_mem_dest => exe_mem_dest,

		exe_mem_lw => exe_mem_lw,
		exe_mem_lb => exe_mem_lb,
		exe_mem_sw => exe_mem_sw,
		exe_mem_sb => exe_mem_sb,

		--! Global interface
		ck => ck,
		reset_n => reset_n,
		vss => vss,
		vdd => vdd
	);

	testBench : process
	begin
		ck <= '0';
		wait for 2 ns;

		reset_n <= '0';
		ck <= '1';
		wait for 2 ns;

		ck <= '0';
		wait for 2 ns;

		dec_shift_lsl <= '0';
		dec_shift_lsr <= '0';
		dec_shift_asr <= '1';
		dec_shift_ror <= '0';
		dec_shift_rrx <= '0';
		dec_shift_val <= "00001";

		dec_alu_add <= '0';
		dec_alu_and <= '0';
		dec_alu_or <= '1';
		dec_alu_xor <= '0';

		dec_alu_cy <= '0';

		dec_comp_op1 <= '0';
		dec_comp_op2 <= '0';

		dec_op1 <= x"01000001";
		dec_op2 <= "1000" & x"0000002";
		ck <= '1';

		wait for 2 ns;
		ck <= '0';
		wait for 2 ns;
		ck <= '1';
		wait for 2 ns;
		ck <= '0';
		wait for 2 ns;
		ck <= '1';
		wait;
	end process;
end behavior;