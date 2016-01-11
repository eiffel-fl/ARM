library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sw_F0_decod_tb IS --nom de l'entity utilisé pour ghdl -e
end sw_F0_decod_tb;

architecture behavior OF sw_F0_decod_tb IS
	component decod is
	port(
		-- Exec  operands
			dec_op1			: out Std_Logic_Vector(31 downto 0); -- first alu input
			dec_op2			: out Std_Logic_Vector(31 downto 0); -- shifter input

	-- Decod to mem via exec
			dec_mem_data	: out Std_Logic_Vector(31 downto 0); -- data to MEM
			dec_mem_dest	: out Std_Logic_Vector(3 downto 0);

			dec_mem_lw		: out Std_Logic;
			dec_mem_lb		: out Std_Logic;
			dec_mem_sw		: out Std_Logic;
			dec_mem_sb		: out Std_Logic;

	-- Shifter command
			dec_shift_lsl : out Std_Logic;
			dec_shift_lsr : out Std_Logic;
			dec_shift_asr : out Std_Logic;
			dec_shift_ror : out Std_Logic;
			dec_shift_rrx : out Std_Logic;
			dec_shift_val : out Std_Logic_Vector(4 downto 0);
			dec_cy			: out Std_Logic;

	-- Alu operand selection
			dec_comp_op1	: out Std_Logic;
			dec_comp_op2	: out Std_Logic;
			dec_alu_cy 		: out Std_Logic;
			dec_zero_op1	: out Std_Logic; -- For mul if multiplier(0) = '0'

	-- Alu command
			dec_alu_add		: out Std_Logic;
			dec_alu_and		: out Std_Logic;
			dec_alu_or		: out Std_Logic;
			dec_alu_xor		: out Std_Logic;

	-- Exe bypass to decod
			exe_alu_res		: in Std_Logic_Vector(31 downto 0);

			exe_c				: in Std_Logic;
			exe_v				: in Std_Logic;
			exe_n				: in Std_Logic;
			exe_z				: in Std_Logic;

	-- Ifetch interface
			if_pc				: in Std_Logic_Vector(31 downto 0) ;
			if_next_pc		: in Std_Logic_Vector(31 downto 0) ;
			if_ir				: in Std_Logic_Vector(31 downto 0) ;

			dec_fetch_inst	: out Std_Logic;
			dec_bpc			: out Std_Logic_Vector(31 downto 0) ;
			dec_bpc_valid	: out Std_Logic;

	-- Mem WB
			mem_commit_val	: in Std_Logic_Vector(31 downto 0);
			mem_commit_reg	: in Std_Logic_Vector(3 downto 0);
			mem_commit		: in Std_Logic;

	-- global interface
			ck					: in Std_Logic;
			reset_n			: in Std_Logic;
			vdd				: in Std_Logic;
			vss				: in Std_Logic);
	end component;

			signal dec_op1 : Std_Logic_Vector(31 downto 0); -- first alu input
			signal dec_op2 : Std_Logic_Vector(31 downto 0); -- shifter input

			-- Decod to mem via exec
			signal dec_mem_data : Std_Logic_Vector(31 downto 0); -- data to MEM
			signal dec_mem_dest : Std_Logic_Vector(3 downto 0);

			signal dec_mem_lw : Std_Logic;
			signal dec_mem_lb : Std_Logic;
			signal dec_mem_sw : Std_Logic;
			signal dec_mem_sb : Std_Logic;

	-- Shifter command
			signal dec_shift_lsl : Std_Logic;
			signal dec_shift_lsr : Std_Logic;
			signal dec_shift_asr : Std_Logic;
			signal dec_shift_ror : Std_Logic;
			signal dec_shift_rrx : Std_Logic;
			signal dec_shift_val : Std_Logic_Vector(4 downto 0);
			signal dec_cy : Std_Logic;

			-- Alu operand selection
			signal dec_comp_op1 : Std_Logic;
			signal dec_comp_op2 : Std_Logic;
			signal dec_alu_cy : Std_Logic;
			signal dec_zero_op1 : Std_Logic; -- For mul if multiplier(0) = '0'

			-- Alu command
			signal dec_alu_add : Std_Logic;
			signal dec_alu_and : Std_Logic;
			signal dec_alu_or : Std_Logic;
			signal dec_alu_xor : Std_Logic;

			-- Esignal xe bypass to decod
			signal exe_alu_res : Std_Logic_Vector(31 downto 0);
			signal exe_c : Std_Logic;
			signal exe_v : Std_Logic;
			signal exe_n : Std_Logic;
			signal exe_z : Std_Logic;

			-- Ifetch interface
			signal if_pc : Std_Logic_Vector(31 downto 0) ;
			signal if_next_pc : Std_Logic_Vector(31 downto 0) ;
			signal if_ir : Std_Logic_Vector(31 downto 0) ;

			signal dec_fetch_inst : Std_Logic;
			signal dec_bpc : Std_Logic_Vector(31 downto 0) ;
			signal dec_bpc_valid : Std_Logic;

	-- Mem WB
			signal mem_commit_val : Std_Logic_Vector(31 downto 0);
			signal mem_commit_reg : Std_Logic_Vector(3 downto 0);
			signal mem_commit : Std_Logic;

			-- global interface
			signal ck : Std_Logic;
			signal reset_n : Std_Logic;
			signal vdd : Std_Logic;
			signal vss : Std_Logic;
----------------------------------------------------------------------
begin
	testb : decod
	port map(
		dec_op1 => dec_op1,
		dec_op2 => dec_op2,

		dec_mem_data => dec_mem_data,
		dec_mem_dest => dec_mem_dest,
		dec_mem_lw => dec_mem_lw,
		dec_mem_lb => dec_mem_lb,
		dec_mem_sw => dec_mem_sw,
		dec_mem_sb => dec_mem_sb,

		dec_shift_lsl => dec_shift_lsl,
		dec_shift_lsr => dec_shift_lsr,
		dec_shift_asr => dec_shift_asr,
		dec_shift_ror => dec_shift_ror,
		dec_shift_rrx => dec_shift_rrx,
		dec_shift_val => dec_shift_val,
		dec_cy => dec_cy,

		dec_comp_op1 => dec_comp_op1,
		dec_comp_op2 => dec_comp_op2,
		dec_alu_cy => dec_alu_cy,
		dec_zero_op1 => dec_zero_op1,

		dec_alu_add => dec_alu_add,
		dec_alu_and => dec_alu_and,
		dec_alu_or => dec_alu_or,
		dec_alu_xor => dec_alu_xor,
		exe_alu_res => exe_alu_res,

		exe_c => exe_c,
		exe_v => exe_v,
		exe_n => exe_n,
		exe_z => exe_z,

		if_pc => if_pc,
		if_next_pc => if_next_pc,
		if_ir => if_ir,

		dec_fetch_inst => dec_fetch_inst,
		dec_bpc => dec_bpc,
		dec_bpc_valid => dec_bpc_valid,

		mem_commit_val => mem_commit_val,
		mem_commit_reg => mem_commit_reg,
		mem_commit => mem_commit,

		--! Global interface
		ck => ck,
		reset_n => reset_n,
		vss => vss,
		vdd => vdd
	);

	testBench : process
	begin
		if_ir <= X"E" & X"40" & X"0" & X"F" & X"000";
		ck <= '0';
		wait for 2 ns;

		reset_n <= '0';
		ck <= '1';
		wait for 2 ns;

		ck <= '0';
		wait for 2 ns;
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