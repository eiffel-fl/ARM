library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity EXec is
	port(
	-- Decode interface operands
			dec_op1			: in Std_Logic_Vector(31 downto 0); -- first alu input
			dec_op2			: in Std_Logic_Vector(31 downto 0); -- shifter input

	-- Decode to mem interface 
			dec_mem_data	: in Std_Logic_Vector(31 downto 0); -- data to MEM
			dec_mem_dest	: in Std_Logic_Vector(5 downto 0);

			dec_mem_lw		: in Std_Logic;
			dec_mem_lb		: in Std_Logic;
			dec_mem_sw		: in Std_Logic;
			dec_mem_sb		: in Std_Logic;

	-- Shifter command
			dec_shift_lsl	: in Std_Logic;
			dec_shift_lsr	: in Std_Logic;
			dec_shift_asr	: in Std_Logic;
			dec_shift_ror	: in Std_Logic;
			dec_shift_rrx	: in Std_Logic;
			dec_shift_val	: in Std_Logic_Vector(4 downto 0);
			dec_cy			: in Std_Logic;

	-- Alu operand selection
			dec_comp_op1	: in Std_Logic;
			dec_comp_op2	: in Std_Logic;
			dec_alu_cy 		: in Std_Logic;
			dec_zero_op1	: in Std_Logic; -- For mul if multiplier(0) = '0'

	-- Alu command
			dec_alu_add		: in Std_Logic;
			dec_alu_and		: in Std_Logic;
			dec_alu_or		: in Std_Logic;
			dec_alu_xor		: in Std_Logic;

	-- Exe bypass to decod
			exe_alu_res		: out Std_Logic_Vector(31 downto 0);

			exe_c				: out Std_Logic;
			exe_v				: out Std_Logic;
			exe_n				: out Std_Logic;
			exe_z				: out Std_Logic;

	-- Mem interface
			exe_mem_adr		: out Std_Logic_Vector(31 downto 0); -- Alu res register
			exe_mem_data	: out Std_Logic_Vector(31 downto 0);
			exe_mem_dest	: out Std_Logic_Vector(5 downto 0);

			exe_mem_lw		: out Std_Logic;
			exe_mem_lb		: out Std_Logic;
			exe_mem_sw		: out Std_Logic;
			exe_mem_sb		: out Std_Logic;

		--exe_res_valid	: out Std_Logic;

	-- global interface
			ck					: in Std_Logic;
			reset_n			: in Std_Logic;
			vdd				: in Std_Logic;
			vss				: in Std_Logic);
end EXec;

----------------------------------------------------------------------

architecture Behavior OF EXec is

signal op2_lsl		: std_logic_vector(31 downto 0);
signal op2_lsr		: std_logic_vector(31 downto 0);
signal op2_asr		: std_logic_vector(31 downto 0);
signal op2_ror		: std_logic_vector(31 downto 0);

signal shift_asr32	: std_logic_vector(31 downto 0);
signal shift_ror32	: std_logic_vector(31 downto 0);
signal sign_op2	: std_logic_vector(31 downto 0);
signal shift_right_in	: std_logic_vector(31 downto 0);
signal op2_right	: std_logic_vector(31 downto 0);
signal op2_shift	: std_logic_vector(31 downto 0);

signal left_cy		: std_logic;
signal right_cy	: std_logic;
signal shift_c	: std_logic;

signal op2			: std_logic_vector(31 downto 0);
signal op1			: std_logic_vector(31 downto 0);

signal alu_add		: std_logic_vector(31 downto 0);
signal p				: std_logic_vector(31 downto 0);
signal g				: std_logic_vector(31 downto 0);
signal p4b			: std_logic_vector(7 downto 0);
signal g4b			: std_logic_vector(7 downto 0);
signal p16b			: std_logic_vector(1 downto 0);
signal g16b			: std_logic_vector(1 downto 0);
signal c				: std_logic_vector(31 downto 0);

signal add32		: std_logic_vector(31 downto 0);
signal and32		: std_logic_vector(31 downto 0);
signal or32			: std_logic_vector(31 downto 0);
signal xor32		: std_logic_vector(31 downto 0);
signal alu_res		: std_logic_vector(31 downto 0);
signal res_reg		: std_logic_vector(31 downto 0);

--signal ope1_res	: std_logic;
--signal ope2_zero	: std_logic;
--signal ope_zero	: std_logic;
--
--signal res_valid	: std_logic;
--type state is (RUN, MUL);
--signal cur_state, next_state : state;

begin
end Behavior;
