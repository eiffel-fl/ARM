library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Decod is
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
			dec_shift_lsl	: out Std_Logic;
			dec_shift_lsr	: out Std_Logic;
			dec_shift_asr	: out Std_Logic;
			dec_shift_ror	: out Std_Logic;
			dec_shift_rrx	: out Std_Logic;
			dec_shift_val	: out Std_Logic_Vector(4 downto 0);
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
end Decod;

----------------------------------------------------------------------

architecture Behavior OF Decod is

signal cond	: Std_Logic;
signal cond_en	: Std_Logic;

signal regop_t  : Std_Logic;
signal mult_t   : Std_Logic;
signal swap_t   : Std_Logic;
signal trans_t  : Std_Logic;
signal mtrans_t : Std_Logic;
signal branch_t : Std_Logic;

-- regop instructions
signal and_i  : Std_Logic;
signal eor_i  : Std_Logic;
signal sub_i  : Std_Logic;
signal rsb_i  : Std_Logic;
signal add_i  : Std_Logic;
signal adc_i  : Std_Logic;
signal sbc_i  : Std_Logic;
signal rsc_i  : Std_Logic;
signal tst_i  : Std_Logic;
signal teq_i  : Std_Logic;
signal cmp_i  : Std_Logic;
signal cmn_i  : Std_Logic;
signal orr_i  : Std_Logic;
signal mov_i  : Std_Logic;
signal bic_i  : Std_Logic;
signal mvn_i  : Std_Logic;

-- mult instruction
signal mul_i  : Std_Logic;
signal mla_i  : Std_Logic;

-- trans instruction
signal ldr_i  : Std_Logic;
signal str_i  : Std_Logic;
signal ldrb_i : Std_Logic;
signal strb_i : Std_Logic;

-- mtrans instruction
signal ldm_i  : Std_Logic;
signal stm_i  : Std_Logic;

-- branch instruction
signal b_i    : Std_Logic;
signal bl_i   : Std_Logic;

-- RF 
type rf_array is array(14 downto 0) of std_logic_vector(31 downto 0);
signal r_reg	: rf_array;
signal r 		: rf_array;

signal r_valid : Std_Logic_Vector(13 downto 0);

signal r_dest_reg : Std_Logic_Vector(3 downto 0);
signal r_dest_we_reg : Std_Logic;
signal s_set_reg : Std_Logic;

signal load_r : Std_Logic;
signal mtrans_list : Std_Logic_Vector(14 downto 0);
signal mtrans_list_reg : Std_Logic_Vector(14 downto 0);
signal list_cy : Std_Logic_Vector(13 downto 0);

-- RF read ports
signal rf_op1 : Std_Logic_Vector(31 downto 0);
signal rf_op2 : Std_Logic_Vector(31 downto 0);
signal rf_op3 : Std_Logic_Vector(31 downto 0);

signal rf_radr1 : Std_Logic_Vector(3 downto 0);
signal rf_radr2 : Std_Logic_Vector(3 downto 0);
signal rf_radr3 : Std_Logic_Vector(3 downto 0);

-- Flags
signal n_reg : Std_Logic;
signal z_reg : Std_Logic;
signal c_reg : Std_Logic;
signal v_reg : Std_Logic;

signal n : Std_Logic;
signal z : Std_Logic;
signal c : Std_Logic;
signal v : Std_Logic;

-- Operand


signal op1_reg : Std_Logic_Vector(31 downto 0);
signal op2_reg : Std_Logic_Vector(31 downto 0);
signal op3_reg : Std_Logic_Vector(31 downto 0); -- data to mem

signal dest_reg : Std_Logic_Vector(3 downto 0);
signal fset : Std_Logic;

-- DECOD FSM

type state_type is (RUN, BRANCH, FETCH, MTRANS, MUL, OPWAIT, PCADR, PCLOAD, SWAP);
signal cur_state, next_state : state_type;

begin
end Behavior;
