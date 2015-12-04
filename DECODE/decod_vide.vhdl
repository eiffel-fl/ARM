library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Decod is
	port(
	-- Exec interface
			dec_op1			: out Std_Logic_Vector(31 downto 0); -- first alu input

			dec_op2			: out Std_Logic_Vector(31 downto 0); -- shifter input
			dec_op3			: out Std_Logic_Vector(31 downto 0); -- data to MEM or MUL accu
			dec_shift_lsl	: out Std_Logic;
			dec_shift_lsr	: out Std_Logic;
			dec_shift_asr	: out Std_Logic;
			dec_shift_ror	: out Std_Logic;
			dec_shift_rrx	: out Std_Logic;
			dec_shift_val	: out Std_Logic_Vector(4 downto 0);

			dec_cy			: out Std_Logic;

			dec_comp_op1	: out Std_Logic;
			dec_comp_op2	: out Std_Logic;
			dec_alu_cy 		: out Std_Logic;

			dec_alu_add		: out Std_Logic;
			dec_alu_and		: out Std_Logic;
			dec_alu_or		: out Std_Logic;
			dec_alu_xor		: out Std_Logic;
			exe_res			: in Std_Logic_Vector(31 downto 0);
			exe_res_valid	: in Std_Logic;

			dec_mul			: out Std_Logic;

	-- Ifetch interface
			if_pc				: in Std_Logic_Vector(31 downto 0) ;
			if_next_pc		: in Std_Logic_Vector(31 downto 0) ;
			if_ir				: in Std_Logic_Vector(31 downto 0) ;
			if_ir_valid		: in Std_Logic;

			dec_fetch_inst	: out Std_Logic;
			dec_bpc			: out Std_Logic_Vector(31 downto 0) ;
			dec_bpc_valid	: out Std_Logic;

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

type rf_array is array(14 downto 0) of std_logic_vector(31 downto 0);
signal r  : rf_array;
signal r_valid : Std_Logic_Vector(14 downto 0);

signal n : Std_Logic;
signal z : Std_Logic;
signal c : Std_Logic;
signal v : Std_Logic;

signal f_en : Std_Logic;

signal op1 : Std_Logic_Vector(31 downto 0);
signal op2 : Std_Logic_Vector(31 downto 0);
signal op3 : Std_Logic_Vector(31 downto 0); -- data to mem

signal dest : Std_Logic_Vector(3 downto 0);
signal fset : Std_Logic;

-- DECOD FSM

type state_type is (RUN, BRANCH, FETCH, MTRANS, MUL, OPWAIT);
signal cur_state, next_state : state_type;

begin

-- Execution condition

	cond <= '1' when	(f_en = '1' and ((if_ir(31 dwnto 28) = X"0" and z = '1') or
							(if_ir(31 dwnto 28) = X"1" and z = '0') or
							(if_ir(31 dwnto 28) = X"2" and c = '1') or
							(if_ir(31 dwnto 28) = X"D" and z = '1' or n /= v))) or
							(if_ir(31 dwnto 28) = X"E") else '0';

-- decod instruction type

	mult_t <= '1' when	if_ir(27 downto 22) = "000000" and
								if_ir(7 downto 4) = "1001" else '0';

-- decod regop opcode

	and_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"0" else '0';

-- mult instruction

-- trans instruction

-- mtrans instruction

-- branch instruction


-- register file

process (ck, reset)
begin
	if rising_edge(ck) then

		if reset_n = '0' then
			r_valid <= "111111111111111";
		else
		end if;
	end if;
end process;

-- decod reg pipe
-- output to exec

	dec_op1 <= op1;
	dec_op2 <= op2;
	dec_op3 <= op3;

	dec_dest <= dest;
	dec_fest <= fset;

-- FSM

process (ck, reset_n)
begin

if (rising_edge(clk)) then
	if (reset_n = '0') then
		cur_state <= FETCH;
	else
		cur_state <= next_state;
	end if;
end if;

end process;

--state machine process.
process (cur_state, branch_t, mtrans_t, mul_t, opok)
begin
	case cur_state is
	when FETCH =>

		next_state <= RUN;
	
	when RUN =>
	end case;
end process;

end Behavior;
