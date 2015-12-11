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
end Decod;

----------------------------------------------------------------------

architecture Behavior OF Decod is

signal cond	: Std_Logic;
signal cond_en	: Std_Logic;

--décodage du type d'instruction
signal regop_t  : Std_Logic;
signal mult_t   : Std_Logic;
signal swap_t   : Std_Logic;
signal trans_t  : Std_Logic;
signal mtrans_t : Std_Logic;
signal branch_t : Std_Logic;

-- regop instructions
signal and_i : Std_Logic;
signal eor_i : Std_Logic;
signal sub_i : Std_Logic;
signal rsb_i : Std_Logic;
signal add_i : Std_Logic;
signal adc_i : Std_Logic;
signal sbc_i : Std_Logic;
signal rsc_i : Std_Logic;
signal tst_i : Std_Logic;
signal teq_i : Std_Logic;
signal cmp_i : Std_Logic;
signal cmn_i : Std_Logic;
signal orr_i : Std_Logic;
signal mov_i : Std_Logic;
signal bic_i : Std_Logic;
signal mvn_i : Std_Logic;

-- mult instruction
signal mul_i : Std_Logic;
signal mla_i : Std_Logic;

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
signal r_reg	: rf_array; --vrai registre
signal r 		: rf_array; --"sortie" : on choisit entre le vrai registre et le bypass

signal r_valid : Std_Logic_Vector(14 downto 0);

signal r_dest_reg : Std_Logic_Vector(3 downto 0);
signal r_dest_we_reg : Std_Logic;
signal s_set_reg : Std_Logic; --flag S ou non ?

signal load_r : Std_Logic; --load ou store
signal mtrans_list : Std_Logic_Vector(15 downto 0);
signal mtrans_list_reg : Std_Logic_Vector(15 downto 0);
signal list_cy : Std_Logic_Vector(14 downto 0);

-- RF read ports
signal rf_op1 : Std_Logic_Vector(31 downto 0);
signal rf_op2 : Std_Logic_Vector(31 downto 0);
signal rf_op3 : Std_Logic_Vector(31 downto 0);

signal rf_radr1 : Std_Logic_Vector(3 downto 0);
signal rf_radr2 : Std_Logic_Vector(3 downto 0);
signal rf_radr3 : Std_Logic_Vector(3 downto 0);

-- Flags
signal n_reg : Std_Logic; --les registres
signal z_reg : Std_Logic; --les registres
signal c_reg : Std_Logic; --les registres
signal v_reg : Std_Logic; --les registres

signal n : Std_Logic; --registre ou bypass
signal z : Std_Logic; --registre ou bypass
signal c : Std_Logic; --registre ou bypass
signal v : Std_Logic; --registre ou bypass

-- Operand


signal op1_reg : Std_Logic_Vector(31 downto 0);
signal op2_reg : Std_Logic_Vector(31 downto 0);
signal op3_reg : Std_Logic_Vector(31 downto 0); -- data to mem

signal dest_reg : Std_Logic_Vector(3 downto 0);
signal fset : Std_Logic;

-- DECOD FSM

type state_type is (RUN, BRANCH, FETCH, MTRANS, MUL, OPWAIT, PCADR, PCLOAD, SWAP);
signal cur_state, next_state : state_type;

signal opok : Std_Logic;

begin

-- Bypass or commit res

	n <= exe_n when s_set_reg = '1' else n_reg;
	z <= exe_z when s_set_reg = '1' else z_reg;
	c <= exe_c when s_set_reg = '1' else c_reg;
	v <= exe_v when s_set_reg = '1' else v_reg;

-- register file read

	rf_op1 <=	exe_alu_res		when r_dest_we_reg = '1' and r_dest_reg = rf_radr1		else
					mem_commit_val	when mem_commit = '1' and mem_commit_reg = rf_radr1	else
					if_next_pc		when rf_radr1 = X"F"												else
					r_reg(to_integer(unsigned (rf_radr1)));

	rf_op2 <=	exe_alu_res		when r_dest_we_reg = '1' and r_dest_reg = rf_radr2		else
					mem_commit_val	when mem_commit = '1' and mem_commit_reg = rf_radr2	else
					if_next_pc		when rf_radr2 = X"F"												else
					r_reg(to_integer(unsigned(rf_radr2)));

	rf_op3 <=	exe_alu_res when r_dest_we_reg = '1' and r_dest_reg = rf_radr3			else
					mem_commit_val when mem_commit = '1' and mem_commit_reg = rf_radr3	else
					r_reg(to_integer(unsigned(rf_radr3)));

-- register file write

process (ck)
begin
	if rising_edge(ck) then

		if mem_commit = '1' and (r_dest_we_reg = '0' or (r_dest_reg /= mem_commit_reg)) then
			r_reg(to_integer(unsigned (mem_commit_reg))) <= mem_commit_val;
			r_valid(to_integer(unsigned (mem_commit_reg))) <= '1';
		end if;

		if r_dest_we_reg = '1' then
			r_reg(to_integer(unsigned (r_dest_reg))) <= exe_alu_res;
			--r_valid(to_integer(unsigned(r_dest_reg))) <= '1';
		end if;

		if reset_n = '0' then
			r_valid <= "111111111111111";
		elsif load_r = '1' then
			r_valid(to_integer(unsigned (r_dest_reg))) <= '0'; --load sur un registre alors on passe son r_valid à 0
		end if;

		if s_set_reg = '1' then
			n_reg <= exe_n;
			z_reg <= exe_z;
			c_reg <= exe_c;
			v_reg <= exe_v;
		end if;

	end if;
end process;

-- Execution condition

	cond <= '1' when (if_ir(31 downto 28) = X"0" and z = '1') or
							(if_ir(31 downto 28) = X"1" and z = '0') or
							(if_ir(31 downto 28) = X"2" and c = '1') or
							(if_ir(31 downto 28) = X"3" and c = '0') or
							(if_ir(31 downto 28) = X"4" and n = '1') or
							(if_ir(31 downto 28) = X"5" and n = '0') or
							(if_ir(31 downto 28) = X"6" and v = '1') or
							(if_ir(31 downto 28) = X"7" and v = '0') or
							(if_ir(31 downto 28) = X"8" and c = '1' and z = '0') or
							(if_ir(31 downto 28) = X"9" and (c = '0' or z = '1')) or
							(if_ir(31 downto 28) = X"A" and n = v) or
							(if_ir(31 downto 28) = X"B" and n /= v) or
							(if_ir(31 downto 28) = X"C" and z = '0' and n = v) or
							(if_ir(31 downto 28) = X"D" and (z = '1' or n /= v)) or
							(if_ir(31 downto 28) = X"E") else '0';

-- decod instruction type

	regop_t <= '1' when	if_ir(27 downto 26) = "00" and
								mult_t = '0' and swap_t = '0' else '0';
	mult_t <= '1' when if_ir(27 downto 22) = "000000" else '0';
	swap_t <= '1' when if_ir(27 downto 23) = "00010" else '0';
	trans_t <= '1' when if_ir(27 downto 26) = "01" else '0';
	mtrans_t <= '1' when if_ir(27 downto 26) = "10" else '0';
	branch_t <= '1' when if_ir(27 downto 25) = "101" else '0';

	--mult_t, swap_t, trans_t, mtrans_t, branch_t
	--faire pour les autres types d'op
-- decod regop opcode

	and_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"0" else '0';
	eor_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"1" else '0';
	sub_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"2" else '0';
	rsb_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"3" else '0';
	add_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"4" else '0';
	adc_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"5" else '0';
	sbc_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"6" else '0';
	rsc_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"7" else '0';
	tst_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"8" else '0';
	teq_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"9" else '0';
	cmp_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"A" else '0';
	cmn_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"B" else '0';
	orr_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"C" else '0';
	mov_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"D" else '0';
	bic_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"E" else '0';
	mvn_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"F" else '0';

-- mult instruction
	mul_i <= '1' when mult_t = '1' and if_ir(21) = '0' else '0';
	mla_i <= '1' when mult_t = '1' and if_ir(21) = '1' else '0';

-- trans instruction
	ldr_i  <= '1' when trans_t = '1' and if_ir(22) = '0' and if_ir(20) = '1' else '0';
  str_i  <= '1' when trans_t = '1' and if_ir(22) = '0' and if_ir(20) = '0' else '0';
	ldrb_i <= '1' when trans_t = '1' and if_ir(22) = '1' and if_ir(20) = '1' else '0';
	strb_i <= '1' when trans_t = '1' and if_ir(22) = '1' and if_ir(20) = '0' else '0';

-- mtrans instruction
	stm_i <= '1' when mtrans_t = '1' and if_ir(20) = '0' else '0';
	ldm_i_i <= '1' when mtrans_t = '1' and if_ir(20) = '1' else '0';

-- branch instruction
	b_i  <= '1' when branch_t = '1' and if_ir(24) = '0' else '0';
  bl_i <= '1' when branch_t = '1' and if_ir(24) = '1' else '0';

-- Shifter command
	process (ck)
	begin
		if (rising_edge(ck)) then
			dec_cy <= c;
			dec_shift_lsl <= '0';
			dec_shift_lsr <= '0';
			dec_shift_asr <= '0';
			dec_shift_ror <= '0';
			dec_shift_rrx <= '0';
		end if;
	end process;

	dec_shift_lsl <= '1' when regop_t = '1' and if_ir(6 downto 5) = X"0" else '0';
	dec_shift_lsr <= '1' when regop_t = '1' and if_ir(6 downto 5) = X"1" else '0';
	dec_shift_asr <= '1' when regop_t = '1' and if_ir(6 downto 5) = X"2" else '0';
	dec_shift_ror <= '1' when regop_t = '1' and if_ir(6 downto 5) = X"3" and if_ir(11 downto 7) /= X"0" else '0';
	dec_shift_rrx <= '1' when regop_t = '1' and if_ir(6 downto 5) = X"3" and if_ir(11 downto 7) = X"0" else '0';

-- Alu operand selection
	process (ck)
	begin
		if (rising_edge(ck)) then
			dec_comp_op1	<= '0';
			dec_comp_op2	<= '0';
			dec_zero_op1	<= '0';
			dec_alu_cy 		<= '0';

			if (rsb_i = '1' or rsc_i = '1') then
				dec_comp_op1 <= '1';
			end if;

			if(sbc_i = '1' or sub_i = '1' or bic_i = '1' or cmp_i = '1') then
				dec_comp_op2 <= '1';
			end if;

			if(mov_i = '1' or mvn_i = '1') then
				dec_zero_op1 <= '1';
			end if;

			if(rsc_i = '1' or adc_i = '1' or sbc_i = '1') then
				dec_alu_cy <= '1';
			end if;
		end if;
	end process;

-- Alu command
	process (ck)
	begin
		if (rising_edge(ck)) then
			dec_alu_and <= '0';
			dec_alu_or  <= '0';
			dec_alu_xor <= '0';
			dec_alu_add <= '1';

			if (eor_i = '1' or teq_i = '1') then
				dec_alu_xor <= '1';
				dec_alu_add <= '0';
			end if;

			if(and_i = '1' or tst_i = '1' or bic_i = '1') then
				dec_alu_and <= '1';
				dec_alu_add <= '0';
			end if;

			if(orr_i = '1' or mov_i = '1' or mvn_i = '1') then
				dec_alu_or <= '1';
				dec_alu_add <= '0';
			end if;
		end if;
	end process;

-- Ifetch interface

	dec_fetch_inst	<= '1' when next_state = RUN or next_state = FETCH else '0';

-- Mtrans reg list

	list_cy(0) <= mtrans_list_reg(0);
	mtrans_next_reg <=	X"0" when mtrans_list(0) = '1' else
								X"1" when mtrans_list(1) = '1' else
								X"2" when mtrans_list(2) = '1' else
								X"3" when mtrans_list(3) = '1' else
								X"4" when mtrans_list(4) = '1';

-- Decod to mem via exec

	process (ck)
	begin
		if (rising_edge(ck)) then

			if (next_state = RUN and ldr_i = '1') or
				((next_state = MTRANS or cur_state = MTRANS) and ldm_i = '1') then
				dec_mem_lw <= '1';
			else
				dec_mem_lw <= '0';
			end if;
		end if;
	end process;

	load_r <= '1' when (next_state = RUN and (ldr_i = '1' or ldrb_i = '1')) or
								((next_state = MTRANS or cur_state = MTRANS) and ldm_i = '1') else '0';

	dec_mem_data <= op3_reg;



-- decod reg pipe
-- output to exec

	dec_op1 <= op1_reg;
	dec_op2 <= op2_reg;

	dec_dest <= dest;
	dec_fset <= fset;

-- FSM

process (ck)
begin

if (rising_edge(ck)) then
	if (reset_n = '0') then
		cur_state <= FETCH;
	else
		cur_state <= next_state;
	end if;
end if;

end process;

--state machine process.
process (cur_state, branch_t, mtrans_t, mult_t, opok)
begin
	case cur_state is
	when FETCH =>
		next_state <= RUN;

	when RUN =>

	when OPWAIT =>
		if opok = '0' then
			next_state <= OPWAIT;
		end if;

	when BRANCH =>

	when SWAP =>

	when MTRANS =>

	when PCADR =>

	when PCLOAD =>

	when MUL =>
	end case;
end process;

end Behavior;