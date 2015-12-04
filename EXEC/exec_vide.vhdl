library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity EXec is
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
end EXec;

----------------------------------------------------------------------

architecture Behavior OF EXec is
	--! op2 pour les décalages
	signal op2_lsl : std_logic_vector(31 downto 0);
	signal op2_lsr : std_logic_vector(31 downto 0);
	signal op2_asr : std_logic_vector(31 downto 0);
	signal op2_ror : std_logic_vector(31 downto 0);

	--décalages
	signal shift_asr32 : std_logic_vector(31 downto 0);
	signal shift_ror32 : std_logic_vector(31 downto 0);
	signal sign_op2 : std_logic_vector(31 downto 0);
	signal shift_right_in : std_logic_vector(31 downto 0);
	signal op2_right : std_logic_vector(31 downto 0);
	signal op2_shift : std_logic_vector(31 downto 0);

	--carry décalage
	signal left_cy : std_logic;
	signal right_cy : std_logic;
	signal shift_cy : std_logic;

	--opérandes
	signal op2 : std_logic_vector(31 downto 0);
	signal op1 : std_logic_vector(31 downto 0);

	--additionneur
-- 	signal alu_add : std_logic_vector(31 downto 0);
-- 	signal p : std_logic_vector(31 downto 0);
-- 	signal g : std_logic_vector(31 downto 0);
	signal p4b : std_logic_vector(7 downto 0);
	signal g4b : std_logic_vector(7 downto 0);
	signal p16b : std_logic_vector(1 downto 0);
	signal g16b : std_logic_vector(1 downto 0);
-- 	signal c : std_logic_vector(31 downto 0);

	--resultats
	signal add32 : std_logic_vector(31 downto 0);
	signal and32 : std_logic_vector(31 downto 0);
	signal or32 : std_logic_vector(31 downto 0);
	signal xor32 : std_logic_vector(31 downto 0);
	signal alu_res : std_logic_vector(31 downto 0);

begin
	--! shifter result
	op2_shift <=	op2_lsl when dec_shift_lsl = '1' else
	dec_cy & dec_op2(31 downto 1) when dec_shift_rrx = '1' else
	op2_right when dec_shift_asr = '1' or dec_shift_lsr = '1' or dec_shift_ror = '1' else
	dec_op2;

	--! LSL
	with dec_shift_val select
		op2_lsl <=	dec_op2(0) & X"0000000" & "000" when "11111",
		dec_op2( 1 downto 0) & X"0000000" &  "00" when "11110",
		dec_op2( 2 downto 0) & X"0000000" &   "0" when "11101",
		dec_op2( 3 downto 0) & X"0000000" when "11100",
		dec_op2( 4 downto 0) & X"000000" & "000" when "11011",
		dec_op2( 5 downto 0) & X"000000" &  "00" when "11010",
		dec_op2( 6 downto 0) & X"000000" &   "0" when "11001",
		dec_op2( 7 downto 0) & X"000000"         when "11000",
		dec_op2( 8 downto 0) & X"00000" & "000" when "10111",
		dec_op2( 9 downto 0) & X"00000" &  "00" when "10110",
		dec_op2(10 downto 0) & X"00000" &   "0" when "10101",
		dec_op2(11 downto 0) & X"00000"         when "10100",
		dec_op2(12 downto 0) & X"0000" & "000" when "10011",
		dec_op2(13 downto 0) & X"0000" &  "00" when "10010",
		dec_op2(14 downto 0) & X"0000" &   "0" when "10001",
		dec_op2(15 downto 0) & X"0000"         when "10000",
		dec_op2(16 downto 0) & X"000" & "000" when "01111",
		dec_op2(17 downto 0) & X"000" &  "00" when "01110",
		dec_op2(18 downto 0) & X"000" &   "0" when "01101",
		dec_op2(19 downto 0) & X"000"         when "01100",
		dec_op2(20 downto 0) & X"00" & "000" when "01011",
		dec_op2(21 downto 0) & X"00" &  "00" when "01010",
		dec_op2(22 downto 0) & X"00" &   "0" when "01001",
		dec_op2(23 downto 0) & X"00"         when "01000",
		dec_op2(24 downto 0) & X"0" & "000" when "00111",
		dec_op2(25 downto 0) & X"0" &  "00" when "00110",
		dec_op2(26 downto 0) & X"0" &   "0" when "00101",
		dec_op2(27 downto 0) & X"0"         when "00100",
		dec_op2(28 downto 0) & "000" when "00011",
		dec_op2(29 downto 0) & "00" when "00010",
		dec_op2(30 downto 0) & "0" when "00001",
		dec_op2(31 downto 0) when others;

	--! LSR
	with dec_shift_val select
		op2_lsr <= X"0000000" &  "000" & dec_op2(31) when "11111",
		X"0000000" &  "00" & dec_op2(31 downto 30) when "11110",
		X"0000000" &   "0" & dec_op2(31 downto 29) when "11101",
		X"0000000"         & dec_op2(31 downto 28) when "11100",
		X"000000" & "000" & dec_op2(31 downto 27) when "11011",
		X"000000" &  "00" & dec_op2(31 downto 26) when "11010",
		X"000000" &   "0" & dec_op2(31 downto 25) when "11001",
		X"000000"         & dec_op2(31 downto 24) when "11000",
		X"00000" & "000" & dec_op2(31 downto 23) when "10111",
		X"00000" &  "00" & dec_op2(31 downto 22) when "10110",
		X"00000" &   "0" & dec_op2(31 downto 21) when "10101",
		X"00000"         & dec_op2(31 downto 20) when "10100",
		X"0000" & "000" & dec_op2(31 downto 19) when "10011",
		X"0000" &  "00" & dec_op2(31 downto 18) when "10010",
		X"0000" &   "0" & dec_op2(31 downto 17) when "10001",
		X"0000"         & dec_op2(31 downto 16)when "10000",
		X"000" & "000" & dec_op2(31 downto 15) when "01111",
		X"000" &  "00" & dec_op2(31 downto 14) when "01110",
		X"000" &   "0" & dec_op2(31 downto 13) when "01101",
		X"000"         & dec_op2(31 downto 12)when "01100",
		X"00" & "000" & dec_op2(31 downto 11) when "01011",
		X"00" &  "00" & dec_op2(31 downto 10) when "01010",
		X"00" &   "0"   & dec_op2(31 downto 9) when "01001",
		X"00"           & dec_op2(31 downto 8) when "01000",
		X"0" & "000"   & dec_op2(31 downto 7) when "00111",
		X"0" &  "00"   & dec_op2(31 downto 6) when "00110",
		X"0" &   "0"   & dec_op2(31 downto 5) when "00101",
		X"0" & dec_op2(31 downto 4) when "00100",
		"000" & dec_op2(31 downto 3) when "00011",
		"00" & dec_op2(31 downto 2) when "00010",
		"0" & dec_op2(31 downto 1) when "00001",
		dec_op2(31 downto 0) when others;


	--carry decalage
	shift_cy <= right_cy when dec_shift_lsr = '1' or dec_shift_asr = '1' or dec_shift_ror = '1' else dec_op2(0) when dec_shift_rrx = '1' else
	left_cy;

	--! right carry
	with dec_shift_val select
		right_cy <= dec_op2(30) when "11111",
		dec_op2(29) when "11110",
		dec_op2(28) when "11101",
		dec_op2(27) when "11100",
		dec_op2(26) when "11011",
		dec_op2(25) when "11010",
		dec_op2(24) when "11001",
		dec_op2(23) when "11000",
		dec_op2(22) when "10111",
		dec_op2(21) when "10110",
		dec_op2(20) when "10101",
		dec_op2(19) when "10100",
		dec_op2(18) when "10011",
		dec_op2(17) when "10010",
		dec_op2(16) when "10001",
		dec_op2(15) when "10000",
		dec_op2(14) when "01111",
		dec_op2(13) when "01110",
		dec_op2(12) when "01101",
		dec_op2(11) when "01100",
		dec_op2(10) when "01011",
		dec_op2(9)  when "01010",
		dec_op2(8) when "01001",
		dec_op2(7) when "01000",
		dec_op2(6) when "00111",
		dec_op2(5) when "00110",
		dec_op2(4) when "00101",
		dec_op2(3) when "00100",
		dec_op2(2) when "00011",
		dec_op2(1) when "00010",
		dec_op2(0) when "00001",
		dec_cy when others;

	--! left carry
	with dec_shift_val select
		left_cy <= dec_op2(1) when "11111",
		dec_op2(2) when "11110",
		dec_op2(3) when "11101",
		dec_op2(4) when "11100",
		dec_op2(5) when "11011",
		dec_op2(6) when "11010",
		dec_op2(7) when "11001",
		dec_op2(8) when "11000",
		dec_op2(9) when "10111",
		dec_op2(10) when "10110",
		dec_op2(11) when "10101",
		dec_op2(12) when "10100",
		dec_op2(13) when "10011",
		dec_op2(14) when "10010",
		dec_op2(15) when "10001",
		dec_op2(16) when "10000",
		dec_op2(17) when "01111",
		dec_op2(18) when "01110",
		dec_op2(19) when "01101",
		dec_op2(20) when "01100",
		dec_op2(21) when "01011",
		dec_op2(22) when "01010",
		dec_op2(23) when "01001",
		dec_op2(24) when "01000",
		dec_op2(25) when "00111",
		dec_op2(26) when "00110",
		dec_op2(27) when "00101",
		dec_op2(28) when "00100",
		dec_op2(29) when "00011",
		dec_op2(30) when "00010",
		dec_op2(31) when "00001",
		dec_cy when others;

	--! extension du but de signe de op2 pour le décalage arithmétique
	sign_op2 <= x"ffffffff" when dec_op2(31) = '1' else x"00000000";

	--! ASR
	with dec_shift_val select
		op2_asr <= sign_op2(31 downto 1) & dec_op2(31) when "11111",
		sign_op2(31 downto 2) & dec_op2(31 downto 30) when "11110",
		sign_op2(31 downto 3) & dec_op2(31 downto 29) when "11101",
		sign_op2(31 downto 4) & dec_op2(31 downto 28) when "11100",
		sign_op2(31 downto 5) & dec_op2(31 downto 27) when "11011",
		sign_op2(31 downto 6) & dec_op2(31 downto 26) when "11010",
		sign_op2(31 downto 7) & dec_op2(31 downto 25) when "11001",
		sign_op2(31 downto 8) & dec_op2(31 downto 24) when "11000",
		sign_op2(31 downto 9) & dec_op2(31 downto 23) when "10111",
		sign_op2(31 downto 10) & dec_op2(31 downto 22) when "10110",
		sign_op2(31 downto 11) & dec_op2(31 downto 21) when "10101",
		sign_op2(31 downto 12) & dec_op2(31 downto 20) when "10100",
		sign_op2(31 downto 13) & dec_op2(31 downto 19) when "10011",
		sign_op2(31 downto 14) & dec_op2(31 downto 18) when "10010",
		sign_op2(31 downto 15) & dec_op2(31 downto 17) when "10001",
		sign_op2(31 downto 16) & dec_op2(31 downto 16)when "10000",
		sign_op2(31 downto 17) & dec_op2(31 downto 15) when "01111",
		sign_op2(31 downto 18) & dec_op2(31 downto 14) when "01110",
		sign_op2(31 downto 19) & dec_op2(31 downto 13) when "01101",
		sign_op2(31 downto 20) & dec_op2(31 downto 12)when "01100",
		sign_op2(31 downto 21) & dec_op2(31 downto 11) when "01011",
		sign_op2(31 downto 22) & dec_op2(31 downto 10) when "01010",
		sign_op2(31 downto 23) & dec_op2(31 downto 9) when "01001",
		sign_op2(31 downto 24) & dec_op2(31 downto 8) when "01000",
		sign_op2(31 downto 25) & dec_op2(31 downto 7) when "00111",
		sign_op2(31 downto 26) & dec_op2(31 downto 6) when "00110",
		sign_op2(31 downto 27) & dec_op2(31 downto 5) when "00101",
		sign_op2(31 downto 28) & dec_op2(31 downto 4) when "00100",
		sign_op2(31 downto 29) & dec_op2(31 downto 3) when "00011",
		sign_op2(31 downto 30) & dec_op2(31 downto 2) when "00010",
		sign_op2(31) & dec_op2(31 downto 1) when "00001",
		dec_op2(31 downto 0) when others;

	--! ROR
	with dec_shift_val select
		op2_ror <= dec_op2(30 downto 1) & dec_op2(31) when "11111",
		dec_op2(29 downto 0) & dec_op2(31 downto 30) when "11110",
		dec_op2(28 downto 0) & dec_op2(31 downto 29) when "11101",
		dec_op2(27 downto 0) & dec_op2(31 downto 28) when "11100",
		dec_op2(26 downto 0) & dec_op2(31 downto 27) when "11011",
		dec_op2(25 downto 0) & dec_op2(31 downto 26) when "11010",
		dec_op2(24 downto 0) & dec_op2(31 downto 25) when "11001",
		dec_op2(23 downto 0) & dec_op2(31 downto 24) when "11000",
		dec_op2(22 downto 0) & dec_op2(31 downto 23) when "10111",
		dec_op2(21 downto 0) & dec_op2(31 downto 22) when "10110",
		dec_op2(20 downto 0) & dec_op2(31 downto 21) when "10101",
		dec_op2(19 downto 0) & dec_op2(31 downto 20) when "10100",
		dec_op2(18 downto 0) & dec_op2(31 downto 19) when "10011",
		dec_op2(17 downto 0) & dec_op2(31 downto 18) when "10010",
		dec_op2(16 downto 0) & dec_op2(31 downto 17) when "10001",
		dec_op2(15 downto 0) & dec_op2(31 downto 16) when "10000",
		dec_op2(14 downto 0) & dec_op2(31 downto 15) when "01111",
		dec_op2(13 downto 0) & dec_op2(31 downto 14) when "01110",
		dec_op2(12 downto 0) & dec_op2(31 downto 13) when "01101",
		dec_op2(11 downto 0) & dec_op2(31 downto 12) when "01100",
		dec_op2(10 downto 0) & dec_op2(31 downto 11) when "01011",
		dec_op2(9 downto 0) & dec_op2(31 downto 10) when "01010",
		dec_op2(8 downto 0) & dec_op2(31 downto 9) when "01001",
		dec_op2(7 downto 0) & dec_op2(31 downto 8) when "01000",
		dec_op2(6 downto 0) & dec_op2(31 downto 7) when "00111",
		dec_op2(5 downto 0) & dec_op2(31 downto 6) when "00110",
		dec_op2(4 downto 0) & dec_op2(31 downto 5) when "00101",
		dec_op2(3 downto 0) & dec_op2(31 downto 4) when "00100",
		dec_op2(2 downto 0) & dec_op2(31 downto 3) when "00011",
		dec_op2(1 downto 0) & dec_op2(31 downto 2) when "00010",
		dec_op2(0) & dec_op2(31 downto 1) when "00001",
		dec_op2(31 downto 0) when others;

		op1 <= dec_op1;

	--! Operations process
	process(ck)
	variable sout1 : std_logic_vector(31 downto 0);
	variable sout : std_logic_vector(31 downto 0);

	variable cout1 : std_logic_vector(32 downto 0);
	variable cout2 : std_logic_vector(31 downto 0);
	variable cout : std_logic_vector(32 downto 0);

	variable p : std_logic_vector(31 downto 0);
	variable g : std_logic_vector(31 downto 0);
	variable c : std_logic_vector(31 downto 0);
	variable alu_add : std_logic_vector(31 downto 0);
	begin
		if rising_edge(ck) then
			if reset_n = '0' then
				exe_res <= X"00000000";
			end if;

			if dec_alu_add = '1' then
-- 				p := op1 or op2_shift;
-- 				g := op1 and op2_shift;
--
-- 				c(0) := dec_alu_cy;
--
-- 				for i in 0 to 30 loop
-- 					c(i + 1) := g(i) or (p(i) and c(i));
-- 				end loop;
--
-- 				alu_add := (op1 xor op2_shift) xor c;
-- 				exe_res <= alu_add;

				cout(0) := dec_alu_cy;

				report "dec_op1 = " & std_logic'image(dec_op1(0));
				report "dec_op2 = " & std_logic'image(dec_op2(0));

				for i in 0 to 31 loop
					sout1(i) := op1(i) xor op2_shift(i);

					cout1(i) := op1(i) and op2_shift(i);

					sout(i) := cout(i) xor sout1(i);
					cout2(i) := cout(i) and sout1(i);

					cout(i + 1) := cout1(i) or cout2(i);

					report "cout(" & integer'image(i)  & ") = " & std_logic'image(cout(i));
					report "op1(" & integer'image(i) & ") = " & std_logic'image(op1(i));
					report "op2_shift(" & integer'image(i) & ") = " & std_logic'image(op2_shift(i));

					report "sout1(" & integer'image(i) & ") = " & std_logic'image(sout1(i));
					report "cout1(" & integer'image(i) & ") = " & std_logic'image(cout1(i));
					report "cout2(" & integer'image(i) & ") = " & std_logic'image(cout2(i));

					report "sout(" & integer'image(i) & ") = " & std_logic'image(sout(i));
					report "cout(" & integer'image(i + 1)  & ") = " & std_logic'image(cout(i + 1));
					report "";
				end loop;

				exe_res <= sout;
				--p4b
				p4b(0) <= p(0) or p(1) or p(2) or p(3);
				p4b(1) <= p(4) or p(5) or p(6) or p(7);
				p4b(2) <= p(8) or p(9) or p(10) or p(11);
				p4b(3) <= p(12) or p(13) or p(14) or p(15);
				p4b(4) <= p(16) or p(17) or p(18) or p(19);
				p4b(5) <= p(20) or p(21) or p(22) or p(23);
				p4b(6) <= p(24) or p(25) or p(26) or p(27);
				p4b(7) <= p(28) or p(29) or p(30) or p(31);

				--g4b
				g4b(0) <= g(0) and g(1) and g(2) and g(3);
				g4b(1) <= g(4) and g(5) and g(6) and g(7);
				g4b(2) <= g(8) and g(9) and g(10) and g(11);
				g4b(3) <= g(12) and g(13) and g(14) and g(15);
				g4b(4) <= g(16) and g(17) and g(18) and g(19);
				g4b(5) <= g(20) and g(21) and g(22) and g(23);
				g4b(6) <= g(24) and g(25) and g(26) and g(27);
				g4b(7) <= g(28) and g(29) and g(30) and g(31);

				--p16b
				p16b(0) <= p4b(0) or p4b(1) or p4b(2) or p4b(3);
				p16b(1) <= p4b(4) or p4b(5) or p4b(6) or p4b(7);

				--g16b
				g16b(0) <= g4b(0) and g4b(1) and g4b(2) and g4b(3);
				g16b(1) <= g4b(4) and g4b(5) and g4b(6) and g4b(7);
			end if;

			if dec_alu_and = '1' then
				exe_res <= op2_shift and dec_op1;
			end if;

			if dec_alu_or = '1' then
				exe_res <= op2_shift or dec_op1;
			end if;

			if dec_alu_xor = '1' then
				exe_res <= op2_shift xor dec_op1;
			end if;
		end if;
	end process;

	--! Flags process
	process(ck) begin
		if rising_edge(ck) then
			if alu_res(31) = '1' then
				exe_n <= '1';
			end if;

			if alu_res = X"00000000" then
				exe_z <= '1';
			end if;

			--if alu_res = '' then
				--exe_v <=
			--end if;
		end if;
	end process;
end Behavior;