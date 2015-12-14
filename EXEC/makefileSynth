all : exec.o

exec.o : exec.vhdl
	ghdl -a -v exec.vhdl

exec.vbe : exec.vhdl
	vasy -I vhdl -V -o -a exec.vhdl exec

exec_o.vbe : exec.vbe
	boom -V exec exec_o

exec.vst : exec_o.vbe
	boog exec_o exec

exec.vhd : exec.vst
	vasy -I vst -s -o -S exec exec

#sed "/ENTITY/i\library cells;\nuse cells.all;\n" ${nomf}.vhd > ${nomf}_net.vhdl

clean :
	rm exec.o work-obj93.cf *.vbe
