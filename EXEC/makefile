CC=ghdl

all : exec_tb

exec_tb : exec_vide_tb.o
	$(CC) -e exec_tb

exec_vide_tb.o : exec_vide.o
	$(CC) -a exec_vide_tb.vhdl

exec_vide.o :
	$(CC) -a exec_vide.vhdl

exec :	exec_tb
	-$$PWD/exec_tb --vcd=exec.vcd

clean : clean_o
	ghdl --remove

clean_o :
	ghdl --clean