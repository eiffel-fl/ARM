CC=ghdl

all : exec_tb

exec_tb : exec_tb.o
	$(CC) -e exec_tb

exec_tb.o : exec.o
	$(CC) -a exec_tb.vhdl

exec.o :
	$(CC) -a exec.vhdl

exec :	exec_tb
	-$$PWD/exec_tb --vcd=exec.vcd

clean : clean_o
	ghdl --remove
	rm exec.vcd

clean_o :
	ghdl --clean
