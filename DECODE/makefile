CC=ghdl

all : decod_tb

decod_tb : decod_tb.o
	$(CC) -e decod_tb

decod_tb.o : decod.o
	$(CC) -a decod_tb.vhdl

decod.o :
	$(CC) -a decod.vhdl

decod :	decod_tb
	-$$PWD/decod_tb --vcd=decod.vcd

clean : clean_o
	ghdl --remove
	rm decod.vcd

clean_o :
	ghdl --clean
