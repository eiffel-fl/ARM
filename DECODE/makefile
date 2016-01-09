CC=ghdl

all : decod_tb decod_tb2

decod_tb : decod_tb.o
	$(CC) -e decod_tb

decod_tb.o : decod.o
	$(CC) -a decod_tb.vhdl

decod_tb2 : decod_tb2.o
	$(CC) -e decod_tb2

decod_tb2.o : decod.o
	$(CC) -a decod_tb2.vhdl

decod.o :
	$(CC) -a decod.vhdl

decod : decod_tb
	-$$PWD/decod_tb --vcd=decod.vcd

decod2 : decod_tb2
	-$$PWD/decod_tb2 --vcd=decod2.vcd

clean : clean_o
	ghdl --remove
	rm decod.vcd

clean_o :
	ghdl --clean
