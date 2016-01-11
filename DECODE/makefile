CC=ghdl
TB = $(basename $(wildcard *_tb.vhdl))

all : $(TB) clean

decod.o :
	$(CC) -a decod.vhdl

%_tb : %_tb.o
	$(CC) -e $@
	$(CC) -r $@ --vcd=$@.vcd

%_tb.o : decod.o
	$(CC) -a $(basename $@).vhdl


mrProper : clean
	rm *.vcd

clean :
	ghdl --clean
	ghdl --remove
