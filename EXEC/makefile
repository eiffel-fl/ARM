CC=ghdl
TB = $(basename $(wildcard *_tb.vhdl))

all : $(TB) clean

exec.o :
	$(CC) -a exec.vhdl

%_tb : %_tb.o
	$(CC) -e $@
	$(CC) -r $@ --vcd=$@.vcd

%_tb.o : exec.o
	$(CC) -a $(basename $@).vhdl


mrProper : clean
	rm *.vcd

clean :
	ghdl --clean
	ghdl --remove
