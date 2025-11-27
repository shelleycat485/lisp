
.SUFFIXES: .c .o

objects = lexical.o main.o liststor.o prsexp.o \
	setcontc.o turtgrap.o turtinf.o linenoise.o 
CFLAGS = -Wall -g 

lisp: $(objects)
	cc -o $@ $(objects) -lX11 -L/usr/X11/lib -lm
	mv lisp ~/bin/
	sudo cp ~/bin/lisp /usr/local/bin/

.PHONEY: clean
clean:
	rm -f $(objects)
	rm -f ~/bin/lisp

