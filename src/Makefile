
.SUFFIXES: .c .o

objects = lexical.o main.o liststor.o prsexp.o \
	turtgrap.o turtinf.o linenoise.o  \
	compex.o
CFLAGS = -Wall -DDEBUG -g -pedantic -std=gnu2x -O2 

lisp: $(objects)
	cc -o $@ $(objects) -lX11 -L/usr/X11/lib -lm
	cp lisp ~/bin/

install: $(objects)
	cc -o $@ $(objects) -lX11 -L/usr/X11/lib -lm
	cp lisp ~/bin/
	sudo cp lisp /usr/local/bin/

# header dependencies, so editing a .h forces a rebuild of everything
# that includes it -- make's implicit .c.o rule only looks at the .c
# file's own timestamp otherwise.
lexical.o:  listspec.h
main.o:     listspec.h turtinf.h linenoise.h
liststor.o: listspec.h
prsexp.o:   listspec.h
turtgrap.o: turtle.h
turtinf.o:  listspec.h turtinf.h turtle.h
linenoise.o: linenoise.h
compex.o:   listspec.h turtinf.h

.PHONEY: clean
clean:
	rm -f $(objects)
	rm -f ~/bin/lisp

