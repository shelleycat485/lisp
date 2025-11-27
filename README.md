# lisp

## LISP Interpreter 

This is an in-memory, garbage collected, LISP interpreter. It was originally
based on the BBC Micro Lisp version, and is written in C, originally on an 
IBM-PC.

Extensions are Turtle graphics, using X11.  Hence X11 is needed to link.
A complete environment can be loaded/saved using the load and save primitives.

File reading and writing is supported.  No network connectivity.  A System promitive allows shell execution.

Tested in Linux, Ubuntu and Devuan. Also Debian (Raspian) on ARM64.

## Build and install.

### Prerequisites are:

GNU Make 4.3
libx11-dev
GCC 12.2.0 or above

### Build 
Change directory to lisp/src
make clean
make

### Running

lisp [files to load]
e.g. lisp stdload.lsp a.lsp tri2.lsp
Useful ones are in lisp/lisplib
stdload is in the lisp directory for convenience.  Lisp source can be loaded using the (load 'filename.lsp)

(oblist) shows all functions
(help) shows Subrs


