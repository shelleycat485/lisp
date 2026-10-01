# lisp

## LISP Interpreter 

This is an in-memory, garbage collected, LISP interpreter. It was originally
based on the BBC Micro Lisp version, and is written in C, originally on an 
IBM-PC. (Functions can also be compiled.) 

Extensions are Turtle graphics, using X11.  Hence X11 is needed to link.
A complete environment can be loaded or saved using the load and save functions 
(load is built in, save is written in lisp).

File reading and writing is supported.  No network connectivity.  A System primitive allows shell execution.

Tested on Linux, Ubuntu and Devuan. Also Debian (Raspian) on ARM64.

> Quotes are either ( quote word ) or '(quoted list)  or "text" .
> Also a ; in source is a comment to end of line,
 and a ! in text escapes the space.
> So 'some! text  is a single string.

## Build and install.

### Prerequisites are:

GNU Make 4.3

libx11-dev

GCC 12.2.0 or above

### Build 

> cd lisp/src

> make clean

> make lisp

> make install

### Running

lisp [files to load]

```
lisp stdload.lsp a.lsp tri2.lsp
```

Useful ones are in lisp/lisplib

stdload.lsp is in the lisp directory for convenience. An environment variable LISPLIB=stdload.lsp loads that file on start.  File loads can be nested.

Lisp source can be loaded using the function (load 'filename.lsp). To save the entire environment, (save 'yourenv.lsp); save is defined in lisplib/writer.lsp.

(oblist) shows all current functions

(help) shows Subrs (built ins)

### Compilation

Functions can be compiled; controlled by the compex function.

Every numeric form except a bad number returns a list of the compiled-code store, `(bytes-used store-size)`, for example `(1234 65536)`. That list does not include the mode.

- `(compex 0)` switches to interpret only. Returns `(bytes-used store-size)`.
- `(compex 1)` switches to compiled: functions are compiled on first call and the compiled code runs. Returns `(bytes-used store-size)`.
- `(compex 2)` switches to both: each call runs compiled and interpreted, and it warns if the results differ. Calls nested inside it run interpreted only. Returns `(bytes-used store-size)`.
- `(compex 4)` clears all compiled code and resets the compiler's flags, and the mode stays as it was. Returns `(0 65536)`, because the store is now empty. If compiled code is running, it is refused with the error "cannot clear compiled code while it is running".
- `(compex)` leaves the mode as it is. Returns `(bytes-used store-size)`.
- `(compex 3)` or any other number gives the error "mode must be 0, 1 or 2, or 4 to clear".
- `(compex form)` with a non-numeric argument compiles that form once, runs it and returns the form's own result. If `LISPCSPRINT` is set, it also runs the form interpreted and prints both results and the compile trace. If it is called from inside running compiled code, it just interprets the form.

On a host where compiled code isn't supported, the status list is `(0 0)`.

### Garbage Collection

List cells are garbage collected when needed. A mark/sweep collector is used. Strings are not garbage collected so a long run with many strings may exhaust storage.
