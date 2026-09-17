; test4   18/5/88
; updated 17/9/26 - fixed stale 'b:init.lsp load path (was hanging the
; interpreter) and switched progress reporting to one line per tick (edit by Claude)

; testing cons, binding and set for all list cells used ;
; this test takes about 15 mins to run

; requires init.lsp to be available

; the path 'b:init.lsp is a leftover from the BBC Micro/DOS "b:" drive
; convention and does not exist on this system - load() only warns when a
; file is missing, but the very next form (defun) then becomes an
; undefined-function call, which hangs the interpreter (see test2.lsp), so
; this silently wedged the whole process instead of running the soak test.
(load 'lisplib/init.lsp)

(print "test4: unbounded cons/gc soak test - runs until interrupted")
(print cr)
(print "reporting list length every 100 cells:")
(print cr)

(defun eq_100 (n) (eq n (* 100 (/ n 100))) )

(setq qq ())

(loop
 (setq qq (cons (length qq) qq) )
 (and (eq_100 (car qq)) (pr2 (cadr qq)) )
)

; when eval finishes will exit on eof
