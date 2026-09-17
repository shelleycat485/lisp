; lisp test file 3   18/5/88
; updated 17/9/26 - now loads init.lsp itself and labels/reports the floop
; results so they're readable when this file is loaded (edit by Claude)
; use with standard library init.lsp

; tests lamdba functions, recursion, looping and binding

; make this file loadable standalone instead of assuming init.lsp was
; already loaded by the caller - without it, defun/zerop/onep/plus are
; undefined and evaluating them hangs the interpreter (see test2.lsp).
(load 'lisplib/init.lsp)

(defun fibo (n)
(cond
 ((zerop n) 1)
 ((onep n ) 1)
 ((eq n 2)  1)
 ( t (plus (fibo (- n 1)) (fibo (- n 2)) ))
))

(setq floop (quote
 ( lambda ( n )
  ( loop ( prin ( fibo  n ))
         ( prin ( quote !  ))
         ( setq  n ( -  n  1 ))
         ( until ( zerop  n ))
 ))))

(setq test_cons
 (cons (quote hhh) (cons (quote hhh) () ))
)

; floop already reports its own progress via prin as it loops; wrap each
; call with a label and a trailing newline so the output is legible when
; this file is loaded rather than typed at the REPL.
(print "recursive fibo, floop 9: ") (floop 9) (print cr)


; much faster fibo, using property lists

(defun put_fibo (n v) (put n (quote fibo) v))
(defun get_fibo (n) (get n (quote fibo)) )
(put_fibo 1 1)
(put_fibo 2 1)
(defun fibo (n)
(cond
((get_fibo n))
( t (put_fibo n (plus (fibo (- n 1)) (fibo (- n 2)))) (get_fibo n) )
))

(print "property-list cached fibo, floop 20: ") (floop 20) (print cr)
