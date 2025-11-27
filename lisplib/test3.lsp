; lisp test file 3   18/5/88
; use with standard library init.lsp

; tests lamdba functions, recursion, looping and binding

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

(floop 9)


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

(floop 20)
