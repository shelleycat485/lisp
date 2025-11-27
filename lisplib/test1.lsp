; lisp test file 1   18/5/88
; these are test of comment lines
  ; as is this

;basic list test

(setq t1 '(a b c d e f))
(car t1)
(cdr t1)
t1
(cons (quote b) t1)
(setq t2 (cons ; comment in middle of expression 
 (quote c) t1))
t1

(length t1)  ; should be 6 
(listp t1)   ;should be true
(atom t1)   ; false
(atom 4)     ; true
(atom 'gg)   ;true
(numberp 4)  ;true
(numberp 'gg); false
(listp ())

(setq f ())
(listp f) ;true
(atom f) ;true
(car f)   ;false
(cdr f)   ; false


