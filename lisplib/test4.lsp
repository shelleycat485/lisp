; test4   18/5/88

; testing cons, binding and set for all list cells used ;
; this test takes about 15 mins to run

; requires init.lsp to be available

(load 'b:init.lsp)

(defun eq_100 (n) (eq n (* 100 (/ n 100))) )

(setq qq ())

(loop
 (setq qq (cons (length qq) qq) )
 (and (eq_100 (car qq)) (prin (cadr qq)) )
)

; when eval finishes will exit on eof