; lisp test file 1   18/5/88
; updated 17/9/26 - checks now report their result when this file is
; loaded, not just when typed at the REPL (edit by Claude)
; these are test of comment lines
  ; as is this

;basic list test

; The REPL prints the value of every top-level form automatically; a loaded
; file (lisp lisplib/test1.lsp, or (load 'lisplib/test1.lsp)) does not, so
; each check below is passed through report to make the result visible.
; test1 uses only core primitives, so report is built by hand here rather
; than relying on defun/pr2, which come from init.lsp.
(setq report (quote (lambda (x) (print x) (print (implode (list 10 13))))))

(report (setq t1 '(a b c d e f)))
(report (car t1))
(report (cdr t1))
(report t1)
(report (cons (quote b) t1))
(report (setq t2 (cons ; comment in middle of expression
 (quote c) t1)))
(report t1)

(report (length t1))  ; should be 6
(report (listp t1))   ;should be true
(report (atom t1))   ; false
(report (atom 4))     ; true
(report (atom 'gg))   ;true
(report (numberp 4))  ;true
(report (numberp 'gg)); false
(report (listp ()))

(report (setq f ()))
(report (listp f)) ;true
(report (atom f)) ;true
(report (car f))   ;false
(report (cdr f))   ; false

