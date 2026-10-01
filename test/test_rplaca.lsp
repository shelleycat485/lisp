; test_rplaca.lsp
;
; Regression test for rplaca (lx_rplaca in src/main.c).
;
;   A  rplaca copies its value: the variable, quoted form or function body
;      that supplied the value is not changed (fixed in v3.61).
;   B  rplaca changes the cell it is given, so it works through (cdr x),
;      (cddr x) and (car x), and a tail shared with another list shows the
;      change in both (fixed in v3.62; before, only a list held directly in
;      a variable was changed).
;   C  an empty value gives a null first element.
;
; The error branches (rplaca 5 6) etc. call report_error, which needs an
; interactive terminal, so they are not tested here.
;
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp test/test_rplaca.lsp

(load 'lisplib/init.lsp)

(setq outfh (open "test/test_rplaca_output.txt" 'w))
(setq pass 0)
(setq fail 0)

(defun check (label expected actual)
  (cond
    ((equal expected actual)
     (setq pass (plus pass 1))
     (write outfh label) (write outfh " ... PASS  got=") (write outfh actual) (write outfh cr))
    (t
     (setq fail (plus fail 1))
     (write outfh label) (write outfh " ... FAIL  expected=") (write outfh expected)
     (write outfh "  got=") (write outfh actual) (write outfh cr))))

(write outfh "rplaca regression test") (write outfh cr)
(write outfh "======================") (write outfh cr)

; --- A: the value is copied, never linked in ---
(setq v 5)
(setq l1 (list 1 2 3))
(rplaca l1 v)
(check "A1 list takes the value" (list 5 2 3) l1)
(check "A1 variable unchanged" 5 v)

(defun five () 5)
(setq l2 (list 1 2 3))
(rplaca l2 (five))
(check "A2 function result unchanged" 5 (five))

(setq sub (list 'p 'q))
(setq l3 (list 1 2 3))
(rplaca l3 sub)
(check "A3 list value becomes the element" (list (list 'p 'q) 2 3) l3)
(check "A3 list value unchanged" (list 'p 'q) sub)

; --- B: acts on the cell it is given ---
(setq l4 (list 1 2 3))
(rplaca l4 9)
(check "B1 head" (list 9 2 3) l4)

(setq l5 (list 1 2 3))
(rplaca (cdr l5) 9)
(check "B2 through cdr" (list 1 9 3) l5)

(setq l6 (list 1 2 3 4))
(rplaca (cdr (cdr l6)) 'k)
(check "B3 through cddr" (list 1 2 'k 4) l6)

(setq l7 (list (list 'a 1) (list 'b 2)))
(rplaca (car l7) 'q)
(check "B4 first element of a sublist" (list (list 'q 1) (list 'b 2)) l7)

(setq l8 (list 1 2 3))
(rplaca (cdr l8) (list 'p 'q))
(check "B5 list value through cdr" (list 1 (list 'p 'q) 3) l8)

(setq base (list 1 2 3))
(setq other (cons 0 (cdr base)))
(rplaca (cdr base) 'S)
(check "B6 shared tail, base" (list 1 'S 3) base)
(check "B6 shared tail, other" (list 0 'S 3) other)

; --- C: empty value ---
(setq l9 (list 1 2 3))
(rplaca l9 ())
(check "C1 empty value" (list () 2 3) l9)

; --- B again under garbage collection pressure ---
(setq big (list 1 2 3 4 5))
(setq numiters 3000)
(setq i 0)
(setq stressfail 0)
(loop
  (rplaca (cdr (cdr big)) i)
  (cond ((not (equal i (car (cdr (cdr big)))))
         (setq stressfail (plus stressfail 1))))
  ; extra garbage to help trigger garbage_coll() during the above
  (setq junk (list i i i i i i i i i i))
  (setq i (plus i 1))
  (until (eq i numiters))
)
(check "B7 repeated rplaca through cddr, zero mismatches" 0 stressfail)
(check "B7 rest of list intact" (list 1 2 2999 4 5) big)

(write outfh cr)
(write outfh "results: ") (write outfh pass) (write outfh " passed, ")
(write outfh fail) (write outfh " failed") (write outfh cr)
