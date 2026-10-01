; test_compex_fnparam.lsp
;
; Regression test for compiled (compex 1) calls of a function held in a
; variable, (f a b) where f is a parameter or a let variable. Fixed in v3.63.
;
; Before, compex compiled such a call once and reused it:
;   - the compiled body was kept under the variable's name, so the first
;     function the variable held was called every time (a gen2 that picks
;     greaterp or lesserp from the sign of its step gave a right answer for
;     the first sign called and a wrong one for the other);
;   - the call was compiled before its arguments were bound, so a parameter
;     with the same name as a global function called the global function;
;   - a function redefined after it was compiled kept running the old body.
; Each case is checked against its answer from the interpreter (compex 0).
;
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp test/test_compex_fnparam.lsp

(load 'lisplib/init.lsp)

(setq outfh (open "test/test_compex_fnparam_output.txt" 'w))
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

(write outfh "compiled calls of a function held in a variable") (write outfh cr)
(write outfh "===============================================") (write outfh cr)

(compex 1)

; --- a let variable holding greaterp or lesserp, as gen2 in a.lsp does ---
(defun tgen (start stop step)
  (let ( ( res ()) (test
     (cond ((greaterp step 0) greaterp) ( t lesserp ) ) ) )
  (loop
    (setq res (cons start res))
    (until (test (setq start (plus start step)) stop))
  )
  (reverse res)
  )
  )
(check "A1 up" (list 1 2 3 4 5) (tgen 1 5 1))
(check "A2 then down" (list 5 4 3 2 1) (tgen 5 1 -1))
(check "A3 then up again" (list 1 2 3 4 5) (tgen 1 5 1))
(check "A4 then down again" (list 5 4 3 2 1) (tgen 5 1 -1))
(check "A5 step 2" (list 1 3 5 7 9) (tgen 1 9 2))
(check "A6 step -3" (list 10 7 4 1) (tgen 10 1 -3))

; --- the same, other order first (a fresh function, so compiled afresh) ---
(defun tgen2 (start stop step)
  (let ( ( res ()) (test
     (cond ((greaterp step 0) greaterp) ( t lesserp ) ) ) )
  (loop
    (setq res (cons start res))
    (until (test (setq start (plus start step)) stop))
  )
  (reverse res)
  )
  )
(check "B1 down first" (list 6 5 4 3 2 1) (tgen2 6 1 -1))
(check "B2 then up" (list 1 2 3 4 5 6) (tgen2 1 6 1))

; --- one function parameter given different functions on each call ---
(defun inc2 (x) (plus x 1))
(defun dbl2 (x) (times x 2))
(defun apply1 (f a) (f a))
(check "C1 first function" 6 (apply1 inc2 5))
(check "C2 second function" 10 (apply1 dbl2 5))
(check "C3 first function again" 8 (apply1 inc2 7))
(check "C4 built in function" 12 (apply1 dbl2 6))

; --- two functions whose parameters have the same name ---
(defun same1 (f a) (f a))
(defun same2 (f a) (f a))
(check "D1 first function, parameter f" 6 (same1 inc2 5))
(check "D2 second function, parameter f" 10 (same2 dbl2 5))

; --- a parameter or let variable named like a global function ---
(defun fclash (a b) 'GLOBAL)
(defun param_clash (fclash a b) (fclash a b))
(check "E1 parameter named like a global function" 3 (param_clash plus 1 2))
(defun let_clash (a b) (let ((fclash plus)) (fclash a b)))
(check "E2 let variable named like a global function" 3 (let_clash 1 2))
(check "E3 the global function itself" 'GLOBAL (fclash 1 2))

; --- a function redefined after it was compiled ---
(defun redef (x) (plus x 1))
(check "F1 before redefinition" 2 (redef 1))
(defun redef (x) (plus x 100))
(check "F2 after redefinition" 101 (redef 1))

; --- the same cases again under repeated calls (garbage collection) ---
(setq numiters 2000)
(setq i 0)
(setq badcount 0)
(loop
  (cond ((not (equal (list 1 2 3) (tgen 1 3 1))) (setq badcount (plus badcount 1))))
  (cond ((not (equal (list 3 2 1) (tgen 3 1 -1))) (setq badcount (plus badcount 1))))
  (cond ((not (equal 6 (apply1 inc2 5))) (setq badcount (plus badcount 1))))
  (cond ((not (equal 10 (apply1 dbl2 5))) (setq badcount (plus badcount 1))))
  (setq i (plus i 1))
  (until (eq i numiters))
)
(check "G1 alternating calls, zero wrong answers" 0 badcount)

(write outfh cr)
(write outfh "results: ") (write outfh pass) (write outfh " passed, ")
(write outfh fail) (write outfh " failed") (write outfh cr)
