; test_compex_redefine.lsp
;
; Regression test for compiled calls of a function that has since been
; redefined, or of a name that has since been given another function.
;
; Before, a compiled call site kept calling the body it was compiled with:
; a redefined callee still ran its old body from a compiled caller, and
; a global set to another function, (setq op lesserp), still called the
; first one, until (compex 4). Now the compiled code belongs to the
; definition (its lambda cell) and each compiled call checks the name's
; current definition first (in full only after a set that gave or took
; away a function, or for a name that has been bound as a variable).
;
; The cases are in the function cases, so they run compiled (and the
; defuns in it are made by compiled code): twice in mode 1, again after
; (compex 4), then interpreted in mode 0. All must give the same answers.
;
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp test/test_compex_redefine.lsp

(load 'lisplib/init.lsp)

(setq outfh (open "test/test_compex_redefine_output.txt" 'w))
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

(write outfh "compiled calls of redefined functions") (write outfh cr)
(write outfh "=====================================") (write outfh cr)

; callers, defined once; their callees are (re)defined in cases
(defun rcall () (ra))
(defun rcb (n) (rb n 10))
(defun callfact (n) (rfact n))
(defun useop (a b) (op a b))
(defun callself () (selfr))
(defun callrl () (rl))

(defun cases (tag)
  (let ((k 0) (bad 0))
    ; A: callee redefined after its caller was compiled
    (defun ra () 'first)
    (check (list tag "A1 first definition") 'first (rcall))
    (defun ra () 'second)
    (check (list tag "A2 redefined") 'second (rcall))
    ; B: more formals, fewer formals, (defun f lis ...), and back
    (defun rb (x) (+ x 1))
    (check (list tag "B1 one formal (extra arg ignored)") 6 (rcb 5))
    (defun rb (x y) (+ x y))
    (check (list tag "B2 two formals") 15 (rcb 5))
    (defun rb lis (length lis))
    (check (list tag "B3 (defun rb lis ...)") 2 (rcb 5))
    (defun rb (x y) (- x y))
    (check (list tag "B4 two formals again") -5 (rcb 5))
    ; C: a recursive function redefined
    (defun rfact (n) (cond ((eq n 0) 1) (t (* n (rfact (- n 1))))))
    (check (list tag "C1 factorial") 120 (callfact 5))
    (defun rfact (n) (cond ((eq n 0) 0) (t (+ n (rfact (- n 1))))))
    (check (list tag "C2 redefined as a sum") 15 (callfact 5))
    ; D: a global given another function, and back
    (setq op greaterp)
    (check (list tag "D1 op greaterp") t (useop 5 1))
    (setq op lesserp)
    (check (list tag "D2 op lesserp") () (useop 5 1))
    (setq op greaterp)
    (check (list tag "D3 op greaterp again") t (useop 5 1))
    (setq op lesserp)
    (check (list tag "D4 op lesserp again") () (useop 5 1))
    ; E: a function that redefines itself while it runs
    (defun selfr () (defun selfr () 'new) 'old)
    (check (list tag "E1 first call") 'old (callself))
    (check (list tag "E2 second call") 'new (callself))
    ; F: redefined 60 times in a loop, each called through a compiled caller
    (setq k 0)
    (loop
      (until (eq k 60))
      (eval (list 'defun 'rl () k))
      (cond ((not (eq k (callrl))) (setq bad (+ bad 1))))
      (setq k (+ k 1)))
    (check (list tag "F1 60 redefinitions, wrong answers") 0 bad)
    ; G: garbage collections ((obl 3)) between redefinitions: the old
    ; definitions keep their cells while they have compiled code
    (defun ra () 'before)
    (check (list tag "G1 before a garbage collection") 'before (rcall))
    (obl 3)
    (check (list tag "G2 after it") 'before (rcall))
    (defun ra () 'after)
    (obl 3)
    (setq k 0)
    (loop
      (until (eq k 2000))
      (setq junk (list k k k k k k k k k k))
      (setq k (+ k 1)))
    (check (list tag "G3 redefined, collected, new garbage") 'after (rcall))
    (setq op greaterp)
    (obl 3)
    (check (list tag "G4 op greaterp after a collection") t (useop 5 1))))

(compex 1)
(cases "mode 1:")
(cases "mode 1 again:")
(compex 4)
(cases "after (compex 4):")
(compex 0)
(cases "mode 0:")
(compex 1)

; H: enough redefinitions to fill the store for compiled code; the rest
; are interpreted, and all still give the right answer
(setq k 0)
(setq bad 0)
(loop
  (until (eq k 4000))
  (eval (list 'defun 'rl () k))
  (cond ((not (eq k (callrl))) (setq bad (+ bad 1))))
  (setq k (+ k 1)))
(setq st (compex))
(check "H1 4000 redefinitions, wrong answers" 0 bad)
(check "H2 store full" t (greaterp (car st) 60000))
(obl 3)
(check "H3 rl still right after a collection" 3999 (callrl))
(compex 4)
(setq st (compex))
(obl 3)
(check "H4 store empty after (compex 4)" 0 (car st))
(defun ra () 'last)
(check "H5 compiled again after (compex 4)" 'last (rcall))

; I: a global function's name bound as a parameter by a caller: calls of
; it made while the binding lasts (dynamic scope) get the bound function
(defun rk () 'global)
(defun other () 'other)
(defun callrk () (rk))
(defun withrk (rk) (callrk))
(check "I1 global" 'global (callrk))
(check "I2 bound to another function" 'other (withrk other))
(check "I3 global again" 'global (callrk))
(check "I4 bound again" 'other (withrk other))
(check "I5 bound to itself" 'global (withrk rk))

(write outfh cr)
(write outfh "results: ") (write outfh pass) (write outfh " passed, ")
(write outfh fail) (write outfh " failed") (write outfh cr)
(close outfh)
