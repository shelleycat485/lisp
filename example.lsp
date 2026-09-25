; example.lsp
;
; Demonstrates an argument binding bug in the interpreter, fixed in v3.41
; (up to v3.40 cases 1, 2 and 5 print WRONG; from v3.41 all print ok).
; Case 6 shows the same fault in let, fixed in v3.42.
;
; Up to v3.40, when a lambda was applied, each formal parameter was bound as soon as its
; actual argument has been evaluated, before the next argument is
; evaluated. So a later argument that referred to a variable with the same
; name as an earlier formal parameter saw the new value, not the
; caller's value:
;
;   (defun f (a b) (list a b))
;   (defun g (a b) (f 'x a))
;   (g 'one 'two)   ; gave (x x) up to v3.40, (x one) from v3.41
;
; Cause (v3.40): lambda_bind() in src/main.c evaluated an actual argument with
; lx_eval() and then immediately pushed the binding onto binlptr, inside
; the same loop. v3.41 evaluates all the actual arguments first
; (holding the bindings on a pending list), then pushes them all.
;
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp example.lsp

(load 'lisplib/init.lsp)

(defun show (label expected actual)
  (print label) (print cr)
  (print "   expected ") (print expected) (print cr)
  (print "   actual   ") (print actual)
  (cond
    ((equal expected actual) (print "   ok"))
    ( t (print "   WRONG"))
  )
  (print cr)
)

(print "argument binding bug demonstration") (print cr) (print cr)


; 1. minimal case: inside g, the argument a to f should be g's a (one)
(defun f (a b) (list a b))
(defun g (a b) (f 'x a))
(show "1. (g 'one 'two) calls (f 'x a)" '(x one) (g 'one 'two))


; 2. no enclosing function needed, a global is hidden in the same way
(setq a 'global)
(show "2. top level (f 'x a) with a = global" '(x global) (f 'x a))


; 3. only later arguments are affected: here q is the SECOND formal, so it
;    is not yet bound while the first argument is evaluated
(defun h (p q) (list p q))
(setq q 'outer)
(show "3. (h q 'y) with q = outer" '(outer y) (h q 'y))


; 4. workaround: formal parameter names that no caller uses
(defun f2 (fa fb) (list fa fb))
(defun g2 (a b) (f2 'x a))
(show "4. renamed params, (g2 'one 'two)" '(x one) (g2 'one 'two))


; 5. how it shows up in real code (as found in redblacktree.lsp): one call
;    to a node maker nested inside another. The outer mk binds v to top
;    before the inner (mk v () ()) is evaluated, so the inner node gets
;    top instead of leaf.
(defun mk (v l r) (list v l r))
(defun wrap (v) (mk 'top (mk v () ()) ()))
(show "5. nested node building, (wrap 'leaf)" '(top (leaf () ()) ()) (wrap 'leaf))

; 6. let had the same fault up to v3.41: each (var val) was bound before
;    the next val was evaluated, so let acted like let*. From v3.42 all
;    the vals are evaluated first, so b gets the outer a.
(setq a (quote outer))
(show "6. (let ((a (quote inner)) (b a)) (list a b))" (quote (inner outer))
      (let ((a (quote inner)) (b a)) (list a b)))

(exit)
