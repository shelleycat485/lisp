; closure.lsp
;
; Test and demonstration of (closure fn), defined in lisplib/init.lsp.
;
; (closure fn) takes a function (a lambda expression, or the name of a
; defun'd function) and returns a NEW SYMBOL that can be called. The
; function keeps the values that its free variables had when (closure) was
; called, even after the frame that held them has returned - something
; that an ordinary function cannot do in this dynamically scoped lisp.
;
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp closure.lsp

(load 'lisplib/init.lsp)

(setq pass 0)
(setq fail 0)

(defun check (label expected actual)
  (cond
    ((equal expected actual)
     (setq pass (plus pass 1))
     (print "PASS  ") (print label) (print "  got=") (print actual) (print cr))
    (t
     (setq fail (plus fail 1))
     (print "FAIL  ") (print label) (print "  expected=") (print expected)
     (print "  got=") (print actual) (print cr))))

(print "closure test") (print cr)
(print "============") (print cr)


; --- 1. the example from the specification -------------------------------
; func1 has a free variable k. Ordinarily k is looked up dynamically.
(defun func1 (x) (plus x k))

(setq k 1)                                   ; global k
(setq closed_func1 (let ((k 100)) (closure func1)))   ; captures k = 100
(setq k 2)                                   ; change the global afterwards

(check "closure returns a symbol" t (atom closed_func1))
(check "plain func1 still sees the global k" 7 (func1 5))
(check "closed_func1 kept k = 100" 105 (closed_func1 5))
(setq closed_func2 (let ((k 200)) (closure 'func1)))   ; quoted name works too
(check "closure of a quoted name" 205 (closed_func2 5))


; --- 2. a function that builds functions ----------------------------------
; The frame holding n has gone by the time add5 is called.
(defun make_adder (n)
  (closure '(lambda (x) (plus x n))))

(setq add5 (make_adder 5))
(setq add10 (make_adder 10))

(check "add5 1"   6 (add5 1))
(check "add10 1" 11 (add10 1))
(check "add5 again, still separate from add10" 7 (add5 2))
(check "add10 again" 20 (add10 10))


; --- 3. state that persists between calls -------------------------------
; setq inside a closure changes the closure's own private copy.
(defun make_counter (start)
  (closure '(lambda () (setq start (plus start 1)))))

(setq count_a (make_counter 0))
(setq count_b (make_counter 100))

(check "counter a first call" 1 (count_a))
(check "counter a second call" 2 (count_a))
(check "counter b is independent" 101 (count_b))
(check "counter a third call" 3 (count_a))


; --- 4. the function's own parameters are not captured -------------------
(defun make_doubler (x)              ; this x is NOT the x in the lambda
  (closure '(lambda (x) (times x 2))))

(setq doubler (make_doubler 99))
(check "own parameter x shadows the outer x" 8 (doubler 4))


; --- 5. captured lists, and more than one captured variable --------------
(defun make_scaler (lst factor)
  (closure '(lambda (i) (times factor (car (reverse lst)) i))))

(setq scaler (make_scaler '(1 2 3) 10))
(check "captured list and number" 60 (scaler 2))


; --- 6. a closure can capture a function value too -----------------------
(defun make_applier (f)
  (closure '(lambda (arg) (eval (list f (list 'quote arg))))))

(setq rev_it (make_applier 'reverse))
(setq len_it (make_applier 'length))
(check "captured function name, reverse" '(3 2 1) (rev_it '(1 2 3)))
(check "captured function name, length" 3 (len_it '(a b c)))


(print "done. pass=") (print pass) (print " fail=") (print fail)
(print cr)
