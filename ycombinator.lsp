; ycombinator.lsp
; A Y-combinator for R Haxby's Lisp interpreter.
;
; This interpreter is DYNAMICALLY scoped and has no closures: a lambda's
; free variables are resolved by walking the live call stack (binlptr) at
; call time, not by capturing an environment at creation time. Two extra
; wrinkles specific to this implementation, found by testing on the running
; binary (version 3.37):
;
;   1. A symbol used in OPERATOR position -- e.g. (x x) -- is only looked up
;      in the CURRENT call frame (this is the binl_floor bound added for the
;      sear_oblist redefinition-check optimisation). So (x x) only works if
;      x was bound by the immediately enclosing lambda/let. Calling a
;      variable-held function from a *nested* lambda must instead go through
;      the primitive `apply`, since apply splices the function's VALUE
;      directly into operator position of a freshly built form -- that's a
;      structural "(lambda ...) ..." match, not a symbol lookup, so it
;      works at any call depth.
;   2. A bare (lambda ...) expression cannot be evaluated as a value (it
;      errors "Lambda cannot be used directly"). A lambda must always
;      either sit directly in operator position, or be wrapped in `quote`
;      when passed around as data.
;   3. A plain VALUE reference (a variable used as an argument, not as the
;      operator) is NOT frame-bounded -- it searches the whole live call
;      chain. So as long as the whole computation stays inside one
;      continuous nested call (never "returns out and gets re-entered
;      later"), outer variables remain visible to inner lambdas.
;
; Putting these together: Y is written uncurried, computing Y(f, n) in a
; single nested call chain rather than building/returning a reusable
; closure (which would have to survive past the call that built it, and
; this interpreter has no closures to make that survive).
;
;   gen = (lambda (x n) (apply f (list (quote (lambda (v) (apply x (list x v)))) n)))
;   Y(f, n) = apply(gen, (list gen n))
;
; f is called as f(self, n), where `self` is a callback that -- when later
; applied to v, from *inside* f's own dynamic extent -- re-derives the
; whole structure via (apply x (list x v)) and recurses. Since every
; recursive step is a nested call within the previous one, `x` and `f`
; stay reachable as plain value lookups all the way down.

(defun Y (f n)
  (let ( (gen (quote (lambda (x n)
                        (apply f (list (quote (lambda (v) (apply x (list x v)))) n))))) )
    (apply gen (list gen n))
  )
)

; --- demo: recursion for completely anonymous lambdas, no defun name needed ---

(print "factorial via Y, unnamed lambda: (Y ... 6) = ")
(print (Y (quote (lambda (self n)
                    (cond
                      ((zerop n) 1)
                      (t (* n (apply self (list (- n 1)))))
                    )))
          6))
(print cr)

(print "fibonacci via Y, unnamed lambda: (Y ... 10) = ")
(print (Y (quote (lambda (self n)
                    (cond
                      ((or (eq n 0) (eq n 1)) n)
                      (t (+ (apply self (list (- n 1))) (apply self (list (- n 2)))))
                    )))
          10))
(print cr)
