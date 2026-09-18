; ycombinator_v331.lsp
;
; A "classic-style" Y-combinator for R Haxby's Lisp interpreter -- but it
; ONLY works on versions up to and including v3.31. It breaks starting at
; v3.32 (commit 729c70d, "sear_oblist: bound binding-list search to the
; redefinition-check path only"), which is why the companion file
; ycombinator.lsp (in the main tree) needs the `apply`-based workaround
; instead. See that file's header for the general background on this
; interpreter's dynamic scoping.
;
; The difference from ycombinator.lsp: up to v3.31, a symbol used in
; OPERATOR position -- e.g. (x x) -- is resolved by an UNBOUNDED search of
; the whole live binding list (binlptr), not just the current call frame.
; That means a variable bound several lambdas up can still be called
; directly, without going through `apply`, as long as we're still nested
; inside the call that bound it (never "returned and re-entered"). This
; lets the combinator be written with direct calls, closer to the textbook
; applicative-order Y (Z) combinator:
;
;   Z = lambda f. (lambda x. x x) (lambda x. f (lambda v. (x x) v))
;
; Verified empirically against a live v3.31 build (git worktree checked out
; at tag v3.31, rebuilt from src/):
;   - cross-frame operator calls like (x v), from a nested lambda, resolve
;     fine on v3.31 -- and fail ("non translatable list name") on v3.37.
;   - this file's Y, unchanged, computes 6! = 720 and fib(10) = 55 on
;     v3.31, and fails the same way v3.37 does if run there.
;
; Note this still can't be the fully classic Y (returning a reusable
; closure you call later): this interpreter has no closures at all, and
; every lambda's bindings are popped the moment its call returns, on every
; version. So Y here is still uncurried -- it computes Y(f, n) as one
; continuous nested call -- but unlike ycombinator.lsp it needs no `apply`
; calls, only direct application, which is the more "textbook" shape.
;
; This file will NOT run correctly under the `lisp` on your PATH (that's
; v3.37 or later). Build v3.31 separately and run it with that binary, e.g.
; from a git worktree of this repo checked out at tag v3.31:
;   git worktree add ../lisp_v331 v3.31
;   cd ../lisp_v331/src && make && cp ~/bin/lisp ~/bin/lisp_v331
;   cd .. && ~/bin/lisp_v331 stdload.lsp ycombinator_v331.lsp
; (the plain `make` target moves/installs its output to ~/bin and
; /usr/local/bin, overwriting the live lisp -- rebuild the main tree
; afterwards to restore it, as was done when this file was verified.)

(defun Y (f n)
  ((lambda (x n) (x x n))
   (quote (lambda (x n) (f (quote (lambda (v) (x x v))) n)))
   n)
)

; --- demo: anonymous lambdas, no defun name needed ---

(print "factorial via Y (v3.31-style), unnamed lambda: (Y ... 6) = ")
(print (Y (quote (lambda (self n)
                    (cond
                      ((zerop n) 1)
                      (t (* n (self (- n 1)))))))
          6))
(print cr)

(print "fibonacci via Y (v3.31-style), unnamed lambda: (Y ... 10) = ")
(print (Y (quote (lambda (self n)
                    (cond
                      ((or (eq n 0) (eq n 1)) n)
                      (t (+ (self (- n 1)) (self (- n 2)))))))
          10))
(print cr)
