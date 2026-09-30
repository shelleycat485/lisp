; test_mark_balance.lsp
;
; Regression test for the mark_req/mark_not balance fixes made this
; session to lx_remprop, lx_rplaca and lx_rplacd in src/main.c.
;
; This only exercises the valid-argument (success) paths of the three
; functions, run in a loop long enough to force garbage_coll() to run
; repeatedly while cells returned by them are still live. The bug
; that was actually fixed lived in the *error* branches (bad arg
; count / wrong type) - those call report_error(), which drops into
; an interactive Abort/Trace/Return prompt reading from the same pipe
; the forked REPL writes into. That can only be triggered safely by
; running `lisp` interactively yourself, not from a batch-loaded
; script like this one (report_error would block waiting on input
; that can never arrive during file load). See the note written to
; the end of the output file.
;
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp test_mark_balance.lsp

(load 'lisplib/init.lsp)

(setq outfh (open "test/test_mark_balance_output.txt" 'w))
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

(write outfh "mark_req/mark_not regression test") (write outfh cr)
(write outfh "===================================") (write outfh cr)

; --- basic put/get/remprop ---
(setq s1 (quote symA))
(put s1 (quote propX) 111)
(check "get after put" 111 (get s1 (quote propX)))
(remprop s1 (quote propX))
(check "get after remprop" () (get s1 (quote propX)))

; --- basic rplaca ---
(setq lst1 (list 1 2 3))
(rplaca lst1 99)
(check "rplaca replaces car" (list 99 2 3) lst1)

; --- basic rplacd ---
; note: from v3.61 a list arg's elements become the rest of the list,
; as cons makes it - (rplacd '(1 2 3) '(8 9)) => (1 8 9). Before that
; the list was linked in as one nested element, (1 (8 9)).
(setq lst2 (list 1 2 3))
(rplacd lst2 (list 8 9))
(check "rplacd replaces cdr" (list 1 8 9) lst2)

(write outfh cr)
(write outfh "stress loop: put/get/remprop/rplaca/rplacd under GC pressure")
(write outfh cr)

; --- stress loop, forcing many garbage_coll() cycles while these
; functions' returned/marked cells are still on the C stack ---
(setq anchor (list 'stressfail 'pass 'numiters 'i))
(setq numiters 3000)
(setq i 0)
(setq stressfail 0)
(loop
  (setq sym (implode (append (explode (quote stress_)) (explode i))))
  (put sym (quote markprop) i)
  (cond ((not (equal i (get sym (quote markprop))))
         (setq stressfail (plus stressfail 1))))
  (remprop sym (quote markprop))
  (cond ((not (equal () (get sym (quote markprop))))
         (setq stressfail (plus stressfail 1))))
  (setq lst (list 1 2 3))
  (rplaca lst (* i 2))
  (cond ((not (equal (* i 2) (car lst)))
         (setq stressfail (plus stressfail 1))))
  (rplacd lst (list i i))
  (cond ((not (equal (list i i) (cdr lst)))
         (setq stressfail (plus stressfail 1))))
  ; extra garbage to help trigger garbage_coll() during the above
  (setq junk (list i i i i i i i i i i))
  (setq i (plus i 1))
  (until (eq i numiters))
)
(check "stress loop had zero mismatches" 0 stressfail)

(write outfh cr)
(write outfh "results: ") (write outfh pass) (write outfh " passed, ")
(write outfh fail) (write outfh " failed") (write outfh cr)
(write outfh cr)
(write outfh "NOTE: the error branches of remprop/rplaca/rplacd (the")
(write outfh cr)
(write outfh "code paths that were actually leaking a mark before this")
(write outfh cr)
(write outfh "session's fix) are not exercised here - they call")
(write outfh cr)
(write outfh "report_error(), whose Abort/Trace/Return prompt needs a")
(write outfh cr)
(write outfh "real interactive terminal. To check those by hand, run")
(write outfh cr)
(write outfh "lisp interactively and try e.g. (rplaca 5 6), (rplacd 5 6),")
(write outfh cr)
(write outfh "(remprop) - each should print the error and let you")
(write outfh cr)
(write outfh "continue (Trace/Return) without the interpreter wedging.")
(write outfh cr)

(close outfh)

(princ "done. pass=") (princ pass) (princ " fail=") (princ fail)
(print cr)
(princ "output written to test/test_mark_balance_output.txt")
(print cr)
