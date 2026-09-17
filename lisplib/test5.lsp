; test file 5  23/5/88
; updated 17/9/26 - now loads init.lsp itself and reports each check's
; result so it's readable when this file is loaded (edit by Claude)
; testing operation of init.lsp library

; this file exercises functions defined in init.lsp (greaterp, equal,
; delete, assoc, ...), so it must be loaded standalone rather than assuming
; a caller already loaded it - otherwise those calls are undefined-function
; errors that hang the interpreter when this file is loaded rather than
; typed at the REPL (see test2.lsp).
(load 'lisplib/init.lsp)

; the REPL prints the value of every top-level form automatically; a loaded
; file does not, so each check below is passed through pr2 (from init.lsp)
; to print the result followed by a newline.

(pr2 (setq test '(a b c d e f g)))

; mapc/mapcar/apply already print via prin/print as they run; add a
; trailing newline so they don't run into the next report line
(mapc (quote prin) test) (print cr)

(mapcar (quote prin) test) (print cr)

(apply (quote print) (quote (a b 1 2 c)) ) (print cr)

(pr2 (greaterp 100 50))

(pr2 (greaterp 100 200))

(pr2 (lesserp 50 100))

(pr2 (lesserp 2000 3))

(pr2 (append test test))
(pr2 (append test ()))
(pr2 (eq test (append test ())))
(pr2 (equal test (append test ()) ))

(pr2 (equal 4 4))
(pr2 (equal test test))
(pr2 (equal (quote bb) (quote bb)))

(pr2 (oblist))

(pr2 (reverse test))
(pr2 (reverse ()))

(pr2 (delete (quote d) test))
(pr2 (member (quote d) test))

(pr2 (setq test (quote ((1 a) (2 b) (3 c) (4 d) (5 e) (6 f)) )))

(pr2 (assoc 4 test))

; testing property lists

(pr2 (put 'roger 'son 'daniel))
(pr2 (get 'roger 'son))
(pr2 (obl 1))
(pr2 (put 'roger 'another! son 'lawrence))
(pr2 (get 'roger 'another! son))
(pr2 (remprop 'roger 'son))
(pr2 (get 'roger 'son))
(pr2 (get 'roger 'another! son))
(pr2 (remprop 'roger 'another! son))
(pr2 (obl 1))

; testing unevaluated defuns

(defun adlist arg
 (set (car arg) (plus (cadr arg) (car (cddr arg))) )
)
(pr2 (adlist qq 45 35))
(pr2 qq)
