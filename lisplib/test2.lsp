; lisp test file 2   18/5/88
; updated 17/9/26 - now loads init.lsp itself (previously hung the
; interpreter when loaded standalone) and reports each result (edit by Claude)
; arithmetic tests

; plus/times/difference/divide/zerop/onep are library aliases defined in
; init.lsp, not primitives, so this file cannot run standalone without it -
; loading it here also means the checks below no longer depend on the
; caller having loaded init.lsp first (e.g. via stdload.lsp).
; NOTE: previously, loading this file without init.lsp already present would
; evaluate (plus ...) etc as an undefined function and hang the interpreter:
; the "Abort/Trace/Return" error prompt reads from the interactive prompt
; pipe, which does not exist yet while the initial set of files is being
; loaded, so the error handler blocks forever with no way to respond.
(load 'lisplib/init.lsp)

; the REPL prints the value of every top-level form automatically; a loaded
; file does not, so each check below is passed through pr2 (from init.lsp)
; to print the result followed by a newline.

(pr2 (plus ))
(pr2 (plus 5 6))
(pr2 (plus 1 2 3 4 5 6))

(pr2 (times 8 8))
(pr2 (times 4 4 4))
(pr2 (times 1 2 3 4))

(pr2 (difference 5 4))
(pr2 (difference 100 200 50))
(pr2 (difference 0 100))

(pr2 (minusp -3))
(pr2 (minusp 3))

(pr2 (divide 1000 20))
(pr2 (divide 1000 20 4))

; these two intentionally divide by zero: the interpreter prints its own
; "attempt to divide by 0" message unconditionally (in both the REPL and a
; loaded file), so that message is already this line's report - left
; unwrapped here since wrapping it in pr2 would add nested calls that pick
; up the error's leftover debug-trace flag and print noisy internal detail
(divide 0 0)
(divide 100 0)

(pr2 (divide 0 4))
(pr2 (divide 200 -6))

(pr2 (setq n0 0))
(pr2 (setq n1 1))

(pr2 (zerop n0))
(pr2 (onep n1))
(pr2 (onep n1))
(pr2 (onep n0))
(pr2 (zerop -100))
(pr2 (onep 56))
