; bench_atomstore.lsp
;
; Baseline benchmark for the atom-store search (srchident()/putident() in
; src/liststor.c). Generates numatoms distinct, permanently-referenced
; atoms via makesym (lisplib/init.lsp), so the atomstore genuinely
; accumulates numatoms *live* entries -- none are ever discarded/GC'd,
; so srchident's linear scan has to search further as numatoms grows.
;
; No in-script timing: run the whole process externally under `time`,
; at a few increasing values of numatoms, and compare total elapsed
; time growth. A linear-scan search should show roughly quadratic
; total-time growth (doubling numatoms ~quadruples the time).
;
; Requires SMALLSTORE 0 (production store sizes) -- the small test
; store (MAXATOMS=1000) is nowhere near big enough.
;
; Usage:
;   time lisp lisplib/init.lsp bench_atomstore.lsp

(load 'lisplib/init.lsp)

(setq numatoms 10000)   ; <-- edit this between runs

(setq allsyms ())
(setq k 0)
(loop
  (setq allsyms (cons (makesym) allsyms))   ; kept alive, never reclaimed
  (setq k (+ k 1))
  (until (eq k numatoms))
)

(princ "created ") (princ numatoms) (princ " atoms") (print cr)
(exit)
