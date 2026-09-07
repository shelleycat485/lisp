; test_string_store.lsp
;
; Stress test for the identifier (string) storage garbage collector:
; putident(), string_garbage(), getident() in src/liststor.c.
;
; Builds a large number of DISTINCT, short-lived atoms by exploding a
; base name and a numeric counter into characters, then imploding them
; back together (symstress_0, symstress_1, ... symstress_N-1). None of
; the generated atoms are bound anywhere, so once the cons cell holding
; one is swept by the cons-cell GC, string_garbage() is free to reclaim
; and reuse its slot in idindex[]/idstore[] on a later call - this
; exercises both the free-slot search loop in putident() and the
; mark/compact loop in string_garbage() (the memmove compaction path
; in particular).
;
; Each generated symbol is exploded back apart and compared against the
; character list it was built from, as a cheap corruption check.
;
; Usage (from the lisp directory):
;   lisp stdload.lsp test_string_store.lsp
;
; Raise numids toward MAXNUMIDS (256000) / MAXID (500000 chars, see
; src/listspec.h) to push closer to the hard limits.

(setq numids 100000)     ; how many distinct identifiers to generate
(setq reportevery 5000)  ; progress print interval
(setq basechars (explode 'symstress_))

(setq gensym_and_check (quote
  (lambda (n)
    (let ((namechars (append basechars (explode n))))
      (let ((sym (implode namechars)))
        (cond ((not (equal (explode sym) namechars))
               (princ "MISMATCH at n=") (princ n)
               (princ " got=") (print sym)))
        sym)))))

(setq i 0)
(setq sincereport 0)
(loop
  (setq lastsym (gensym_and_check i))
  (setq sincereport (+ sincereport 1))
  (cond ((eq sincereport reportevery)
         (princ "generated ") (princ i) (princ " ids so far, last=")
         (print lastsym)
         (setq sincereport 0)))
  (setq i (+ i 1))
  (until (eq i numids))
)

(princ "done: generated ") (princ numids) (princ " distinct identifiers.")
(print cr)
(princ "last identifier created: ") (print lastsym)
(print cr)
