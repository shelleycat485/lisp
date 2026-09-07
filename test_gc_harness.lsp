; test_gc_harness.lsp
;
; Combined stress/regression test for:
;   - cell/list garbage collection  (garbage_coll() / recmark(), src/liststor.c)
;   - string garbage collection     (string_garbage(), src/liststor.c)
;   - identifier (ID) slot reclaim  (putident()/idindex[] reuse, src/liststor.c)
;
; This is meant to be run against a build with temporarily shrunk store
; sizes (see src/listspec.h: MAXLELE/MAXID/MAXNUMIDS) so that automatic
; garbage collection is triggered repeatedly within a short loop, rather
; than requiring millions of iterations against the normal-sized store.
;
; Strategy: every iteration allocates throwaway cons-cell garbage AND a
; throwaway, distinct identifier (via explode/implode, same idiom as
; test_string_store.lsp), pressuring both collectors together. Some data
; (root_list, root_sym's property) is kept alive across the whole run and
; is periodically re-checked for exact equality - this is the regression
; check for compaction/reclaim bugs (like the charsreclaimed off-by-one
; fixed just before this harness was written) silently corrupting live
; data during a garbage collection cycle.
;
; Usage (from the lisp directory, after rebuilding with shrunk sizes):
;   lisp lisplib/init.lsp test_gc_harness.lsp

(load 'lisplib/init.lsp)
(gcollon)   ; print a line for every GC cycle so we can see it actually ran

(setq outfh (open "test_gc_harness_output.txt" 'w))
(setq pass 0)
(setq fail 0)

(defun check (label expected actual)
  (cond
    ((equal expected actual)
     (setq pass (+ pass 1))
     (write outfh label) (write outfh " ... PASS") (write outfh cr))
    (t
     (setq fail (+ fail 1))
     (write outfh label) (write outfh " ... FAIL  expected=") (write outfh expected)
     (write outfh "  got=") (write outfh actual) (write outfh cr))))

(write outfh "GC harness: cell GC + string/ID GC under shrunk-store pressure")
(write outfh cr)
(write outfh "================================================================")
(write outfh cr)

; --- roots that must survive many GC cycles unmolested ---
(setq root_list (list 1 2 3 4 5))
(setq root_sym (quote keepme_root_symbol))
(put root_sym (quote tag) 42)

(princ "before: ") (obl 2)

; lengthened from 'gcstress_' so every generated symbol (prefix + digits
; of n) exceeds SSSIZE (15 in liststor.c's SmallString) and forces the
; heap-allocation path in ss_store(), not just the inline buffer.
(setq basechars (explode 'gcstress_heaptest_))

(defun check_sym (n namechars sym)
  (cond ((not (equal (explode sym) namechars))
         (setq fail (+ fail 1))
         (write outfh "id corruption at n=") (write outfh n) (write outfh cr)))
  sym)

(defun build_sym (n namechars)
  (check_sym n namechars (implode namechars)))

(defun mk_and_check_sym (n)
  (build_sym n (append basechars (explode n))))

(setq numiters 6000)
(setq checkevery 250)
(setq i 0)
(setq sincecheck 0)
(loop
  (setq junk (list i i i i i i i i i i))         ; cell-store garbage
  (setq lastsym (mk_and_check_sym i))             ; id/string-store garbage
  (setq sincecheck (+ sincecheck 1))
  (cond ((eq sincecheck checkevery)
         (check "root_list intact mid-run" (list 1 2 3 4 5) root_list)
         (check "root_sym prop intact mid-run" 42 (get root_sym (quote tag)))
         (setq sincecheck 0)))
  (setq i (+ i 1))
  (until (eq i numiters))
)

(check "final root_list intact" (list 1 2 3 4 5) root_list)
(check "final root_sym prop intact" 42 (get root_sym (quote tag)))

; force one final explicit collection and confirm roots still hold, plus
; print bounded id/char usage (proof reclaim happened, not just leaked
; until process exit)
(gcoll)
(check "root_list intact after final gcoll" (list 1 2 3 4 5) root_list)
(check "root_sym prop intact after final gcoll" 42 (get root_sym (quote tag)))

(princ "after: ") (obl 2)

(write outfh cr)
(write outfh "results: ") (write outfh pass) (write outfh " passed, ")
(write outfh fail) (write outfh " failed") (write outfh cr)

(princ "done. pass=") (princ pass) (princ " fail=") (princ fail) (print cr)
(princ "output written to test_gc_harness_output.txt") (print cr)
(close outfh)
(exit)
