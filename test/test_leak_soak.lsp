; test_leak_soak.lsp
;
; Long-running leak-detection harness. Unlike test_gc_harness.lsp
; (which proves things get reclaimed correctly under GC pressure in
; a fairly short run) this is aimed at proving reclaim keeps working
; correctly over a SUSTAINED run: string-store (atom) leaks,
; cell-store leaks, and property-list leaks would all show up as
; slowly climbing resource usage over many thousands of cycles, not
; necessarily in a short one.
;
; Three things are exercised every iteration:
;   1. string storage: a fresh, genuinely-discarded atom (via
;      makesym) -- created then immediately dropped, unlike
;      bench_atomstore.lsp which deliberately keeps every atom alive.
;      If string_garbage() ever failed to reclaim these, the
;      "Chars .. Ids .." figures from (obl 2) would climb without
;      bound over the run instead of staying roughly flat.
;   2. cell storage: a throwaway list, discarded each iteration --
;      same idea, for the cons-cell store. A real cell leak shows up
;      as a shrinking "reclaimed" count in the periodic
;      "Garbage collection N, of X cells, Y reclaimed" lines
;      ((gcollon) is on), trending toward "cannot reclaim any cells".
;   3. property lists: a small, fixed set of "target" symbols get
;      two properties put/got/remproped every iteration, with
;      explicit correctness checks after each remprop -- that the
;      removed property is really gone, and that its sibling
;      property on the same symbol is untouched.
;
; Progress/leak-visibility: every checkevery iterations, prints
; (obl 2)'s atomstore stats. Inspect these over a run: a real leak
; shows as steadily climbing figures, not a bounded oscillation.
;
; Usage:
;   lisp lisplib/init.lsp test/test_leak_soak.lsp
; Edit numiters below for a longer/shorter soak (default here is a
; genuine long-running soak, not a quick smoke test).

(load 'lisplib/init.lsp)
(gcollon)

(setq outfh (open "test/test_leak_soak_output.txt" 'w))
(setq pass 0)
(setq fail 0)

; silent on success (this runs many checks per iteration -- printing
; a PASS line for each would dominate the run with I/O)
(defun check (label expected actual)
  (cond
    ((equal expected actual)
     (setq pass (+ pass 1)))
    (t
     (setq fail (+ fail 1))
     (write outfh label) (write outfh " ... FAIL  expected=") (write outfh expected)
     (write outfh "  got=") (write outfh actual) (write outfh cr))))

; --- roots that must survive the whole run unmolested ---
(setq root_list (list 1 2 3 4 5))
(setq root_sym (quote leaksoak_root_symbol))
(put root_sym (quote tag) 42)

; --- fixed set of symbols that live for the whole run, repeatedly
; --- given and stripped of properties -- this is where a
; --- property-list leak would show up
(defun plist_cycle1 (sym n)
  (put sym (quote propone) n)
  (put sym (quote proptwo) (+ n 1))
  (check "propone set" n (get sym (quote propone)))
  (check "proptwo set" (+ n 1) (get sym (quote proptwo)))
  (remprop sym (quote propone))
  (check "propone gone after remprop" () (get sym (quote propone)))
  (check "proptwo survives sibling remprop" (+ n 1) (get sym (quote proptwo)))
  (remprop sym (quote proptwo))
  (check "proptwo gone after remprop" () (get sym (quote proptwo)))
)

(defun plist_cycle (n)
  (plist_cycle1 (quote leaktarget_a) n)
  (plist_cycle1 (quote leaktarget_b) n)
  (plist_cycle1 (quote leaktarget_c) n)
)

(setq numiters 50000)
(setq checkevery 5000)
(setq i 0)
(setq sincecheck 0)
(loop
  (setq junk (list i i i i i i i i i i))       ; cell-store churn, discarded
  (setq throwaway (makesym))                    ; string-store churn, discarded
  (plist_cycle i)                                ; property-list churn
  (setq sincecheck (+ sincecheck 1))
  (cond ((eq sincecheck checkevery)
         (check "root_list intact mid-run" (list 1 2 3 4 5) root_list)
         (check "root_sym prop intact mid-run" 42 (get root_sym (quote tag)))
         (princ "iter ") (princ i) (princ ": ") (obl 2)
         (setq sincecheck 0)))
  (setq i (+ i 1))
  (until (eq i numiters))
)

(check "final root_list intact" (list 1 2 3 4 5) root_list)
(check "final root_sym prop intact" 42 (get root_sym (quote tag)))

(gcoll)
(check "root_list intact after final gcoll" (list 1 2 3 4 5) root_list)
(check "root_sym prop intact after final gcoll" 42 (get root_sym (quote tag)))

(write outfh cr)
(write outfh "results: ") (write outfh pass) (write outfh " passed, ")
(write outfh fail) (write outfh " failed") (write outfh cr)

(princ "done. pass=") (princ pass) (princ " fail=") (princ fail) (print cr)
(princ "final: ") (obl 2)
(princ "output written to test/test_leak_soak_output.txt") (print cr)
(close outfh)
(exit)
