; test_addtoalist.lsp
;
; Regression test for addtoalist, addtoalist2 and addtoalist3 in a.lsp.
;
;   A  addtoalist on a short list gives the expected list after each call:
;      a hit adds to the count and moves the entry to the front, a new
;      item is added at the front, a hit on the front entry stays there.
;   B  addtoalist2 and addtoalist3 give the same list as addtoalist after
;      each call of the same sequence.
;   C  the same on the 3000-entry list that a.lsp builds (alistvar).
;   A to C are run in compex mode 1 (as left by init.lsp) and in mode 0.
;
; addtoalist3 needs lisp 3.62 or later (rplaca on (cdr entry)).
;
; Usage (from the lisp directory):
;   lisp test/test_addtoalist.lsp

(load 'lisplib/init.lsp)
(load 'a.lsp)

(setq outfh (open "test/test_addtoalist_output.txt" 'w))
(setq pass 0)
(setq fail 0)

(defun check (label expected actual)
  (cond
    ((equal expected actual)
     (setq pass (plus pass 1))
     (write outfh label) (write outfh " ... PASS") (write outfh cr))
    (t
     (setq fail (plus fail 1))
     (write outfh label) (write outfh " ... FAIL  expected=") (write outfh expected)
     (write outfh "  got=") (write outfh actual) (write outfh cr))))

; alist ((1 101) (2 102) ... (n 100+n))
(defun shortlist (n)
  (zip (generate 1 n 1) (generate 101 (plus 100 n) 1)))

; calls addfn with each (item cnt) of calls, starting from alistvar = start;
; returns the list of alistvar after each call (copied, as later calls
; change the cells in place)
(defun runseq (addfn start calls)
  (let ((res ()))
    (setq alistvar (copylist start))
    (loop
      (while calls (reverse res))
      (addfn (car (car calls)) (cadr (car calls)))
      (setq res (cons (copylist alistvar) res))
      (setq calls (cdr calls)))))

; copies the top level and each entry, so no cell is shared with alistvar
(defun copylist (l)
  (let ((res ()))
    (loop
      (while l (reverse res))
      (setq res (cons (list (car (car l)) (cadr (car l))) res))
      (setq l (cdr l)))))

; the first n elements of l
(defun firstn (n l)
  (let ((res ()))
    (loop
      (while (and l (greaterp n 0)) (reverse res))
      (setq res (cons (car l) res))
      (setq l (cdr l))
      (setq n (- n 1)))))

; the calls: hit in the middle, new item, hit at the end, hit at the
; front, hit at the front again, new item then a hit on it
(setq calls '((2 5) (4 7) (3 1) (3 2) (4 1) (9 3) (9 4) (1 10)))

(setq expected
  '( ((2 107) (1 101) (3 103))
     ((4 7) (2 107) (1 101) (3 103))
     ((3 104) (4 7) (2 107) (1 101))
     ((3 106) (4 7) (2 107) (1 101))
     ((4 8) (3 106) (2 107) (1 101))
     ((9 3) (4 8) (3 106) (2 107) (1 101))
     ((9 7) (4 8) (3 106) (2 107) (1 101))
     ((1 111) (9 7) (4 8) (3 106) (2 107)) ))

; mixed calls on the 3000-entry list: front, back, middle, new, repeats
(setq bigcalls '((3000 1) (1 2) (1500 3) (3001 4) (2999 5) (3000 6)
                 (1 7) (3001 8) (750 9) (2250 10) (2 11) (1500 12)))

(defun runall (mode)
  (let* ((short (shortlist 3))
         (r1 (runseq addtoalist short calls))
         (r2 (runseq addtoalist2 short calls))
         (r3 (runseq addtoalist3 short calls))
         (big (zip (generate 1 3000 1) (generate 6000 9000 1)))
         (b1 ()) (b2 ()) (b3 ()))
    (check (list mode "A short list, addtoalist") expected r1)
    (check (list mode "B short list, addtoalist2 = addtoalist") r1 r2)
    (check (list mode "B short list, addtoalist3 = addtoalist") r1 r3)
    (setq b1 (last (runseq addtoalist big bigcalls)))
    (setq b2 (last (runseq addtoalist2 big bigcalls)))
    (setq b3 (last (runseq addtoalist3 big bigcalls)))
    (check (list mode "C 3000 list, length") 3001 (length b1))
    (check (list mode "C 3000 list, front entries")
           '((1500 7514) (2 6012) (2250 8259) (750 6758) (3001 12) (1 6009)
             (3000 9006) (2999 9003))
           (firstn 8 b1))
    (check (list mode "C 3000 list, addtoalist2 = addtoalist") b1 b2)
    (check (list mode "C 3000 list, addtoalist3 = addtoalist") b1 b3)))

(write outfh "addtoalist regression test") (write outfh cr)
(write outfh "==========================") (write outfh cr)

(compex 1)
(runall "mode 1:")
(compex 0)
(runall "mode 0:")
(compex 1)

(write outfh cr)
(write outfh "results: ") (write outfh pass) (write outfh " passed, ")
(write outfh fail) (write outfh " failed") (write outfh cr)
(close outfh)
