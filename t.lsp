;add to alist

(if (not (defined alistvar))
  (setq alistvar (zip (generate 1 3000 1) (generate 6000 9000 1))) )

(defun addtoalist (item cnt)
   (let* ((temp (cons item cnt)) (oldval (assoc item alistvar)) )
     (cond
       (oldval 
	  (setq cnt2 (+ cnt (cadr oldval)))
	  (setq alistvar (delete oldval alistvar))
	  (setq alistvar (cons (cons item cnt2) alistvar))
	)
       ( t (setq alistvar (cons temp alistvar))
        )
     )))


; faster addtoalist: one iterative pass, updates the count in place and
; moves the entry to the front by relinking cells (no delete, no consing on a hit).
; The count is replaced with rplacd on the entry. This also works on lisp 3.61 and earlier,
; where rplaca on (cdr entry) had no effect (fixed in 3.62, see addtoalist3).
(defun addtoalist2 (item cnt)
  (let* ((prev ()) (cur alistvar) (found ()))
    (setq found
      (loop
        (while cur)
        (until (eq item (caar cur)) cur)
        (setq prev cur)
        (setq cur (cdr cur))))
    (cond
      (found
        (rplacd (car found) (list (+ cnt (cadr (car found)))))
        (cond (prev
                (rplacd prev (cdr found))
                (rplacd found alistvar)
                (setq alistvar found))))
      (t (setq alistvar (cons (cons item cnt) alistvar))))))

; as addtoalist2, but replaces the count with rplaca on (cdr entry).
; Needs lisp 3.62 or later: on 3.61 and earlier the count is silently not updated.
(defun addtoalist3 (item cnt)
  (let* ((prev ()) (cur alistvar) (found ()))
    (setq found
      (loop
        (while cur)
        (until (eq item (caar cur)) cur)
        (setq prev cur)
        (setq cur (cdr cur))))
    (cond
      (found
        (rplaca (cdr (car found)) (+ cnt (cadr (car found))))
        (cond (prev
                (rplacd prev (cdr found))
                (rplacd found alistvar)
                (setq alistvar found))))
      (t (setq alistvar (cons (cons item cnt) alistvar))))))
