(setq space '! )
(setq lpar '!()
(setq rpar '!))

; lines written by save are kept to wr_width characters or less:
; wr_item starts a new line before an element that would go past it.
; Only a line holding one long atom (or ")" closing lists) goes past.
; Breaks are only between elements, so the file reads back the same.
(setq wr_width 72)
(setq wr_col 0)

; writes exp to fname as writec does, breaking lines between elements
; wr_col is the length of the line written so far
(defun wr_item (fname exp)
  (let ((len 1))
    (cond ((null exp) (setq len 2))
          ((atom exp) (setq len (+ 2 (length (explode exp))))))
    (cond ((and (lesserp 0 wr_col) (lesserp wr_width (+ wr_col len)))
           (writen fname cr) (setq wr_col 0)))
    (setq wr_col (+ wr_col len))
    (cond ((atom exp) (writec fname exp))
          (t (writen fname lpar)
             (loop (while exp)
               (wr_item fname (car exp))
               (setq exp (cdr exp)))
             (writen fname rpar) (setq wr_col (+ wr_col 1))))))

(setq wr_exp3 (quote
 (lambda (fname exp)
     ( writen  fname lpar) (write fname (quote setq))
     ( writec  fname  exp )
     ( writen  fname lpar) (writen fname (quote quote))
     (writen fname cr)
     (setq wr_col 0)
     ( wr_item  fname ( eval  exp ))
     (writen fname cr)
     ( writen  fname rpar) (writen fname rpar)
     ( writen  fname cr)
)))

; saves entire memory image in file- use as (save 'image.lsp)
; this includes current property lists

(defun save (fname)
  (let ((cr (implode (list 10)))) ; intentional local change of cr
    ( setq  fname ( open fname t ))
      (save* fname ())
      (write fname (quote !"now saving property lists!"))
      (write fname cr)
      (saveplist* fname (proplists))
      (savecr fname) ; make sure cr is saved sensibly
    (close  fname )
))



(defun save* (fname lis)
 (setq lis (obl))  ; get whole oblist definition
 (loop
  (while lis)
  (wr_exp3 fname (car (car lis)))
  (setq lis (cdr lis))
 )
)

(defun savecr (fname)
  (write fname
  (quote (setq cr (implode (list 13 10))))
  )
 )


(defun saveplist* (fname lis)
  (let ((prop ()) )
    (loop
      (cond ((atom (car lis)) (setq prop (car lis)) (setq lis (cdr lis))))
      (cond ((and (listp (car lis) (not (null (car lis)))))
	      (write  fname "(" )    (write  fname  (quote put))
	      (write  fname  " (quote ")  (writec  fname (caar lis))
	      (write  fname " ) ( quote " )  (writec  fname prop)
	      (write  fname " ) "  )
	      (write  fname " ( quote ")
	      (writen fname cr) (setq wr_col 0)
	      (wr_item  fname (cadar lis))
	      (writen fname " ))")
	      (writen fname  cr))

	)
      (while (setq lis (cdr lis)))
    )
  )
)
