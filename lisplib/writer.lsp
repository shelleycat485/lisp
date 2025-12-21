(setq space '! )
(setq lpar '!()
(setq rpar '!))

(setq wr_exp3 (quote
 (lambda (fname exp)
     ( writen  fname lpar) (write fname (quote setq)) 
     ( writec  fname  exp )
     ( writen  fname lpar) (writen fname (quote quote))
     (writen fname cr) 
     ( writec  fname ( eval  exp ))
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

; most of this is because put is written awkwardly, but would have to rewrite
; it and any uses to make it easier
; Each found property is written out as a put statement
(defun saveplist* (fname lis)
  (let ((prop ()) )
    (loop
      (cond ((atom (car lis)) (setq prop (car lis)) (setq lis (cdr lis))))
      (cond ((and (listp (car lis) (not (null (car lis)))))) 
	      (write  fname "(" )
	      (write  fname  (quote put))
	      (write  fname  " (quote ")
	      (writen  fname (caar lis))
	      (write  fname " ) ( quote " )
	      (writec  fname prop) ; properties can have spaces
	      (write  fname " ) "  )
	      (write  fname " ( quote ")
	      (writec  fname (cadar lis)) ; properties can be s-exps
	      (writen fname " ))")
	      (writen fname  cr) 
	      (while (setq lis (cdr lis)))
	))
    )
  )
)

; so the saved cr is the one that works with sprint, no matter the current
; value of cr, otherwise load of a save image is very difficult
(defun savecr (fname)
  (write fname 
  (quote (setq cr (implode (list 13 10))))
  )
 )

