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

(defun saveplist* (fname lis)
  (let ((prop ()) )
    (loop
      (cond ((atom (car lis)) (setq prop (car lis)) (setq lis (cdr lis))))
      (writen fname  (list  'put
		    " '" (caar lis)
		    " '"  prop
		    " '" (cadar lis)))
      (writen fname  cr) 
      (while (setq lis (cdr lis)))
      )
    )
  )

(defun savecr (fname)
  (write fname 
  (quote (setq cr (implode (list 13 10))))
  )
 )

