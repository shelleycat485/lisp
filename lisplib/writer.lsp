(setq space '! )
(setq lpar '!()
(setq rpar '!))
(setq cr (implode '(10 13) ))

( setq  write_exps (quote
( lambda  (fname) 
  ( and 
    ( setq  fname ( open fname t ))
    ( wr_exp2 ())
  )
  ( close  fname ))
))


 (defun  wr_exp2 (exp)
  (loop
    ( print 'give_exp )
    ( while (setq  exp ( read )))
    (wr_exp3 exp)
   ) ;end loop
 )     

(setq wr_exp3 (quote
 (lambda (exp)
     ( writen  fname lpar) (write fname (quote setq)) 
     ( writec  fname  exp )
     ( writen  fname lpar) (writen fname (quote quote))
     (writen fname cr) 
     ( writec  fname ( eval  exp ))
     (writen fname cr) 
     ( writen  fname rpar) (writen fname rpar)
     ( writen  fname cr)
)))


; saves entire memory image in file- use as (save image.lsp)

(defun save (fname)
    (and 
      ( setq  fname ( open fname t ))
      (save2 fname ())
    )
    (close  fname )
)

(defun save2 (fname lis)
 (setq lis (obl))        ; get whole oblist definition
 (loop
  (while lis)
  (wr_exp3 (car (car lis)))
  (setq lis (cdr lis))
 )
)
