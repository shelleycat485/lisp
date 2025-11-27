
(setq endsentliterals (list '!" '. ', lpar rpar '? '!! '!' '!;))

(defun rtfile (fname)
  ( let 
    (( fh ( open  fname ))( cc ()) ( word ()) (wlist ()))
  ( loop 
   ( setq  cc ( readch  fh ))
   (cond
    ( (null cc) ())
    ( (member cc endsentliterals) () )
    (  ( or (eq cc space) (eq (ordinal cc) 10))
       (setq wlist (cons (implode (reverse word )) wlist ))
       ( setq  word ()))
;    ( t  (and cc (setq word (cons cc word))))
     ( t   (setq word (cons cc word)))
   )
   ( until ( eof  fh ) )
  )
   ( close  fh )
   (reverse  wlist)
  ))

(defun procwlist (lis)
       (inittree)
       (loop
	 (addtotree (car lis)) 
         (while (setq lis (cdr lis)))
       )
)


