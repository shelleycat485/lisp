(setq editlist ())
(setq editlist (cons '(see saw) editlist))
(setq editlist (cons '(look looked) editlist))
(setq editlist (cons '(orig changed) editlist))

(defun generate (start end step) ; arguments are start end step
  (let  ( ( res () ) )
  (loop
    (setq res (cons start res))
    (until (greaterp (setq start (plus start step)) end ))
  )
  (reverse res)
  )
)


(defun alphalist ()
 ( explode ( implode ( generate  65  74  1 )))
)

; checks word from editlist, return the changed word if found else returns word
(defun retedit ( word )
 (cond
  (( assoc  word  editlist )
     ( cadr ( assoc  word  editlist )))
  ( t
      word ))
)

(setq endsentliterals (list '. ', rpar '? '!! ))

; this does the substitution
;(mapcar 'retedit (append (alphalist) (list 'see)))

(defun rtfile (fname)
  ( let 
    (( fh ( open  fname ))( cc ()) ( word ()) (wlist ()))
  ( loop 
   ( setq  cc ( readch  fh ))
   ( and (member cc endsentliterals) (setq cc space))
   (cond
    ( (eq cc space) 
       (setq wlist (cons (implode (reverse word )) wlist ))
       ( setq  word ()))
   )
   ( setq  word ( cons  cc  word ))
   ( until ( eof  fh ) )
  )
   ( close  fh )
   (reverse wlist)
  ))

 
