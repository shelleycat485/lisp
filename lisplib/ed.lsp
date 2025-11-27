  


; lisp structure editor
(defun ed arg
 (setq edeflg ()) ; end flag for exiting
 (readch)        ; to avoid initial print of expression 
 (set (car arg) (ed1  () (eval (car arg)))  )
 f      ;to avoid a great long value coming back
)

(setq cr (implode '(10)) )

; editing functions
; a take car of argument
; d take cdr of arg
; b back up one level
; e exits, with changed expression
; q quits, leaving original expression unchanged
; x set arg to cdr of argument, i.e. delete first ele 
; r replace arg with read expressoin
; c cons expression onto arg
; s global subsitution on current arg, prompts for old and new expression
; f finds an expression, then edits the sub expression following

( setq  ederrtext (quote
( lambda ()
  ( mapc
    ( quote ( lambda ( a )( prin  a )( prin  cr )))
    ( quote 
     ( A! car  D! cdr  B! Back  C! cons  R! Replace  F! Find  
       E! Exit  Q! Quit  X! delete  S! Sub! old! new )
))
)
))



(defun ed1 (inch arg)
 (loop
  (until edeflg)
  (setq inch (readch))
  (cond ((eq inch cr) (print arg))
   ( t 
   (cond
    ( (eq inch 'b) (until t) ) 
    ( (eq inch 's) (prin '?) (setq arg (sb arg (read) (read) )) (prin 'ok) 
    )
    ( (eq inch 'f) (prin '?) (setq edfflg f) ; set flag to say found or not
                   (setq arg (edloc1 arg (read)))
		   (cond ((not edfflg) (prin 'nt_fnd) ))
    )
    ( (eq inch 'e) (until (setq edeflg t)) ) ;exit, setting flag
    ( (eq inch 'q) (obl 4) )  ;                force break
    ( (eq inch 'a) (and (not (atom arg)) 
                        (setq arg (cons (ed1 () (car arg)) (cdr arg)))))
    ( (eq inch 'd) (and (not (atom arg))
                        (setq arg (cons (car arg) (ed1 () (cdr arg))))))
    ( (eq inch 'x) (cond ((atom arg) ())
                         ( t (setq arg (cdr arg)) ))
     )
    ( (eq inch 'r) (prin '?) (setq arg (read)) )
    ( (eq inch 'c) (prin '?) (setq arg (cons (read) arg)))
    ( t (ederrtext)  ) 
   )  ; end cond 
  )) ;end outer cond
 )  ; end loop
 arg ;returned value
) 


(defun sb (lis old new)
   (cond ((equal lis old) (prin '#) new ) ; if old and new are equal return new
         ((atom lis) lis)    ; dont recurse if an atom already
         (t  (prin '*)       ;to show working
	     (cons (sb (car lis) old new ) (sb (cdr lis) old new)))
   )  )

    
  (  setq  edloc1  (  quote  
    ( lambda ( arg  fval ) ; locates fval in arg
     ( cond 
       ((or edfflg ( atom  arg )) arg ) ; stop if atom found, or flag set
         ; when found, does edit on it
       (( equal  fval ( car  arg )) 
           (setq edfflg t) (print arg) ( ed1 () arg )
       )
       ( t (prin '*)  ; to show working
           ( cons ( edloc1 ( car  arg ) fval)(edloc1 (cdr arg) fval) )
       )
     )
  )))  
