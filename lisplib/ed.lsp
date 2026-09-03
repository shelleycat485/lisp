
; lisp structure editor

(defun sb (lis old new)
   (cond ((equal lis old) (prin '#) new ) ; if old and new are equal return new
         ((atom lis) lis)    ; dont recurse if an atom already
         (t  (prin '*)       ;to show working
	     (cons (sb (car lis) old new ) (sb (cdr lis) old new)))
   )  )

(defun prcr (arg)
 (screen_col_any 3)  (print arg) (screen_col_green) (prin cr))

( setq  ed (quote
( lambda  arg
  (let ( (endflag ()) (edlevel 0) )
  ( set ( car  arg )( ed1 ()( eval ( car  arg ))))
(screen_col_white)
  (print "editor done" )
(screen_col_tgreen)
))))

(setq altwords '((p print) (P sprint)
		 (w write) (q quit) (a car) (d cdr) (b back) (f find)))

(defun ed1 (inch arg)
  (setq edlevel (+ edlevel 1))
  ( loop 
     ( until endflag)
     ( setq  inch ( read ))
     ( cond
         ((assoc inch altwords) (setq inch (cadr (assoc inch altwords))))
     )
     (prin "read:") (prin inch) (prin ":") (print edlevel) 
     ( cond 
         (( eq  inch "sprint") (sprint arg))
	 (( eq  inch "write") (setq endflag (true)))
         (( eq  inch "print" )( prcr arg))
         (( eq  inch "back"   ) (setq edlevel (- edlevel 1)) (until t))
         (( eq  inch "help")
           ( prcr "print p sprint P back b  help write w quit q  car a cdr d  replace sub find delete cons" ) )
         (( eq  inch ( quote  quit ))( obl  4 ))
         (( eq  inch ( quote  car ))( and ( not ( atom  arg ))( setq  arg ( cons ( ed1 ()( car  arg ))( cdr  arg )))) )
         (( eq  inch ( quote  cdr ))( and ( not ( atom  arg ))( setq  arg ( cons ( car  arg )( ed1 ()( cdr  arg ))))))
         (( eq  inch ( quote  replace ))( prcr ( quote  ? ))( setq  arg ( read )) (prcr arg)) 
         (( eq  inch ( quote  sub ))( prcr ( quote  ? ))( setq  arg ( sb  arg ( read )( read )))( prcr arg))
    ( (eq inch (quote find)) (print '?) (setq edfflg ()) ; set flag to say found or not
                   (setq arg (edloc1 arg (read)))
		   (cond ((not edfflg) (print "not found" ) ))
    )
         (( eq  inch ( quote  delete ))( prcr ( quote  ? ))( setq  arg 
          ( delete ( read ) arg ))( prcr arg))
         (( eq  inch ( quote  cons ))( prcr ( quote  ? ))( setq  arg 
          ( cons ( read ) arg ))( prcr arg))
 ))  arg )


(defun semicolon () (prin (implode (cons 59 ())))() )

(defun screen_col_any (c)
  (prin (implode (list 27 "[")))
  (semicolon)
  (prin "3") (prin c) (prin "m")
  ()
  )

(defun screen_col_red () (screen_col_any 1))
(defun screen_col_white () (screen_col_any 7))
(defun screen_col_green () (screen_col_any 2))

(defun screen_col_tgreen ()
  (prin (implode (list 27 "[" "3" "8")))
  (semicolon)
  (prin "5")
  (semicolon)
  (prin (implode (list "4" "6" "m")))
  ()
)

  (  setq  edloc1  (  quote  
    ( lambda ( arg  fval ) ; locates fval in arg
     ( cond 
       ((or edfflg ( atom  arg )) arg ) ; stop if atom found, or flag set
         ; when found, does edit on it
       (( equal  fval ( car  arg )) 
           (setq edfflg t) (print arg) (print cr) ( ed1 () arg )
       )
       ( t ; (print '*)  ; to show working
           ( cons ( edloc1 ( car  arg ) fval)(edloc1 (cdr arg) fval) )
       )
     )
  )))  
