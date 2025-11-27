(defun hilbert (depth side direction)
( cond
  ((zerop depth) () )
  ( t
   (setq side (/ side 4))
   (setq depth (- depth 1))
   (turn (* 90 (- 0 direction)))
   (hilbert depth side (- 0 direction))
   (move side)
   (turn (* 90 direction))
   (hilbert depth side direction)
   (move side)
   (hilbert depth side direction)
   (turn (* 90 direction))
   (move side)
   (hilbert depth side direction)
   (turn (* 90 direction))
  )
))

(defun dragon (depth side)
( cond
 (( zerop  depth )( move  side ))
 ((minusp depth)
     (dragon (- 0 (+ depth 1)) side)
     (turn 270)
     (dragon (+ depth 1) side)
  )
 ( t 
     (dragon (- depth 1) side)
     (turn 90)
     (dragon (- 0 (- depth 1)) side)
  )
))


( setq  tk2 (quote
( lambda ()( home )( pd )( repeat  4 ( list ( koch  3  600 )( turn  90 ))))
))

( setq  save2 (quote
( lambda ( fname  lis )( setq  lis ( obl ))( loop ( while  lis )( wr_exp3 ( car ( car  lis )))( setq  lis ( cdr  lis ))))
))
( setq  save (quote
( lambda ( fname )( and ( setq  fname ( open  fname  t ))( save2  fname ()))( close  fname ))
))
( setq  wr_exp3 (quote
( lambda ( exp )( writen  fname  lpar )( write  fname ( quote  setq ))( writec  fname  exp )( writen  fname  lpar )( writen  fname ( quote  quote ))( writen  fname  cr )( writec  fname ( eval  exp ))( writen  fname  cr )( writen  fname  rpar )( writen  fname  rpar )( writen  fname  cr ))
))
( setq  wr_exp2 (quote
( lambda ( exp )( loop ( print ( quote  give_exp ))( while ( setq  exp ( read )))( wr_exp3  exp )))
))
( setq  write_exps (quote
( lambda ( fname )( and ( setq  fname ( open  fname  t ))( wr_exp2 ()))( close  fname ))
))
( setq  edfflg (quote
 true 
))
( setq  tk1 (quote
( lambda ()( home )( pd )( repeat  6 ( list ( koch  3  450 )( turn  60 ))))
))
( setq  ang (quote
 -20 
))
( setq  edeflg (quote
 true 
))
( setq  edloc1 (quote
( lambda ( arg  fval )( cond (( or  edfflg ( atom  arg )) arg )(( equal  fval ( car  arg ))( setq  edfflg  t )( print  arg )( ed1 () arg ))( t ( prin ( quote  * ))( cons ( edloc1 ( car  arg ) fval )( edloc1 ( cdr  arg ) fval )))))
))
( setq  sb (quote
( lambda ( lis  old  new )( cond (( equal  lis  old )( prin ( quote  # )) new )(( atom  lis ) lis )( t ( prin ( quote  * ))( cons ( sb ( car  lis ) old  new )( sb ( cdr  lis ) old  new )))))
))
( setq  ed1 (quote
( lambda ( inch  arg )( loop ( until  edeflg )( setq  inch ( readch ))( cond (( eq  inch  cr )( print  arg ))( t ( cond (( eq  inch ( quote  b ))( until  t ))(( eq  inch ( quote  s ))( prin ( quote  ? ))( setq  arg ( sb  arg ( read )( read )))( prin ( quote  ok )))(( eq  inch ( quote  f ))( prin ( quote  ? ))( setq  edfflg  () )( setq  arg ( edloc1  arg ( read )))( cond (( not  edfflg )( prin ( quote  nt_fnd )))))(( eq  inch ( quote  e ))( until ( setq  edeflg  t )))(( eq  inch ( quote  q ))( obl  4 ))(( eq  inch ( quote  a ))( and ( not ( atom  arg ))( setq  arg ( cons ( ed1 ()( car  arg ))( cdr  arg )))))(( eq  inch ( quote  d ))( and ( not ( atom  arg ))( setq  arg ( cons ( car  arg )( ed1 ()( cdr  arg ))))))(( eq  inch ( quote  x ))( cond (( atom  arg )())( t ( setq  arg ( cdr  arg )))))(( eq  inch ( quote  r ))( prin ( quote  ? ))( setq  arg ( read )))(( eq  inch ( quote  c ))( prin ( quote  ? ))( setq  arg ( cons ( read ) arg )))( t ( print ( quote  error:! adbxrcsfqe ))))))) arg )
))
( setq  ed (quote
( lambda  arg ( setq  edeflg ())( readch )( set ( car  arg )( ed1 ()( eval ( car  arg )))) () )
))
( setq  testkoch (quote
( lambda ( depth  side )( pu )( moveto  0  -500 )( pd )( koch  depth  side ))
))
( setq  koch (quote
( lambda ( depth  side )( cond (( zerop  depth )( move  side ))( t ( koch ( -  depth  1 )( /  side  3 ))( turn  -60 )( koch ( -  depth  1 )( /  side  3 ))( turn  120 )( koch ( -  depth  1 )( /  side  3 ))( turn  -60 )( koch ( -  depth  1 )( /  side  3 )))))
))
