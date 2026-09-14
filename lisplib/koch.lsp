(defun hilbert (depth side direction)
( cond
  ((zerop depth) () )
  ( t
   (setq side (/ side 2))
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
   (hilbert depth side (- 0 direction))
   (turn (* 90 (- 0 direction)))
  )
))

(defun dragon (depth side)
( cond
 (( zerop  depth )( move  side ))
 ((minusp depth)
     (dragon (- 0 (+ depth 1)) (/ side 1.4142136))
     (turn 270)
     (dragon (+ depth 1) (/ side 1.4142136))
  )
 ( t 
     (dragon (- depth 1) (/ side 1.4142136))
     (turn 90)
     (dragon (- 0 (- depth 1)) (/ side 1.4142136))
  )
))


( setq  tk2 (quote
( lambda ()( home )( pd )( repeat  4 
	(quote ( list ( koch  3  600 )( turn  90 )))))
))

( setq  tk1 (quote
( lambda ()( home )( pd )( repeat  6 
	(quote ( list ( koch  3  450 )( turn  60 ))))
)))
( setq  testkoch (quote
( lambda ( depth  side )( pu )( moveto  0  -500 )( pd )( koch  depth  side ))
))
( setq  koch (quote
( lambda ( depth  side )( cond (( zerop  depth )( move  side ))( t ( koch ( -  depth  1 )( /  side  3 ))( turn  -60 )( koch ( -  depth  1 )( /  side  3 ))( turn  120 )( koch ( -  depth  1 )( /  side  3 ))( turn  -60 )( koch ( -  depth  1 )( /  side  3 )))))
))
