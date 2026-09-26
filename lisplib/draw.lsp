( setq  radial (quote
( lambda ( size )( pd )( move  size )( pu )( turn  180 )( move  size )( turn  180 ))
))
( setq  radturn (quote
( lambda ( size  ang )( radial  size )( turn  ang ))
))
( setq  starburst (quote
( lambda ( size )( repeat  15 '( radturn  size ( /  360  15 ))))
))
( setq  star20 (quote
( lambda ( size )( repeat  20 '( radturn  size ( /  360  20 ))))
))
( setq  pu (quote
( lambda ()( pendown  0 ))
))
( setq  pd (quote
( lambda ()( pendown  1 ))
))

( setq  repeat (quote
( lambda  (mm fun) 
	  (loop
	    (until (minusp (setq mm (difference mm 1))))
	    (eval fun)
	  )
)))

( setq  newscreen (quote
( lambda (x y )( initturtle  x  y ))
))
( setq  spiral1 (quote
( lambda ( steps  size  ang  inc )( loop ( until ( minusp ( setq  steps ( -  steps  1 ))))( turn  ang )( move ( setq  size ( +  size  inc )))))
))
( setq  spiral (quote
( lambda ()  (home) (pencolour 0 0 255 )( pendown  1 )( spiral1  200  0  41  1 ))
))


(defun red () (pencolour 255 0 0))
(defun white () (pencolour 255 255 255))
(defun black () (pencolour 0 0 0))
(defun blue () (pencolour 0 0 255))
(defun green () (pencolour 0 255 0))


(defun triangle (len colour)
  (let ((oa (turn 0)) (clen (/ len 1.732)))
(pu)
(colour)
(move clen)
(pd)
(turn 30)
(repeat 3 '(list (turn 120) (move len)))
(turn 150)
(pu)
(move clen)
(turnto oa)
))

(defun square (len colour)
  (pu)
  (colour)
  (turn 45)
  (move (* len 0.707))
  (turn 45)
  (pd)
  (repeat 4 '(list (turn 90) (move len)))
  (pu)
  (turn -45)
  (move (* len -0.707))
  (turn -45)
)


