
; functions to set screen colours and video attributes
; the expression returned by this function should be printed

(defun vid_att (n)
 (cond
  ((not (numberp n)) 'give! a! number)
  ( t  (implode (list 27 '[ (+ 48 (/ n 10)) (+ 48 n (* -10 (/ n 10))) 'm)))
))

(defun vid_mode (n)
 (cond
  ((not (numberp n)) 'give! a! number)
  ( t  (implode
        (list 
          27 '[ '=  (+ 48 n ) 'h
      )))
))
 
(defun clr_screen ()  (implode (list 27 '[ 50 'J)))

