
; radial element for cogwheel, drawn this way so they intersect
; at the centre of the wheel so I get a centre point
(defun cogradial (r t) 
  (pd) (move r) (pu) (turn 270) (move (/ t 2)) (pd) (triangle t black)
    (pu) (move (/ t -2)) (turn 270) (move r)
  )

; draws a cogwheel type of thing
; this version uses let
(defun cog (r teeth )
  (let ( (tau (* 2 3.14)) )
 (repeat teeth  '(list (cogradial r  (* tau (/ r teeth)) )(turn (/ 360 teeth)) ))
  ))
