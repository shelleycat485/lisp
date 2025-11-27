
; radial element for cogwheel, drawn this way so they intersect
; at the centre of the wheel so I get a centre point
(defun radial (r t) 
  (pd) (move r) (pu) (turn 270) (move (/ t 2)) (pd) (triangle t)
    (pu) (move (/ t -2)) (turn 270) (move r)
  )

; draws a cogwheel type of thing
; this version uses let
(defun cog (r teeth )
  (let ( (tau (* 2 3.14)) )
 (repeat teeth  (list (radial r  (* tau (/ r teeth)) )(turn (/ 360 teeth)) ))
  ))

; draws a cogwheel as above, but
; this version uses local scope variables
(defunloc cogl (radius teeth)
	  (local tau) (local turnang)
	  (setq tau (* 2 3.142))
	  (setq turnang (/ 360 teeth))
  (repeat teeth (and (radial radius (* tau (/ radius teeth))) (turn turnang)
		     ))
  )



