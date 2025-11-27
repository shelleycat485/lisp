(defun ss2 (ang size frac)
    (let (
	  (s2 (* size frac) )
	  )
      (turn ang)  
      (pd) (move s2)
      (pu) (move (- size s2)) 
      )
    )

(defun tri2 (size frac)
 (list  
  (ss2 120  size frac)
  (ss2 120  size frac)
  (ss2 120  size frac)
  )
)

(newscreen 800 800) (pencolour 0 0 0 )
(repeat 40  (list (turn 15) (tri2 220 0.30)))
