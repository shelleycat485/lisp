
; produces the nth triangle number
(defun trinum (n)
  (cond
    ((eq n 1) 1)
    (t (+ n (trinum (- n 1)))
       )
  )
)

; gives the total of a list of numbers
(defun totallis (lis)
  (cond
    ((null lis) 0)
    ( t (+ (totallis (cdr lis)) (car lis)))
    )
  )

; (generate 1 4 1) returns (1 2 3 4)
(defun generate (start end step) ; arguments are start end step
  (let  ( ( res () ) )
  (loop
    (setq res (cons start res))
    (until (greaterp (setq start (plus start step)) end ))
  )
  (reverse res)
  )
)

; to generate the tertahedron numbers: 
; (mapcar (quote trinum) (generate 1 4 1))

