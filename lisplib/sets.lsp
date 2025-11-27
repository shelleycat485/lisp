; list set functions
; delete, member  in init.lsp 


(defun mark1 (a) (put a 'mark1 't))
(defun mark2 (a) (put a 'mark2 't))
(defun getm1 (a) (get a 'mark1 ))
(defun getm2 (a) (get a 'mark2 ))
(defun remm1 (a) (remprop a 'mark1))
(defun remm2 (a) (remprop a 'mark2))



(defun intersection (set1 set2)
 (cond ((or (null set1) (null set2)) () )
       ( t
          (mapc mark2 set1)
	  (mapc '(lambda (a) (and
                      (getm2 a) (mark1 a))) set2)
	  (mapc remm2 set1)
	  (makeset* set2 () )
       )
))


(defun union (set1 set2)
   (cond ((null set1) set2)
         ((null set2) set1)
         ( t (makeset  (append set1 set2)))
   )
 )


 (defun complement (set1 uset)  ; uset is the universal set
  (mapc mark1 uset)
  (mapc remm1 set1)
  (makeset* uset () )
 )
 

 (defun issubset (set1 set2)       ; checks if set1 is subset of set2
   (cond ((null set1) t)
         ((greaterp (length set1) (length set2)) () ) 
         ( t  (mapc mark1 set2)
	   (loop
             (cond
              ((getm1 (car set1)) (while (setq set1 (cdr set1)))  )
              ( t  (until true)) ; exit with some of set1 left
             )
           )             ; exit loop
           (mapc remm1 set2)
	   (not set1)
         )
  ) )

 (defun setequal (set1 set2)   ; checks to see if both sets are the same
  (cond ((null set1) (null set2))
        ((not (eq (length set1) (length set2))) () ) ; different lengths
        ( t (issubset set1 set2) )
  )
 )

 (defun makeset (lis)    ; removes duplicate elements from a list, 
  (cond
   ((null (cdr lis))  lis)   ; empty list and single element case
   ( t ( mapc mark1 lis ) 
       (makeset*  lis () ))
 ))       

 (defun makeset* (lis clis )   ; aux function, list has at least two eles
  (loop
     (and 
         (getm1 (car lis) )
         (setq clis (cons (car lis) clis))
         (remm1 (car lis))  
     )
     (while (setq lis (cdr lis)))  
  )
  clis )


 (defun every (fn lis) ; returns true if fn on every member of lis is true
   (cond
     ((null lis) () ) ; returns false for a null lis
     ((fn (car lis))
       (cond
         ((onep (length lis)) t)
         ( t (every fn (cdr lis)))
       )
     )
     (t  () )
   )
  )

 (defun some (fn lis) ; returns true if fn is true on some members of list
                    ; returns the first non-null value
    (cond
      ((null lis)               ()  )
      ((fn (car lis)) (car lis))
      (t              (some fn (cdr lis)))
    )
 )
