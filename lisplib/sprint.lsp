
(defun xtab (n) 
  (loop (until (minusp (setq n (- n 1)))) (prin space) )
)

; set up info as to which primitives to treat specially
(mapc '(lambda (arg) (put arg 'sprint t))
  '( loop while until and or let)
)


(defun sprint (arg)
  (sprint1 arg 0)
)

(defun sprint1 (arg n)
  (xtab n)
  (cond
   ((atom arg) (print arg))
   ((eq (car arg) 'lambda)
     (prin lpar) (print 'lambda) (print (cadr arg))
     (sprrest (cddr arg) (+ n 1))
     (prin rpar)
   )
   ((eq (car arg) 'cond )
     (prin lpar) (prin 'cond)
     (sprcond (cdr arg) (+ n 1))
     (prin rpar)
   )
   ((and (atom (car arg)) (car arg) (get (car arg) 'sprint))
     (prin lpar) (print (car arg))
     (sprrest (cdr arg) (+ n 1))
     (prin rpar)
   )
   (t (print arg) )
  )
 space )

(defun sprrest (arg n)
 (cond
 ((atom arg) (prin cr) (sprint1 arg n))
 ( t (loop
     (prin cr) (sprint1 (car arg) n)
     (while (setq arg (cdr arg)) )
 )))
)



(defun sprcond (arg n)
 (loop
  (prin cr) (xtab n) (prin lpar)
  (print (caar arg) n)
  (sprrest (cdar arg) (+ n 3))
  (prin rpar)
  (while (setq arg (cdr arg)) )
 )
)

