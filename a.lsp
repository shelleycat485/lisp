; returns last element of a list
(defun last (lis)
 (car (reverse lis))
)
 

(defun rep1 exp ; version of repeat use as (rep 10 (print 2))
  (let ( (n (car exp)) (e1 (cadr exp)) )
   (loop
     (until (minusp (setq n (- n 1))))
     (eval e1)
   )
  )    
)


(defun generate (start end step) ; arguments are start end step
  (let  ( ( res () ) )
  (loop
    (setq res (cons start res))
    (until (greaterp (setq start (plus start step)) end ))
  )
  (reverse res)
  )
)

(defun gen2 (start stop step)
  (let ( ( res ()) (test
     (cond ((greaterp step 0) greaterp) ( t lesserp ) ) ) )
  (loop
    (setq res (cons start res))
    (until (test (setq start (plus start step)) stop))
  )
  (reverse res)
  )
  )

(defun concatenate (listofwords) ; argument a listofwordst of '(the big cat) 
                                 ; you cannot make a string longer than 132
                                 ; chars though (limitation of this language        
 (let ( (res () ) )
 (loop
   (setq res (append res (explode (car listofwords)) ))
   (while (setq listofwords (cdr listofwords)))
 )
 (implode res)
 )
)

(defun orderword (w1 w2) ; orders words letter by letter
  (cond
   ((null w1) (true))
   ((null w2) ())
   ( t
      (setq w1 (explode w1)) (setq w2 (explode w2))
      (let ( (o1 (ordinal (car w1))) (o2 (ordinal (car w2))) )
       (cond
        ((minusp (difference o1 o2 )) (true) )
        ((zerop  (difference o1 o2)) 
               (orderword (implode (cdr w1)) (implode (cdr w2))) )
        ( t () )
       )
      )
   )
  )
)


(defun orderall (l m) 
  (cond
   ((null l) ())
   ((null m) t )
   ((and (atom l) (listp m)) t )
   ((and (listp l) (atom m)) ())
   ((and (atom l) (atom m)) (orderword l m) )
   ( t (orderall (cdr l) (cdr m) )   )
  )
)

; picks and removes a number n from a numeric list lat
; returns unchanged lat if the number is not present
(defun rempick (n lat)
  (cond
   ((null lat) () )
   ((eq n (car lat)) (cdr lat) )
   ( t (cons (car lat) (rempick  n  (cdr lat))))
  )
)


(defun alphalist ()
 ( explode ( implode ( generate  65  74  1 )))
)


(defun readtextfile (fname)
  (let ( (fh (open fname)) (cc ()) (word ())   )
  (inittree)
  (loop
    (until (eof fh))
    (setq cc (readch fh))
    (cond
      ( (isnotalphanum cc) (addtotree (rlword word))  (setq word ()))
      ( t  (setq word (cons cc word)) )
    )
  )
  (close fh)
  )
)


(defun rlword (wlis)
  (implode (reverse wlis))
)

(setq literals 
   (list space '!' lpar rpar '. ', '!" "'" ";" ":" "?" (implode '(13)) (implode '(10)) ))
 
(defun isnotalphanum (atm)
    (member atm literals)  
)


(defun makenode (val left right) (list val left right))

(defun compnode (n1 n2) 
  (cond
    ((eq (caar n2) (caar n1)) 'eqi)
    ((orderp (caar n2) (caar n1)) 'greater)
    ( (true) 'lesser)
  )
)

(defun cadar (lis) (car (cdar lis)))
(defun caadr (lis) (car (cadr lis)))

(defun orderp (a1 a2)  (orderall a1 a2))

(defun inittree () (setq wtree ()))

(defun addtotree (val) (setq wtree (addtotree* val wtree)))

(defun addtotree* (val tree)
  (let ( (compres () ) (nta (makenode (list val 1) () ()))  )
  (cond
    ((null val) tree )
    ((null tree) (setq tree nta) )
    ( t (setq compres (compnode nta tree))
      (cond
        ((eq compres 'eqi) (list (list (caar tree) (plus 1 (cadar tree))) (cadr tree) (caddr tree)))
        ((eq compres 'greater) (list (car tree) (cadr tree) (addtotree* val (caddr tree))))
        ( t       (list (car tree) (addtotree* val (cadr tree)) (caddr tree)))
      )
    )
  )
  )
)

(defun printtree (tree) 
 (cond 
  ((null tree) ())
  ((caadr tree)(printtree (cadr tree))(printtreeele  tree )(printtree (caddr tree))  )
  (t (printtreeele tree) (printtree (caddr tree)))
 )
)    

(defun printtreeele(ele)
  (mapc 'print  (list  (cadar ele) (implode '(8) ) space (caar ele) cr))
  )


(defun toassoclist (lis) ; (a b c d e f) becomes ((a b) (c d) (e f))
  (cond
   ((null lis) () )
   ( t (cons (cons (car lis) (cadr lis)) (toassoclist (cddr lis))))
  )
)

(defun torevassoclist (alis) ; ((a b) (c d)) becomes ((b a) (d c))
  (cond
    ((null alis) () )
    (t (cons (reverse (car alis)) (torevassoclist (cdr alis))))
  )
)


(defun maxcount (alis)  ; given assoclist ((a 1) (b 4) (c 2) ) gives (b 4)
  (let ( (res (car alis)) ( temp ()) )
  (loop
    (while alis)
    (setq temp (car alis))
    (and (greaterp (cadr temp) (cadr res)) (setq res temp ))
    (setq alis (cdr alis))
  )
  res
  )
)


(defun getmaxcountfromlis (lis) ; list should be numeric for greaterp to work
  (cond
    ((null (cdr lis)) (car lis) )
    ((greaterp (car lis) (cadr lis))
       (getmaxcountfromlis (cons (car lis) (cddr lis))))
    ( t (getmaxcountfromlis (cdr lis)))
  )
)
 



(defun printfirst (alis)
  ( cond 
    (( null  alis )())
    ( t ( print ( caar  alis ))( prin  cr )( printfirst ( cdr  alis )))
  )
)

(defun printboth (alis)
  ( cond
    (( null alis) ())
    ( t (print (reverse (car alis))) (prin cr) (printboth (cdr alis)))
  )
)


(defun fac (n)
  (cond
    ((zerop n) 1)
    ( t (* n (fac (- n 1))))
  )
)


(defun fib (n)
  (cond
    ((or (onep n) (eq n 2)) 1 )
    ( t (+ (fib (- n 1)) (fib (- n 2))) )
    )
  )

(defun rmember (a lis)
  (cond
    ((null lis) () )
    ((eq a (car lis)) (cdr lis))
    (t (cons (car lis) (rmember a (cdr lis)) ))
    )
  )

(defun if arglist (and (eval (car arglist))   (eval (cadr arglist )) ))



