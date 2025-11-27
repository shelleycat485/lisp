; init.lsp        6/6/92  R. Haxby

; standard lisp functions that are not provided as primitives
; this file is intended to be the base lisp library file read

; this atom indicates true
(setq t (true))
; a null list () indicates false

(setq defun
 '(lambda lis (set (car lis) (cons (quote lambda) (cdr lis))))
)

(setq - 'difference)
(setq + 'plus)
(setq * 'times)
(setq / 'divide)

(defun onep (n) (eq n 1) )
(defun zerop (n) (eq n 0) )


; does the function fn  on each item of the list l in turn
; in this lisp, looping is faster than recursion
(defun mapc (fn l)
 (loop
  (and l
       (eval (list fn  (list 'quote (car l))))
  )
  (while (setq l (cdr l)))
 )
)

; like mapc, but returns a list of the results
(defun mapcar (fn l)
 (cond 
  ((null l) ())
  ( t (cons (eval (list fn 
               (list (quote quote) (car l))
            )) 
            (mapcar fn (cdr l))
      )
  )
 )
)

; applies the function name, which should be supplied quoted
; to the list of arguments, i.e. (apply 'print '(a b c))

(defun apply (fn args)
 (eval 
  (cons fn  
        (mapcar (quote 
                  (lambda (a) (list (quote quote) a))
                )
         args ))))


(defun greaterp (n1 n2) (minusp (- n2 n1))  )
(defun lesserp (n1 n2) (not (greaterp n1 n2))   )


; to give an printout of all defined objects
(defun oblist () (mapcar 'car (obl)))

(defun caar (lis) (car (car lis)) )
(defun cdar (lis) (cdr (car lis)) )
(defun cadr (lis) (car (cdr lis)) )
(defun cddr (lis) (cdr (cdr lis)) )
(defun cdddr (lis) (cdr (cdr (cdr lis)))) 
(defun cddddr (lis) (cdr (cdr (cdr (cdr lis)))))
(defun caddr (lis) (car (cdr (cdr lis))))
(defun cadddr (lis) (car (cdr (cdr (cdr lis)))))

(defun equal (lis1 lis2)  ;like the primitive eq, but works on lists too
 (cond
  ((atom lis1) (eq lis1 lis2))
  ((atom lis2) () )
  (t (and
      (equal (car lis1) (car lis2))
      (equal (cdr lis1) (cdr lis2))
  ))
))


 (defun delete ( item lis)
 (cond
  ((null lis) ())
  ((equal item (car lis)) (delete item (cdr lis)) )
  ( t (cons (car lis) (delete item (cdr lis)) ) )
))

(defun member (item lis) ;returns the ele found if a member of the list
 (cond
  ((null lis) ())
  ((equal (car lis) item) lis)
  ( t (member item (cdr lis)))
))

(defun assoc (item lis)
 (cond
  ((null lis) ())
  ((eq item (caar lis)) (car lis))
  ( t (assoc item (cdr lis)))
))


; special atoms definitions.  Will also need special for ' and ; if used
 (setq space '! )
 (setq lpar '!( )
 (setq rpar '!) )
 (setq cr (implode '(10 13)))

; uses of obl primitive - this is specific to this implementation
; (obl) produces all oblist defs in full
; (obl 1) produces all property lists
; (obl 2) gives string stats
(defun proplists() (obl 1)) ;
(defun gcoll    () (obl 3)) ; triggers garbage collection (test use mainly)
(defun break    () (obl 4)) ; stops evaluation and returns to command
(defun gcollon  () (obl 5)) ; set garbage collection reporting on
(defun gcolloff () (obl 6)) ; set reporting off
(defun bindings () (obl 9)) ; show current binding of variables
                            ; only useful in a let or lambda environment
(defun help     () (print "Get more help by (load 'lisplib/helptext.lsp)") (print cr) (obl 10)) ;show all keywords
(defun exit     () (obl 11)) ; exits program with return code 0

; global substitution throughout a list, any depth
 (defun subst (old new lis)
  (cond
   ((eq old lis) new)
   ((atom lis) lis)
   (t (cons (subst old new (car lis))
            (subst old new (cdr lis))
   ))
 ))


(defun power (n e) ; only integer exponents, and this only uses float 
  (cond            ; numbers   e.g. (power 3 2) = 9 (power 2 -2) = 0.25
                   ; so cannot use this for square roots
    ((minusp e) (/ 1 (power1 n (- 0 e) 1)))
    ( t              (power1 n e 1))
  )
)


(defun power1 (n e x)
 (loop (until (eq e 0)) (setq x (* x n)) (setq e (- e 1)) )
  x
)

; lisp functions reverse and append are provided as primitives

; returns a unique atom (in current run of program), of form A10001
; the main use for it is in defunloc
; note that this does not work in recursion
(defun makesym () 
  (implode    (cons 'A (explode 
      (cond
        ((null (get 'i 'g_symbol)) (put 'i 'g_symbol 10001)) 
        ( t (put 'i 'g_symbol (+ (get 'i 'g_symbol ) 1 ) ) )
      )
))))

; returns the current internal symbol
(defun currentsym () (get 'i 'g_symbol))

(defun alt (x) ; (given list '(a b c d e) returns alt entries '(a c e)
 (cond
  ((or (null x) (null (cdr x))) x )
  (  t (cons (car x) (alt  (cddr x))))
 )
)


(defun pr2 (x) (print x) (print (implode (list '10 '13)) ))


; like defun, but can have local variables for the function. e.g.
;(defunloc ff(x)(local n m)(setq n 2)(setq m 3)(pr2(+ n m))(pr2(quote ff)))
;e.g (defunloc gg() (local a) (local b) (setq a (+(setq b 1) 1)) (pr2 a))
; note the (local v1 v2...) expression
; does it by rewriting function definition to use (makesym) variables
(defun defunloc lis
  (cond
         ( (collectlocal lis () )  (defunloc1 lis () ))
         ( t "if using defunloc must have a (local x ) clause" )
  )
)

(defun defunloc1 (lis locs)
  (setq locs (collectlocal lis ()))
  ;(print (cons 'locs! are: locs))
  (setq lis (discardlocal lis))
  (loop
    (setq lis (subst (car locs) (makesym) lis ))
    (while (setq locs (cdr locs)))
  )  
  (eval (cons 'defun lis))
)


(defun collectlocal (lis res) ; collects all the (local a b) terms
  (loop
    (and (not (atom (car lis))) (eq (caar lis) 'local)
               (setq res (append res (cdar lis)) ))
    (while (setq lis (cdr lis)))
  )
  res
)

(defun discardlocal (lis) ; removes all the (local b) terms from the list
  (cond
    ((null lis) lis)
    ((atom (car lis)) (cons (car lis) (discardlocal (cdr lis))))
    ((eq (caar lis) 'local)           (discardlocal (cdr lis)))
    ( t               (cons (car lis) (discardlocal (cdr lis))))
  )
)


(defunloc revdemo (l) ; reverses a top level list, using a local variable
                      ; this is not going to be recursive
  (local result)
  (setq result ())
  (loop
    (setq result (cons (car l) result))
    (while (setq l (cdr l)))
  )
  result
)

; let is a Subr, allows local variables to be declared and bound
; within the let block only.  Will work will recursion.
; This example shows how to use it.
; (let ( (var1 val1) (var2 val2)...) form1 form2... )
;
(defun rev (lis) ; reverses a top level list, useing a let block
  (let ( (res () ) )
    (loop
      (setq res (cons (car lis) res))
      (while (setq lis (cdr lis)))
    )
    res
  )
)


(defun flatten (lis)
 (cond
   ((atom lis) lis)
   ((atom (car lis)) (cons (car lis) (flatten (cdr lis)) ) )
   (t  (append (flatten (car lis)) (flatten (cdr lis)) ) )
 )
)








