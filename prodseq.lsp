(defun prodseq (n)
  (let (( count 1)( result ()))
     (loop
       (until (greaterp count n))
       (setq result (append result (list count)))
       (setq count (plus count 2))
       )
    result   )
    ) 


(defun sumpair (p) (apply 'plus p) )

(defun combine (la lb)
  (cond
    ((null la) ())
    (t (cons (list (car la) (car lb)) (combine (cdr la) (cdr lb))))
    )
)

(defun printcr (a) (print a) (print cr))

(defun showres (n)
  (print "The list of numbers is ") (printcr (prodseq n))
  (print "The length of the list is (we will call this n) is ") (printcr (length (prodseq n)))
  (print "Here is the reversed list ") (printcr (reverse (prodseq n))) 
  (printcr "Now sum each element of the two lists. First we show them in pairs:")
  (printcr (combine (prodseq n) (reverse (prodseq n)) ) )
  (printcr "Then show each pair added.")
  (printcr (mapcar sumpair (combine (prodseq n) (reverse (prodseq n)) ) ))
  (printcr "Every added number is the same: 2 * n. Both the list and the reversed list have the same numbers in. So each list must total n.")
  )
