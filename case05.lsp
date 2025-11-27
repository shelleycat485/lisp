; coding and decoding using an keyword to substitute and shift some letter
;
;(make_encoder_decoder '(MATTHEW))
;(encode '(THE word))
;(decode '(RCEVLPH))

(defun alphabet () (explode (implode (generate 65 90 1))))
(defun alphablo () (explode (implode (generate 97 122 1))))

; removes letters that are in keyword from the alphabet, returns keyword
; at the front of the result
(defun delkwfromalphabet (kwl allst)
   ( let ((res kwl))
   (loop
     (while kwl)
     (setq allst (delete (car kwl) allst))
     (setq kwl (cdr kwl))
) (append res allst) ))

; makes the association list from two lists, length to be the same
(defun combine (la lb)
  (cond
    ((null la) ())
    (t (cons (list (car la) (car lb)) (combine (cdr la) (cdr lb))))
    )
)

; could be much more efficient, since the alis is calculated on every call
(defun toupcase (lis)
  (let (
     (alis (combine (alphablo) (alphabet) ))
       )
  (cond 
    (( null lis) ())
    ((assoc (car lis)  alis) (cons (cadr (assoc (car lis) alis)) (toupcase (cdr lis))))
    ( t (cons (car lis) (toupcase (cdr lis))))
  )
  )
)

; using local variable to save recalculating alis in every call
(defunloc toupcase (lis)
  (local alis)
  (or (member 'alis (oblist)) (setq alis (combine (alphablo) (alphabet))))
  (cond
    ((null lis) () )
    ((assoc (car lis) alis) (cons (cadr (assoc (car lis) alis)) (toupcase (cdr lis))))
    (t (cons (car lis) (toupcase (cdr lis))))
    )
)



; deletes letters from list, so SOSE becomes SOE
(defun deleteduplicates (lis) (reverse (deldup1 (reverse lis))))
(defun deldup1 (lis)
  (cond
    ( (null lis) ())
    ( (null (cdr lis)) lis )
    ( (member (car lis) (cdr lis)) (deldup1 (cdr lis)))
    ( t (cons (car lis) (deldup1 (cdr lis))))
    )
) 


; delete repeated letters from the keyword
(defun preprocess (kw)
    (deleteduplicates (toupcase (explode (concatenate kw))))
)

; whole of this is concatenated, so will run into length limit if too long
(defun preprocessword (kw)
   (toupcase (explode (concatenate kw)))
)

; makes a pair of association lists
(defun make_encoder_decoder (kw)
  (setq kw (preprocess kw))
  (setq decoder (combine (delkwfromalphabet kw (alphabet)) (alphabet) ))
  (setq encoder (combine (alphabet) (delkwfromalphabet kw (alphabet))  ))
)

(defun encode (words)
 (mapcar '(lambda (n) (cadr (assoc n encoder))) (preprocessword words))
)

(defun decode (words)
 (mapcar '(lambda (n) (cadr (assoc n decoder))) (preprocessword words))
)

