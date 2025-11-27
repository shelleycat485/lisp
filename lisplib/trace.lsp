(defun tron () (obl 7))
(defun troff () (obl 8))

; trace works for a function with evaluated arguments, e.g. (defun ggg (arg)...
; one function can be traced at a time at the moment
(defun savetracename (name) (put 'trace 'tracefunc name))
(defun gettracename () (get 'trace 'tracefunc))
(defun savetracedef (def) (put 'trace 'olddef def))
(defun gettracedef () (get 'trace 'olddef))
(defun cleartracedef () (remprop 'trace 'olddef) (remprop 'trace 'tracefunc))

(defun trace fn
 (setq fn (car fn))
  (cond
   ((not (null (gettracename()))) (list (gettracename) 'function 'is 'already 'traced) )
   ( t  (savetracedef (eval fn))
        (savetracename fn)
        (set fn (subst fn 'fn tracedef)) fn )
  )
)


(setq tracedef
    '(lambda *x
      (setq *x (mapcar 'eval *x)) ; eval args
      (prin " [traced]" ) (prin (gettracename)) (print *x)
      ; apply old function to the evaluated arguments
      (setq *x (apply (gettracedef)  *x )) 
      (prin " [traced]") (prin (gettracename)) (prin "=")  (print *x )
     *x   )
)

(defun untrace fn
  (setq fn (car fn))
  (cond
   ((gettracedef) (set fn (gettracedef)) (cleartracedef()) 'untraced ) 
   ( t (list fn 'is 'not 'traced))
  )
)


