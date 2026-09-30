; Show what type of arguments functions have, and what definions are not
; functions.  Written to output to a terminal, in colour: the terminal
; colour functions (screen_col_...) are in lisplib/ed.lsp, loaded here.
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp detect_function.lsp

; definitions look like this roughly:
; funname lambda (par par par) other_eles
; funname lambda uneval_par other_eles

(load 'lisplib/ed.lsp)

(setq aaa (oblist))

(defun prname (ele)
  (let ( (ele1 ()) )
  (print cr) (print ele)
   (setq ele1 (eval ele))
   (cond
     ((listp ele1)
        (cond 
	       ((not (listp (cadr ele1)))
		 (screen_col_white) (print "arg not list") 
		 (screen_col_tgreen) (print ele1))
	       (t (screen_col_green) (print (cadr ele1)) (prin rpar) (screen_col_tgreen))
       ))
     (t (screen_col_red) (print "not a func") (print ele1) (screen_col_tgreen))
   )
))

(mapc 'prname aaa)
(prin cr)
(exit)
