; Show what type of arguments functions have, and what definions are not
; functions.  Need to load read_eles_from_file.lsp.

; definitions look like this roughly:
; funname lambda (par par par) other_eles
; funname lambda uneval_par other_eles

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
(exit)
