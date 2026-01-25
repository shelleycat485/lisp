; debugging tool, when something is wrong with a sexp in a file
; see what can be read from the file

(defun semicolon () (prin (implode (cons 59 ())))() )

(defun screen_col_any (c)
  (prin (implode (list 27 "[")))
  (semicolon)
  (prin "3") (prin c) (prin "m")
  ()
  )

(defun screen_col_red () (screen_col_any 1))
(defun screen_col_white () (screen_col_any 7))
(defun screen_col_green () (screen_col_any 2))

(defun screen_col_tgreen ()
  (prin (implode (list 27 "[" "3" "8")))
  (semicolon)
  (prin "5")
  (semicolon)
  (prin (implode (list "4" "6" "m")))
  ()
  )


(defun read_eles_from_file (fname)
  (let ( (fh (open fname)) (exp () ) (count 0)  )
    (loop
      (until (eof fh))
      (setq exp (read fh))
      (setq count (+ count 1))
      (mapc 'print (list (screen_col_red) count "reading"
               (screen_col_tgreen) exp cr cr))
      (readch) ; pauses until return pressed
      )
    (close fh)
    )
  )


