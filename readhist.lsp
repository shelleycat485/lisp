(setq fh (open "history.txt"))

(setq hist ())
(defun readhist (fh)
  (loop
    (until (eof fh))
    (cons (read) hist)
    )
  )
)


