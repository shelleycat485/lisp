; wcreadtext.lsp
;
; Word count of a text file, held in a red-black tree.
;
; Usage (from the lisp directory):
;   lisp stdload.lsp a.lsp wcreadtext.lsp
;   (wcreadtext 'aetdra.txt)   ; read the file, prints totals
;   (wcshow)                   ; every word, most frequent first
;   (wctop 20)                 ; just the 20 most frequent
;
; Words are runs of letters and digits. An apostrophe or hyphen between
; two letters is part of the word, so don't, I'd, Juliette's and
; revenge-porn are each one word; a leading or trailing one (quote marks
; such as 'Shame' or a plural possessive such as friends') is dropped.
; Double quotes, other punctuation, spaces and line ends separate words.
; Plurals are not combined: word and words are counted separately.
;
; Case is ignored when counting, so The and the are one word. A word is
; shown in lower case if it ever appears in lower case in the text,
; otherwise as first written, so I, I'd and names keep their capitals.
;
; Each node is (val left right colour) as in redblacktree.lsp, where val is
; (key count shown): key is the lower case word, shown is the form printed.
; The tree is kept in wctree, so (rbcheck wctree) and (treedepth wctree)
; work on it.

(load 'redblacktree.lsp)  ; for rbmakenode, rbbalance, rbblacken, rbcheck


(defun wcreadtext (wcfname)
  (let ( (wcfh (open wcfname)) (wcch ()) )
  (setq wctree ())
  (setq wcword ())
  (setq wctotal 0)
  (setq wcdistinct 0)
  (loop
    (until (eof wcfh))
    (setq wcch (readch wcfh))
    (wcaddchar wcch)
  )
  (wcflush)
  (close wcfh)
  (print wcfname) (print ": ") (print wctotal) (print " words, ")
  (print wcdistinct) (print " distinct") (print cr)
  wcdistinct
  )
)

; letters and digits
(defun wcalnum (wcc)
  (let ((wco (ordinal wcc)))
  (or (and (greaterp wco 47) (greaterp 58 wco))
      (and (greaterp wco 64) (greaterp 91 wco))
      (and (greaterp wco 96) (greaterp 123 wco)))
  )
)

; apostrophe or hyphen, kept only between two letters or digits
(defun wcjoiner (wcc) (member (ordinal wcc) '(39 45)))

; one character of the file, wcword holds the current word reversed
(defun wcaddchar (wcc)
  (cond
    ((null wcc) (wcflush))                 ; readch gives () at the end
    ((wcalnum wcc) (setq wcword (cons wcc wcword)))
    ((wcjoiner wcc)
      (cond
        ((null wcword) ())                 ; leading, not part of a word
        ((wcjoiner (car wcword)) (wcflush)) ; two together, e.g. -- dash
        ( t (setq wcword (cons wcc wcword)))
      ))
    ( t (wcflush))
  )
)

; end of a word: drop any trailing apostrophe or hyphen, then count it
(defun wcflush ()
  (loop
    (while (and wcword (wcjoiner (car wcword))))
    (setq wcword (cdr wcword))
  )
  (and wcword (wcaddword (reverse wcword)))
  (setq wcword ())
)

(defun wclower (wcc)
  (let ((wco (ordinal wcc)))
  (cond
    ((and (greaterp wco 64) (greaterp 91 wco)) (implode (list (plus wco 32))))
    ( t wcc)
  )
  )
)

(defun wcaddword (wcchars)
  (setq wctotal (plus wctotal 1))
  (setq wctree
    (rbblacken (wcins (implode (mapcar 'wclower wcchars)) (implode wcchars) wctree)))
)

; true if word wka sorts before word wkb, character by character
(defun wcless (wka wkb) (wcless* (explode wka) (explode wkb)))

(defun wcless* (wla wlb)
  (cond
    ((null wlb) ())
    ((null wla) t)
    ((eq (car wla) (car wlb)) (wcless* (cdr wla) (cdr wlb)))
    ( t (greaterp (ordinal (car wlb)) (ordinal (car wla))))
  )
)

; insert word wkey (lower case), as written wshown, below tree wt
(defun wcins (wkey wshown wt)
  (cond
    ((null wt)
      (setq wcdistinct (plus wcdistinct 1))
      (rbmakenode (list wkey 1 wshown) () () 'red))
    ((eq wkey (caar wt))
      (rbmakenode
        (list wkey (plus 1 (cadar wt))
              (cond ((eq wshown wkey) wkey) ( t (caddr (car wt)))))
        (cadr wt) (caddr wt) (cadddr wt)))
    ((wcless wkey (caar wt))
      (rbbalance (cadddr wt) (car wt) (wcins wkey wshown (cadr wt)) (caddr wt)))
    ( t
      (rbbalance (cadddr wt) (car wt) (cadr wt) (wcins wkey wshown (caddr wt))))
  )
)


; the (key count shown) of every node, in alphabetical order
(defun wcinorder (wit wacc)
  (cond
    ((null wit) wacc)
    ( t (wcinorder (cadr wit) (cons (car wit) (wcinorder (caddr wit) wacc))))
  )
)

; insert count wn into the list wdl of distinct counts, largest first
(defun wcaddcount (wn wdl)
  (cond
    ((null wdl) (list wn))
    ((eq wn (car wdl)) wdl)
    ((greaterp wn (car wdl)) (cons wn wdl))
    ( t (cons (car wdl) (wcaddcount wn (cdr wdl))))
  )
)

; print up to wmax words (all if wmax is 0), most frequent first,
; words with the same count in alphabetical order
(defun wctop (wmax)
  (let ( (wall (wcinorder wctree ())) (wcounts ()) (wrest ()) (wdone 0) )
  (setq wrest wall)
  (loop
    (while wrest)
    (setq wcounts (wcaddcount (cadar wrest) wcounts))
    (setq wrest (cdr wrest))
  )
  (loop
    (while wcounts)
    (until (and (greaterp wmax 0) (not (greaterp wmax wdone))))
    (setq wrest wall)
    (loop
      (while wrest)
      (until (and (greaterp wmax 0) (not (greaterp wmax wdone))))
      (and (eq (cadar wrest) (car wcounts))
           (wcshowone (car wrest))
           (setq wdone (plus wdone 1)))
      (setq wrest (cdr wrest))
    )
    (setq wcounts (cdr wcounts))
  )
  wdone
  )
)

(defun wcshowone (wval)
  (print (cadr wval)) (print (caddr wval)) (print cr)
  t
)

; every word, most frequent first
(defun wcshow () (wctop 0))
