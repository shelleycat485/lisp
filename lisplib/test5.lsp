; test file 5  23/5/88
; testing operation of init.lsp library

(setq test '(a b c d e f g))

(mapc (quote prin) test)

(mapcar (quote prin) test)

(apply (quote print) (quote (a b 1 2 c)) )

(greaterp 100 50)

(greaterp 100 200)

(lesserp 50 100)

(lesserp 2000 3)

(append test test)
(append test ())
(eq test (append test ()))
(equal test (append test ()) )

(equal 4 4)
(equal test test)
(equal (quote bb) (quote bb))

(oblist)

(reverse test)
(reverse ())

(delete (quote d) test)
(member (quote d) test)

(setq test (quote ((1 a) (2 b) (3 c) (4 d) (5 e) (6 f)) ))

(assoc 4 test)
 
; testing property lists

(put 'roger 'son 'daniel)
(get 'roger 'son)
(obl 1)
(put 'roger 'another! son 'lawrence)
(get 'roger 'another! son)
(remprop 'roger 'son)
(get 'roger 'son)
(get 'roger 'another! son)
(remprop 'roger 'another! son)
(obl 1)

; testing unevaluated defuns

(defun adlist arg
 (set (car arg) (plus (cadr arg) (car (cddr arg))) )
)
(adlist qq 45 35)
qq
