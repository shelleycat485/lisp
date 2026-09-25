; redblacktree.lsp
;
; A red-black (self balancing) version of the word tree in a.lsp.
;
; a.lsp builds an ordinary binary tree in wtree with (inittree) and
; (addtotree word); each node is (val left right) where val is (word count).
; Words added in sorted order make that tree degenerate into a list.
;
; Here each node is (val left right colour), colour being red or black.
; The first three elements are exactly as in a.lsp, so printtree,
; printtreeele, compnode and readtextfile all work unchanged on a
; red-black tree. Insertion uses the Okasaki balance: any black node with a
; red child that itself has a red child is rewritten as a red node with two
; black children.
;
; Needs a.lsp loaded first (for compnode/orderp/orderall). Loading this
; file redefines addtotree so that readtextfile builds a red-black tree;
; the original unbalanced insert is still available as addtotree*.
;
; Usage (from the lisp directory):
;   lisp lisplib/init.lsp a.lsp redblacktree.lsp
;   (inittree) (mapc 'addtotree '(a b c d e f g)) (printtree wtree)
;   (rbcheck wtree)      ; black height, or () if the tree is not valid
;   (treedepth wtree)    ; depth, works on either kind of tree


(defun rbmakenode (mkv mkl mkr mkc) (list mkv mkl mkr mkc))

(defun isred (irn) (and irn (eq (cadddr irn) 'red)))

; the same node coloured black
(defun rbblacken (bkn)
  (cond
    ((null bkn) ())
    ( t (rbmakenode (car bkn) (cadr bkn) (caddr bkn) 'black))
  )
)

; build a node, fixing a red child with a red child under a black node
(defun rbbalance (bc bv bl br)
  (cond
    ((eq bc 'red) (rbmakenode bv bl br 'red))
    ((and (isred bl) (isred (cadr bl)))              ; left left
      (rbmakenode (car bl)
                  (rbblacken (cadr bl))
                  (rbmakenode bv (caddr bl) br 'black)
                  'red))
    ((and (isred bl) (isred (caddr bl)))             ; left right
      (let ((blr (caddr bl)))
        (rbmakenode (car blr)
                    (rbmakenode (car bl) (cadr bl) (cadr blr) 'black)
                    (rbmakenode bv (caddr blr) br 'black)
                    'red)))
    ((and (isred br) (isred (cadr br)))              ; right left
      (let ((brl (cadr br)))
        (rbmakenode (car brl)
                    (rbmakenode bv bl (cadr brl) 'black)
                    (rbmakenode (car br) (caddr brl) (caddr br) 'black)
                    'red)))
    ((and (isred br) (isred (caddr br)))             ; right right
      (rbmakenode (car br)
                  (rbmakenode bv bl (cadr br) 'black)
                  (rbblacken (caddr br))
                  'red))
    ( t (rbmakenode bv bl br 'black))
  )
)

; insert iv below tree it, a new word goes in as a red leaf with count 1,
; an existing word has its count incremented
(defun rbins (iv it)
  (let ( (ires ()) (inta (rbmakenode (list iv 1) () () 'red)) )
  (cond
    ((null it) inta)
    ( t (setq ires (compnode inta it))
      (cond
        ((eq ires 'eqi)
          (rbmakenode (list (caar it) (plus 1 (cadar it)))
                      (cadr it) (caddr it) (cadddr it)))
        ((eq ires 'greater)
          (rbbalance (cadddr it) (car it) (cadr it) (rbins iv (caddr it))))
        ( t
          (rbbalance (cadddr it) (car it) (rbins iv (cadr it)) (caddr it)))
      )
    )
  )
  )
)

; returns the new tree, the root is always black
(defun rbaddtotree* (av at)
  (cond
    ((null av) at)
    ( t (rbblacken (rbins av at)))
  )
)

; drop-in replacement for addtotree in a.lsp
(defun addtotree (val) (setq wtree (rbaddtotree* val wtree)))


; black height of the tree if it is a valid red-black tree, else ()
; (root black, no red node with a red child, same black count on every path)
(defun rbcheck (ct)
  (cond
    ((isred ct) ())
    ( t (rbheight ct))
  )
)

(defun rbheight (ht)
  (cond
    ((null ht) 1)
    ((and (isred ht) (or (isred (cadr ht)) (isred (caddr ht)))) ())
    ( t
      (let ((hl (rbheight (cadr ht))))
      (let ((hr (rbheight (caddr ht))))
        (cond
          ((or (null hl) (null hr)) ())
          ((not (eq hl hr)) ())
          ((isred ht) hl)
          ( t (plus hl 1))
        )
      ))
    )
  )
)

; number of nodes on the longest path from the root, for either tree type
(defun treedepth (dt)
  (cond
    ((null dt) 0)
    ( t
      (let ((dl (treedepth (cadr dt))))
      (let ((dr (treedepth (caddr dt))))
        (cond
          ((greaterp dl dr) (plus dl 1))
          ( t (plus dr 1))
        )
      ))
    )
  )
)
