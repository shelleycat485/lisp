; extract every nth item from a list
; (everyn 2 '(1 2 3 4 5 6)) => (1 3 5)

(defun everyn (n lis)
  (everyn1 n n lis))

(defun everyn1 (n count lis)
  (cond ((null lis) ())
        ((eq count n) (cons (car lis) (everyn1 n 1 (cdr lis))))
        (t (everyn1 n (+ count 1) (cdr lis)))))

; single-function version using let and an iterative loop/until

(defun everyn2 (n lis)
  (let ( (result ()) (count n) (rest lis) )
    (loop
      (until (null rest))
      (cond ((eq count n) (setq result (cons (car rest) result)) (setq count 1))
            (t (setq count (+ count 1))))
      (setq rest (cdr rest))
    )
    (reverse result)
  ))

; prime factors of a positive integer, smallest first
; (primefactors 60) => (2 2 3 5)
; uses subtraction-based remainder test rather than divide, to avoid
; float-precision issues (as a.lsp's mod function also does)
;
; note: the stopping conditions below are written with "greaterp"
; (strict, built on minusp which is "< 0"). init.lsp's "lesserp" is
; strict too, (greaterp n2 n1), so either could be used.

(defun divisiblep (a b)
  (let ( (r a) )
    (loop
      (until (greaterp b r))    ; stop once r < b (strictly)
      (setq r (- r b))
    )
    (eq r 0)
  ))

(defun primefactors (n)
  (let ( (factors ()) (d 2) (m n) )
    (loop
      (until (greaterp 2 m))    ; stop once m < 2 (strictly)
      (cond
        ((divisiblep m d) (setq factors (cons d factors)) (setq m (/ m d)))
        (t (setq d (+ d 1)))
      )
    )
    (reverse factors)
  ))

; ---------------------------------------------------------------------
; witness/criminal interrogation puzzle
;
; 80 suspects, one criminal, one witness. When called into an
; interrogation together with a group of suspects, the witness will
; name the criminal if the witness is present AND the criminal is not
; (a group with neither the witness nor the criminal, or with both of
; them, produces nothing useful). We don't know who the witness or the
; criminal is, so a fixed sequence of interrogation groups must be
; chosen in advance that is guaranteed to put the (unknown) witness in
; a group without the (unknown) criminal at least once, whichever two
; of the 80 suspects they turn out to be.
;
; solution: factor n into primes, e.g. 80 = 2 * 2 * 2 * 2 * 5. Process
; the factors largest first. At each stage split every suspect's id
; (1..n) into equal contiguous chunks of the current "group size", and
; run one interrogation per chunk-position (so a stage with factor f
; needs f interrogations). Two different suspects always end up in a
; different chunk at some stage (their id's are different, and the
; stages together uniquely place every id), and each stage's f
; interrogations separate that difference in both directions (a
; genuine witness will always end up alone with the interrogator, i.e.
; in a group without the actual criminal, at least once).
;
; for 80 = 5 * 2 * 2 * 2 * 2: stage 1 (factor 5) splits the 80 suspects
; into 5 groups of 80/5 = 16; stage 2 (factor 2) uses a group/chunk
; size of 8; stage 3 a chunk size of 4; stage 4 a chunk size of 2;
; stage 5 a chunk size of 1 - all "2, or a multiple of 2" as expected
; since the remaining factors are all 2's. Total interrogations needed
; is 5+2+2+2+2 = 13, the sum of the prime factors - the minimum
; possible, since splitting by primes instead of any other factoring
; of 80 never increases that sum.
;
; ids that sit on several stage boundaries at once (multiples of the
; larger chunk sizes, e.g. 20, 40, 60 for n=80) are still uniquely
; identified by this scheme - every id gets a distinct set of
; group memberships across all stages - but they are the ones where an
; extra confirming question would be needed if the protocol only gave
; a yes/no answer instead of the witness naming the criminal outright.

; integer quotient and remainder of a/b (non-negative integers, b>0),
; built from subtraction so as not to depend on float divide/mod

(defun idiv (a b)
  (let ( (q 0) (r a) )
    (loop
      (until (greaterp b r))    ; stop once r < b (strictly)
      (setq r (- r b))
      (setq q (+ q 1))
    )
    q
  ))

(defun imod (a b)
  (let ( (r a) )
    (loop
      (until (greaterp b r))    ; stop once r < b (strictly)
      (setq r (- r b))
    )
    r
  ))

; which chunk-position (0..factor-1) id falls into at a stage with the
; given chunk size ("weight")

(defun digitof (id weight factor)
  (imod (idiv (- id 1) weight) factor))

; the interrogation set: every id in 1..n whose chunk-position at this
; stage equals v

(defun roundset (n weight factor v)
  (let ( (result ()) (id 1) )
    (loop
      (until (greaterp id n))
      (cond ((eq (digitof id weight factor) v) (setq result (cons id result))))
      (setq id (+ id 1))
    )
    (reverse result)
  ))

; suffixproducts of (f1 f2 ... fk) is (f1*f2*...*fk ... fk-1*fk fk 1)
; i.e. element i is the product of all factors from position i onward,
; with a trailing 1. Used to get each stage's chunk size (the product
; of all factors that come after it).

(defun suffixproducts (lis)
  (cond ((null lis) (list 1))
        (t (let ( (rest (suffixproducts (cdr lis))) )
             (cons (* (car lis) (car rest)) rest)))))

(defun weightsfor (factors)
  (cdr (suffixproducts factors)))

(defun printlevel (n weight factor stagenum)
  (let ( (v 0) )
    (prin "stage ") (prin stagenum) (prin ": factor ") (prin factor)
    (prin ", group size ") (print weight)
    (loop
      (until (eq v factor))
      (print (roundset n weight factor v))
      (setq v (+ v 1))
    )
    (print cr)
  ))

(defun interrolevels (n factors weights stagenum)
  (cond
    ((null factors) ())
    (t
      (printlevel n (car weights) (car factors) stagenum)
      (interrolevels n (cdr factors) (cdr weights) (+ stagenum 1))
    )
  ))

; prints the interrogation sets needed to identify the criminal among n
; suspects (ids 1..n), given one witness, using the minimum possible
; number of interrogations
; (interrogations 80)

(defun interrogations (n)
  (let ( (factors (reverse (primefactors n))) )
    (interrolevels n factors (weightsfor factors) 1)
  ))

; ---------------------------------------------------------------------
; given one specific witness/criminal pair, find how many interrogations
; are actually needed to solve it (may be fewer than the guaranteed
; worst-case total from `interrogations`), by walking the same
; largest-set-first stage order
; (solvepair 80 37 12)

(defun stagesolve (n witness criminal factors weights stagenum priorcount)
  (cond
    ((null factors) priorcount)  ; witness and criminal never differ (same id)
    (t
      (let ( (weight (car weights)) (factor (car factors)) )
        (let ( (wv (digitof witness weight factor)) (cv (digitof criminal weight factor)) )
          (cond
            ((eq wv cv)
              (stagesolve n witness criminal (cdr factors) (cdr weights)
                          (+ stagenum 1) (+ priorcount factor)))
            (t
              (prin "stage ") (prin stagenum)
              (prin ", interrogation ") (prin (+ wv 1)) (prin " of ") (prin factor)
              (prin ": group ") (print (roundset n weight factor wv))
              (+ priorcount wv 1))
          )
        )
      )
    )
  ))

(defun solvepair (n witness criminal)
  (let ( (factors (reverse (primefactors n))) )
    (let ( (count (stagesolve n witness criminal factors (weightsfor factors) 1 0)) )
      (prin "witness ") (prin witness) (prin " identifies criminal ") (prin criminal)
      (prin " after ") (prin count) (print " interrogation(s)")
      count
    )
  ))
