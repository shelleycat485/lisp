 (defun delete ( item lis)
 (cond
  ((null lis) ())
  ((equal item (car lis)) (delete item (cdr lis)) )
  ( t (cons (car lis) (delete item (cdr lis)) ) )
))
