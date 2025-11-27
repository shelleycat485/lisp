( setq  radial (quote
( lambda ( size )( pd )( move  size )( pu )( turn  180 )( move  size )( turn  180 ))
))
( setq  radturn (quote
( lambda ( size  ang )( radial  size )( turn  ang ))
))
( setq  starburst (quote
( lambda ( size )( repeat  15 ( radturn  size ( /  360  15 ))))
))
( setq  star20 (quote
( lambda ( size )( repeat  20 ( radturn  size ( /  360  20 ))))
))
( setq  pu (quote
( lambda ()( pendown  0 ))
))
( setq  pd (quote
( lambda ()( pendown  1 ))
))
( setq  triangle (quote
( lambda ( size )( repeat  3 ( single_side  120  size )))
))
( setq  square (quote
( lambda ( size )( repeat  4 ( single_side  90  size )))
))
( setq  single_side (quote
( lambda ( ang  length )( turn  ang )( move  length ))
))
( setq  repeat1 (quote
( lambda ( n  arg )( loop ( until ( minusp ( setq  n ( -  n  1 ))))( eval  arg )))
))
( setq  repeat (quote
( lambda  lis ( repeat1 (eval ( car  lis ))( cadr  lis )))
))
( setq  newscreen (quote
( lambda (x y )( initturtle  x  y ))
))
( setq  spiral1 (quote
( lambda ( steps  size  ang  inc )( loop ( until ( minusp ( setq  steps ( -  steps  1 ))))( turn  ang )( move ( setq  size ( +  size  inc )))))
))
( setq  spiral (quote
( lambda ()  (home) (pencolour 0 0 255 )( pendown  1 )( spiral1  200  0  41  1 ))
))

