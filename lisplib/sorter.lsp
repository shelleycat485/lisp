( setq  tree (quote
((() car ((() mud ()) on (() the ()))) the ())
))
( setq  orderp (quote
 greaterp 
))
( setq  put_in_right_subtree (quote
( lambda ( item  tree )( make_node ( item_in_node  tree )( left_subtree  tree )( add_item  item ( right_subtree  tree ))))
))
( setq  put_in_left_subtree (quote
( lambda ( item  tree )( make_node ( item_in_node  tree )( add_item  item ( left_subtree  tree ))( right_subtree  tree )))
))
( setq  add_item (quote
( lambda ( item  tree )( cond (( null  tree )( make_node  item ()()))(( orderp  item ( item_in_node  tree ))( put_in_left_subtree  item  tree ))( t ( put_in_right_subtree  item  tree ))))
))
( setq  sort (quote
( lambda ( lis  tree )( loop ( until ( null  lis ) tree ) ( setq  tree ( add_item ( car  lis ) tree ))( setq  lis ( cdr  lis ))))
  ))

( setq  print_tree (quote
( lambda ( tree )( cond (( null  tree )())( t ( print_tree ( left_subtree  tree ))( print ( item_in_node  tree ))( print_tree ( right_subtree  tree )))))
))

( setq  item_in_node (quote
( lambda ( tree )( cadr  tree ))
))

( setq  right_subtree (quote
( lambda ( tree )( car ( cddr  tree )))
))

( setq  left_subtree (quote
( lambda ( tree )( car  tree ))
))

( setq  make_node (quote
( lambda ( val  left  right )( list  left  val  right ))
))


