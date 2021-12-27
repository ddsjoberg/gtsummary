## Acquiring a GtkTreeIter

## Three ways of getting the iter pointing to the location

## get the iterator from a string 
model$getIterFromString("3:2:5")$iter

## get the iterator from a path
path <- gtkTreePathNewFromString("3:2:5")
model$getIter(path)$iter

## walk the tree to find the iterator
parent_iter <- model$iterNthChild(NULL, 3)$iter
parent_iter <- model$iterNthChild(parent_iter, 2)$iter
model$iterNthChild(parent_iter, 5)$iter
