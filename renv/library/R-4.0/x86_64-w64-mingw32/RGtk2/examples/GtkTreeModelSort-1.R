## Using a GtkTreeModel sort

## get the child model
child_model <- get_my_model()

## Create the first tree 
sort_model1 <- gtkTreeModelSort(child_model)
tree_view1 <- gtkTreeView(sort_model1)

## Create the second tree
sort_model2 <- gtkTreeModelSort(child_model)
tree_view2 <- gtkTreeView(sort_model2)

## Now we can sort the two models independently
sort_model1$setSortColumnId(0, "ascending")
sort_model2$setSortColumnId(0, "descending")
