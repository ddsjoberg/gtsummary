list_store <- gtk_list_store_new ("character", "integer", "logical")

sapply(character_vector,
       function(string) {
         ## Add a new row to the model
         iter <- list_store$append(iter)$iter
         list_store$set(iter, 0, string, 1, i, 2,  FALSE)
       })

## Modify a particular row
path <- gtkTreePathNewFromString("4")
iter <- list_store$getIter(path)$iter
list_store$set(iter, 2, TRUE)

