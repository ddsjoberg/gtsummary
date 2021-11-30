## Reading data from a GtkTreeModel

## make a new list_store
list_store <- gtkListStore("character", "integer")

## Fill the list store with data
populate_model(list_store)

## Get the first iter in the list 
result <- list_store$getIterFirst()

row_count <- 1
while(result[[1]]) {
  ## Walk through the list, reading each row
      
  data <- list_store$get(result$iter, 0, 1)
  
  ## Do something with the data
  print(paste("Row ", row_count, ": (", data[[1]], ",", data[[2]], ")", sep=""))
  
  row_count <- row_count + 1
  result <- list_store$iterNext()
}
