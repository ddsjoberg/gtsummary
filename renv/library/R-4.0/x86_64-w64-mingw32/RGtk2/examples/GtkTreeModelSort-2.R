# Accessing the child model in a selection changed callback

selection_changed <- function(selection, data)
{
  # Get the current selected row and the model.
  selected <- selection$getSelected()
  if (!selected[[1]])
    return()

  ## Look up the current value on the selected row and get a new value
  ## to change it to.
  some_data <- selected$model$get(selected$iter, COLUMN_1)
  
  modified_data <- change_the_data(some_data)

  ## Get an iterator on the child model, instead of the sort model.
  child_iter <- sort_model$convertIterToChildIter(selected$iter)$iter

  ## Get the child model and change the value of the row.  In this
  ## example, the child model is a GtkListStore.  It could be any other
  ## type of model, though.
  child_model <- sort_model$getModel()
  child_model$set(child_iter, COLUMN_1, modified_data)
}
