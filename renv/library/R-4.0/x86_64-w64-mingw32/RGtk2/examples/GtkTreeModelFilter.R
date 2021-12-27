visible_func <- function(model, iter, data)
{
  ## Visible if row is non-empty and first column is "HI"
  visible <- FALSE

  str <- model$get(iter, 0)[[1]]
  if (identical(str, "HI"))
    visible <- TRUE

  return(visible)
}
