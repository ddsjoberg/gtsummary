
frobnitz_result_func <- function(source_object, res, user_data)
{
  success <- _theoretical_frobnitz_finish (source_object, res, NULL)

  if (success)
    message("Hurray!")
  else 
    message("Uh oh!")

  ## ....
}

_theoretical_frobnitz_async (theoretical_data, 
                             NULL, 
                             frobnitz_result_func, 
                             NULL)
