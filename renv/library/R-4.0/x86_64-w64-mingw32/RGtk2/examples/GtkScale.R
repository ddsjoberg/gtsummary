format_value_callback <- function(scale, value)
{
  return(paste("-->", format(value, nsmall=scale$getDigits()), "<--"), sep="")
}
