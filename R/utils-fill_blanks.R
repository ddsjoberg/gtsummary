

# little function to fill in blank rows if not specified
# used in the constuction of header rows
fill_blanks <- function(x, max) {
  c(rep("", max - length(x)), x)
}
