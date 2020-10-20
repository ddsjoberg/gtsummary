
# Escapes any characters that would have special
# meaning in a regular expression
# based on Hmisc::escapeRegex
.escape_regex <- function (string)
{
  gsub(
    "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
    string
  )
}


# remove backticks around variable names
.clean_backticks <- function(x, variable_names = x) {
  for (i in stats::na.omit(variable_names)) {
    x <- stringr::str_replace_all(
      x,
      paste0("`", .escape_regex(i), "`"),
      i
    )

    if (stringr::str_detect(i, "^`.*`$")) {
      x <- stringr::str_replace_all(
        x,
        .escape_regex(i),
        stringr::str_sub(i, 2, -2)
      )
    }
  }
  x
}
