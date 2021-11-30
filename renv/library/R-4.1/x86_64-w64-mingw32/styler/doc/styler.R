## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "")
knitr::knit_engines$set(list(
  styler = function(options) {
    options$comment <- ""
    knitr::engine_output(
      options,
      c("# Before", options$code),
      c("# After", styler::style_text(options$code))
    )
  }
))

## -----------------------------------------------------------------------------
library(styler)
style_text("a + b", transformers = tidyverse_style(scope = "indention"))

## ---- results = 'hide'--------------------------------------------------------
# equivalent
style_text("a + b", transformers = tidyverse_style(scope = "indention"))
style_text("a + b", scope = "indention")

## -----------------------------------------------------------------------------
# tokens and everything less invasive
style_text("a=2", scope = "tokens")

# just tokens and indention
style_text("a=2", scope = I(c("tokens", "indention")))

## -----------------------------------------------------------------------------
style_text(
  "tibble::tibble(
     small  = 2 ,
     medium = 4,#comment without space
     large  = 6
   )"
)

## -----------------------------------------------------------------------------
styler::style_text(
  "
  #> blocks
  blibala= 3
  # styler: off
  I_have(good+reasons, to = turn_off,
    styler
  )
  # styler: on
  1+1

  #> inline
  ignore( this) # styler: off
  f( ) # not ignored anymore
"
)

## -----------------------------------------------------------------------------
out <- withr::with_tempfile(
  "code.R",
  {
    writeLines("1+1", "code.R")
    style_file("code.R", dry = "on")
  }
)
out

## -----------------------------------------------------------------------------
style_text(
  "1++1/2*2^2",
  math_token_spacing = specify_math_token_spacing(zero = c("'/'", "'*'", "'^'"))
)

## -----------------------------------------------------------------------------
style_text(
  c(
    "a <- function() {",
    "### not to be indented",
    "# indent normally",
    "33",
    "}"
  ),
  reindention = specify_reindention(regex_pattern = "###", indention = 0),
  indent_by = 4
)

