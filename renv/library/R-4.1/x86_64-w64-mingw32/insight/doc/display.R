## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("gt", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}


## ---- warning=FALSE, message=FALSE--------------------------------------------
library(insight)
library(dplyr)

df <- data.frame(
  Variable = c(1, 3, 5, 3, 1),
  Group = c("A", "A", "A", "B", "B"),
  CI = c(0.95, 0.95, 0.95, 0.95, 0.95),
  CI_low = c(3.35, 2.425, 6.213, 12.1, 1.23),
  CI_high = c(4.23, 5.31, 7.123, 13.5, 3.61),
  p = c(0.001, 0.0456, 0.45, 0.0042, 0.34)
)

df

## ---- eval=FALSE--------------------------------------------------------------
#  knitr::kable(df, format = "markdown")
#  

## ---- results='asis'----------------------------------------------------------
knitr::kable(df, format = "html")

## -----------------------------------------------------------------------------
format_table(df)

## -----------------------------------------------------------------------------
df %>% 
  mutate(p = format_p(p, stars = TRUE)) %>% 
  format_table()

## -----------------------------------------------------------------------------
cat(export_table(df))

## -----------------------------------------------------------------------------
export_table(df, format = "md")

## -----------------------------------------------------------------------------
export_table(df, format = "html")

## -----------------------------------------------------------------------------
df %>% 
  format_table(ci_brackets = c("(", ")")) %>% 
  export_table(format = "html")

