## ---- warning=FALSE-----------------------------------------------------------
library(kableExtra)
dt <- mtcars[1:5, 1:6]

## -----------------------------------------------------------------------------
kable(dt, "html") %>%
  kable_styling("striped") %>%
  add_footnote(c("Footnote 1", "Have a good day."), notation = "alphabet")

## -----------------------------------------------------------------------------
kable(dt, "html") %>%
  kable_styling("striped") %>%
  add_footnote(c("Footnote 1", "Have a good day."), notation = "number")

## -----------------------------------------------------------------------------
kable(dt, "html") %>%
  kable_styling("striped") %>%
  add_footnote(c("Footnote 1", "Footnote 2", "Footnote 3"), notation = "symbol")

## -----------------------------------------------------------------------------
kable(dt, "html", caption = "Demo Table[note]") %>%
  kable_styling("striped") %>%
  add_header_above(c(" ", "Group 1[note]" = 3, "Group 2[note]" = 3)) %>%
  add_footnote(c("This table is from mtcars", 
                 "Group 1 contains mpg, cyl and disp", 
                 "Group 2 contains hp, drat and wt"), 
               notation = "symbol")

