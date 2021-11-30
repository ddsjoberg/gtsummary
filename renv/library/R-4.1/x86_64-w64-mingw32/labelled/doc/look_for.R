## ----message=FALSE------------------------------------------------------------
library(dplyr)

## -----------------------------------------------------------------------------
iris %>% as_tibble()

## -----------------------------------------------------------------------------
data(fertility, package = "questionr")
women

## -----------------------------------------------------------------------------
glimpse(iris)
glimpse(women)

## -----------------------------------------------------------------------------
library(labelled)
look_for(iris)
look_for(women)

## -----------------------------------------------------------------------------
# Look for a single keyword.
look_for(iris, "petal")
look_for(iris, "s")

# Look for with a regular expression
look_for(iris, "petal|species")
look_for(iris, "s$")

# Look for with several keywords
look_for(iris, "pet", "sp")

# Look_for will take variable labels into account
look_for(women, "read", "level")

## -----------------------------------------------------------------------------
look_for(women, "read")
look_for(women, "read", labels = FALSE)

## -----------------------------------------------------------------------------
look_for(iris, "sepal")
look_for(iris, "sepal", ignore.case = FALSE)

## -----------------------------------------------------------------------------
look_for(women, "id", details = "none")

## -----------------------------------------------------------------------------
look_for(women, details = "full")
look_for(women, details = "full") %>%
  dplyr::glimpse()

## ---- eval=FALSE--------------------------------------------------------------
#  look_for(women) %>% View()

## -----------------------------------------------------------------------------
look_for(women) %>% as_tibble()
glimpse(look_for(women))

## -----------------------------------------------------------------------------
look_for(women) %>% convert_list_columns_to_character()

## -----------------------------------------------------------------------------
look_for(women) %>% lookfor_to_long_format()

## -----------------------------------------------------------------------------
look_for(women) %>%
  lookfor_to_long_format() %>%
  convert_list_columns_to_character()

