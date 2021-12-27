## ----setup, echo = FALSE, warning = FALSE, message = FALSE--------------------

library(knitr)
library(dplyr)
library(huxtable)
options(
        huxtable.knit_print_df       = FALSE, 
        huxtable.add_colnames        = TRUE,  # needed when run by testthat
        huxtable.latex_use_fontspec  = TRUE
      )

is_latex <- guess_knitr_output_format() == "latex"
# is_latex <- TRUE
knitr::knit_hooks$set(
  barrier = function(before, options, envir) {
    if (! before && is_latex) knitr::asis_output("\\FloatBarrier")
  }
)

if (is_latex) knitr::opts_chunk$set(barrier = TRUE)


## ---- echo = FALSE------------------------------------------------------------
huxtable::hux_logo(latex = is_latex, html = ! is_latex)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("huxtable")

## -----------------------------------------------------------------------------
library(huxtable)
 
jams <- hux(
        Type  = c("Strawberry", "Raspberry", "Plum"),
        Price = c(1.90, 2.10, 1.80)
      )

## -----------------------------------------------------------------------------
data(mtcars)
car_ht <- as_hux(mtcars)

## ---- results = "markup"------------------------------------------------------
print_screen(jams)     # on the R command line, you can just type "jams"

## -----------------------------------------------------------------------------
jams

## -----------------------------------------------------------------------------

library(dplyr) 

jams %>% 
      set_all_padding(4) %>% 
      set_outer_padding(0) %>% 
      set_number_format(2) %>% 
      set_bold(row = 1, col = everywhere) %>% 
      set_bottom_border(row = 1, col = everywhere) %>% 
      set_width(0.4) %>% 
      set_caption("Pots of jam for sale")
  

## ---- eval = FALSE------------------------------------------------------------
#  jams <- set_all_padding(jams, 4)
#  jams <- set_outer_padding(jam, 0)

## -----------------------------------------------------------------------------

# set all padding:
left_padding(jams) <- 4
right_padding(jams) <- 4
top_padding(jams) <- 4
bottom_padding(jams) <- 4

# set outer padding:
left_padding(jams)[1:nrow(jams), 1] <- 0
top_padding(jams)[1, 1:ncol(jams)] <- 0
right_padding(jams)[1:nrow(jams), ncol(jams)] <- 0
bottom_padding(jams)[nrow(jams), 1:ncol(jams)] <- 0

number_format(jams) <- 2
bold(jams)[1, 1:ncol(jams)] <- TRUE
bottom_border(jams)[1, 1:ncol(jams)] <- 0.4
width(jams) <- 0.4
caption(jams) <- "Pots of jam for sale"


## ---- eval = FALSE------------------------------------------------------------
#  names(x)[1] <- "Name"

## ---- eval = FALSE------------------------------------------------------------
#  bold(jams)[1, 1:ncol(jams)] <- TRUE

## ---- eval = FALSE------------------------------------------------------------
#  ht <- set_property(ht, row = rows, col = cols, value)

## ---- eval = FALSE------------------------------------------------------------
#  ht <- set_property(ht, value)

## ---- eval = FALSE------------------------------------------------------------
#  ht <- set_property(ht, value)

## ----props, echo = FALSE------------------------------------------------------
sides <- c("left_", "right_", "top_", "bottom_")
props <- list()
props[["Cell Text"]] <- sort(c("text_color", "wrap", "bold", "italic", "font",
      "font_size", "na_string", "escape_contents", "markdown", "number_format",
      "rotation"))

props[["Cell"]] <- sort(c(
        "align", "valign", "rowspan", "colspan", "background_color", 
        paste0(sides, "border"), 
        paste0(sides, "border_color"), 
        paste0(sides, "border_style"), 
        paste0(sides, "padding")
      ))

props[["Row"]]    <- c("row_height", "header_rows")
props[["Column"]] <- c("col_width", "header_cols")
props[["Table"]]  <- sort(c("width", "height", "position", "caption", 
  "caption_pos", "caption_width", "tabular_environment", "table_environment", 
  "label", "latex_float"))

maxl <- max(sapply(props, length))
props <- lapply(props, function(x) c(x, rep("", maxl - length(x))))

mono_font <- if (is_latex) "LucidaConsole" else "monospace"

prop_hux <- hux(as.data.frame(props, check.names = FALSE)) %>% 
      set_font_size(10)                                    %>% 
      set_font(-1, everywhere, mono_font)                  %>% 
      set_header_rows(1, TRUE)                             %>% 
      set_width(0.9)                                       %>% 
      set_tb_padding(2)                                    %>% 
      set_caption("Huxtable properties")                   %>% 
      set_label("tab:props")                               %>% 
      set_col_width(c(.2, .25, .15, .15, .25))             %>% 
      theme_bright()                                      

prop_hux

## ---- eval = FALSE------------------------------------------------------------
#  # Set the italic property on row 1, column 1:
#  jams %>% set_italic(1, 1)

## ---- eval = FALSE------------------------------------------------------------
#  # Set the italic property on column 1 of every row matching "berry":
#  is_berry <- grepl("berry", jams$Type)
#  jams %>% set_italic(is_berry, 1)

## ---- eval = FALSE------------------------------------------------------------
#  # Set the italic property on row 1 of the column named "Type":
#  jams %>% set_italic(1, "Type")

## ---- eval = FALSE------------------------------------------------------------
#  italic(jams)[1, "Type"] <- TRUE
#  # the same as:
#  jams <- jams %>% set_italic(1, "Type")

## ---- eval = FALSE------------------------------------------------------------
#  # Set the italic property on row 1 of every column whose name starts with "T":
#  jams %>%
#        set_italic(1, starts_with("T"))

## ---- eval = FALSE------------------------------------------------------------
#  # Set the italic property on row 1 of all columns:
#  jams %>% set_italic(1, everywhere)
#  
#  # Set the italic property on all rows of column 1:
#  jams %>% set_italic(everywhere, 1)

## ---- eval = FALSE------------------------------------------------------------
#  
#  jams %>% set_italic(final(2), everywhere)
#  # same as:
#  jams %>% set_italic(3:4, 1:2)

## -----------------------------------------------------------------------------
jams %>% 
      set_text_color(2:3, 1, "purple")

## -----------------------------------------------------------------------------
jams %>% 
      set_background_color(evens, everywhere, "grey95")

## -----------------------------------------------------------------------------
jams %>% 
      set_markdown_contents(1, 1, "*Type* of jam") %>% 
      set_markdown_contents(1, 2, "*Price* of jam") %>% 
      set_markdown_contents(3, 2, "~~2.10~~ **Sale!** 1.50")

## -----------------------------------------------------------------------------
jams %>%
      set_align(1, everywhere, "center")

## -----------------------------------------------------------------------------
numbers <- hux(Numbers = c(100, 3.14, 0.0002))
numbers %>%
      set_align(-1, 1, ".") %>%
      theme_basic()

## -----------------------------------------------------------------------------
jams %>% 
      set_right_border(everywhere, 1, brdr(3, "double", "grey"))

## -----------------------------------------------------------------------------
jams %>% 
      set_right_border(everywhere, 1, 3) %>% 
      set_right_border_style(everywhere, 1, "double") %>% 
      set_right_border_color(everywhere, 1, "grey")

## -----------------------------------------------------------------------------
jams %>% 
      set_background_color(evens, everywhere, "grey80") %>% 
      set_background_color(odds, everywhere, "grey90") %>% 
      set_all_borders(brdr(0.4, "solid", "white")) %>% 
      set_outer_padding(4)

## ---- eval = FALSE------------------------------------------------------------
#  jams[3, 1] <- "Snozberry"

## ---- eval = FALSE------------------------------------------------------------
#  # Summer sale!
#  jams$Price <- c("Price", 1.50, 1.60, 1.50)

## -----------------------------------------------------------------------------
jams$Sugar <- c("Sugar content", "40%", "50%", "30%")
jams

## -----------------------------------------------------------------------------
rbind(jams, c("Gooseberry", 2.1, "55%"))

## -----------------------------------------------------------------------------
best_before <- c("Best before", c("Aug 2022", "Sept 2022", "June 2022"))

cbind(jams[, 1], best_before, jams[, -1])

## -----------------------------------------------------------------------------
jams %>% 
      insert_column(best_before, after = "Type") %>% 
      set_number_format(everywhere, 2, 0) # correct the formatting for dates


## -----------------------------------------------------------------------------
jams %>% 
      mutate(
        Type = toupper(Type)
      ) %>% 
      select(Type, Price)

## -----------------------------------------------------------------------------
jams_data <- data.frame(
        Type = c("Strawberry", "Raspberry", "Plum"),
        Price = c(1.90, 2.10, 1.80)
      )

jams_ordered <- jams_data %>% 
      arrange(Price) %>% 
      as_hux() %>% 
      set_bold(1, everywhere) # et cetera...

## ---- eval = FALSE------------------------------------------------------------
#  # Same result as above
#  
#  jams_data %>%
#        as_hux(add_colnames = FALSE) %>%
#        arrange(Price) %>%
#        add_colnames()

## -----------------------------------------------------------------------------
iris_hux <- iris %>% 
      group_by(Species) %>% 
      select(Species, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
      slice(1:5) %>% 
      as_hux() %>%
      theme_basic() %>% 
      set_tb_padding(2)

iris_hux      

## -----------------------------------------------------------------------------
iris_hux <- iris_hux %>% 
  set_contents(1, 2:5, c("Length", "Width", "Length", "Width")) %>% 
  insert_row("", "Sepal", "", "Petal", "", after = 0) %>% 
  merge_cells(1, 2:3) %>% 
  merge_cells(1, 4:5) %>% 
  set_align(1, everywhere, "center") %>% 
  set_tb_padding(1, everywhere, 0) %>% 
  set_bold(1, everywhere)
  
iris_hux

## -----------------------------------------------------------------------------
iris_hux_wide <- iris_hux %>% 
      set_header_rows(1:2, TRUE) %>% 
      restack_across(rows = 7) %>% 
      set_bottom_border(final(1), everywhere)

iris_hux_wide

## -----------------------------------------------------------------------------
lego_hux <- as_hux(matrix(1:16, 4, 4)) %>% 
      set_background_color(1:2, 1:2, "red") %>% 
      set_background_color(1:2, 3:4, "yellow") %>% 
      set_background_color(3:4, 1:2, "darkgreen") %>% 
      set_background_color(3:4, 3:4, "blue") %>% 
      set_text_color(3:4, 1:4, "white") %>% 
      set_all_borders(brdr(2, "solid", "white"))

lego_hux %>% set_caption("Original table")

lego_hux %>% 
      restack_across(rows = 2) %>% 
      set_caption("Restacked across")

lego_hux %>% 
      restack_down(cols = 2) %>% 
      set_caption("Restacked down")

## -----------------------------------------------------------------------------
iris_hux_wide %>% 
      set_width(0.8) %>% 
      set_font_size(8) %>% 
      set_lr_padding(2) %>% 
      set_col_width(rep(c(0.4, 0.2, 0.2, 0.2, 0.2)/3, 3)) %>% 
      set_position("left")

## ---- echo = FALSE------------------------------------------------------------
jams %>% 
      set_position("wrapright") %>% 
      set_width(0.35) %>% 
      set_caption(NA) %>% 
      set_font_size(8) %>% 
      theme_compact()

## ---- eval = FALSE------------------------------------------------------------
#  set_row_property(ht, row, value)

## -----------------------------------------------------------------------------
iris_hux <- iris_hux %>% 
      set_header_rows(1:2, TRUE) %>% 
      set_header_cols(1, TRUE) %>% 
      style_headers(bold = TRUE, text_color = "grey40")

iris_hux

## -----------------------------------------------------------------------------
list_of_iris <- split_across(iris_hux, c(7, 12))
list_of_iris[[1]] %>% set_caption("Setosa Irises")
list_of_iris[[2]] %>% set_caption("Versicolor Irises")
list_of_iris[[3]] %>% set_caption("Virginica Irises")

## -----------------------------------------------------------------------------
theme_mondrian(jams)

## -----------------------------------------------------------------------------
jams %>% map_background_color(by_rows("grey90", "grey95"))

## -----------------------------------------------------------------------------

iris_hux %>% 
      map_text_color(-(1:2), -1, 
        by_colorspace("darkred", "grey50", "darkgreen", colwise = TRUE)
      )


## -----------------------------------------------------------------------------
jams %>% map_text_color(by_regex("berry" = "red4", "navy"))

## ---- include = FALSE---------------------------------------------------------
options(huxtable.knit_print_df = TRUE)

## -----------------------------------------------------------------------------
head(iris)

## -----------------------------------------------------------------------------
options(huxtable.knit_print_df = FALSE)

head(iris) # back to normal

## ---- include = FALSE---------------------------------------------------------
options(huxtable.knit_print_df = TRUE)

## ---- results = "markup"------------------------------------------------------
print_screen(jams)

## ---- eval = FALSE------------------------------------------------------------
#  quick_pdf(iris_hux)
#  quick_pdf(iris_hux, file = "iris.pdf")

## -----------------------------------------------------------------------------

lm1 <- lm(mpg ~ cyl, mtcars)
lm2 <- lm(mpg ~ hp, mtcars)
lm3 <- lm(mpg ~ cyl + hp, mtcars)

huxreg(lm1, lm2, lm3)

