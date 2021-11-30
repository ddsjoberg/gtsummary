## ---- echo = F----------------------------------------------------------------
options(kableExtra.latex.load_packages = F)

## -----------------------------------------------------------------------------
library(kableExtra)
dt <- mtcars[1:5, 1:6]

## -----------------------------------------------------------------------------
# If you are using kableExtra < 0.9.0, you are recommended to set a global option first.
# options(knitr.table.format = "latex") 
## If you don't define format here, you'll need put `format = "latex"` 
## in every kable function.

## ---- eval = FALSE------------------------------------------------------------
#  # Not evaluated. Illustration purpose
#  options(kableExtra.latex.load_packages = FALSE)
#  library(kableExtra)

## -----------------------------------------------------------------------------
# Again, with kableExtra >= 0.9.0, `format = "latex"` is automatically defined
# when this package gets loaded. Otherwise, you still need to define formats
kbl(dt)
# Same: kable(dt, "latex")

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T)

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped")

## -----------------------------------------------------------------------------
kbl(mtcars[1:8, 1:4], booktabs = T, linesep = "") %>%
  kable_styling(latex_options = "striped", stripe_index = c(1,2, 5:6))

## -----------------------------------------------------------------------------
kbl(dt, caption = "Demo table", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

## -----------------------------------------------------------------------------
kbl(cbind(dt, dt, dt), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## -----------------------------------------------------------------------------
kbl(cbind(dt), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## -----------------------------------------------------------------------------
long_dt <- rbind(mtcars, mtcars) 

kbl(long_dt, longtable = T, booktabs = T, caption = "Longtable") %>%
  add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6)) %>%
  kable_styling(latex_options = c("repeat_header"))

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling(full_width = T) %>%
  column_spec(1, width = "8cm")

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling(position = "center")

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling(position = "float_right")

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling(font_size = 7)

## -----------------------------------------------------------------------------
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", 
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)

kbl(text_tbl, booktabs = T) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, color = "red") %>%
  column_spec(2, width = "30em")

## -----------------------------------------------------------------------------
that_cell <- c(rep(F, 7), T)
mtcars[1:8, 1:8] %>%
  kbl(booktabs = T, linesep = "") %>%
  kable_paper(full_width = F) %>%
  column_spec(2, color = spec_color(mtcars$mpg[1:8]),
              link = "https://haozhu233.github.io/kableExtra") %>%
  column_spec(6, color = "white", 
              background = spec_color(mtcars$drat[1:8], end = 0.7),
              popover = paste("am:", mtcars$am[1:8])) %>%
  column_spec(9, strikeout = that_cell, bold = that_cell,
              color = c(rep("black", 7), "red"))

## -----------------------------------------------------------------------------
tbl_img <- data.frame(
  name = c("kableExtra 1", "kableExtra 2"),
  logo = ""
)
tbl_img %>%
  kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(2, image = "kableExtra_sm.png")

## -----------------------------------------------------------------------------
tbl_img %>%
  kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(2, image = spec_image(
    c("kableExtra_sm.png", "kableExtra_sm.png"), 50, 50))

## -----------------------------------------------------------------------------
mpg_list <- split(mtcars$mpg, mtcars$cyl)
disp_list <- split(mtcars$disp, mtcars$cyl)
inline_plot <- data.frame(cyl = c(4, 6, 8), mpg_box = "", mpg_hist = "",
                          mpg_line1 = "", mpg_line2 = "",
                          mpg_points1 = "", mpg_points2 = "", mpg_poly = "")
inline_plot %>%
  kbl(booktabs = TRUE) %>%
  kable_paper(full_width = FALSE) %>%
  column_spec(2, image = spec_boxplot(mpg_list)) %>%
  column_spec(3, image = spec_hist(mpg_list)) %>%
  column_spec(4, image = spec_plot(mpg_list, same_lim = TRUE)) %>%
  column_spec(5, image = spec_plot(mpg_list, same_lim = FALSE)) %>%
  column_spec(6, image = spec_plot(mpg_list, type = "p")) %>%
  column_spec(7, image = spec_plot(mpg_list, disp_list, type = "p")) %>%
  column_spec(8, image = spec_plot(mpg_list, polymin = 5))

## -----------------------------------------------------------------------------
coef_table <- data.frame(
  Variables = c("var 1", "var 2", "var 3"),
  Coefficients = c(1.6, 0.2, -2.0),
  Conf.Lower = c(1.3, -0.4, -2.5),
  Conf.Higher = c(1.9, 0.6, -1.4)
) 

data.frame(
  Variable = coef_table$Variables,
  Visualization = ""
) %>%
  kbl(booktabs = T) %>%
  kable_classic(full_width = FALSE) %>%
  column_spec(2, image = spec_pointrange(
    x = coef_table$Coefficients, 
    xmin = coef_table$Conf.Lower, 
    xmax = coef_table$Conf.Higher, 
    vline = 0)
    )

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(7, border_left = T, bold = T) %>%
  row_spec(1, strikeout = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "black")

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T, align = "c") %>%
  kable_styling(latex_options = "striped", full_width = F) %>%
  row_spec(0, angle = 45)

## ---- message=FALSE, warning=FALSE--------------------------------------------
cs_dt <- mtcars[1:10, 1:2]
cs_dt$car = row.names(cs_dt)
row.names(cs_dt) <- NULL
cs_dt$mpg = cell_spec(cs_dt$mpg, color = ifelse(cs_dt$mpg > 20, "red", "blue"))
cs_dt$cyl = cell_spec(
  cs_dt$cyl, color = "white", align = "c", angle = 45, 
  background = factor(cs_dt$cyl, c(4, 6, 8), c("#666666", "#999999", "#BBBBBB")))
cs_dt <- cs_dt[c("car", "mpg", "cyl")]

kbl(cs_dt, booktabs = T, escape = F) %>%
  kable_paper("striped", full_width = F)

# You can also do this with dplyr and use one pipe from top to bottom
# mtcars[1:10, 1:2] %>%
#   mutate(
#     car = row.names(.),
#     mpg = cell_spec(mpg, "html", color = ifelse(mpg > 20, "red", "blue")),
#     cyl = cell_spec(cyl, "html", color = "white", align = "c", angle = 45, 
#                     background = factor(cyl, c(4, 6, 8), 
#                                         c("#666666", "#999999", "#BBBBBB")))
#   ) %>%
#   select(car, mpg, cyl) %>%
#   kbl(format = "html", escape = F) %>%
#   kable_styling("striped", full_width = F)

## -----------------------------------------------------------------------------
vs_dt <- iris[1:10, ]
vs_dt[1:4] <- lapply(vs_dt[1:4], function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
})
vs_dt[5] <- cell_spec(vs_dt[[5]], color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, option = "A", direction = -1))
kbl(vs_dt, booktabs = T, escape = F, align = "c") %>%
  kable_classic("striped", full_width = F)
# Or dplyr ver
# iris[1:10, ] %>%
#   mutate_if(is.numeric, function(x) {
#     cell_spec(x, bold = T, 
#               color = spec_color(x, end = 0.9),
#               font_size = spec_font_size(x))
#   }) %>%
#   mutate(Species = cell_spec(
#     Species, color = "white", bold = T,
#     background = spec_color(1:10, end = 0.9, option = "A", direction = -1)
#   )) %>%
#   kable(escape = F, align = "c") %>%
#   kable_styling(c("striped", "condensed"), full_width = F)

## -----------------------------------------------------------------------------
sometext <- strsplit(paste0(
  "You can even try to make some crazy things like this paragraph. ", 
  "It may seem like a useless feature right now but it's so cool ",
  "and nobody can resist. ;)"
), " ")[[1]]
text_formatted <- paste(
  text_spec(sometext, color = spec_color(1:length(sometext), end = 0.9),
            font_size = spec_font_size(1:length(sometext), begin = 5, end = 20)),
  collapse = " ")

# To display the text, type `r text_formatted` outside of the chunk

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped") %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6), bold = T, italic = T)

## -----------------------------------------------------------------------------
kbl(mtcars[1:10, 1:6], caption = "Group Rows", booktabs = T) %>%
  kable_styling() %>%
  pack_rows("Group 1", 4, 7) %>%
  pack_rows("Group 2", 8, 10)

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  pack_rows("Group 1", 4, 5, latex_gap_space = "2em")

## ---- eval=FALSE--------------------------------------------------------------
#  kbl(mtcars[1:10, 1:6], caption = "Group Rows", booktabs = T) %>%
#    kable_styling() %>%
#    pack_rows(index=c(" " = 3, "Group 1" = 4, "Group 2" = 3))
#  # Not evaluated. The code above should have the same result as the first example in this section.

## ---- eval=F------------------------------------------------------------------
#  kbl(mtcars[1:2, 1:2], align = c("cl"))
#  # \begin{tabular}{l|cl|cl}  # Note the column alignment here
#  # \hline
#  #   & mpg & cyl\\
#  # ...

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T) %>%
  add_indent(c(1, 3, 5))

## -----------------------------------------------------------------------------
kbl(dt, booktabs = T, align = "l") %>%
  add_indent(c(1, 3, 5), level_of_indent = 2, all_cols = T)

## -----------------------------------------------------------------------------
collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
                 C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
                 C3 = 1:15,
                 C4 = sample(c(0,1), 15, replace = TRUE))
kbl(collapse_rows_dt, booktabs = T, align = "c") %>%
  column_spec(1, bold=T) %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")

## -----------------------------------------------------------------------------
kbl(collapse_rows_dt[-1], align = "c", booktabs = T) %>%
  column_spec(1, bold = T, width = "5em") %>%
  row_spec(c(1:7, 11:12) - 1, extra_latex_after = "\\rowcolor{gray!6}") %>%
  collapse_rows(1, latex_hline = "none")

## -----------------------------------------------------------------------------
collapse_rows_dt <- expand.grid(
  District = sprintf('District %s', c('1', '2')),
  City = sprintf('City %s', c('1', '2')),
  State = sprintf('State %s', c('a', 'b')),
  Country = sprintf('Country with a long name %s', c('A', 'B'))
) 
collapse_rows_dt <- collapse_rows_dt[c("Country", "State", "City", "District")]
collapse_rows_dt$C1 = rnorm(nrow(collapse_rows_dt))
collapse_rows_dt$C2 = rnorm(nrow(collapse_rows_dt))

kbl(collapse_rows_dt, 
      booktabs = T, align = "c", linesep = '') %>%
  collapse_rows(1:3, row_group_label_position = 'stack') 

## -----------------------------------------------------------------------------
row_group_label_fonts <- list(
  list(bold = T, italic = T), 
  list(bold = F, italic = F)
  )
kbl(collapse_rows_dt, 
                     booktabs = T, align = "c", linesep = '') %>%
  column_spec(1, bold=T) %>%
  collapse_rows(1:3, latex_hline = 'custom', custom_latex_hline = 1:3, 
                row_group_label_position = 'stack', 
                row_group_label_fonts = row_group_label_fonts) 

## -----------------------------------------------------------------------------
kbl(dt, align = "c") %>%
  kable_styling(full_width = F) %>%
  footnote(general = "Here is a general comments of the table. ",
           number = c("Footnote 1; ", "Footnote 2; "),
           alphabet = c("Footnote A; ", "Footnote B; "),
           symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
           )

## -----------------------------------------------------------------------------
kbl(dt, align = "c", booktabs = T) %>%
  footnote(general = "Here is a general comments of the table. ",
           number = c("Footnote 1; ", "Footnote 2; "),
           alphabet = c("Footnote A; ", "Footnote B; "),
           symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2"),
           general_title = "General: ", number_title = "Type I: ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T, title_format = c("italic", "underline")
           )

## -----------------------------------------------------------------------------
dt_footnote <- dt
names(dt_footnote)[2] <- paste0(names(dt_footnote)[2], 
                                # That "latex" can be eliminated if defined in global
                                footnote_marker_symbol(1, "latex"))
row.names(dt_footnote)[4] <- paste0(row.names(dt_footnote)[4], 
                                footnote_marker_alphabet(1))
kbl(dt_footnote, align = "c", booktabs = T,
      # Remember this escape = F
      escape = F) %>%
  footnote(alphabet = "Footnote A; ",
           symbol = "Footnote Symbol 1; ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T)

## -----------------------------------------------------------------------------
kbl(dt, align = "c", booktabs = T, caption = "s") %>%
  footnote(general = "Here is a very very very very very very very very very very very very very very very very very very very very long footnote", 
           threeparttable = T)

## -----------------------------------------------------------------------------
dt_lb <- data.frame(
  Item = c("Hello\nWorld", "This\nis a cat"), 
  Value = c(10, 100)
)
dt_lb$Item = linebreak(dt_lb$Item)

# Or you can use
# dt_lb <- dt_lb %>%
#   mutate_all(linebreak)

dt_lb %>%
  kbl(booktabs = T, escape = F,
      col.names = linebreak(c("Item\n(Name)", "Value\n(Number)"), align = "c"))

## -----------------------------------------------------------------------------
kbl(dt, caption = "Demo Table (Landscape)[note]", booktabs = T) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(" ", "Group 1[note]" = 3, "Group 2[note]" = 3)) %>%
  add_footnote(c("This table is from mtcars", 
                 "Group 1 contains mpg, cyl and disp", 
                 "Group 2 contains hp, drat and wt"), 
               notation = "symbol") %>%
  pack_rows("Group 1", 4, 5) %>%
  landscape()

## ---- eval = FALSE------------------------------------------------------------
#  # not evaluated
#  k <- mtcars[1:10,1:5]
#  names(k) <- paste("{", names(k), "}")
#  kableExtra::kable(
#       k, "latex", booktabs = TRUE, longtable = TRUE,
#       align = c("l", rep("d", 4)), linesep = "", escape = FALSE) %>%
#    kable_styling(full_width=FALSE)

## ---- eval = F----------------------------------------------------------------
#  # Not evaluated.
#  
#  # The code below will automatically include the image in the rmarkdown document
#  kbl(dt, booktabs = T) %>%
#    column_spec(1, bold = T) %>%
#    as_image()
#  
#  # If you want to save the image locally, just provide a name
#  kbl(dt, booktabs = T) %>%
#    column_spec(1, bold = T) %>%
#    save_kable("my_latex_table.png")

## ---- eval=F------------------------------------------------------------------
#  # Not evaluating
#  xtable::xtable(mtcars[1:4, 1:4], caption = "Hello xtable") %>%
#    xtable2kable() %>%
#    column_spec(1, color = "red")

