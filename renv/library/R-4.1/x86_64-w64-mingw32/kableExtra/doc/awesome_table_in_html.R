## -----------------------------------------------------------------------------
library(kableExtra)
dt <- mtcars[1:5, 1:6]

## -----------------------------------------------------------------------------
# If you are using kableExtra < 0.9.0, you are recommended to set a global option first.
# options(knitr.table.format = "html") 
## If you don't define format here, you'll need put `format = "html"` in every kable function.

## -----------------------------------------------------------------------------
kbl(dt)

## -----------------------------------------------------------------------------
dt %>%
  kbl() %>%
  kable_styling()

## -----------------------------------------------------------------------------
dt %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

## -----------------------------------------------------------------------------
dt %>%
  kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

## -----------------------------------------------------------------------------
dt %>%
  kbl() %>%
  kable_classic_2(full_width = F)

## -----------------------------------------------------------------------------
dt %>%
  kbl() %>%
  kable_minimal()

## -----------------------------------------------------------------------------
dt %>%
  kbl() %>%
  kable_material(c("striped", "hover"))

## -----------------------------------------------------------------------------
dt %>%
  kbl() %>%
  kable_material_dark()

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_paper(bootstrap_options = "striped", full_width = F)

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

## -----------------------------------------------------------------------------
kbl(mtcars[1:10, 1:5]) %>%
  kable_styling(fixed_thead = T)

## -----------------------------------------------------------------------------
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", 
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)

kbl(text_tbl) %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "30em", background = "yellow")

## -----------------------------------------------------------------------------
mtcars[1:8, 1:8] %>%
  kbl() %>%
  kable_paper(full_width = F) %>%
  column_spec(2, color = spec_color(mtcars$mpg[1:8]),
              link = "https://haozhu233.github.io/kableExtra/") %>%
  column_spec(6, color = "white",
              background = spec_color(mtcars$drat[1:8], end = 0.7),
              popover = paste("am:", mtcars$am[1:8]))

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
kbl(dt) %>%
  kable_paper("striped", full_width = F) %>%
  column_spec(5:7, bold = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_paper("striped", full_width = F) %>%
  row_spec(0, angle = -45)

## ---- message=FALSE, warning=FALSE--------------------------------------------
cs_dt <- mtcars[1:10, 1:2]
cs_dt$car = row.names(cs_dt)
row.names(cs_dt) <- NULL
cs_dt$mpg = cell_spec(cs_dt$mpg, color = ifelse(cs_dt$mpg > 20, "red", "blue"))
cs_dt$cyl = cell_spec(
  cs_dt$cyl, color = "white", align = "c", angle = 45, 
  background = factor(cs_dt$cyl, c(4, 6, 8), c("#666666", "#999999", "#BBBBBB")))
cs_dt <- cs_dt[c("car", "mpg", "cyl")]

kbl(cs_dt, escape = F) %>%
  kable_paper("striped", full_width = F)

# You can also do this with dplyr and use one pipe from top to bottom
# library(dplyr)
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
#   kable_paper("striped", full_width = F)

## -----------------------------------------------------------------------------
vs_dt <- iris[1:10, ]
vs_dt[1:4] <- lapply(vs_dt[1:4], function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
})
vs_dt[5] <- cell_spec(vs_dt[[5]], color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, option = "A", direction = -1))
kbl(vs_dt, escape = F, align = "c") %>%
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
#   kable_paper(c("striped", "condensed"), full_width = F)

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
popover_dt <- data.frame(
  position = c("top", "bottom", "right", "left"),
  stringsAsFactors = FALSE
)
popover_dt$`Hover over these items` <- cell_spec(
  paste("Message on", popover_dt$position), # Cell texts
  popover = spec_popover(
    content = popover_dt$position,
    title = NULL,                           # title will add a Title Panel on top
    position = popover_dt$position
  ))
kbl(popover_dt, escape = FALSE) %>%
  kable_paper("striped", full_width = FALSE)

## ---- message = FALSE, warning=FALSE------------------------------------------
library(formattable)
ft_dt <- mtcars[1:5, 1:4]
ft_dt$car <- row.names(ft_dt)
row.names(ft_dt) <- NULL
ft_dt$mpg <- color_tile("white", "orange")(ft_dt$mpg)
ft_dt$cyl <- cell_spec(ft_dt$cyl, angle = (1:5)*60, 
                      background = "red", color = "white", align = "center")
ft_dt$disp <- ifelse(
  ft_dt$disp > 200,
  cell_spec(ft_dt$disp, color = "red", bold = T),
  cell_spec(ft_dt$disp, color = "green", italic = T)
)
ft_dt$hp <- color_bar("lightgreen")(ft_dt$hp)
ft_dt <- ft_dt[c("car", "mpg", "cyl", "disp", "hp")]

kbl(ft_dt, escape = F) %>%
  kable_paper("hover", full_width = F) %>%
  column_spec(5, width = "3cm") %>%
  add_header_above(c(" ", "Hello" = 2, "World" = 2))

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_classic() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_paper() %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6))

## -----------------------------------------------------------------------------
kbl(mtcars[1:10, 1:6], caption = "Group Rows") %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Group 1", 4, 7) %>%
  pack_rows("Group 2", 8, 10)

## ---- eval = F----------------------------------------------------------------
#  # Not evaluated. This example generates the same table as above.
#  kbl(mtcars[1:10, 1:6], caption = "Group Rows") %>%
#    kable_paper("striped", full_width = F) %>%
#    pack_rows(index = c(" " = 3, "Group 1" = 4, "Group 2" = 3))

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Group 1", 3, 5, label_row_css = "background-color: #666; color: #fff;")

## ---- eval=F------------------------------------------------------------------
#  # Method 1
#  pack_rows() # instead of group_rows()
#  
#  # Method 2
#  library(dplyr)
#  library(kableExtra)
#  
#  # Method 3
#  conflicted::conflict_prefer("group_rows", "kableExtra", "dplyr")

## -----------------------------------------------------------------------------
kbl(dt) %>%
  kable_paper("striped", full_width = F) %>%
  add_indent(c(1, 3, 5))

## -----------------------------------------------------------------------------
collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
                 C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
                 C3 = 1:15,
                 C4 = sample(c(0,1), 15, replace = TRUE))
kbl(collapse_rows_dt, align = "c") %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1:2, valign = "top")

## -----------------------------------------------------------------------------
kbl(dt, align = "c") %>%
  kable_classic(full_width = F) %>%
  footnote(general = "Here is a general comments of the table. ",
           number = c("Footnote 1; ", "Footnote 2; "),
           alphabet = c("Footnote A; ", "Footnote B; "),
           symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
           )

## -----------------------------------------------------------------------------
kbl(dt, align = "c") %>%
  kable_paper(full_width = F) %>%
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
                                footnote_marker_symbol(1))
row.names(dt_footnote)[4] <- paste0(row.names(dt_footnote)[4], 
                                footnote_marker_alphabet(1))
kbl(dt_footnote, align = "c", 
      # Remember this escape = F
      escape = F) %>%
  kable_paper(full_width = F) %>%
  footnote(alphabet = "Footnote A; ",
           symbol = "Footnote Symbol 1; ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T)

## -----------------------------------------------------------------------------
kbl(cbind(mtcars, mtcars)) %>%
  kable_paper() %>%
  scroll_box(width = "500px", height = "200px")

## -----------------------------------------------------------------------------
kbl(cbind(mtcars, mtcars)) %>%
  add_header_above(c("a" = 5, "b" = 18)) %>%
  kable_paper() %>%
  scroll_box(width = "100%", height = "200px")

## ---- eval=FALSE--------------------------------------------------------------
#  kbl(mtcars) %>%
#    kable_paper() %>%
#    save_kable(file = "table1.html", self_contained = T)

## -----------------------------------------------------------------------------
# Not evaluated
library(sparkline)
sparkline(0)

## -----------------------------------------------------------------------------
spk_dt <- data.frame(
  var = c("mpg", "wt"),
  sparkline = c(spk_chr(mtcars$mpg), spk_chr(mtcars$wt))
)

kbl(spk_dt, escape = F) %>%
  kable_paper(full_width = F)

## ---- eval=F------------------------------------------------------------------
#  # Not evaluating
#  xtable::xtable(mtcars[1:4, 1:4], caption = "Hello xtable") %>%
#    xtable2kable() %>%
#    column_spec(1, color = "red")

