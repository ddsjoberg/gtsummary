## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(gt)

## ---- echo=FALSE, warning=FALSE-----------------------------------------------

# list of all the icons used in table
path_figure <- list(
  "img/icons8-smiling-100.png",
  "img/icons8-neutral-100.png",
  "img/icons8-disappointed-100.png",
  "img/icons8-no-entry-100.png",
  "img/icons8-under-construction-100.png"
)

# making table with gt
list(
  printer = c("gt", "kable", "flextable", "kableExtra", "huxtable", "tibble"),
  output = c("HTML", "PDF", "RTF", "Word")
) %>%
  purrr::cross_df() %>%
  dplyr::mutate(
    rating = dplyr::case_when(
      printer == "gt" & output == "HTML" ~ 1, # good output
      printer == "gt" & output %in% c("PDF", "RTF") ~ 5, # under construction
      printer == "gt" & output == "Word" ~ 4, # not supported
      printer == "kable" ~ 2, # ok output
      printer == "flextable" & output != "RTF" ~ 1, # good output
      printer == "flextable" & output == "RTF" ~ 4, # not supported
      printer == "kableExtra" & output %in% c("PDF", "HTML") ~ 1, # good output
      printer == "kableExtra" & output %in% c("RTF", "Word") ~ 4, # not supported
      printer == "huxtable" ~ 1, # good output
      printer == "tibble" ~ 3 # not great
    ) %>%
      factor()
  ) %>%
  tidyr::pivot_wider(id_cols = printer, names_from = output, values_from = rating) %>%
  dplyr::mutate(
    link = dplyr::case_when(
      printer == "gt" ~ 
        "[gt](https://gt.rstudio.com/index.html)",
      printer == "kable" ~ 
        "[kable](https://bookdown.org/yihui/rmarkdown-cookbook/kable.html)",
      printer == "flextable" ~
        "[flextable](https://davidgohel.github.io/flextable/articles/overview.html)",
      printer == "kableExtra" ~ 
        "[kableExtra](http://haozhu233.github.io/kableExtra/)",
      printer == "huxtable" ~
        "[huxtable](https://hughjonesd.github.io/huxtable/)",
      printer == "tibble" ~ 
        "[tibble](https://tibble.tidyverse.org/)"
    ),  
    fns = dplyr::case_when(
      printer == "gt" ~ "`as_gt()`",
      printer == "kable" ~ "`as_kable()`",
      printer == "flextable" ~ "`as_flex_table()`",
      printer == "kableExtra" ~ "`as_kable_extra()`",
      printer == "huxtable" ~ "`as_hux_table()`",
      printer == "tibble" ~ "`as_tibble()`"
    )
  ) %>%
  gt() %>%
  cols_move_to_start(columns = vars(link, fns)) %>%
  cols_hide(columns = vars(printer)) %>%
  cols_label(link = md("**Print Engine**"), 
             fns = md("**Function**"), 
             HTML = md("**HTML**"), PDF = md("**PDF**"), 
             RTF = md("**RTF**"), Word = md("**Word**")) %>%
  fmt_markdown(columns = vars(fns, link)) %>%
  data_color(
    columns = vars(HTML, PDF, RTF, Word),
    colors = scales::col_factor(
      palette = c("#bae1ff", "#ffb3ba", "#ffdfba", "#ffffba", "#baffc9"),
      domain = NULL,
      reverse = TRUE
    ),
    alpha = 0.8
  ) %>%
  text_transform(
    locations = cells_body(columns = vars(HTML, PDF, RTF, Word)),
    fn = function(x) {
      dplyr::case_when(
        x == 1 ~ local_image(filename = path_figure[[1]]),
        x == 2 ~ local_image(filename = path_figure[[2]]),
        x == 3 ~ local_image(filename = path_figure[[3]]),
        x == 4 ~ local_image(filename = path_figure[[4]]),
        x == 5 ~ local_image(filename = path_figure[[5]])
      )
    }
  ) %>%
  cols_width(vars(HTML, PDF, RTF, Word) ~ px(60),
             vars(link) ~ px(110),
             vars(link, fns) ~ px(140))

## ---- echo=FALSE--------------------------------------------------------------
tibble::tibble(
  figure = 1:5,
  desc = c(
    "Output fully supported",
    "Formatted output, but missing indentation, footnotes, spanning headers",
    "No formatted output",
    "Output not supported",
    "Under development"
  )
) %>%
  gt() %>%
  cols_label(figure = md("**Key**"), desc = "") %>%
  data_color(
    columns = vars(figure),
    colors = scales::col_factor(
      palette = c("#bae1ff", "#ffb3ba", "#ffdfba", "#ffffba", "#baffc9"),
      domain = NULL,
      reverse = TRUE
    ),
    alpha = 0.8
  ) %>%
  text_transform(
    locations = cells_body(columns = vars(figure)),
    fn = function(x) {
      dplyr::case_when(
        x == 1 ~ local_image(filename = path_figure[[1]], height = 20),
        x == 2 ~ local_image(filename = path_figure[[2]], height = 20),
        x == 3 ~ local_image(filename = path_figure[[3]], height = 20),
        x == 4 ~ local_image(filename = path_figure[[4]], height = 20),
        x == 5 ~ local_image(filename = path_figure[[5]], height = 20)
      )
    }
  ) %>%
  tab_options(table.font.size = 'x-small', data_row.padding = px(3))

## ---- eval=FALSE--------------------------------------------------------------
#  tbl_summary(trial) %>%
#    as_flex_table()

## ---- echo = FALSE------------------------------------------------------------
tibble::tibble(
  output = c("**HTML**", "**PDF**", "**RTF**", "**Word**"),
  default_printer = c("**{gt}**", "**kable**", "**kable**", "**flextable**"),
  desc = c(
    "**{gt}** output is fully supported with **HTML** output.",
    paste("You may force printing with **{gt}** by converting a **{gtsummary}**",
          "object to **{gt}** with `as_gt()`, e.g. `tbl_summary(trial) %>% as_gt()`.",
          "**PDF** output is under development in the **{gt}** package.",
          "You may get a gorgeous table, but you may also get an error or a malformed table."),
    paste("You may force printing with **{gt}** by converting a **{gtsummary}**",
          "object to **{gt}** with `as_gt()`, e.g. `tbl_summary(trial) %>% as_gt()`.",
          "**RTF** output is under development in the **{gt}** package.",
          "You may get a gorgeous table, but you may also get an error or a malformed table."),
    paste("**{flextable}** is the default print engine for **Word** output,",
          "as **{gt}** does not support **Word**. If **{flextable}** is not installed,",
          "**kable** is used.")
  )
) %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  cols_label(output = md("**Output Type**"), 
             default_printer = md("**Default Engine**"), 
             desc = md("**Details**")) %>%
  tab_options(table.font.size = 'small')

## ---- eval = FALSE------------------------------------------------------------
#  library(gtsummary)
#  system.file(package = "gtsummary") %>%
#    file.path("rmarkdown_example/gtsummary_rmarkdown_html.Rmd") %>%
#    file.edit()

## ---- eval = FALSE------------------------------------------------------------
#  # build gtsummary table
#  tbl <- tbl_summary(trial)
#  
#  # using the {gt} package
#  as_gt(tbl) %>% gt::as_latex()
#  
#  # using the {huxtable} package
#  as_hux_table(tbl) %>% huxtable::to_latex()
#  
#  # using the {kableExtra} package
#  as_kable_extra(tbl, format = "latex")
#  
#  # using the knitr::kable function
#  as_kable(tbl, format = "latex")

## ---- eval = FALSE------------------------------------------------------------
#  tbl_summary(trial) %>%    # build gtsummary table
#    as_gt() %>%             # convert to gt table
#    gt::gtsave(             # save table as image
#      filename = "my_table_image.png"
#    )

