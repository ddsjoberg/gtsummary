## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

pillar:::set_show_source_hooks()

## ----setup--------------------------------------------------------------------
library(pillar)

## ----echo = FALSE, error = TRUE-----------------------------------------------
DiagrammeR::mermaid("format.mmd")

## -----------------------------------------------------------------------------
tbl <- tibble::tibble(a = 1:3, b = tibble::tibble(c = 4:6, d = 7:9), e = 10:12)
print(tbl, width = 23)
str(tbl)

## ----show_source = TRUE-------------------------------------------------------
#  print.tbl <- function (x, width = NULL, ..., n = NULL, n_extra = NULL) 
#  {
#      writeLines(format(x, width = width, ..., n = n, n_extra = n_extra))
#      invisible(x)
#  }

## ----show_source = TRUE-------------------------------------------------------
#  format.tbl <- function (x, width = NULL, ..., n = NULL, n_extra = NULL) 
#  {
#      check_dots_empty(action = signal)
#      force(x)
#      num_colors(forget = TRUE)
#      setup <- tbl_format_setup(x, width = width, ..., n = n, max_extra_cols = n_extra)
#      header <- tbl_format_header(x, setup)
#      body <- tbl_format_body(x, setup)
#      footer <- tbl_format_footer(x, setup)
#      c(header, body, footer)
#  }

## -----------------------------------------------------------------------------
setup <- tbl_format_setup(tbl, width = 24)
setup

## ----show_source = TRUE-------------------------------------------------------
#  tbl_format_setup <- function (x, width = NULL, ..., n = NULL, max_extra_cols = NULL) 
#  {
#      "!!!!DEBUG tbl_format_setup()"
#      width <- get_width_print(width)
#      n <- get_n_print(n, nrow(x))
#      max_extra_cols <- get_max_extra_cols(max_extra_cols)
#      out <- tbl_format_setup_(x, width, ..., n = n, max_extra_cols = max_extra_cols)
#      return(out)
#      UseMethod("tbl_format_setup")
#  }

## ----show_source = TRUE-------------------------------------------------------
#  tbl_format_setup.tbl <- function (x, width, ..., n, max_extra_cols) 
#  {
#      "!!!!DEBUG tbl_format_setup.tbl()"
#      rows <- nrow(x)
#      if (is.na(rows)) {
#          df <- df_head(x, n + 1)
#          if (nrow(df) <= n) {
#              rows <- nrow(df)
#          }
#          else {
#              df <- vec_head(df, n)
#          }
#      }
#      else {
#          df <- df_head(x, n)
#      }
#      if (is.na(rows)) {
#          needs_dots <- (nrow(df) >= n)
#      }
#      else {
#          needs_dots <- (rows > n)
#      }
#      if (needs_dots) {
#          rows_missing <- rows - n
#      }
#      else {
#          rows_missing <- 0
#      }
#      tbl_sum <- tbl_sum(x)
#      rownames(df) <- NULL
#      colonnade <- ctl_colonnade(df, has_row_id = if (.row_names_info(x) > 
#          0) 
#          "*"
#      else TRUE, width = width, controller = x)
#      body <- colonnade$body
#      extra_cols <- colonnade$extra_cols
#      extra_cols_total <- length(extra_cols)
#      if (extra_cols_total > max_extra_cols) {
#          length(extra_cols) <- max_extra_cols
#      }
#      new_tbl_format_setup(x = x, df = df, width = width, tbl_sum = tbl_sum, 
#          body = body, rows_missing = rows_missing, rows_total = rows, 
#          extra_cols = extra_cols, extra_cols_total = extra_cols_total)
#  }

## -----------------------------------------------------------------------------
tbl_format_header(tbl, setup)
tbl_format_body(tbl, setup)
tbl_format_footer(tbl, setup)

## -----------------------------------------------------------------------------
class(tbl_format_body(tbl, setup))
typeof(tbl_format_body(tbl, setup))

## ----show_source = TRUE-------------------------------------------------------
#  tbl_format_header.tbl <- function (x, setup, ...) 
#  {
#      named_header <- setup$tbl_sum
#      if (all(names2(named_header) == "")) {
#          header <- named_header
#      }
#      else {
#          header <- paste0(align(paste0(names2(named_header), ":"), 
#              space = NBSP), " ", named_header)
#      }
#      style_subtle(format_comment(header, width = setup$width))
#  }

## ----show_source = TRUE-------------------------------------------------------
#  tbl_format_body.tbl <- function (x, setup, ...) 
#  {
#      force(setup)
#      setup$body
#  }

## ----show_source = TRUE-------------------------------------------------------
#  tbl_format_footer.tbl <- function (x, setup, ...) 
#  {
#      footer <- pre_dots(format_footer(x, setup))
#      footer_comment <- split_lines(format_comment(footer, width = setup$width))
#      style_subtle(footer_comment)
#  }

## -----------------------------------------------------------------------------
ctl_new_compound_pillar(tbl, tbl$a, width = 20)
ctl_new_compound_pillar(tbl, tbl$b, width = 20)

## ----show_source = TRUE-------------------------------------------------------
#  ctl_new_compound_pillar.tbl <- function (controller, x, width, ..., title = NULL) 
#  {
#      "!!!!DEBUG ctl_new_compound_pillar.tbl(`v(width)`, `v(title)`)"
#      if (is.data.frame(x)) {
#          new_data_frame_pillar(x, controller, width, title = title)
#      }
#      else if (is.matrix(x)) {
#          new_matrix_pillar(x, controller, width, title = title)
#      }
#      else if (is.array(x) && length(dim(x)) > 1) {
#          new_array_pillar(x, controller, width, title = title)
#      }
#      else {
#          ctl_new_pillar(controller, x, width, ..., title = prepare_title(title))
#      }
#  }

## -----------------------------------------------------------------------------
ctl_new_compound_pillar(tbl, tbl$a, width = 20)

## ----show_source = TRUE-------------------------------------------------------
#  ctl_new_pillar.tbl <- function (controller, x, width, ..., title = NULL) 
#  {
#      "!!!!DEBUG ctl_new_pillar.tbl(`v(width)`, `v(title)`)"
#      pillar(x, title, if (!is.null(width)) 
#          max0(width))
#  }

## ----show_source = TRUE-------------------------------------------------------
#  pillar <- function (x, title = NULL, width = NULL, ...) 
#  {
#      "!!!!DEBUG pillar(`v(class(x))`, `v(title)`, `v(width)`)"
#      pillar_from_shaft(new_pillar_title(title), new_pillar_type(x), 
#          pillar_shaft(x, ...), width)
#  }

## ----show_source = TRUE-------------------------------------------------------
#  new_pillar <- function (components, ..., width = NULL, class = NULL) 
#  {
#      "!!!!DEBUG new_pillar(`v(width)`, `v(class)`)"
#      check_dots_empty()
#      if (length(components) > 0 && !is_named(components)) {
#          abort("All components must have names.")
#      }
#      structure(components, width = width, class = c(class, "pillar"))
#  }

## ----show_source = TRUE-------------------------------------------------------
#  format.pillar <- function (x, width = NULL, ...) 
#  {
#      if (is.null(width)) {
#          width <- get_width(x)
#      }
#      if (is.null(width)) {
#          widths <- pillar_get_widths(x)
#          width <- sum(widths) - length(widths) + 1
#      }
#      new_vertical(pillar_format_parts_2(x, width))
#  }

