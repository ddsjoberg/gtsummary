skip_on_cran()
skip_if_not(is_pkg_installed("flextable"))

my_tbl_summary <- trial |>
  select(trt, age, death) |>
  tbl_summary()
my_tbl_regression <- lm(marker ~ age, trial) |> tbl_regression()
my_spanning_tbl <- trial |>
  tbl_summary(by = grade, include = age) |>
  modify_spanning_header(c(stat_1, stat_3) ~ "**Testing**")

ft_tbl_summary <- my_tbl_summary |> as_flex_table()
ft_tbl_regression <- my_tbl_regression |> as_flex_table()
ft_spanning_tbl <- my_spanning_tbl |> as_flex_table()

test_that("as_flex_table works with standard use", {
  # return_calls argument does not produce warnings
  expect_silent(my_tbl_summary |> as_flex_table(return_calls = TRUE))

  # include argument does not produce warnings
  expect_silent(my_tbl_summary |> as_flex_table(include = tibble))

  # correct elements are returned
  expect_equal(
    names(ft_tbl_summary),
    c("header", "body", "footer", "col_keys", "caption", "blanks", "properties")
  )
})

test_that("as_flex_table works with tbl_survfit", {
  skip_if_not(is_pkg_installed("survival"))
  fit1 <- survival::survfit(survival::Surv(ttdeath, death) ~ trt, trial)
  tbl <- tbl_survfit(fit1, times = c(12, 24), label_header = "{time} Months")

  expect_silent(ft_tbl <- tbl |> as_flex_table())

  # check for correct table contents
  expect_equal(
    tbl |> as_tibble(col_labels = FALSE),
    ft_tbl$body$dataset |> as_tibble(),
    ignore_attr = "names"
  )
})

test_that("as_flex_table works with tbl_merge", {
  skip_if_not(is_pkg_installed("survival"))

  t1 <- glm(response ~ trt + grade + age, trial, family = binomial) |>
    tbl_regression(exponentiate = TRUE)
  t2 <- survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) |>
    tbl_regression(exponentiate = TRUE)

  tbl <- tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )
  ft_tbl <- tbl |> as_flex_table()

  # header labels span correct columns
  expect_equal(
    ft_tbl$header$spans$rows[1, ],
    c(1, 3, 0, 0, 3, 0, 0)
  )
  expect_equal(
    ft_tbl$header$spans$rows[2, ],
    rep(1, 7)
  )

  # spanning header labels are correct
  expect_equal(
    ft_tbl$header$dataset[1, ] |> unlist(use.names = FALSE),
    c(" ", rep("**Tumor Response**", 3), rep("**Time to Death**", 3))
  )
})

test_that("as_flex_table passes table contents correctly", {
  # tbl_summary
  vis_cols <- my_tbl_summary$table_styling$header |>
    dplyr::filter(!hide) |>
    dplyr::pull(column)

  expect_equal(
    my_tbl_summary$table_body[vis_cols],
    ft_tbl_summary$body$dataset |> as_tibble()
  )

  # tbl_cross
  my_tbl_cross <- tbl_cross(trial, trt, grade)
  expect_silent(ft_tbl_cross <- my_tbl_cross |> as_flex_table())

  expect_equal(
    my_tbl_cross |> as_tibble(col_labels = FALSE),
    ft_tbl_cross$body$dataset |> as_tibble()
  )

  # tbl_regression
  expect_equal(
    my_tbl_regression |> as_tibble(col_labels = FALSE),
    ft_tbl_regression$body$dataset |> as_tibble(),
    ignore_attr = c("class", "names")
  )

  # tbl_uvregression
  my_tbl_uvregression <- trial |> tbl_uvregression(method = lm, y = age)
  expect_silent(ft_tbl_uvregression <- my_tbl_uvregression |> as_flex_table())

  expect_equal(
    my_tbl_uvregression |> as_tibble(col_labels = FALSE),
    ft_tbl_uvregression$body$dataset |> as_tibble(),
    ignore_attr = c("class", "names")
  )
})

test_that("as_flex_table works with bold/italics", {
  tbl <- my_tbl_summary |>
    bold_labels() |>
    italicize_levels()
  ft_tbl <- tbl |> as_flex_table()

  # labels correctly bolded
  expect_equal(
    eval_tidy(tbl$table_styling$text_format$rows[[1]], data = tbl$table_body),
    ft_tbl$body$styles$text$bold$data[, 1]
  )

  # labels correctly italicized
  expect_equal(
    eval_tidy(tbl$table_styling$text_format$rows[[2]], data = tbl$table_body),
    ft_tbl$body$styles$text$italic$data[, 1]
  )

  # markdown syntax for bold/italics in headers & spanning headers works
  tbl <- my_spanning_tbl |>
    modify_spanning_header(
      stat_1 ~ "_Test_ **1**",
      stat_3 ~ "_Test_ **2**"
    ) |>
    modify_header(
      all_stat_cols() ~ "**Level**: _{level}_"
    )
  ft_tbl <- tbl |> as_flex_table()

  # formatting kept in header$dataset
  expect_equal(
    ft_tbl$header$dataset[1, ] |> unlist(use.names = FALSE),
    c(" ", "_Test_ **1**", " ", "_Test_ **2**")
  )
  # formatting removed in header$content$data & applied correctly to text (spanning header)
  expect_equal(
    ft_tbl$header$content$data[1, ]$stat_1[c("txt", "italic", "bold")],
    data.frame(txt = c("Test", " ", "1"), italic = c(TRUE, NA, NA), bold = c(NA, NA, TRUE))
  )
  # formatting removed in header$content$data & applied correctly to text (non-spanning header)
  expect_equal(
    ft_tbl$header$content$data[2, ]$stat_2[c("txt", "italic", "bold")],
    data.frame(txt = c("Level", ": ", "II", "1"), italic = c(NA, NA, TRUE, NA), bold = c(TRUE, NA, NA, NA))
  )
})

test_that("as_flex_table passes table header labels correctly", {
  # tbl_summary
  vis_cols <- which(!my_tbl_summary$table_styling$header$hide)
  expect_equal(
    my_tbl_summary$table_styling$header$column[vis_cols],
    ft_tbl_summary$header$col_keys
  )
  expect_equal(
    gsub("[**|_]", "", my_tbl_summary$table_styling$header$label[vis_cols]),
    apply(ft_tbl_summary$header$content$data, 2, \(x) x[[1]]$txt[1]) |> unname()
  )

  # spanning header placed correctly
  vis_cols <- which(!my_spanning_tbl$table_styling$header$hide)
  expect_equal(
    my_spanning_tbl$table_styling$spanning_header |>
      dplyr::filter(!is.na(spanning_header)) |>
      dplyr::pull(column),
    which(nchar(apply(ft_spanning_tbl$header$content$data, c(1, 2), \(x) x[[1]]$txt[1])[1, ]) > 1) |> names()
  )

  # checking the placement of a second spanning header
  expect_silent(
    tbl2 <-
      my_spanning_tbl |>
      modify_spanning_header(all_stat_cols() ~ "**Tumor Grade**", level = 2) |>
      as_flex_table()
  )

  expect_equal(
    tbl2$header$dataset,
    data.frame(
      stringsAsFactors = FALSE,
      label = c(" ", " ", "label"),
      stat_1 = c("**Tumor Grade**", "**Testing**", "stat_1"),
      stat_2 = c("**Tumor Grade**", " ", "stat_2"),
      stat_3 = c("**Tumor Grade**", "**Testing**", "stat_3")
    )
  )
})

test_that("as_flex_table passes table column visibility correctly", {
  expect_equal(
    my_tbl_regression$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(column),
    ft_tbl_regression$body$col_keys
  )

  # customize visibility
  tbl <- my_tbl_regression |>
    modify_table_styling(columns = "estimate", hide = TRUE) |>
    modify_table_styling(columns = "N_obs", hide = FALSE)
  ft_tbl <- tbl |> as_flex_table()

  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(column),
    ft_tbl$body$col_keys
  )
})

test_that("as_flex_table passes table column alignment correctly", {
  expect_equal(
    my_tbl_regression$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(align),
    ft_tbl_regression$body$styles$pars$text.align$data |> c()
  )

  # customize alignment
  tbl <- my_tbl_regression |>
    modify_table_styling(columns = "estimate", align = "right")
  ft_tbl <- tbl |> as_flex_table()

  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(!hide) |>
      dplyr::pull(align),
    ft_tbl$body$styles$pars$text.align$data |> c()
  )
})

test_that("as_flex_table passes table footnotes & abbreviations correctly", {
  tbl_fn <- my_tbl_summary |>
    modify_footnote_body(columns = label, footnote = "test footnote", rows = variable == "age")
  ft_tbl_fn <- tbl_fn |> as_flex_table()

  # footnote
  fn1 <- ft_tbl_fn$footer$content$data[1, ]$label$txt
  fn2 <- ft_tbl_fn$footer$content$data[2, ]$label$txt

  expect_equal(nrow(ft_tbl_fn$footer$content$data), 2) # correct number of footnotes
  expect_equal(c(fn1[1], fn2[1]), c("1", "2")) # correct ordering
  expect_equal(
    tbl_fn$table_styling$footnote_header$footnote, # correct labels
    fn1[2]
  )
  expect_equal(
    tbl_fn$table_styling$footnote_body$footnote, # correct labels
    fn2[2]
  )

  tbl_fa <- tbl_fn |>
    modify_abbreviation("N = number of observations")
  ft_tbl_fa <- tbl_fa |> as_flex_table()

  # footnote_abbrev
  fn1 <- ft_tbl_fa$footer$content$data[1, ]$label$txt
  fn2 <- ft_tbl_fa$footer$content$data[2, ]$label$txt
  fn3 <- ft_tbl_fa$footer$content$data[3, ]$label$txt

  expect_equal(nrow(ft_tbl_fa$footer$content$data), 3) # correct number of footnotes
  expect_equal(c(fn1[1], fn2[1], fn3[1]), c("1", "2", "Abbreviation: N = number of observations")) # correct ordering and label for abbreviation is correct

  # customized footnotes
  tbl <- my_tbl_summary |>
    modify_footnote_header("replace old footnote", columns = all_stat_cols()) |>
    modify_footnote_header("another new footnote", columns = label)
  ft_tbl <- tbl |> as_flex_table()

  fn1 <- ft_tbl$footer$content$data[1, ]$label$txt
  fn2 <- ft_tbl$footer$content$data[2, ]$label$txt

  expect_equal(nrow(ft_tbl$footer$content$data), 2) # correct number of footnotes
  expect_equal(c(fn1[1], fn2[1]), c("1", "2")) # correct ordering
  expect_equal(
    c(fn1[2], fn2[2]), # correct labels
    c("another new footnote", "replace old footnote")
  )

  # footnotes in spanning headers
  expect_equal(
    my_spanning_tbl |>
      modify_footnote_spanning_header(
        footnote = "spanning footnote",
        columns = stat_1
      ) |>
      as_flex_table() |>
      getElement("footer") |>
      getElement("content") |>
      getElement("data") %>%
      `[`(1,) |>
      getElement("label") |>
      getElement("txt"),
    c("1", "spanning footnote")
  )
  expect_equal(
    my_spanning_tbl |>
      modify_spanning_header(stat_1 = "**2 levels**", level = 2L) |>
      modify_footnote_spanning_header(
        footnote = "spanning footnote",
        columns = stat_1
      ) |>
      modify_footnote_spanning_header(
      footnote = "spanning footnote 2",
      columns = stat_1,
      level = 2
    ) |>
    as_flex_table() |>
    getElement("footer") |>
    getElement("content") |>
    getElement("data") %>%
    `[`(1,) |>
    getElement("label") |>
    getElement("txt"),
    c("1", "spanning footnote 2")
  )
})

test_that("as_flex_table passes multiple table footnotes correctly", {
  # testing one footnote passed to multiple columns and rows, addresses issue #2062
  out <- my_tbl_summary |>
    remove_footnote_header(stat_0) |>
    modify_footnote_body(
      columns = c(label, stat_0),
      rows = (variable %in% "trt") & (row_type == "level"),
      footnote = "my footnote"
    ) |>
    as_flex_table()

  dchunk <- flextable::information_data_chunk(out)

  # Checking footer's footnotes
  cell_1 <- dchunk |> dplyr::filter(.part %in% "footer")

  expect_equal(cell_1$txt, c("1", "my footnote", ""))

  # Checking table notation (it is .chunk_index 2 after the normal txt)
  notation_ind <- which(dchunk$.chunk_index > 1)
  cell_notations <- dchunk[sort(c(notation_ind - 1, notation_ind)), ] |>
    dplyr::filter(.part %in% "body") |>
    dplyr::select(.row_id, .col_id, .chunk_index, txt) |>
    dplyr::group_by(.row_id, .col_id) |>
    dplyr::group_split()

  expect_true(all(sapply(cell_notations, function(x) x$txt[nrow(x)]) == "1"))

  trial_reduced <- trial |>
    dplyr::select(grade, trt) |>
    dplyr::filter(trt == "Drug A") |>
    dplyr::filter(grade == "I") |>
    dplyr::mutate(grade = factor(grade, levels = c("I")))

  out <- trial_reduced |>
    tbl_summary(
      by = trt,
      include = grade
    ) |>
    modify_footnote_body(
      columns = stat_1,
      rows = (variable %in% "grade") & (row_type == "level"),
      footnote = "my footnote"
    ) |>
    modify_footnote_body(
      columns = label,
      rows = label == "grade",
      footnote = "my footnote"
    ) |>
    modify_footnote_body(
      columns = label,
      rows = label == "I",
      footnote = "my footnote"
    ) |>
    as_flex_table()

  dchunk <- flextable::information_data_chunk(out)
  cell_1 <- dchunk |> dplyr::filter(.part %in% "footer")

  expect_equal(cell_1$txt, c(
    "1", "n (%)", "",
    "2", "my footnote", ""
  ))

  # Checking table notation (it is .chunk_index 2 after the normal txt)
  notation_ind <- which(dchunk$.chunk_index > 1)
  cell_notations <- dchunk[sort(c(notation_ind - 1, notation_ind)), ] |>
    dplyr::filter(.part %in% c("body", "header")) |>
    dplyr::select(.row_id, .col_id, .chunk_index, txt) |>
    dplyr::group_by(.row_id, .col_id) |>
    dplyr::group_split()

  expect_equal(
    sapply(cell_notations, function(x) x$txt[nrow(x)]),
    c("2", "1", "2", "2")
  )
})

test_that("as_flex_table passes table indentation correctly", {
  expect_equal(
    ft_tbl_summary$body$styles$pars$padding.left$data[, 1],
    c(5, 15, 15, 5, 15, 5)
  )

  # indentation removed
  tbl <- my_tbl_summary |>
    modify_column_indent(columns = label, indent = 0)
  ft_tbl <- tbl |> as_flex_table()

  expect_equal(
    ft_tbl$body$styles$pars$padding.left$data[, 1],
    rep(5, 6)
  )
})

test_that("as_flex_table passes appended glance statistics correctly", {
  tbl <- my_tbl_regression |>
    add_glance_source_note(c("r.squared", "BIC")) |>
    add_glance_table(c("r.squared", "BIC"))
  ft_tbl <- tbl |> as_flex_table()

  expect_equal(
    tbl |> as_tibble(col_labels = FALSE),
    ft_tbl$body$dataset |> as_tibble(),
    ignore_attr = c("class", "names")
  )
  expect_equal(
    tbl$table_styling$source_note$source_note,
    ft_tbl$footer$content$data[2, ]$label$txt
  )
  expect_equal(length(ft_tbl$body$hrule), 3)
})

test_that("as_flex_table passes captions correctly", {
  tbl <- my_tbl_regression |>
    modify_caption("My table caption")
  ft_tbl <- tbl |> as_flex_table()

  expect_equal(
    tbl$table_styling$caption,
    ft_tbl$caption$value
  )
})

test_that("as_flex_table passes missing symbols correctly", {
  tbl <- my_tbl_summary |>
    modify_table_body(~ .x |> mutate(stat_0 = NA_character_))
  ft_tbl <- tbl |> as_flex_table()

  # no substitution for missing values
  expect_equal(
    sapply(ft_tbl$body$content$data[, 2], \(x) x$txt),
    rep("", 6)
  )

  # specify missing symbol
  tbl <- tbl |>
    modify_missing_symbol(stat_0, rows = !is.na(label), symbol = "n / a")
  ft_tbl <- tbl |> as_flex_table()

  # correct substitution for missing values
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE, fmt_missing = TRUE) |>
      dplyr::pull(stat_0),
    sapply(ft_tbl$body$content$data[, 2], \(x) x$txt)
  )
})

test_that("as_flex_table applies formatting functions correctly", {
  tbl <- glm(response ~ age + grade, trial, family = binomial(link = "logit")) |>
    tbl_regression(exponentiate = TRUE) |>
    modify_fmt_fun(
      p.value ~ function(x) style_pvalue(x, digits = 3),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      estimate ~ function(x) style_ratio(x, digits = 4, decimal.mark = ",")
    )
  ft_tbl <- tbl |> as_flex_table()

  # formatted cells
  expect_equal(
    ft_tbl$body$dataset$p.value,
    c("0.10", NA, NA, "0.688", "0.972")
  )

  # formatted column
  expect_equal(
    ft_tbl$body$dataset$estimate,
    c("1,0191", NA, NA, "0,8535", "1,0136")
  )

  tbl2 <- tbl_uvregression(
    trial |> dplyr::select(response, age, grade),
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |>
    modify_fmt_fun(
      stat_n ~ function(x) style_number(x, digits = 2),
      rows = variable == "age"
    ) |>
    modify_fmt_fun(
      stat_n ~ label_style_number(digits = 4),
      rows = variable == "grade"
    ) |>
    modify_fmt_fun(
      c(conf.low, conf.high) ~ label_style_sigfig(digits = 3)
    )
  ft_tbl2 <- tbl2 |> as_flex_table()

  # formatted cell
  expect_equal(
    ft_tbl2$body$dataset$stat_n,
    c("183.00", "193.0000", NA, NA, NA)
  )

  # formatted column
  expect_equal(
    ft_tbl2$body$dataset$conf.low,
    c("0.997, 1.04", NA, NA, "0.446, 2.00", "0.524, 2.29")
  )
})

test_that("as_flex_table passes column merging correctly", {
  tbl <- my_tbl_regression |>
    modify_column_merge(
      pattern = "{estimate} (pval {p.value})",
      rows = !is.na(estimate)
    )
  ft_tbl <- tbl |> as_flex_table()

  # conf.low (default column merging)
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(conf.low),
    ft_tbl$body$dataset$conf.low
  )

  # estimate (added custom column merging)
  expect_equal(
    tbl |>
      as_tibble(col_labels = FALSE) |>
      dplyr::pull(estimate),
    ft_tbl$body$dataset$estimate
  )
})
