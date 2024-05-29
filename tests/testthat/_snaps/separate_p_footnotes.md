# separate_p_footnotes()

    Code
      as.data.frame(dplyr::mutate(dplyr::filter(getElement(getElement(
        separate_p_footnotes(add_p(tbl, test = list(age = function(data, variable, by,
          ...) broom::tidy(t.test(data[[variable]] ~ data[[by]]))))), "table_styling"),
      "footnote"), dplyr::row_number() %in% c(dplyr::n(), dplyr::n() - 1L)), rows = map_chr(
        rows, ~ expr_deparse(quo_squash(.x)))))
    Output
         column                                                      rows
      1 p.value   .data$variable %in% "age" & .data$row_type %in% "label"
      2 p.value .data$variable %in% "grade" & .data$row_type %in% "label"
        text_interpret                   footnote
      1         gt::md    Welch Two Sample t-test
      2         gt::md Pearson's Chi-squared test

---

    Code
      as.data.frame(dplyr::mutate(dplyr::filter(getElement(getElement(
        separate_p_footnotes(add_difference(tbl)), "table_styling"), "footnote"),
      dplyr::row_number() %in% seq(dplyr::n(), dplyr::n() - 4L)), rows = map_chr(rows,
        ~ expr_deparse(quo_squash(.x)))))
    Output
          column                                                      rows
      1 estimate   .data$variable %in% "age" & .data$row_type %in% "label"
      2 conf.low   .data$variable %in% "age" & .data$row_type %in% "label"
      3  p.value   .data$variable %in% "age" & .data$row_type %in% "label"
      4 estimate .data$variable %in% "grade" & .data$row_type %in% "label"
      5 conf.low .data$variable %in% "grade" & .data$row_type %in% "label"
        text_interpret                     footnote
      1         gt::md      Welch Two Sample t-test
      2         gt::md      Welch Two Sample t-test
      3         gt::md      Welch Two Sample t-test
      4         gt::md Standardized Mean Difference
      5         gt::md Standardized Mean Difference

# separate_p_footnotes() messaging

    Code
      separate_p_footnotes(add_p(add_difference(tbl_summary(trial, by = trt, include = grade))))
    Condition
      Error in `separate_p_footnotes()`:
      ! One (and only one) of `add_p()` and `add_difference()` needs to be run before `separate_p_footnotes()`.

