# separate_p_footnotes()

    Code
      as.data.frame(dplyr::mutate(dplyr::filter(getElement(getElement(separate_p_footnotes(add_p(tbl, test = list(age = function(data,
        variable, by, ...) broom::tidy(t.test(data[[variable]] ~ data[[by]]))))), "table_styling"), "footnote_body"), dplyr::row_number() %in%
        c(dplyr::n(), dplyr::n() - 1L)), rows = map_chr(rows, ~ expr_deparse(quo_squash(.x)))))
    Output
         column                                                      rows                   footnote text_interpret replace remove
      1 p.value   .data$variable %in% "age" & .data$row_type %in% "label"    Welch Two Sample t-test         gt::md    TRUE  FALSE
      2 p.value .data$variable %in% "grade" & .data$row_type %in% "label" Pearson's Chi-squared test         gt::md    TRUE  FALSE

---

    Code
      as.data.frame(dplyr::mutate(dplyr::filter(getElement(getElement(separate_p_footnotes(add_difference(tbl)), "table_styling"),
      "footnote_body"), dplyr::row_number() %in% seq(dplyr::n(), dplyr::n() - 4L)), rows = map_chr(rows, ~ expr_deparse(quo_squash(.x)))))
    Output
          column                                                      rows                     footnote text_interpret replace remove
      1 estimate   .data$variable %in% "age" & .data$row_type %in% "label"      Welch Two Sample t-test         gt::md    TRUE  FALSE
      2 conf.low   .data$variable %in% "age" & .data$row_type %in% "label"      Welch Two Sample t-test         gt::md    TRUE  FALSE
      3  p.value   .data$variable %in% "age" & .data$row_type %in% "label"      Welch Two Sample t-test         gt::md    TRUE  FALSE
      4 estimate .data$variable %in% "grade" & .data$row_type %in% "label" Standardized Mean Difference         gt::md    TRUE  FALSE
      5 conf.low .data$variable %in% "grade" & .data$row_type %in% "label" Standardized Mean Difference         gt::md    TRUE  FALSE

# separate_p_footnotes() messaging

    Code
      separate_p_footnotes(add_p(add_difference(tbl_summary(trial, by = trt, include = grade))))
    Condition
      Error in `separate_p_footnotes()`:
      ! One (and only one) of `add_p()` and `add_difference()` needs to be run before `separate_p_footnotes()`.

