fit = lm(age ~ response:trt, trial)
t = broom::tidy(fit)

parse_ugh <- function(fit, tidy) {
  # extracting model frame
  mf <- model.frame(fit)

  # counting number of colons in term names
  n_colon <- tidy$term %>% str_count(pattern = ":")

  # parsing terms and separating interactions (sep by :)
  term_match <-
    tibble(
      term = tidy$term %>%
        str_split_fixed(pattern = ":", n_colon + 1) %>%
        keep(~ . != "") %>%
        discard(~ . == "(Intercept)") %>%
        unique() %>%
        rev(),
      variable = NA_character_
    )


  # cycling over variables and assigning to terms in model
  for(v in (names(mf) %>% rev())){

    # checking character and factor levels
    if(class(mf[[v]]) %in% c("character", "factor")){
      term_match <-
        term_match %>%
        mutate(
          variable = ifelse(
            str_starts(term, v) & term %in% paste0(v, unique(mf[[v]])) & is.na(variable),
            v,
            variable
          )
        )
    }
    # checking numeric variable names
    else {
      term_match <-
        term_match %>%
        mutate(
          variable = ifelse(
            term == v & is.na(variable), v, variable
          )
        )
    }
  }

  print(term_match)

  term_matrix <-
    tidy$term %>%
    str_split_fixed(pattern = ":", n_colon + 1)
  print(term_matrix)
  for(i in 1:nrow(term_match)) {
    term_matrix[term_matrix == term_match$term[i]] <- term_match$variable[i]
  }
  print(term_matrix)

  apply(term_matrix, 1, function(x) str_glue(x, sep = ":", .na = ""))
}

parse_ugh(fit, t)


t$term %>% str_split_fixed(pattern = ":", 2) %>% keep(~ . != "") %>% unique()

t$term %>% str_starts()


fit = lm(age ~ response:factor(trt), trial)

t = gtsummary:::tidy_wrap(
    fit,
    FALSE,
    0.95
  )


gtsummary:::parse_terms(
  fit, t, FALSE
)

tbl_regression(fit)
