library(tidyverse)
fit = lm(age ~ grade:trt*response, trial)
t = broom::tidy(fit, conf.int = TRUE)

parse_ugh <- function(fit, tidy) {
  # extracting model frame
  mf <- model.frame(fit)

  # all terms in model ---------------------------------------------------------
  # this code looks over the model terms, and extracts a list of each term
  # for interaction terms, the terms are reported separately
  # for example, trtDrug:age will result in trt:Drug and age.
  # counting number of colons in term names
  n_colon <- tidy$term %>% str_count(pattern = ":")

  # putting unique terms in tibble (to later be matched to the parent variable)
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

  # match term to variable -----------------------------------------------------
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

  # creating labels ------------------------------------------------------------
  # this is one line per term, where a sinlge interaction term gets more than one row
  tidy_long <-
    tidy %>%
    mutate(
      # making a table with info about variables and levels
      term_id = 1:n(),
      term_split = map(
        term,
        ~str_split_fixed(
          .x,
          pattern = ":",
          str_count(.x, pattern = ":") + 1
        ) %>%
          as.character()
      )
    ) %>%
    unnest(term_split) %>%
    mutate(
      variable = map_chr(
        term_split,
        ~ term_match %>%
          filter(term == .x) %>%
          pull(variable) %>%
          {ifelse(.x == "(Intercept)", NA, .)}
      ),
      # variable labels
      variable_lbl = map_chr(
        variable,
        ~ attr(mf[[.x]], "label") %||% .x
      ),
      variable_lbl = ifelse(is.na(variable_lbl) & term == "(Intercept)",
                            "(Intercept)",
                            variable_lbl),
      # indicating whether each variable is categorical or continuous
      variable_type = map_chr(
        variable,
        ~ case_when(
          class(mf[[.x]]) %in% c("character", "factor") ~ "categorical",
          TRUE ~ "continuous"
        )
      ),
      # parsing the term to extract the levels to report
      # if variable is categorical, remove the variable name from term and report level,
      # if variable is not categorical, replace the varname with the label
      level = pmap_chr(
        list(term_split, variable, variable_lbl, variable_type),
        function(term_split, variable, variable_lbl, variable_type) {
          if (variable_type == "continuous") return(variable_lbl)
          str_remove(term_split, pattern = fixed(variable))
        }
      )
    )


  # one line per term in the model
  tidy_term <-
    tidy_long %>%
    group_by(term_id, term) %>%
    mutate(
      interaction = n() > 1,
      group = variable %>% paste(collapse = ":"),
      group = ifelse(term == "(Intercept)" & is.na(variable), "(Intercept)", group),
      group_lbl = variable_lbl %>% paste(collapse = " * "),
      level_lbl = level %>% paste(collapse = " * "),
      type = ifelse(interaction, "interaction", variable_type),
    ) %>%
    select(-term_split, -variable, -variable_lbl, -variable_type, -level) %>%
    ungroup() %>%
    distinct()


  # grouped by variable (categorical variables on one line)
  tidy_group <-
    tidy_term %>%
    group_by(group, group_lbl, type) %>%
    nest() %>%
    mutate(
      # assess how many line to display results one (one or more than one)
      single_row = pmap_lgl(
        list(type, group_lbl, data),
        function(type, group_lbl, data) {
          if(type == "continuous") return(TRUE)
          if(type == "categorical") return(FALSE)
          # display on single line of it a numeric-numeric interaction
          if(type == "interaction") {
            if(nrow(data) > 1) return(FALSE)
            else if(group_lbl == data$level_lbl) return(TRUE)
            else return(FALSE)
          }
        }
      )
    )

  result =
    tidy_group %>%
    mutate(
      table = pmap(
        list(group, group_lbl, single_row, type, data),
        function(group, group_lbl, single_row, type, data) {
          if(single_row == TRUE) {
            result = data %>%
              mutate(
                variable = group,
                row_type = "label",
                label = group_lbl
              )
          }
          else if(single_row == FALSE){
            # for interaction, do not add reference rows (just a header)
            if(type == "interaction") {
              result = data %>%
                mutate(
                  variable = group,
                  row_type = "level",
                  label = level_lbl
                ) %>%
                {bind_rows(
                  tibble(
                    variable = group,
                    row_type = "label",
                    label = group_lbl
                  ), . # levels go below the header
                )}
            }
            if(type == "categorical") {
              result <-
                tibble(
                  level_lbl = mf[[group]] %>% unique() %>% sort() %>% as.character()
                ) %>%
                left_join(
                  data,
                  by = "level_lbl"
                ) %>%
                mutate(
                  variable = group,
                  row_type = "level",
                  label = level_lbl
                ) %>%
                {bind_rows(
                  tibble(
                    variable = group,
                    row_type = "label",
                    label = group_lbl
                  ),
                  . # levels go below the header
                )}
            }
          }
          result %>%
            rename(
              coef = estimate,
              ll = conf.low,
              ul = conf.high,
              pvalue = p.value
            ) %>%
            select(variable, row_type, label, coef, ll, ul, pvalue)
        }
      )
    )

  map_dfr(result$table, ~.x)
}

r = parse_ugh(fit, t)
r


fit = lm(age ~ marker + grade:trt*response, trial)
t = broom::tidy(fit, conf.int = TRUE)
parse_ugh(fit, t)
