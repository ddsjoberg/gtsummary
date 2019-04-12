
# this function takea a model object and a tidied version (fomr the broom package)
# and returns a parsed table of all results with labels and reference rows included
parse_fit <- function(fit, tidy, label) {
  # extracting model frame
  model_frame <- model.frame(fit)

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
  for(v in (names(model_frame) %>% rev())){

    # checking character and factor levels
    if(class(model_frame[[v]]) %in% c("character", "factor")){
      term_match <-
        term_match %>%
        mutate(
          variable = ifelse(
            str_starts(term, v) & term %in% paste0(v, unique(model_frame[[v]])) & is.na(variable),
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

  # tidy_long ------------------------------------------------------------------
  # this is one line per term, AND interaction terms have one row per variable in the interaction
  tidy_long <-
    tidy %>%
    mutate(
      # making a table with info about variables and levels
      term_id = 1:n(),
      # term_split finds all the variables invovled in interaction terms
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
      # matching the variable name to each term in the model
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
        ~ label[[.x]] %||% attr(model_frame[[.x]], "label") %||% .x
      ),
      variable_lbl = ifelse(is.na(variable_lbl) & term == "(Intercept)",
                            "(Intercept)",
                            variable_lbl),
      # indicating whether each variable is categorical or continuous
      variable_type = map_chr(
        variable,
        ~ case_when(
          class(model_frame[[.x]]) %in% c("character", "factor") ~ "categorical",
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


  # tidy_term ------------------------------------------------------------------
  # one line per term in the model
  tidy_term <-
    tidy_long %>%
    group_by(term_id, term) %>%
    mutate(
      # indicating whether obs is an interaction term or not
      interaction = n() > 1,
      # groups are terms that belong to the same variable (or interaction set)
      group = variable %>% paste(collapse = ":"),
      group = ifelse(term == "(Intercept)" & is.na(variable), "(Intercept)", group),
      # the collpase only comes into play when there are interactions present
      group_lbl = variable_lbl %>% paste(collapse = " * "),
      level_lbl = level %>% paste(collapse = " * "),
      # types are continuous, categorical, and interaction
      type = ifelse(interaction, "interaction", variable_type),
    ) %>%
    select(-term_split, -variable, -variable_lbl, -variable_type, -level) %>%
    ungroup() %>%
    distinct()


  # tidy_group -----------------------------------------------------------------
  # groups are terms that belong to the same variable (or interaction set)
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

  # final touches to result ----------------------------------------------------
  # adding in refernce rows, and header rows for categorical and interaction variables
  result =
    tidy_group %>%
    mutate(
      table = pmap(
        list(group, group_lbl, single_row, type, data),
        ~parse_final_touches(group = ..1,
                             group_lbl = ..2,
                             single_row = ..3,
                             type = ..4,
                             data = ..5,
                             model_frame = model_frame)
      )
    )

  # returning final formatted tibble of results
  map_dfr(result$table, ~.x)
}

# adding in refernce rows, and header rows for categorical and interaction variables
parse_final_touches <- function(group, group_lbl, single_row, type, data, model_frame) {
  # this is for continuous variables, and numeric on numeric interactions
  if(single_row == TRUE) {
    result = data %>%
      mutate(
        variable = group,
        row_type = "label",
        label = group_lbl
      )
  }
  # for interaction, do not add reference rows (just a header)
  else if(type == "interaction") {
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
  # adding reference rows AND header row for categorical variables
  else if(type == "categorical") {
    result <-
      tibble(
        level_lbl = model_frame[[group]] %>% unique() %>% sort() %>% as.character()
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

  # keeping necessary vars and renaming
  result %>%
    rename(
      coef = estimate,
      ll = conf.low,
      ul = conf.high,
      pvalue = p.value
    ) %>%
    select(variable, row_type, label, coef, ll, ul, pvalue)
}

fit = lm(age ~ trt*grade, trial)
t = broom::tidy(fit, conf.int = TRUE)
parse_fit(fit, t, label = list(trt = "YAY TX"))
