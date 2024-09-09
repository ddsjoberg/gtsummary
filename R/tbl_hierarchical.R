tbl_hierarchical <- function(data,
                             hierarchies,
                             by = NULL,
                             id = NULL,
                             denominator = NULL,
                             include = everything(), # this would be the variables from `hierarchy` that we would include summary stats for (some of the nested drug class tables don't need stats on the class level)
                             statistic = ifelse(!missing(id), "{n} ({p})", "{n}"),
                             digits = NULL,
                             overall_row = FALSE) {
  set_cli_abort_call()

  # process and check inputs ---------------------------------------------------
  browser()
  check_not_missing(data)
  check_data_frame(data)
  check_not_missing(hierarchies)
  check_string(statistic)
  check_scalar_logical(overall_row)

  # evaluate tidyselect
  cards::process_selectors(data, hierarchies = {{ hierarchies }}, id = {{ id }}, by = {{ by }})
  cards::process_selectors(data[hierarchies], include = {{ include }})
  # if id provided, then check that denominator also provided
  if (!is_empty(id)) {
    check_data_frame(
      denominator,
      message = "A {.cls data.frame} must be passed in argument {.arg denominator} when argument {.arg id} is supplied."
    )
  }

  if (is_empty(id) + is_empty(denominator) == 1L) {
    cil::cli_abort(
      "Specify both arguments {.arg id} and {.arg denominator}, or neither.",
      call = get_cli_abort_call()
    )
  }

  # check that neither 'hierarchies' nor 'include' is empty
  if (is_empty(hierarchies) || is_empty(include)) {
    cli::cli_abort(
      message = "Arguments {.arg hierarchies} and {.arg include} cannot be empty.",
      call = get_cli_abort_call()
    )
  }

  # check that 'include' is the correct subset of 'hierarchies'
  if (!setequal(include, hierarchies[seq_len(length(include))])) {
    cli::cli_abort(
      message = c("The columns selected in {.arg include} must be nested within the columns in {.arg hierarchies}",
                  "i" = "For example, when {.code hierarchies = c(SOC, AETERM)}, {.arg include} can be {.code AETERM} but not {.code SOC}.")
    )
  }

  # TODO: Add digits argument processing

  # save arguments
  tbl_hierarchical_inputs <- as.list(environment())

  # calculate statistics -------------------------------------------------------
  # TODO: Update this with cards::ard_stack_hierarchical() when it's ready
  cards <-
    cards::ard_hierarchical(
      data = data,
      variables = all_of(hierarchies),
      by = all_of(by),
      denominator = denominator,
      id = all_of(id)
    )

  # call bridge function here

}


brdg_hierarchical <- function() {

}
