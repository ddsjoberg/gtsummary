#' Stratified gtsummary tables
#'
#' @description
#' Build a stratified gtsummary table. Any gtsummary table that accepts
#' a data frame as its first argument can be stratified.
#' - In `tbl_strata()`, the stratified or subset data frame is passed to the
#'   function in `.tbl_fun=`, e.g. `purrr::map(data, .tbl_fun)`.
#' - In `tbl_strata2()`, both the stratified data frame and the strata level
#'   are passed to `.tbl_fun=`, e.g. `purrr::map2(data, strata, .tbl_fun)`.
#'
#' Tables are created _independently_ within each stratum.
#' When merging, keep in mind that merging works best with **like tables**.
#' See [`tbl_merge()`] for details.
#'
#' @param data (`data.frame`, `survey.design`)\cr
#'   a data frame or survey object
#' @param .tbl_fun (`function`)
#'   A function or formula. If a _function_, it is used as is.
#'   If a formula, e.g. `~ .x %>% tbl_summary() %>% add_p()`, it is converted to a function.
#'   The stratified data frame is passed to this function.
#' @param ... Additional arguments passed on to the `.tbl_fun` function.
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   character vector or tidy-selector of columns in data to stratify results by.
#'   Only _observed_ combinations are shown in results.
#' @param .sep (`string`)\cr
#'   when more than one stratifying variable is passed, this string is
#'   used to separate the levels in the spanning header. Default is `", "`
#' @param .combine_with (`string`)\cr
#'   One of `c("tbl_merge", "tbl_stack")`. Names the function
#'   used to combine the stratified tables.
#' @param .combine_args (named `list`)\cr
#'   named list of arguments that are passed to function specified in `.combine_with`
#' @param .header (`string`)\cr
#'   String indicating the headers that will be placed.
#'   Default is `"**{strata}**"` when `.combine_with = "tbl_merge"` and
#'   `"{strata}"` when `.combine_with = "tbl_stack"`. Items placed in
#'   curly brackets will be evaluated according to `glue::glue()` syntax.
#'
#'     - `strata` stratum levels
#'
#'     - `n` N within stratum
#'
#'     - `N` Overall N
#'
#'   The evaluated value of `.header` is also available within `tbl_strata2(.tbl_fun)`
#' @param .quiet `r lifecycle::badge("deprecated")`
#'
#' @section Tips:
#'
#' * `tbl_summary()`
#'
#'     * The number of digits continuous variables are rounded to is determined
#'     separately within each stratum of the data frame. Set the `digits=`
#'     argument to ensure continuous variables are rounded to the same number
#'     of decimal places.
#'
#'     * If some levels of a categorical variable are unobserved within a
#'     stratum, convert the variable to a factor to ensure all levels appear in
#'     each stratum's summary table.
#'
#'     * The summary type for variables (e.g. continuous vs categorical vs dichotomous)
#'     are determined separately within stratum. Use the `tbl_summary(type)`
#'     argument to assign a summary type consistent across all tables being combined.
#'
#'     * By default, a "missing" row appears when there are missing values only.
#'     Use the `tbl_summary(missing)` argument to ensure there is always/never
#'     a missing row for the combining of the tables.
#'
#' @author Daniel D. Sjoberg
#' @name tbl_strata
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("broom")
#' # Example 1 ----------------------------------
#' trial |>
#'   select(age, grade, stage, trt) |>
#'   mutate(grade = paste("Grade", grade)) |>
#'   tbl_strata(
#'     strata = grade,
#'     .tbl_fun =
#'       ~ .x |>
#'         tbl_summary(by = trt, missing = "no") |>
#'         add_n(),
#'     .header = "**{strata}**, N = {n}"
#'   )
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   select(grade, response) |>
#'   mutate(grade = paste("Grade", grade)) |>
#'   tbl_strata2(
#'     strata = grade,
#'     .tbl_fun =
#'       ~ .x %>%
#'         tbl_summary(
#'           label = list(response = .y),
#'           missing = "no",
#'           statistic = response ~ "{p}%"
#'         ) |>
#'         add_ci(pattern = "{stat} ({ci})") |>
#'         modify_header(stat_0 = "**Rate (95% CI)**") |>
#'         remove_footnote_header(stat_0),
#'     .combine_with = "tbl_stack",
#'     .combine_args = list(group_header = NULL)
#'   ) |>
#'   modify_caption("**Response Rate by Grade**")
NULL

#' @export
#' @rdname tbl_strata
tbl_strata <- function(data,
                       strata,
                       .tbl_fun,
                       ...,
                       .sep = ", ",
                       .combine_with = c("tbl_merge", "tbl_stack"),
                       .combine_args = NULL,
                       .header =
                         ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}"),
                       .quiet = NULL) {
  set_cli_abort_call()

  # deprecations ---------------------------------------------------------------
  if (!missing(.quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::tbl_strata(.quiet)",
      details = "Argument has been ignored."
    )
  }

  # process inputs -------------------------------------------------------------
  check_class(data, c("data.frame", "survey.design"))
  .combine_with <- match.arg(.combine_with)

  # run `tbl_strata()``
  tbl_strata_internal(
    data = data,
    strata = {{ strata }},
    .tbl_fun = .tbl_fun,
    ...,
    .sep = .sep,
    .combine_with = .combine_with,
    .combine_args = .combine_args,
    .header = .header,
    .parent_fun = "tbl_strata"
  )
}

#' @export
#' @rdname tbl_strata
tbl_strata2 <- function(data,
                        strata,
                        .tbl_fun,
                        ...,
                        .sep = ", ",
                        .combine_with = c("tbl_merge", "tbl_stack"),
                        .combine_args = NULL,
                        .header =
                          ifelse(.combine_with == "tbl_merge", "**{strata}**", "{strata}"),
                        .quiet = TRUE) {
  set_cli_abort_call()

  # deprecations ---------------------------------------------------------------
  if (!missing(.quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::tbl_strata(.quiet)",
      details = "Argument has been ignored."
    )
  }

  # process inputs -------------------------------------------------------------
  check_class(data, c("data.frame", "survey.design"))
  .combine_with <- match.arg(.combine_with)

  # run `tbl_strata()``
  tbl_strata_internal(
    data = data,
    strata = {{ strata }},
    .tbl_fun = .tbl_fun,
    ...,
    .sep = .sep,
    .combine_with = .combine_with,
    .combine_args = .combine_args,
    .header = .header,
    .parent_fun = "tbl_strata2"
  )
}

tbl_strata_internal <- function(data,
                                strata,
                                .tbl_fun,
                                ...,
                                .sep = ", ",
                                .combine_with = c("tbl_merge", "tbl_stack"),
                                .combine_args = NULL,
                                .header = NULL,
                                .parent_fun) {
  check_string(.header)


  # selecting stratum ----------------------------------------------------------
  cards::process_selectors(
    data = as.data.frame(data),
    strata = {{ strata }}
  )

  new_strata_names <-
    as.list(strata) %>%
    set_names(paste0("strata_", seq_len(length(strata))))

  # calculating df_by ----------------------------------------------------------
  data_for_strata <- data
  if (!is_survey(data_for_strata)) {
    # base R paste instead of dplyr::mutate for strata column
    strata_pasted <- do.call(paste, c(as.data.frame(data_for_strata)[strata], list(sep = .sep)))
    data_tmp <- as.data.frame(data_for_strata)
    data_tmp[["strata"]] <- strata_pasted
    df_by <- df_by(data_tmp, by = "strata")
  } else {
    data_for_strata$variables <-
      data_for_strata$variables %>%
      dplyr::mutate(strata = paste(!!!syms(strata), sep = .sep))
    df_by <-
      data_for_strata %>%
      df_by(by = "strata")
  }
  # base R column select + rename instead of dplyr::select + dplyr::mutate
  keep_cols <- intersect(
    c("n", "N", "p", "n_unweighted", "N_unweighted", "p_unweighted"),
    names(df_by)
  )
  df_by <- df_by[, c("by", keep_cols), drop = FALSE]
  names(df_by)[names(df_by) == "by"] <- "strata"
  df_by$header <- as.character(glue::glue_data(
    setNames(as.list(df_by), names(df_by)), .header))

  # nesting data and building tbl objects --------------------------------------
  df_tbls <-
    nest_df_and_svy(data, strata) %>%
    dplyr::arrange(!!!syms(strata)) %>%
    dplyr::rename(!!!syms(new_strata_names))

  # vectorized paste instead of rowwise + mutate + ungroup
  df_tbls$strata <- do.call(paste, c(df_tbls[names(new_strata_names)], list(sep = .sep)))

  df_tbls <-
    dplyr::left_join(
      df_tbls,
      df_by[, c("strata", "header"), drop = FALSE],
      by = "strata"
    )

  # direct assignment instead of dplyr::mutate
  df_tbls$tbl <-
    switch(.parent_fun,
           "tbl_strata" = map(df_tbls$data, .tbl_fun, ...),
           "tbl_strata2" = map2(df_tbls$data, df_tbls$header, .tbl_fun, ...)
    )

  # base R tbl_id instead of across + cur_column
  tbl_id_parts <- lapply(names(new_strata_names), function(col)
    paste(new_strata_names[[col]], cli::cli_format(df_tbls[[col]]), sep = "="))
  df_tbls$tbl_id <- do.call(paste, c(tbl_id_parts, list(sep = ",")))



  # combining tbls -------------------------------------------------------------
  .combine_args <-
    # default arguments
    switch(.combine_with,
           "tbl_merge" = list(tab_spanner = df_tbls$header),
           "tbl_stack" = list(group_header = df_tbls$header)
    ) %>%
    # update with user-passed arguments
    utils::modifyList(val = .combine_args %||% list())

  if (.combine_with == "tbl_merge") {
    tbl <- inject(tbl_merge(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, !!!.combine_args))
  } else if (.combine_with == "tbl_stack") {
    tbl <- inject(tbl_stack(tbls = df_tbls$tbl, tbl_ids = df_tbls$tbl_id, !!!.combine_args, tbl_id_lbls = df_tbls$strata))
  }

  # return tbl -----------------------------------------------------------------
  tbl$df_strata <- df_tbls %>% dplyr::select(starts_with("strata_"), "header")
  class(tbl) <- c("tbl_strata", .combine_with, "gtsummary")
  tbl
}

nest_df_and_svy <- function(data, strata) {
  # if data frame, use base R split instead of tidyr::nest
  if (is.data.frame(data)) {
    non_strata <- setdiff(names(data), strata)
    if (length(strata) == 1L) {
      grp <- data[[strata]]
      keys <- if (is.factor(grp)) levels(grp) else sort(unique(grp))
      split_data <- split(data[non_strata], grp)
      result <- vctrs::new_data_frame(setNames(list(keys), strata))
    } else {
      grp_df <- data[strata]
      grp_key <- do.call(paste, c(grp_df, list(sep = "\x01")))
      split_data_raw <- split(data[non_strata], grp_key)
      unique_idx <- !duplicated(grp_key)
      result <- grp_df[unique_idx, , drop = FALSE]
      rownames(result) <- NULL
      split_keys <- do.call(paste, c(result, list(sep = "\x01")))
      split_data <- split_data_raw[split_keys]
    }
    result[["data"]] <- unname(lapply(split_data, function(d) { rownames(d) <- NULL; d }))
    return(tibble::as_tibble(result))
  }

  if (length(strata) > 1) {
    cli::cli_abort(
      "Survey objects can only be stratified by a single variable.",
      call = get_cli_abort_call()
    )
  }

  # if survey object, construct a nested tibble
  dplyr::tibble(strata_var = getElement(data, "variables") |> getElement(strata) |> unique()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      data = data[data$variables[[.env$strata]] %in% .data$strata_var, ] %>% list()
    ) %>%
    dplyr::ungroup() %>%
    set_names(c(strata, "data"))
}

#' Simple utility function to get extract and calculate additional information
#' about the 'by' variable in \code{\link{tbl_summary}}
#'
#' Given a dataset and the name of the 'by' variable, this function returns a
#' data frame with unique levels of the by variable, the by variable ID, a character
#' version of the levels, and the column name for each level in the \code{\link{tbl_summary}}
#' output data frame.
#'
#' @param data data frame
#' @param by character name of the `by` variable found in data
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

df_by <- function(data, by) {
  if (is.null(by)) {
    return(NULL)
  }

  if (!is_survey(data)) {
    # classic data.frame — base R tabulation instead of dplyr pipeline
    by_vec <- data[[by]]

    if (is.factor(by_vec)) {
      levs <- levels(by_vec)
      counts <- tabulate(by_vec, nbins = length(levs))
      by_col_vals <- factor(levs, levels = levs)
      by_fct_vals <- factor(levs, levels = levs, ordered = FALSE)
    } else {
      tab <- table(by_vec, dnn = NULL)
      levs_chr <- names(tab)
      counts <- as.integer(tab)
      # preserve original type (integer stays integer, character stays character)
      by_col_vals <- utils::type.convert(levs_chr, as.is = TRUE)
      if (!identical(class(by_col_vals), class(by_vec)))
        by_col_vals <- as(levs_chr, class(by_vec)[1]) # nocov
      by_fct_vals <- factor(by_col_vals)
      levs <- levs_chr
    }

    n_levs <- length(levs)
    N_total <- sum(counts)
    by_ids <- seq_len(n_levs)

    result <- vctrs::new_data_frame(list(
      by = by_col_vals,
      by_id = by_ids,
      by_chr = as.character(levs),
      by_fct = by_fct_vals,
      by_col = paste0("stat_", by_ids),
      n = counts,
      N = rep(N_total, n_levs),
      p = counts / N_total
    ))
  } else {
    # survey object
    svy_table <- survey::svytable(c_form(right = by), data, round = TRUE) %>%
      dplyr::as_tibble() %>%
      set_names("by", "n") %>%
      dplyr::mutate(
        N = sum(.data$n),
        p = .data$n / .data$N
      )

    result <- df_by(data$variables, by) %>%
      dplyr::rename(n_unweighted = "n", N_unweighted = "N", p_unweighted = "p") %>%
      dplyr::left_join(svy_table, by = "by")
  }

  attr(result$by, "label") <- NULL
  result
}

c_form <- function(left = NULL, right = 1) {
  # quoting to take into account complex names
  if (!is.null(left)) left <- paste0("`", left, "`")
  right <- paste0("`", right, "`")
  left <- paste(left, collapse = "+")
  right <- paste(right, collapse = "+")
  stats::as.formula(paste(left, "~", right))
}
