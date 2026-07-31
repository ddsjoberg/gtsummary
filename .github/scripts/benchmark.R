library(callr)

run_benchmarks <- function(version = "main", n_rounds = 5L) {
  # This function runs in a completely isolated R session
  callr::r(function(version, n_rounds) {
    if (version == "pr") {
      message("--- Installing and loading PR version ---")
      tmp_lib <- file.path(tempdir(), "pr_lib")
      dir.create(tmp_lib, showWarnings = FALSE)
      .libPaths(c(tmp_lib, .libPaths()))
      pak::pkg_install("local::.", lib = tmp_lib)
      library(gtsummary, lib.loc = tmp_lib)
    } else {
      message("--- Installing and loading main version ---")
      tmp_lib <- file.path(tempdir(), "main_lib")
      dir.create(tmp_lib, showWarnings = FALSE)
      .libPaths(c(tmp_lib, .libPaths()))
      pak::pkg_install("ddsjoberg/gtsummary", lib = tmp_lib)
      library(gtsummary, lib.loc = tmp_lib)
    }

    pkg_version <- as.character(packageVersion("gtsummary"))
    message("Version: ", pkg_version)

    # Global Setup for brdg_summary benchmark
    set.seed(42)
    bench_data <- as.data.frame(matrix(rnorm(500 * 50), ncol = 50))
    names(bench_data) <- paste0("v", 1:50)
    bench_data$trt <- sample(c("Drug A", "Drug B"), 500, TRUE)
    bench_data$group <- factor(rep(1:20, length.out = 500))

    suppressMessages({
      bench_tbl <- gtsummary::tbl_summary(bench_data, by = trt)
    })
    bench_cards <- bench_tbl$cards[[1]]
    bench_variables <- setdiff(unique(bench_tbl$table_body$variable), "..trt..")
    bench_type <- bench_tbl$inputs$type
    bench_statistic <- bench_tbl$inputs$statistic
    bench_by <- "trt"

    # dummy data for style and translation
    x <- rnorm(10000)
    d <- rep_len(c(0L, 1L, 2L), 10000)
    strings <- c(
      "Characteristic", "Overall", "p-value", "Unknown", "Mean",
      "Median", "SD", "N", "CI", "Variable"
    )

    res_list <- lapply(seq_len(n_rounds), function(r) {
      message("  Round ", r)
      # style
      style_res <- bench::mark(
        style_number = gtsummary::style_number(x, digits = 2),
        `style_number varying digits` = gtsummary::style_number(x, digits = d),
        style_sigfig = gtsummary::style_sigfig(x),
        iterations = 30, check = FALSE
      )

      # translation
      trans_res <- bench::mark(
        `translate_string en` = for (s in strings) gtsummary:::translate_string(s),
        `translate_string es` = for (s in strings) gtsummary:::translate_string(s, language = "es"),
        iterations = 500, check = FALSE
      )

      # pipelines
      pipe_res <- bench::mark(
        tbl_summary = {
          gtsummary::trial |>
            gtsummary::tbl_summary(
              by = trt,
              include = c(age, marker, grade, response),
              type = list(
                age ~ "continuous",
                marker ~ "continuous2",
                grade ~ "categorical",
                response ~ "dichotomous"
              ),
              statistic = list(marker ~ c("{median} ({p25}, {p75})", "{mean} ({sd})"))
            ) |>
            gtsummary::add_overall() |>
            gtsummary::add_p() |>
            gtsummary::bold_labels() |>
            gtsummary::as_gt()
        },
        tbl_hierarchical = {
          gtsummary::tbl_hierarchical(
            data = cards::ADAE,
            variables = c(AESOC, AETERM, AESEV),
            by = TRTA,
            id = USUBJID,
            denominator = cards::ADSL,
            overall_row = TRUE,
            label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
          ) |>
            gtsummary::add_overall() |>
            gtsummary::as_gt()
        },
        tbl_strata = {
          gtsummary::tbl_strata(
            bench_data,
            strata = group,
            .tbl_fun = ~ .x |> gtsummary::tbl_summary(by = trt, include = c(v1, v2, v3, v4, v5))
          )
        },
        iterations = 5, check = FALSE
      )

      # brdg_summary
      brdg_res <- bench::mark(
        brdg_summary = gtsummary::brdg_summary(
          cards = bench_cards, variables = bench_variables, type = bench_type,
          statistic = bench_statistic, by = bench_by
        ),
        iterations = 20, check = FALSE
      )

      all_res <- rbind(style_res, trans_res, pipe_res, brdg_res)
      data.frame(
        expression = as.character(all_res$expression),
        median_s = as.numeric(all_res$median),
        mem_bytes = as.numeric(all_res$mem_alloc),
        round = r,
        version = version,
        pkg_version = pkg_version,
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, res_list)
  }, args = list(version = version, n_rounds = n_rounds), show = TRUE)
}

n_rounds <- as.integer(Sys.getenv("N_ROUNDS", unset = "5"))
df_pr <- run_benchmarks("pr", n_rounds)
df_main <- run_benchmarks("main", n_rounds)
df_all <- rbind(df_main, df_pr)

pr_version <- unique(df_pr$pkg_version)
main_version <- unique(df_main$pkg_version)

build_comparison <- function(rounds_df) {
  groups <- unique(rounds_df$expression)
  rows <- lapply(groups, function(g) {
    main_medians <- rounds_df$median_s[rounds_df$expression == g & rounds_df$version == "main"]
    pr_medians <- rounds_df$median_s[rounds_df$expression == g & rounds_df$version == "pr"]

    ratios <- pr_medians / main_medians
    mean_ratio <- mean(ratios)
    diff_pct <- (mean_ratio - 1) * 100

    n <- length(ratios)
    if (n > 1) {
      se <- sd(ratios) / sqrt(n)
      t_crit <- qt(0.975, df = n - 1)
      ci_lo <- (mean_ratio - t_crit * se - 1) * 100
      ci_hi <- (mean_ratio + t_crit * se - 1) * 100
    } else {
      ci_lo <- diff_pct
      ci_hi <- diff_pct
    }

    if (ci_hi < 0) {
      verdict <- paste0("\U2705 ", round(diff_pct, 1), "%")
    } else if (ci_lo > 0) {
      verdict <- paste0("\U274C +", round(diff_pct, 1), "%")
    } else {
      sign_chr <- ifelse(diff_pct >= 0, "+", "")
      verdict <- paste0("\U2796 ", sign_chr, round(diff_pct, 1), "%")
    }

    # memory allocation is deterministic, so summarize with the mean across
    # rounds (no confidence interval) and flag purely by sign
    main_mem <- mean(rounds_df$mem_bytes[rounds_df$expression == g & rounds_df$version == "main"])
    pr_mem <- mean(rounds_df$mem_bytes[rounds_df$expression == g & rounds_df$version == "pr"])
    mem_pct <- (pr_mem / main_mem - 1) * 100

    if (mem_pct < -0.05) {
      mem_verdict <- paste0("\U2705 ", round(mem_pct, 1), "%")
    } else if (mem_pct > 0.05) {
      mem_verdict <- paste0("\U274C +", round(mem_pct, 1), "%")
    } else {
      mem_verdict <- paste0("\U2796 ", ifelse(mem_pct >= 0, "+", ""), round(mem_pct, 1), "%")
    }

    data.frame(
      expression = g,
      main = paste0(round(mean(main_medians) * 1000, 1), "ms"),
      pr = paste0(round(mean(pr_medians) * 1000, 1), "ms"),
      change = verdict,
      ci = paste0("[", round(ci_lo, 1), "%, ", round(ci_hi, 1), "%]"),
      `main mem` = format(bench::as_bench_bytes(main_mem)),
      `pr mem` = format(bench::as_bench_bytes(pr_mem)),
      mem_delta = mem_verdict,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tab <- build_comparison(df_all)
# display label for the memory-change column (\U escapes are not allowed inside
# backtick names, so set the column name here where a string literal is fine)
names(tab)[names(tab) == "mem_delta"] <- "mem \U0394"

style_names <- c("style_number", "style_number varying digits", "style_sigfig")
trans_names <- c("translate_string en", "translate_string es")
pipe_names <- c("tbl_summary", "tbl_hierarchical", "tbl_strata")
brdg_names <- c("brdg_summary")

style_tab <- tab[tab$expression %in% style_names, ]
trans_tab <- tab[tab$expression %in% trans_names, ]
pipe_tab <- tab[tab$expression %in% pipe_names, ]
brdg_tab <- tab[tab$expression %in% brdg_names, ]

header <- paste0(
  "## Performance Benchmark\n\n",
  "Comparing **main** (`", main_version, "`) vs **PR** (`", pr_version, "`)\n\n",
  "Each benchmark runs ", n_rounds, " independent rounds. ",
  "The **change** column shows the mean % difference (negative = faster).\n",
  "The **95% CI** column shows the confidence interval on the change. ",
  "If the CI excludes 0%, the result is flagged as a real improvement (\U2705) or regression (\U274C).\n\n",
  "The **main mem** / **pr mem** columns show total memory allocated ",
  "(`bench::mark()` `mem_alloc`), and **mem \U0394** its % change (negative = less memory). ",
  "Allocation is deterministic, so no confidence interval is shown.\n\n"
)

style_section <- paste0(
  "### Style functions (10k elements)\n\n",
  paste(knitr::kable(style_tab, format = "markdown", row.names = FALSE), collapse = "\n"),
  "\n\n"
)
trans_section <- paste0(
  "### Translation (10 strings per iteration)\n\n",
  paste(knitr::kable(trans_tab, format = "markdown", row.names = FALSE), collapse = "\n"),
  "\n\n"
)
pipe_section <- paste0(
  "### Pipeline benchmarks\n\n",
  paste(knitr::kable(pipe_tab, format = "markdown", row.names = FALSE), collapse = "\n"),
  "\n\n"
)
brdg_section <- paste0(
  "### `brdg_summary` (50 variables)\n\n",
  paste(knitr::kable(brdg_tab, format = "markdown", row.names = FALSE), collapse = "\n"),
  "\n"
)

report <- paste0(header, style_section, trans_section, pipe_section, brdg_section)
writeLines(report, "bench_report.md")
cat(report)
