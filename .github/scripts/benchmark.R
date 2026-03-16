# ── 1. Load PR version and capture functions ──
message("--- Loading PR version ---")
pkgload::load_all(".")
pr_version <- as.character(packageVersion("gtsummary"))
message("PR version: ", pr_version)

pr_style_number <- gtsummary::style_number
pr_style_sigfig <- gtsummary::style_sigfig

# Capture pipeline functions as closures so they use the PR namespace
pr_tbl_summary <- function() {
  gtsummary::trial |>
    gtsummary::tbl_summary(by = trt, include = c(age, response, grade)) |>
    gtsummary::add_overall() |>
    gtsummary::add_p() |>
    gtsummary::bold_labels() |>
    gtsummary::as_gt()
}
pr_tbl_hierarchical <- function() {
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
}

# Force-run closures once to verify they work before unloading
pr_tbl_summary()
pr_tbl_hierarchical()

pkgload::unload("gtsummary")

# ── 2. Install and load main version, capture functions ──
message("--- Installing main version ---")
remotes::install_github("ddsjoberg/gtsummary", quiet = TRUE)
library(gtsummary)
main_version <- as.character(packageVersion("gtsummary"))
message("Main version: ", main_version)

main_style_number <- gtsummary::style_number
main_style_sigfig <- gtsummary::style_sigfig

main_tbl_summary <- function() {
  gtsummary::trial |>
    gtsummary::tbl_summary(by = trt, include = c(age, response, grade)) |>
    gtsummary::add_overall() |>
    gtsummary::add_p() |>
    gtsummary::bold_labels() |>
    gtsummary::as_gt()
}
main_tbl_hierarchical <- function() {
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
}

# ── 3. Multi-round benchmarks ──
# Run multiple independent rounds and collect medians from each.
# This lets us compute confidence intervals on the ratio and
# distinguish real changes from CI runner noise.

set.seed(42)
x <- rnorm(10000)
d <- rep_len(c(0L, 1L, 2L), 10000)

n_rounds <- 5L

message("--- Running style benchmarks (", n_rounds, " rounds) ---")
style_rounds <- lapply(seq_len(n_rounds), function(r) {
  message("  Round ", r)
  res <- bench::mark(
    `style_number (main)` = main_style_number(x, digits = 2),
    `style_number (pr)`   = pr_style_number(x, digits = 2),
    `style_number varying digits (main)` = main_style_number(x, digits = d),
    `style_number varying digits (pr)`   = pr_style_number(x, digits = d),
    `style_sigfig (main)` = main_style_sigfig(x),
    `style_sigfig (pr)`   = pr_style_sigfig(x),
    iterations = 30,
    check = FALSE
  )
  data.frame(
    expression = as.character(res$expression),
    median_s = as.numeric(res$median),
    round = r
  )
}) |> do.call(what = rbind)

message("--- Running pipeline benchmarks (", n_rounds, " rounds) ---")
pipeline_rounds <- lapply(seq_len(n_rounds), function(r) {
  message("  Round ", r)
  res <- bench::mark(
    `tbl_summary (main)`      = main_tbl_summary(),
    `tbl_summary (pr)`        = pr_tbl_summary(),
    `tbl_hierarchical (main)` = main_tbl_hierarchical(),
    `tbl_hierarchical (pr)`   = pr_tbl_hierarchical(),
    iterations = 5,
    check = FALSE
  )
  data.frame(
    expression = as.character(res$expression),
    median_s = as.numeric(res$median),
    round = r
  )
}) |> do.call(what = rbind)

# ── 4. Build comparison table with confidence intervals ──
build_comparison <- function(rounds_df) {
  rounds_df$group <- sub(" \\((main|pr)\\)$", "", rounds_df$expression)
  rounds_df$version <- ifelse(
    grepl("(main)", rounds_df$expression, fixed = TRUE), "main", "pr"
  )

  groups <- unique(rounds_df$group)
  rows <- lapply(groups, function(g) {
    main_medians <- rounds_df$median_s[rounds_df$group == g & rounds_df$version == "main"]
    pr_medians   <- rounds_df$median_s[rounds_df$group == g & rounds_df$version == "pr"]

    # Paired ratio per round: pr / main
    ratios <- pr_medians / main_medians
    mean_ratio <- mean(ratios)
    diff_pct <- (mean_ratio - 1) * 100

    # 95% CI on the mean ratio via t-distribution
    n <- length(ratios)
    se <- sd(ratios) / sqrt(n)
    t_crit <- qt(0.975, df = n - 1)
    ci_lo <- (mean_ratio - t_crit * se - 1) * 100
    ci_hi <- (mean_ratio + t_crit * se - 1) * 100

    # Verdict based on whether CI excludes zero
    if (ci_hi < 0) {
      verdict <- paste0("\U2705 ", round(diff_pct, 1), "%")
    } else if (ci_lo > 0) {
      verdict <- paste0("\U274C +", round(diff_pct, 1), "%")
    } else {
      sign_chr <- ifelse(diff_pct >= 0, "+", "")
      verdict <- paste0("\U2796 ", sign_chr, round(diff_pct, 1), "%")
    }

    data.frame(
      expression = g,
      main = paste0(round(mean(main_medians) * 1000, 1), "ms"),
      pr   = paste0(round(mean(pr_medians) * 1000, 1), "ms"),
      change = verdict,
      ci = paste0("[", round(ci_lo, 1), "%, ", round(ci_hi, 1), "%]")
    )
  })
  do.call(rbind, rows)
}

style_tab <- build_comparison(style_rounds)
pipeline_tab <- build_comparison(pipeline_rounds)

header <- paste0(
  "## Performance Benchmark\n\n",
  "Comparing **main** (`", main_version, "`) vs **PR** (`", pr_version, "`)\n\n",
  "Each benchmark runs ", n_rounds, " independent rounds. ",
  "The **change** column shows the mean % difference (negative = faster).\n",
  "The **95% CI** column shows the confidence interval on the change. ",
  "If the CI excludes 0%, the result is flagged as a real improvement (\U2705) or regression (\U274C).\n\n"
)

style_section <- paste0(
  "### Style functions (10k elements)\n\n",
  paste(knitr::kable(style_tab, format = "markdown"), collapse = "\n"),
  "\n\n"
)

pipeline_section <- paste0(
  "### Pipeline benchmarks\n\n",
  paste(knitr::kable(pipeline_tab, format = "markdown"), collapse = "\n"),
  "\n"
)

report <- paste0(header, style_section, pipeline_section)
writeLines(report, "bench_report.md")
cat(report)
