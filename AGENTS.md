# AGENTS.md

Guidance for AI agents (and humans) working in this repository. For the
full contributor policy, defer to
[`.github/CONTRIBUTING.md`](https://www.danieldsjoberg.com/gtsummary/CONTRIBUTING.md);
this file summarizes the conventions an agent needs to make correct
changes.

## Project overview

**gtsummary** is an R package for presentation-ready summary and
analytic result tables. It builds tidy summary objects
([`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.md),
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.md),
etc.) that print to several engines
([`as_gt()`](https://www.danieldsjoberg.com/gtsummary/reference/as_gt.md),
[`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/reference/as_flex_table.md),
[`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/reference/as_kable_extra.md),
…). Requires R (\>= 4.2).

## Repository layout

- `R/` — package source. S3 methods are split one-per-file by table type
  (e.g. `add_p.tbl_summary.R`, `add_p.tbl_svysummary.R`). Add a new
  `method.tbl_type.R` file rather than branching inside one function.
- `R/import-standalone-*.R` — vendored helpers; **do not edit by hand**
  (see below).
- `tests/testthat/` — `test-*.R` files plus snapshot output in
  `_snaps/`.
- `man/` — **generated** by roxygen2; never edit `.Rd` files directly.
- `vignettes/`, `pkgdown/` — docs/website.
- `data-raw/` — scripts that build `R/sysdata.rda` and `data/`.
- `NEWS.md`, `DESCRIPTION`, `NAMESPACE` — package metadata (NAMESPACE is
  generated).

## Build, document & test

``` r
# regenerate man/*.Rd and NAMESPACE after changing roxygen or exports
Rscript -e 'devtools::document()'

# run the full test suite
Rscript -e 'devtools::test()'

# run a single test file
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-modify_source_note.R")'

# accept intended snapshot changes
Rscript -e 'testthat::snapshot_accept("modify_source_note")'
```

Notes: - Many tests use `skip_on_cran()`. Set `NOT_CRAN=true` (or run
via `devtools::test()`) to exercise them. - Some suites need optional
Suggests packages (e.g. `broom.helpers`, `survey`, `flextable`,
`officer` (Word export via
[`save_flex_docx()`](https://www.danieldsjoberg.com/gtsummary/reference/save_flex_docx.md)),
`cardx`); they `skip` if the package is absent. - testthat edition 3,
parallel enabled. Dev container image:
`ghcr.io/rocker-org/devcontainer/tidyverse:4`.

### Theme elements / internal data

The set of valid theme elements (used by
[`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/reference/set_gtsummary_theme.md)
/
[`with_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/reference/set_gtsummary_theme.md))
is **data-driven**, not hand-coded. It lives in
`data-raw/gtsummary_theme_elements.csv` and is baked into the package as
the `df_theme_elements` object inside `R/sysdata.rda`.

CSV columns: `deprecated` (logical), `fn` (the function the element
applies to), `name` (the theme-element name users set,
e.g. `tbl_summary-arg:missing_text`), `argument` (logical — is it a
function argument), `eval` (logical), `desc` (description), `example`.

To add, update, or delete a theme element:

1.  Edit `data-raw/gtsummary_theme_elements.csv` (add/modify/remove the
    row).

2.  Regenerate `R/sysdata.rda` by sourcing the build script:

    ``` r
    Rscript -e 'source("data-raw/internal_data.R")'
    ```

    This re-reads the CSV (plus the tests/translations data) and
    rewrites `R/sysdata.rda` via
    `usethis::use_data(..., internal = TRUE, overwrite = TRUE)`. Run it
    from the repo root so the relative CSV paths resolve.

3.  Commit **both** the edited CSV and the regenerated `R/sysdata.rda`.

4.  Update the corresponding docs/vignette
    (`vignettes/articles/themes.Rmd`) and any affected code in `R/` if
    you added/removed an element name.

Never edit `R/sysdata.rda` by hand — it is generated. The same script
also rebuilds `df_add_p_tests`, `lst_translations`, and `special_char`
from their respective `data-raw/` sources, so expect those to be
re-serialized too.

## Conventions

- Follow the tidyverse style guide. Use `styler`, but don’t restyle
  unrelated code.
- Edit roxygen in `.R` files (markdown syntax enabled), then run
  `devtools::document()`. Never edit generated `man/*.Rd`.
- Add a `NEWS.md` bullet under `# gtsummary (development version)` for
  any user-facing change, referencing the issue/PR number,
  e.g. `(#1987)`.
- New code ships with tests. Place them in the existing `test-*.R` file
  for the function being changed rather than creating a new standalone
  test file.
- File an issue before substantial PRs; PRs are merged with “Squash and
  merge”.

### Namespacing / imports

- `rlang` is imported in full (`@import rlang` in
  `R/gtsummary-package.R`), so its functions are called **bare — no
  `rlang::` prefix**: e.g. `set_cli_abort_call()`, `arg_match()`,
  `enquo()`, `expr()`, `inject()`, `.data`, `%||%`.
- **All other packages are called namespaced** as `pkg::fn()`:
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`gt::md()`](https://gt.rstudio.com/reference/md.html),
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), etc.
  (dplyr/glue/utils are selectively re-exported via `@importFrom`;
  everything else uses `::`).

### Standalone (vendored) files

`R/import-standalone-*.R` are copies of tidyverse-style helpers (the
rlang/usethis “standalone” pattern) that let gtsummary use familiar
idioms without taking the dependency. They provide the `check_*()` input
validators and `map()`/`imap()`, `str_*()`, `fct_*()`, and `tibble`
helpers used throughout the codebase.

- **Do not hand-edit them** — each starts with
  `# Standalone file: do not edit by hand`.
- Refresh/add one with the command in its header, e.g.
  `usethis::use_standalone("insightsengineering/standalone", "checks")`.
- Upstream sources: `insightsengineering/standalone` (checks,
  check_pkg_installed, cli_call_env, forcats, stringr, tibble) and
  `r-lib/rlang` (purrr). Fix bugs upstream, not here.

## Code patterns

Exported functions follow this skeleton — note `set_cli_abort_call()`
first, the `check_*()` validators, and the `call_list` bookkeeping
around the return:

``` r

modify_source_note <- function(x, source_note, text_interpret = c("md", "html")) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_source_note = match.call()))

  # check inputs
  check_not_missing(x)
  check_not_missing(source_note)
  check_class(x, "gtsummary")
  check_string(source_note)
  text_interpret <- arg_match(text_interpret, error_call = get_cli_abort_call())

  # ... modify x$table_body / x$table_styling ...

  # return table
  x$call_list <- updated_call_list
  x
}
```

Key object structure: `x$table_body`, `x$table_styling` (header,
footnotes, source notes, etc.), `x$call_list`, `x$inputs`.

### Performance-critical paths

Table-assembly and styling internals (the `brdg_*()` bridges, the
`modify_*()`/`bold_*()` styling functions,
[`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.md)/[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.md)/
[`add_overall()`](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.md),
the
[`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/sort_hierarchical.md)/[`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/filter_hierarchical.md)
helpers, and the `as_*()` converters) were heavily optimized in the
2.6.0 cycle. When editing these, preserve the established patterns:
prefer vectorized `map()`/base-R subsetting over
[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)
and per-row/per-table dplyr pipelines in hot loops; skip work that is a
no-op (e.g. literal-`NULL`/`TRUE` row selections, constant
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) text);
and compute shared values (visible-column sets, parsed interpreters,
data masks) once rather than per row/column/table. These refactors are
guaranteed to be output-preserving (“no change to the returned tables”),
and the snapshot tests in `tests/testthat/_snaps/` are what enforce that
— do not accept snapshot changes for a change advertised as
performance-only.

## Pull requests

- Honor
  [`.github/PULL_REQUEST_TEMPLATE.md`](https://www.danieldsjoberg.com/gtsummary/PULL_REQUEST_TEMPLATE.md);
  `.github/CONTRIBUTING.md` is the source of truth.
- **Performance-sensitive PRs:** if a change may affect package
  performance, **begin the PR title with `perf`**. The “Performance
  Benchmark” workflow (`.github/workflows/benchmark.yaml`) only runs
  when the PR title starts with `perf`; it runs
  `.github/scripts/benchmark.R` (in a memory-profiling `rocker/r-ver`
  container) to compare PR-vs-`main` time and memory across the full set
  of hot paths — the `style_*()` functions, the
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.md)
  /
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_hierarchical.md)
  /
  [`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_strata.md)
  pipelines, the `brdg_*()` assembly steps,
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.md),
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.md),
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.md),
  the `modify_*()` family, the
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/sort_hierarchical.md)/[`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/reference/filter_hierarchical.md)
  helpers, and the `as_*()` converters — and posts a benchmark report as
  a PR comment. Add a new `bench::mark()` block there when you add a hot
  path. Longer-term trends across releases are tracked in
  [ddsjoberg/tracking-gtsummary-cards-efficiency](https://github.com/ddsjoberg/tracking-gtsummary-cards-efficiency).
