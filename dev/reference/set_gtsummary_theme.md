# Set gtsummary theme

Functions to **set**, **reset**, **get**, and evaluate **with**
gtsummary themes.

- `set_gtsummary_theme()` set a theme

- `reset_gtsummary_theme()` reset themes

- `get_gtsummary_theme()` get a named list with all active theme
  elements

- `with_gtsummary_theme()` evaluate an expression with a theme
  temporarily set

- `check_gtsummary_theme()` checks if passed theme is valid

## Usage

``` r
set_gtsummary_theme(x, quiet)

reset_gtsummary_theme()

get_gtsummary_theme()

with_gtsummary_theme(
  x,
  expr,
  env = rlang::caller_env(),
  msg_ignored_elements = NULL
)

check_gtsummary_theme(x)
```

## Arguments

- x:

  (named `list`)  
  A named list defining a gtsummary theme.

- quiet:

  **\[deprecated\]**

- expr:

  (`expression`)  
  Expression to be evaluated with the theme specified in `x=` loaded

- env:

  (`environment`)  
  The environment in which to evaluate `expr=`

- msg_ignored_elements:

  (`string`)  
  Default is `NULL` with no message printed. Pass a string that will be
  printed with
  [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html).
  The `"{elements}"` object contains vector of theme elements that will
  be overwritten and ignored.

## Details

The default formatting and styling throughout the gtsummary package are
taken from the published reporting guidelines of the top four urology
journals: European Urology, The Journal of Urology, Urology and the
British Journal of Urology International. Use this function to change
the default reporting style to match another journal, or your own
personal style.

## See also

[Themes
vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)

Available [gtsummary
themes](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)

## Examples

``` r
# Setting JAMA theme for gtsummary
set_gtsummary_theme(theme_gtsummary_journal("jama"))
#> Setting theme "JAMA"
#> Setting theme "JAMA"
# Themes can be combined by including more than one
set_gtsummary_theme(theme_gtsummary_compact())
#> Setting theme "Compact"
#> Setting theme "Compact"

set_gtsummary_theme_ex1 <-
  trial |>
  tbl_summary(by = trt, include = c(age, grade, trt)) |>
  add_stat_label() |>
  as_gt()
#> `add_stat_label()` has previously been applied. Returning gtsummary table
#> unaltered.

# reset gtsummary theme
reset_gtsummary_theme()
```
