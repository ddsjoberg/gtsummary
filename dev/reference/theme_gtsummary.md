# Available gtsummary themes

The following themes are available to use within the gtsummary package.
Print theme elements with
`theme_gtsummary_journal(set_theme = FALSE) |> print()`. Review the
[themes
vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)
for details.

## Usage

``` r
theme_gtsummary_journal(
  journal = c("jama", "lancet", "nejm", "qjecon"),
  set_theme = TRUE
)

theme_gtsummary_compact(set_theme = TRUE, font_size = NULL)

theme_gtsummary_printer(
  print_engine = c("gt", "kable", "kable_extra", "flextable", "huxtable", "tibble"),
  set_theme = TRUE
)

theme_gtsummary_language(
  language = c("de", "en", "es", "fr", "gu", "hi", "is", "ja", "kr", "mr", "nl", "no",
    "pt", "se", "zh-cn", "zh-tw"),
  decimal.mark = NULL,
  big.mark = NULL,
  iqr.sep = NULL,
  ci.sep = NULL,
  set_theme = TRUE
)

theme_gtsummary_continuous2(
  statistic = "{median} ({p25}, {p75})",
  set_theme = TRUE
)

theme_gtsummary_mean_sd(set_theme = TRUE)

theme_gtsummary_eda(set_theme = TRUE)
```

## Arguments

- journal:

  String indicating the journal theme to follow. One of
  `c("jama", "lancet", "nejm", "qjecon")`. Details below.

- set_theme:

  (scalar `logical`)  
  Logical indicating whether to set the theme. Default is `TRUE`. When
  `FALSE` the named list of theme elements is returned invisibly

- font_size:

  (scalar `numeric`)  
  Numeric font size for compact theme. Default is 13 for gt tables, and
  8 for all other output types

- print_engine:

  String indicating the print method. Must be one of `"gt"`, `"kable"`,
  `"kable_extra"`, `"flextable"`, `"tibble"`

- language:

  (`string`)  
  String indicating language. Must be one of `"de"` (German), `"en"`
  (English), `"es"` (Spanish), `"fr"` (French), `"gu"` (Gujarati),
  `"hi"` (Hindi), `"is"` (Icelandic),`"ja"` (Japanese), `"kr"` (Korean),
  `"nl"` (Dutch), `"mr"` (Marathi), `"no"` (Norwegian), `"pt"`
  (Portuguese), `"se"` (Swedish), `"zh-cn"` (Chinese Simplified),
  `"zh-tw"` (Chinese Traditional)

  If a language is missing a translation for a word or phrase, please
  feel free to reach out on
  [GitHub](https://github.com/ddsjoberg/gtsummary/issues) with the
  translated text.

- decimal.mark:

  (`string`)  
  The character to be used to indicate the numeric decimal point.
  Default is `"."` or `getOption("OutDec")`

- big.mark:

  (`string`)  
  Character used between every 3 digits to separate
  hundreds/thousands/millions/etc. Default is `","`, except when
  `decimal.mark = ","` when the default is a space.

- iqr.sep:

  (`string`)  
  String indicating separator for the default IQR in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
  If `decimal.mark=` is NULL, `iqr.sep=` is `", "`. The comma separator,
  however, can look odd when `decimal.mark = ","`. In this case the
  argument will default to an en dash

- ci.sep:

  (`string`)  
  String indicating separator for confidence intervals. If
  `decimal.mark=` is NULL, `ci.sep=` is `", "`. The comma separator,
  however, can look odd when `decimal.mark = ","`. In this case the
  argument will default to an en dash

- statistic:

  Default statistic continuous variables

## Themes

- `theme_gtsummary_journal(journal)`

  - `"jama"` *The Journal of the American Medical Association*

    - Round large p-values to 2 decimal places; separate confidence
      intervals with `"ll to ul"`.

    - [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
      Doesn't show percent symbol; use em-dash to separate IQR; run
      [`add_stat_label()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat_label.md)

    - [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)/[`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
      show coefficient and CI in same column

  - `"lancet"` *The Lancet*

    - Use mid-point as decimal separator; round large p-values to 2
      decimal places; separate confidence intervals with `"ll to ul"`.

    - [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
      Doesn't show percent symbol; use em-dash to separate IQR

  - `"nejm"` *The New England Journal of Medicine*

    - Round large p-values to 2 decimal places; separate confidence
      intervals with `"ll to ul"`.

    - [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
      Doesn't show percent symbol; use em-dash to separate IQR

  - `"qjecon"` *The Quarterly Journal of Economics*

    - [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
      all percentages rounded to one decimal place

    - [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md),[`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
      add significance stars with
      [`add_significance_stars()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_significance_stars.md);
      hides CI and p-value from output

      - For flextable and huxtable output, the coefficients' standard
        error is placed below. For gt, it is placed to the right.

- `theme_gtsummary_compact()`

  - tables printed with gt, flextable, kableExtra, or huxtable will be
    compact with smaller font size and reduced cell padding

- `theme_gtsummary_printer(print_engine)`

  - Use this theme to permanently change the default printer.

- `theme_gtsummary_continuous2()`

  - Set all continuous variables to summary type `"continuous2"` by
    default

- `theme_gtsummary_mean_sd()`

  - Set default summary statistics to mean and standard deviation in
    [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)

  - Set default continuous tests in
    [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
    to t-test and ANOVA

- `theme_gtsummary_eda()`

  - Set all continuous variables to summary type `"continuous2"` by
    default

  - In
    [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
    show the median, mean, IQR, SD, and Range by default

Use
[`reset_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
to restore the default settings

Review the [themes
vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)
to create your own themes.

## See also

[Themes
vignette](https://www.danieldsjoberg.com/gtsummary/articles/themes.html)

[`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md),
[`reset_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)

## Examples

``` r
# Setting JAMA theme for gtsummary
theme_gtsummary_journal("jama")
#> Setting theme "JAMA"
# Themes can be combined by including more than one
theme_gtsummary_compact()
#> Setting theme "Compact"

trial |>
  select(age, grade, trt) |>
  tbl_summary(by = trt) |>
  as_gt()


  

Characteristic
```

**Drug A**  
N = 98

**Drug B**  
N = 102

Age, Median (IQR)

46 (37 – 60)

48 (39 – 56)

    Unknown

7

4

Grade, n (%)

  

  

    I

35 (36)

33 (32)

    II

32 (33)

36 (35)

    III

31 (32)

33 (32)

\# reset gtsummary themes
[reset_gtsummary_theme](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)()
