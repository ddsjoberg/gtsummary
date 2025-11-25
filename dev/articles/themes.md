# gtsummary themes

It’s possible to set themes in {gtsummary}. The themes control many
aspects of how a table is printed. Function defaults can be controlled
with themes, as well as other aspects that are not modifiable with
function arguments.

The {gtsummary} package comes with a few themes. Our focus is tables
that are ready for publication and encourage themes that assist in that
process; for example, the `theme_gtsummary_journal(journal = "jama")`
theme sets defaults that align with the [published
guidelines](https://jamanetwork.com/journals/jama/pages/instructions-for-authors)
from the *Journal of the American Medical Association*—*JAMA*. The
defaults in {gtsummary} were written to align with the reporting
guidelines for *European Urology*, *The Journal of Urology*, *Urology*,
and the *British Journal of Urology International*.

## Setting Themes

To set a pre-defined theme, simply include the theme function in a
script or the R console.

``` r
theme_gtsummary_journal(journal = "jama")
```

Use the
[`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
function to set user-defined themes (more on that below).

``` r
set_gtsummary_theme(my_custom_theme)
```

Themes must be set before you create the {gtsummary} tables. Let’s take
a look at the default table, comparing data between treatment groups.

#### Default Theme

``` r
library(gtsummary)

trial |> 
  tbl_summary(by = trt, include = c(age, grade)) |> 
  add_p()
```

[TABLE]

#### JAMA Theme

Now, the same code with the JAMA theme.

``` r
theme_gtsummary_journal(journal = "jama")
#> Setting theme "JAMA"
```

[TABLE]

By setting the theme, we were able to change the default formatting for
the p-value and add a dash between the 25th and 75th percentiles.

#### JAMA + Compact Theme

Themes can be stacked as well. In the example below, the JAMA theme and
the compact theme (reduces font size and cell padding) are both called
and both themes are utilized.

``` r
theme_gtsummary_journal(journal = "jama")
#> Setting theme "JAMA"
theme_gtsummary_compact()
#> Setting theme "Compact"
```

[TABLE]

#### JAMA + Compact + Language Theme

All {gtsummary} tables can be translated into another language using
[`theme_gtsummary_language()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)!

[TABLE]

*Clear all previously set themes using*
[`reset_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md).

``` r
reset_gtsummary_theme()
```

## Writing Themes

### Theme Structure

There are many parts of a {gtsummary} table that may be controlled with
theme elements. To construct a personalized theme, create a named list
of at least one theme element. Here’s an example of a theme that
modifies the function that styles p-values and updates the default
statistics reported in
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).

``` r
my_theme <-
  list(
    # round large p-values to two places
    "pkgwide-fn:pvalue_fun" = label_style_pvalue(digits = 2),
    "pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 2, prepend_p = TRUE),
    # report median (Q1 - Q2) and n (percent) as default stats in `tbl_summary()`
    "tbl_summary-arg:statistic" = list(all_continuous() ~ "{median} ({p25} - {p75})",
                                       all_categorical() ~ "{n} ({p})")
  )
```

Once you create the theme, first check the structure using
[`check_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md).
Then apply or set the theme with
[`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md).

``` r
set_gtsummary_theme(my_theme)
```

### Theme Elements

Each theme element follows a naming structure:
`"<function name>-<input type>:<description>"`. The function name is the
function the change applies to, the input type specifies class or type
of the theme element, and the description is brief text characterizing
the theme element.

Theme elements fall into two categories. The first is modifying internal
behavior of the functions that is not directly controllable by function
arguments.

[TABLE]

The second type of theme elements set function argument defaults. The
values of these theme elements must align with the functions’ accepted
input for the argument.

| Theme Element                                                                                                                                                                                                                                                                                               |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| add_overall.tbl_summary                                                                                                                                                                                                                                                                                     |
| `add_overall.tbl_summary-arg:col_label`, `add_overall.tbl_svysummary-arg:col_label`                                                                                                                                                                                                                         |
| add_p.tbl_cross                                                                                                                                                                                                                                                                                             |
| `add_p.tbl_cross-arg:pvalue_fun`, `add_p.tbl_cross-arg:source_note`, `add_p.tbl_cross-arg:test`                                                                                                                                                                                                             |
| add_p.tbl_summary                                                                                                                                                                                                                                                                                           |
| `add_p.tbl_summary-arg:test`                                                                                                                                                                                                                                                                                |
| add_p.tbl_svysummary                                                                                                                                                                                                                                                                                        |
| `add_p.tbl_svysummary-arg:pvalue_fun`, `add_p.tbl_svysummary-arg:test`                                                                                                                                                                                                                                      |
| add_q                                                                                                                                                                                                                                                                                                       |
| `add_q-arg:method`, `add_q-arg:pvalue_fun`                                                                                                                                                                                                                                                                  |
| add_stat_label                                                                                                                                                                                                                                                                                              |
| `add_stat_label-arg:location`                                                                                                                                                                                                                                                                               |
| as_kable                                                                                                                                                                                                                                                                                                    |
| `as_kable-arg:dots`                                                                                                                                                                                                                                                                                         |
| assign_summary_type                                                                                                                                                                                                                                                                                         |
| `assign_summary_type-arg:cat_threshold`                                                                                                                                                                                                                                                                     |
| style_number                                                                                                                                                                                                                                                                                                |
| `style_number-arg:big.mark`, `style_number-arg:decimal.mark`                                                                                                                                                                                                                                                |
| tbl_regression                                                                                                                                                                                                                                                                                              |
| `tbl_regression-arg:conf.level`, `tbl_regression-arg:conf.int`, `tbl_regression-arg:estimate_fun`, `tbl_regression-arg:add_estimate_to_reference_rows`, `tbl_regression-arg:tidy_fun`                                                                                                                       |
| tbl_summary                                                                                                                                                                                                                                                                                                 |
| `tbl_summary-arg:digits`, `tbl_summary-arg:missing`, `tbl_summary-arg:missing_text`, `tbl_summary-arg:missing_stat`, `tbl_summary-arg:percent`, `tbl_summary-arg:sort`, `tbl_summary-arg:statistic`, `tbl_summary-arg:type`, `tbl_summary-arg:value`                                                        |
| tbl_survfit                                                                                                                                                                                                                                                                                                 |
| `tbl_survfit-arg:statistic`                                                                                                                                                                                                                                                                                 |
| tbl_svysummary                                                                                                                                                                                                                                                                                              |
| `tbl_svysummary-arg:digits`, `tbl_svysummary-arg:label`, `tbl_svysummary-arg:missing`, `tbl_svysummary-arg:missing_text`, `tbl_svysummary-arg:missing_stat`, `tbl_svysummary-arg:percent`, `tbl_svysummary-arg:sort`, `tbl_svysummary-arg:statistic`, `tbl_svysummary-arg:type`, `tbl_svysummary-arg:value` |
| tbl_custom_summary                                                                                                                                                                                                                                                                                          |
| `tbl_custom_summary-arg:digits`, `tbl_custom_summary-arg:label`, `tbl_custom_summary-arg:missing`, `tbl_custom_summary-arg:missing_text`, `tbl_custom_summary-arg:type`, `tbl_custom_summary-arg:value`                                                                                                     |
