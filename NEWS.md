# gtsummary (development version)

* Users can add an option to their script to append any {gt} calls when a {gtsummary} object is printed: `gtsummary.as_gt.addl_cmds`

* No longer checking outcome variable name for consistency in `tbl_regression()`---only checking independent variable names (#287)

* Added a vignette documenting each global option that can be set in {gtsummary} (#289)

* Added a gallery of tables possible by merging, stacking, and modifying gtsummary arguments (#258)

* `tbl_summary` objects are now stackable with `tbl_stack()` (#255)

* Users may now pass a list of formulas, named lists, or a combination of both (#251)

* Bug fix in `vars()` selection where only first variable listed was being selected (#259)

* The `by = ` column in `tbl_summary()` now has missing variables dropped rather than halting with error (#279)

* Added {lifecycle} badges to mark deprecated and experimental functions (#225)

* `tbl_merge()` now interprets `tab_spanner =` text with `gt::md()` (#253)

* Bug fix where logical variable labels printed as `NA` in `tbl_regression()` (#248)

* Added `as_tibble()` function that converts any gtsummary table to a tibble (#245)

* Improved error messaging for `add_global_p()` (#243)

* The `add_n()` function now reports additional statistics: total N, non-missing N, missing N, and their percentages.  The `missing = ` argument has been deprecated in favor of the `statistic = ` argument. (#237)

* Added `include =` and `exclude =` arguments to `tbl_uvregression()` (#240)

* Added ability to merge `tbl_summary` objects in `tbl_merge()` (#230)

* Added standard evaluation variants, `tbl_summary_()` and `add_p_()` (#223)

* Removed `gt::cols_merge()` function ahead of the {gt} package PR #355 that changes the `cols_merge()` API (#222)

* Updated the `show_yesno` argument to `show_single_row`. It is now generic and any binary variable can be shown on a single row. (#220)

* Updated API for using custom functions to calculate p-values in `add_p()`. User now may reference a custom function by its name. (#226)

* Removed legacy support for tidyr version less than 1.0.0 (#235)

* Improvements to documentation

# gtsummary 1.2.1

* Vignettes no longer install the {gt} package (required for CRAN check) (#217)

* Added ability to name custom `add_p()` tests (#213)

# gtsummary 1.2.0

* Users can pass variable names in backticks (#212)

* The `group = ` argument in `tbl_summary()` has been moved to `add_p()` (#208)

* Users can now write custom functions to calculate p-values in `add_p()` (#194)

* In `tbl_summary()` the `by = ` argument accepts a bare variable name instead of the variable name passed as a string (#193)

* Added support for column, row, and cell percentages in `tbl_summary()` (#181)

* Users can now set default p-value formatting functions, regression coefficient formatting functions, default level for confidence intervals, and formatting functions in `tbl_survival()` (#120)

* The {gt} package is no longer a required dependency.  If {gt} is not installed, tables will be printed with `knitr::kable()`.  The `as_kable()` function was added to the package as well. (#180)

* The function `as_gt()` now has `include = ` and `exclude = ` arguments

* Updated some function names to be the same as they were in the first version

```r
    bold_p()            <-  tab_style_bold_p()  
    bold_labels()       <-  tab_style_bold_labels()  
    bold_levels()       <-  tab_style_bold_levels()  
    italicize_labels()  <-  tab_style_italicize_labels()  
    italicize_levels()  <-  tab_style_italicize_levels()  
```

* Passing named lists in `tbl_summary()` is now defunct. 

* `tbl_stack()` fix for `tbl_uvregression` objects (#175)

* Option to exclude some variables from testing when using `add_p()` (#164)

* Updates after {gt} package renamed `cells_style()` to `cell_text()` (#78)

# gtsummary 1.1.1

* Modified `tbl_merge()` to accommodate `tbl_stack()` object (#167)

* Bug fix with incorrect column order in `tbl_summary()` with 10+ levels of by variable (#166)

# gtsummary 1.1.0

* Added {tidyselect} and {gtsummary} variable select functions (#146)

* Added `tbl_stack()` function (#152)

* Bug fix for dichotomous yes/no variables in `tbl_summary` (#158)

* Changed `add_comparison()` and `add_global()` to `add_p()` and `add_global_p()` (#143)

* Added `sort_p()` function (#105)

* Allow unobserved values to serve as the level for dichotomous variables (#149)

* Bug fix in `add_nevent()` when formula was passed to `glm()` as a string (#148)

* Added returned `call_list` to some functions without it previously (#137)

* Corrected name of call list in returned `tbl_regression()` results (#128)

* `tbl_survival()`: bug fix when upper bound of CI was not able to be estimated (#134)

* `tbl_survival()`: the `groupname` column name has been changed to `label_level` (#133)

# gtsummary 1.0.0 

First release since major re-factoring.  The {gt} package is now used as the print engine to create all tables.  Some function names have been modified to be more in line with other {gt} function calls, and additional functions have been added.  The API for some functions has also been updated.  Review documentation and vignettes for details.

### Updated Function Names

```r
    tbl_summary()       <-  fmt_table1()  
    tbl_regression()    <-  fmt_regression()  
    tbl_uvregression()  <-  fmt_uni_regression()  
    style_pvalue()      <-  fmt_pvalue()  
    style_percent       <-  fmt_percent()  
    style_ratio         <-  fmt_beta()  
```

### New Functions

```r
    tbl_survival()          as_gt()  
    tbl_merge()             style_sigfig()  
    add_nevent()            gtsummary_logo()
```

# gtsummary 0.1.0 

First version of package available [here](https://github.com/ddsjoberg/gtsummary-v0.1).
