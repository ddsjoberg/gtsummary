# gtsummary (development version)

* Add standard evaluation variants, `tbl_summary_()` and `add_p_()` (#223)

# gtsummary 1.2.1

* Vignettes install the {gt} package to a temporary directory (required for CRAN check) (#217)

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
