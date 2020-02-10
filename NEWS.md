# gtsummary (development version)

## Documentation

* Updated documentation and README to improve readability, added more cross-linking across pages, added search terms to help users find our package, and added gif demonstrations (#340)

* README images now build differently for website vs GitHub markdown to accommodate different output formats

## Breaking changes

* Removed deprecated `fmt*_()` and `cols_label_summary()` functions (#365)

* Functions `tbl_summary_()` and `add_p_()` have been deprecated because the `by=` and `group=` arguments now accept strings (#250)

## Syntax

* Package-wide update allowing arguments that accept variable names to accept bare/symbol inputs, character inputs, stored character inputs, and tidyselect helpers.  When passing a single variable, the `vars()` function wrapper is no longer required. (#250)

    ```r
    tbl_summary(trial, label = age ~ "NEW LABEL"))
    tbl_summary(trial, label = "age" ~ "NEW LABEL"))
    tbl_summary(trial, label = c("age", "trt") ~ "NEW LABEL"))
    tbl_summary(trial, label = c(age, trt) ~ "NEW LABEL"))
    tbl_summary(trial, label = vars(age, trt) ~ "NEW LABEL"))
    tbl_summary(trial, label = vars(everything(), -age, -trt) ~ "NEW LABEL"))
    
    age_column = "age"
    tbl_summary(trial, label = age_column ~ "NEW LABEL"))
    tbl_summary(trial, label = vars(age_column) ~ "NEW LABEL")
    
    purrr::map(c("trt", "grade"), ~tbl_summary(trial, by = .x))
    ```
* Updated `tbl_uvregression()` to allow flexible tidyselect inputs and improved error messaging for arguments `x` and `y` (#249)

* After the input updates in #250, the `exclude=` argument appearing in `add_p()`, `tbl_regression()`, `tbl_uvregression()`, `as_gt()`, `as_kable()`, and `as_tibble()` was redundant.  The `exclude=` argument is now deprecated. Use `include=` instead, with tidyselect syntax. (#331)

* Functions `all_categorical()`, `all_dichotomous()`, and `all_continuous()` may now be used in `tbl_summary()` argument `type=` (#256)

## User-facing improvements

* New `pattern=` argument in `inline_text.tbl_summary()`.  Previously, we could only grab the entire cell from a `tbl_summary()` with `inline_text()`, and now we can get any single statistic reported (#254)

* Improved messaging for users who use `knitr::kable()` to print gtsummary tables, and users who have not yet installed the {gt} package (#347)

* New function `combine_terms()` allows users to combine multiple independent variables in a regression model into a single line after `tbl_regression()`.  The single line does not report regression coefficients, rather a single p-value from the `anova()` function. (#310)

* In the regression modeling functions `tbl_regression()` and `tbl_uvregression()`, the users are presented an informative error message when the tidier fails (e.g. `broom::tidy()`) alerting them to the location of the error so they may address the issue (#337, #338)

* For each release of gtsummary, we now can make reference to the version of gt our release coincides with. The commit SHA for gt is now saved in an object called `gt_sha`, and the version of gt can be installed with `remotes::install_github("rstudio/gt", ref = gt_sha)` (#329)

* Created "style" family of functions

* Improved error messaging in `tidyselect_to_list()` (#300)

## Other features and fixes

* Updated class detection to use `inherits()`, and added secondary class of `"gtsummary"` to all objects. This allows users to create their own cobbled/custom  gtsummary objects while utilizing the gtsummary print functions (#249)

* `tbl_summary()` will now summarize columns of class `difftime` (#343)

* Infrastructure update to the way styling/formatting functions are returned.  Styling functions are now returned as a column in the `.$table_header` tibble.  The update simplifies handling of these styling functions in `tbl_merge()` and `tbl_stack()`. (#298, #299)

* Bug fix for `tbl_summary()` when variables were all NA (#344)

* The data summary function `add_p()` now uses probabilities rather than counts to calculate expected cell counts to avoid an error when working with large data sets. (#341) 

* Cubic spline terms are now accurately matched to a variable name/term (#312)

* Bug fix when non-standard evaluation arguments were passed in `method.args=` argument of `tbl_uvregression()` (#322)

* Updates after the gt package deprecated `gt::cells_data()` in favor of `gt::cells_body()`. Check added to `as_gt()` ensuring a version of gt with `gt::cells_body()` in its NAMESPACE

* Lowered minimum required version of R to v3.4 (#356)

* Removed {broom.mixed} dependency as the {broom} package contained all necessary tidiers (#354)


# gtsummary 1.2.4

* Bug fix in `as_kable()` where column header did not match statistics presented when certain levels of the `by=` variable are entirely missing in `tbl_summary()` (#304)

* Updated the trial example data set `"trt"` variable to be `"Drug A"` and `"Drug B"` instead of `"Placebo"` and `"Drug"`

* Improved messaging to users when an error or warning occurs while calculating a p-value in `add_p()`.  Also, p-values are no longer omitted from output when a warning is encountered during their calculation (#283) 

* Added `tidy_fun=` argument to `tbl_regression()` and `tbl_uvregression()` allowing users to pass tidiers that are not in the {broom} and {broom.mixed} packages (#247)

# gtsummary 1.2.3

* `tbl_uvregression()` now accepts an `x=` argument to build univariate regression models where the covariate `x` remains the same while models are built the with remaining variables as the outcome (#294)

* Internal updates to the way {gt} is installed during CRAN checks.

* Bug fix when stacking `tbl_summary` objects with calculated p-values.

# gtsummary 1.2.2

## New Features

* `tbl_summary` objects may be stacked and merged with `tbl_stack()` and `tbl_merge()` (#230, #255)

* The `add_n()` function now reports additional statistics: total N, non-missing N, missing N, and their percentages.  The `missing = ` argument has been deprecated in favor of the `statistic = ` argument. (#237)

* Users may now pass a list of formulas, named lists, or a combination of both (#251)

* Users can add an option to their script to append any {gt} calls when a {gtsummary} object is printed: `gtsummary.as_gt.addl_cmds`

* Added `include =` and `exclude =` arguments to `tbl_uvregression()` (#240)

* Added standard evaluation variants, `tbl_summary_()` and `add_p_()` (#223)

* Added `as_tibble()` function that converts any {gtsummary} table to a tibble (#245)

* New `show_single_row` argument in `tbl_regression()` and `tbl_uvregression()` allows any binary variable to be printed on a single row.  Previous argument `show_yesno` is now deprecated. (#220)

## Documentation 

* Added a gallery of tables possible by merging, stacking, and modifying {gtsummary} arguments (#258)

* Added a vignette documenting each global option that can be set in {gtsummary} (#289)

* Added {lifecycle} badges to mark deprecated and experimental functions (#225)

## Other Updates

* The `by = ` column in `tbl_summary()` now has missing variables dropped rather than halting with error (#279)

* Bug fix in `vars()` selection where only first variable listed was being selected (#259)

* Bug fix where logical variable labels printed as `NA` in `tbl_regression()` (#248)

* `tbl_merge()` now interprets `tab_spanner =` text with `gt::md()` (#253)

* No longer checking outcome variable name for consistency in `tbl_regression()`---only checking independent variable names (#287)

* Improved error messaging for `add_global_p()` (#243)

* Removed `gt::cols_merge()` function ahead of the {gt} package [PR 355](https://github.com/rstudio/gt/pull/355) that changes the `cols_merge()` API (#222)

* Updated API for using custom functions to calculate p-values in `add_p()`. User now may reference a custom function by its name. (#226)

* Removed legacy support for tidyr version less than 1.0.0 (#235)

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
