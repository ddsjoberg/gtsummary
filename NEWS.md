# gtsummary 1.7.2

* Removed messaging about the former auto-removal of the `tbl_summary(group)` variable from the table: a change that occurred 3+ years ago in gtsummary v1.3.1

* Fix in `as_flex_table()` where source notes were not accurately rendered. (#1520)

* Fix in column order when `add_ci()` is run after `add_overall(last=TRUE)`. Previously, the overall columns were placed in front. (#1525)

* Line breaks (i.e. `\n`) are now removed from column headers and table cells when `as_kable()` is called. (#1526)

* Fix in `as_gt()` where columns with common spanning headers were gathered. Corrected with `gt::tab_spanner(gather = FALSE)`. (#1527)

* Fix in `remove_row_type()` where header rows for `continuous2` type variables was not removed when requested. (#1507)

* Fix where some default `add_p.tbl_summary()` categorical tests were chi-squared when it should have been Fisher's exact test. This misclassification occurred in some cases when there was a large differential in the missing pattern for one of the variables in the cross table. (#1513)

* Fix in `add_overall(col_label=)` where specified label was not always placed.  (#1505)

# gtsummary 1.7.1

### New Functions

* Added `as.data.frame()` S3 method for gtsummary class.

### New Functionality

* The `tbl_svysummary()` function may now report the design effect, e.g. `tbl_svysummary(statistic = ~"{deff}")`. (#1486)

* Added French translations for new marginal effects tidiers housed in {broom.helpers}. (#1417)

* Added theme elements to control the default headers in `tbl_svysummary()`. (#1452)

* Improved error messaging in `tbl_uvregression()` when `method=` argument is not correctly specified. (#1469)

* Updates to account for changes in {forcats} v1.0.0 and {dplyr} v1.1.0.

* `tbl_svysummary()` can now report design effects (#1486)

### Bug Fixes

* Fix in the footnote of `add_overall()` when run after `tbl_continuous()`. (#1436)

* Updating the levels of precision used in `round2()`, which is used in the background for every rounded/formatted number in a gtsummary table. (#1494)

* Bug fix when a subset of CIs are requested in `add_ci(include=)`. (#1484)

* Update in `as_hux_table()` to ensure the Ns in header are not incorrectly auto-formatted by {huxtable}.

* Fix in the `style_*()` family of functions. The attributes of the input vector--excluding the class--are retained. (#1460)

* Updated `style_ratio()` to now format negative values.

* Bug fix in `add_ci.tbl_svysummary()` for dichotomous variables.

* `add_ci.tbl_svysummary()` now takes properly into account the `percent` argument (#1470)

# gtsummary 1.7.0

### Breaking Changes

* Updated the default argument values in `tidy_robust(vcov=NULL, vcov_args=NULL)`. Users must specify the type of robust standard errors using these arguments.

* Fully removed deprecated items that were originally deprecated in v1.2.5 (released 3 years ago).
  - `add_p(exclude=)`, `as_gt(exclude=)`, `as_kable(exclude=)`, `as_tibble.gtsummary(exclude=)`, `tbl_regression(exclude=)`,  `tbl_uvregression(exclude=)` 
  - `tbl_summary_()`,`add_p_()`
  - `add_global_p(terms=)`

### New Functions

* New function `add_ci.tbl_svysummary()` for adding confidence intervals to `tbl_svysummary()` summary statistics. (#965)

### New Functionality

* Arguments pass via the dots in `tbl_uvregression(...)` are now passed to `broom.helpers::tidy_plus_plus(...)`. (#1396)

* Added new theme elements to control the default headers in `tbl_summary()`. (#1401)

* All examples that previously used `<br>` for line breaks in gt tables have been updated to use `  \n`. Additionally, the `"qjecon"` journal theme has been updated to use the updated line breaker as well. (#1311)

* Now allowing for mixed-class numeric types in `tbl_summary()`, such that `inline_text()` will not throw an error when the pattern argument is specified.

* Added `stats::mood.test()` to `add_p.tbl_summary()`. (#1397)

### New Documentation

* Added a new article illustrating how to place gtsummary tables into Shiny applications. (#1335)

### Bug Fixes

* Updated Lancet journal theme to report p-values with more precision, per the journal's reporting guidelines. (#1442)

* Fix for `as_flex_table()` when the header is blank. (#1406)

* Fix in `tbl_summary()` that now allows for column vectors to be named within a data frame. (#1403)

# gtsummary 1.6.3

* The `as_flex_table()` function now recognizes markdown bold (`**`) and italic (`_`) syntax in the headers and spanning headers. Restrictions apply. See help file for details. Users can no longer place sets of double stars and underscores *without* the text being formatted as markdown syntax. (#1361)

* The `modify_caption()` function now works with tables created with `gtreg::tbl_listing()` that do not contain a column named `"label"`. (#1358)

* Functions `tbl_summary()` and `tbl_svysummary()` now support `"{n}"`, `"{p}"`, and `"{level}"` when no `by=` variable is present for use in functions like `modify_header()`. For example, the following previously invalid code works well for both the overall column and the stratified columns:

  ```r
  trial %>%
    tbl_summary(by = trt) %>%
    add_overall() %>%
    modify_header(all_stat_cols() ~ "**{level}**, N = {n}")
  ```
  
  For the survey summary, the unweighted variants are also available. (#1366)
  
* Added experimental feature where additional arguments can be passed to `broom.helpers::tidy_plus_plus()` via `tbl_regression(...)`. (#1383)

* Updated the arguments in `tidy_robust()` to account for updates in {parameters}. (#1376)

* Allowing for 'survfit' objects of class `survfit2` in `add_nevent.tbl_survfit()`. (#1389)

* Added `oneway.test()` test to `add_p()`. (#1382)

* Now using {ggstats} to plot regression model coefficients via `plot()` instead of {GGally}. (#1367)

* Bug fix in `tbl_custom_summary()`: the full dataset (including missing observations) is now properly passed as `full_data` (#1388)

# gtsummary 1.6.2

* The following updates were made to the indentation implementation for gt output:
  - Previously, only HTML output was able to indent for gt tables, and this was implemented via `gt::tab_style()`.
  - Indentation is now available for HTML, PDF, and Word and is implemented by adding unicode non-breaking spaces to the data frame via `gt::text_transform()`.
  - The "names" for the indentation calls have been updated to `"indent"` and `"indent2"`. This change should affect very very few users. If you're not sure what the names refer to, then this does not affect you.
  - Indentation for RTF does not currently work. Instead of indented columns, irregular unicode characters are shown. This issue will be addressed in a future gt release. If you do use RTF output, and would like your output to be identical to what it was before this update, use `as_gt(include = -indent)`.

* A link to the cheat sheet has been added to the website's navigation bar. 

* Added additional options to `remove_row_type(type = c("level", "all"))`.
  - Use `type = "all"` to remove all rows associated with the variable(s) specified in `remove_row_type(variables=)`.
  - Use `type = "level"` in conjunction with new argument `level_values=` to remove specified levels for a variable, or do not use the new argument to remove all levels for categorical variables.

* Added the standard error of means to the list of available statistics for continuous data summaries in `tbl_svysummary()`. (#1291)

* Added Dutch language translations. (#1302)

* Updated `add_significance_stars()` to accept any gtsummary table (instead of only regression model summaries) and to work with `add_global_p()` (#1320)

* Added the `"var_type"` hidden column to the output of `tbl_survfit()`. This addition ensures the table will work with `remove_row_type()`. (#1343)

* Updated calls to `round()` in the `style_*()` functions to `round2()`, which implements classic rounding rules. (#1304)

* Fixed bug in `style_sigfig()` with numbers close to the thresholds. (#1298)

* Fixed bug when a column named `"variable"` was passed to `tbl_custom_summary(by=)`, which resulted in an error. (#1285)

* Bug fix in `as_tibble(fmt_missing = TRUE)`. Previously, missing assignments applied to more than one row were being ignored. (#1327)

* Bug fix in column alignment with `tbl_stack()` for `as_kable_extra()` output. (#1326)

# gtsummary 1.6.1

### New Functionality

* Added the standard error of proportions to the list of available statistics for categorical data summaries in `tbl_svysummary()`. (#1187)

* Added Tarone-Ware test to `add_p.tbl_survfit()` (#732)

* Updated `add_global_p()` to handle `tbl_uvregression()` objects where users specified the `x=` argument (when `y=` argument is more common). (#1260)

### Other Updates

* Updated start-up messaging. (#1228)

* The `paired.wilcox.test` available in `add_p.tbl_summary()` and `add_difference.tbl_summary()` was mistakenly marked as returning a difference, but it does not. The documentation has been corrected, which results in improved messaging to the user when the test is selected in `add_difference()`. (#1279)

* Improved error messages for paired tests in `add_p()` and `add_difference()` when `group=` argument is not specified. (#1273)

* Added argument `with_gtsummary_theme(msg_ignored_elements=)` argument. Use this argument to message users if any theme elements will be overwritten and therefore ignored inside the `with_gtsummary_theme()` call. (#1266)

* Swapped `gt::fmt_missing()` for `gt::sub_missing()` as the former is now deprecated. (#1257)

* The checks for `"haven_labelled"` class are now only performed for the variables indicated in `include=` and `by=` in `tbl_summary()` and `tbl_svysummary()`. The checks in `tbl_uvregression()` and `tbl_survfit.data.frame()` are only applied to the variables in `include=`, e.g. no checking for the outcome variable(s).

* Updates to labels and default formatting functions of unweighted statistics presented in `tbl_svysummary()`. (#1253)

* Adding additional structural checks in `tbl_merge()` and `inline_text()` to provide better error messaging. (#1248)

* Added `tbl_regression.crr()` method with messaging recommending use of `tidycmprsk::crr()` instead. (#1237)

* The experimental support for `ftExtra::colformat_md()` in `as_flex_table()` has been removed. The function requires evaluated YAML paths and does not allow un-evaluated references like ``bibliography:: "`r here::here()`"``. (#1229)

* Update for `tbl_summary(by=)` that now allows for a column named `"variable"` to be passed. (#1234)

* Added theme element to control what missing statistic is shown in summary tables with options to display number or percent missing or non-missing or total number of observations. (#1224)

* Renamed `modify_cols_merge()` to `modify_column_merge()` to be inline with the other `modify_column_*()` functions.

### Bug Fixes

* Fix in `as_kable_extra()` when output format is `'latex'` where a cell that had been bold or italicized had special characters double-escaped. Added a condition not to escape special characters in these styled cells. (#1230)

* Fix in `with_gtsummary_theme()`. The function restored any previously set theme, but inadvertently included the temporary theme along with it.

# gtsummary 1.6.0

### Improvements to `as_kable_extra()`

* For users who used the default kableExtra print without output-specific formatting, there are no breaking changes...the only changes will be improved output styling.
* The biggest user-facing change is that the default results for **LaTeX output** are now greatly improved compared to previous releases, when `escape=FALSE` (the new default).
  - Markdown bold, italic, and underline syntax in the headers, spanning headers, caption, and footnote will be converted to escaped LaTeX code.
  - Special LaTeX characters in the body of the table will be escaped with `.escape_latex()` or `.escape_latex2()`, e.g. `%` will be updated to `\\%`, and rendered as `%` in the PDF document.
  - The "\n" symbol will be recognized as a line break in the table headers, spanning headers, caption, and the table body.
  - `\n` is removed from footnotes 
  - The `escape=` argument is now passed to `kableExtra::add_header_row()` and `kableExtra::footnote()` as well (previously, was only passed to `knitr::kable()`).
* The `as_kable_extra(escape=, format=)` arguments have been made explicit, where previously, these arguments were passed via `...`.
* *Breaking Change* The default value of `escape=` is now `FALSE`.  If users previously used `as_kable_extra(escape=FALSE)` and had manually escaped LaTeX/HTML characters in the body of the table, these characters will now be double escaped. To print the table without the auto-escaping that is now present, utilize the new argument `as_kable_extra(addtl_fmt=FALSE)`
* *Breaking Change* The `fmt_missing=` argument was added to `as_tibble()`, `as_kable()`, and `as_kable_extra()` in the last release. This argument is now deprecated in `as_kable()` and `as_kable_extra()`. If a user does not want missing values formatted, they can exclude these commands with the `include=` argument. 
* *Breaking Change* The `strip_md_bold=` has been deprecated. The markdown syntax will automatically be stripped from headers, unless `escape = FALSE` and `format = "latex"`. In that case, the markdown syntax will be converted to LaTeX commands.
* **HTML Updates** 
  - The default markdown syntax in the headers and spanning headers is removed.
  - Special characters in the table body, headers, spanning headers, caption, and footnote will be escaped with `.escape_html()`.
  - The `"\n"` symbol is removed from the footnotes
 
### Improvements to `as_flex_table()`

* Added support for markdown syntax in {flextable} header rows by utilizing the {ftExtra} package. If this package is installed and `options(gtsummary.use_ftExtra = TRUE)` has been set (or the equivalent theme element), the bold/italic markdown syntax found in the headers will be styled. Otherwise, the markdown syntax is stripped from the header rows. (#1200)

### New Functions

* New function `as_hux_xlsx()` added to export a formatted {gtsummary} table directly to Excel.

* Added a `tbl_regression.tidycrr()` method to summarize competing risks regression models. (#1169)

* Added tidier `tidy_wald_test()`, a generic function that can calculate Wald test statistics for groups of variables in a model. The tidier expects the model object is supported by both `vcov()` and `coef()` to obtain the variance-covariance matrix and the coef vector.

* Adding functions `get_gtsummary_theme()` and `with_gtsummary_theme()` for extracting the current gtsummary theme and running code with a temporarily theme.

* Added new function  `modify_column_indent()`--a wrapper for `modify_table_styling()`--to make it easier to add and remove indentation in a table.

* Now exporting a function primarily used internally as a helper for converting a gtsummary table to gt (and other formats): `.table_styling_expr_to_row_number()`.

* Theme helper function `check_gtsummary_theme()` has been added. The function takes a gtsummary theme list as the input, and runs various consistency checks. Useful when constructing a personalized theme. This function replaces the internal checks that a passed theme element is indeed a valid theme element.

### New Functionality

* Functions `bold_labels()`, `bold_levels()`, `italicize_labels()`, `italicize_levels()` are now method functions so they can work better on `tbl_cross()` objects. 

* Total overhaul to the way statistics are saved and reported in `modify_header()`, `modify_spanning_header()`, `modify_footnote()`, and `modify_caption()`. There is now a standardized place to these statistics to be saved in all gtsummary tables (in `x$table_styling$header` in columns starting with `"modify_stat_"`). The modify functions have been updated to access the statistics from the header data frame. An added benefit to this structure, is that the statistics are available after tables are merged and stacked. Statistics available in `modify_caption()` are taken from the `"label"` column. (#1165, #1101)

* Added `add_global_p(anova_fun=)` argument allowing users to pass custom functions to calculate global p-values when `car::Anova()` does not support the model type. (#1149)

* Functions `bold_labels()`, `bold_levels()`, `italicize_labels()`, and `italicize_levels()` now bold/italicize the first column shown in the table. Previously, the `"label"` column (which is most often the first shown column) was styled.

* Added theme element to pass arguments to `knitr::kable()` in `as_kable()` and `as_kable_extra()`.

* Updated the default function in `add_glance_*(glance_fun=)` for MICE models. (#1137)

* Add `tbl_butcher(keep=)` argument to optionally keep some internal objects as needed.  (#1148)

* Adding Norwegian translations (#1143)

### Other Updates

* Removed use of `round()` in `style_number()`, and replaced it with a round function that does _not_ "round-to-even". (#1140)

* Converted the Table Gallery vignette into an FAQ+Gallery (#811)

* Added error messaging if user tries to run `add_p()` or `add_difference()` twice. (#1209)

* All models CIs were labelled as a Confidence Interval. Now Bayesian models will correctly label the Credible Interval. (#1196)

* Improved error messaging the functions `as_gt()`, `as_kable()`, `as_flex_table()`, `as_hux_table()` when an object that is not class 'gtsummary' is passed. (#1188)

* Deprecated the `as_huxtable(strip_md_bold=)` as {huxtable} now recognizes the markdown syntax and there is no reason to remove the markdown syntax.

* Added `huxtable::set_header_rows()` to the `as_hux_table()` stack.

* Improved error messaging in `modify_*()` functions. (#914)

* Added `style_percent()` as the default formatting function for un-weighted proportions. (#1181)

* The global options previously available have now been soft deprecated. All documentation of the global options was removed in v1.3.1. (#1085)

* The `as_flextable()` function has been upgraded from a soft to a hard deprecation; use `as_flex_table()` instead.

* No longer removing the survey design columns from survey objects from the columns that will be summarized in `tbl_svysummary()` and those summarized in `tbl_uvregression()`. If users previously didn't indicate which variables to summarize with `include=`, then the design columns will now appear in the summary tables. (#1166)

* Added a check for functions that accept `...` where nothing should be passed in the `...`. If a bad or misspelled argument is found, the users are informed. (#1083)

### Bug Fixes

* Fix in `"emmeans"` methods for `add_difference()` to due an argument name change in the emmeans package. We now require the most recent version of the package. (#1205)

* Bug fix in `inline_text.tbl_summary()` where one could not pass a pattern only when the column argument was `NULL`. (#1193)

* Fix in `as_gt()` when there are no hidden columns.

* Fix in `add_p.tbl_svysummary()` when Wald tests were converted to flextable. The survey tidier saved the column as a matrix-column instead of a vector, which was incompatible with flextable output. (#1153)

* Fixing Lancet theme mid-point encoding issue on Linux and MacOS. (#1146)

* Fix when using summary type gtsummary selectors (e.g. `all_continuous()`) with the `add_ci(style_fun=)` argument. (#1141)

# gtsummary 1.5.2

* Removed foreign reference to external functions in the top level of the package and replaced with indirect calls (the reason for the short time between releases). (#1129)

* Updates to the way footnotes are printed in `tbl_summary()` when there is  summary type `"continuous2"` present. Previously, all footnotes were removed, and now only the `"continuous2"` footnotes are removed. (#1127)

* Added the continuous variable name/label to the footnote for greater clarity in `tbl_continuous()` (#1123)

* Now exporting the `.create_gtsummary_object()` function as a utility for other packages to build gtsummary tables. (#1130)

* Fix when `add_overall()` was run before `add_n()`. The overall row was not being omitted from the sum and the Ns were doubled. (#1126)

* Added method "emmeans" to `add_difference()` for `tbl_svysummary()` objects.

* Updated default `add_difference()` for `tbl_svysummary()` objects to be "emmeans" for continuous and dichotomous variables, and "smd" for categorical variables.

# gtsummary 1.5.1

### New Functions

* Added new function `modify_column_alignment()` to updated column alignment. Function is a wrapper for the more complex `modify_table_styling()` function.

* New function `tbl_strata2()` that passes both the the stratified data frame as well as the stratum level to the user function. (#1091)

* Added a `add_p.tbl_continuous()` method for adding p-values to `tbl_continuous()` tables. (#1023)

* Added `add_overall.tbl_continuous()` method. (#1037)

### New Functionality

* New test option "emmeans" in `add_difference()` and `add_p()` uses the {emmeans} package to estimate marginal means/least-squares means for continuous variables, binary variables and random intercept models. (#1112)

* Column alignment is now recognized in `as_kable()` and `as_kable_extra()`. Previously, the alignment utilized the `kable()` defaults and ignored any alignment instructions included in the gtsummary table styling.

* The `as_kable_extra()` was updated to utilize `kableExtra::column_spec()` and `kableExtra::cell_spec()` to apply bold and italic styling. The choice of the function depends on the use of `escape=` in `knitr::kable()` (#1107)

* Default arguments to `knitr::kable()` may now be overwritten by passing `...` to either `as_kable()` or `as_kable_extra()`. Previously, passing a user-defined argument previously in use would result in error.

* Added `tbl_strata(.header=)` argument providing greater control over the stratum headers that are added to the tables, e.g. you can now add Ns to the headers using this argument.

* Added `add_p.tbl_cross(test.args=)` argument. (#1095)

* The `tbl_strata(.combine_args=)` has been added that lets you control all arguments in the `tbl_merge()` or `tbl_stack()` that occurs in `tbl_strata()`. (#1090)

* Added the `add_ci(pattern=)` argument, which makes it easier to merge the CI column with the primary statistics column. (#1029)

* Suppress `tbl_merge()` spanning headers by passing `tbl_merge(tab_spanner = FALSE)` (#1067)

* Functions `as_tibble()`, `as_kable()`, and `as_kable_extra()` gain the `fmt_missing=` argument that applies missing symbols to missing values. The `as_tibble()` argument defaults to `FALSE`, while the others' default is `TRUE`. (#1073)

* Adding `tbl_regression(conf.int=)` and `tbl_uvregression(conf.int=)` argument. For some models, the confidence interval adds to the computation time significantly and may not be needed. This argument will omit the CI calculation. (#1052)

* The `add_stat()` function was updated to accept `tbl_continuous()` tables.

* Multinomial models computed using MICE are now supported. (#1065)

* Added theme element to control the `tbl_regression(conf.int=)` default argument.

* It is now possible to pass a single tbl to `tbl_merge()`. This is useful when using `tbl_merge()` as a helper in other functions. (#1068)

* Added `statistics=` and `digits=` arguments to the `add_overall()` family of functions. (#1047)

* Added `digits=` argument to `tbl_cross()`. (#1046)

### Other Updates

* Allowed modification to font_size for compact theme. (#1106)

* Automatically reduced vertical white space between columns for compact flextable theme. 

* Improved user interface for `modify_*()` functions (#1064)

* Improved error messaging throughout the package. (#1050)

* Added link to the `syntax` help file to functions throughout the package. The `syntax` help file illustrates how to use the gtsummary selectors and details the formula-list notation. (#981)

* Updated Spanish translation for Wilcoxon Rank-sum Test.

* Updates and additions to Portuguese language translations. (#1098)

* Updates to the French translations.

* Updating the `add_overall()` S3 method to have a more common structure, e.g. `add_overall(x, ....)`, where previously, the `...` were not present. (#1066)

* Updated the `theme_gtsummary_journal("qjecon")` to set  `tbl_regression(conf.int =  FALSE)` by default.

* Improved error messaging in `tbl_custom_summary()`

* Updated the default formatting functions in `tbl_custom_summary()`. Previously, summaries with character results erred because the default summary function was `style_number()`. This has been updated to `style_sigfig()` for numeric columns, and `as.character()` for everything else. (#983)

* All `style_*()` functions will retain attribute, such as the names. (#1035, #1031, #981)

* The `add_n.tbl_regression()` (which is also utilized in `tbl_uvregression()`) was adding the N column without applying a formatting function. The `style_number()` function has now been added as the default styler. (#1022)

* Added class `"tbl_continuous"` to the output of `tbl_continuous()`.

* Adding `add_p()` test `"mcnemar.test.wide"` to calculate the p-value when the data are stored in a wide format, e.g. one column for a before value and a second column for after. The other McNemar test variant available in {gtsummary} expects data in a long format.

* Converted `tbl_split()` to S3 function.

* Update how calls to `gt::fmt_missing()` are constructed to be more memory efficient.

### Bug Fixes

* Fix in `add_significance_stars()` that led to an error when the summarized model did not have a confidence interval column.

* Fix in `as_flex_table()` and `as_hux_table()` where reference row was not properly placed after a `tbl_merge()` when the merged tables share common categorical variables but different reference rows. (#1063)

* Fix in `inline_text.gtsummary()` where the first level of a categorical variable could not be selected if the table had also been processed with `remove_row_type()`. (#1078)

* Fix in `modify_table_styling(cols_merge_pattern)` when it is used with `tbl_stack()` followed by `tbl_merge()`. (#1057)

* Bug fix in `separate_p_footnotes()` where test names were not being translated when `theme_gtsummary_language()` was set. (#1055)

* Fix in `tbl_merge()` when rows in a merging table are not present in the first table. (#1033)

* Fix in `as_tibble()` for `nnet::multinom()` regression models.

* Fix in `tbl_continuous()` when `include=` is not specified.

* Fix in `tbl_regression()` when a tidier returns CI columns that are all missing. (#1012)

* Fix in `add_p()`/`add_difference()` when check whether the passed test is an internal method or a custom method. The previous code required Suggested packages, such as, {lme4}, {effectsize}, and {survey}, to be installed. (#1018) 

### Breaking Changes

* No longer exporting `assert_package()`. It has been migrated to {broom.helpers} and we now use `broom.helpers::.assert_package()`. (#1051)

# gtsummary 1.5.0

### New Functions

* Added new function `tbl_continuous()` to summarize a continuous variable by 1 or more categorical variables.

* Added new function `add_ci()` that adds a new column with the confidence interval for proportions/means reported in `tbl_summary()`. (#868)

* Migrated a new function `tbl_split()` from the {bstfun} package. Function allows users to split a {gtsummary} table into multiple tables.

* Migrated a new function `separate_p_footnotes()` from the {bstfun} package. Function allows users to separate the composite footnote listing the tests performed in `add_p()`, and replaces it with individual footnotes for each test name.

* New function `tbl_custom_summary()` allowing to create a table of summary statistics using a custom summary function (#973, #976)

    * Set of helpers to be used with `tbl_custom_summary()`: `continuous_summary()`, `proportion_summary()`, `ratio_summary()`

* New function `modify_cols_merge()` that can merge two or more columns in a {gtsummary} table. (#939)

* Added function `tbl_butcher()` to reduce the size of a {gtsummary} table. After an object has been butchered, other {gtsummary} functions may not be able to execute on the object.

* Added new function `tidy_robust()` that will add robust standard errors, confidence intervals, and p-values with `tbl_regression()` and `tbl_uvregression()`. The function is a wrapper for `parameters::model_paramters()`. (#979)

### New Functionality

* Added a `CITATION` file so users can now cite the R Journal manuscript using `citation("gtsummary")`.

* Added Standardized Mean Difference method to `add_difference()`, wrapping the {smd} package's calculations. (#966)

* Extended `add_difference()` to accept `tbl_svysummary()` objects in addition to `tbl_summary()` objects.

* Added a standardized mean difference method for  `tbl_svysummary()` tables.

* Added `tbl_strata(.stack_group_header=)` argument to include/exclude the headers when tables are combined with `tbl_stack()`

* Added `tbl_strata(.quiet=)` argument.

* Allow `add_p()` and `add_difference()` to be run on the same table. (#959)

* Updated `add_overall()` to include the overall statistics in the `df_stats` tibbles saved in `.$meta_data.` This makes it possible to report any of the overall statistics using the `inline_text(pattern=)` argument.

### Other Updates

* Added a help file detailing the formula list notation used throughout the {gtsummary} package. (#981)

* Updates to `tbl_regression()` documentation. The model N is no longer reported by default, and removed that section from the help file. (#998)

* Updates to make the internal `df_stats` objects consistent across various {gtsummary} objects. Added internal function `df_stats_to_table_body` that adds the numeric df_stats tibble to `.$table_body`. The formatting functions are also added for the new columns to `.$table_styling$fmt_fun`. This function is now used in `inline_text.gtsummary()` to prepare the returned statistics (#921)

* Now using `broom::tidy()` to prepare the `car::Anova()` results. This will be more stable than the version originally written. (#898)

* The function `assert_package()` now takes the minimum required version of the package from the DESCRIPTION file, and the function is now exported.

* Now using `broom::tidy()` to prepare `aov()` test results in `add_p.tbl_summary()`, which adds additional columns to `.$table_body()` (#956)

* Updated the `README` to include links to a recording of a gtsummary presentation and to the RStudio Education blog post.

* Removed `maturing` lifecycle tag from `README`.

* Updated deprecated function `workflows::pull_workflow_fit(x)` to `workflows::extract_fit_parsnip(x)`.

### Bug Fixes

* Fix in `tbl_summary()` when a factor variable is passed that is all `NA` with no specified levels. (#977)

* Fix in `add_p.tbl_summary()` when a factor variable with all NA values is passed. (#977)

* Bug fix for the `add_difference(estimate_fun=)` argument.

### Breaking Changes

* Updated `add_p.tbl_summary(test = . ~ "mcnemar.test", group  = id)` syntax to require the `group=` argument to align with the paired Wilcoxon rank-sum test and paired t-test syntax.

* Deleted deprecated functions `add_comparison()`, `add_global()`, `tab_style_bold_p()`, `tab_style_bold_labels()`, `tab_style_italicize_levels()`, `tab_style_italicize_labels()`, `tab_style_bold_levels()`.

* The following deprecated arguments have been removed: `tbl_summary(group=)`, `as_gt(omit=)`.

* The survival package has been moved from Imports to Suggests, and will no longer automatically be installed when {gtsummary} is installed. Additionally, `survival::Surv()` is no longer re-exported with the package.

# gtsummary 1.4.2

* Update to the internals of `tbl_stack()` to better handle when two or more stacked tables are then stacked again (#906)

* Updates to make `tbl_svysummary()` compatible with {survey} package updates in v4.1 (#930)

* The `as_hux_table()` function previously stripped markdown old syntax from column headers and spanning headers. The output now uses markdown syntax in the headers by default utilizing `huxtable::set_markdown()` (#885)

* Variables passed in the `tbl_svysummary(by=)` argument will now automatically be added to `include=`. (#925)

* Bold and italic requests are now ignored for kableExtra output. These are carried out via markdown syntax, which is not supported by {kableExtra} (#917)

* Bug fix for `add_p.tbl_cross(pvalue_fun=)`; argument was being ignored.

* Updated `style_pvalue()` to format p-values slightly larger than 1 and slightly lower than 0 (due to imprecise numeric storage). (#907)

* Fix allowing for factor vectors to be passed in `tbl_stack(group_header=)`. (#908)

* Updated arguments `y=` and `x=` in `tbl_uvregression()` to allow for non-standard names (#912)

# gtsummary 1.4.1

* Updated `tbl_regression()` to be compatible with models created with the {parsnip} and {workflows} packages (#890, #647)

* Added the `modify_table_styling(text_format = "indent2")` option to double indent a row. (#864)

* Messaging update when `inline_text.gtsummary()` suspects a variable has been repeated in the gtsummary table. (#855)

* Bug fix in `add_p.tbl_summary()` for columns that are all `NA`. These variables no longer error; rather, a message is printed indicating the p-value is not possible to calculate. (#889)

* Updated `tbl_svysummary()` to be compatible with {srvyr} package (#886)

* Updated default header when using `tbl_uvregression(x=)` to `"**Outcome**"` (#867)

* The `tbl_summary(by=)` variable is now added to `include=` by default (#871)

* Variables are converted to numeric before being passed to `wilcox.test()` in `add_p()`. This avoids an error when a date difference is passed. (#880)

* Bug fix for {Hmisc} labeled data with `tbl_summary()` (#876)

* Bug fix in `add_n.tbl_summary()` to proportion missing in some cases. (#903)

* Updates to the default formatting functions in the `add_glance_*()` functions. P-values are now styled with `style_pvalue()` and No. Obs. and degrees of freedom with `style_number()` (#870)

# gtsummary 1.4.0

### New Functions

* Added new function `add_glance_table()` as a companion to `add_glance_source_note()`. Function adds model statistics, such as R-squared, to the bottom of the model summary table.

* Added new function `add_significance_stars()` adding star indicators to significant estimates and an explanatory footnote.

* Added new function `tbl_strata()`. The function aids prepares gtsummary tables stratified by one or more variables (#679)

* Adding coefficient `plot()` methods for `tbl_regression()` and `tbl_uvregression()`. Function creates a forest plot of model coefficients via `GGally::ggcoef_plot()`.

* New function `modify_fmt_fun()` has been introduced to help update the functions that format numeric columns and rows in `.x$table_body`.

* New function introduced,  `modify_table_styling()`, to update printing instructions of tables. This function replaces `modify_table_header()`, which is now soft deprecated. 

* Added function `add_vif()` to include variance inflation factors in `tbl_regression()` output. (#717)

* Added a generic function `inline_text.gtsummary()` that can report results from any gtsummary table. (#398)

### New Functionality

* Print infrastructure has been updated to allow for both row and column specification when formatting data or table styling. The `x$table_header` object has been replaced with a more general `x$table_styling`. Review the updated vignette `"gtsummary_definition.Rmd"` for details. The `x$table_body` is no longer grouped after `tbl_stack()`; rather, the grouping variable is specified in `gt::gt(groupname_col=)`

* `tbl_summary()` now accepts any class as input. Previously only non-date base classes were accepted. For non-base R classes, the summary type must be specified using `tbl_summary(type=)`. The default summary statistic for dates/times is the minimum and maximum. (#488)

* The `add_stat()` function may now return multiple columns of new statistics. Some arguments have been deprecated in the update. (#746)

* `tbl_uvregression()` now accepts both data frames and survey design objects as input. (#742)

* If the default `tidy_fun = broom::tidy` fails, `parameters::model_parameters()` attempts to tidy the model, if {parameters} is installed. (#854)

* Added a custom tidier for `mgcv::gam()` models (`tidy_gam()`) and a method function (`tbl_regression.gam()`) that uses the new tidier by default. (#745)

* Added default support for `brmsfit` model in `tbl_regression()` with new method function. (#751)

* Functions `modify_footnote()` and `modify_spanning_header()` now include the `text_interpret=` argument indicating whether to use `gt::md()` or `gt::html()` to format text. Default is `gt::md()`.

* Added/updated functions `add_n()` and `add_nevent()` to work with `tbl_regression` and `tbl_uvregression` objects. Each function now has an argument to place Ns on the label or level rows. (#744)

* Added _The Quarterly Journal of Economics_ to `theme_gtsummary_journal()`. This journal theme will be updated again after the gt package updates `cols_merge()` with a rows argument and allows for line breaks within cell.

* Korean and Icelandic language translations added for `theme_gtsummary_language()`.

* Ability to merge two or more columns with `modify_table_styling(cols_merge_pattern=)` argument.

* Added theme element `"pkgwide-fun:pre_conversion"`. The function specified here will be executed on the gtsummary object before it is printed or converted with the `as_gt()`, `as_flex_table()`, etc functions. (#773)

* The `modify_table_body(fun=)` argument has been generalized to accept formula shortcut notation.

* Added exploratory data analysis theme that shows more details by default, `theme_gtsummary_eda()`.

### Other Updates

* Updated` tbl_merge()` and `tbl_stack()` to capture the first source note and caption in the merged/stacked tables. Previously, any captions and source notes were lost. 

* Added messaging when table caption requested via `modify_caption()` for a gt table when using a version of gt that does not support captions.

* Updates to the table gallery vignette reflecting changes in the package. (#775)

* Improved error messaging when there is a problem constructing one of the univariate regression models in `tbl_uvregression()`.

* Added variable-specific formatting to `add_difference(estimate_fun=)` allowing a single table to show, for example, mean and rate differences that are formatted/rounded differently.

* Improved handling and messaging to users when columns with `"haven_labelled"` class are passed to gtsummary functions. (#805)

* Improved handling of ordered factors as the `by=` variable (#569, #540)

* Removed {usethis} package dependency and replaced with {cli}. (#768)

* Added the survey-adapted t-test to `theme_gtsummary_mean_sd()` for continuous variables in `add_p.tbl_svysummary()` (#758)

* Allowing for tidyverse shortcut notation in `tbl_survfit(estimate_fun=)` specification, e.g. `tbl_survfit(estimate_fun= ~style_sigfig(.x * 100))` (#761)

* The JAMA journal theme has been updated to merge the coefficient and confidence interval columns.

* Updated other `inline_text()` functions to wrap `inline_text.gtsummary()`

### Bug Fixes

* The `label=` argument for unstratified models was being ignored in `tbl_survfit()` (#842)

* Preserve ordering for factor variables in `tbl_survfit()`. (#764)

* Bug fix for spanning headers with kableExtra output. The spanning header was misplaced when the header text was sandwiched between two blank spanning headers.

* Bug fix when displaying an unobserved level of a factor variable dichotomously in `tbl_summary()`. (#780)

* Bug fix in `add_p()` where test name footnote was not being translated with `theme_gtsummary_language()`.

### Breaking Changes

* `tbl_survival()` has moved from questioning to deprecated. This function maintains the old `x$table_header` format (instead of the more flexible `x$table_styling`). The `"level_label"` column was renamed to `"groupname_col"` and the `x$table_body` is no longer grouped with `group_by(level_label)` 

* The back-end implementation of `add_stat_label(location = "row")` has been updated. The function now merges columns `"label"` and `"stat_label"` instead of modifying the `"label"` column directly. This _could_ be a breaking change if users had manually updated results, for example, from a `tbl_regression()` table to merge with `tbl_summary()` using `tbl_merge()` 

* The function `add_stat_label()` no longer auto-switches `location = "column"` requests to `"row"` in the presence of `"continuous2"` variables.

# gtsummary 1.3.7

* No changes. Resubmitting to resolve CRAN quirk.

# gtsummary 1.3.6
  
### New Functions

* Added function `add_difference()`, which adds difference between groups, confidence interval and p-value. (#617)

* Added function `modify_caption()` that includes table captions. For gt output, requires gt version > 0.2.2 (#701)

* Added function `modify_table_body()` allowing users to more easily make changes to gtsummary tables

* Added function `remove_row_type()` for removing header, reference, or missing rows from a gtsummary tables. (#678)

* Added selecting function `all_stat_cols()` that selects columns from `tbl_summary`/`tbl_svysummary` object with summary statistics (i.e. `"stat_0"`, `"stat_1"`, `"stat_2"`, etc.).

* New selecting function was added `all_tests()` to make it easier to select variables based on the test used to calculate the p-value, e.g. `add_p(test = c(age, marker) ~ "t.test", test.args = all_tests("t.test") ~ list(var.equal = TRUE))`

* Added functions `modify_column_hide()` and `modify_column_unhide()` to hide and unhide columns in `.$table_body`. Simple wrappers for `modify_table_header()`.

### New Functionality

* Previously, the `tbl_summary(digits=)` only accepted integer values specifying the number of decimal places to round a statistic. The argument now accepts both integers or functions, e.g. `digits = age ~ style_sigfig`. (#708)

* The `add_stat()` function now supports `tbl_svysummary()` objects in addition to `tbl_summary()` (#688)

* The `add_stat()` function can now place statistics on the label row, or the level rows for categorical variables. (#714)

* `inline_text.tbl_survfit()` updated to allow users to select p-value (and other) columns. (#589)

### Other Updates

* `modify_header()` has been updated so users may more easily access internal data while defining headers and so that it no longer adds its call to the gtsummary `.$call_list` (#719)

* The default value for `include=` argument for the `add_global_p.tbl_regression()` and `add_global_p.tbl_uvregression()` methods have been made the same with `include = everything()`, and the help files for these methods have been brought together in a single file. (#721)

* Introducing new package dependency {broom.helpers}

    * The tidying and preparation of `tbl_regression()` tables are now being performed by {broom.helpers} (#636, #607)

    * All select helper functions and the utility functions that make them possible, have been cleaned up and migrated to {broom.helpers}. (#648, #680)

    * Importing  `.generic_selector()`, `.select_to_varnames()`, and `.formula_list_to_named_list()` from {broom.helpers}: this is a function that makes it easy to create selecting functions like `all_continuous()`. The internals allow for it to be used in {broom.helpers} and {gtsummary} seamlessly. (#680)

    * Theme element has been added for controlling the other `tidy_plus_plus()` arguments. (#692)

    * Variables that do not follow standard naming conventions are now parsed correctly and presented in the regression tables.
    
* The `tbl_regression()` function is now an S3 method. The new interface allows for special handling of different model types using S3 methods: `tbl_regression.default()`, `tbl_regression.lmer()`, `tbl_regression.glmer()`, `tbl_regression.survreg()`

* `tbl_regression(add_estimate_to_reference_rows=)` argument has been added. Also added to `tbl_uvregression()`. (#692)

* Theme element for `tbl_regression(add_estimate_to_reference_rows=)` has been added. (#677) 

* Removed `"Statistics presented:"` and `"Statistical tests performed:"` prefixes from the `tbl_summary() %>% add_p()` footnotes.

* The codebase powering `add_p()` and related methods has been refactored for better performance, organization, and customizability. (#622)

    * For clarity, a help file listing each test available within gtsummary and the pseudo code for calculating the p-value has been added (see `?tests`)
    
    * Each `add_p()` method now has the `test.args=` argument. Use this argument to pass additional arguments to the statistical method, e.g. `add_p(test = c(age, marker) ~ "t.test", test.args = c(age, marker) ~ list(var.equal = TRUE))`
    
    * Additional tests have been added: paired t-test, signed rank test, and more. See `?tests` for full list.
    
    * More robust unit testing implemented for all `add_p()` methods.

* Added messaging to `tbl_stack()` to inform users that the attributes from the first table passed take precedent over the others'. (#699)

* In the `modfiy_*()` functions, if users did not select any columns, they encountered an error. Now, if no columns are selected, instructions are printed for how to correctly select columns in a gtsummary table. Moreover, if no columns are selected, the gtsummary object is now returned unaltered. (#699)

* The `add_glance_source_note()` function has been generalized so users may pass any glance function. Previously, `broom::glance()` was being used with no option to change it. (#699)

* The `...` arguments have been added to `as_gt()`. These dots are subsequently passed to `gt::gt(...)`. (#701)

* Multiple imputation models created with {mice}, and multinomial regression models created with {nnet} are now supported in `tbl_regression()` (#645)

* Updates to `add_global_p.tbl_regression()` allowing for variable names with spaces and special characters (#682)

* Added `digits=` argument to `style_percent()` (#690)

* Users may now choose which `tbl_regression()` columns to report with a theme element. they can choose among the `"estimate"`, `"std.error"`, `"statistic"`, `"ci"`, `"conf.low"`, `"conf.high"` and `"p.value"` (#637)

* Allow users to include the reference value in `tbl_regression()` via a theme element

* Users may change the symbol with a reference row symbol with a theme element. (#628)

### Bug Fixes

* Bug fix when a default statistic is set using themes for `"continuous2"` variables that has length larger than one

* Bug fix when missing/non-missing percentages requested in `add_n.tbl_summary()`

### Breaking Changes

* The default test for 2-by-2 tables with expected cell counts has been updated from the chi-squared test with continuity correction to the original chi-squared test for `add_p.tbl_summary()` (#721)

* Experimental function `add_p.tbl_survfit(test.args=)` in addition to accepting the formula list notation, also accepted a single string naming a test that was interpreted as `everything() ~ "test_name"`.  The single string is no longer accepted, and users must use the formula notation.

* Removed theme element `N_fun` that was previously marked as questioning and likely to be removed from the package.

# gtsummary 1.3.5

### New Functionality

* New summary type `continuous2` allows adding labelled statistic rows to tables in `tbl_summary()` and `tbl_svysummary()`. You can report several lines of statistics with this type. (#620)
    - The `all_continuous()` function now selects summary types `continuous` and `continuous2` by default.
    - Added `all_continuous2()` function for selecting summary type `continuous2` exclusively.
    - Added `theme_gtsummary_continuous2()` to make `continuous2` the default summary type for all continuous variables.

* New function `add_glance_source_note()` adds the statistics returned in `broom::glance()` as a source note on a `tbl_regression()` (#434)

* Exporting the `modify_table_header()` function, which is an advanced-use function used to make modifications to the `.$table_header` object to update printing instructions for the gtsummary object.

* Added two custom tidiers for use in `tbl_regression()` and `tbl_uvregression()`. (#635) 
  - `tidy_standardize()` returns standardized coefficients using the {effectsize} package
  - `tidy_bootstrap()` gives bootstrapped parameter estimates, calculated using the {parameters} package

### Bug Fixes

* Bug fix where `estimate_fun=` and `pvalue_fun=` were not being passed to `tbl_regression()` in `tbl_uvregression()`

* There was an environments bug when evaluating the LHS of the formula inputs. In some complex situations, a stored character vector of column names could not properly evaluate (#604)

* Fixed `style_ratio()` bug where there were rounding errors near one (#651)

* Fixed `style_sigfig()` bug where there were rounding errors near thresholds (#638)

* Adding the footnote from the stat columns describing the statistics presented to the overall column (#643)

### Other Updates

* Refresh of vignettes to use recently released functions (#649)

* Moved the nevent column to after the N column when `add_nevent()` is called on a `tbl_regression()` object (#439)

* gtsummary themes updates
  - Add `theme_gtsummary_mean_sd()` theme to report mean and SD by default and use t-tests and ANOVA in `add_p()` (#654)
  - Added first draft of the NEJM theme
  - Added the mid-point decimal separator for the Lancet theme

# gtsummary 1.3.4

* Added a copy of tidyselect's `where()` function to allow users to use predicate select helpers (#632)

* Fixed `tbl_cross()` bug where function defaulted to `'column'` when `margin = NULL`. Now it defaults to display no margins when `NULL`. (#624)

* Changed default of `tbl_survfit()` `missing` argument from `'\U2014'` (em dash) to NULL (CRAN issue). Em dash is still displayed by default in tables but it is set later in function. 

# gtsummary 1.3.3

### New Functions

* The {flextable} has graduated from Experimental status! Introducing `as_flex_table()`, which replaces `as_flextable()`. The updated function includes improvements to the default aesthetics of the tables, and improved consistency for spanning header rows.

* Added `tbl_svysummary()` function to summarize complex and weighted survey designs. `tbl_svysummary` is now its own class that works with `add_n()`, `add_p()`, `add_q()`, `add_stat_label()`, `add_overall()`, `inline_text()`, `tbl_merge()` and `tbl_stack()` (#460).

* The family of `tbl_survfit()` functions has been greatly expanded! 
  - `tbl_survfit.survfit()`, `tbl_survfit.list()`, and `tbl_survfit.data.frame()` have been added that accept a single survfit object, list of survfit objects, or a data frame for more flexible creation of univariate survival tables.
  - `add_p.tbl_survfit()`, `add_n.tbl_survfit()`, `add_nevent.tbl_survfit()` have been added to include additional information in `tbl_survfit()` tables.
  
* Adding `as_hux_table()` after the huxtable 5.0.0 release.

* Added `show_header_names()` function to assist when updating table headers, footnotes, and spanning headers. The function prints the current underlying column names along with their labels easing the process of identifying the column names needed to make updates. (#539)

* Added a language theme, `theme_gtsummary_language()` for translating tables into Spanish, French, Portuguese, German, Chinese (Traditional and Simplified), Hindi, Marathi, Gujarati, Japanese and Swedish (#511)

* Added `style_number()`  function which allows for granular control of how numbers are formatted throughout the package. The `style_percent()`, `style_pvalue()`, `style_sigfig()`, and `style_ratio()` functions have been updated to use `style_number()` in the background. The implication is that users can now control how every number in a gtsummary table appears. For example, formatting can be updated to use the comma as the decimal mark and also specify the big mark using the gtsummary themes. (#458)

### User-facing Updates

* Added support for competing risk cumulative incidence estimates to `tbl_survfit()` (#64, #448) 

* Updated API for `set_gtsummary_theme()`. Users no longer need call `set_gtsummary_theme(theme_gtsummary_journal())`; rather, they call `theme_gtsummary_journal()`. Each built-in theme now has an argument `set_theme = TRUE`. When `FALSE`, the theme function will invisibly return the named list the theme elements, and the theme will not be set. (#522)

* Users can now specify how many decimal places to round statistics for categorical variables in `tbl_summary()`, e.g. `45 / 100 (45.0%)`. (#458)

* Added the 'The Lancet' theme to `theme_gtsummary_journal()`

* Updated the handling for arguments that accept functions to allow users to pass anonymous functions using the tidyverse shortcut notation, e.g. `~style_pvalue(.x, digits = 2)`.

* The header for the `tbl_stack(group_header=)` column is now integrated into a typical gtsummary framework, meaning that all standard functions can be executed on it, e.g. `modify_header()` for non-gt output.

* Added `type=` argument to `add_global_p()`, and added `include=` and `keep=` arguments to `add_global_p.tbl_uvregression()` (#554)

### Internal Updates

* Updated `add_n()` internals to be more efficient without recalculating statistics previously saved in `tbl_summary()`

* {survival} package moved from Imports to Suggests

* The `add_overall()` function is now a method function, and `add_overall.tbl_summary` and `add_overall.tbl_svysummary` added (#460).

* Updated the variable labels for age and marker in the trial dataset. 

* Removed large *.gif files out of the package to reduce build size (#485)

### Bug Fixes

* Fixed bug where source note was not made smaller font size with compact theme when table was printed with {flextable} (#584)

* Bug fix for `tbl_summary()` when a data frame contained an ordered factor column (#567)

* Bug fix when only categorical summary statistics were requested for continuous variables in `tbl_summary()` and `tbl_svysummary()` (#528)

* Bug fix when _named_ list of {gtsummary} objects is passed to `tbl_merge(x=)` (#541)

* Bug fix when for `tbl_uvregression()` when adjustment variables were included in `formula = "{y} ~ {x} + age"`.  The adjustment variables were being printed in the resulting table. (#555)

### Breaking Changes

* All `lifecycle::deprecate_warn()` have been upgraded to `lifecycle::deprecate_stop()` for <= v1.2.0 (released Aug 2019)

* Removing `as_flextable()` and replacing with `as_flex_table()` due to a name conflict with `flextable::as_flextable` (#462)

# gtsummary 1.3.2

* Now returning all columns from `broom::tidy()` in `.$table_body()` (#516)

* `tbl_stack()` now accepts a named list of gtsummary tables when using the `group_header=` argument (#524)

* `add_p.tbl_summary()` bug fix for Wilcoxon rank-sum p-value calculation introduced in the last release (#525)

* Delaying the release of `as_huxtable()` until the next {huxtable} release.

# gtsummary 1.3.1

### New Functions

* Introducing themes in {gtsummary}. Use the function `set_gtsummary_theme()` to set new themes, and review the themes vignette for details on setting and creating personalized themes. (#424)

* New functions `modify_footnote()` and `modify_spanning_header()` give users control over table footnotes and spanning headers. (#464)

* Introducing `as_huxtable()`! The function converts gtsummary objects to {huxtable} objects. {huxtable} supports indentation, footnotes, and spanning headers with Word, HTML, and PDF output. (#469)

* New function `add_stat()`! Add a new column of any statistic to a `tbl_summary()` table (#495)

### User-facing Updates

* The following columns in `tbl_summary()` are now available to print for both continuous and categorical variables: total number of observations `{N_obs}`, number of missing observations `{N_miss}`, number of non-missing observations `{N_nomiss}`, proportion of missing observations `{p_miss}`, proportion of non-missing observations `{p_nomiss}`. (#473)

* Improved appearance of default `as_flextable()` output (#499)

* Added `tbl_cross(margin=)` argument to control which margins are shown in the output table. (#444)

* The missing values are now included in the calculation of p-values in `tbl_cross()`.

* Messaging about statistical methods used has been added for `add_global_p()`, `add_q()`, and `combine_terms()`. (#471)

* Added `include=` argument to `tbl_summary()`. The preferred syntax for p-values with correlated data is now `tbl_summary(..., include = -group_var) %>% add_p(group = group_var)`. The group variable is now no longer removed from the table summary. (#477)

* `add_stat_label()` function updated with `location=` and `label=` arguments to change the location of the statistic labels and to modify the text of the labels. `location = "row"` is now the default. (#467)

* `tbl_stack()` function added the `group_header=` argument that groups the stacked tables and adds a horizontal header row between them. (#468)

* Updated handling for interaction terms in `tbl_regression()`. Interaction terms may now be specified in the `show_single_row=` and `label=` arguments. (#451, #452)

### Internal Updates

* Improved error messaging when invalid statistics are requested in `tbl_summary(statistic=)` (#502)

* All columns in `as_tibble()` are now styled and converted to character. Previously, styling was applied to most columns, but there were a few that relied on default printing for the type of underlying data.  This was ok to rely on this default behavior for `as_kable()`, but with the introduction of `as_flextable()` we needed to style and format each column to character. Potential to break some code in edge cases. (#493)

* Handling of passed custom p-value functions in `add_p.tbl_summary()` has been improved with more careful handling of the environments from which the functions were passed. Other related updates were also made:
    - Users may pass their custom p-value function as a quoted string or bare.
    - The basic functions for calculating p-values, such as `t.test()` can now be passed directly e.g. `test = age ~ t.test`. We now perform a check match check for functions passed. If it is passed, we replace it with our internal version that returns the p-value and assigns the test name.
    - If a user passes a custom function, and it's not the proper form (i.e. a named list return object) AND the function returns a single numeric value, we assume that is the p-value and it's added to the gtsummary table.

* Updated the gtsummary core script, `utils-gtsummary_core.R`, to refer to all non-base R functions with the `pkg::` prefix, so other packages that copy the file don't need to import the same functions as {gtsummary} in the NAMESPACE. Now they just need to depend on the same packages. (#454)

### Bug Fixes

* Bug fix for `inline_text.tbl_summary()` when categorical variable contained levels with empty strings. There is still an issue if a user tries to select the empty string, however.

* Fixed bug where some variables in `tbl_cross()` defaulted to dichotomous instead of showing as categorical (#506)

* Bug fix when using a `tbl_summary(by=)` with missing observations in `by=` followed by `add_overall()`

* Bug fix where values `">0.9"` were incorrectly made bold using `bold_p()`. (#489)

* Bug fix for `as_flextable()`. (#482)
  - Added a formatting function to all numeric columns to force conversion to character.
  - Spanning headers were being printed in alphabetical order! Update to preserve the ordering.

# gtsummary 1.3.0

### New Functions

* Introducing `tbl_cross()`, `add_p.tbl_cross()`, and `inline_text.tbl_cross()`! Easily construct cross tabulations of two variables.

* Introducing `tbl_survfit()` and `inline_text.tbl_survfit()`!  These will eventually replace `tbl_survival()`, which will no longer be encouraged. The new functions follow the structural guidelines of a {gtsummary} object and can be merged and stacked with any other {gtsummary} object (#280)

* Introducing `as_flextable()`! The function converts gtsummary objects to {flextable} objects. {flextable} is a great option when using R markdown with Microsoft Word output. {flextable} supports indentation, footnotes, and spanning headers with Word, HTML, and PDF output.

*  Introducing `as_kable_extra()`! The function converts gtsummary objects to {kableExtra} objects. {kableExtra} supports indentation, footnotes, and spanning headers with HTML and PDF output. (#394)

### User-facing Updates

* Updated default print engine for {gtsummary} objects. {gt} is the default printer for the R console and R markdown documents with HTML output.  PDF, RTF, and Word output default to using {kable} with a note referring users to a new vignette explaining why {gt} was not used. (#395, #396)

* Updated `add_p()` custom p-value description to NOT require double escape characters for quotation marks (#361)

* Created structure and enumerated list of unit tests each vetted model must pass, and added the tests (#383)

### Internal Updates

* Each gtsummary object has an associated `.$table_header`. The code needed to print a table with either gt or kable was previously a mix of information stored in the `table_header`, and code manually added to the `gt_calls` or `kable_calls` object. Now, all the information needed to print a table is stored in `table_header`. This has the advantage that any updates to the printing will now require an update to `table_header` only, and we no longer need to update the tibble, kable and gt calls. (#412, #414)

* The {gt} package is now released on CRAN, and we've updated to depend on the CRAN version instead of the version on GitHub. This also resulted in significant updates throughout the documentation and code.  For example, we no longer provide instructions for installing {gt}, or include internal checks if {gt} is installed. (#420)

* Several functions have been made general, and may now be applied to any {gtsummary}-like object. Functions updated are `add_q()`, `bold_p()`, `sort_p()`, `tbl_merge()`, `tbl_stack()`, `bold_labels()`, `bold_levels()`, `italicize_labels()`, and `italicize_levels()`. (#434, #429, #373)

* In `tbl_regression()` we previously printed the estimate, confidence interval, and p-value for all models. But some models don't have associated methods for calculating the p-value or the confidence intervals. In this update, we now print the p-value if `tidy()` returns a `"p.value"` column.  Similarly, the confidence interval is printed if `tidy()` returns `"conf.low"` and `"conf.high"` columns. (#391, #404)

### Bug Fixes

* Bug fix when data frame passed to `tbl_summary()` with a single column (#389)

* In `tbl_summary()` passing an ordered factor in the `by=` argument no longer causes as error. (#453)

# gtsummary 1.2.6

* Bug fix for random effects regression model where coefficients were not exponentiated when requested.  Using `broom.mixed::tidy()` rather than `broom::tidy()` resolved issue.

# gtsummary 1.2.5

### Documentation

* Updated documentation and README to improve readability, added more cross-linking across pages, added search terms to help users find our package, and added gif demonstrations (#340)

* README images now build differently for website vs GitHub markdown to accommodate different output formats

### Breaking changes

* Removed deprecated `fmt*_()` and `cols_label_summary()` functions (#365)

* Functions `tbl_summary_()` and `add_p_()` have been deprecated because the `by=` and `group=` arguments now accept strings (#250)

### Syntax

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

### User-facing improvements

* New `pattern=` argument in `inline_text.tbl_summary()`.  Previously, we could only grab the entire cell from a `tbl_summary()` with `inline_text()`, and now we can get any single statistic reported (#254)

* Improved messaging for users who use `knitr::kable()` to print gtsummary tables, and users who have not yet installed the {gt} package (#347)

* New function `combine_terms()` allows users to combine multiple independent variables in a regression model into a single line after `tbl_regression()`.  The single line does not report regression coefficients, rather a single p-value from the `anova()` function. (#310)

* In the regression modeling functions `tbl_regression()` and `tbl_uvregression()`, the users are presented an informative error message when the tidier fails (e.g. `broom::tidy()`) alerting them to the location of the error so they may address the issue (#337, #338)

* For each release of gtsummary, we now can make reference to the version of gt our release coincides with. The commit SHA for gt is now saved in an object called `gt_sha`, and the version of gt can be installed with `remotes::install_github("rstudio/gt", ref = gt_sha)` (#329)

* Created "style" family of functions

* Improved error messaging in `tidyselect_to_list()` (#300)

### Other features and fixes

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

### New Features

* `tbl_summary` objects may be stacked and merged with `tbl_stack()` and `tbl_merge()` (#230, #255)

* The `add_n()` function now reports additional statistics: total N, non-missing N, missing N, and their percentages.  The `missing = ` argument has been deprecated in favor of the `statistic = ` argument. (#237)

* Users may now pass a list of formulas, named lists, or a combination of both (#251)

* Users can add an option to their script to append any {gt} calls when a {gtsummary} object is printed: `gtsummary.as_gt.addl_cmds`

* Added `include =` and `exclude =` arguments to `tbl_uvregression()` (#240)

* Added standard evaluation variants, `tbl_summary_()` and `add_p_()` (#223)

* Added `as_tibble()` function that converts any {gtsummary} table to a tibble (#245)

* New `show_single_row` argument in `tbl_regression()` and `tbl_uvregression()` allows any binary variable to be printed on a single row.  Previous argument `show_yesno` is now deprecated. (#220)

### Documentation 

* Added a gallery of tables possible by merging, stacking, and modifying {gtsummary} arguments (#258)

* Added a vignette documenting each global option that can be set in {gtsummary} (#289)

* Added {lifecycle} badges to mark deprecated and experimental functions (#225)

### Other Updates

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
