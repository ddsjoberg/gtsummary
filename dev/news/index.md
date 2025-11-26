# Changelog

## gtsummary (development version)

- Added AI chat bot to homepage, powered by `kapa.ai`.

- Fixed bug in
  [`tbl_strata_nested_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata_nested_stack.md)
  causing incorrect column indentation in non-standard tables.
  ([\#2272](https://github.com/ddsjoberg/gtsummary/issues/2272))

- Adding functions `tlb_ard_strata()` and `tlb_ard_strata2()`.
  ([\#1852](https://github.com/ddsjoberg/gtsummary/issues/1852))

- Added [`head()`](https://rdrr.io/r/utils/head.html) and
  [`tail()`](https://rdrr.io/r/utils/head.html) S3 methods for
  `gtsummary` objects.
  ([\#2335](https://github.com/ddsjoberg/gtsummary/issues/2335))

- Added additional messaging to
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  when tables *may not* merge properly.
  ([\#2348](https://github.com/ddsjoberg/gtsummary/issues/2348))

- Updated
  [`tbl_ard_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_hierarchical.md)
  to pre-process non-standard hierarchical ARDs so sorting can be
  applied.
  ([\#2308](https://github.com/ddsjoberg/gtsummary/issues/2308))

- Added theme element `"assign_summary_type-arg:cat_threshold"` for
  greater control over default summary types.

- When a data frame is passed in the `tbl_summary(percent)` argument,
  the headers are now tabulated with this data frame.
  ([\#2322](https://github.com/ddsjoberg/gtsummary/issues/2322))

- Corrected handling in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  of character vectors that contains `"NULL"` values.
  ([\#2345](https://github.com/ddsjoberg/gtsummary/issues/2345))

- Fixed bug in
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)
  causing variable labels to disappear from table when not all variables
  are in `include`.
  ([\#2332](https://github.com/ddsjoberg/gtsummary/issues/2332))

- Added the `"add_overall.tbl_svysummary-arg:col_label"` theme element
  to control the default value for
  `add_overall.tbl_svysummary(col_label)` argument.
  ([\#2354](https://github.com/ddsjoberg/gtsummary/issues/2354))

## gtsummary 2.4.0

CRAN release: 2025-08-28

#### New Features and Functions

- Greatly improved messaging when column headers differ in
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md).
  ([\#2266](https://github.com/ddsjoberg/gtsummary/issues/2266))

- Added the `tbl_split_by_rows(variable_level)` argument to split a
  table by the levels of a column in `x$table_body`.

- The `tbl_stack(tbl_id_lbls)` argument has been added. When specified,
  a new column is added to the resulting `.$table_body` labeling the
  rows associated with each table ID. This argument is utilized in
  [`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md)
  and all returned tables include a hidden column with the stratum
  levels. ([\#2288](https://github.com/ddsjoberg/gtsummary/issues/2288))

- Refactored
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)
  to allow for different sorting methods at each hierarchy variable
  level.

- Updated
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)
  and
  [`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/filter_hierarchical.md)
  to always keep attribute and total N rows at the bottom of the ARD.

- Added parameters `var` and `quiet` to
  [`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/filter_hierarchical.md)
  to allow for filtering by specific hierarchy variables and to silence
  messages, respectively.

#### Other Updates

- No longer calculating `deff` by default in
  `tbl_svysummary(type='categorical')`.

- Improved the print method for class `'tbl_split'` to be more
  informative.

- Added a [`dim()`](https://rdrr.io/r/base/dim.html) S3 method for
  `'gtsummary'` class, which also allows for
  [`nrow()`](https://rdrr.io/r/base/nrow.html) calls on gtsummary
  objects.

- The {cardx} package is now a strong dependency.
  ([\#2256](https://github.com/ddsjoberg/gtsummary/issues/2256))

- Added a method for class `"tbl_ard_hierarchical"` to
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)
  and
  [`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/filter_hierarchical.md).

#### Bug Fixes

- Bug fix in
  [`add_p.tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_cross.md)
  when custom p-value methods were created without any additional
  arguments. The test and test label would revert to the default, and
  this is resolved.

## gtsummary 2.3.0

CRAN release: 2025-07-03

#### New Features and Functions

- Added
  [`tbl_split_by_rows()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_split_by.md)
  and
  [`tbl_split_by_columns()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_split_by.md)
  to split tables horizontally (row-wise) and vertically (column-wise).
  ([\#2216](https://github.com/ddsjoberg/gtsummary/issues/2216))

- Users are now allows to specify/override the denominator by passing an
  integer or a data frame to the `tbl_summary(percent)` argument.
  ([\#2239](https://github.com/ddsjoberg/gtsummary/issues/2239))

- Added the `tbl_merge(tbl_ids)` and `tbl_stack(tbl_ids)` arguments that
  allows used to label the gtsummary input tables. This is particularly
  helpful when calling
  [`gather_ard()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/gather_ard.md),
  which will return a named list of ARDs where the names are the
  assigned tbl IDs.
  ([\#2224](https://github.com/ddsjoberg/gtsummary/issues/2224))

- Adding new function
  [`add_difference_row()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference_row.md).
  The function is similar to
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md),
  except that differences are placed on the rows below the summary
  statistics.
  ([\#2138](https://github.com/ddsjoberg/gtsummary/issues/2138))

- The `style_*()` and `label_style_*()` functions have gained the `na`
  argument allowing users to specify how `NA` values are returned.

- The `tbl_stack(attr_order)` argument has been added that allows users
  to specify the order in which the individual attributes take precedent
  when they are stacked. For example, to use the headers from the second
  table, specify `attr_order=2`.
  ([\#2202](https://github.com/ddsjoberg/gtsummary/issues/2202))

- The
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  function now takes advantage of a new feature in {broom.helpers} to
  support multicomponent models, such as, multinomial models and
  mixed-effects models. Review
  [`broom.helpers::tidy_group_by()`](https://larmarange.github.io/broom.helpers/reference/tidy_group_by.html)
  for details.
  ([\#1540](https://github.com/ddsjoberg/gtsummary/issues/1540))

#### Other Updates

- Added an article on `modify_*()` functions.
  ([\#2209](https://github.com/ddsjoberg/gtsummary/issues/2209))

- Added theme elements `tbl_hierarchical_count-fn:addnl-fn-to-run` and
  `tbl_hierarchical_count-fn:addnl-fn-to-run`.
  ([\#2262](https://github.com/ddsjoberg/gtsummary/issues/2262))

- Added the `"add_overall.tbl_summary-arg:col_label"` theme element to
  control the default value of `add_overall.tbl_summary(col_label)`.
  ([\#2246](https://github.com/ddsjoberg/gtsummary/issues/2246))

- Variable labels are now retained with
  `tbl_summary(sort=~'frequency')`.
  ([\#2260](https://github.com/ddsjoberg/gtsummary/issues/2260))

- Functions
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)
  and
  [`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/filter_hierarchical.md)
  are now S3 generics.

- The
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  function no longer returns an error when an unstratified table is
  passed. Rather, a message is printed and the unaltered table is
  returned.

#### Lifecycle Updates

- Updated the function name of
  [`modify_column_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated.md)
  to the more accurately named
  [`modify_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_indent.md)
  as the function operates on cells rather than columns.

#### Bug Fixes

- Fix in
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  when there are missing values in the `by` column.

- Fixed bug in
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
  causing the overall row label to be incorrectly used as a variable
  label.

- Fixed bug in
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)
  causing some rows to be dropped when sorting unstratified tables.
  ([\#2237](https://github.com/ddsjoberg/gtsummary/issues/2237))

- Fixed bug in
  [`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/filter_hierarchical.md)
  causing an error for unstratified tables.

- Fix in
  [`gather_ard()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/gather_ard.md)
  for
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  and
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md).
  Previously, only the ARD for the primary regression model(s) would be
  returned, and now all ARDs are returned including those from
  subsequent calls to `add_*()` functions.
  ([\#2208](https://github.com/ddsjoberg/gtsummary/issues/2208))

- Fix in `tbl_merge(merge_vars)` argument when a table contained a
  `"row_type"` column and the tables were not merged by this variable
  (which is included in the default).
  ([\#2205](https://github.com/ddsjoberg/gtsummary/issues/2205))

- Fix in
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  where not all table styling were copied from the overall table to the
  newly merged table. This did not affect any native gtsummary tables,
  but did appear in an extension package.

- Fix in experimental function
  [`modify_post_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_post_fmt_fun.md)
  when tables including this call, did not specify the `rows` argument,
  and were later stacked. The `enquo` environment associated with the
  `rows` argument was empty when the argument is not specified, and the
  empty environment caused an issue with evaluation if the table was
  later stacked.

- Fix in `tbl_svysummary(missing='ifany')` when the weighted number of
  missing observations was \<= 0.5. Previously, these counts were
  coerced to integer and rounded to zero, and therefore, did not appear
  in the table.
  ([\#2229](https://github.com/ddsjoberg/gtsummary/issues/2229))

- Fix in
  [`gather_ard()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/gather_ard.md)
  for
  [`tbl_strata_nested_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata_nested_stack.md)
  tables, where the function returned an empty list.
  ([\#2223](https://github.com/ddsjoberg/gtsummary/issues/2223))

## gtsummary 2.2.0

CRAN release: 2025-04-14

#### New Features and Functions

- Added function
  [`modify_post_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_post_fmt_fun.md)
  to perform formatting on cells after the primary formatting function
  has been applied. The functionality is similar to
  [`gt::text_transform()`](https://gt.rstudio.com/reference/text_transform.html).
  ([\#2014](https://github.com/ddsjoberg/gtsummary/issues/2014))

- Data pre-processing has now been re-introduced for calculations in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  and
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md).
  Data pre-processing steps were removed in the v2.0 release; however,
  in some cases—particularly
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  for dichotomous variables—the reduced functionality was affecting the
  user experience. See
  [`?tests`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
  for details on data pre-processing.
  ([\#2165](https://github.com/ddsjoberg/gtsummary/issues/2165))

- The
  [`add_variable_group_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_variable_group_header.md)
  function has been generalized to work with any gtsummary table, where
  previously only `'tbl_summary'` were accepted.
  ([\#2197](https://github.com/ddsjoberg/gtsummary/issues/2197))

- The footnote placed on the p-value column by
  [`add_significance_stars()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_significance_stars.md)
  no longer replaces any existing footnote. Rather the footnote is added
  to any existing footnote.
  ([\#2184](https://github.com/ddsjoberg/gtsummary/issues/2184))

- The `remove_*()` family of functions default argument values have been
  updated to remove *all* footnotes, source notes, abbreviations, column
  merging, bold and italic styling by default, so the users are not
  longer required to remove, for example, one source note at a time. The
  one exception is `remove_spanning_headers()`, which will remove the
  first level spanning headers be default.
  ([\#2161](https://github.com/ddsjoberg/gtsummary/issues/2161))

- Added a new theme element `tbl_svysummary-arg:missing_stat`

- Added functions
  [`sort_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_hierarchical.md)
  and
  [`filter_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/filter_hierarchical.md)
  to sort and filter hierarchical tables.
  ([\#2096](https://github.com/ddsjoberg/gtsummary/issues/2096))

#### Bug Fixes

- Fixed a bug in
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
  where the `group*` variables of the resulting `table_body` were not
  fully populated.
  ([\#2192](https://github.com/ddsjoberg/gtsummary/issues/2192))

- Corrected bug causing an error in
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
  when more than one `by` variable level had been filtered out of the
  data. ([\#2173](https://github.com/ddsjoberg/gtsummary/issues/2173))

- Corrected the class of objects returned from
  [`tbl_strata_nested_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata_nested_stack.md).

- Corrected bug in
  [`tbl_strata_nested_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata_nested_stack.md)
  when the strata variable had a single level. Previously, the
  indentation of the single strata level was not correct.

- Fix in
  [`tbl_strata_nested_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata_nested_stack.md)
  when there are unobserved combinations of strata levels.
  ([\#2179](https://github.com/ddsjoberg/gtsummary/issues/2179))

- Fixed bug in
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
  when a column was named `'missing'`.
  ([\#2182](https://github.com/ddsjoberg/gtsummary/issues/2182))

- Fix in theme element `tbl_summary-arg:missing_stat` that was not being
  applied.
  ([\#2176](https://github.com/ddsjoberg/gtsummary/issues/2176))

- Corrected bug in
  [`tbl_likert()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_likert.md)
  where variables in table always appeared in alphabetical order.
  ([\#2195](https://github.com/ddsjoberg/gtsummary/issues/2195))

- Corrected bug in
  [`add_n.tbl_likert()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n_summary.md)
  where the Ns were not formatted with
  [`style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_number.md).
  ([\#2195](https://github.com/ddsjoberg/gtsummary/issues/2195))

## gtsummary 2.1.0

CRAN release: 2025-02-19

#### New Features and Functions

- Added function
  [`tbl_strata_nested_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata_nested_stack.md).
  The function is similar to
  [`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md),
  but this function nests the resulting tables, indented under each of
  the strata headers.
  ([\#2006](https://github.com/ddsjoberg/gtsummary/issues/2006))

- The
  [`add_ci.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)
  function now works with categorical variables that were summarized
  using `tbl_summary(percent = c('row', 'cell'))`.
  ([\#1929](https://github.com/ddsjoberg/gtsummary/issues/1929))

- Adding the `tbl_merge(merge_vars)` argument. This argument allows
  users to specify any merging columns providing much more flexibility
  when merging unlike tables. Additionally, columns selected by
  [`cards::all_ard_groups()`](https://insightsengineering.github.io/cards/latest-tag/reference/selectors.html)
  have been added to the default merging columns, which provides the
  functionality for merging the results from
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md)
  and
  [`tbl_hierarchical_count()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md).
  ([\#1861](https://github.com/ddsjoberg/gtsummary/issues/1861))

  This does, however, introduce one change in behavior from the previous
  version of
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md).
  Previously, merging on a table with the same variable, but with a
  different label would be reconciled silently in the background and the
  first label would be used in the final table. While this may have been
  useful in a few edge cases, it largely was an unintuitive result. This
  update performs more straightforward merging and the results are more
  aligned with users’ expectations.

- Added
  [`add_variable_group_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_variable_group_header.md)
  function that adds a header row above a group of variables.
  ([\#2150](https://github.com/ddsjoberg/gtsummary/issues/2150))

#### Updates to `modify_*()` Functions

There were a number of updates to the family of `modify_*()` functions:
functions that modify table attributes like headers, footnotes, bold,
titles, etc.

- Adding functions
  [`modify_bold()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md),
  [`modify_italic()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md),
  [`remove_bold()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md),
  and
  [`remove_italic()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_bold_italic.md)
  for adding or remove bold or italic styling to cells in the table.
  ([\#2125](https://github.com/ddsjoberg/gtsummary/issues/2125))

- Updates to the handling of footnotes. Previously, header footnotes
  were handled with
  [`modify_footnote()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated_modify_footnote.md)
  and `modify_table_styling(footnote)`. It was possible to also include
  footnotes in the table body with `modify_table_styling(footnote)`, but
  this was largely a hidden feature. Also confusingly, a special
  abbreviation footnote was handled with
  `modify_footnote(abbreviation=TRUE)`.

  In this update, we now export separate user-facing functions for each
  of these with clearer names and scope:
  [`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
  [`modify_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
  and
  [`modify_abbreviation()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md).
  As the names indicate, the
  [`modify_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)
  and
  [`modify_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)
  functions place footnotes in the header and table body. Abbreviations
  are now treated like source notes and do not have footnote markers
  associated with them. We also export functions
  [`remove_footnote_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
  [`remove_footnote_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
  and
  [`remove_abbreviation()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_abbreviation.md)
  to remove previously assigned footnotes and abbreviations.

  Also, multiple footnotes may now reference the same cell in the table
  or column header by utilizing the
  `modify_footnote_header(replace=FALSE)`,
  `modify_footnote_body(replace=FALSE)` argument.

- Previously, source notes were an undocumented feature and only a
  single source note could be included in a table. We now export
  [`modify_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_source_note.md)
  and
  [`remove_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_source_note.md)
  to add and remove any number of source notes. Also, when merging and
  stacking tables, previously due to the one source note limit, only the
  first source note was retained. Now all source notes will be included
  below the resulting table. *This is different behavior compared to
  previous versions of the package and in rare cases may result in a
  different source note.* Moreover, `kableExtra` output now supports
  source notes, where previously they were omitted.

- The `modify_spanning_header(level)` argument has been added to allow
  for multiple levels of spanning headers in the resulting tables. The
  [`remove_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  function has also been added to ease the removal of spanning headers.
  ([\#2099](https://github.com/ddsjoberg/gtsummary/issues/2099))

- The
  [`modify_footnote_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md)
  function has been added to ease adding footnotes to spanning headers.
  A companion function,
  [`remove_footnote_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_footnote2.md),
  has been added to remove spanning headers.

- Added new function
  [`modify_missing_symbol()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_missing_symbol.md)
  to update how a missing value is displayed in a table.
  ([\#2121](https://github.com/ddsjoberg/gtsummary/issues/2121))

- Added new function
  [`remove_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md)
  to undo a column merge.
  ([\#2130](https://github.com/ddsjoberg/gtsummary/issues/2130))

- The `modify_caption(caption)` argument now accepts a vector of
  captions, instead of just a string. Note, however, that not all print
  engines support a vector of captions.
  ([\#2107](https://github.com/ddsjoberg/gtsummary/issues/2107))

- Added `show_header_names(show_hidden)` argument, which will print both
  hidden and printed column information.
  ([\#2147](https://github.com/ddsjoberg/gtsummary/issues/2147))

#### Other Updates

- Language translations have been updated with a handful of missing
  translations.
  ([\#2100](https://github.com/ddsjoberg/gtsummary/issues/2100))

#### Bug Fixes

- Swapped out
  [`dplyr::rows_update()`](https://dplyr.tidyverse.org/reference/rows.html)
  with a base R implementation in
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  that allows for tables with mixed types in
  `x$table_styling$header$modify_*` columns. For example,
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  has integer Ns and
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  has double Ns that can now be combined.
  ([\#1626](https://github.com/ddsjoberg/gtsummary/issues/1626))

- Corrected the
  [`?tests`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
  documentation file to reflect that, as of v2.0, we no longer perform
  pre-processing (such as, converting a column to a factor) on variables
  before computing tests.
  ([\#2135](https://github.com/ddsjoberg/gtsummary/issues/2135))

- When
  [`add_ci.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)
  was for a variable that was all `NA` we no longer return an error.
  Users will now see the confidence interval as `'NA%, NA%'`.
  ([\#2139](https://github.com/ddsjoberg/gtsummary/issues/2139))

## gtsummary 2.0.4

CRAN release: 2024-11-30

#### New Features and Functions

- Added S3 methods
  [`add_overall.tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  and
  [`add_overall.tbl_hierarchical_count()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md).

- Added the `tidy_wald_test(vcov)` argument to allow for the calculation
  of p-values via alternative variance-covariance structure (e.g. for
  robust SEs).
  ([\#2076](https://github.com/ddsjoberg/gtsummary/issues/2076);
  [@aghaynes](https://github.com/aghaynes))

#### Other Updates

- The
  [`with_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
  has been updated to no longer print theme names when the applied, nor
  when the original theme is re-applied.
  ([\#2031](https://github.com/ddsjoberg/gtsummary/issues/2031))

- Updated the `theme_gtsummary_journal("jama")` theme to apply changes
  to
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  ([\#1964](https://github.com/ddsjoberg/gtsummary/issues/1964);
  [@vjcatharine](https://github.com/vjcatharine))

#### Lifecycle Updates

- Removed `global_pvalue_fun.tidycrr()`, which was previously migrated
  to the {tidycmprsk} package.
  ([\#1997](https://github.com/ddsjoberg/gtsummary/issues/1997);
  [@jwoolfolk](https://github.com/jwoolfolk))

#### Bug Fixes

- Bug fix for footnote markers when placing a footnote for flextable and
  gt tables on multiple columns and rows in the table body.
  ([\#2062](https://github.com/ddsjoberg/gtsummary/issues/2062))

- Fix for setting default formatting functions in
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  Previously, defaults were assigned similarly to those in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
  which led to survey-only statistics being assigned sub-optimal
  defaults.
  ([\#2078](https://github.com/ddsjoberg/gtsummary/issues/2078))

- Bug fix in
  [`add_ci.tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.tbl_svysummary.md)
  for factor variables where order was alphabetical instead of the
  factor levels.
  ([\#2036](https://github.com/ddsjoberg/gtsummary/issues/2036))

- Addressing encoding issue where
  [`sort()`](https://rdrr.io/r/base/sort.html) and
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  sorted differently, and the order of the `by` levels was inconsistent
  in the resulting table.
  ([\#2038](https://github.com/ddsjoberg/gtsummary/issues/2038))

- Users may now pass a vector of integers to
  `tbl_hierarchical*(digits)`, as is possible in other summary
  functions.
  ([\#2080](https://github.com/ddsjoberg/gtsummary/issues/2080))

- The `tbl_summary(statistic)` argument now allows users to pass curly
  brackets that appear in the final table. Just like in
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  double curly brackets are escaped to a single bracket. For example,
  `tbl_summary(statistic=~"{{{mean}}}")` results in `"{<mean value>}"`.
  ([\#2123](https://github.com/ddsjoberg/gtsummary/issues/2123))

## gtsummary 2.0.3

CRAN release: 2024-10-04

#### New Features and Functions

- Added function
  [`tbl_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md),
  [`tbl_hierarchical_count()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_hierarchical.md),
  [`tbl_ard_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_hierarchical.md),
  [`brdg_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/brdg_hierarchical.md),
  and
  [`pier_summary_hierarchical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/brdg_hierarchical.md).
  Consider these functions as a preview. We will be making changes
  without the full deprecation cycle in the coming releases.
  ([\#1872](https://github.com/ddsjoberg/gtsummary/issues/1872))

- Adding the `style_*(prefix, suffix)` and
  `label_style_*(prefix, suffix)` for adding a string before or after
  the formatted results. These arguments have not been added to the
  p-value formatting functions.
  ([\#1690](https://github.com/ddsjoberg/gtsummary/issues/1690))

- Added argument `tbl_ard_summary(overall)`. When `TRUE`, the ARD is
  parsed into primary ARD and the Overall ARD and we run
  `tbl_ard_summary() |> add_overall()`.
  ([\#1940](https://github.com/ddsjoberg/gtsummary/issues/1940))

- Added
  [`add_stat_label.tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat_label.md)
  method. ([\#1969](https://github.com/ddsjoberg/gtsummary/issues/1969))

#### Other Updates

- Headers in {gt} tables being exported to PDF do not support the `\n`
  line breaker. Previously, line breakers were stripped from the header
  in the
  [`print.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/print_gtsummary.md)
  S3 method. But this did not apply to users utilizing
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
  to further customize their tables. As a result, the line breaking
  strip has been migrated to
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md).
  ([\#1960](https://github.com/ddsjoberg/gtsummary/issues/1960))

- Migrated the `tbl_survfit.list(conf.level)` up to
  `tbl_survfit.data.frame(conf.level)` where the confidence level is
  passed to
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

- Update in
  [`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md)
  to better handle non-standard ARDs (i.e. not our typical continuous or
  categorical summaries) by assigning them a default summary type.
  ([\#1991](https://github.com/ddsjoberg/gtsummary/issues/1991))

- Made the [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html)
  available in
  [`add_p.tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_continuous.md).
  ([\#1970](https://github.com/ddsjoberg/gtsummary/issues/1970))

- Removed the deprecated `'aov'` test from the `tests.R` file listing
  available tests.
  ([\#1970](https://github.com/ddsjoberg/gtsummary/issues/1970))

- Removed documentation for the `add_overall.tbl_ard_summary(digits)`
  argument, which was never meant to be a part of this function.
  ([\#1975](https://github.com/ddsjoberg/gtsummary/issues/1975))

#### Bug Fixes

- Bug fix in
  [`add_overall.tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  due to extraneous argument being passed to
  [`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md).
  ([\#2027](https://github.com/ddsjoberg/gtsummary/issues/2027))

- Bug fix in
  [`add_p.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_survfit.md)
  when the original call included `tbl_survfit(type)` specification.
  ([\#2002](https://github.com/ddsjoberg/gtsummary/issues/2002))

- Removed the `"tbl_summary-arg:statistic"` theme that was incorrectly
  added to
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md).

## gtsummary 2.0.2

CRAN release: 2024-09-05

Updates to address regressions in the v2.0.0 release:

- The default `add_glance_*(glance_fun)` function fixed for `mice`
  models with class `'mira'`.
  ([\#1912](https://github.com/ddsjoberg/gtsummary/issues/1912))
- We can again report unweighted statistics in the headers of
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  tables. ([\#1911](https://github.com/ddsjoberg/gtsummary/issues/1911))
- [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  properly handles variables specified in the `include` argument with
  non-syntactic names.
  ([\#1932](https://github.com/ddsjoberg/gtsummary/issues/1932))
- `NA` values can again be specified in `add_stat_label(label)` to
  suppress a statistic label from being placed.
  ([\#1937](https://github.com/ddsjoberg/gtsummary/issues/1937))
- Corrected bug in
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
  where the `digits` argument was not always being passed accurately to
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
  ([\#1943](https://github.com/ddsjoberg/gtsummary/issues/1943))

#### Other updates

- The Themes and Inline Text vignettes have been converted to articles
  (that is, they are no longer bundled with the package, but are still
  available on the website).

- The total N is now returned with `.$cards` using the
  [`cards::ard_total_n()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_total_n.html)
  function for the calculation.

- The default headers for `tbl_ard_*()` functions no longer include
  counts, as these are not required data to be passed along in the ARD
  input.

- The summary statistics of the `'by'` variable are no longer required
  in the ARD for functions
  [`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md)
  and
  [`tbl_ard_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_continuous.md).
  When the tabulation summary statistics are passed, they are available
  to place in the header dynamically.
  ([\#1860](https://github.com/ddsjoberg/gtsummary/issues/1860))

- The
  [`tbl_ard_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_wide_summary.md)
  function no longer requires the results from
  [`cards::ard_attributes()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_attributes.html)
  to create tables.
  ([\#1873](https://github.com/ddsjoberg/gtsummary/issues/1873))

- Added the `label` argument to functions
  [`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md),
  [`tbl_ard_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_wide_summary.md),
  and
  [`tbl_ard_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_continuous.md).
  ([\#1850](https://github.com/ddsjoberg/gtsummary/issues/1850))

- The `add_glance*(glance_fun)` argument’s default value has been
  updated to an S3 generic, allowing bespoke handling for some
  regression classes.
  ([\#1822](https://github.com/ddsjoberg/gtsummary/issues/1822))

- Added
  [`add_overall.tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall_ard.md)
  S3 method.
  ([\#1848](https://github.com/ddsjoberg/gtsummary/issues/1848))

- Added function
  [`tbl_likert()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_likert.md)
  for summarizing ordered categorical (or Likert scales) data as well as
  the associated
  [`add_n.tbl_likert()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n_summary.md)
  S3 method.
  ([\#1660](https://github.com/ddsjoberg/gtsummary/issues/1660))

- Fix where error or warning condition messages containing curly brace
  pairs could not be printed.

- Updated the
  [`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  output to include the values that may be dynamically placed in the
  headers. Additionally, the `include_example` and `quiet` arguments
  have been deprecated.
  ([\#1696](https://github.com/ddsjoberg/gtsummary/issues/1696))

## gtsummary 2.0.1

CRAN release: 2024-08-17

Updates to address regressions in the v2.0.0 release:

- Restore functionality of `inline_text.tbl_summary(column)` argument to
  specify a by level when the by variable is a factor.
  ([\#1883](https://github.com/ddsjoberg/gtsummary/issues/1883))
- Correct the order of the columns when the `tbl_summary(by)` variables
  has ten or more levels.
  ([\#1877](https://github.com/ddsjoberg/gtsummary/issues/1877))
- Re-establishing strong link between header by variable levels and
  those in the table body to ensure correct ordering of columns in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
- The `tbl_survfit(times)` argument accepts integers once again.
  ([\#1867](https://github.com/ddsjoberg/gtsummary/issues/1867))
- Fix in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  for the `formula` argument when it includes a hard-coded column name,
  e.g. `formula='{y} ~ {x} + grade'`. The hard-coded variable name is
  now removed from the `include` argument.
  ([\#1886](https://github.com/ddsjoberg/gtsummary/issues/1886))
- Fix for non-Base R classes tabulated with
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  that would not coerce to character correctly after
  [`unlist()`](https://rdrr.io/r/base/unlist.html).
  ([\#1893](https://github.com/ddsjoberg/gtsummary/issues/1893))
- Updated the styling function from
  [`style_percent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_percent.md)
  to `style_number(scale=100)` when user passes an integer to change the
  rounding of percentages in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
  ([\#1899](https://github.com/ddsjoberg/gtsummary/issues/1899))

#### Other updates

- The {tidycmprsk} dependency has been removed and the
  `tbl_regression.tidycrr()` method has been migrated to the
  {tidycmprsk} package.
  ([\#1865](https://github.com/ddsjoberg/gtsummary/issues/1865))

- The class of
  [`tbl_split()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated.md)
  objects has been updated from `"tbl_split"` to
  `c("tbl_split", "list")`.
  ([\#1854](https://github.com/ddsjoberg/gtsummary/issues/1854))

- Updated the default value of `tbl_ard_summary(missing)` to `"no"`.
  ([\#1857](https://github.com/ddsjoberg/gtsummary/issues/1857))

- Line breaks (i.e. `'\n'`) are now auto-stripped from gt-rendered
  tables when in an R markdown or Quarto environment.
  ([\#1896](https://github.com/ddsjoberg/gtsummary/issues/1896))

## gtsummary 2.0.0

CRAN release: 2024-07-23

#### New Features

- Clearer error messages have been introduced throughout the package.
  We’ve adopted {cli} for all our messaging to users. Our goal was to
  return a clear message to users for all scenarios.

- Added functions
  [`tbl_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_wide_summary.md)
  and
  [`tbl_ard_wide_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_wide_summary.md)
  for simple summaries across multiple columns.

- The {gt} package is now the default printer for all Quarto and R
  markdown output formats.

  - Previously, when printing a gtsummary table in a Quarto or R
    markdown document, we would detect the output format and convert to
    gt, flextable, or kable to provide the best-looking table. The {gt}
    package has matured and provides lovely tables for nearly all output
    types, and we have now made {gt} the default table drawing tool for
    all gtsummary tables. These output types are still supported.

- Previously, if I wanted a single statistic to be reported to
  additional levels of precision in a
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  table, I would need to specify the precision of every summary
  statistic for a variable. Now, we can simple update the one statistic
  we’re interested in with a named list of vector:
  `tbl_summary(digits = age ~ list(sd = 2))`.

- New functions
  [`tbl_ard_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_summary.md)
  and
  [`tbl_ard_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_ard_continuous.md)
  have been added. These provide general tools for creating bespoke
  summary tables. Rather than accepting a data frame, these functions
  accept an ARD object (Analysis Results Dataset often created with the
  {cards} or {cardx} packages). The ARD objects align with the emerging
  [CDISC Analysis Results
  Standard](https://www.cdisc.org/standards/foundational/analysis-results-standard).
  ARDs are now used throughout the package. See below under the
  “Internal Storage” heading.

- The default `add_global_p(anova_fun)` argument value has been updated
  to
  [`global_pvalue_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/global_pvalue_fun.md),
  which is an S3 generic. The default method still calls
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) for the
  calculation. Methods for `tidycmprsk::crr()` and
  [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html)
  have been added that wrap
  [`aod::wald.test()`](https://rdrr.io/pkg/aod/man/wald.test.html) as
  these regression model types are not supported by
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html).

- The
  [`add_ci.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)
  S3 method has been updated with new ways to calculate the confidence
  interval: Wald with and without continuity correction, Agresti-Coull,
  and Jeffreys.

- Added a family of function `label_style_*()` that are similar to the
  `style_*()` except they return a styling *function*, rather than a
  styled value.

- Functions
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  and
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  have gained the `missing_stat` argument, which gives users great
  control over the statistics presented in the missing row of a summary
  table.

#### Internal Storage

- Greater consistency has been put in place for all calculated
  statistics in gtsummary. Previously, each function handled its own
  calculations and transforming these statistics into data frames that
  would be printed. Now each function will first prepare an Analysis
  Result Dataset (ARD), and ARDs are converted to gtsummary structures
  using bridge functions (prefixed with `brdg_*()`). The bridge
  functions will be exported to allow anyone to more easily extend
  gtsummary functions.

  - These ARDs are now used to calculate the summary statistics for
    nearly every function in gtsummary. The raw summary statistics are
    saved in `.$cards`.
  - *Users who previously accessed the internals of a gtsummary object
    will find the structure has been updated, and this may be an
    important breaking change.*

- Calculations that require other packages have been placed in another
  package called {cardx}. This package creates ARD objects with the
  calculated statistics.

- In
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md),
  the `.$model_obj` is no longer returned with the object. The modeling
  object is, and always has been, available in `.$inputs$x`.

- When the gtsummary package was first written, the gt package was not
  on CRAN and the version of the package that was available did not have
  the ability to merge columns. Due to these limitations, the `"ci"`
  column was added to show the combined `"conf.low"` and `"conf.high"`
  columns. Column merging in both gt and gtsummary packages has matured
  over the years, and we are now adopting a more modern approach by
  using these features. As a result, the `"ci"` column will eventually
  be dropped from `.$table_body`. By using column merging, the conf.low
  and conf.high remain numeric and we can to continue to update how
  these columns are formatted. Review
  [`?deprecated_ci_column`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated_ci_column.md)
  for details.

#### Documentation

- The vignettes “FAQ+Gallery”,
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  Tutorial,
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  Tutorial, and Quarto+R Markdown have been converted to articles. The
  URLs on the website have not changed for these pages, but the
  vignettes are no longer is bundled in the package. This change allows
  us to provide better documentation, utilizing more tools that don’t
  need to be included in the package.

#### Minor Improvements

- Argument `add_p.tbl_summary(adj.vars)` was added to more easily add
  p-values that are adjusted/stratified by other columns in a data
  frame.

- Messaging and checks have been improved when tidyselect is invoked in
  the package, i.e. when the tilda is used to select variables
  `age ~ "Patient Age"`. The subset of variables that can be selected is
  now reduced the variables present in the table. For example, if you
  have a summary table of patient age (and only patient age), and age is
  a single column from a data set of many columns and you mis-spell age
  (`aggge ~ "Patient Age"`), the error message will now ask if you meant
  `"age"` instead of listing every column in the data set.

  - Note that as before, you can circumvent tidyselect by using a named
    list, e.g. `list(age = "Patient Age")`.

- Added the following methods for calculating differences in
  [`add_difference.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.tbl_summary.md):
  Hedge’s G, Paired data Cohen’s D, and Paired data Hedge’s G. All three
  are powered by the {effectsize} package.

- The counts in the header of `tbl_summary(by)` tables now appear on a
  new line, e.g. `"**{level}** \nN = {n}"`.

- In
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
  the default calculation for quantiles (e.g. statistics of the form
  `"p25"` or `"p75"`) has been updated with type `quantile(type=2)`.

- In
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
  dates and times showed the minimum and maximum values only by default.
  They are now treated as all other continuous summaries and share their
  default statistics of the median and IQR.

- Previously, indentation was handled with
  `modify_table_styling(text_format = c("indent", "indent2"))`, which
  would indent a cell 4 and 8 spaces, respectively. Handling of
  indentation has been migrated to
  `modify_table_styling(indent = integer())`, and by default, the label
  column is indented to zero spaces. This makes it easier to indent a
  group of rows.

- The inputs for `modify_table_styling(undo_text_format)` has been
  updated to mirror its counterpart `modify_table_styling(text_format)`
  and no longer accepts `TRUE` or `FALSE`.

- The values passed in `tbl_summary(value)` are now only checked for
  columns that are summary type `"dichotomous"`.

- The gtsummary selecting functions,
  e.g. [`all_categorical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  etc., are now simplified by wrapping
  [`tidyselect::where()`](https://tidyselect.r-lib.org/reference/where.html),
  which not available when these functions were originally written.
  Previously, these functions would error if used out of context; they
  now, instead,select no columns when used out-of-context.

- The design-based t-test has been added as possible methods for
  [`add_difference.tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.tbl_svysummary.md)
  and is now the default for continuous variables.

- When
  [`add_ci()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)
  is run after
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md),
  the overall column is now populated with the confidence interval.
  ([\#1569](https://github.com/ddsjoberg/gtsummary/issues/1569))

- Added
  [`pkgdown_print.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/print_gtsummary.md)
  method that is only registered when the pkgdown package is loaded.
  This enables printing of gtsummary tables on the pkgdown site in the
  Examples section.
  ([\#1771](https://github.com/ddsjoberg/gtsummary/issues/1771))

- The package now uses updated
  [`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
  function to calculate quantiles, which was introduced in survey v4.1

#### Bug fixes

- Fix in
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  for paired t-tests. Previously, the sign of the reported difference
  depended on which group appeared first in the source data. Function
  has been updated to consistently report the difference as the first
  group mean minus the second group mean.
  ([\#1557](https://github.com/ddsjoberg/gtsummary/issues/1557))

#### Lifecycle changes

- A couple of small changes to the default summary type in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  have been made.

  - If a column is all `NA_character_` in
    [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
    the default summary type is now `"continuous"`, where previously it
    was `"dichotomous"`.
  - Previously, in a
    [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
    variables that were `c(0, 1)`, `c("no", "yes")`, `c("No", "Yes")`,
    and `c("NO", "YES")` would default to a dichotomous summary with the
    `1` and `yes` level being shown in the table. This would occur even
    in the case when, for example, only `0` was observed. In this
    release, the line shown for dichotomous variables must be observed
    OR the unobserved level must be either explicitly defined in a
    factor or be a logical vector. This means that a character vector of
    all `"yes"` or all `"no"` values will default to a categorical
    summary instead of dichotomous.

- When using the `tbl_summary(value)` argument, we no longer allow
  unobserved levels to be used unless it is an unobserved factor level
  or logical level.

- The `quiet` argument has been deprecated throughout the package,
  except in
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md).
  Documentation has been updated to ensure clarity in all methods.

- The `inline_text(level)` argument now expects a character value.

- The `tbl_butcher(include)` argument now only accepts character
  vectors.

- The following theme elements have been deprecated:

  - These theme elements will eventually be removed from the package:
    `'tbl_summary-arg:label'`, `'add_p.tbl_summary-arg:pvalue_fun'`,
    `'tbl_regression-arg:pvalue_fun'`,
    `'tbl_regression-chr:tidy_columns'`.
    - The `pvalue_fun` elements should switch to the package-wide theme
      for p-value styling–`'pkgwide-fn:pvalue_fun'`.
  - These theme elements have been removed from the package immediately
    due to structural changes within the package:
    `'tbl_summary-str:continuous_stat'`,
    `'tbl_summary-str:categorical_stat'`.
    - The default statistics can still be modified with
      `'tbl_summary-arg:statistic'`

- The `add_p(test = ~'aov')` test is now deprecated as identical results
  can be obtained with
  `add_p(test = ~'oneway.test', test.args = ~list(var.equal = TRUE))`.

- Previously,
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  would coerce various data types to classes compatible with some base R
  tests. For example, we would convert `difftime` classes to general
  numeric before passing to
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html). We have
  eliminated type- and class-specific handling in these functions and it
  is now left to the the user pass data compatible with the functions
  that calculate the p-values or to create a custom test that wraps
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) and
  performs the conversion. This change is effective immediately.

- Arguments `modify_header(update)`, `modify_footnote(update)`,
  `modify_spanning_header(update)`, and `modify_fmt_fun(update)` have
  been deprecated. Use dynamic dots instead, e.g. `modify_header(...)`,
  which has been the preferred method for passing updates for a few
  years.

- Function
  [`continuous_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/continuous_summary.md)
  has been deprecated immediately. Apologies for the inconvenience of
  the immediate deprecation. The way the function originally worked is
  not compatible with the updated internal structures. In most cases,
  users can use the
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
  function instead.

- Arguments `add_stat(fmt_fun, header, footnote, new_col_name)` have
  been deprecated since v1.4.0 (2021-04-13). They have now been fully
  removed from the package.

- Global options have been deprecated in gtsummary since v1.3.1
  (2020-06-02). They have now been fully removed from the package.

- The `modify_header(stat_by)` argument was deprecated in v1.3.6
  (2021-01-08), and has now been fully removed from the package.

- Use of the [`vars()`](https://dplyr.tidyverse.org/reference/vars.html)
  selector was first removed in v1.2.5 (2020-02-11), and the messaging
  about the deprecation was kicked up in June 2022. This use is now
  defunct and the function will soon no longer be exported.

- The `as_flextable()` function was deprecated in v1.3.3 (2020-08-11),
  and has now been fully removed from the package.

- Custom selectors `all_numeric()`, `all_character()`, `all_integer()`,
  `all_double()`, `all_logical()`, `all_factor()` functions were
  deprecated in v1.3.6 (2021-01-08), and has now been fully removed from
  the package. These functions were added before the
  [`tidyselect::where()`](https://tidyselect.r-lib.org/reference/where.html)
  function was released, which is a replacement for all these functions.

- The `modify_cols_merge()` functions was renamed to
  [`modify_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md)
  to match the other function names in v1.6.1 (2022-06-22). The
  deprecation has been upgraded from a warning to an error.

- There is a change in the `theme_gtsummary_journal("qjecon")` theme for
  gt output. The journal prefers to present regression coefficients
  above their standard errors. To achieve this placement in gt table, we
  were taking advantage of a bug or feature (depending on your point of
  view) that allowed this placement when a gt table was output to HTML
  and HTML only. The gt package is now working on a proper solution for
  linebreaks within a cell, and until that feature is active, we are not
  using our hack. There is no change for this theme for the other
  tabling engine packages.

## gtsummary 1.7.2

CRAN release: 2023-07-15

- Removed messaging about the former auto-removal of the
  `tbl_summary(group)` variable from the table: a change that occurred
  3+ years ago in gtsummary v1.3.1

- Fix in
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)
  where source notes were not accurately rendered.
  ([\#1520](https://github.com/ddsjoberg/gtsummary/issues/1520))

- Fix in column order when
  [`add_ci()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)
  is run after `add_overall(last=TRUE)`. Previously, the overall columns
  were placed in front.
  ([\#1525](https://github.com/ddsjoberg/gtsummary/issues/1525))

- Line breaks (i.e. `\n`) are now removed from column headers and table
  cells when
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)
  is called.
  ([\#1526](https://github.com/ddsjoberg/gtsummary/issues/1526))

- Fix in
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
  where columns with common spanning headers were gathered. Corrected
  with `gt::tab_spanner(gather = FALSE)`.
  ([\#1527](https://github.com/ddsjoberg/gtsummary/issues/1527))

- Fix in
  [`remove_row_type()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/remove_row_type.md)
  where header rows for `continuous2` type variables was not removed
  when requested.
  ([\#1507](https://github.com/ddsjoberg/gtsummary/issues/1507))

- Fix where some default
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  categorical tests were chi-squared when it should have been Fisher’s
  exact test. This misclassification occurred in some cases when there
  was a large differential in the missing pattern for one of the
  variables in the cross table.
  ([\#1513](https://github.com/ddsjoberg/gtsummary/issues/1513))

- Fix in `add_overall(col_label=)` where specified label was not always
  placed. ([\#1505](https://github.com/ddsjoberg/gtsummary/issues/1505))

## gtsummary 1.7.1

CRAN release: 2023-04-27

#### New Functions

- Added [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  S3 method for gtsummary class.

#### New Functionality

- The
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  function may now report the design effect,
  e.g. `tbl_svysummary(statistic = ~"{deff}")`.
  ([\#1486](https://github.com/ddsjoberg/gtsummary/issues/1486))

- Added French translations for new marginal effects tidiers housed in
  {broom.helpers}.
  ([\#1417](https://github.com/ddsjoberg/gtsummary/issues/1417))

- Added theme elements to control the default headers in
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  ([\#1452](https://github.com/ddsjoberg/gtsummary/issues/1452))

- Improved error messaging in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  when `method=` argument is not correctly specified.
  ([\#1469](https://github.com/ddsjoberg/gtsummary/issues/1469))

- Updates to account for changes in {forcats} v1.0.0 and {dplyr} v1.1.0.

- [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  can now report design effects
  ([\#1486](https://github.com/ddsjoberg/gtsummary/issues/1486))

#### Bug Fixes

- Fix in the footnote of
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  when run after
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md).
  ([\#1436](https://github.com/ddsjoberg/gtsummary/issues/1436))

- Updating the levels of precision used in `round2()`, which is used in
  the background for every rounded/formatted number in a gtsummary
  table. ([\#1494](https://github.com/ddsjoberg/gtsummary/issues/1494))

- Bug fix when a subset of CIs are requested in `add_ci(include=)`.
  ([\#1484](https://github.com/ddsjoberg/gtsummary/issues/1484))

- Update in
  [`as_hux_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)
  to ensure the Ns in header are not incorrectly auto-formatted by
  {huxtable}.

- Fix in the `style_*()` family of functions. The attributes of the
  input vector–excluding the class–are retained.
  ([\#1460](https://github.com/ddsjoberg/gtsummary/issues/1460))

- Updated
  [`style_ratio()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_ratio.md)
  to now format negative values.

- Bug fix in
  [`add_ci.tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.tbl_svysummary.md)
  for dichotomous variables.

- [`add_ci.tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.tbl_svysummary.md)
  now takes properly into account the `percent` argument
  ([\#1470](https://github.com/ddsjoberg/gtsummary/issues/1470))

## gtsummary 1.7.0

CRAN release: 2023-01-13

#### Breaking Changes

- Updated the default argument values in
  `tidy_robust(vcov=NULL, vcov_args=NULL)`. Users must specify the type
  of robust standard errors using these arguments.

- Fully removed deprecated items that were originally deprecated in
  v1.2.5 (released 3 years ago).

  - `add_p(exclude=)`, `as_gt(exclude=)`, `as_kable(exclude=)`,
    `as_tibble.gtsummary(exclude=)`, `tbl_regression(exclude=)`,
    `tbl_uvregression(exclude=)`
  - `tbl_summary_()`,`add_p_()`
  - `add_global_p(terms=)`

#### New Functions

- New function
  [`add_ci.tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.tbl_svysummary.md)
  for adding confidence intervals to
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  summary statistics.
  ([\#965](https://github.com/ddsjoberg/gtsummary/issues/965))

#### New Functionality

- Arguments pass via the dots in `tbl_uvregression(...)` are now passed
  to `broom.helpers::tidy_plus_plus(...)`.
  ([\#1396](https://github.com/ddsjoberg/gtsummary/issues/1396))

- Added new theme elements to control the default headers in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
  ([\#1401](https://github.com/ddsjoberg/gtsummary/issues/1401))

- All examples that previously used `<br>` for line breaks in gt tables
  have been updated to use `\n`. Additionally, the `"qjecon"` journal
  theme has been updated to use the updated line breaker as well.
  ([\#1311](https://github.com/ddsjoberg/gtsummary/issues/1311))

- Now allowing for mixed-class numeric types in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
  such that
  [`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md)
  will not throw an error when the pattern argument is specified.

- Added [`stats::mood.test()`](https://rdrr.io/r/stats/mood.test.html)
  to
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md).
  ([\#1397](https://github.com/ddsjoberg/gtsummary/issues/1397))

#### New Documentation

- Added a new article illustrating how to place gtsummary tables into
  Shiny applications.
  ([\#1335](https://github.com/ddsjoberg/gtsummary/issues/1335))

#### Bug Fixes

- Updated Lancet journal theme to report p-values with more precision,
  per the journal’s reporting guidelines.
  ([\#1442](https://github.com/ddsjoberg/gtsummary/issues/1442))

- Fix for
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)
  when the header is blank.
  ([\#1406](https://github.com/ddsjoberg/gtsummary/issues/1406))

- Fix in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  that now allows for column vectors to be named within a data frame.
  ([\#1403](https://github.com/ddsjoberg/gtsummary/issues/1403))

## gtsummary 1.6.3

CRAN release: 2022-12-06

- The
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)
  function now recognizes markdown bold (`**`) and italic (`_`) syntax
  in the headers and spanning headers. Restrictions apply. See help file
  for details. Users can no longer place sets of double stars and
  underscores *without* the text being formatted as markdown syntax.
  ([\#1361](https://github.com/ddsjoberg/gtsummary/issues/1361))

- The
  [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)
  function now works with tables created with `gtreg::tbl_listing()`
  that do not contain a column named `"label"`.
  ([\#1358](https://github.com/ddsjoberg/gtsummary/issues/1358))

- Functions
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  and
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  now support `"{n}"`, `"{p}"`, and `"{level}"` when no `by=` variable
  is present for use in functions like
  [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md).
  For example, the following previously invalid code works well for both
  the overall column and the stratified columns:

  ``` r
  trial %>%
    tbl_summary(by = trt) %>%
    add_overall() %>%
    modify_header(all_stat_cols() ~ "**{level}**, N = {n}")
  ```

  For the survey summary, the unweighted variants are also available.
  ([\#1366](https://github.com/ddsjoberg/gtsummary/issues/1366))

- Added experimental feature where additional arguments can be passed to
  [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html)
  via `tbl_regression(...)`.
  ([\#1383](https://github.com/ddsjoberg/gtsummary/issues/1383))

- Updated the arguments in
  [`tidy_robust()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md)
  to account for updates in {parameters}.
  ([\#1376](https://github.com/ddsjoberg/gtsummary/issues/1376))

- Allowing for ‘survfit’ objects of class `survfit2` in
  [`add_nevent.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_nevent.tbl_survfit.md).
  ([\#1389](https://github.com/ddsjoberg/gtsummary/issues/1389))

- Added [`oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) test
  to
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md).
  ([\#1382](https://github.com/ddsjoberg/gtsummary/issues/1382))

- Now using {ggstats} to plot regression model coefficients via
  [`plot()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/plot.md)
  instead of {GGally}.
  ([\#1367](https://github.com/ddsjoberg/gtsummary/issues/1367))

- Bug fix in
  [`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md):
  the full dataset (including missing observations) is now properly
  passed as `full_data`
  ([\#1388](https://github.com/ddsjoberg/gtsummary/issues/1388))

## gtsummary 1.6.2

CRAN release: 2022-09-30

- The following updates were made to the indentation implementation for
  gt output:

  - Previously, only HTML output was able to indent for gt tables, and
    this was implemented via
    [`gt::tab_style()`](https://gt.rstudio.com/reference/tab_style.html).
  - Indentation is now available for HTML, PDF, and Word and is
    implemented by adding unicode non-breaking spaces to the data frame
    via
    [`gt::text_transform()`](https://gt.rstudio.com/reference/text_transform.html).
  - The “names” for the indentation calls have been updated to
    `"indent"` and `"indent2"`. This change should affect very very few
    users. If you’re not sure what the names refer to, then this does
    not affect you.
  - Indentation for RTF does not currently work. Instead of indented
    columns, irregular unicode characters are shown. This issue will be
    addressed in a future gt release. If you do use RTF output, and
    would like your output to be identical to what it was before this
    update, use `as_gt(include = -indent)`.

- A link to the cheat sheet has been added to the website’s navigation
  bar.

- Added additional options to
  `remove_row_type(type = c("level", "all"))`.

  - Use `type = "all"` to remove all rows associated with the
    variable(s) specified in `remove_row_type(variables=)`.
  - Use `type = "level"` in conjunction with new argument
    `level_values=` to remove specified levels for a variable, or do not
    use the new argument to remove all levels for categorical variables.

- Added the standard error of means to the list of available statistics
  for continuous data summaries in
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  ([\#1291](https://github.com/ddsjoberg/gtsummary/issues/1291))

- Added Dutch language translations.
  ([\#1302](https://github.com/ddsjoberg/gtsummary/issues/1302))

- Updated
  [`add_significance_stars()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_significance_stars.md)
  to accept any gtsummary table (instead of only regression model
  summaries) and to work with
  [`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  ([\#1320](https://github.com/ddsjoberg/gtsummary/issues/1320))

- Added the `"var_type"` hidden column to the output of
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md).
  This addition ensures the table will work with
  [`remove_row_type()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/remove_row_type.md).
  ([\#1343](https://github.com/ddsjoberg/gtsummary/issues/1343))

- Updated calls to [`round()`](https://rdrr.io/r/base/Round.html) in the
  `style_*()` functions to `round2()`, which implements classic rounding
  rules. ([\#1304](https://github.com/ddsjoberg/gtsummary/issues/1304))

- Fixed bug in
  [`style_sigfig()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_sigfig.md)
  with numbers close to the thresholds.
  ([\#1298](https://github.com/ddsjoberg/gtsummary/issues/1298))

- Fixed bug when a column named `"variable"` was passed to
  `tbl_custom_summary(by=)`, which resulted in an error.
  ([\#1285](https://github.com/ddsjoberg/gtsummary/issues/1285))

- Bug fix in `as_tibble(fmt_missing = TRUE)`. Previously, missing
  assignments applied to more than one row were being ignored.
  ([\#1327](https://github.com/ddsjoberg/gtsummary/issues/1327))

- Bug fix in column alignment with
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  for
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)
  output. ([\#1326](https://github.com/ddsjoberg/gtsummary/issues/1326))

## gtsummary 1.6.1

CRAN release: 2022-06-22

#### New Functionality

- Added the standard error of proportions to the list of available
  statistics for categorical data summaries in
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  ([\#1187](https://github.com/ddsjoberg/gtsummary/issues/1187))

- Added Tarone-Ware test to
  [`add_p.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_survfit.md)
  ([\#732](https://github.com/ddsjoberg/gtsummary/issues/732))

- Updated
  [`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  to handle
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  objects where users specified the `x=` argument (when `y=` argument is
  more common).
  ([\#1260](https://github.com/ddsjoberg/gtsummary/issues/1260))

#### Other Updates

- Updated start-up messaging.
  ([\#1228](https://github.com/ddsjoberg/gtsummary/issues/1228))

- The `paired.wilcox.test` available in
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  and
  [`add_difference.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.tbl_summary.md)
  was mistakenly marked as returning a difference, but it does not. The
  documentation has been corrected, which results in improved messaging
  to the user when the test is selected in
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md).
  ([\#1279](https://github.com/ddsjoberg/gtsummary/issues/1279))

- Improved error messages for paired tests in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  and
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  when `group=` argument is not specified.
  ([\#1273](https://github.com/ddsjoberg/gtsummary/issues/1273))

- Added argument `with_gtsummary_theme(msg_ignored_elements=)` argument.
  Use this argument to message users if any theme elements will be
  overwritten and therefore ignored inside the
  [`with_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
  call. ([\#1266](https://github.com/ddsjoberg/gtsummary/issues/1266))

- Swapped
  [`gt::fmt_missing()`](https://gt.rstudio.com/reference/fmt_missing.html)
  for
  [`gt::sub_missing()`](https://gt.rstudio.com/reference/sub_missing.html)
  as the former is now deprecated.
  ([\#1257](https://github.com/ddsjoberg/gtsummary/issues/1257))

- The checks for `"haven_labelled"` class are now only performed for the
  variables indicated in `include=` and `by=` in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  and
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  The checks in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  and
  [`tbl_survfit.data.frame()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
  are only applied to the variables in `include=`, e.g. no checking for
  the outcome variable(s).

- Updates to labels and default formatting functions of unweighted
  statistics presented in
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  ([\#1253](https://github.com/ddsjoberg/gtsummary/issues/1253))

- Adding additional structural checks in
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  and
  [`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md)
  to provide better error messaging.
  ([\#1248](https://github.com/ddsjoberg/gtsummary/issues/1248))

- Added
  [`tbl_regression.crr()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression_methods.md)
  method with messaging recommending use of `tidycmprsk::crr()` instead.
  ([\#1237](https://github.com/ddsjoberg/gtsummary/issues/1237))

- The experimental support for `ftExtra::colformat_md()` in
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)
  has been removed. The function requires evaluated YAML paths and does
  not allow un-evaluated references like
  `` bibliography:: "`r here::here()`" ``.
  ([\#1229](https://github.com/ddsjoberg/gtsummary/issues/1229))

- Update for `tbl_summary(by=)` that now allows for a column named
  `"variable"` to be passed.
  ([\#1234](https://github.com/ddsjoberg/gtsummary/issues/1234))

- Added theme element to control what missing statistic is shown in
  summary tables with options to display number or percent missing or
  non-missing or total number of observations.
  ([\#1224](https://github.com/ddsjoberg/gtsummary/issues/1224))

- Renamed `modify_cols_merge()` to
  [`modify_column_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_merge.md)
  to be inline with the other `modify_column_*()` functions.

#### Bug Fixes

- Fix in
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)
  when output format is `'latex'` where a cell that had been bold or
  italicized had special characters double-escaped. Added a condition
  not to escape special characters in these styled cells.
  ([\#1230](https://github.com/ddsjoberg/gtsummary/issues/1230))

- Fix in
  [`with_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md).
  The function restored any previously set theme, but inadvertently
  included the temporary theme along with it.

## gtsummary 1.6.0

CRAN release: 2022-04-25

#### Improvements to `as_kable_extra()`

- For users who used the default kableExtra print without
  output-specific formatting, there are no breaking changes…the only
  changes will be improved output styling.
- The biggest user-facing change is that the default results for **LaTeX
  output** are now greatly improved compared to previous releases, when
  `escape=FALSE` (the new default).
  - Markdown bold, italic, and underline syntax in the headers, spanning
    headers, caption, and footnote will be converted to escaped LaTeX
    code.
  - Special LaTeX characters in the body of the table will be escaped
    with
    [`.escape_latex()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/kableExtra_utils.md)
    or
    [`.escape_latex2()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/kableExtra_utils.md),
    e.g. `%` will be updated to `\\%`, and rendered as `%` in the PDF
    document.
  - The “” symbol will be recognized as a line break in the table
    headers, spanning headers, caption, and the table body.
  - `\n` is removed from footnotes
  - The `escape=` argument is now passed to
    `kableExtra::add_header_row()` and
    [`kableExtra::footnote()`](https://rdrr.io/pkg/kableExtra/man/footnote.html)
    as well (previously, was only passed to
    [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)).
- The `as_kable_extra(escape=, format=)` arguments have been made
  explicit, where previously, these arguments were passed via `...`.
- *Breaking Change* The default value of `escape=` is now `FALSE`. If
  users previously used `as_kable_extra(escape=FALSE)` and had manually
  escaped LaTeX/HTML characters in the body of the table, these
  characters will now be double escaped. To print the table without the
  auto-escaping that is now present, utilize the new argument
  `as_kable_extra(addtl_fmt=FALSE)`
- *Breaking Change* The `fmt_missing=` argument was added to
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md),
  and
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)
  in the last release. This argument is now deprecated in
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)
  and
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md).
  If a user does not want missing values formatted, they can exclude
  these commands with the `include=` argument.
- *Breaking Change* The `strip_md_bold=` has been deprecated. The
  markdown syntax will automatically be stripped from headers, unless
  `escape = FALSE` and `format = "latex"`. In that case, the markdown
  syntax will be converted to LaTeX commands.
- **HTML Updates**
  - The default markdown syntax in the headers and spanning headers is
    removed.
  - Special characters in the table body, headers, spanning headers,
    caption, and footnote will be escaped with
    [`.escape_html()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/kableExtra_utils.md).
  - The `"\n"` symbol is removed from the footnotes

#### Improvements to `as_flex_table()`

- Added support for markdown syntax in {flextable} header rows by
  utilizing the {ftExtra} package. If this package is installed and
  `options(gtsummary.use_ftExtra = TRUE)` has been set (or the
  equivalent theme element), the bold/italic markdown syntax found in
  the headers will be styled. Otherwise, the markdown syntax is stripped
  from the header rows.
  ([\#1200](https://github.com/ddsjoberg/gtsummary/issues/1200))

#### New Functions

- New function
  [`as_hux_xlsx()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)
  added to export a formatted {gtsummary} table directly to Excel.

- Added a `tbl_regression.tidycrr()` method to summarize competing risks
  regression models.
  ([\#1169](https://github.com/ddsjoberg/gtsummary/issues/1169))

- Added tidier
  [`tidy_wald_test()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md),
  a generic function that can calculate Wald test statistics for groups
  of variables in a model. The tidier expects the model object is
  supported by both [`vcov()`](https://rdrr.io/r/stats/vcov.html) and
  [`coef()`](https://rdrr.io/r/stats/coef.html) to obtain the
  variance-covariance matrix and the coef vector.

- Adding functions
  [`get_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
  and
  [`with_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
  for extracting the current gtsummary theme and running code with a
  temporarily theme.

- Added new function
  [`modify_column_indent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated.md)–a
  wrapper for
  [`modify_table_styling()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_styling.md)–to
  make it easier to add and remove indentation in a table.

- Now exporting a function primarily used internally as a helper for
  converting a gtsummary table to gt (and other formats):
  [`.table_styling_expr_to_row_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/dot-table_styling_expr_to_row_number.md).

- Theme helper function
  [`check_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
  has been added. The function takes a gtsummary theme list as the
  input, and runs various consistency checks. Useful when constructing a
  personalized theme. This function replaces the internal checks that a
  passed theme element is indeed a valid theme element.

#### New Functionality

- Functions
  [`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  [`bold_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  [`italicize_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  [`italicize_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)
  are now method functions so they can work better on
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
  objects.

- Total overhaul to the way statistics are saved and reported in
  [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md),
  [`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md),
  [`modify_footnote()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated_modify_footnote.md),
  and
  [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md).
  There is now a standardized place to these statistics to be saved in
  all gtsummary tables (in `x$table_styling$header` in columns starting
  with `"modify_stat_"`). The modify functions have been updated to
  access the statistics from the header data frame. An added benefit to
  this structure, is that the statistics are available after tables are
  merged and stacked. Statistics available in
  [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)
  are taken from the `"label"` column.
  ([\#1165](https://github.com/ddsjoberg/gtsummary/issues/1165),
  [\#1101](https://github.com/ddsjoberg/gtsummary/issues/1101))

- Added `add_global_p(anova_fun=)` argument allowing users to pass
  custom functions to calculate global p-values when
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) does not
  support the model type.
  ([\#1149](https://github.com/ddsjoberg/gtsummary/issues/1149))

- Functions
  [`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  [`bold_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  [`italicize_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  and
  [`italicize_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md)
  now bold/italicize the first column shown in the table. Previously,
  the `"label"` column (which is most often the first shown column) was
  styled.

- Added theme element to pass arguments to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) in
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)
  and
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md).

- Updated the default function in `add_glance_*(glance_fun=)` for MICE
  models. ([\#1137](https://github.com/ddsjoberg/gtsummary/issues/1137))

- Add `tbl_butcher(keep=)` argument to optionally keep some internal
  objects as needed.
  ([\#1148](https://github.com/ddsjoberg/gtsummary/issues/1148))

- Adding Norwegian translations
  ([\#1143](https://github.com/ddsjoberg/gtsummary/issues/1143))

#### Other Updates

- Removed use of [`round()`](https://rdrr.io/r/base/Round.html) in
  [`style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_number.md),
  and replaced it with a round function that does *not* “round-to-even”.
  ([\#1140](https://github.com/ddsjoberg/gtsummary/issues/1140))

- Converted the Table Gallery vignette into an FAQ+Gallery
  ([\#811](https://github.com/ddsjoberg/gtsummary/issues/811))

- Added error messaging if user tries to run
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  or
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  twice. ([\#1209](https://github.com/ddsjoberg/gtsummary/issues/1209))

- All models CIs were labelled as a Confidence Interval. Now Bayesian
  models will correctly label the Credible Interval.
  ([\#1196](https://github.com/ddsjoberg/gtsummary/issues/1196))

- Improved error messaging the functions
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md),
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md),
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md),
  [`as_hux_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)
  when an object that is not class ‘gtsummary’ is passed.
  ([\#1188](https://github.com/ddsjoberg/gtsummary/issues/1188))

- Deprecated the `as_huxtable(strip_md_bold=)` as {huxtable} now
  recognizes the markdown syntax and there is no reason to remove the
  markdown syntax.

- Added
  [`huxtable::set_header_rows()`](https://hughjonesd.github.io/huxtable/reference/header_cols.html)
  to the
  [`as_hux_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)
  stack.

- Improved error messaging in `modify_*()` functions.
  ([\#914](https://github.com/ddsjoberg/gtsummary/issues/914))

- Added
  [`style_percent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_percent.md)
  as the default formatting function for un-weighted proportions.
  ([\#1181](https://github.com/ddsjoberg/gtsummary/issues/1181))

- The global options previously available have now been soft deprecated.
  All documentation of the global options was removed in v1.3.1.
  ([\#1085](https://github.com/ddsjoberg/gtsummary/issues/1085))

- The `as_flextable()` function has been upgraded from a soft to a hard
  deprecation; use
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)
  instead.

- No longer removing the survey design columns from survey objects from
  the columns that will be summarized in
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  and those summarized in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md).
  If users previously didn’t indicate which variables to summarize with
  `include=`, then the design columns will now appear in the summary
  tables. ([\#1166](https://github.com/ddsjoberg/gtsummary/issues/1166))

- Added a check for functions that accept `...` where nothing should be
  passed in the `...`. If a bad or misspelled argument is found, the
  users are informed.
  ([\#1083](https://github.com/ddsjoberg/gtsummary/issues/1083))

#### Bug Fixes

- Fix in `"emmeans"` methods for
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  to due an argument name change in the emmeans package. We now require
  the most recent version of the package.
  ([\#1205](https://github.com/ddsjoberg/gtsummary/issues/1205))

- Bug fix in
  [`inline_text.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.tbl_summary.md)
  where one could not pass a pattern only when the column argument was
  `NULL`. ([\#1193](https://github.com/ddsjoberg/gtsummary/issues/1193))

- Fix in
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
  when there are no hidden columns.

- Fix in
  [`add_p.tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_svysummary.md)
  when Wald tests were converted to flextable. The survey tidier saved
  the column as a matrix-column instead of a vector, which was
  incompatible with flextable output.
  ([\#1153](https://github.com/ddsjoberg/gtsummary/issues/1153))

- Fixing Lancet theme mid-point encoding issue on Linux and MacOS.
  ([\#1146](https://github.com/ddsjoberg/gtsummary/issues/1146))

- Fix when using summary type gtsummary selectors
  (e.g. [`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md))
  with the `add_ci(style_fun=)` argument.
  ([\#1141](https://github.com/ddsjoberg/gtsummary/issues/1141))

## gtsummary 1.5.2

CRAN release: 2022-01-29

- Removed foreign reference to external functions in the top level of
  the package and replaced with indirect calls (the reason for the short
  time between releases).
  ([\#1129](https://github.com/ddsjoberg/gtsummary/issues/1129))

- Updates to the way footnotes are printed in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  when there is summary type `"continuous2"` present. Previously, all
  footnotes were removed, and now only the `"continuous2"` footnotes are
  removed.
  ([\#1127](https://github.com/ddsjoberg/gtsummary/issues/1127))

- Added the continuous variable name/label to the footnote for greater
  clarity in
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
  ([\#1123](https://github.com/ddsjoberg/gtsummary/issues/1123))

- Now exporting the
  [`.create_gtsummary_object()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/dot-create_gtsummary_object.md)
  function as a utility for other packages to build gtsummary tables.
  ([\#1130](https://github.com/ddsjoberg/gtsummary/issues/1130))

- Fix when
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  was run before
  [`add_n()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md).
  The overall row was not being omitted from the sum and the Ns were
  doubled.
  ([\#1126](https://github.com/ddsjoberg/gtsummary/issues/1126))

- Added method “emmeans” to
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  for
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  objects.

- Updated default
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  for
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  objects to be “emmeans” for continuous and dichotomous variables, and
  “smd” for categorical variables.

## gtsummary 1.5.1

CRAN release: 2022-01-20

#### New Functions

- Added new function
  [`modify_column_alignment()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_alignment.md)
  to updated column alignment. Function is a wrapper for the more
  complex
  [`modify_table_styling()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_styling.md)
  function.

- New function
  [`tbl_strata2()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md)
  that passes both the the stratified data frame as well as the stratum
  level to the user function.
  ([\#1091](https://github.com/ddsjoberg/gtsummary/issues/1091))

- Added a
  [`add_p.tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_continuous.md)
  method for adding p-values to
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
  tables. ([\#1023](https://github.com/ddsjoberg/gtsummary/issues/1023))

- Added
  [`add_overall.tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  method. ([\#1037](https://github.com/ddsjoberg/gtsummary/issues/1037))

#### New Functionality

- New test option “emmeans” in
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  and
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  uses the {emmeans} package to estimate marginal means/least-squares
  means for continuous variables, binary variables and random intercept
  models. ([\#1112](https://github.com/ddsjoberg/gtsummary/issues/1112))

- Column alignment is now recognized in
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)
  and
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md).
  Previously, the alignment utilized the `kable()` defaults and ignored
  any alignment instructions included in the gtsummary table styling.

- The
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)
  was updated to utilize
  [`kableExtra::column_spec()`](https://rdrr.io/pkg/kableExtra/man/column_spec.html)
  and
  [`kableExtra::cell_spec()`](https://rdrr.io/pkg/kableExtra/man/cell_spec.html)
  to apply bold and italic styling. The choice of the function depends
  on the use of `escape=` in
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
  ([\#1107](https://github.com/ddsjoberg/gtsummary/issues/1107))

- Default arguments to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) may now
  be overwritten by passing `...` to either
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)
  or
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md).
  Previously, passing a user-defined argument previously in use would
  result in error.

- Added `tbl_strata(.header=)` argument providing greater control over
  the stratum headers that are added to the tables, e.g. you can now add
  Ns to the headers using this argument.

- Added `add_p.tbl_cross(test.args=)` argument.
  ([\#1095](https://github.com/ddsjoberg/gtsummary/issues/1095))

- The `tbl_strata(.combine_args=)` has been added that lets you control
  all arguments in the
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  or
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  that occurs in
  [`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md).
  ([\#1090](https://github.com/ddsjoberg/gtsummary/issues/1090))

- Added the `add_ci(pattern=)` argument, which makes it easier to merge
  the CI column with the primary statistics column.
  ([\#1029](https://github.com/ddsjoberg/gtsummary/issues/1029))

- Suppress
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  spanning headers by passing `tbl_merge(tab_spanner = FALSE)`
  ([\#1067](https://github.com/ddsjoberg/gtsummary/issues/1067))

- Functions
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md),
  and
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)
  gain the `fmt_missing=` argument that applies missing symbols to
  missing values. The
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  argument defaults to `FALSE`, while the others’ default is `TRUE`.
  ([\#1073](https://github.com/ddsjoberg/gtsummary/issues/1073))

- Adding `tbl_regression(conf.int=)` and `tbl_uvregression(conf.int=)`
  argument. For some models, the confidence interval adds to the
  computation time significantly and may not be needed. This argument
  will omit the CI calculation.
  ([\#1052](https://github.com/ddsjoberg/gtsummary/issues/1052))

- The
  [`add_stat()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat.md)
  function was updated to accept
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
  tables.

- Multinomial models computed using MICE are now supported.
  ([\#1065](https://github.com/ddsjoberg/gtsummary/issues/1065))

- Added theme element to control the `tbl_regression(conf.int=)` default
  argument.

- It is now possible to pass a single tbl to
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md).
  This is useful when using
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  as a helper in other functions.
  ([\#1068](https://github.com/ddsjoberg/gtsummary/issues/1068))

- Added `statistics=` and `digits=` arguments to the
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  family of functions.
  ([\#1047](https://github.com/ddsjoberg/gtsummary/issues/1047))

- Added `digits=` argument to
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md).
  ([\#1046](https://github.com/ddsjoberg/gtsummary/issues/1046))

#### Other Updates

- Allowed modification to font_size for compact theme.
  ([\#1106](https://github.com/ddsjoberg/gtsummary/issues/1106))

- Automatically reduced vertical white space between columns for compact
  flextable theme.

- Improved user interface for `modify_*()` functions
  ([\#1064](https://github.com/ddsjoberg/gtsummary/issues/1064))

- Improved error messaging throughout the package.
  ([\#1050](https://github.com/ddsjoberg/gtsummary/issues/1050))

- Added link to the `syntax` help file to functions throughout the
  package. The `syntax` help file illustrates how to use the gtsummary
  selectors and details the formula-list notation.
  ([\#981](https://github.com/ddsjoberg/gtsummary/issues/981))

- Updated Spanish translation for Wilcoxon Rank-sum Test.

- Updates and additions to Portuguese language translations.
  ([\#1098](https://github.com/ddsjoberg/gtsummary/issues/1098))

- Updates to the French translations.

- Updating the
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  S3 method to have a more common structure,
  e.g. `add_overall(x, ....)`, where previously, the `...` were not
  present.
  ([\#1066](https://github.com/ddsjoberg/gtsummary/issues/1066))

- Updated the `theme_gtsummary_journal("qjecon")` to set
  `tbl_regression(conf.int = FALSE)` by default.

- Improved error messaging in
  [`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md)

- Updated the default formatting functions in
  [`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md).
  Previously, summaries with character results erred because the default
  summary function was
  [`style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_number.md).
  This has been updated to
  [`style_sigfig()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_sigfig.md)
  for numeric columns, and
  [`as.character()`](https://rdrr.io/r/base/character.html) for
  everything else.
  ([\#983](https://github.com/ddsjoberg/gtsummary/issues/983))

- All `style_*()` functions will retain attribute, such as the names.
  ([\#1035](https://github.com/ddsjoberg/gtsummary/issues/1035),
  [\#1031](https://github.com/ddsjoberg/gtsummary/issues/1031),
  [\#981](https://github.com/ddsjoberg/gtsummary/issues/981))

- The
  [`add_n.tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n_regression.md)
  (which is also utilized in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md))
  was adding the N column without applying a formatting function. The
  [`style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_number.md)
  function has now been added as the default styler.
  ([\#1022](https://github.com/ddsjoberg/gtsummary/issues/1022))

- Added class `"tbl_continuous"` to the output of
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md).

- Adding
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  test `"mcnemar.test.wide"` to calculate the p-value when the data are
  stored in a wide format, e.g. one column for a before value and a
  second column for after. The other McNemar test variant available in
  {gtsummary} expects data in a long format.

- Converted
  [`tbl_split()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated.md)
  to S3 function.

- Update how calls to
  [`gt::fmt_missing()`](https://gt.rstudio.com/reference/fmt_missing.html)
  are constructed to be more memory efficient.

#### Bug Fixes

- Fix in
  [`add_significance_stars()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_significance_stars.md)
  that led to an error when the summarized model did not have a
  confidence interval column.

- Fix in
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)
  and
  [`as_hux_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)
  where reference row was not properly placed after a
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  when the merged tables share common categorical variables but
  different reference rows.
  ([\#1063](https://github.com/ddsjoberg/gtsummary/issues/1063))

- Fix in
  [`inline_text.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.gtsummary.md)
  where the first level of a categorical variable could not be selected
  if the table had also been processed with
  [`remove_row_type()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/remove_row_type.md).
  ([\#1078](https://github.com/ddsjoberg/gtsummary/issues/1078))

- Fix in `modify_table_styling(cols_merge_pattern)` when it is used with
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  followed by
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md).
  ([\#1057](https://github.com/ddsjoberg/gtsummary/issues/1057))

- Bug fix in
  [`separate_p_footnotes()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/separate_p_footnotes.md)
  where test names were not being translated when
  [`theme_gtsummary_language()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)
  was set.
  ([\#1055](https://github.com/ddsjoberg/gtsummary/issues/1055))

- Fix in
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  when rows in a merging table are not present in the first table.
  ([\#1033](https://github.com/ddsjoberg/gtsummary/issues/1033))

- Fix in
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  for [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  regression models.

- Fix in
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
  when `include=` is not specified.

- Fix in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  when a tidier returns CI columns that are all missing.
  ([\#1012](https://github.com/ddsjoberg/gtsummary/issues/1012))

- Fix in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)/[`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  when check whether the passed test is an internal method or a custom
  method. The previous code required Suggested packages, such as,
  {lme4}, {effectsize}, and {survey}, to be installed.
  ([\#1018](https://github.com/ddsjoberg/gtsummary/issues/1018))

#### Breaking Changes

- No longer exporting `assert_package()`. It has been migrated to
  {broom.helpers} and we now use
  [`broom.helpers::.assert_package()`](https://larmarange.github.io/broom.helpers/reference/assert_package.html).
  ([\#1051](https://github.com/ddsjoberg/gtsummary/issues/1051))

## gtsummary 1.5.0

CRAN release: 2021-10-16

#### New Functions

- Added new function
  [`tbl_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_continuous.md)
  to summarize a continuous variable by 1 or more categorical variables.

- Added new function
  [`add_ci()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_ci.md)
  that adds a new column with the confidence interval for
  proportions/means reported in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
  ([\#868](https://github.com/ddsjoberg/gtsummary/issues/868))

- Migrated a new function
  [`tbl_split()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated.md)
  from the {bstfun} package. Function allows users to split a
  {gtsummary} table into multiple tables.

- Migrated a new function
  [`separate_p_footnotes()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/separate_p_footnotes.md)
  from the {bstfun} package. Function allows users to separate the
  composite footnote listing the tests performed in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md),
  and replaces it with individual footnotes for each test name.

- New function
  [`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md)
  allowing to create a table of summary statistics using a custom
  summary function
  ([\#973](https://github.com/ddsjoberg/gtsummary/issues/973),
  [\#976](https://github.com/ddsjoberg/gtsummary/issues/976))

  - Set of helpers to be used with
    [`tbl_custom_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_custom_summary.md):
    [`continuous_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/continuous_summary.md),
    [`proportion_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/proportion_summary.md),
    [`ratio_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/ratio_summary.md)

- New function `modify_cols_merge()` that can merge two or more columns
  in a {gtsummary} table.
  ([\#939](https://github.com/ddsjoberg/gtsummary/issues/939))

- Added function
  [`tbl_butcher()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_butcher.md)
  to reduce the size of a {gtsummary} table. After an object has been
  butchered, other {gtsummary} functions may not be able to execute on
  the object.

- Added new function
  [`tidy_robust()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md)
  that will add robust standard errors, confidence intervals, and
  p-values with
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  and
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md).
  The function is a wrapper for `parameters::model_paramters()`.
  ([\#979](https://github.com/ddsjoberg/gtsummary/issues/979))

#### New Functionality

- Added a `CITATION` file so users can now cite the R Journal manuscript
  using `citation("gtsummary")`.

- Added Standardized Mean Difference method to
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md),
  wrapping the {smd} package’s calculations.
  ([\#966](https://github.com/ddsjoberg/gtsummary/issues/966))

- Extended
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  to accept
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  objects in addition to
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  objects.

- Added a standardized mean difference method for
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  tables.

- Added `tbl_strata(.stack_group_header=)` argument to include/exclude
  the headers when tables are combined with
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md).

- Added `tbl_strata(.quiet=)` argument.

- Allow
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  and
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md)
  to be run on the same table.
  ([\#959](https://github.com/ddsjoberg/gtsummary/issues/959))

- Updated
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  to include the overall statistics in the `df_stats` tibbles saved in
  `.$meta_data.` This makes it possible to report any of the overall
  statistics using the `inline_text(pattern=)` argument.

#### Other Updates

- Added a help file detailing the formula list notation used throughout
  the {gtsummary} package.
  ([\#981](https://github.com/ddsjoberg/gtsummary/issues/981))

- Updates to
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  documentation. The model N is no longer reported by default, and
  removed that section from the help file.
  ([\#998](https://github.com/ddsjoberg/gtsummary/issues/998))

- Updates to make the internal `df_stats` objects consistent across
  various {gtsummary} objects. Added internal function
  `df_stats_to_table_body` that adds the numeric df_stats tibble to
  `.$table_body`. The formatting functions are also added for the new
  columns to `.$table_styling$fmt_fun`. This function is now used in
  [`inline_text.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.gtsummary.md)
  to prepare the returned statistics
  ([\#921](https://github.com/ddsjoberg/gtsummary/issues/921))

- Now using
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) to
  prepare the [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)
  results. This will be more stable than the version originally written.
  ([\#898](https://github.com/ddsjoberg/gtsummary/issues/898))

- The function `assert_package()` now takes the minimum required version
  of the package from the DESCRIPTION file, and the function is now
  exported.

- Now using
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) to
  prepare [`aov()`](https://rdrr.io/r/stats/aov.html) test results in
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md),
  which adds additional columns to `.$table_body()`
  ([\#956](https://github.com/ddsjoberg/gtsummary/issues/956))

- Updated the `README` to include links to a recording of a gtsummary
  presentation and to the RStudio Education blog post.

- Removed `maturing` lifecycle tag from `README`.

- Updated deprecated function `workflows::pull_workflow_fit(x)` to
  `workflows::extract_fit_parsnip(x)`.

#### Bug Fixes

- Fix in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  when a factor variable is passed that is all `NA` with no specified
  levels. ([\#977](https://github.com/ddsjoberg/gtsummary/issues/977))

- Fix in
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  when a factor variable with all NA values is passed.
  ([\#977](https://github.com/ddsjoberg/gtsummary/issues/977))

- Bug fix for the `add_difference(estimate_fun=)` argument.

#### Breaking Changes

- Updated `add_p.tbl_summary(test = . ~ "mcnemar.test", group = id)`
  syntax to require the `group=` argument to align with the paired
  Wilcoxon rank-sum test and paired t-test syntax.

- Deleted deprecated functions `add_comparison()`, `add_global()`,
  `tab_style_bold_p()`, `tab_style_bold_labels()`,
  `tab_style_italicize_levels()`, `tab_style_italicize_labels()`,
  `tab_style_bold_levels()`.

- The following deprecated arguments have been removed:
  `tbl_summary(group=)`, `as_gt(omit=)`.

- The survival package has been moved from Imports to Suggests, and will
  no longer automatically be installed when {gtsummary} is installed.
  Additionally,
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) is no
  longer re-exported with the package.

## gtsummary 1.4.2

CRAN release: 2021-07-13

- Update to the internals of
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  to better handle when two or more stacked tables are then stacked
  again ([\#906](https://github.com/ddsjoberg/gtsummary/issues/906))

- Updates to make
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  compatible with {survey} package updates in v4.1
  ([\#930](https://github.com/ddsjoberg/gtsummary/issues/930))

- The
  [`as_hux_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)
  function previously stripped markdown old syntax from column headers
  and spanning headers. The output now uses markdown syntax in the
  headers by default utilizing
  [`huxtable::set_markdown()`](https://hughjonesd.github.io/huxtable/reference/markdown.html)
  ([\#885](https://github.com/ddsjoberg/gtsummary/issues/885))

- Variables passed in the `tbl_svysummary(by=)` argument will now
  automatically be added to `include=`.
  ([\#925](https://github.com/ddsjoberg/gtsummary/issues/925))

- Bold and italic requests are now ignored for kableExtra output. These
  are carried out via markdown syntax, which is not supported by
  {kableExtra}
  ([\#917](https://github.com/ddsjoberg/gtsummary/issues/917))

- Bug fix for `add_p.tbl_cross(pvalue_fun=)`; argument was being
  ignored.

- Updated
  [`style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_pvalue.md)
  to format p-values slightly larger than 1 and slightly lower than 0
  (due to imprecise numeric storage).
  ([\#907](https://github.com/ddsjoberg/gtsummary/issues/907))

- Fix allowing for factor vectors to be passed in
  `tbl_stack(group_header=)`.
  ([\#908](https://github.com/ddsjoberg/gtsummary/issues/908))

- Updated arguments `y=` and `x=` in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  to allow for non-standard names
  ([\#912](https://github.com/ddsjoberg/gtsummary/issues/912))

## gtsummary 1.4.1

CRAN release: 2021-05-19

- Updated
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  to be compatible with models created with the {parsnip} and
  {workflows} packages
  ([\#890](https://github.com/ddsjoberg/gtsummary/issues/890),
  [\#647](https://github.com/ddsjoberg/gtsummary/issues/647))

- Added the `modify_table_styling(text_format = "indent2")` option to
  double indent a row.
  ([\#864](https://github.com/ddsjoberg/gtsummary/issues/864))

- Messaging update when
  [`inline_text.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.gtsummary.md)
  suspects a variable has been repeated in the gtsummary table.
  ([\#855](https://github.com/ddsjoberg/gtsummary/issues/855))

- Bug fix in
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  for columns that are all `NA`. These variables no longer error;
  rather, a message is printed indicating the p-value is not possible to
  calculate.
  ([\#889](https://github.com/ddsjoberg/gtsummary/issues/889))

- Updated
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  to be compatible with {srvyr} package
  ([\#886](https://github.com/ddsjoberg/gtsummary/issues/886))

- Updated default header when using `tbl_uvregression(x=)` to
  `"**Outcome**"`
  ([\#867](https://github.com/ddsjoberg/gtsummary/issues/867))

- The `tbl_summary(by=)` variable is now added to `include=` by default
  ([\#871](https://github.com/ddsjoberg/gtsummary/issues/871))

- Variables are converted to numeric before being passed to
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md).
  This avoids an error when a date difference is passed.
  ([\#880](https://github.com/ddsjoberg/gtsummary/issues/880))

- Bug fix for {Hmisc} labeled data with
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  ([\#876](https://github.com/ddsjoberg/gtsummary/issues/876))

- Bug fix in
  [`add_n.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n_summary.md)
  to proportion missing in some cases.
  ([\#903](https://github.com/ddsjoberg/gtsummary/issues/903))

- Updates to the default formatting functions in the `add_glance_*()`
  functions. P-values are now styled with
  [`style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_pvalue.md)
  and No. Obs. and degrees of freedom with
  [`style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_number.md)
  ([\#870](https://github.com/ddsjoberg/gtsummary/issues/870))

## gtsummary 1.4.0

CRAN release: 2021-04-13

#### New Functions

- Added new function
  [`add_glance_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_glance.md)
  as a companion to
  [`add_glance_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_glance.md).
  Function adds model statistics, such as R-squared, to the bottom of
  the model summary table.

- Added new function
  [`add_significance_stars()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_significance_stars.md)
  adding star indicators to significant estimates and an explanatory
  footnote.

- Added new function
  [`tbl_strata()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_strata.md).
  The function aids prepares gtsummary tables stratified by one or more
  variables ([\#679](https://github.com/ddsjoberg/gtsummary/issues/679))

- Adding coefficient
  [`plot()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/plot.md)
  methods for
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  and
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md).
  Function creates a forest plot of model coefficients via
  `GGally::ggcoef_plot()`.

- New function
  [`modify_fmt_fun()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_fmt_fun.md)
  has been introduced to help update the functions that format numeric
  columns and rows in `.x$table_body`.

- New function introduced,
  [`modify_table_styling()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_styling.md),
  to update printing instructions of tables. This function replaces
  `modify_table_header()`, which is now soft deprecated.

- Added function
  [`add_vif()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_vif.md)
  to include variance inflation factors in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  output. ([\#717](https://github.com/ddsjoberg/gtsummary/issues/717))

- Added a generic function
  [`inline_text.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.gtsummary.md)
  that can report results from any gtsummary table.
  ([\#398](https://github.com/ddsjoberg/gtsummary/issues/398))

#### New Functionality

- Print infrastructure has been updated to allow for both row and column
  specification when formatting data or table styling. The
  `x$table_header` object has been replaced with a more general
  `x$table_styling`. Review the updated vignette
  `"gtsummary_definition.Rmd"` for details. The `x$table_body` is no
  longer grouped after
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md);
  rather, the grouping variable is specified in `gt::gt(groupname_col=)`

- [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  now accepts any class as input. Previously only non-date base classes
  were accepted. For non-base R classes, the summary type must be
  specified using `tbl_summary(type=)`. The default summary statistic
  for dates/times is the minimum and maximum.
  ([\#488](https://github.com/ddsjoberg/gtsummary/issues/488))

- The
  [`add_stat()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat.md)
  function may now return multiple columns of new statistics. Some
  arguments have been deprecated in the update.
  ([\#746](https://github.com/ddsjoberg/gtsummary/issues/746))

- [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  now accepts both data frames and survey design objects as input.
  ([\#742](https://github.com/ddsjoberg/gtsummary/issues/742))

- If the default `tidy_fun = broom::tidy` fails,
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  attempts to tidy the model, if {parameters} is installed.
  ([\#854](https://github.com/ddsjoberg/gtsummary/issues/854))

- Added a custom tidier for
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) models
  ([`tidy_gam()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md))
  and a method function
  ([`tbl_regression.gam()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression_methods.md))
  that uses the new tidier by default.
  ([\#745](https://github.com/ddsjoberg/gtsummary/issues/745))

- Added default support for `brmsfit` model in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  with new method function.
  ([\#751](https://github.com/ddsjoberg/gtsummary/issues/751))

- Functions
  [`modify_footnote()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated_modify_footnote.md)
  and
  [`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  now include the `text_interpret=` argument indicating whether to use
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html) to format
  text. Default is
  [`gt::md()`](https://gt.rstudio.com/reference/md.html).

- Added/updated functions
  [`add_n()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md)
  and
  [`add_nevent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_nevent_regression.md)
  to work with `tbl_regression` and `tbl_uvregression` objects. Each
  function now has an argument to place Ns on the label or level rows.
  ([\#744](https://github.com/ddsjoberg/gtsummary/issues/744))

- Added *The Quarterly Journal of Economics* to
  [`theme_gtsummary_journal()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md).
  This journal theme will be updated again after the gt package updates
  [`cols_merge()`](https://gt.rstudio.com/reference/cols_merge.html)
  with a rows argument and allows for line breaks within cell.

- Korean and Icelandic language translations added for
  [`theme_gtsummary_language()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md).

- Ability to merge two or more columns with
  `modify_table_styling(cols_merge_pattern=)` argument.

- Added theme element `"pkgwide-fun:pre_conversion"`. The function
  specified here will be executed on the gtsummary object before it is
  printed or converted with the
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md),
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md),
  etc functions.
  ([\#773](https://github.com/ddsjoberg/gtsummary/issues/773))

- The `modify_table_body(fun=)` argument has been generalized to accept
  formula shortcut notation.

- Added exploratory data analysis theme that shows more details by
  default,
  [`theme_gtsummary_eda()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md).

#### Other Updates

- Updated[`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  and
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  to capture the first source note and caption in the merged/stacked
  tables. Previously, any captions and source notes were lost.

- Added messaging when table caption requested via
  [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)
  for a gt table when using a version of gt that does not support
  captions.

- Updates to the table gallery vignette reflecting changes in the
  package. ([\#775](https://github.com/ddsjoberg/gtsummary/issues/775))

- Improved error messaging when there is a problem constructing one of
  the univariate regression models in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md).

- Added variable-specific formatting to `add_difference(estimate_fun=)`
  allowing a single table to show, for example, mean and rate
  differences that are formatted/rounded differently.

- Improved handling and messaging to users when columns with
  `"haven_labelled"` class are passed to gtsummary functions.
  ([\#805](https://github.com/ddsjoberg/gtsummary/issues/805))

- Improved handling of ordered factors as the `by=` variable
  ([\#569](https://github.com/ddsjoberg/gtsummary/issues/569),
  [\#540](https://github.com/ddsjoberg/gtsummary/issues/540))

- Removed {usethis} package dependency and replaced with {cli}.
  ([\#768](https://github.com/ddsjoberg/gtsummary/issues/768))

- Added the survey-adapted t-test to
  [`theme_gtsummary_mean_sd()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)
  for continuous variables in
  [`add_p.tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_svysummary.md)
  ([\#758](https://github.com/ddsjoberg/gtsummary/issues/758))

- Allowing for tidyverse shortcut notation in
  `tbl_survfit(estimate_fun=)` specification,
  e.g. `tbl_survfit(estimate_fun= ~style_sigfig(.x * 100))`
  ([\#761](https://github.com/ddsjoberg/gtsummary/issues/761))

- The JAMA journal theme has been updated to merge the coefficient and
  confidence interval columns.

- Updated other
  [`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md)
  functions to wrap
  [`inline_text.gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.gtsummary.md)

#### Bug Fixes

- The `label=` argument for unstratified models was being ignored in
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
  ([\#842](https://github.com/ddsjoberg/gtsummary/issues/842))

- Preserve ordering for factor variables in
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md).
  ([\#764](https://github.com/ddsjoberg/gtsummary/issues/764))

- Bug fix for spanning headers with kableExtra output. The spanning
  header was misplaced when the header text was sandwiched between two
  blank spanning headers.

- Bug fix when displaying an unobserved level of a factor variable
  dichotomously in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
  ([\#780](https://github.com/ddsjoberg/gtsummary/issues/780))

- Bug fix in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  where test name footnote was not being translated with
  [`theme_gtsummary_language()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md).

#### Breaking Changes

- `tbl_survival()` has moved from questioning to deprecated. This
  function maintains the old `x$table_header` format (instead of the
  more flexible `x$table_styling`). The `"level_label"` column was
  renamed to `"groupname_col"` and the `x$table_body` is no longer
  grouped with `group_by(level_label)`

- The back-end implementation of `add_stat_label(location = "row")` has
  been updated. The function now merges columns `"label"` and
  `"stat_label"` instead of modifying the `"label"` column directly.
  This *could* be a breaking change if users had manually updated
  results, for example, from a
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  table to merge with
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  using
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)

- The function
  [`add_stat_label()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat_label.md)
  no longer auto-switches `location = "column"` requests to `"row"` in
  the presence of `"continuous2"` variables.

## gtsummary 1.3.7

CRAN release: 2021-02-26

- No changes. Resubmitting to resolve CRAN quirk.

## gtsummary 1.3.6

CRAN release: 2021-01-08

#### New Functions

- Added function
  [`add_difference()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_difference.md),
  which adds difference between groups, confidence interval and p-value.
  ([\#617](https://github.com/ddsjoberg/gtsummary/issues/617))

- Added function
  [`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_caption.md)
  that includes table captions. For gt output, requires gt version \>
  0.2.2 ([\#701](https://github.com/ddsjoberg/gtsummary/issues/701))

- Added function
  [`modify_table_body()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_table_body.md)
  allowing users to more easily make changes to gtsummary tables

- Added function
  [`remove_row_type()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/remove_row_type.md)
  for removing header, reference, or missing rows from a gtsummary
  tables. ([\#678](https://github.com/ddsjoberg/gtsummary/issues/678))

- Added selecting function
  [`all_stat_cols()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)
  that selects columns from `tbl_summary`/`tbl_svysummary` object with
  summary statistics (i.e. `"stat_0"`, `"stat_1"`, `"stat_2"`, etc.).

- New selecting function was added
  [`all_tests()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)
  to make it easier to select variables based on the test used to
  calculate the p-value,
  e.g. `add_p(test = c(age, marker) ~ "t.test", test.args = all_tests("t.test") ~ list(var.equal = TRUE))`

- Added functions
  [`modify_column_hide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md)
  and
  [`modify_column_unhide()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify_column_hide.md)
  to hide and unhide columns in `.$table_body`. Simple wrappers for
  `modify_table_header()`.

#### New Functionality

- Previously, the `tbl_summary(digits=)` only accepted integer values
  specifying the number of decimal places to round a statistic. The
  argument now accepts both integers or functions,
  e.g. `digits = age ~ style_sigfig`.
  ([\#708](https://github.com/ddsjoberg/gtsummary/issues/708))

- The
  [`add_stat()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat.md)
  function now supports
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  objects in addition to
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  ([\#688](https://github.com/ddsjoberg/gtsummary/issues/688))

- The
  [`add_stat()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat.md)
  function can now place statistics on the label row, or the level rows
  for categorical variables.
  ([\#714](https://github.com/ddsjoberg/gtsummary/issues/714))

- [`inline_text.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.tbl_survfit.md)
  updated to allow users to select p-value (and other) columns.
  ([\#589](https://github.com/ddsjoberg/gtsummary/issues/589))

#### Other Updates

- [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  has been updated so users may more easily access internal data while
  defining headers and so that it no longer adds its call to the
  gtsummary `.$call_list`
  ([\#719](https://github.com/ddsjoberg/gtsummary/issues/719))

- The default value for `include=` argument for the
  [`add_global_p.tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  and
  [`add_global_p.tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  methods have been made the same with `include = everything()`, and the
  help files for these methods have been brought together in a single
  file. ([\#721](https://github.com/ddsjoberg/gtsummary/issues/721))

- Introducing new package dependency {broom.helpers}

  - The tidying and preparation of
    [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
    tables are now being performed by {broom.helpers}
    ([\#636](https://github.com/ddsjoberg/gtsummary/issues/636),
    [\#607](https://github.com/ddsjoberg/gtsummary/issues/607))

  - All select helper functions and the utility functions that make them
    possible, have been cleaned up and migrated to {broom.helpers}.
    ([\#648](https://github.com/ddsjoberg/gtsummary/issues/648),
    [\#680](https://github.com/ddsjoberg/gtsummary/issues/680))

  - Importing `.generic_selector()`, `.select_to_varnames()`, and
    `.formula_list_to_named_list()` from {broom.helpers}: this is a
    function that makes it easy to create selecting functions like
    [`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md).
    The internals allow for it to be used in {broom.helpers} and
    {gtsummary} seamlessly.
    ([\#680](https://github.com/ddsjoberg/gtsummary/issues/680))

  - Theme element has been added for controlling the other
    `tidy_plus_plus()` arguments.
    ([\#692](https://github.com/ddsjoberg/gtsummary/issues/692))

  - Variables that do not follow standard naming conventions are now
    parsed correctly and presented in the regression tables.

- The
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  function is now an S3 method. The new interface allows for special
  handling of different model types using S3 methods:
  [`tbl_regression.default()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md),
  `tbl_regression.lmer()`, `tbl_regression.glmer()`,
  [`tbl_regression.survreg()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression_methods.md)

- `tbl_regression(add_estimate_to_reference_rows=)` argument has been
  added. Also added to
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md).
  ([\#692](https://github.com/ddsjoberg/gtsummary/issues/692))

- Theme element for `tbl_regression(add_estimate_to_reference_rows=)`
  has been added.
  ([\#677](https://github.com/ddsjoberg/gtsummary/issues/677))

- Removed `"Statistics presented:"` and `"Statistical tests performed:"`
  prefixes from the `tbl_summary() %>% add_p()` footnotes.

- The codebase powering
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  and related methods has been refactored for better performance,
  organization, and customizability.
  ([\#622](https://github.com/ddsjoberg/gtsummary/issues/622))

  - For clarity, a help file listing each test available within
    gtsummary and the pseudo code for calculating the p-value has been
    added (see
    [`?tests`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md))

  - Each
    [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
    method now has the `test.args=` argument. Use this argument to pass
    additional arguments to the statistical method,
    e.g. `add_p(test = c(age, marker) ~ "t.test", test.args = c(age, marker) ~ list(var.equal = TRUE))`

  - Additional tests have been added: paired t-test, signed rank test,
    and more. See
    [`?tests`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tests.md)
    for full list.

  - More robust unit testing implemented for all
    [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
    methods.

- Added messaging to
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  to inform users that the attributes from the first table passed take
  precedent over the others’.
  ([\#699](https://github.com/ddsjoberg/gtsummary/issues/699))

- In the `modfiy_*()` functions, if users did not select any columns,
  they encountered an error. Now, if no columns are selected,
  instructions are printed for how to correctly select columns in a
  gtsummary table. Moreover, if no columns are selected, the gtsummary
  object is now returned unaltered.
  ([\#699](https://github.com/ddsjoberg/gtsummary/issues/699))

- The
  [`add_glance_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_glance.md)
  function has been generalized so users may pass any glance function.
  Previously,
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  was being used with no option to change it.
  ([\#699](https://github.com/ddsjoberg/gtsummary/issues/699))

- The `...` arguments have been added to
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md).
  These dots are subsequently passed to `gt::gt(...)`.
  ([\#701](https://github.com/ddsjoberg/gtsummary/issues/701))

- Multiple imputation models created with {mice}, and multinomial
  regression models created with {nnet} are now supported in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  ([\#645](https://github.com/ddsjoberg/gtsummary/issues/645))

- Updates to
  [`add_global_p.tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  allowing for variable names with spaces and special characters
  ([\#682](https://github.com/ddsjoberg/gtsummary/issues/682))

- Added `digits=` argument to
  [`style_percent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_percent.md)
  ([\#690](https://github.com/ddsjoberg/gtsummary/issues/690))

- Users may now choose which
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  columns to report with a theme element. they can choose among the
  `"estimate"`, `"std.error"`, `"statistic"`, `"ci"`, `"conf.low"`,
  `"conf.high"` and `"p.value"`
  ([\#637](https://github.com/ddsjoberg/gtsummary/issues/637))

- Allow users to include the reference value in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  via a theme element

- Users may change the symbol with a reference row symbol with a theme
  element. ([\#628](https://github.com/ddsjoberg/gtsummary/issues/628))

#### Bug Fixes

- Bug fix when a default statistic is set using themes for
  `"continuous2"` variables that has length larger than one

- Bug fix when missing/non-missing percentages requested in
  [`add_n.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n_summary.md)

#### Breaking Changes

- The default test for 2-by-2 tables with expected cell counts has been
  updated from the chi-squared test with continuity correction to the
  original chi-squared test for
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  ([\#721](https://github.com/ddsjoberg/gtsummary/issues/721))

- Experimental function `add_p.tbl_survfit(test.args=)` in addition to
  accepting the formula list notation, also accepted a single string
  naming a test that was interpreted as `everything() ~ "test_name"`.
  The single string is no longer accepted, and users must use the
  formula notation.

- Removed theme element `N_fun` that was previously marked as
  questioning and likely to be removed from the package.

## gtsummary 1.3.5

CRAN release: 2020-09-29

#### New Functionality

- New summary type `continuous2` allows adding labelled statistic rows
  to tables in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  and
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md).
  You can report several lines of statistics with this type.
  ([\#620](https://github.com/ddsjoberg/gtsummary/issues/620))

  - The
    [`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)
    function now selects summary types `continuous` and `continuous2` by
    default.
  - Added
    [`all_continuous2()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)
    function for selecting summary type `continuous2` exclusively.
  - Added
    [`theme_gtsummary_continuous2()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)
    to make `continuous2` the default summary type for all continuous
    variables.

- New function
  [`add_glance_source_note()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_glance.md)
  adds the statistics returned in
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  as a source note on a
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  ([\#434](https://github.com/ddsjoberg/gtsummary/issues/434))

- Exporting the `modify_table_header()` function, which is an
  advanced-use function used to make modifications to the
  `.$table_header` object to update printing instructions for the
  gtsummary object.

- Added two custom tidiers for use in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  and
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md).
  ([\#635](https://github.com/ddsjoberg/gtsummary/issues/635))

  - [`tidy_standardize()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md)
    returns standardized coefficients using the {effectsize} package
  - [`tidy_bootstrap()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/custom_tidiers.md)
    gives bootstrapped parameter estimates, calculated using the
    {parameters} package

#### Bug Fixes

- Bug fix where `estimate_fun=` and `pvalue_fun=` were not being passed
  to
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  in
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)

- There was an environments bug when evaluating the LHS of the formula
  inputs. In some complex situations, a stored character vector of
  column names could not properly evaluate
  ([\#604](https://github.com/ddsjoberg/gtsummary/issues/604))

- Fixed
  [`style_ratio()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_ratio.md)
  bug where there were rounding errors near one
  ([\#651](https://github.com/ddsjoberg/gtsummary/issues/651))

- Fixed
  [`style_sigfig()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_sigfig.md)
  bug where there were rounding errors near thresholds
  ([\#638](https://github.com/ddsjoberg/gtsummary/issues/638))

- Adding the footnote from the stat columns describing the statistics
  presented to the overall column
  ([\#643](https://github.com/ddsjoberg/gtsummary/issues/643))

#### Other Updates

- Refresh of vignettes to use recently released functions
  ([\#649](https://github.com/ddsjoberg/gtsummary/issues/649))

- Moved the nevent column to after the N column when
  [`add_nevent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_nevent_regression.md)
  is called on a
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  object ([\#439](https://github.com/ddsjoberg/gtsummary/issues/439))

- gtsummary themes updates

  - Add
    [`theme_gtsummary_mean_sd()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)
    theme to report mean and SD by default and use t-tests and ANOVA in
    [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
    ([\#654](https://github.com/ddsjoberg/gtsummary/issues/654))
  - Added first draft of the NEJM theme
  - Added the mid-point decimal separator for the Lancet theme

## gtsummary 1.3.4

CRAN release: 2020-08-27

- Added a copy of tidyselect’s
  [`where()`](https://tidyselect.r-lib.org/reference/where.html)
  function to allow users to use predicate select helpers
  ([\#632](https://github.com/ddsjoberg/gtsummary/issues/632))

- Fixed
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
  bug where function defaulted to `'column'` when `margin = NULL`. Now
  it defaults to display no margins when `NULL`.
  ([\#624](https://github.com/ddsjoberg/gtsummary/issues/624))

- Changed default of
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
  `missing` argument from `'\U2014'` (em dash) to NULL (CRAN issue). Em
  dash is still displayed by default in tables but it is set later in
  function.

## gtsummary 1.3.3

CRAN release: 2020-08-11

#### New Functions

- The {flextable} has graduated from Experimental status! Introducing
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md),
  which replaces `as_flextable()`. The updated function includes
  improvements to the default aesthetics of the tables, and improved
  consistency for spanning header rows.

- Added
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  function to summarize complex and weighted survey designs.
  `tbl_svysummary` is now its own class that works with
  [`add_n()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md),
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md),
  [`add_q()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_q.md),
  [`add_stat_label()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat_label.md),
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md),
  [`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md),
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  and
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  ([\#460](https://github.com/ddsjoberg/gtsummary/issues/460)).

- The family of
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
  functions has been greatly expanded!

  - [`tbl_survfit.survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md),
    [`tbl_survfit.list()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md),
    and
    [`tbl_survfit.data.frame()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
    have been added that accept a single survfit object, list of survfit
    objects, or a data frame for more flexible creation of univariate
    survival tables.
  - [`add_p.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_survfit.md),
    [`add_n.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.tbl_survfit.md),
    [`add_nevent.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_nevent.tbl_survfit.md)
    have been added to include additional information in
    [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
    tables.

- Adding
  [`as_hux_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_hux_table.md)
  after the huxtable 5.0.0 release.

- Added
  [`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  function to assist when updating table headers, footnotes, and
  spanning headers. The function prints the current underlying column
  names along with their labels easing the process of identifying the
  column names needed to make updates.
  ([\#539](https://github.com/ddsjoberg/gtsummary/issues/539))

- Added a language theme,
  [`theme_gtsummary_language()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)
  for translating tables into Spanish, French, Portuguese, German,
  Chinese (Traditional and Simplified), Hindi, Marathi, Gujarati,
  Japanese and Swedish
  ([\#511](https://github.com/ddsjoberg/gtsummary/issues/511))

- Added
  [`style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_number.md)
  function which allows for granular control of how numbers are
  formatted throughout the package. The
  [`style_percent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_percent.md),
  [`style_pvalue()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_pvalue.md),
  [`style_sigfig()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_sigfig.md),
  and
  [`style_ratio()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_ratio.md)
  functions have been updated to use
  [`style_number()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/style_number.md)
  in the background. The implication is that users can now control how
  every number in a gtsummary table appears. For example, formatting can
  be updated to use the comma as the decimal mark and also specify the
  big mark using the gtsummary themes.
  ([\#458](https://github.com/ddsjoberg/gtsummary/issues/458))

#### User-facing Updates

- Added support for competing risk cumulative incidence estimates to
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
  ([\#64](https://github.com/ddsjoberg/gtsummary/issues/64),
  [\#448](https://github.com/ddsjoberg/gtsummary/issues/448))

- Updated API for
  [`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md).
  Users no longer need call
  `set_gtsummary_theme(theme_gtsummary_journal())`; rather, they call
  [`theme_gtsummary_journal()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md).
  Each built-in theme now has an argument `set_theme = TRUE`. When
  `FALSE`, the theme function will invisibly return the named list the
  theme elements, and the theme will not be set.
  ([\#522](https://github.com/ddsjoberg/gtsummary/issues/522))

- Users can now specify how many decimal places to round statistics for
  categorical variables in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md),
  e.g. `45 / 100 (45.0%)`.
  ([\#458](https://github.com/ddsjoberg/gtsummary/issues/458))

- Added the ‘The Lancet’ theme to
  [`theme_gtsummary_journal()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/theme_gtsummary.md)

- Updated the handling for arguments that accept functions to allow
  users to pass anonymous functions using the tidyverse shortcut
  notation, e.g. `~style_pvalue(.x, digits = 2)`.

- The header for the `tbl_stack(group_header=)` column is now integrated
  into a typical gtsummary framework, meaning that all standard
  functions can be executed on it,
  e.g. [`modify_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  for non-gt output.

- Added `type=` argument to
  [`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md),
  and added `include=` and `keep=` arguments to
  [`add_global_p.tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  ([\#554](https://github.com/ddsjoberg/gtsummary/issues/554))

#### Internal Updates

- Updated
  [`add_n()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md)
  internals to be more efficient without recalculating statistics
  previously saved in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)

- {survival} package moved from Imports to Suggests

- The
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)
  function is now a method function, and `add_overall.tbl_summary` and
  `add_overall.tbl_svysummary` added
  ([\#460](https://github.com/ddsjoberg/gtsummary/issues/460)).

- Updated the variable labels for age and marker in the trial dataset.

- Removed large \*.gif files out of the package to reduce build size
  ([\#485](https://github.com/ddsjoberg/gtsummary/issues/485))

#### Bug Fixes

- Fixed bug where source note was not made smaller font size with
  compact theme when table was printed with {flextable}
  ([\#584](https://github.com/ddsjoberg/gtsummary/issues/584))

- Bug fix for
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  when a data frame contained an ordered factor column
  ([\#567](https://github.com/ddsjoberg/gtsummary/issues/567))

- Bug fix when only categorical summary statistics were requested for
  continuous variables in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  and
  [`tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_svysummary.md)
  ([\#528](https://github.com/ddsjoberg/gtsummary/issues/528))

- Bug fix when *named* list of {gtsummary} objects is passed to
  `tbl_merge(x=)`
  ([\#541](https://github.com/ddsjoberg/gtsummary/issues/541))

- Bug fix when for
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  when adjustment variables were included in
  `formula = "{y} ~ {x} + age"`. The adjustment variables were being
  printed in the resulting table.
  ([\#555](https://github.com/ddsjoberg/gtsummary/issues/555))

#### Breaking Changes

- All
  [`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
  have been upgraded to
  [`lifecycle::deprecate_stop()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)
  for \<= v1.2.0 (released Aug 2019)

- Removing `as_flextable()` and replacing with
  [`as_flex_table()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_flex_table.md)
  due to a name conflict with
  [`flextable::as_flextable`](https://davidgohel.github.io/flextable/reference/as_flextable.html)
  ([\#462](https://github.com/ddsjoberg/gtsummary/issues/462))

## gtsummary 1.3.2

CRAN release: 2020-06-14

- Now returning all columns from
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) in
  `.$table_body()`
  ([\#516](https://github.com/ddsjoberg/gtsummary/issues/516))

- [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  now accepts a named list of gtsummary tables when using the
  `group_header=` argument
  ([\#524](https://github.com/ddsjoberg/gtsummary/issues/524))

- [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  bug fix for Wilcoxon rank-sum p-value calculation introduced in the
  last release
  ([\#525](https://github.com/ddsjoberg/gtsummary/issues/525))

- Delaying the release of `as_huxtable()` until the next {huxtable}
  release.

## gtsummary 1.3.1

CRAN release: 2020-06-02

#### New Functions

- Introducing themes in {gtsummary}. Use the function
  [`set_gtsummary_theme()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/set_gtsummary_theme.md)
  to set new themes, and review the themes vignette for details on
  setting and creating personalized themes.
  ([\#424](https://github.com/ddsjoberg/gtsummary/issues/424))

- New functions
  [`modify_footnote()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/deprecated_modify_footnote.md)
  and
  [`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/modify.md)
  give users control over table footnotes and spanning headers.
  ([\#464](https://github.com/ddsjoberg/gtsummary/issues/464))

- Introducing `as_huxtable()`! The function converts gtsummary objects
  to {huxtable} objects. {huxtable} supports indentation, footnotes, and
  spanning headers with Word, HTML, and PDF output.
  ([\#469](https://github.com/ddsjoberg/gtsummary/issues/469))

- New function
  [`add_stat()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat.md)!
  Add a new column of any statistic to a
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  table ([\#495](https://github.com/ddsjoberg/gtsummary/issues/495))

#### User-facing Updates

- The following columns in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  are now available to print for both continuous and categorical
  variables: total number of observations `{N_obs}`, number of missing
  observations `{N_miss}`, number of non-missing observations
  `{N_nomiss}`, proportion of missing observations `{p_miss}`,
  proportion of non-missing observations `{p_nomiss}`.
  ([\#473](https://github.com/ddsjoberg/gtsummary/issues/473))

- Improved appearance of default `as_flextable()` output
  ([\#499](https://github.com/ddsjoberg/gtsummary/issues/499))

- Added `tbl_cross(margin=)` argument to control which margins are shown
  in the output table.
  ([\#444](https://github.com/ddsjoberg/gtsummary/issues/444))

- The missing values are now included in the calculation of p-values in
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md).

- Messaging about statistical methods used has been added for
  [`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md),
  [`add_q()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_q.md),
  and
  [`combine_terms()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/combine_terms.md).
  ([\#471](https://github.com/ddsjoberg/gtsummary/issues/471))

- Added `include=` argument to
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md).
  The preferred syntax for p-values with correlated data is now
  `tbl_summary(..., include = -group_var) %>% add_p(group = group_var)`.
  The group variable is now no longer removed from the table summary.
  ([\#477](https://github.com/ddsjoberg/gtsummary/issues/477))

- [`add_stat_label()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_stat_label.md)
  function updated with `location=` and `label=` arguments to change the
  location of the statistic labels and to modify the text of the labels.
  `location = "row"` is now the default.
  ([\#467](https://github.com/ddsjoberg/gtsummary/issues/467))

- [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  function added the `group_header=` argument that groups the stacked
  tables and adds a horizontal header row between them.
  ([\#468](https://github.com/ddsjoberg/gtsummary/issues/468))

- Updated handling for interaction terms in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md).
  Interaction terms may now be specified in the `show_single_row=` and
  `label=` arguments.
  ([\#451](https://github.com/ddsjoberg/gtsummary/issues/451),
  [\#452](https://github.com/ddsjoberg/gtsummary/issues/452))

#### Internal Updates

- Improved error messaging when invalid statistics are requested in
  `tbl_summary(statistic=)`
  ([\#502](https://github.com/ddsjoberg/gtsummary/issues/502))

- All columns in
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  are now styled and converted to character. Previously, styling was
  applied to most columns, but there were a few that relied on default
  printing for the type of underlying data. This was ok to rely on this
  default behavior for
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md),
  but with the introduction of `as_flextable()` we needed to style and
  format each column to character. Potential to break some code in edge
  cases. ([\#493](https://github.com/ddsjoberg/gtsummary/issues/493))

- Handling of passed custom p-value functions in
  [`add_p.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_summary.md)
  has been improved with more careful handling of the environments from
  which the functions were passed. Other related updates were also made:

  - Users may pass their custom p-value function as a quoted string or
    bare.
  - The basic functions for calculating p-values, such as
    [`t.test()`](https://rdrr.io/r/stats/t.test.html) can now be passed
    directly e.g. `test = age ~ t.test`. We now perform a check match
    check for functions passed. If it is passed, we replace it with our
    internal version that returns the p-value and assigns the test name.
  - If a user passes a custom function, and it’s not the proper form
    (i.e. a named list return object) AND the function returns a single
    numeric value, we assume that is the p-value and it’s added to the
    gtsummary table.

- Updated the gtsummary core script, `utils-gtsummary_core.R`, to refer
  to all non-base R functions with the `pkg::` prefix, so other packages
  that copy the file don’t need to import the same functions as
  {gtsummary} in the NAMESPACE. Now they just need to depend on the same
  packages. ([\#454](https://github.com/ddsjoberg/gtsummary/issues/454))

#### Bug Fixes

- Bug fix for
  [`inline_text.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.tbl_summary.md)
  when categorical variable contained levels with empty strings. There
  is still an issue if a user tries to select the empty string, however.

- Fixed bug where some variables in
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md)
  defaulted to dichotomous instead of showing as categorical
  ([\#506](https://github.com/ddsjoberg/gtsummary/issues/506))

- Bug fix when using a `tbl_summary(by=)` with missing observations in
  `by=` followed by
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_overall.md)

- Bug fix where values `">0.9"` were incorrectly made bold using
  [`bold_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_p.md).
  ([\#489](https://github.com/ddsjoberg/gtsummary/issues/489))

- Bug fix for `as_flextable()`.
  ([\#482](https://github.com/ddsjoberg/gtsummary/issues/482))

  - Added a formatting function to all numeric columns to force
    conversion to character.
  - Spanning headers were being printed in alphabetical order! Update to
    preserve the ordering.

## gtsummary 1.3.0

CRAN release: 2020-04-17

#### New Functions

- Introducing
  [`tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_cross.md),
  [`add_p.tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.tbl_cross.md),
  and
  [`inline_text.tbl_cross()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.tbl_cross.md)!
  Easily construct cross tabulations of two variables.

- Introducing
  [`tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_survfit.md)
  and
  [`inline_text.tbl_survfit()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.tbl_survfit.md)!
  These will eventually replace `tbl_survival()`, which will no longer
  be encouraged. The new functions follow the structural guidelines of a
  {gtsummary} object and can be merged and stacked with any other
  {gtsummary} object
  ([\#280](https://github.com/ddsjoberg/gtsummary/issues/280))

- Introducing `as_flextable()`! The function converts gtsummary objects
  to {flextable} objects. {flextable} is a great option when using R
  markdown with Microsoft Word output. {flextable} supports indentation,
  footnotes, and spanning headers with Word, HTML, and PDF output.

- Introducing
  [`as_kable_extra()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable_extra.md)!
  The function converts gtsummary objects to {kableExtra} objects.
  {kableExtra} supports indentation, footnotes, and spanning headers
  with HTML and PDF output.
  ([\#394](https://github.com/ddsjoberg/gtsummary/issues/394))

#### User-facing Updates

- Updated default print engine for {gtsummary} objects. {gt} is the
  default printer for the R console and R markdown documents with HTML
  output. PDF, RTF, and Word output default to using {kable} with a note
  referring users to a new vignette explaining why {gt} was not used.
  ([\#395](https://github.com/ddsjoberg/gtsummary/issues/395),
  [\#396](https://github.com/ddsjoberg/gtsummary/issues/396))

- Updated
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  custom p-value description to NOT require double escape characters for
  quotation marks
  ([\#361](https://github.com/ddsjoberg/gtsummary/issues/361))

- Created structure and enumerated list of unit tests each vetted model
  must pass, and added the tests
  ([\#383](https://github.com/ddsjoberg/gtsummary/issues/383))

#### Internal Updates

- Each gtsummary object has an associated `.$table_header`. The code
  needed to print a table with either gt or kable was previously a mix
  of information stored in the `table_header`, and code manually added
  to the `gt_calls` or `kable_calls` object. Now, all the information
  needed to print a table is stored in `table_header`. This has the
  advantage that any updates to the printing will now require an update
  to `table_header` only, and we no longer need to update the tibble,
  kable and gt calls.
  ([\#412](https://github.com/ddsjoberg/gtsummary/issues/412),
  [\#414](https://github.com/ddsjoberg/gtsummary/issues/414))

- The {gt} package is now released on CRAN, and we’ve updated to depend
  on the CRAN version instead of the version on GitHub. This also
  resulted in significant updates throughout the documentation and code.
  For example, we no longer provide instructions for installing {gt}, or
  include internal checks if {gt} is installed.
  ([\#420](https://github.com/ddsjoberg/gtsummary/issues/420))

- Several functions have been made general, and may now be applied to
  any {gtsummary}-like object. Functions updated are
  [`add_q()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_q.md),
  [`bold_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_p.md),
  [`sort_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_filter_p.md),
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md),
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md),
  [`bold_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  [`bold_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  [`italicize_labels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md),
  and
  [`italicize_levels()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/bold_italicize_labels_levels.md).
  ([\#434](https://github.com/ddsjoberg/gtsummary/issues/434),
  [\#429](https://github.com/ddsjoberg/gtsummary/issues/429),
  [\#373](https://github.com/ddsjoberg/gtsummary/issues/373))

- In
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  we previously printed the estimate, confidence interval, and p-value
  for all models. But some models don’t have associated methods for
  calculating the p-value or the confidence intervals. In this update,
  we now print the p-value if `tidy()` returns a `"p.value"` column.
  Similarly, the confidence interval is printed if `tidy()` returns
  `"conf.low"` and `"conf.high"` columns.
  ([\#391](https://github.com/ddsjoberg/gtsummary/issues/391),
  [\#404](https://github.com/ddsjoberg/gtsummary/issues/404))

#### Bug Fixes

- Bug fix when data frame passed to
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  with a single column
  ([\#389](https://github.com/ddsjoberg/gtsummary/issues/389))

- In
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  passing an ordered factor in the `by=` argument no longer causes as
  error. ([\#453](https://github.com/ddsjoberg/gtsummary/issues/453))

## gtsummary 1.2.6

CRAN release: 2020-02-13

- Bug fix for random effects regression model where coefficients were
  not exponentiated when requested. Using
  [`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  rather than
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  resolved issue.

## gtsummary 1.2.5

CRAN release: 2020-02-11

#### Documentation

- Updated documentation and README to improve readability, added more
  cross-linking across pages, added search terms to help users find our
  package, and added gif demonstrations
  ([\#340](https://github.com/ddsjoberg/gtsummary/issues/340))

- README images now build differently for website vs GitHub markdown to
  accommodate different output formats

#### Breaking changes

- Removed deprecated `fmt*_()` and `cols_label_summary()` functions
  ([\#365](https://github.com/ddsjoberg/gtsummary/issues/365))

- Functions `tbl_summary_()` and `add_p_()` have been deprecated because
  the `by=` and `group=` arguments now accept strings
  ([\#250](https://github.com/ddsjoberg/gtsummary/issues/250))

#### Syntax

- Package-wide update allowing arguments that accept variable names to
  accept bare/symbol inputs, character inputs, stored character inputs,
  and tidyselect helpers. When passing a single variable, the
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html) function
  wrapper is no longer required.
  ([\#250](https://github.com/ddsjoberg/gtsummary/issues/250))

  ``` r
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

- Updated
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  to allow flexible tidyselect inputs and improved error messaging for
  arguments `x` and `y`
  ([\#249](https://github.com/ddsjoberg/gtsummary/issues/249))

- After the input updates in
  [\#250](https://github.com/ddsjoberg/gtsummary/issues/250), the
  `exclude=` argument appearing in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md),
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md),
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md),
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md),
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md),
  and
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  was redundant. The `exclude=` argument is now deprecated. Use
  `include=` instead, with tidyselect syntax.
  ([\#331](https://github.com/ddsjoberg/gtsummary/issues/331))

- Functions
  [`all_categorical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_dichotomous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  and
  [`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)
  may now be used in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  argument `type=`
  ([\#256](https://github.com/ddsjoberg/gtsummary/issues/256))

#### User-facing improvements

- New `pattern=` argument in
  [`inline_text.tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.tbl_summary.md).
  Previously, we could only grab the entire cell from a
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  with
  [`inline_text()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/inline_text.md),
  and now we can get any single statistic reported
  ([\#254](https://github.com/ddsjoberg/gtsummary/issues/254))

- Improved messaging for users who use
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) to print
  gtsummary tables, and users who have not yet installed the {gt}
  package ([\#347](https://github.com/ddsjoberg/gtsummary/issues/347))

- New function
  [`combine_terms()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/combine_terms.md)
  allows users to combine multiple independent variables in a regression
  model into a single line after
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md).
  The single line does not report regression coefficients, rather a
  single p-value from the
  [`anova()`](https://rdrr.io/r/stats/anova.html) function.
  ([\#310](https://github.com/ddsjoberg/gtsummary/issues/310))

- In the regression modeling functions
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  and
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md),
  the users are presented an informative error message when the tidier
  fails
  (e.g. [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html))
  alerting them to the location of the error so they may address the
  issue ([\#337](https://github.com/ddsjoberg/gtsummary/issues/337),
  [\#338](https://github.com/ddsjoberg/gtsummary/issues/338))

- For each release of gtsummary, we now can make reference to the
  version of gt our release coincides with. The commit SHA for gt is now
  saved in an object called `gt_sha`, and the version of gt can be
  installed with `remotes::install_github("rstudio/gt", ref = gt_sha)`
  ([\#329](https://github.com/ddsjoberg/gtsummary/issues/329))

- Created “style” family of functions

- Improved error messaging in `tidyselect_to_list()`
  ([\#300](https://github.com/ddsjoberg/gtsummary/issues/300))

#### Other features and fixes

- Updated class detection to use
  [`inherits()`](https://rdrr.io/r/base/class.html), and added secondary
  class of `"gtsummary"` to all objects. This allows users to create
  their own cobbled/custom gtsummary objects while utilizing the
  gtsummary print functions
  ([\#249](https://github.com/ddsjoberg/gtsummary/issues/249))

- [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  will now summarize columns of class `difftime`
  ([\#343](https://github.com/ddsjoberg/gtsummary/issues/343))

- Infrastructure update to the way styling/formatting functions are
  returned. Styling functions are now returned as a column in the
  `.$table_header` tibble. The update simplifies handling of these
  styling functions in
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  and
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md).
  ([\#298](https://github.com/ddsjoberg/gtsummary/issues/298),
  [\#299](https://github.com/ddsjoberg/gtsummary/issues/299))

- Bug fix for
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  when variables were all NA
  ([\#344](https://github.com/ddsjoberg/gtsummary/issues/344))

- The data summary function
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  now uses probabilities rather than counts to calculate expected cell
  counts to avoid an error when working with large data sets.
  ([\#341](https://github.com/ddsjoberg/gtsummary/issues/341))

- Cubic spline terms are now accurately matched to a variable name/term
  ([\#312](https://github.com/ddsjoberg/gtsummary/issues/312))

- Bug fix when non-standard evaluation arguments were passed in
  `method.args=` argument of
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  ([\#322](https://github.com/ddsjoberg/gtsummary/issues/322))

- Updates after the gt package deprecated `gt::cells_data()` in favor of
  [`gt::cells_body()`](https://gt.rstudio.com/reference/cells_body.html).
  Check added to
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
  ensuring a version of gt with
  [`gt::cells_body()`](https://gt.rstudio.com/reference/cells_body.html)
  in its NAMESPACE

- Lowered minimum required version of R to v3.4
  ([\#356](https://github.com/ddsjoberg/gtsummary/issues/356))

- Removed {broom.mixed} dependency as the {broom} package contained all
  necessary tidiers
  ([\#354](https://github.com/ddsjoberg/gtsummary/issues/354))

## gtsummary 1.2.4

CRAN release: 2019-12-16

- Bug fix in
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)
  where column header did not match statistics presented when certain
  levels of the `by=` variable are entirely missing in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  ([\#304](https://github.com/ddsjoberg/gtsummary/issues/304))

- Updated the trial example data set `"trt"` variable to be `"Drug A"`
  and `"Drug B"` instead of `"Placebo"` and `"Drug"`

- Improved messaging to users when an error or warning occurs while
  calculating a p-value in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md).
  Also, p-values are no longer omitted from output when a warning is
  encountered during their calculation
  ([\#283](https://github.com/ddsjoberg/gtsummary/issues/283))

- Added `tidy_fun=` argument to
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  and
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  allowing users to pass tidiers that are not in the {broom} and
  {broom.mixed} packages
  ([\#247](https://github.com/ddsjoberg/gtsummary/issues/247))

## gtsummary 1.2.3

CRAN release: 2019-11-12

- [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  now accepts an `x=` argument to build univariate regression models
  where the covariate `x` remains the same while models are built the
  with remaining variables as the outcome
  ([\#294](https://github.com/ddsjoberg/gtsummary/issues/294))

- Internal updates to the way {gt} is installed during CRAN checks.

- Bug fix when stacking `tbl_summary` objects with calculated p-values.

## gtsummary 1.2.2

CRAN release: 2019-11-10

#### New Features

- `tbl_summary` objects may be stacked and merged with
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  and
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  ([\#230](https://github.com/ddsjoberg/gtsummary/issues/230),
  [\#255](https://github.com/ddsjoberg/gtsummary/issues/255))

- The
  [`add_n()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_n.md)
  function now reports additional statistics: total N, non-missing N,
  missing N, and their percentages. The `missing =` argument has been
  deprecated in favor of the `statistic =` argument.
  ([\#237](https://github.com/ddsjoberg/gtsummary/issues/237))

- Users may now pass a list of formulas, named lists, or a combination
  of both ([\#251](https://github.com/ddsjoberg/gtsummary/issues/251))

- Users can add an option to their script to append any {gt} calls when
  a {gtsummary} object is printed: `gtsummary.as_gt.addl_cmds`

- Added `include =` and `exclude =` arguments to
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  ([\#240](https://github.com/ddsjoberg/gtsummary/issues/240))

- Added standard evaluation variants, `tbl_summary_()` and `add_p_()`
  ([\#223](https://github.com/ddsjoberg/gtsummary/issues/223))

- Added
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  function that converts any {gtsummary} table to a tibble
  ([\#245](https://github.com/ddsjoberg/gtsummary/issues/245))

- New `show_single_row` argument in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  and
  [`tbl_uvregression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_uvregression.md)
  allows any binary variable to be printed on a single row. Previous
  argument `show_yesno` is now deprecated.
  ([\#220](https://github.com/ddsjoberg/gtsummary/issues/220))

#### Documentation

- Added a gallery of tables possible by merging, stacking, and modifying
  {gtsummary} arguments
  ([\#258](https://github.com/ddsjoberg/gtsummary/issues/258))

- Added a vignette documenting each global option that can be set in
  {gtsummary}
  ([\#289](https://github.com/ddsjoberg/gtsummary/issues/289))

- Added {lifecycle} badges to mark deprecated and experimental functions
  ([\#225](https://github.com/ddsjoberg/gtsummary/issues/225))

#### Other Updates

- The `by =` column in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  now has missing variables dropped rather than halting with error
  ([\#279](https://github.com/ddsjoberg/gtsummary/issues/279))

- Bug fix in [`vars()`](https://dplyr.tidyverse.org/reference/vars.html)
  selection where only first variable listed was being selected
  ([\#259](https://github.com/ddsjoberg/gtsummary/issues/259))

- Bug fix where logical variable labels printed as `NA` in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  ([\#248](https://github.com/ddsjoberg/gtsummary/issues/248))

- [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  now interprets `tab_spanner =` text with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html)
  ([\#253](https://github.com/ddsjoberg/gtsummary/issues/253))

- No longer checking outcome variable name for consistency in
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)—only
  checking independent variable names
  ([\#287](https://github.com/ddsjoberg/gtsummary/issues/287))

- Improved error messaging for
  [`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  ([\#243](https://github.com/ddsjoberg/gtsummary/issues/243))

- Removed
  [`gt::cols_merge()`](https://gt.rstudio.com/reference/cols_merge.html)
  function ahead of the {gt} package [PR
  355](https://github.com/rstudio/gt/pull/355) that changes the
  [`cols_merge()`](https://gt.rstudio.com/reference/cols_merge.html) API
  ([\#222](https://github.com/ddsjoberg/gtsummary/issues/222))

- Updated API for using custom functions to calculate p-values in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md).
  User now may reference a custom function by its name.
  ([\#226](https://github.com/ddsjoberg/gtsummary/issues/226))

- Removed legacy support for tidyr version less than 1.0.0
  ([\#235](https://github.com/ddsjoberg/gtsummary/issues/235))

## gtsummary 1.2.1

CRAN release: 2019-08-20

- Vignettes no longer install the {gt} package (required for CRAN check)
  ([\#217](https://github.com/ddsjoberg/gtsummary/issues/217))

- Added ability to name custom
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  tests ([\#213](https://github.com/ddsjoberg/gtsummary/issues/213))

## gtsummary 1.2.0

CRAN release: 2019-08-19

- Users can pass variable names in backticks
  ([\#212](https://github.com/ddsjoberg/gtsummary/issues/212))

- The `group =` argument in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  has been moved to
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  ([\#208](https://github.com/ddsjoberg/gtsummary/issues/208))

- Users can now write custom functions to calculate p-values in
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  ([\#194](https://github.com/ddsjoberg/gtsummary/issues/194))

- In
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  the `by =` argument accepts a bare variable name instead of the
  variable name passed as a string
  ([\#193](https://github.com/ddsjoberg/gtsummary/issues/193))

- Added support for column, row, and cell percentages in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  ([\#181](https://github.com/ddsjoberg/gtsummary/issues/181))

- Users can now set default p-value formatting functions, regression
  coefficient formatting functions, default level for confidence
  intervals, and formatting functions in `tbl_survival()`
  ([\#120](https://github.com/ddsjoberg/gtsummary/issues/120))

- The {gt} package is no longer a required dependency. If {gt} is not
  installed, tables will be printed with
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html). The
  [`as_kable()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_kable.md)
  function was added to the package as well.
  ([\#180](https://github.com/ddsjoberg/gtsummary/issues/180))

- The function
  [`as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
  now has `include =` and `exclude =` arguments

- Updated some function names to be the same as they were in the first
  version

``` r
    bold_p()            <-  tab_style_bold_p()  
    bold_labels()       <-  tab_style_bold_labels()  
    bold_levels()       <-  tab_style_bold_levels()  
    italicize_labels()  <-  tab_style_italicize_labels()  
    italicize_levels()  <-  tab_style_italicize_levels()  
```

- Passing named lists in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  is now defunct.

- [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  fix for `tbl_uvregression` objects
  ([\#175](https://github.com/ddsjoberg/gtsummary/issues/175))

- Option to exclude some variables from testing when using
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  ([\#164](https://github.com/ddsjoberg/gtsummary/issues/164))

- Updates after {gt} package renamed `cells_style()` to
  [`cell_text()`](https://gt.rstudio.com/reference/cell_text.html)
  ([\#78](https://github.com/ddsjoberg/gtsummary/issues/78))

## gtsummary 1.1.1

- Modified
  [`tbl_merge()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_merge.md)
  to accommodate
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  object ([\#167](https://github.com/ddsjoberg/gtsummary/issues/167))

- Bug fix with incorrect column order in
  [`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_summary.md)
  with 10+ levels of by variable
  ([\#166](https://github.com/ddsjoberg/gtsummary/issues/166))

## gtsummary 1.1.0

- Added {tidyselect} and {gtsummary} variable select functions
  ([\#146](https://github.com/ddsjoberg/gtsummary/issues/146))

- Added
  [`tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_stack.md)
  function ([\#152](https://github.com/ddsjoberg/gtsummary/issues/152))

- Bug fix for dichotomous yes/no variables in `tbl_summary`
  ([\#158](https://github.com/ddsjoberg/gtsummary/issues/158))

- Changed `add_comparison()` and `add_global()` to
  [`add_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_p.md)
  and
  [`add_global_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_global_p.md)
  ([\#143](https://github.com/ddsjoberg/gtsummary/issues/143))

- Added
  [`sort_p()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/sort_filter_p.md)
  function ([\#105](https://github.com/ddsjoberg/gtsummary/issues/105))

- Allow unobserved values to serve as the level for dichotomous
  variables ([\#149](https://github.com/ddsjoberg/gtsummary/issues/149))

- Bug fix in
  [`add_nevent()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/add_nevent_regression.md)
  when formula was passed to [`glm()`](https://rdrr.io/r/stats/glm.html)
  as a string
  ([\#148](https://github.com/ddsjoberg/gtsummary/issues/148))

- Added returned `call_list` to some functions without it previously
  ([\#137](https://github.com/ddsjoberg/gtsummary/issues/137))

- Corrected name of call list in returned
  [`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/tbl_regression.md)
  results ([\#128](https://github.com/ddsjoberg/gtsummary/issues/128))

- `tbl_survival()`: bug fix when upper bound of CI was not able to be
  estimated ([\#134](https://github.com/ddsjoberg/gtsummary/issues/134))

- `tbl_survival()`: the `groupname` column name has been changed to
  `label_level`
  ([\#133](https://github.com/ddsjoberg/gtsummary/issues/133))

## gtsummary 1.0.0

First release since major re-factoring. The {gt} package is now used as
the print engine to create all tables. Some function names have been
modified to be more in line with other {gt} function calls, and
additional functions have been added. The API for some functions has
also been updated. Review documentation and vignettes for details.

#### Updated Function Names

``` r
    tbl_summary()       <-  fmt_table1()  
    tbl_regression()    <-  fmt_regression()  
    tbl_uvregression()  <-  fmt_uni_regression()  
    style_pvalue()      <-  fmt_pvalue()  
    style_percent       <-  fmt_percent()  
    style_ratio         <-  fmt_beta()  
```

#### New Functions

``` r
    tbl_survival()          as_gt()  
    tbl_merge()             style_sigfig()  
    add_nevent()            gtsummary_logo()
```

## gtsummary 0.1.0

CRAN release: 2019-05-10

First version of package available
[here](https://github.com/ddsjoberg/gtsummary-v0.1).
