# flextable 0.6.5

## new features

* add function `as_equation` for 'MathJax' equations.
* add argument `text_after` to function `flextable_to_rmd` to let append 
any text to the produced flextable.

# flextable 0.6.4

## new features

* export of `chunk_dataframe` for those who want to create functions that work with 
 `as_paragraph`.
* in R Markdown for Word, bookmarks are now added to captions when output format is from bookdown
* shadow hosts HTML elements get the class `flextable-shadow-host`.
* `set_flextable_defaults` now accept argument `padding` that set values for padding 
top, bottom, left and right.
* new functions `colorize`, `as_highlight`
* functions `nrow_part` and `ncol_keys` are now exported

## Issues

* fix for minibar when all values are equal to zero (thanks to Martin Durian)
* fix URLs formatted incorrectly in Word and PowerPoint (thanks to David Czihak)

# flextable 0.6.3

## new features

* `compose` has a new argument `use_dot` to let 
use `.` and loop over columns 
* new function `init_flextable_defaults()`
* inst/mediasrc/man-figures.R can also be used for visual testing 
with `git diff`

## Issues

* fix line spacing with pdf output
* Now `colformat_num` calls the `format` function on the numeric values 
(integer and float) which are therefore displayed as in console R. This function
is used during the creation of a flextable so that by default the content of the
cells is the same as that displayed in console R.

# flextable 0.6.2

## changes

* new documentation! See at https://ardata-fr.github.io/flextable-book/


## new features

* `merge_v` has a new argument `combine` to let use j columns 
be used as a single value (all values are pasted).
* new function `add_body` for adding rows into a flextable body
* new function `colformat_image` for images in flextable
* new method `as_flextable` for `gam` models
* function `set_flextable_defaults` gained 4 new arguments 
 `post_process_pdf`, `post_process_html`, `post_process_docx` 
 and `post_process_pptx` to enable flextable post-treatments 
 conditionned by the output format.
* new helper functions `fp_text_default` and `fp_border_default`.


## Issues

* fix encoding issue with Windows platforms
* bring back caption into the table
* fix overlapping issue with hline_top #244
* fix `\n` and `\t` usage for pdf

# flextable 0.6.1

## new features

* HTML flextable are now isolated from the document CSS (except caption which 
is copied before).

## Issues

* correction of latex tables which resulted in a centering of the following text.
* minor correction for density graphs inserted in tables
* fix suffix/prefix usage in colformat_* functions

## changes

* drop defunct functions.


# flextable 0.6.0

## new features

* flextable now supports PDF/latex output format.
* new function `highlight()` for text highlighting color
* new function `set_flextable_defaults()` to set some default 
formatting properties, i.e. default border color, font color, padding, decimal.mark ...
* `save_as_docx` gained a new argument `pr_section` to define page 
layout with section properties, `save_as_html` can now output more than 
a single table.
* `colformat_` functions now use default values and filter columns that 
are irrelevant (i.e. if colformat_num, only numeric values are formatted). 
Also, new `colformat_` functions have been implemented (`colformat_date`, `colformat_datetime`
and `colformat_double`).
* new functions `plot_chunk` and `gg_chunk` to add miniplots or ggplots into a flextable

## changes

* defunct of `ph_with_flextable()`
* use pandoc's raw attribute when possible within "R Markdown" documents.

## Issues

* fix bug in HTML output with invalid css when locale makes decimal separator not `.`
* `fix_border_issues` is the last instruction of all theme functions so that borders 
are corrected if some cells have been merged.
* caption was always printed in bookdown and now it's conditionned by 
testing if `tab_props$cap` has a value.
* fix missing tfoot tag in HTML output

# flextable 0.5.11

## Changes

* HTML code is now minimized as CSS styles are now used instead of inline CSS.

## new features

* save_as_html now accepts argument `encoding`
* line spacing (for Word and PowerPoint) or line height (for  HTML) can now be
defined with function `line_spacing()` (or with function `style()`).

## Issues

* selection when i or j was integer(0) was resulting to all rows, it's now fixed. To
select all rows or columns, use `i = NULL` or `j = NULL`, to select none, `i = integer(0)` or
`j = integer(0)`.
* tab were not displayed when output was HTML

# flextable 0.5.10

## new features

* flextable captions in Word can be auto-numbered and bookmarked
* function `footnote` is now able to add inline footnotes
* support for bookdown references
* new as_flextable methods for lm and glm objects and xtable (replacing `xtable_to_flextable()`)
* new sugar function `continuous_summary()`: summarize continuous columns in a flextable
* function `autofit` can now use only some parts of the tables. This allows
for example to no longer have gigantic columns by not taking into account the "footer" part that is often composed of long texts.
* bookdown and xaringan HTML outputs should now be rendered as expected - table css has been overwritten.
* new function `set_table_properties` lets adapt flextable size as "100%", "50%" of the available width
for Word and HTML.

## Changes

* manual pages have been improved and illustrations are added
* `bg()` and `color()` now accept functions (i.e. `scales::col_numeric()`)

# flextable 0.5.9

## Changes

* defunct of `display()`
* rename arg 'formater' to 'formatter' of `as_chunk` (#152)

## Internal

* drop `officer::fp_sign` importation that was not used anymore so that officer can
 drop digest dependency.


# flextable 0.5.8

## Changes

* deprecation of `display()`.
* defunct of `ph_with_flextable_at()`
* function `docx_value()` has new arguments `ft.align`, `ft.split` and `tab.cap.style`
* function `htmltools_value()` has a new argument `ft.align`

## new features

* new function `flextable_html_dependency` to get flextable htmltools::htmlDependancy. This is
necessary to output flextables in html R Markdown documents from loop or other nested operations.


## Issues

* fix issue #188, add_rows error that came with version 0.5.7


# flextable 0.5.7

## new features

* new suger functions `save_as_docx`, `save_as_pptx` that lets users export flextable objects
  to PowerPoint or Word documents.

## Changes

* merge_v can use hidden columns.
* new function `hrule` to control how row heights should be
  understood (at least, auto, exact)
* Allow unused values in set_header_labels - PR #172 from DanChaltiel
* deprecation of ph_with_flextable_at, ph_with_flextable will be deprected in the next release

## Issues

* fix issue #180, background color of runs transparency issue with googlesheet
* fix issue #157, issue with rotate and HTML output


# flextable 0.5.6

## Issues

* force officer >= 0.3.6
* fix rounding issue for css borders


# flextable 0.5.6

## new features

* new function `lollipop` that lets users add mini lollipop chart to flextable
  (kindly provided by github.com/pteridin)
* function `proc_freq` got a new argument `weight` to enable weighting of results.
* function `as_flextable.grouped_data()` has now an argument `hide_grouplabel` to
let not print the group names.

## Issues

* let footnotes symbols be affected by style functions (related to #137)
* enable usage of 'webshot2' instead of 'webshot'. It enable better screenshots. It
  can be specified with argument `webshot` in function `save_as_image` or with
  chunk option `webshot="webshot2"`.

# flextable 0.5.5

## new features

* new function `docx_value` to let display flextables from non top level
 calls inside R Markdown document.
* ph_with method for flextable object. This enable `ph_location*` usage
  and make placement into slides easier.
* new function `fit_to_width` to fit a flextable to a maximum width
* `set_caption` can now be used with R Markdown for Word document and caption
  style can be defined with chunk option `tab.cap.style`.

## Issues

* fix issue with `save_as_image` with R for Windows

# flextable 0.5.3

## new features

* new functions to render flextable in plot (see `plot`), as an image (see `save_as_image`)
  and raster for ggplot2 (see `as_raster`).
* new function `footnote` to ease footnotes management
* colformat functions are suporting i argument now for rows selection.

# flextable 0.5.2

## new features

* new function `valign` to align vertically paragraphs in cell
* new function `proc_freq` that mimic SAS proc freq provided by Titouan Robert.
* new function `linerange` to produce mini lineranges.

## Issues

* fix issue with `set_footer_df`

# flextable 0.5.1

## Issues

* fix issue with font colors in powerpoint
* fix issues with colors for Windows RStudio viewer


## new features

* new themes functions `theme_alafoli()` and `theme_vader()`
* new functions `align_text_col()` and `align_nottext_col()` to align
  columns by data type
* new functions `merge_h_range()` to merge a set of columns row by row
* new functions `fix_border_issues()` fix issues with borders when cells are merged
* new functions `add_header_row()`, `add_footer_row()`, `add_header_lines()`
 and `add_footer_lines()` to add easily data in header or footer.
* new generic function `as_flextable()` to let develop new flextable functions
* new function `as_grouped_data()` and its method `as_flextable()` to
  create row titles to separate data in a flextable.


# flextable 0.5.0

## Improvement

* new arguments `values` for functions `set_header_labels` and `set_formatter`
* styles functions now support injection of more than a single value
* this version a big refactoring and got rid of R6

## new features

* new function `compose` that will replace `display`
* new function `set_caption` only available for html output

# flextable 0.4.7

## new features

* `knit_print()` can be used with rmarkdown when rendering to PowerPoint.

## Issues

* fix issue with `regulartable` and logical columns

# flextable 0.4.6

## new features

* a new helper function `body_replace_flextable_at_bkm` to
  replace a bookmarked paragraph by a flextable.
* new functions `colformat_*` to make content formatting easier.
  It also deals with NA.

## Improvement

* Documentation `format.flextable` method so that users can create
  their components.
* new knitr chunk options `ft.align` to align tables in
  `rmarkdown::word_document` and `ft.split` to activate Word
  option 'Allow row to break across pages'.

## Issues

* fix issue (unordered and duplicated chunk of text) in function `display()`


# flextable 0.4.5

## Improvement

* flextable will not be split across rows at a page break in Word documents.

## Issues

* fix border rendering with `vline()`
* empty data.frame are no more generating an error

# flextable 0.4.4

## new features

* Soft return `\n` is now supported. Function `autofit` and `dim_pretty` do not
  support soft returns and may return wrong results (\n will be considered as "").
* format function for flextable objects.

## Issues

* fix border rendering with `border_outer()`


# flextable 0.4.3

## new features

* new functions: `hyperlink_text()` to be used with `display`, `font()`
* new functions `hline*()` and `vline*()` and many new helper functions
  to be used instead of borders.

## Improvement

* manuals have been refactored


## Issues

* fix display issue when a cell was containing NA


# flextable 0.4.2

## new features

* new function `xtable_to_flextable()` that is returning
  a flextable from an xtable object.
* function `htmltools_value()` is exported for shiny applications.

# flextable 0.4.1

## new features

* flextables have now a footer part


# flextable 0.4.0

## new features

* new function `knit_print()` to render flextable in rmarkdown.

## Changes

* function tabwid() is deprecated in favor of a knit_print implementation.
* list of dependencies has been reduced.

# flextable 0.3

## new features

* new function `regulartable`, faster and simpler than `flextable`

## Issues

* characters <, > and & are now html encoded
* fix columns widths when output format is HTML

# flextable 0.2

## new features

* new function `ph_with_flextable_at` to add a flextable at any position in a slide.
* new function `merge_at` is providing a general way of merging cells.
* new theme function: `theme_box()`

## Changes

* function display() now works with a mustache template

## Issues

* fix fontsize when part == "all"

