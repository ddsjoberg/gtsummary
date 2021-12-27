
# huxtable 5.4.0

* New behaviour: setting `colspan()` or `rowspan()` overwrites the content of
  cells that have been shadowed. 
  
  ```r
  
  ht <- hux(c(1, 1), c(2, 2), c(3, 3))
  ht <- set_all_borders(ht)
  colspan(ht)[1, 1] <- 3
  
  # old behaviour                
  ht[, c(2, 1, 3)]
  ##   +--------------------------+
  ##   |                  2       |
  ##   +--------+--------+--------+
  ##   |      2 |      1 |      3 |
  ##   +--------+--------+--------+

  # new behaviour
  ht[, c(2, 1, 3)]
  ##   +--------------------------+
  ##   |                  1       |
  ##   +--------+--------+--------+
  ##   |      2 |      1 |      3 |
  ##   +--------+--------+--------+
  ```
  
* New option `huxtable.latex_siunitx_align` allows you to use the LaTeX
  `siunitx` package to handle decimal point alignment. This is `FALSE` by
  default.
* Bugfix: centre alignment was not working in `print_screen()`.
* Bugfix: failure in `to_md()` with recent versions of `stringi` package.
* Bugfix: repeating a single row in a subset, like `ht[c(1, 1, 2, 3), ]`,
  was setting `colspan = 2` on the repeated row.
* Bugfix: zero-argument subset replacement like `ht[] <- ...` wasn't working.


# huxtable 5.3.0

* Improve decimal alignment in LaTeX when `align(ht) == "."`. This may change
  the appearance of some documents.
* Allow `tidy_override()` to extend columns of `tidy` and `glance`.
* Bugfix: #196 `^` was giving errors in LaTeX.


# huxtable 5.2.0

* Add `table_environment` property so you can use e.g. `"table*"` in TeX.
* Bugfix: `print_screen(h, colnames = FALSE)` didn't print a final newline.
* Bugfix: italic from markdown was being printed as underlined in TeX.
* Minor test update for compatibility with broom.


# huxtable 5.1.1

* Minor test update for compatibility with broom.
* Fixes for R 4.1.0.


# huxtable 5.1.0

* `as_flextable()` now exports markdown in cells to RTF, and to Word with the 
  help of the optional `ftExtra` package. Thanks @atusy for adding this feature.
  
* Improvements to markdown screen export. This now uses the optional `fansi` 
  package.
  
* New feature: `as_Workbook()` gains `start_row` and `start_col` arguments, to 
  write a huxtable into an Excel worksheet starting at a particular row or column.
  
* New feature: `huxreg()` gains a `glance_args` argument to pass arguments to 
  `glance()`.
  
* New feature: `options(huxtable.long_minus = TRUE)` will try to use long
  minus signs before numbers. The default is `FALSE`. It will probably become
  `TRUE` in a future version.
  
* Bugfix: `insert_row/column(..., after = 0)` was unsetting table properties.

* Bugfix: unicode characters above 32767 were incorrectly represented in RTF.
  Thanks @kaigu1990.
  
* Bugfix: columns were being collapsed in `as_Workbook()`. 

* Bugfix: `style_cells` didn't work unless huxtable was on the search path.

* Bugfix: `merge_repeated_rows` merged `NA` rows incorrectly.

* Bugfix: number format was not set correctly in `huxreg()`'s `note`.

* Bugfix: in `huxreg()`, `tidy_args` threw an error if the first argument to
  `tidy()` was a named list.
  
* Bugfix: `tidy_replace()` was broken.

* Clearer error messages for `tidy_override()` when `extend = FALSE`. In future,
  `extend` will probably default to `TRUE`.

## Other news:

* Huxtable received its first Patreon sponsor! Thanks to Ross Mattheis.


# huxtable 5.0.0

Huxtable 5.0.0 brings numerous changes. For a more user-friendly introduction,
see https://hughjonesd.github.io/whats-new-in-huxtable-5.0.0.html.

## Breaking changes

* There are changes to LaTeX output. 
  - LaTeX `\tabcolsep` is now set to 0 within huxtable tables, while left and right
    padding should now take effect even when `wrap` is `FALSE`.
  - The default LaTeX table environment is now "tabular" unless `width` is set. 
    If `width` is set, it is "tabularx".
  - `wrap` only matters if `width` is set. Otherwise, cell wrapping is off.
  - the `\centerbox` macro from the LaTeX "adjustbox" package is used to centre
    tables. This should improve centring when tables are too wide.
  You may need to update the LaTeX "adjustbox" package to a recent
  version. `check_latex_dependencies()` can inform you about this.

* As previously signalled, `add_colnames` has now become `TRUE` by default in 
  `huxtable()` and `as_huxtable()`. Set `options(huxtable.add_colnames = FALSE)` 
  to go back to the old behaviour.
  
* Newlines in cell contents are now respected (in LaTeX, so long as 
  `wrap = TRUE` and `width` has been set).
  
* Huxtable borders have been reworked, fixing some longstanding bugs and
  adding new features. 
  - Borders are now automatically collapsed. For example:

    ```r
    jams %>% 
        set_right_border(everywhere, 1, 1) %>% 
        set_left_border(everywhere, 2, 0.4)
    ```
will set the border in between the columns of `jams` to `0.4`, overwriting
the previous value. This is more in line with what you would expect. 
For example, the following code now does what you probably want:

    ```
    jams %>% 
        set_rowspan(2, 1, 3) %>% 
        set_bottom_border(4, everywhere, 1)
    ##                 Type              Price  
    ##                 Strawberry         1.90  
    ##                                    2.10  
    ##                                    1.80  
    ##               ---------------------------
    ```

    instead of the old behaviour:

    ```
    jams %>% 
        set_rowspan(2, 1, 3) %>% 
        set_bottom_border(4, everywhere, 1)
    ##                 Type           Price  
    ##                 Strawberry      1.90  
    ##                                 2.10  
    ##                                 1.80  
    ##                            -----------
    ```

  - `set_left_border()`, `set_all_borders()` and friends all use a default value
    of 0.4. So to set a default border, write e.g.
  
    ```r
    as_hux(head(iris)) %>% 
          set_bottom_border(1, everywhere)
    ```

  - A new `brdr()` class encapsulates border thickness, style
    and colour. You can set all properties at once by writing, e.g.:
  
      ```r
      as_hux(jams) %>% 
            set_bottom_border(1, everywhere, brdr(1, "dotted", "darkgreen"))
      ```
  
    `left_border(ht)` and friends return a `brdr` object. To access the border
    thickness, write `brdr_thickness(left_border(ht))`.
  
* Various deprecated items have been removed:
  - The 3-argument form of `set_*`. Instead, use `map_*`. 
  - The `byrow` argument to `set_*`. Instead, use `map_*` and `by_cols()`.
  - `error_style` and `pad_decimal` arguments in `huxreg`. Use `error_format` and
    `align(hx) <- "."`.
  - The `where()`, `is_a_number()` and `pad_decimal()` functions. Use `map_*`
    functions, `! is.na(as.numeric(x))`, and `align(ht) <- "."`.
    
* Default padding has been increased to 6 points.

* By default, `width()` is now unset.

* By default, `wrap()` is now `TRUE`.

* `every()` has been renamed to `stripe()`, to avoid a clash with `purrr::every()`.
  `everywhere`, `evens` and `odds` are still the same.
  
* The little-used ability to set `copy_cell_props` to a character vector in
  `rbind.huxtable` and `cbind.huxtable` has been removed. You can still set it
  to `FALSE`.

* `add_rows()` and `add_columns()` now always call `rbind.huxtable()` 
  or `cbind.huxtable()` and return a huxtable.

* Huxtable no longer supports dplyr versions less than 0.7.0 (released mid-2017).

* `set_cell_properties()` has been renamed `style_cells()`. It is retained
  as a soft-deprecated alias.
  
* Various themes have been tweaked:
  - `theme_basic()` now has bold headers and no header column by default.
  - `theme_plain()` defaults to `position = "centre"`.
  - `theme_striped()` uses grey stripes, a white border, and subtler headers.
  - `theme_article()` has thinner borders.


## Other changes

* You can now use [markdown](https://commonmark.org/help/) within table cells. 
  - Use `set_markdown(ht, rows, cols)` to turn this on.
  - Or use the convenience function `set_markdown_contents()` to set cell 
    contents that will be interpreted as markdown. 
  - Markdown works for HTML and LaTeX. There's basic support for on-screen
    display.

* Huxtable now has the concept of header row and columns. 
  - By default, data frame column names will be headers.
  - To set other rows to be headers,
    use `set_header_rows(ht, row_numbers, TRUE)`.
    For columns, use `header_cols()` or `set_header_cols()`.
  - New functions `style_headers()`, `style_header_cols()`, and 
    `style_header_rows()` to set multiple properties on headers.
  - In themes, `header_row/col = TRUE` set the first row/col to a header,
    and style all header rows/cols.

* `set_bold()` and `set_italic()` now use a default value of `TRUE`. So you
  can write e.g.
  
  ```r
  as_hux(head(iris)) %>% 
        set_bold(1, everywhere)
  ```

* Console output in R now shows table position and caption position.

* By default, huxtable now sets labels from the current knitr chunk label, if 
  there is one. This is consistent with `kable()`. In bookdown, you can then do 
  e.g.

  ````
  Some iris species are shown in \@ref(tab:mytable):
  
  ```r
  as_hux(iris)
  ```
  ````

  Set `options(huxtable.autolabel = FALSE)` to turn off this behaviour.

* The one-argument form of `[` now works for huxtables just as it does for 
  data frames. For example, `ht[2:3]` selects columns 2 and 3.
  
* New functions `fmt_percent()` and `fmt_pretty()` for passing into
  `number_format()`:

  ```r
  jams$Sugar <-c ("Sugar content", 0.4, 0.35, 0.45)
  set_number_format(jams, -1, "Sugar", fmt_percent(1))
  ```

* `split_across()` and `split_down()` split a huxtable into a
  list of sub-tables. Headers can be automatically included.

* `restack_across()` and `restack_down()` split a huxtable, then join it back
  up. This is useful for making a table fit on a page.

* `merge_across()` and `merge_down()` merge an area of cells
  horizontally across rows, or vertically down columns.

* New functions `set_lr_borders()/_border_colors()/_border_styles()/_padding()`  
  set left and right borders and padding simultaneously.
  New functions `set_tb_borders()` etc. set top and bottom properties 
  simultaneously. There are `map_` equivalents of all of these.
  
* `set_outer_padding()` sets padding around a range of cells,
  similarly to `set_outer_borders()`.

* A new table-level property, `caption_width()`, allows you to set the width of 
  the caption. The default, `NA`, sets the width equal to the table width.
  
* There are two new themes: `theme_compact()` and `theme_bright()`.  

* For `huxreg()`, a new function `tidy_replace()` allows you to replace the 
  output of `tidy(x)` entirely.

* huxtable now only sets `options(huxtable.knit_print_df = TRUE)` if it is 
  attached, not if it is loaded.
  
* huxtable supports `dplyr::relocate()`, new in dplyr 1.0.0.

* Improvements to `as_flextable()`.

* Improvements to `quick_pptx()` (thanks @davidgohel).

* Bugfixes for `options(huxtable.use_fontspec = TRUE)`.

* Bugfix: `add_rownames = "string"` now works as promised.

* Bugfix: non-ASCII characters are now supported in RTF.

## Other news

* New versions of the [gtsummary](https://cran.r-project.org/package=gtsummary) 
  package will have an `as_huxtable()` method. 

* Package [texreg](https://cran.r-project.org/package=texreg) on CRAN includes a
  `huxtablereg()` function for creating a table of regression outputs.

# huxtable 4.7.1

* The [expss](https://github.com/gdemin/expss) package now supports export to
  huxtables.
* `by_quantiles()`, `by_equal_groups()` and `by_colorspace()` have gained a `colwise`
  argument, which calculates quantiles or colors separately for each column.
* Add caption support for `as_flextable()` (thanks @sjewo).


# huxtable 4.7.0

* Better error messages.
* New `merge_repeated_rows()` function: merge repeated rows into a single
  cell.
* New `fill` and `colspan/rowspan` arguments for `insert_row()/insert_column()`: 
  - `insert_row(ht, "blah", "", "", "", "", ...)` can be written
    `insert_row(ht, "blah", fill = "")`.
  - `colspan/rowspan` set `colspan/rowspan` of the first cell in the inserted
    row/column.


# huxtable 4.6.1

* Bugfix: right borders in wrong place when cells were merged.
* Bugfix: chinese characters were displaying wrongly in `to_screen()`.


# huxtable 4.6.0

* Set `options('huxtable.latex_use_fontspec')` to `TRUE` to use portable font 
  names in TeX documents, with the LaTeX "fontspec" package.
* Bugfix: attributes were being copied wrongly in subset assignment of huxtables.
* Bugfix: text colors in `hux_logo()`.
* Bugfix: rbind of huxtable and matrix wasn't setting `row_height` correctly.


# huxtable 4.5.0

* Add `quick_latex()` function.
* The `texreg` package now includes a `huxtablereg` function, analogous
  to `huxreg`, which outputs a huxtable from a list of regressions. This will
  be available from the next version of `texreg`.


# huxtable 4.4.0

* Huxtables can now be printed directly in Word documents and Powerpoint presentations,
  thanks to the `flextable` package and recent versions of Pandoc. (Powerpoint printing
  requires Pandoc >= 2.4.0.)
* New "wrapleft" and "wrapright" options to `position()` allow text wrapping around tables.
* New `set_outer_border_colors()` and `set_outer_border_styles()` functions, like
  `set_outer_borders()`.
* Huxtable no longer requires the `broom` package, instead using the `generics` package. If you use
  `huxreg()`, you will still need e.g. `broom` or `broom.mixed` to provide `tidy()` and `glance()`
  methods for specific models.
* Bugfix: `tidy.tidy_override()` and `glance.tidy_override()` should work even if underlying object
  has no `tidy()` or `glance()` method.
* Bugfix: huxtables had option clash when `echo = TRUE` in Rmd pdf_document format.
* Bugfix: `caption()` and `height()` weren't playing nicely.
* Bugfix: `mutate(..., copy_cell_props = FALSE)` was adding a column named `copy_cell_props`.
* Bugfix: `check_latex_dependencies` and `install_latex_dependencies` gave misleading errors.
* Enhancement: when `stars` is `NULL` in `huxreg`, don't print a note by default.
* Enhancement: use `tinytex` when available, allowing autoinstallation of latex packages.


# huxtable 4.3.0

* More work on TeX. Tables *should* now compile when raw_attributes is not set.
* New `map_xxx` functions to set properties variably by cell values.
* Functions for mapping properties variably: `by_rows`, `by_values`, `by_ranges`,
  `by_quantiles` etc.
* Correct bookdown labels are now automatically created.
* New grey, blue, green and orange themes.
* New "themes" vignette.
* New `tidy_override` function to override p values etc. in `huxreg`.
* New `set_contents` function to change huxtable contents within dplyr pipes.
* Enhancement: left- and right-aligned captions are now set above the table in LaTeX, using the
  "threeparttable" package. You will need to install this using e.g. `install_latex_dependencies()` 
  or `tlmgr` if it is not already on your system.
* Enhancement: in `huxtable()` and friends, `add_rownames = "Colname"` now 
  sets the name for the new column.
* Improvements to the vignettes and help files.
* Bugfix: to_md could hang with bold/italic cells.


## Deprecated

* The 3 argument form of `set_xxx` functions is deprecated, as is the `where` function.
  Use `map_xxx` instead.
* Argument `byrow` is soft-deprecated. Use `by_cols()` instead.


# huxtable 4.2.1
 
* Bugfix: `wrap=TRUE` caused squeezed text in RTF.

## Important 

* TeX code was getting escaped by pandoc. To avoid this, if possible, huxtable now
  adds fenced code blocks round latex tables (see 
  https://pandoc.org/MANUAL.html#extension-raw_attribute). You must add
  
    md_extensions: +raw_attribute

  to your YAML header for this to work, and you will need a recent (> 2.0.0) version of Pandoc.


# huxtable 4.2.0

* More speedups: LaTeX 2-3x faster, as_Workbook 2-3x faster.
* Simplify LaTeX output using our own LaTeX commands.
* RTF support: new `print_rtf`, `to_rtf` and `quick_rtf` functions.
* New `border_style` properties to set "solid", "double", "dotted" or "dashed" borders.
  (At present, LaTeX only allows "solid" or "double".)
* New `merge_cells` function, an alternative interface to `colspan` and `rowspan`.
* New `quick_pptx` function to print data frames and huxtables into Powerpoint.
* New `install_latex_dependencies` and `check_latex_dependencies` utility functions.
* `add_rows` and `add_columns` now accept data frames as arguments.
* New `theme_mondrian` theme :-D
* Enhancement: `print_md` now handles **bold** and *italic* cells.
* Enhancement: `quick_pdf` has new `width` and `height` options to change paper size.
* Use CSS writing-mode where possible for text rotation. Note that this may break on non-LTR 
  languages. If this affects you, please file an issue.
* Bugfix: LaTeX didn't compile when height and caption were both set.
* Bugfix: `print_screen` and `print_md` would hang with a wide huxtable.
* Tweaks to documentation.


# huxtable 4.1.0

* dplyr, knitr, rmarkdown and some other packages have moved to "Suggests:", lowering the dependency
  load considerably. All the functionality is still present. huxtable gives an informative warning 
  if a needed package is not installed.
* Code rewrites for better performance and maintainability: HTML is up to 10x faster,
  LaTeX is up to 4x faster.
* Documentation improvements.
* New `tribble_hux` function wrapping `tibble::tribble()` for readable data input.
* New `add_rows` and `add_columns` functions to insert one or more rows into the middle of a
  huxtable.
* New option "huxtable.knitr_output_format" to override the default output format in knitr documents.
* Numeric row heights and column widths are rescaled to 1 when huxtables are cbinded/rbinded.
* LaTeX: at points where borders cross, priority is given to the horizontal border color.
* Bugfix: property accessors had the wrong environment. Thanks to Iñaki Úcar.
* Bugfix: row heights and column widths weren't being copied with cbind/rbind.
* Bugfixes for 0-row or 0-column huxtables:
  - Output works, usually with a warning.
  - cbind and rbind work.
* Bugfix: HTML cols were printed with 'width: NA'.
* Bugfix: width, col_width etc. can be reset to a number after setting them to a string.
  - The (undocumented) ability to mix numeric and non-numeric values for padding and/border widths 
    has been removed. If you want a number, set a number and not a string.
* Bugfix: HTML tables with position "right" weren't right-aligned.
* Nicer error messages when rbinding objects with different numbers of rows.
* Vignette improvements.
* `is_a_number` is deprecated.
* ... and a cool new randomized `hux_logo()` ;-)


# huxtable 4.0.1

* Improved formatting in Excel output.
* New `format` method which returns the result of `to_html`, `to_latex` etc. as appropriate.
* Bugfix: `to_html` printing e.g. "left-border: NA;" in cell CSS.
* Bugfix: `set_all_*` not working when huxtable is not attached.
* Bugfix: `as_Workbook` failing with non-numeric `width`.
* Bugfix: `hux_logo` was using multiple fonts, fails with Excel output.
* Bugfix: `as_flextable` borders not working in cells with colspan > 1.
* Documentation bugfixes.
* Compatibility with broom 5.0.0 - thanks @alexpghayes


# huxtable 4.0.0

* New `theme_plain` theme.
* The default value for `add_colnames` is going to become `TRUE`. At present it remains `FALSE`. Set 
  `options("huxtable.add_colnames")` to `TRUE` or `FALSE` to set the default and avoid warnings in
  future.
* `quick_*` functions now automatically open documents if used interactively. Use `open = FALSE` to
  avoid.
* Tweak top and bottom margins for HTML tables.
* `pad_decimal` is deprecated in favour of `align(ht) <- "."`.
* `huxreg` continues with a warning if `statistics` are unavailable for some models.

## Breaking changes 

* huxtable now provides `knit_print.data.frame` methods. This 
  means that bare data frames will be pretty-printed via huxtable if the package is loaded. 
  - Set `options("huxtable.knit_print_df")` to `FALSE` if you don't want this.
  - By default data frames are printed using the `theme_plain` theme. Set
    options("huxtable.knit_print_df_theme") to a different one-argument function if you want to 
    use a different theme.
* The new `autoformat` argument lets `huxtable()` and `as_huxtable()` automatically choose alignment 
  and number format based on column type. Set `options("huxtable.autoformat")` to `FALSE` to turn 
  off this feature by default.
* The default value of `number_format` has changed from "%5.3g" to "%.3g", which no longer space-pads numbers.
* `as_flextable` now does not print column names in the header. This matches the standard
  huxtable behaviour whereby headers are "just another row/column". To get the old behaviour, 
  use `colnames_to_header = TRUE`.
  
## Bugfixes

* Bugfix: Date and datetime columns were converted to numbers by `add_colnames`.
* LaTeX bugfix: background colors were printing an extra space.
* `huxreg` was never using built-in confidence intervals.
* Screen bugfixes:
  - set max_width to screen width (thanks @jacob-long)
  - misaligned decimal points
* Various bugfixes for `number_format`, `huxreg`, `as_hux.table`, `as_flextable`.
  

# huxtable 3.0.0

* Output to Excel workbooks using the `openxlsx` package.
* New `quick_xlsx` function.
* dplyr select helpers now work inside `set_*` column specifications: e.g. set_bold(ht, 1:3, matches("ab"), TRUE)
* Column names can now be used for the `after` argument to `insert_column`.
* `quick_*` functions: when the `file` argument is not explicitly specified, confirm overwrites manually, or fail 
  if called non-interactively.
* Add pointless quote marks in Description and Title... I don't make the rules.
* Don't apply number_format to negative exponents (e.g. 1.12e-3).
* New `tidy_args` argument to huxreg allows per-model customization of the call to `tidy`.

## Breaking changes

* `quick_xxx` functions without an explicit `file` argument throw an error if called non-interactively,
  and prompt before overwriting files if called interactively.


# huxtable 2.0.2

* Don't apply `number_format` to exponents in scientific notation.
* Turn off some tests on CRAN, as they fail there but not elsewhere.


# huxtable 2.0.1

* Fix `quick_pdf` error when moving output across filesystems.


# huxtable 2.0.0

* New `quick_html`, `quick_pdf` and `quick_docx` functions to print table-like objects to a new document.
* `to_screen` only shows colnames if there are any non-zero-length column names.

## Breaking changes

* `number_format` now applies to any number-like substrings in cells. This means you can include e.g.
  significance stars in a cell and still use `number_format` to format the content.
* If `number_format` is NA, numbers are unchanged. 
* Default value of `number_format` has changed from "%5.2f" to "%5.3g", which plays nicer with integers
  but may surprise you by using scientific format for large numbers.


# huxtable 1.2.0

* New `outer_borders` argument for `huxreg`. This changes default behaviour slightly.
* New `border` argument for `add_footnote` to choose width of footnote's top border.
* Added guard assertions to many exported functions.
* Bugfix: captions and colnames are wrapped in to_screen to respect max_width.


# huxtable 1.1.0

* No more ugly autocreated column names.
* Allow huxtable to have invalid or empty column names in general.
* LaTeX should now be *much* faster on large tables.
* `set_outer_borders` now accepts the same row/column arguments as other `set_` functions.
* Better handling in LaTeX of horizontal borders which don't cross the entire table. (But not varying
  positive border widths....)
* Bugfix: flextable didn't like huxreg's syntactically invalid column names.
* Accept, but silently change, English spelling of 'centre' in `align`, `position` and `caption_pos`.


# huxtable 1.0.0

* LaTeX implements different thicknesses for vertical and horizontal borders (but only one horizontal thickness per row).
* LaTeX border colors now collapse nicely: set colors override unset ones.
* React gracefully to lack of p values in `huxreg`.
* New `set_outer_borders` function to set borders round a rectangle of cells.
* `to_screen` and `to_md` now respect `wrap` and `col_widths` properties.
* Screen and markdown wrap respect word boundaries.
* `to_screen` and `to_md` gain a `min_width` argument; `to_md` gains a logical `header` argument; `to_screen` gains
  a `compact` argument replacing `blank = NULL`.
* On screen colour and bold support, if the `crayon` package is installed. New `huxtable.color_screen` option.
* Move from `ReporteRs` to `officer` and `flextable`. No more `RJava` horror.
* New `error_format` argument to `huxreg` for flexible control over uncertainty estimates.
* Infrastructure improvements: slightly less ugly code in screen.R and LaTeX.R.


## Breaking changes

* Removed options `collapse`, `borders`, `blank` and `colname_color` from `to_screen`/`print_screen`.
* `as_FlexTable` is deprecated and calls `as_flextable` with a warning. `header_rows` and `footer_rows` 
  arguments are ignored. If you need this feature, tell me.
* HTML border sizes are now set in points, not pixels.
* In `huxreg`:
  - `ci_level` is `NULL` by default. Set it to a number to calculate confidence intervals.
  - `error_style` is deprecated with a warning in favour of `error_format`.
  - Use `{stars}` not `%stars%` to display significance levels in the `note` argument.
  - `borders` becomes a number specifying border width. Set to 0 for no borders.


# huxtable 0.3.1

* New convenience functions `insert_row` and `insert_column`.
* `latex_float` property allows you to change positioning in LaTeX.
* (Semantic versioning fail: this should have been 0.4.0.)


# huxtable 0.3.0

* New borders argument for huxreg, gives borders in sensible places.
* Allow more flexible caption positioning with `caption_pos`.
* New `set_default_properties` function to set default properties for new huxtables.
* Fix compatibility with dplyr 0.6.0.


# huxtable 0.2.2

* Fix a bug that could lead to wrong significance stars in `huxreg`.


# huxtable 0.2.1

* Compatibility with dplyr 0.6.0.
* Use ~ for decimal padding in LaTeX.


# huxtable 0.2.0

* New `huxreg` function to convert a list of models to a huxtable.
* New set_* interface allowing column ranges, expressions a la `subset`, and filling in values by row.
* Replacement methods `$<-`, `[<-` and `[[<-` now work better.
* New function `set_cell_properties` to set multiple properties on cells.
* `evens`, `odds`, `everywhere`, `every(n, from)`, `final(n)`, `where(cond)`: 
  convenience functions to select rows, columns and cells.
* Export to Word/Powerpoint via `ReporteRs`.
* Huxtable now supports dplyr verbs like `filter` and `select`.
* Exported function `guess_knitr_output_format`.
* Ability to set border colors.
* Prevent overlapping row/colspans.
* Expanded introduction and new vignette for `huxreg`.
* Numerous bugs have been fixed and replaced with new, more advanced bugs.

## Breaking changes

* `theme_minimal` has been renamed `theme_basic` to avoid a name clash with `ggplot2`.


# huxtable 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* First CRAN release.



