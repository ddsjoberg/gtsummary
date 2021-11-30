# GGally 2.1.2

### Bug fixes

* Replace `ggplot2` usage of `*_guide = FALSE` with `*_guide = "none"` (@larmarange, #418)
* Require `network >= 1.17.1` (#418)

# GGally 2.1.1

### Bug fixes

* Ignore `colour` aesthetic if all values are `NA`. (@larmarange, #404)
* Avoid all duplicates within `stat_cross()`. (@larmarange, #402)
* Avoid an error when tidiers do not return p-values. (@larmarange, #400)
* Suggest `emmeans` to allow `ggcoef()` example to execute. (#407)

# GGally 2.1.0

### Breaking changes

* Following version 7.0.0 of `broom`, computed residuals in `stat_cross()`
  are now named `"resid"` and `"std.resid"`. `cells` and `fill` arguments
  of `ggally_crosstable()` and `ggtable()` have been updated accordingly
  (@larmarange, #391)

### Other changes

* `ggcoef()` redesign based on `broom.helpers` with four new functions:
  `ggcoef_model()`, `ggcoef_compare()`, `ggcoef_multinom()` and `ggcoef_plot()`
  (more informations in the dedicated vignette, @larmarange, #392)
* New geometries: `geom_stripped_rows()` and `geom_stripped_cols()`
  (#392, @larmarange)
* New option `reverse_fill_labels` for `ggally_colbar()`
  and `ggally_rowbar()` (@larmarange, #374)
* `stat_prop()` now accepts a **x** or a **y** aesthetic (#395, @larmarange)
* Temporarily not listening to `ggally_statistic(family)` to avoid monospaced font issues. See #373 for more details. (#387)


# GGally 2.0.0

### New Vignettes

* [`vig_ggally("ggally_plots")`](https://ggobi.github.io/ggally/articles/ggally_plots.html) - ggally_*(): List of available high-level plots
* [`vig_ggally("ggally_stats")`](https://ggobi.github.io/ggally/articles/ggally_stats.html) - stat_*(): Additional statistics for ggplot2
* [`vig_ggally("ggbivariate")`](https://ggobi.github.io/ggally/articles/ggbivariate.html) - ggbivariate(): Plot an outcome with several potential explanatory variables
* [`vig_ggally("ggtable")`](https://ggobi.github.io/ggally/articles/ggtable.html) - ggtable(): Cross-tabulated tables
* To view all vignettes for `GGally`, call `GGally::vig_ggally()`


### New functions

`ggbivariate()` (@larmarange, #324)
* Display an outcome using several potential explanatory variables
* [`vig_ggally("ggbivariate")`](https://ggobi.github.io/ggally/articles/ggbivariate.html)

`ggtable()` (@larmarange, #351)
* Cross-tabulated tables of discrete variables
* [`vig_ggally("ggtable")`](https://ggobi.github.io/ggally/articles/ggtable.html)

`add_to_ggmatrix()` (#362)
* Add ggplot2 objects to `ggmatrix` objects at selected locations
* Locations can be rows, columns, matrices, or other shorthand values.

`ggally_autopoint()`, `ggally_autopointDiag()` (@larmarange, #325)
* Make scatterplots compatible with both continuous and categorical variables using `ggforce::geom_autopoint()`.

`ggally_colbar()`, `ggally_rowbar()` (@larmarange, #324)
* Plot column or row percentage using bar plots.

`ggally_count()`, `ggally_countDiag()` (@larmarange, #321)
* Plot the number of observations by using rectangles with proportional areas.

`ggally_cross()` (@larmarange, #326)
* Plot the number of observations by using square points with proportional areas.

`ggally_crosstable()` (@larmarange, #351)
* Display a cross-tabulated table.

`ggally_statistic()` (#327)
* A generalized version of `ggally_cor()`
* Use this method to create functions similar to `ggally_cor()` that return any text value given and `x` and `y` vector of data

`ggally_summarise_by()` (@larmarange, #325)
* Display summary statistics of a continuous variable for each value of a discrete variable.

`ggally_table()` (@larmarange, #326)
* Plot the number of observations as a table.

`ggally_trends()` (@larmarange, #333)
* Plot trends using line plots.

`signif_stars()` (@larmarange, #327)
* Return the appropriate number of significance stars as a character vector for the provided numeric input values.

### New `ggplot2` plot statistics:

`stat_cross()` (@larmarange, #326)
* Computes statistics of a 2-dimensional matrix using `broom::augment.htest`.

`stat_prop()` (@larmarange, #324)
* Compute proportions according to custom denominator.

`stat_weighted_mean()` (@larmarange, #333)
* Compute the mean of y aesthetic for each unique value of x, taking into account weight aesthetic if provided.

### Major updates

`ggally_cor()` (#327)
* New implementation using `ggally_statistic()`
* Will now hide the grid by default and add a border (`displayGrid = FALSE`)
* Added the ability to display significance stars (`stars = TRUE`)
* Alignment has been fixed so both short and long names should be displayed within view. `alignPercent` now corresponds to the center of the text.
* Added the ability to separate the arguments sent to the title and the groups (`title_args` and `group_args`)
* Digits now represents the total number of digits after the decimal place.
* To use the old version, change your `ggally_cor()` function calls to `ggally_cor_v1_5()`.
* Previously deprecated parameters have been removed

Website
* Updated to use `pkgdown` (#335)

### Features and bug fixes:

`ggpairs()` (#331)
* New `proportion` argument to control relative size of sub-plots
* option `proportion = "auto"` for automatic guess based on the number of levels for discrete variables

`ggduo()` (#331)
* New `xProportion` and `yProportion` arguments to control relative size of sub-plots
* Set option `xProportion = "auto"` and `yProportion = "auto"` for automatic guess based on the number of levels for discrete variables

`ggscatmat()`
* `lowertriangle()` now preallocates it's memory usage for a 2-5x speed improvement. (@vlepori, #328)
* Fixed `facet`'ing error where the factor order was not preserved. This error caused the facets to be alphabetically sorted, cause plots to appear in unexpected locations. (#355)


# GGally 1.5.0

* Updated to work with ggplot2 v3.3.0 (#308)

`ggnet` and `ggnet2`
* Fixed some logic bugs from newer R versions

`ggally_box` and `ggally_dot`
* Label now appears axis and is displayed in a plot matrix. (#253)

`ggsurv`
* Provide sensible legend values when multiple factors are present. (#310)

`ggally_cor`
* Added `displayGrid` argument to turn of the background grid. (#312)




GGally 1.3.3
----------------

`ggpairs` and `ggduo`

* Become ggplot2 v2.2.2 compliant (#266)
* When retrieving functions with wrap, `ggally_*` functions do not require the GGally namespace (#269)
* Exported `eval_data_col`, `mapping_string`, and `mapping_swap_x_y` (5d157f6)
* Exported `is_horizontal` and `is_character_column` (#270)
* Logical values are now treated as discrete (#272)

`ggmatrix`

* `progress` parameter added to ggmatrix (and appropriate parent functions). Allows for `TRUE`, `FALSE`, `NULL`, and `function(pm){...}` (#271)

`ggnostic`

* Cooks distance cutoff is now at F_{p, n - p}(0.5) (#274)

`ggnet2`

* Replaced loading packages with loading namespaces(#262)

`ggally_smooth`

* Added `shrink` and `se` parameters to `ggally_smooth` (#247)

`ggcoef`

* Added `sort` parameter to sort by beta values (#273)

`ggparcoord`

* Fixed bug where x axis breaks and labels did not appear when `splineFactor = TRUE` (#279)



GGally 1.3.2
-----------------

`ggpairs` and `ggduo`

* Removed warning where pure numeric names gave a warning (#238, @lepennec)
* Fixed ordering issue with horizontal boxplots (#239)

`ggparcoord`

* Fixed missing `x` aes requirement when shadebox is provided (#237, @treysp)

Package

* Made igraph a non required dependency for tests (#240)


GGally 1.3.1
-----------------

Added new dataset `psychademic`

* See `?psychademic` for more details
* (And updated the broken UCLA links)

Added original ggmatrix theme

* added function to set theme to have clear strip background and rearrange the strip positions
* added parameter `switch` to ggmatrix (and friends) to allow for strip repositioning.  See `?ggplot::facet_grid` for more documentation on `switch` (#223, #224)

`ggsurv` error reporting

* removed a one error check that is covered in other places (#222)

`+.gg`

* allow to add a list of items to a ggmatrix (#228)

`ggmatrix.print`

* fix strip issues with ggplot2 name update



GGally 1.3.0
-----------------

`ggmatrix.print` - massive update!

* Now prints with a ggplot2 facet'ed structure
* Column titles are now placed in the strip of a plot matrix
* If there are 16 plots or more, a progress bar is displayed automatically (if interactive).  Please look at the documentation for `ggmatrix_gtable` more details.


`ggmatrix` legend

* A legend may be added with the `legend` parameter in `ggduo`, `ggpairs`, and `ggmatrix`
* May specify a (length two) numeric plot coordinate
* May specify a (length one) numeric plot position
* May specify a legend object retrieved from `grab_legend`


`ggnostic` - New function!

* Produces a `ggmatrix` of diagnostic plots from a model object
* Uses broom to retrieve model information
* Each column of the plot matrix is a predictor variable. The rows can display the response variables, fitted points, residuals, standardized residuals, leave one out model sigma values, diagonals of the hat matrix, and cook's distance for each point.


`ggfacet` - New function!

* Produces single ggplot2 object
* interface is very similar to `ggduo` and `ggpairs`


`fn_switch` - New function!

* Provide many functions in a list but only call one function at run time according to a mapping value
* Useful for `ggnostic` for different behavior depending on the y variable
* Allows for a 'default' value for the default switch case


`ggmatrix` - allow custom labellers for facet labels

* Added labeller parameter which is supplied to `ggplot2::facet_grid()`
* Allows for labels with plotmath expressions


`ggmatrix` and `ggplot2::last_plot()`

* If a `ggmatrix` object is printed, `ggplot2::last_plot()` will return the plot matrix


`ggmatrix` and ggplot2 labels

* `ggplot2::labs` `+`'ed to a ggmatrix object
* `ggplot2::xlab` and `ggplot2::ylab` may be `+`'ed to a ggmatrix object
* `ggplot2::ggtitle` `+`'ed to a ggmatrix object
* (anything that returns a class of "labels" may be added to a ggmatrix object)


`ggmatrix` and `ggplot2::ggsave()`

* `ggsave` now works with `ggmatrix` objects


`ggpairs` and `ggduo` check for cardinality (#197)

* Before creating a ggmatrix object, a check is made for character/factor columns
* If there are more than 15 (default) unique combinations, an error is thrown.
* Setting `cardinality_threshold` parameter to a higher value can fix the problem (knowing single cell plots may take more time to produce)
* Setting `cardinality_threshold` parameter to `NULL` can stop the check


`ggmatrix` plot proportions

* `ggmatrix` can set the plot proportions with the parameters `xProportions` and `yProportions`
* These will change the relative size of the plot panels produced.


`ggally_cor` colour aesthetic

* color must be a non-numeric value

`ggsurv`

* added boolean to allow for legend to not be sorted
* fixed bug where censored points with custom color didn't match properly (#185)


Vignettes

* vignettes are now displayed using `packagedocs`.  More info at http://hafen.github.io/packagedocs/

`ggally_box_no_facet` and `ggally_dot_no_facet`

* New methods added as defaults to pair with new ggmatrix print method



GGally 1.2.0
-----------------

install requirements

* relaxed install requirements on grid (5d06dfc, d57469a, 933bb14, 73b314d)

ggduo - New!

* plot two grouped data in a plot matrix (#173)
* helpful for plotting two sets of columns, multivariate analysis, and canonical correlation analysis
* be sure to check out the examples!

ggally_smooth_loess - New!

* uses the loess method with drawing a line (1552f96)

ggally_smooth_lm - New!

* uses the lm method with drawing a line (1552f96)
* alias of ggally_smooth

ggmatrix.print

* fixed bug strips where causing spacing issue when printing axis labels (174630d)

ggnetworkmap

* fixed bug where checking for the package 'intergraph' couldn't be reached

ggsurv

* changed default of plotting multiple censored data color to match the survival line

package testing

* added many more tests!

GGally 1.1.0
-----------------

ggcoef - New!

* plot model coefficients with broom and ggplot2 PR#162
* Plotting model coefficients (http://www.r-statistics.com/2010/07/visualization-of-regression-coefficients-in-r/)

gglegend - New!

* pull out the legend of a plot which can also be used in ggpairs PR#155, PR#169

ggally_densityDiag

* fixed bug where '...' was not respected (d0fe633)

ggally_smooth

* added 'method' parameter (411213c)

ggally_ratio

* Does not call ggfluctuation2 anymore. PR#165

ggcorr

* fixed issue with unnamed correlation matrix used as input PR#146
* fixed issue undesired shifting when layout.exp was > 0 PR#171

ggfluctuation2

* is being deprecated. Please use ggally_ratio instead PR#165

ggnetworkmap

* fixed issue with overlaying network on a world map PR#157

ggparcoord

* Fixed odd bug where a list was trying to be forced as a double PR#162

ggpairs

* Fixed improperly rotated axes with ggally_ratio PR#165

ggscatmat

* added 'corMethod' parameter for use in upper triangle PR#145

ggsurv

* size.est and size.ci parameters added PR#153
* ordering changed to reflect survival time PR#147
* added a vignette PR#154

wrap

* documentation updated PR#152
* changes default behavior only. If an argument is supplied, the argument will take precedence

github chat

* https://gitter.im/ggobi/ggally is the place to visit for general questions.

travis-ci

* cache packages for faster checking
* install covr and lintr from github for testing purposes


GGally 1.0.1
-----------------

ggparcoord

* fix handling of factor group variable PR#131

ggscatmat

* force all char columns to factors PR#134

print.ggmatrix

* add boolean for grid.newpage ggmatrix print method PR#126


GGally 1.0.0
-----------------

ggplot2

* GGally has been upgraded to run on the latest ggplot2 v1.1.0. PR#109

New functions

* ggmatrix. Make a generic matrix of ggplot2 plots
* ggnetworkmap. Plot a network with ggplot2 suitable for overlay on a ggmap::map ggplot, or other ggplot
* ggnet2. Function for plotting network objects using ggplot2, with additional control over graphical parameters that are not supported by the ggnet function


Vignettes

* glyph - new!
* ggmatrix - new!
* ggnetworkmap - new!
* ggpairs - new!
* ggscatmat - new!

ggmatrix

* allows for bracket notation when getting or setting plots. PR#61
* full control over axis labels and axis text. PR#107, PR#111

ggpairs

* is now wrapper to ggmatrix
* takes in 'wrapped' functions.  This better handles the case of many different parameters being supplied to different plot types. PR#90
* dates are better handled in ggpairs.  Still room for improvement for default behavior, but they do not cause errors. PR#58, PR#59
* displays a 'NA' plot when all or a combination of the data is NA. PR#119

ggcorr

* legend title expressions may be used. PR#55
* handles objects that may be coerced into a data.frame PR#70

gglyph

* changed geom_line to geom_path in gglyph. Fixes ordering issue. PR#51

ggparcoord

* remaining columns are passed through so aesthetics may be added later. PR#54
* fixed parcoord ordering issues with odd names. PR#106
* fixed scaling when unique length equals 1. PR#122

ggsurv

* color censored marks the same color as the line. PR#74
* allow for different censored color marks. PR#113

ggally_density

* add fake data points to extend the limits of the stat_density2d. PR#114

ggally_na

* new plot type!

Data

* removed cityServiceFirms
* added twitter_spambots
