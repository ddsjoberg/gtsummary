# bslib 0.2.5.1

Small patch release to fix failing test on Solaris.

# bslib 0.2.5

## New features and improvements

* Closed #251: New `bs_theme()` options (`navbar-bg`, `navbar-light-bg`, and `navbar-dark-bg`) for more easily customizing the navbar's background (and foreground) color (#253, #271).
* Closed #281: New `bs_theme()` argument (`font_scale`) for easier scaling of the base font size (#288).
* Closed #256 and #282: Font file importers (`font_google()`, `font_link()`, and `font_face()`) are now re-exported from the `{sass}` package. As a result, they may now be used with any Sass variable (e.g., `bs_theme("input-font-family" = font_google("Pacifico"))`) as well as inside Rmd yaml without `!expr` (e.g., `input-font-family: google: Pacifico` -- see #256 for more details). A new `font_collection()` function was also added for a more convenient way to specify font fallbacks (#291).
* Closed #255: `bs_themer()` now emits sensible `yaml` front matter when used within an Rmd document (#288).
* Closed #227: `bs_themer()` now overlays a spinner during Sass compilation (#243).
* Closed #278: `{bslib}` now includes `rmarkdown::html_document` templates demonstrating example usage with `{bslib}` and `{thematic}` (#288).
* Closed #231: Upgraded from Bootstrap 4.5.3 to 4.6.0 (#254).
* Closed #237: `<blockquote>` tags now have border-left/padding styles with `version = 4` (to mirror the `version = 3` behavior) (#239).
* Closed #279: Warnings about low color contrasts are now suppressed by default, unless `shiny::devmode()` is enabled. To enable/disable these warnings, set the new `options(bslib.color_contrast_warnings = )` to `TRUE`/`FALSE` (#287).
* `bs_theme_dependencies()` now includes Sass source maps when `shiny::devmode()` is enabled (#312).
* Added new `bs_add_functions()`/`bs_add_mixins()` and deprecated `bs_add_declarations()` to reflect `sass::sass_layer()`'s new ability to place `functions` _before_ variable `defaults`. As a result, variable definitions may now use functions defined with `bs_add_functions()`. (#311)

## Bug fixes

* Closed #236, #230, #242, #187, #215, #250: Addressed various cosmetic issues with CSS (#249). 
* Closed #289: collapsed navbar toggle now correctly floats to the right (#290).
* Closed [rstudio/flexdashboard#316](https://github.com/rstudio/flexdashboard/issues/316): fixed an issue with navbar nav spacing/alignment (#286).

# bslib 0.2.4

* Initial release of the package, see https://rstudio.github.io/bslib/
