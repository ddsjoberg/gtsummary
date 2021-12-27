# ragg 1.1.2

* Fix bug in `agg_capture()` that resulted in premultiplied colour values being
  returned

# ragg 1.1.1

* Fix a bug in glyph dimension lookup that could cause system crashes
* Fix bug in font caching when multiple ragg devices are used simultaneously

# ragg 1.1.0

* Major version release to signify the much improved text support that includes
  full support for right-to-left scripts and bidirectional text (mix of RtL and
  LtR scripts). It further adds full support for OpenType features and 
  non-scalable fonts.
* Re-exporting `register_font()`, `register_variant()`, and `font_feature()` 
  from systemfonts
* Re-exporting `get_font_features()` from textshaping
* Use new textshaping API and handle font fallback correctly
* Add support for rendering colour fonts (#1)

# ragg 0.4.1

* Skip text tests on CRAN as no text is plottet on the CRAN solaris machine
* Fixed a bug resulting in system crash on certain systems, as well as 
  clang-ASAN error. (#59)
  
# ragg 0.4.0

* ragg now requires the Harfbuzz and Fribidi libraries to be available when 
  installing from source due to their dependency in the textshaping package.
* Move text shaping to the new textshaping package.
* Fix `agg_capture()` on big endian systems (#49, @QuLogic)
* Fix use of symbol font on Windows by moving to Segoe UI Symbol which has a 
  Unicode charmap (#51)
* Better compatibility with knitr and `ggplot2::ggsave()`

# ragg 0.3.1

* Roll back support for new clipping options in the graphic engine as it was 
  buggy.

# ragg 0.3.0

* Fix a bug when plotting partially transparent raster (#44)
* Add a `scaling` argument to all devices allowing you to change relative 
  scaling of output.
* Horizontal and vertical text are now snapped to the pixel grid in order to 
  improve rendering quality.
* Internal changes to prepare for coming updates to the graphic engine

# ragg 0.2.0

* Fix compilation on R <= 3.3 by including Rdynload.h explicitly
* Fix a performance regression when plotting text (#33)
* Fix erroneous width calculations of strings starting with a space on windows 
  (#32)
* Fix a bug in `agg_capture()` where the output became mangled if device 
  height != width
* Fix a bug in raster support where raster data did not get premultiplied before
  rendering (#38, @yixuan)
* Fix an integer overflow issue in the AGG source code

# ragg 0.1.5

* Fix compilation on macOS

# ragg 0.1.4

* Fix a bug in AGG's font manager that ignored the font index when it stored and
  retrieved cached faces

# ragg 0.1.3

* Fix bug preventing ragg from displaying 50% transparent black
* Another attempt at fixing compilation on mac build machines

# ragg 0.1.2

* Fix compilation on certain Linux systems by preferring dynamic libraries over 
  static ones (#25, @jimhester).

# ragg 0.1.1

* Avoid a bug when the call to start a device included too many characters (#16)
* Fix integer overflow runtime errors in agg source code 
  (`agg_scanline_storage_aa.h`), by changing storage to `long`
* Remove benchmarking vignettes as it was causing too much trouble on stripped
  down systems... They are still available on <https://ragg.r-lib.org>
* Better build setup to properly build on all macOS systems

# ragg 0.1.0

* Basic setup of package. png, tiff, ppm, and buffer capture support
* Added a `NEWS.md` file to track changes to the package.
