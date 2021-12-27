webshot 0.5.2
=============

* Modified `install_phantomjs()` function to only install a new version of PhantomJS if the installed version is out of date or it isn't installed. (@coatless, [#82](https://github.com/wch/webshot/pull/82))

* Added `force` parameter. When it is set to `TRUE`, `install_phantomjs()` will reinstall phantomjs. (@coatless, [#82](https://github.com/wch/webshot/pull/82))

* Added `is_phantomjs_installed()` function to check if PhantomJS was installed on the user's computer. (@coatless, [#82](https://github.com/wch/webshot/pull/82))

* Fixed `phantom_paths()` function to detect the path to PhantomJS on Linux (@coatless, @wildintellect, [#84](https://github.com/wch/webshot/pull/84))


webshot 0.5.1
=============

* Added `debug` parameter. When it is set to `TRUE`, `webshot()` will print out debugging messages from PhantomJS and CasperJS.

* Fixed [#51](https://github.com/wch/webshot/issues/51): Webshot had trouble with some sites that use HTTPS.

* Added `appshot.shiny.appobj` functionality (schloerke, [#55](https://github.com/wch/webshot/pull/55))

webshot 0.5.0
=============

* Added support for R Markdown documents. ([#48](https://github.com/wch/webshot/pull/48))

* Closed [#42](https://github.com/wch/webshot/issues/42): Converted some instances of `system2()` to use processx instead.

webshot 0.4.2
=============

* Fixed [#43](https://github.com/wch/webshot/issues/43): The `eval` argument for `webshot()` did not work.

webshot 0.4.1
=============

* Updated vignette so it doesn't error when PhantomJS is not present.

webshot 0.4.0
=============

* `webshot`, `resize`, and `shrink` all now accept a vector of URLs or filenames. (([#32](https://github.com/wch/webshot/pull/32)), [#33](https://github.com/wch/webshot/pull/33))

* Updated to CasperJS 1.1.3.

* Added `zoom` option for higher-resolution screen shots. ([#26](https://github.com/wch/webshot/issues/26))

* `webshot()` now returns objects with class `webshot`. There is also a new `knit_print` method for `webshot` objects. ([#27](https://github.com/wch/webshot/pull/27))

* Fixed problem installing PhantomJS on R 3.3.2 and above. ([#35](https://github.com/wch/webshot/pull/35))

webshot 0.3.2
=============

* Better handling of local paths in Windows. ([#23](https://github.com/wch/webshot/issues/23))

* More robust searching for ImageMagick. ([#13](https://github.com/wch/webshot/issues/13))

webshot 0.3.1
=============

* The leading tilde in the path of PhantomJS is expanded now ([#19](https://github.com/wch/webshot/issues/19)).

* Changed URL for PhantomJS binaries so that `install_phantomjs()` doesn't hit rate limits, and added workaround for downloading problems with R 3.3.0 and 3.3.1.

webshot 0.3
===========

* The first CRAN release. Provided functions `webshot()`/`appshot()` to take screenshots via PhantomJS, and `resize()`/`shrink()` to manipulate images via GraphicsMagick/ImageMagick and OptiPNG.
