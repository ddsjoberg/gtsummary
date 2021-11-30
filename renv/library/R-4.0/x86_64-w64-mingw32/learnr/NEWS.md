learnr 0.10.1
===========

## New features

## Minor new features and improvements

* `learnr` gained the function `learnr::tutorial_package_dependencies()`, used to enumerate a tutorial's R package dependencies. Front-ends can use this to ensure a tutorial's dependencies are satisfied before attempting to run that tutorial. `learnr::available_tutorials()` gained the column `package_dependencies` containing the required packages to run the document. ([#329](https://github.com/rstudio/learnr/pull/329))

* Include vignette about publishing learnr tutorials on shinyapps.io

* `learnr`'s built-in tutorials now come with a description as part of the YAML header, with the intention of this being used in front-end software that catalogues available `learnr` tutorials on the system. ([#312](https://github.com/rstudio/learnr/issues/312))

* Add `session_start` and `session_stop` events. ([#311](https://github.com/rstudio/learnr/pull/328))

## Bug fixes

* Fixed a bug where broken exercise code created non-"length-one character vector". ([#311](https://github.com/rstudio/learnr/pull/311))

* Fixed extra parameter documentation bug for CRAN. ([#323](https://github.com/rstudio/learnr/pull/323))

* Fixed video initialization error caused by a jQuery version increase in Shiny. ([#326](https://github.com/rstudio/learnr/pull/326))

* Fixed progressive reveal bug where the next section would not be displayed unless refreshed. ([#330](https://github.com/rstudio/learnr/pull/330))

* Fixed a bug where topics would not be loaded if they contained non-ascii characters. ([#330](https://github.com/rstudio/learnr/pull/330))


learnr 0.10.0
===========

## New features

* Quiz questions are implemented using shiny modules (instead of htmlwidgets). ([#194](https://github.com/rstudio/learnr/pull/194))

* Aggressively rerender prerendered tutorials in favor of a cohesive exercise environment ([#169](https://github.com/rstudio/learnr/issues/169), [#179](https://github.com/rstudio/learnr/pull/179), and [rstudio/rmarkdown#1420](https://github.com/rstudio/rmarkdown/pull/1420))

* Added a new function, `safe`, which evaluates code in a new, safe R environment. ([#174](https://github.com/rstudio/learnr/pull/174))

## Minor new features and improvements

* Added the last evaluated exercise submission value, `last_value`, as an exercise checker function argument. ([#228](https://github.com/rstudio/learnr/pull/228))

* Added tabset support. ([#219](https://github.com/rstudio/learnr/pull/219) [#212](https://github.com/rstudio/learnr/issues/212))

* Question width will expand to the container width. ([#222](https://github.com/rstudio/learnr/pull/222))

* Available tutorial names will be displayed when no `name` parameter or an incorrect `name` is provided to `run_tutorial()`. ([#234](https://github.com/rstudio/learnr/pull/234))

* The `options` parameter was added to `question` to allow custom questions to pass along custom information.  See `sortable::sortable_question` for an example. ([#243](https://github.com/rstudio/learnr/pull/243))

* Missing package dependencies will ask to be installed at tutorial run time. (@isteves, [#253](https://github.com/rstudio/learnr/issues/253))

* When questions are tried again, the existing answer will remain, not forcing the user to restart from scratch. ([#270](https://github.com/rstudio/learnr/issues/270))

* A version number has been added to `question_submission` events.  This will help when using custom storage methods. ([#291](https://github.com/rstudio/learnr/pull/291))

* Tutorial storage on the browser is now executed directly on `indexedDB` using `idb-keyval` (dropping `localforage`).  This change prevents browser tabs from blocking each other when trying to access `indexedDB` data. ([#305](https://github.com/rstudio/learnr/pull/305))

## Bug fixes

* Fixed a spurious console warning when running exercises using Pandoc 2.0. ([#154](https://github.com/rstudio/learnr/issues/154))

* Added a fail-safe to try-catch bad student code that would crash the tutorial. ([@adamblake](https://github.com/adamblake), [#229](https://github.com/rstudio/learnr/issues/229))

* Replaced references to `checkthat` and `grader` in docs with [gradethis](https://github.com/rstudio-education/gradethis) ([#269](https://github.com/rstudio/learnr/issues/269))

* Removed a warning created by pandoc when evaluating exercises where pandoc was wanting a title or pagetitle. [#303](https://github.com/rstudio/learnr/pull/303)



learnr 0.9.2
===========

* Fixed [#136](https://github.com/rstudio/learnr/issues/136) by displaying full HTML messages (rather than just the text) if provided by the `incorrect` or the `correct` args to `question()`. ([#146](https://github.com/rstudio/learnr/pull/146))

* Improved documentation for deploying `learnr` tutorials in Shiny Server. ([#142](https://github.com/rstudio/learnr/issues/142))

* Fixed a highlight.js issue from rmarkdown 1.8. ([#133](https://github.com/rstudio/learnr/issues/133))

* Fixed an false positive in the diagnostics system. ([#141](https://github.com/rstudio/learnr/issues/141))

learnr 0.9.1
===========

* Fixed a compatibility issue, so that existing tutorials don't break when using Pandoc 2.0. ([#130](https://github.com/rstudio/learnr/pull/130))

learnr 0.9.0
===========

@ commit [#14413cc](https://github.com/rstudio/learnr/commit/14413cc7ea20fa3b5938b29fab2b01282e6f0c1f)

learnr 0.8.0
===========

@ commit [#eeae534](https://github.com/rstudio/learnr/commit/eeae534fa792dcd369075a90b59b042ad26f945f)

learnr 0.7.0
===========

@ commit [#b71c637](https://github.com/rstudio/learnr/commit/b71c637cb0b1e0cb817e8e0c2fa56a4fcabd58dd)

learnr 0.6.0
===========

@ commit [#55c33cf](https://github.com/rstudio/learnr/commit/55c33cf616d3259c508ae234d301964c599a3039)

learnr 0.5.0
===========

@ commit [#a853163](https://github.com/rstudio/learnr/commit/a8531633f38c13333da6e1c76c6cb6c720e299dd)

learnr 0.4.0
===========

@ commit [#3339f8a](https://github.com/rstudio/learnr/commit/3339f8aaa2d0402622b1881aa42fcc78ea87db51)

learnr 0.3.0
===========

@ commit [#9cd0082](https://github.com/rstudio/learnr/commit/9cd00828bfa2429d88ad9efdbd51ad8475a6efb2)

learnr 0.2.0
===========

@ commit [#a81a694](https://github.com/rstudio/learnr/commit/a81a69498823d860f54c153128719e280de3d831)

learnr 0.1.0
===========

init commit! [#e2dbb20](https://github.com/rstudio/learnr/commit/e2dbb20d8fb7208cffcb339ea0fc5a8c9c45adb5)
