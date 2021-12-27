# reprex 2.0.0

## When the clipboard isn't available

We've made reprex more pleasant to use in settings where we cannot access the user's clipboard from R.
Specifically, this applies to use on RStudio Server and RStudio Cloud.

* When `reprex()` is called without `expr` or `input`, in a context where the
  user's clipboard can't be reached from R, the default is now to consult the
  current selection for reprex source. Previously this was only available via
  the `reprex_selection()` addin. Note that this "current selection" default
  behaviour propagates to convenience wrappers around `reprex()`, such as
  `reprex_locale()` and venue-specific functions like `reprex_r()`, and to the
  un-`reprex()` functions, such as `reprex_clean()`.
* In this context, the file containing the (un)rendered reprex is opened so the
  user can manually copy its contents.
  
## Filepaths

`wd` is a new argument to set the reprex working directory.
As a result, the `outfile` argument is deprecated and the `input` argument has new significance.
Here's how to use `input` and `wd` to control reprex filepaths:

* To reprex in the current working directory,  
  Previously: `reprex(outfile = NA)`  
  Now: `reprex(wd = ".")`  
  More generally, usage looks like `reprex(wd = "path/to/desired/wd")`.
* If you really care about reprex filename (and location), write your source to
  `path/to/stuff.R` and call `reprex(input = "path/to/stuff.R")`. When `input`
  is a filepath, that filepath determines the working directory and how reprex
  files are named and `wd` is never even consulted.

Various changes mean that more users will see reprex filepaths.
Therefore, we've revised them to be more self-explanatory and human-friendly.
When reprex needs to invent a file name, it is now based on a random "adjective-animal" slug.
Bring on the `angry-hamster`!

## `.Rprofile`

`reprex()` renders the reprex in a separate, fresh R session using `callr::r()`.
As of callr 3.4.0 (released 2019-12-09), the default became `callr::r(..., user_profile = "project")`, which means that callr executes a `.Rprofile` found in current working directory.
Most reprexes happen in a temp directory and there will be no such `.Rprofile`.
But if the user intentionally reprexes in an existing project with a `.Rprofile`, `callr::r()` and therefore `reprex()` honor it.
In this version of reprex:

* We explicitly make sure that the working directory of the `callr::r()` call is
  the same as the effective working directory of the reprex.
* We alert the user that a local `.Rprofile` has been found.
* We indicate the usage of a local `.Rprofile` in the rendered reprex.

These changes are of special interest to users of the [renv package](https://rstudio.github.io/renv/), which uses `.Rprofile` to implement a project-specific R package library.
Combined with the filepath changes (described above), this means an renv user can call `reprex(wd = ".")`, to render a reprex with respect to a project-specific library.

## Other

HTML preview should work better with more ways of using `reprex_render()`, i.e. usage that doesn't come via a call to `reprex()` (#293).

## Dependency changes

* rstudioapi moves from Suggests to Imports. Related to improving the experience
  when reprex cannot access the user's clipboard.

* mockr is new in Suggests; it's used in the tests.

* We bumped the documented minimum version of Pandoc, because we use the `gfm`
  markdown variant to get GitHub-Flavored Markdown. The `gfm` variant was
  introduced in Pandoc 2.0 (released 2017-10-29).

# reprex 1.0.0

## Venues

* `reprex_VENUE(...)` is a new way to call `reprex(..., venue = "VENUE")`. For example, `reprex_r()` is equivalent to `reprex(venue = "r")`. This makes non-default venues easier to access via auto-completion (#256).

* `"slack"` is a new venue that tweaks the default Markdown output for pasting into Slack messages. It removes the `r` language identifier from the opening code fence, simplifies image links and, by default, suppresses the ad. Note that `venue = "slack"` or `reprex_slack()` work best for people who opt-out of the WYSIWYG message editor: in *Preferences > Advanced*, select "Format messages with markup".

* `venue = "so"` (SO = Stack Overflow) has converged with default `venue = "gh"` (GitHub). As of January 2019, SO [supports CommonMark fenced code blocks](https://meta.stackexchange.com/questions/125148/implement-style-fenced-markdown-code-blocks/322000#322000). The only remaining difference is that Stack Overflow does not support the collapsible details tag that we use on GitHub to reduce the clutter from, e.g., session info (#231).

* `"rtf"` (Rich Text Format) is a new experimental `venue` for pasting into applications like PowerPoint and Keynote. It is experimental because it requires a working installation of the highlight command line tool, which is left as a somewhat fiddly exercise for the user (#331). `venue = "rtf"` is documented in its [own article](https://reprex.tidyverse.org/articles/articles/rtf.html).

* `reprex.current_venue` is a new read-only option that is set during `reprex_render()`. Other packages can use it to generate `reprex()`-compatible, `venue`-aware output, such as an renv lockfile.

## Implementation and internals

* `reprex()` has been internally refactored to make better use of the official
  machinery for extending rmarkdown:
  - `reprex_document()` is a new R Markdown output format.
  - `reprex_render()` is a newly exported function.
  - A `reprex_document()` is designed to be rendered with `reprex_render()`.
    `reprex_render()` is designed to act on a `reprex_document()`. This is
    (still) the heart of what the `reprex()` function does, in addition to
    various interface and workflow niceties.
  - Two R Markdown templates ship with the package, which an RStudio user can
    access via *File > New File > R Markdown ... > From Template*. One is
    minimal; the other uses lots of reprex features. Both include
    `knit: reprex::reprex_render` in the YAML, which causes the RStudio "Knit"
    button to use `reprex_render()`.

* `prex()`, `prex_VENUE()`, and `prex_render()` are new **unexported** functions that, like `reprex()`, render a small bit of code, but with much less **re**producibility! The code is evaluated in the global workspace of the current process, with the current working directory. This pragmatic hack is useful when preparing a series of related snippets, e.g., for a Keynote or PowerPoint presentation, and there's not enough space to make each one self-contained.

* UTF-8 encoding: Following the lead of knitr, reprex makes explicit use of UTF-8 internally (#237 @krlmlr, #261).

* When the reprex causes R to crash, `reprex(std_out_err = TRUE)` is able to provide more information about the crash, in some cases (#312).

## Other changes and improvements

*  The `tidyverse_quiet` argument and `reprex.tidyverse_quiet` option also control startup messages from the [tidymodels](https://www.tidymodels.org) meta-package (#326, @juliasilge).

* `reprex_locale()` is a new thin wrapper around `reprex()` that renders in a temporarily-altered locale (#250).

* The `si` argument of `reprex()` is now `session_info`. Being explicit seems more important than saving characters, given auto-completion.

* The `show` argument of `reprex()` is now `html_preview`, for the sake of consistency with other R Markdown output formats.

* New article on techniques for making package startup quieter (#187, @marionlouveaux).

## Dependency changes

R 3.1 and R 3.2 are no longer explicitly supported or tested. Our general practice is to support the current release (4.0, at time of writing), devel, and the 4 previous versions of R (3.6, 3.5, 3.4, 3.3).

* sessioninfo is new in Suggests, replacing devtools.

* glue is new in Imports, replacing whisker.

* knitr moves from Suggests to Imports (although it was already a hard dependency via rmarkdown), so we can require v1.23 or higher, which represents a major switch to UTF-8.

* cli is new in Imports.

* reprex now relies on testthat >= 3.0.0 and, specifically, uses third edition features.

# reprex 0.3.0

* The `crayon.enabled` option is explicitly set to `FALSE` when rendering the reprex (#238, #239).

* Expression input is once again captured via `substitute()` (as opposed to `rlang::enexpr()`), which is more favorable for reprexes involving tidy eval (#241).

* New venue "html" to render HTML fragments, useful for pasting in sites without markdown but that allow HTML (#236 @cwickham).

* The YAML of reprex's template has been updated in light of the stricter YAML parser used in Pandoc >= 2.2.2.

* `rlang::set_attrs()` has been soft-deprecated and is no longer used internally.

# reprex 0.2.1

* The reprex ad is formatted as superscript for `venue = "gh"` and `venue = "so"`, i.e. it is more subtle (#201).

* New experimental venue "rtf" produces syntax highlighted snippets suitable for pasting into presentation software such as Keynote or PowerPoint. This venue is discussed in [an article](https://reprex.tidyverse.org/articles/articles/rtf.html) (#26).

* Arguments `opts_chunk` and `opts_knit` have been removed from `reprex()`. The same effect has always been achievable via roxygen comments in the reprex code and the examples have always demonstrated this. Overriding knitr options doesn't seem to come up often enough in real-world `reprex()` usage to justify these arguments.

* Internal file system operations use the [fs](https://fs.r-lib.org) package. This should not make any user-facing changes in reprex and we definitely want to know if it does.

# reprex 0.2.0

reprex has a website: <https://reprex.tidyverse.org>. It includes a contributed article from @njtierney (#103).

reprex has moved to the [tidyverse Organization](https://github.com/tidyverse). It is installed as part of the [tidyverse meta-package](https://www.tidyverse.org) and is [suggested to those seeking help](https://www.tidyverse.org/help/). 

`reprex()` gains several arguments and many arguments can now be controlled via an option, in case a user wants their own defaults.

The new `reprex_selection()` add-in reprexes the current selection, with venue controlled by the option `reprex.venue`. It can be handy to bind to a keyboard shortcut (#84 @hadley).

If reprex can't write to the user's clipboard (e.g. on RStudio server or Unix-like systems lacking xclip or xsel), it offers to open the output file for manual copy.

## Option-controlled arguments for custom defaults

These look like `reprex(..., arg = opt(DEFAULT), ...)` in the help file. This is shorthand for `arg = getOption("reprex.arg", DEFAULT)`, i.e. the option `reprex.arg` is consulted and, if unset, the documented default is used. Allows user to define their own default behaviour (#116).

## New arguments to `reprex()`:

  * `advertise`: toggles inclusion of a footer that describes when and how the reprex was created, e.g., "Created on 2017-11-16 by the reprex package (v0.1.1.9000)". Defaults to `TRUE` (#121, #69).
  * `style`: requests code restyling via the newly-Suggested styler package. styler can cope with tidyeval syntactical sugar, e.g. `df %>% group_by(!! group_var)`. Defaults to `FALSE` (#108, #94).
  * `tidyverse_quiet`: affords control of the startup message of the tidyverse meta-package. Defaults to `TRUE`, i.e. suppresses the message (important special case of #70, #100).
  * `std_out_err`: appends output sent to stdout and stderr by the reprex rendering process. This can be necessary to reveal output if the reprex spawns child processes or has `system()` calls. Defaults to `FALSE` (#90, #110).
  * `render`: determines if the reprex is actually rendered or just returns after producing the templated `.R` file. For internal testing.

## Venues

  * Line wrapping is preserved from source via a Pandoc option (#145 @jimhester, #175).

  * `venue = "gh"` now targets CommonMark as the standard for GitHub Flavored Markdown (#77).
  
  * `venue = "so"` has appropriate whitespace at the start.

  * `venue = "ds"` is a new value, corresponding to <https://www.discourse.org>, which is the platform behind [community.rstudio.com](https://community.rstudio.com). This is currently just an alias for the default `"gh"` GitHub venue, because the formatting appears to be compatible. Adding the `"ds"` value so Discourse can be documented and to guard against the possibility that some formatting is actually unique.
  
## Other changes

  * The `keep.source` option is set to `TRUE` when rendering the reprex, so reprexes involving srcrefs should work (#152).
  
  * The "undo" functions (`reprex_invert()`, `reprex_clean()`, `reprex_rescue()`) handle `input` and `outfile` like `reprex()` does. The `outfile` argument is new (#129, #68).

  * The default value for knitr's `upload.fun` is now set according to the venue. It is `knitr::imgur_upload()` for all venues except `"r"`, where it is `identity` (#125).

  * The HTML preview should appear in the RStudio Viewer more consistently, especially on Windows (#75 @yutannihilation).
  
  * More rigorous use of UTF-8 encoding (#76 @yutannihilation).

  * Expression input handling has been refactored. As a result, formatR is no longer Suggested. Trailing comments -- inline and on their own line -- are also now retained (#89, #91, #114, @jennybc and @jimhester).

  * Custom prompts are now escaped when used in regexes (#98, #99 @jimhester). Embedded newlines are now escaped.

# reprex 0.1.2

This was a non-functioning release created by CRAN maintainers by commenting out lines of code relating to the clipboard.

# reprex 0.1.1

  * Pandoc added to SystemRequirements.

# reprex 0.1.0

  * `outfile = NA` causes outfiles to be left in working directory. Filenames will be based on the `input` file, if there was one.

  * `reprex()` strips any leading prompts from input code.

  * Added functions `reprex_clean()`, `reprex_invert()`, and `reprex_rescue()` in order to go backwards, i.e. recover source from a wild-caught reprex.

  * `venue = "R"` (or `"r"`) can be used to get an R script back, augmented with commented output.

  * `comment` argument added to specify prefix for commented output.

  * Added an RStudio addin, accessed via "Render reprex".

  * `input` argument to `reprex()` and friends handles code as string, character vector, or file path.

  * The reprex is rendered via `callr::r_safe()` and is thus run in a clean, separate R process, eliminating any leakage of objects or loaded packages to/from the calling session.

  * `reprex()` gains optional arguments `opts_chunk` and `opts_knit`, taking named list as input, in order to supplement or override default knitr chunk and package options, respectively. (#33)
    - This made the explicit `upload.fun` argument unnecessary, so it's gone. The `upload.fun` option defaults to `knitr::imgur_upload`, which means figures produced by the reprex will be uploaded to [imgur.com](https://imgur.com/) and the associated image syntax will be put into the Markdown, e.g. `![](https://i.imgur.com/QPU5Cg9.png)`. (#15 @paternogbc)
    
  * Order of `reprex()` arguments has changed.

  * `reprex()` gains the `si` argument to request that `devtools::session_info()` or `sessionInfo()` be appended to reprex code (#6 @dgrtwo). When `si = TRUE` and `venue = "gh"` (the default), session info is wrapped in a collapsible details tag. See [an example](https://github.com/tidyverse/reprex/issues/55) (#55).

  * Reprex code can be provided as an R expression. (#6 @dgrtwo, #35)
  
  * `reprex()` uses clipboard functionality from [`clipr`](https://CRAN.R-project.org/package=clipr) and thus should work on Windows and suitably prepared Unix-like systems, in addition to Mac OS. (#16 @mdlincoln)

# reprex 0.0.0.9000

  * I tweeted about this and some people actually used it!
