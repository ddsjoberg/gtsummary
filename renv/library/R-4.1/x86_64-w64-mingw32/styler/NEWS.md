# styler 1.4.1

* Fix interaction between cache and `base_indention`. This also fixes
  the Addin for styling a selection with base indention repeatedly (#764).
* Add more examples to `styler_*` helpfiles (#762).
* Hexadecimal integers now preserve the trailing `L` when styled (#761).
* Add a pre-push hook to make sure news bullets are added to each PR (#765).


# styler 1.4.0

## API Changes

**new**

- `style_file()` and friends gain argument `dry` to control if changes should be
  applied to files or not (#634).

- `style_file()` and friends gain argument `base_indention` (defaulting to 0) to
  control by how much the output code is indented (#649, #692). The Addin for
  styling a selection picks that up, e.g. you can style a function body and
  indention is preserved (#725).

- added an option for disabling all communication when using the package
  (`styler.quiet`) (#640).

- `scope` in `tidyverse_style()` can now be specified with higher granularity
  through `I()`, e.g. `I(c('spaces', 'tokens'))` allows us to style spaces and
  tokens without styling line breaks and indention. Previously, only a string
  was allowed and all less invasive scopes were included, e.g. if you wanted to
  style tokens, you had to always also style spaces, indention, line breaks as
  well (#705, #707).

- added an option (`styler.test_dir_writeable`) that changes test behavior to
  not directly modify test files in the current directory (#548).

- New argument `transformers_drop` in `create_style_guide()` to be populated
  with new helper function `specify_transformers_drop()` for specifying
  conditions under which transformers are not going to be used and can therefore
  be omitted without effecting the result of styling (#711).

**deprecated**

- The environment variable `save_after_styling` is deprecated in favor of the R
  option `styler.save_after_styling` to control if a file is saved after styling
  with the RStudio Addin. Note than in RStudio >= 1.3.0, you can auto-save edits
  in general (Code -> Saving -> Auto-Save), e.g. on idle editor or focus loss,
  so this feature becomes less relevant (#631, #726).


## Major changes

- styler is now distributed under the MIT license (#751).

- Documentation overhaul: New README, new "Get started" pkgdown page, new
  vignettes on `strict = FALSE`, `Adoption` renamed to `Third-party
  integrations` (#741), adding search to pkgdown (#623), group functions in
  pkgdown reference page (#625), minor other doc improvements (#643, #618, #614,
  #677, #651, #667, #672, #687, #752, #754).

- `@exampleIsf` roxygen tag for conditional examples is now supported (#743).

- blank lines in function calls and headers are now removed, for the former only
  when there are no comments before or after the blank line (#629, #630, #635,
  #723).

- speed improvements: 15% faster on new code, 70% on repeated styling of 
  compliant code (The latter is not so relevant because it was almost 
  instantaneous already). Most relevant contributions were #679, #691, #681, 
  #711, #739.

- `#<<` is now recognized as the xaringan marker and no space is added after`#`
  (#700).

## Minor changes and fixes

- `style_dir()` and `style_pkg()` now apply directory exclusion recursively with
  `exclude_dirs` (#676).

- `switch()` now has line breaks after every argument to match the tidyverse
  style guide (#722, #727).

- unary `+` before a function call does not give an error anymore, as before
  version 1.3.0 (#697).

- certain combinations of `stylerignore` markers and cached expressions now
  don't give an error anymore (#738).

- cache is now correctly invalidated when style guide arguments change (#647).

- empty lines are now removed between pipes and assignments (#645, #710).

- multiple `@examples` roxygen tags in a code block of `#'` are no longer
  squashed (#748).

- roxygen code examples starting on the same line as the `@examples` tag are no
  longer moved to the next line (#748).

- always strip trailing spaces and make cache insensitive to it (#626).

- `style_text()` can now style all input that `is.character()`, not just if it
  inherits from classes `character`, `utf8` or `vertical` (#693).

- logical operators within square braces are now moved from the start of a line
  to the end of the previous line (#709).

- spaces are now removed before `[` and `[[` (#713).

- The internal `create_tree()` only used in testing of styler now works when the
  cache is activated (#688).

- simplification of internals (#692).

## Infrastructure changes

- switched from travis and AppVeyor to GitHub Actions (#653, #660).

- Added basic continuous benchmarking with
  [lorenzwalthert/touchstone](https://github.com/lorenzwalthert/touchstone)
  (#674, #684, #698).

- include `test-*` files in styling pre-commit hook (#724).


Thanks to all the people who made this release possible:

[&#x0040;assignUser](https://github.com/assignUser), [&#x0040;ColmanHumphrey](https://github.com/ColmanHumphrey), [&#x0040;davidchall](https://github.com/davidchall), [&#x0040;espinielli](https://github.com/espinielli), [&#x0040;giko45](https://github.com/giko45), [&#x0040;hadley](https://github.com/hadley), [&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), [&#x0040;intiben](https://github.com/intiben), [&#x0040;jamespeapen](https://github.com/jamespeapen), [&#x0040;jthomasmock](https://github.com/jthomasmock), [&#x0040;Kalaschnik](https://github.com/Kalaschnik), [&#x0040;kevinushey](https://github.com/kevinushey), [&#x0040;krlmlr](https://github.com/krlmlr), [&#x0040;lcolladotor](https://github.com/lcolladotor), [&#x0040;MichaelChirico](https://github.com/MichaelChirico), [&#x0040;michaelquinn32](https://github.com/michaelquinn32), [&#x0040;mine-cetinkaya-rundel](https://github.com/mine-cetinkaya-rundel), [&#x0040;pat-s](https://github.com/pat-s), [&#x0040;PMassicotte](https://github.com/PMassicotte), [&#x0040;QuLogic](https://github.com/QuLogic), [&#x0040;renkun-ken](https://github.com/renkun-ken), [&#x0040;RichardJActon](https://github.com/RichardJActon), [&#x0040;seed-of-apricot](https://github.com/seed-of-apricot), [&#x0040;select-id-from-users](https://github.com/select-id-from-users), [&#x0040;SimonDedman](https://github.com/SimonDedman), [&#x0040;stefanoborini](https://github.com/stefanoborini), [&#x0040;swsoyee](https://github.com/swsoyee), and [&#x0040;Winterstorm-j](https://github.com/Winterstorm-j).

# styler 1.3.2

Release upon request by the CRAN team.

## Minor changes and fixes

- Add search and reference sections to pkgdown webpage (#623, #625).
- various fixes to handle special cases for caching and stylerignore and their
  interaction (#611, #610, #609, #607, #602, #600).
- also test on macOS (#604).
- skip timing tests on CRAN as requested by CRAN team because they did not pass 
  on all machines (#603).

# styler 1.3.1

Emergency release. In case multiple expressions are on one line and only 
some of them are cached, styler can remove code. To reach this state, 
some of the expressions must have been styled previously alone and the cache
must be active. Example:

```
library(styler)
cache_activate()
#> Using cache 1.3.0 at ~/.Rcache/styler/1.3.0.
style_text("1")
#> 1
style_text("1 # comment")
#> # comment
```

This is obviously detrimental. We have added additional tests and fixed the 
problem (#593, #595), but we want repeat the warning from `?style_file` that all 
style APIs apart from `style_text()` overwrite code and that styler can only 
check the AST remains valid with `scope < "tokens"`. So use this if you are 
conservative. Or deactivate the cache with `deactivate_cache()` until it has 
fully matured.

We thank the people who have contributed to this release:

[&#x0040;ellessenne](https://github.com/ellessenne) and 
[&#x0040;renkun-ken](https://github.com/renkun-ken).

# styler 1.3.0

## Breaking changes

* `style_pkg()` and `style_dir()` gain a new argument `exclude_dirs` to exclude
  directories from styling, by default `renv` and `packrat`. Note that the
  defaults won't change the behavior of `style_pkg()` because it does anyways
  does not style these directories and they were set for consistency.

* `style_file()` and friends now strip `./` in file paths returned invisibly, 
  i.e. `./script.R` becomes `script.R` (#568).

## New features

* ignore certain lines using `# styler: off` and `#styler: on` or custom
  markers, see `?stylerignore` (#560).

* styler caches results of styling, so applying styler to code it has styled
  before will be instantaneous. This brings large speed boosts in many
  situations, e.g. when `style_pkg()` is run but only a few files have changed
  since the last styling or when using the [styler pre-commit
  hook](https://github.com/lorenzwalthert/precommit). Because styler caches by
  expression, you will also get speed boosts in large files with many
  expressions when you only change a few of them. See `?caching` for details
  (#538, #578).

* `create_style_guide()` gains two arguments `style_guide_name` and
  `style_guide_version` that are carried as meta data, in particular to version
  third-party style guides and ensure the proper functioning of caching. This
  change is completely invisible to users who don't create and distribute their
  own style guide like `tidyverse_style()` (#572).

## Minor changes and fixes

* lines are now broken after `+` in `ggplot2` calls for `strict = TRUE` (#569).

* function documentation now contains many more line breaks due to roxygen2 
  update to version 7.0.1 (#566).

* spaces next to the braces in subsetting expressions `[` and `[[` are now
  removed (#580).

* Adapt to changes in the R parser to make styler pass R CMD check again.
  (#583).

Thanks to all contributors involved, in particular
[&#x0040;colearendt](https://github.com/colearendt), 
[&#x0040;davidski](https://github.com/davidski), 
[&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), 
[&#x0040;pat-s](https://github.com/pat-s), and 
[&#x0040;programming-wizard](https://github.com).

# styler 1.2.0

## Breaking changes

* `style_file()` now correctly styles multiple files from different directories.
  We no longer display the file name of the styled file, but the absolute path.
  This is also reflected in the invisible return value of the function (#522).

* `style_file()` and friends do not write content back to a file when styling
  does not cause any changes in the file. This means the modification date of
  styled files is only changed when the content is changed (#532).

## New features

* Aligned function calls are detected and remain unchanged if they match the
  styler [definition for aligned function
  calls](https://styler.r-lib.org/articles/detect-alignment.html) (#537).

* curly-curly (`{{`) syntactic sugar introduced with rlang 0.4.0 is now
  explicitly handled, where previously it was just treated as two consecutive
  curly braces (#528).

* `style_pkg()`, `style_dir()` and the Addins can now style `.Rprofile`, and
  hidden files are now also styled (#530).

## Minor improvements and fixes

* Roxygen code examples: leverage `roxygen2` for correct
  escaping of expressions that contain `\`, in particular in `dontrun{}` and 
  friends, allow quoted braces that are not matched (#729).
  
* Brace expressions in function calls are formatted in a less compact way to
  improve readability. Typical use case: `tryCatch()` (#543).

* Arguments in function declarations in a context which is indented multiple
  times should now be correct. This typically affects `R6::R6Class()` (#546).

* Escape characters in roxygen code examples are now correctly escaped (#512).

* Special characters such as `\n` in strings are now preserved in text and not
  turned into literal values like a line break (#554).

* Style selection Addin now preserves line break when the last line selected is
  an entire line (#520).

* Style file Addin can now properly handle cancelling (#511).

* The body of a multi-line function declaration is now indented correctly for
  `strict = FALSE` and also wrapped in curly braces for `strict = TRUE` (#536).

* Advice for contributors in `CONTRIBUTING.md` was updated (#508).

## Adaption

* styler is now available through the pre-commit hook `style-files` in
  https://github.com/lorenzwalthert/pre-commit-hooks.

Thanks to all contributors involved, in particular

[&#x0040;Banana1530](https://github.com/Banana1530), [&#x0040;batpigandme](https://github.com/batpigandme), [&#x0040;cpsievert](https://github.com/cpsievert), [&#x0040;ellessenne](https://github.com/ellessenne), [&#x0040;Emiller88](https://github.com/Emiller88), [&#x0040;hadley](https://github.com/hadley), [&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil), [&#x0040;krlmlr](https://github.com/krlmlr), [&#x0040;lorenzwalthert](https://github.com/lorenzwalthert), [&#x0040;lwjohnst86](https://github.com/lwjohnst86), [&#x0040;michaelquinn32](https://github.com/michaelquinn32), [&#x0040;mine-cetinkaya-rundel](https://github.com/mine-cetinkaya-rundel), [&#x0040;Moohan](https://github.com/Moohan), [&#x0040;nxskok](https://github.com/nxskok), [&#x0040;oliverbeagley](https://github.com/oliverbeagley), [&#x0040;pat-s](https://github.com/pat-s), [&#x0040;reddy-ia](https://github.com/reddy-ia), and [&#x0040;russHyde](https://github.com/russHyde)

# styler 1.1.1

This is primarily a maintenance release upon the request of the CRAN team
(#490).

## Major changes

- Users can now control style configurations for styler Addins (#463, #500),
  using the `Set style` Addin. See `?styler::styler_addins` for details.

- `return()` is now always put in braces and put on a new line when used in a
  conditional statement (#492).

- `%>%` almost always causes a line break now for `strict = TRUE` (#503).

## Minor changes

- `style_pkg()` now also styles the "demo" directory by default (#453).

- multi-line strings are now styled more consistently (#459).

- indention in roxygen code example styling (#455) and EOF spacing (#469) was
  fixed.

- indention for for loop edge case (#457) and comments in pipe chain (#482) were
  fixed.

- line-break styling around comma is improved (#479).

- bug that can cause an error when the variable `text` in any name space before
  styler on the search path was defined and did not have length 1 is fixed
  (#484).

- slightly confusing warning about empty strings caused with roxygen code
  examples and Rmd was removed.

- right apostrophe to let package pass R CMD Check in strict Latin-1 locale was
  removed (#490, reason for release).

## Adaption of styler

Since it's never been mentioned in the release notes, we also mention here where
else you can use styler functionality:

* `usethis::use_tidy_style()` styles your project according to the tidyverse
  style guide.

* `reprex::reprex(style = TRUE)` to prettify reprex code before printing. To
  permanently use `style = TRUE` without specifying it every time, you can add
  the following line to your `.Rprofile` (via `usethis::edit_r_profile()`):
  `options(reprex.styler = TRUE)`.

* you can pretty-print your R code in RMarkdown reports without having styler
  modifying the source. This feature is implemented as a code chunk option in
  knitr. use `tidy = "styler"` in the header of a code chunks (e.g. ` ```{r
  name-of-the-chunk, tidy = "styler"}`), or `knitr::opts_chunk$set(tidy =
  "styler")` at the top of your RMarkdown script.

* pretty-printing of [drake](https://github.com/ropensci/drake) workflow data
  frames with `drake::drake_plan_source()`.

* Adding styler as a fixer to the [ale
  Plug-in](https://github.com/dense-analysis/ale/pull/2401) for
  VIM.

Thanks to all contributors involved, in particular
[&#x0040;ArthurPERE](https://github.com/ArthurPERE),
[&#x0040;hadley](https://github.com/hadley),
[&#x0040;igordot](https://github.com/igordot),
[&#x0040;IndrajeetPatil](https://github.com/IndrajeetPatil),
[&#x0040;jackwasey](https://github.com/jackwasey),
[&#x0040;jcrodriguez1989](https://github.com/jcrodriguez1989),
[&#x0040;jennybc](https://github.com/jennybc),
[&#x0040;jonmcalder](https://github.com/jonmcalder),
[&#x0040;katrinleinweber](https://github.com/katrinleinweber),
[&#x0040;krlmlr](https://github.com/krlmlr),
[&#x0040;lorenzwalthert](https://github.com/lorenzwalthert),
[&#x0040;michaelquinn32](https://github.com/michaelquinn32),
[&#x0040;msberends](https://github.com/msberends),
[&#x0040;raynamharris](https://github.com/raynamharris),
[&#x0040;riccardoporreca](https://github.com/riccardoporreca),
[&#x0040;rjake](https://github.com/rjake),
[&#x0040;Robinlovelace](https://github.com/Robinlovelace),
[&#x0040;skirmer](https://github.com/skirmer),
[&#x0040;thalesmello](https://github.com/thalesmello),
[&#x0040;tobiasgerstenberg](https://github.com/tobiasgerstenberg),
[&#x0040;tvatter](https://github.com/tvatter),
[&#x0040;wdearden](https://github.com/wdearden),
[&#x0040;wmayner](https://github.com/wmayner), and
[&#x0040;yech1990](https://github.com/yech1990).

# styler 1.1.0

This release introduces new features and is fully backward-compatible. It also
adapts to changes in the R parser committed into R devel (#419).

## Major Changes

* styler can now style roxygen code examples in the source code of package
  (#332) as well as Rnw files (#431).

* the print method for the output of `style_text()` (`print.vertical()`) now
  returns syntax-highlighted code by default, controllable via the option
  `styler.colored_print.vertical` (#417).

* the README was redesigned (#413).

* semi-colon expression that contained multiple assignments was fixed (#404).

## Minor Changes

* cursor position is remembered for styling via Addin (#416).

* adapt spacing around tilde for multi-token expressions(#424) and brace edge
  case (#425).

* only add brackets to piped function call if RHS is a symbol (#422).

* increase coverage again to over 90% (#412).

* move rule that turns single quotes into double quotes to token modifier in
  `tidyverse_style_guide() (#406).

* remove line-breaks before commas (#405).

* removed package dependency enc in favor of xfun (#442).

Thanks to all contributors for patches, issues and the like: @jonmcalder,
@krlmlr, @IndrajeetPatil, @kalibera, @Hasnep, @kiranmaiganji, @dirkschumacher,
@ClaytonJY, @wlandau, @maurolepore

# styler 1.0.2

This is a maintenance release without any breaking API changes.

## Major Changes

* Fixed indention for named multi-line function calls (#372).

* Non-R code chunks in `.Rmd` files are now respected and won't get styled
  (#386).

## Minor Changes

* Fixing an edge case in which, if very long strings were present in the code,
  tokens could be replaced with wrong text (#384).

* Spacing around tilde in formulas depends now on whether there is a LHS in the
  formula (#379).

* Spaces are now also added around `EQ_SUB` (`=`) (#380).

* Added `CONTRIBUTING.md` to outline guidelines for contributing to styler.

* More informative error messages for parsing problems (#401, #400).

* Improved documentation (#387).

Thanks to all contributors for patches, issues and the like: @katrinleinweber,
@krlmlr, @dchiu911, @ramnathv, @aedobbyn, @Bio7, @tonytonov, @samhinshaw, @fny,
@vnijs, @martin-mfg, @NGaffney, @dchiu911.

# styler 1.0.1

This is a maintenance release without any breaking API changes.

## Major & dependency related changes

* Removed implicit `dplyr` dependency via `purrr:::map_dfr()` (thanks
  @jimhester, #324).

* Added required minimal version dependency for purr (`>= 0.2.3`) (#338).

* We rely on the tibble package which was optimized for speed in `v1.4.2` so
  styler should run ~2x as fast
  [(#348)](https://github.com/tidyverse/tibble/pull/348). For that reason,
  styler now depends on `tibble >= 1.4.2`.

* In the dependency `enc`, a bug was fixed that removed/changed non-ASCII
  characters. Hence, styler now depends on `enc >= 0.2` (#348).

## Minor changes

* We're now recognizing and respecting more DSLs used in R comments: rplumber
  (`#*`, #306), shebang `#/!` (#345), knitr chunk headers for spinning (`#+` /
  `#-`, #362).

* Named arguments can stay on the first line if call is multi-line (#318).

* No space anymore with `tidyverse_style()` after `!!` since with `rlang 0.2`,
  `!!` now binds tighter (#322), spacing around `~` (#316), no space anymore
  around `^` (#308).

* Code chunks in Rmd documents that don't use the R engine are no longer
  formatted (#313).

* Various bug fixes and edge case improvements.

Thanks to all contributors for patches, issues and the like: @devSJR, @klrmlr,
@yutannihilation, @samhinshaw, @martin-mfg, @jjramsey, @RMHogervorst, @wlandau,
@llrs, @aaronrudkin, @crew102, @jkgrain, @jennybc, @joranE.

# styler 1.0.0

Initial release.

## stylers
These are functions used to style code. They style a directory, a whole package,
a file or a string.
```
style_dir(path = ".", 
  ..., style = tidyverse_style, transformers = style(...), 
  filetype = "R", recursive = TRUE, exclude_files = NULL
)

style_pkg(pkg = ".", 
  ..., style = tidyverse_style, transformers = style(...), filetype = "R", 
  exclude_files = "R/RcppExports.R"
)


style_file(path, 
  ..., style = tidyverse_style, transformers = style(...)
)

style_text(text, ..., style = tidyverse_style, transformers = style(...))
```

## style guides
These functions are the style guides implemented.
```
tidyverse_style(
  scope = "tokens", 
  strict = TRUE, 
  indent_by = 2, 
  start_comments_with_one_space = FALSE, 
  reindention = tidyverse_reindention(), 
  math_token_spacing = tidyverse_math_token_spacing()
)
tidyverse_reindention()
tidyverse_math_token_spacing())
```

## style guide creators
This function is used to create a style guide.
```
create_style_guide(
  initialize = default_style_guide_attributes, 
  line_break = NULL, 
  space = NULL, 
  token = NULL, 
  indention = NULL, 
  use_raw_indention = FALSE, 
  reindention = tidyverse_reindention()
)
```

## Helpers
These are helper functions used to specify the style guides in use.

```
specify_math_token_spacing(
  zero = NULL, 
  one = c("'+'", "'-'", "'*'", "'/'", "'^'")
)

specify_reindention(
  regex_pattern = NULL, 
  indention = 0, 
  comments_only = TRUE
)
initialize_default_attributes(pd_flat)
```
