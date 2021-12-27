# roxygen2 7.1.1

* When processing cross package markdown links (e.g. `[pkg::fun()]`),
  roxygen2 now looks up the file it needs to link to, instead of linking to
  the topic, to avoid "Non-file package-anchored links" `R CMD check` warnings.

* R6 methods and re-exported functions are always sorted in the C locale;
  this ensures they're always sorted the same way in every environment (#1077).

* roxygen2 now supports inline markdown code and code chunks inside
  Rd tags. In particular in `\out{}` (#1115).

# roxygen2 7.1.0

## New features

* roxygen2 now supports inline markdown code and also code chunks,
  using the same notation as the knitr package. For example:

    ```R
    #' This manual was generated at: `r Sys.time()`.
    #' ...
    #' `mtcars` is a data frame with `r ncol(mtcars)` columns, here
    #' is a summary of them:
    #'
    #' ```{r}
    #' summary(mtcars)
    #' ```
    ```

  See `vignette("rd-formatting")` for details.

* roxygen2 now keeps using Windows (CR LF) line endings for files that
  already have CR LF line endings, and uses LF for new files (#989).

## Minor improvements and bug fixes 

* Auto-generated package documentation can now handle author ORCID comments
  containing full url (#1040).

* Hyperlinks to R6 methods are also added in the PDF manual (#1006).

* Empty annotations (alternate text) for figures added via markdown are now
  omitted. This caused issues when generating pkgdown web sites (#1051).

* Roxygen metadata can now have a `packages` element, giving a character vector 
  of package names to load. This makes it easier to use extension package that 
  provide new tags for existing roclets (#1013). See `?load_options` for
  more details.
  
    ```yaml
    Roxygen: list(markdown = TRUE, packages = "roxygenlabs")
    ```

* `@evalNamespace()` works again (#1022).

* `@description NULL` and `@details NULL` no longer fail; instead, these tags 
  are ignored, except for `@description NULL` in package level documentation, 
  where it can be used to suppress the auto-generated Description section 
  (#1008).

* Multiple `@format` tags are now combined (#1015).

* The warning for `@section` titles spanning multiple lines now includes a 
  hint that you're missing a colon (@maelle, #994).

* Can now document objects created with `delayedAssign()` by forcing
  evaluation at documentation time (#1041)

# roxygen2 7.0.2

* `\example{}` escaping has been improved (again!) so that special escapes 
  within strings are correctly escaped (#990).

# roxygen2 7.0.1

* `@includeRmd` has now an optional second argument, the top level section
  the included file will go to. It defaults to the details section (#970).
  Code chunks are now evaluated in a child of the global environment (#972).

* `@inheritParams` does a better job of munging links. 

  Links of the form `\link[=topic]{text}` are now automatically converted to
  `\link[pkg:topic]{text}` when inherited from other packages (#979).
  
  Internal `has_topic()` helper has a better implementation; this means that
  links should no longer be munged unnecessarily (#973).

* `\example{}` escaping has been considerably simplified (#967), and is now 
  documented in `escape_example()`.

* In `\usage{}`, S3/S4 methods are no longer double-escaped (#976).

* Markdown tables with cells that contain multiple elements (e.g. text and code)
  are now rendered correctly (#985).

* Markdown code blocks containing operators and other special syntax 
  (e.g. `function`, `if`, `+`) now converted to `\code{}` not `\verb{}` (#971).

# roxygen2 7.0.0

## New features

### New tags

* `@includeRmd {path.Rmd}` converts an `.Rmd`/`.md` file to `.Rd` and includes 
  it in the manual page. This allows sharing text between vignettes, 
  `README.Rmd`, and the documentation. See `vignette("rd")` for details (#902).

* `@order {n}` tag controls the order in which blocks are processed. You can
  use it to override the usual ordering which proceeds from the top of 
  each file to the bottom. `@order 1` will be processed before `@order 2`, 
  and before any blocks that don't have an explicit order set (#863).

* `@exportS3Method` tag allows you to generate `S3method()` namespace
  directives (note the different in capitalisation) (#796). Its primary use is 
  for "delayed" method registration, which allows you to define methods for 
  generics found in suggested packages (available in R 3.6 and greater).
  For example,
    
    ```R
    #' @exportS3Method package::generic
    generic.foo <- function(x, ...) {
    
    }
    ```
    
    will generate
    
    ```
    S3method(package::generic, foo)
    ```
    
    (See [`vctrs::s3_register()`](https://vctrs.r-lib.org/reference/s3_register.html)
    you need a version that works for earlier versions of R).
    
    It also has a two argument form allows you generate arbitrary `S3method()` 
    directives:
    
    ```R
    #' @exportS3Method generic class
    NULL
    ```
    
    ```
    S3method(generic, class)
    ```

*  New `@returns` is an alias for `@return` (#952).

### R6

roxygen2 can now document R6 classes (#922). See `vignette("rd")` for details.

### Markdown improvements

* Rd comments (`%`) are now automatically escaped. You will need to replace any
  existing uses of `\%` with `%` (#879).

* Markdown headings are supported in tags like `@description`, `@details`, 
  and `@return` (#907, #908). Level 1 headings create a new top-level 
  `\section{}`. Level 2 headings and below create nested `\subsections{}`.

* Markdown tables are converted to a `\tabular{}` macro (#290). roxygen2 
  supports the [GFM table syntax](https://github.github.com/gfm/#tables-extension-)
  which looks like this:
  
    ```md
    | foo | bar |
    | --- | --- |
    | baz | bim |
    ```

* Markdown code (``` `foofy` ```) is converted to to either `\code{}` or 
  `\verb{}`, depending on whether it not it parses as R code. This better
  matches the description of `\code{}` and `\verb{}` macros, solves a certain
  class of escaping problems, and should make it easier to include arbitrary 
  "code" snippets in documentation without causing Rd failures (#654).

* Markdown links can now contain formatting, e.g. `[*mean*][mean]` will now
  generate `\link[=mean]{\emph{mean}}`.

* Use of unsupported markdown features (e.g. blockquotes, inline HTML, 
  and horizontal rules) generates informative error messages (#804).

### Default usage

* The default formatting for function usage that spans multiple lines has
  now changed. Previously, the usage was wrapped to produce the smallest number 
  of lines, e.g.:
  
    ```R
    parse_package(path = ".", env = env_package(path), 
      registry = default_tags(), global_options = list())
    ```
    
    Now it is wrapped so that each argument gets its own line (#820):
    
    ```R
    parse_package(
      path = ".",
      env = env_package(path),
      registry = default_tags(),
      global_options = list()
    )
    ```
    
    If you prefer the old behaviour you can put the following in your
    `DESCRIPTION`:
    
    ```
    Roxygen: list(old_usage = TRUE)
    ```

### Code loading

roxygen2 now provides three strategies for loading your code (#822):

* `load_pkgload()`, the default, uses [pkgload](https://www.github.com/r-lib/pkgload). 
  Compared to the previous release, this now automatically recompiles your 
  package if needed.

* `load_source()` attaches required packages and `source()`s all files in `R/`. 
  This is a cruder simulation of package loading than pkgload (and e.g. is 
  unreliable if you use S4 extensively), but it does not require that the 
  package be compiled. Use if the default strategy (used in roxygen2 6.1.0 
  and above) causes you grief.

* `load_installed()` assumes you have installed the package. This is best
  used as part of a bigger automated workflow.

You can override the default either by calling (e.g.) `roxygenise(load_code = "source"))` or by setting the `load` option in your DESCRIPTION: `Roxygen: list(load = "source")`.

### Options

* As well as storing roxygen options in the `Roxygen` field of the 
  `DESCRIPTION` you can now also store them in `man/roxygen/meta.R` (#889).
  The evaluation of this file should produce a named list that maps option
  names to values. 
  
* roxygen now also looks for templates in `man/roxygen/templates` (#888).

* New `rd_family_title` option: this should be a named list, and is used to
  overrides the default "Other family: " prefix that `@family` generates. 
  For example, to override the prefix generated by `@family foo` place 
  `rd_family_title <- list(foo = "Custom prefix: ")` in 
  `man/roxygen/meta.R` (#830, @kevinushey).

## Breaking changes

* Rd comments (`%`) are automatically escaped in markdown formatted text. 
  This is a backward incompatible change because you will need to replace 
  existing uses of `\%` with `%` (#879).

* Using `@docType package` no longer automatically adds `-name`. Instead 
  document `_PACKAGE` to get all the defaults for package documentation, or
  use `@name` to override the default file name.

* `@S3method` has been removed. It was deprecated in roxygen2 4.0.0
  released 2014-05-02, over 5 years ago.

* Using the old `wrap` option will now trigger a warning, as hasn't worked
  for quite some time. Suppress the error by deleting the option from your
  `DESCRIPTION`.

### Extending roxygen2

The process for extending roxygen2 with new tags and new roclets has been completely overhauled, and is now documented in `vignette("extending")`. If you're one of the few people who have written a roxygen2 extension, this will break your code - but the documentation, object structure, and print methods are now so much better that I hope it's not too annoying! Because this interface is now documented, it will not change in the future without warning and a deprecation cycle. 

If you have previously made a new roclet, the major changes are:

* The previously internal data structures used to represent blocks and tags
  have been overhauled. They are now documented and stable. See `roxy_block()` 
  and `roxy_tag()` for details.

* `roclet_tags()` is no longer used; instead define a `roxy_tag_parse()` method.
  For example, if you create a new `@mytag` tag, it will generate a class of 
  `roxy_tag_mytag`, and will be parsed by `roxy_tag_parse.roxy_tag_mytag()` 
  method. The method should return a new `roxy_tag()` object with the 
  `val` field set.
  
    This means that the `registry` argument is no longer needed and has 
    been removed.

* `rd_section()` and `roxy_tag_rd()` are now exported so that you can more 
  easily extend `rd_roclet()` with your own tags that generate output in
  `.Rd` files.

* `global_options` is no longer passed to all roclet methods. Instead, use 
  `roxy_meta_get()` to retrieve values stored in the options (#918).

* `tag_two_part()` and `tag_words()` are now simple functions, not function 
  factories. 

* `tag_markdown_restricted()` has been removed because it did exactly the 
   same thing as `tag_markdown()`.

A big thanks goes to @mikldk for starting on the vignette and motivating me to make the extension process much more pleasant (#882).

## Bug fixes and minor improvements

* Empty roxygen2 lines at the start of a block are now silently removed (#710).

* Whitespace is automatically trimmed off the `RoxygenNote` field when 
  comparing the installed version of roxygen2 to the version used to 
  generate the documentation (#802).

* Files generated on Windows systems now retain their existing line endings, or 
  use unix-style line endings for new files (@jonthegeek, @jimhester, #840).
  
* roxygen2 now recognises fully qualified S4 functions like 
  `methods::setGeneric()`, `methods::setClass()` and `methods::setMethod()`
  (#880).

* Package documentation now converts ORCIDs into a useful link (#721).
  The package logo (if found at `man/images/logo.png`) is now scaled to 120px 
  wide (@peterdesmet, #834).

* Documenting an S4 method that has a `.local()` wrapper no longer fails with 
  an obscure error message (#847).

* Functions documented in `reexports` are now sorted alphabetically by
  package (#765).

* `@describeIn` can now be used with any combination of function types 
  (#666, #848).

* `@description` and `@detail` tags are automatically generated from the 
  leading description block, and now have correct line numbers (#917).

* `@example` and `@examples` are interwoven in the order in which they
  appear (#868).

* In `@examples`, escaped `'` and `"` in strings are no longer doubly escaped 
  (#873).

* `@family` automatically adds `()` when linking to functions (#815),
  and print each link on its own line (to improve diffs).

* When `@inherit`ing from external documentation, `\link{foo}` links 
  are automatically transformed to `\link{package}{foo}` so that they work in 
  the generated documentation (#635). `\href{}` links in external inherited are 
  now inserted correctly (without additional `{}`) (#778).

* `@inherit`ing a a function with no arguments no longer throws a confusing
  error message (#898).

* `@inheritDotParams` automatically ignores arguments that can't be inherited
  through `...` because they are used by the current function (@mjskay, #885).
  
* `@inheritDotParams` includes link to function and wraps parameters
  in `\code{}` (@halldc, #842).

* `@inheritDotParams` can be repeated to inherit dot docs from multiple 
  functions (@gustavdelius, #767).

* `@inheritDotParams` avoids multiple `...` arguments (@gustavdelius, #857).

* `@inheritParams` ignores leading dots when comparing argument names (#862).

* `@inheritParams` warns if there are no parameters that require 
  documentation (#836).

* `@param` containing only whitespce gives a clear warning message (#869).

* Multiple `@usage` statements in a single block now generate a warning. 
  Previously, the first was used without a warning.

# roxygen2 6.1.1

* Now specifically imports recent version of desc package (>= 1.2.0) to
  fix various parsing issues (@crsh, #773, #777, #779). Multi-line DESCRIPTION 
  collate directives now correctly parsed on windows (@brodieG, #790).

* `roxygenise()` no longer recompiles packages containing src code (#784).

* `roxygenise()` now stops with an informative error message when run in a
  directory that's not the package root (@mikmart, #704).

# roxygen2 6.1.0

## New features

* The `NAMESPACE` roclet now works in two passes - it first generates the
  `NAMESPACE` containing only import directives because this can be generated
  without evaluating the code in the package. This alleviates a problem
  where it was previously possible to get into a state that you could only
  get out of by carefully editing the `NAMESPACE` by hand (#372).

* `@evalRd foo()` evaluates `foo()` defined in the package namespace and inserts
  the results into the current block (#645). The code should return a character
  vector with one entry for each line (and they should not start with `#'`).

    There are two small limitations to the current implementation:

    1. The generated roxygen will not affect the `@md`/`@noMd` status
    2. `@evalRd` does not work inside templates.

* `@evalNamespace` does for `NAMESPACE` what `@evalRd` does for Rd files:
  you give it R code that produces a literal entry in `NAMESPACE` when
  run. This should make it easier to export functions that are generated by
  other functions in your package (#531, @egnha).

* `@inherits` can now inherit examples (#588).

* `vignette("rd")` received a thorough updating for current best-practices.
  The vignette still needs more work so pull requests are greatly appreciated
  (#650).

* `roxygenise()` uses `pkgload::load_all()` instead of a home grown solution
  to simulate package loading (this is needed because roxygen2 uses run-time
  information to generate the documentation). This should reduce S4 related
  problems and ensures that `devtools::document()` and `roxygenise()` always
  have exactly the same behaviour (#568, #595).

* If an inherited section cannot be found, the warning contains the help
  page from which that section was requested (#732, @krlmlr).

* roxygen2 now always reads and writes using UTF-8 encoding. If used with a
  package that does not have `Encoding: UTF-8` in the DESCRIPTION, you'll
  now get a warning (#564, #592).

## Extension API

* Roxygen blocks now have an official structure as encoded in
  `roxy_block()`. It is a named list containing the tags with attributes
  providing other metadata.

* The `parsed` argument to `roclet_process()` have been replaced with
  separate `blocks` and `env` arguments.

* New `roclet_preprocess()` generic makes it possible for roclets to perform
  actions before code is evaluated.

* `parse_package()`, `parse_file()` and `parse_code()` provide an exported API
  that allows you to use roxygen's parsing code independently of creating
  roclets.

## Minor improvements and bug fixes

* All tags (including `@alias`) are now de-duplicated and consistently sorted.
  This reduces spurious diffs (#586, @flying-sheep).

* `@concept` now generates one `\concept` per tag (#611).

* The default `@description` (i.e. the title) is now added much later in the
  process. That means that `@inherit description` now works when you have
  specified a title for the inheritor (#629) and the default description
  is slightly nicer when merging multiple blocks.

* `@family` automatically adds its value to concepts (#611).

* `@inherits`: The mechanism for extracting inherited Rd does a better job of
  preserving escapes (#624)

* Empty `.Rbuildignore` now handled correctly (#576).

* Stricter regular expression ensures only files ending with `.R` or `.r` are
  parsed for roxygen comments (#625).

* Objects with names starting with a dot are now by default documented in
  files with prefix 'dot-'.

* Roclets can now access global options as designed. This allows templates to
  use markdown formatting if set globally (#594).

* You can now autogenerate package documentation even if you don't have
  `Authors@R` (#606).

* Multiple given and/or family names are now supported in the
  `Authors@R` field of the DESCRIPTION file (#672, @sgibb).

* If a package logo exists (`man/figures/logo.png`) it will be automatically
  included in generated package docs (#609).

* Usage for data objects now correctly generated, avoiding double escaping
  other components of usage (#562).

* Improvements to markdown translation:

    * Code in link text is now properly rendered as code (#620, @egnha).

    * Whitespace between words in link text is now preserved as single
      space for links of the form `[text][fcn]` and `[text](URL)`
      (#628, #754, #760, @egnha and @jennybc).

    * `%` in inline code (#640), code blocks (@nteetor, #699) and
      links (#724) is now automatically escaped.

    * Parsing of markdown links has been tweaked to reduce false positives
      (#555). If you still get a false positive, you can now put `\\` in front
      of the `[` to avoid it being converted to a link (#720). Links can no
      longer be followed by `{` to avoid spurious matches to Rd commands like
      `\Sexpr{}`.

    * Unsupported markdown features now generate a mildly helpful warning
      instead of throwing an utterly useless error (#560).

* `person()` now supports all
  [MARC Relator](http://www.loc.gov/marc/relators/relaterm.html) role codes
  (#662, @publicus).

* `topic_add_usage()` now outputs formatted "Usage" section with max
  width of 80 characters thanks to a now more flexible `wrap_string()`
  (@JoshOBrien, #719).

# roxygen2 6.0.1

* Allowing empty lines in .Rbuildignore. Previously, empty lines caused all
  files to be ignored. (#572, @jakob-r)

* Automatically generating a usage section for an infix function containing "<-"
  no longer removes "<-" from the function name (#554).

# roxygen2 6.0.0

## Markdown

* Most fields can now be written using Markdown markup instead of the
  traditional Rd language. You can turn on Markdown globally by adding
  `Roxygen: list(markdown = TRUE)` to `DESCRIPTION`. The `@md` / `@noMd`
  tags turn Markdown parsing on / off for the given block. See
  `vignette("markdown")` for more details (#364, #431, #499, #506, #507),
  by @gaborcsardi

## Improved inheritance

* New `@inheritDotParams` allows you to automatically generate parameter
  documentation for `...` for the common case where you pass `...` on to
  another function (#512). Because you often override some arguments, it
  comes with a flexible specification for argument selection:

    * `@inheritDotParams foo` takes all parameters from `foo()`
    * `@inheritDotParams foo a b e:h` takes parameters `a`, `b`, and all
       parameters between `e` and `h`
    * `@inheritDotParams foo -x -y` takes all parameters except for `x` and `y`.

    The documentation generated is similar to the style used in `?plot`
    and will eventually be incorporated in to RStudio's autocomplete.

* New `@inherit` generalises `@inheritParams`, and allows to you inherit
  parameters, return, references, title, description, details, sections, and
  seealso.  The default `@inherit my_fun` will inherit all, you can document
  an object entirely by specifying only the `@inherit` tag.  Alternatively,
  you can select specific tags to inherit with `@inherit my_fun return params`
  (#384).

* New `@inheritSection fun title` allows you to inherit the contents of
  a single section from another topic (#513).

* `@inheritParams` now works recursively, so that you can inherit parameters
  from a function that inherited its parameters from somewhere else.  It
  also better handles `\dots` as an alias for `...` (#504).

## Minor improvements and bug fixes

### Tags

* `@aliases` are no longer sorted alphabetically, but instead match the
  order of their usage. This gives you more control in pkgdown.

* `@describeIn` now escapes special characters in function names (#450).

* `@family` see alsos are added in the same order they appear, not
  alphabetically (#315). Fixed an issue where `.`s were sometimes added
  between words within a `@family` tag (#477, @kevinushey).

* `@author` is rendered after `@seealso`.

* `@example` gives a nice warning message if you accidentally use it instead
  of `@examples` (#494). Multiple `@examples` sections are merged (#472, @krlmlr).

* Roxygen will no longer write out topics that don't have a name or title,
  and will instead generate a warning. This makes it easier to detect if
  you've accidentally used `@rdname` with an incorrect value (#474).

### S3

* Non-primitive, internal S3 generics (e.g. 'rbind', 'cbind') are now properly
  detected as S3 generics. (#488, @kevinushey)

* Ensure that `functions` with S3 class are still treated as functions (#455).

* S3 method declarations via `R.methodS3::setMethodS3()` and function
  declarations via `R.oo::setConstructorS3()` are now supported
  (@HenrikBengtsson, #525).

### S4

* You can now document `setClassUnion()`s (#514).

* The default alias for S4 method now re-adds trailing ANY signatures
  that are sometimes dropped (#460).

* Back references are now wrapped over multiple lines, if long
  (#493, @LiNk-NY).

### Other

* `"_PACKAGE"` documentation now generates a default `@seealso` combining
  the `URL` and `BugReport` fields, and a default `@author` field generated
  from the `Authors@R` field (#527). It now works from `roxygenise()`; before
  it only worked from `devtools::document()` (#439, @krlmlr).

* Manually created `NAMESPACE` or documentation files are never overwritten,
  even if using `roxygen2` for the first time (@krlmlr, #436).

* Changes to DESCRIPTION (i.e. `Collate:` and `RoxygenNote`) now use
  the desc package. This will minimise spurious changes (#430).

* `default_data_format()` has been renamed to `object_format()`.

* New `roclet_find()` provides a more flexible way to specify roclets:
  as roclet name (e.g. "rd_roclet"), in an package ("foo::roclet_bar"),
  or with options ("foo::roclet_bar(baz = TRUE)").

* The usage of replacement functions uses non-breaking spaces so that `<-`
  will never get put on its own line (#484).

* Roxygen now parses nonASCII documentation correctly (as long as UTF-8
  encoded or specified Encoding in DESCRIPTION) (#532, @shrektan),
  and ignores files listed in `.Rbuildignore` (#446, @fmichonneau).

## Extending roxygen2

* Deprecated `register.preref.parser()` and `register.preref.parsers()`
  have been removed. `register_tags()` has also been removed in favour of
  a new `roclet_tags()` generic.

* `roclet()` (the constructor), `roclet_tags()`, `roclet_process()`
  `roclet_output()`, `roc_clean()` and now exported making it possible
  to create roclets in other packages.  Helper functions `roxy_tag()` and
  `roxy_tag_warning()` are also exported.

* `new_roclet()` is no longer exported - use `roclet()` instead.

# roxygen2 5.0.1

* Use `ls()`, not `names()` to list elements of environment: fixes R 3.1.0
  incompatibility (#422, @kevinushey).

* `@export` again allows trailing new line (#415).

* Fixed bug in `@noRd`, where usage would cause error (#418).

# roxygen2 5.0.0

## New features

* Roxygen now records its version in a single place: the `RoxygenNote`
  field in the `DESCRIPTION` (#338). This will be the last time an roxygen2
  upgrade changes every file in `man/`.

*   You can now easily re-export functions that you've imported from another
    package:

    ```R
    #' @export
    magrittr::`%>%`
    ```

    All imported-and-re-exported functions will be documented in the same
    file (`rexports.Rd`), containing a brief description and links to the
    original documentation (#376).

*   You can more easily generate package documentation by documenting the
    special string "_PACKAGE" (@krlmlr, #349):

    ```R
    #' @details Details
    "_PACKAGE"
    ```

    The title and description will be automatically filled in from the
    `DESCRIPTION`.

* New tags `@rawRd` and `@rawNamespace` allow you to insert raw (unescaped)
  in Rd and the `NAMESPACE` (this is useful for conditional imports).
  `@evalRd()` is similar, but instead of literal Rd, you give it R code that
  produces literal Rd code when run. This should make it easier to experiment
  with new types of output (#385).

* Roxygen2 now parses the source code files in the order specified in the
  `Collate` field in `DESCRIPTION`. This improves the ordering of the generated
  documentation when using `@describeIn` and/or `@rdname` split across several
  `.R` files, as often happens when working with S4 (#323, #324).

## Minor features and bug fixes

* The contents of documented functions are now also parsed for roxygen comments.
  This allows, e.g., documenting a parameter's type close to where this type is
  checked, or documenting implementation details close to the source, and
  simplifies future extensions such as the documentation of R6 classes
  (#397, @krlmlr).

* Data objects get a simpler default `@format` that describes only the
  object's class and dimensions.  The former default, generated by generated by
  `str()`, didn't usually produce useful output and was quite slow. The new S3
  generic `default_data_format()` generates the format and can be overridden to
  generate a custom format (#410, @krlmlr).

* The roxygen parsers has been completely rewritten in C++ (#295). This gives a
  nice performance boost and gives:

  * Better error messages: you now get the exact the line number of the
    tag, not just the start of the block.

  * The parser has been simplified a little: tags now must always start
    on a new line. This is recommended practice anyway, and it means
    that escaping inline `@` (with `@@`) is now optional. (#235)

  * Unknown tags now emit a warning, rather than an error.

* `@examples` no longer complains about non-matching braces inside
  strings (#329).

* `@family` now cross-links each manual page only once, instead of linking
  to all aliases (@gaborcsardi, #283, #367).

* The special `@include` parser has also been rewritten in C++, giving
  a performance boost for larger packages (#401). This is particularly
  important because it's also called from `devtools::load_all()`.
  Additionally, a space before `@include` is no longer necessary
  (@krlmlr, #342).

* `@inheritParams foo::bar` ensures that `%` remains escaped (#313).

* If you document multiple arguments with one `@param`, (e.g. `@param a,b,c`)
  each parameter will get a space after it so it can be wrapped in the
  generated Rd file (#373).

* `@section`s with identical titles are now merged together, just like
  `@description` and `@details`. This is useful in conjunction with the
  `@rdname` tag. (@krlmlr, #300).

* Automatic `@usage` is now correctly generated for functions with string
  arguments containing `"\""` (#265).

* `load_options()` is now exported so `devtools::document()` doesn't have to
  run `update_collate()` twice (#395).

* `update_collate()` only rewrites the `Collate` entry in the DESCRIPTION file
  when it changes (#325, #723).

* An empty `NAMESPACE` file is written if it is maintained by `roxygen2`
  (@krlmlr, #348).

* Data that is not lazy-loaded can be documented (@krlmlr, #390).

## Internal changes

* `register.preref.parser()` and `register.preref.parsers()`  have been
  deprecated - please use `register_tags()` instead.

* Parser callbacks registered with `register_tags()` are now called for fields
  parsed from the "introduction" (the text before the first tag)
  (@gaborcsardi, #370).

# roxygen2 4.1.1

* Formatting of the `Authors@R` field in the DESCRIPTION file is now retained
  (@jranke, #330).

* The collate roclet falls back to `base::strwrap()` when generating the
  collate field. This makes roxygen2 compatible with the next version of
  stringr.

* New "vignette" roclet. This vignette automatically rebuilds all out of date
  vignettes (#314).

* An off-by-one error in the C++ Roxygen preparser was fixed.

* The new `@backref` tag makes it possible to override the sourceref for
  R code generators like `Rcpp` (@krlmlr, #291, #294).

# roxygen2 4.1.0

* The source of the documentation is added to autogenerated `.Rd` files.

* If there are no `@include` tags, roxygen2 leaves the collate field alone.
  This makes it easier to convert an existing project that uses a predefined
  collate, but if you start with `@include` and later remove them, you'll
  need to also remove the collate field (#302, #303).

* Protected a `dir()` with `sort_c()` - If you'd noticed an inconsistency in
  ordering between `devtools::document()` and `devtools::check()` this
  was the cause of that.

* Fixed broken regular expression that caused problems with stringr 1.0.0.

* The `Authors@R` field in `DESCRIPTION` is now longer wrapped(@krlmlr, #284).

* `@describeIn` with plain functions now correctly includes the function name
  and can be applied to data documentation. (@jimhester, #285, #288).

* Works again when called from `Rscript` and `methods` is not loaded
  (@krlmlr, #305).

# roxygen2 4.0.2

* If you don't use `@exports` or other namespace directives, your namespace
  file will not be touched (#276).

* Methods no longer automatically attempt to inherit parameters from
  their generic. It's too fraught with difficulty (#261).

* Roxygen now understands what to do with `setReplaceMethod()` (#266).

* Parameter documentation is ordered according to the order of the formals, if
  possible (@krlmlr, #63).

* Export `is_s3_method()`.

* Roxygen no longer fails when run in non-UTF-8 locales on windows.

# roxygen2 4.0.1

* Explicit `updateRoxygen()` is no longer needed - `roxygenize()` does the
  right thing the first time it is run.

* Exporting a S4 generic works (#246).

* `roxygenise()` no longer complains about absence of `wrap` field because it's
  so unlikely that anyone wants the old behaviour (#245).

# roxygen2 4.0.0

Roxygen2 4.0.0 is a major update to roxygen2 that makes provides enhanced error handling and considerably safer default behaviour. Now, roxygen2 will never overwrite a file that it did not create. This means that before you run it for the first time, you'll need to run `roxygen2::upgradeRoxygen()`. That will flag all existing files as being created by roxygen2.

## New features

* Six vignettes provide a comprehensive overview of using roxygen2 in
  practice. Run `browseVignettes("roxygen2")` to access.

* `@describeIn` makes it easier to describe multiple functions in
  one file. This is especially useful if you want to document methods with
  their generic, or with a common class, but it's also useful if you want
  to document multiple related functions in one file (#185).

* `@field` documents the fields on a reference class (#181). It works the
  same way as `@slot` for S4 classes.

* You can now document objects defined elsewhere (like datasets) by
  documenting their name as a string (#221). For example, to document an
  dataset called `mydata`, you can do:

    ```R
    #' Mydata set
    #'
    #' Some data I collected about myself
    "mydata"
    ```

* Roxygen2 now adds a comment to all generated files so that you know
  they've been generated, and should not be hand edited.

* Roxygen2 no longer wraps the text in Rd files by default, i.e. the default
  option is `wrap = FALSE` now. To override it, you have to specify a field
  `Roxygen: list(wrap = TRUE)` in `DESCRIPTION` (#178).

* Roxygenise automatically deletes out-of-date Rd files in `man/`.

## Improved error handling

* Roxygen2 will never overwrite a file that was not generated by
  roxygen2. This means that the first time you use this version of
  roxygen, you'll need to delete all existing Rd files. `roxygenise()`
  gains a clean argument that will automatically remove any files
  previously created by roxygen2.

* Parsing is stricter: many issues that were previously warnings are
  now errors. All errors should now give you the line number of the
  roxygen block associated with the error.

* Every input is now checked to make sure that you have matching braces
  (e.g. every `{` has a matching `}`). This should prevent frustrating
  errors that require careful reading of `.Rd` files (#183).

* `@section` titles and `@export` tags can now only span a single line
  to prevent common bugs.

* `@S3method` is deprecated - just use `@export` (#198).

* Namespace tags now throw parsing errors if you give them bad inputs (#220).

* Better error message if you try to document something other than NULL,
  an assignment, a class, a generic or a method (#194).

## Bug fixes and minor improvements

* Better parsing of non-syntactic function names in other packages when
  used in `@inheritParams` (#236).

* Deprecated arguments to `roxygenise()` (`roxygen.dir`, `copy.package`,
  `overwrite`, `unlink.target`) removed.

* Remove unneeded codetools and tools dependencies.

* Bump required Rcpp version to 0.11.0, and remove custom makefiles.

* Non-syntactic argument names (like `_x`) are now surrounded by back-ticks
  in the usage (#191).

* The internal parsers are no longer part of the public roxygen2 interface.

* Usage statements in generated roxygen statements non-longer contain
  non-ASCII characters and will be wrapped if long (#180).

* By default, reference classes now only document their own methods,
  not their methods of parents (#201).

* Default aliases always include the original name of the object, even if
  overridden by `@name`. This also means that `A <- setClass("A")` will get
  two aliases by default: `A` and `A-class` (#202). Use `@aliases NULL` to
  suppress default alias.

* Non-syntactic class names (like `<-`) are now escaped in the usage
  section of S4 methods (#205).

* Eliminated two more cases where wrapping occurred even when `wrap = FALSE`.

# roxygen2 3.1.0

## Documentation for reference classes

It's now possible to document reference classes, using the "docstring"
convention described in `?setRefClass`. If you want to provide a short
paragraph description of what a method does, make the first component of the
message a string containing the description, e.g.:

```R
setRefClass("A", methods = list(
  f = function(a, b) {
    "Take numbers \code{a} and \code{b} and add them together"
    a + b
  }
))
```

Unlike the documentation for R functions, the documentation for methods can
be quite succinct.

Roxygen adopts the convention that documented methods are public, and will
be listed in the man page for the object. Undocumented methods are private and
will not be shown in the documentation. The methods for all superclasses are
also listed, so that you don't need to flip through multiple pages of
documentation to understand what you can do with an object. All documented
methods will be placed in a bulleted list in a section titled "Methods", the
method usage will be automatically prepended to the docstring.

## Minor fixes and improvements

* Fixes for Rcpp 0.11.0 compatibility.

* `roxygenise()` now invisible returns a list of all files generated
  by individual roclets. This is useful for tools that want to figure
  out if there are extra files in the `man/` directory.

* `is_s3_generic()` now recognises group generics (#166).

* Don't try and add parameters for data objects (#165).

* Sort output of families using C locale (#171).

* `@family` now escapes function names in references (#172).

# roxygen2 3.0.0

Roxygen2 now fully supports S4 and RC (reference classes) - you should no
longer need to manually add `@alias` or `@usage` tags for S4 classes, methods
and generics, or for RC classes.

* The default usage definitions are much better, generating the correct
  usage for data sets (#122), S3 methods (without additional `@method` tag),
  S4 generics, S4 methods, and for replacement (#119) and infix functions.
  Backslashes in function arguments in are correctly escaped. Usage statements
  also use a more sophisticated line wrapping algorithm so that they should
  cause fewer problems with the R CMD check line limit. (#89, #125).

* S4 classes, S4 methods, and RC classes are given better default topics,
  and the file names corresponding to those topics are shorter.

* S4 methods will automatically inherit parameter documentation from their
  generic.

* `@slot name description` allows you to document the slots of a S4 class.

S3 support has also been improved: roxygen2 now figures out whether a function
is a S3 method or generic. (In the rare cases it does so incorrectly, use
`@method` to manually describe the generic and class associated with a method). This means you can remove existing uses of `@method`, and can replace
`@S3method` with `@export`.

Roxygen now has support for package specific options through the `Roxygen`
field in the `DESCRIPTION`. The value of the field should be R code that
results in a list. Currently only `wrap` and `roclet` values are supported:

* Turn off Rd re-wrapping with adding `Roxygen: list(wrap = FALSE)`

* Change the default roclets by specifying
  `Roxygen: list(roclets = c("collate", "rd"))`

Roxygen 3.0 also includes a number of minor fixes and improvements:

* Infix functions are now escaped correctly in the `NAMESPACE`. (Thanks to
  @crowding, #111)

* `roxygenise()` now works more like `devtools::document()` and only ever works
  in the current directory. The arguments `roxygen.dir`, `overwrite`,
  `copy.package` and `unlink.target` have been deprecated due to potential
  data loss problems.

* The collate roclet is no longer a roclet: it processes R files using custom
  code (only statically, not dynamically) and is designed to be executed before
  the code is sourced.  Run `update_collate()` to update the Collate directive
  based on `@include` tags - if there are none present, a collate directive
  will not be generated.

* `@useDynLib` now works with more possible specifications - if you include a
  comma in the tag value, the output will be passed as is. This means that
  `@useDynLib mypackage, .registration = TRUE` will now generate
  `useDynLib(mypackage, .registration = TRUE)` in the `NAMESPACE`. (#124)

* `inst` directory not created by default (#56).

* Explicitly depend on `utils` and `methods` packages to make roxygen
  compatible with `Rscript` (#72). Import `digest` package instead of
  depending on it.

* Always use C locale when sorting `NAMESPACE` file or tags in `.Rd` files.
  This ensures a consistent ordering across systems (#127).

* Templates with extension `.r` are supported on case-sensitive file systems
  (#115). Template variables now actually work (#160, thanks to @bronaugh).

* Suppress default aliases, format and usage with `@aliases NULL`,
  `@format NULL` and `@usage NULL`.

# roxygen2 2.2.2

* Correctly use keyword `datasets` not `dataset` (Fixes #60)

* Reference classes no longer given incorrect docType (data).

# roxygen2 2.2.1

* Use unicode escapes in test files so tests pass on all platforms.

* Work around bug in `gsub` in C locale by manually specifying `Encoding()`.

# roxygen2 2.2

## New features

* Package docType will automatically add package alias, if needed. (Fixes #4)

* Data docType will automatically add `datasets` keyword, default usage, and
  default format. (Fixes #5). Data docType automatically added to data
  objects.

* New `@encoding` tag for manually setting non-ASCII encodings when needed.
  (Fixes #7)

## Bug fixes

* `write.description()` now tries much harder to respect
  users' original DESCRIPTION field formatting instead of forcibly
  re-wrapping certain fields at 60 characters.

* `@details` and `@description` now work correctly

* `@useDynLib` now works correctly:

       @useDynLib packageName routine1 routine2

   produces

       useDynLib(packageName, routine1)
       useDynLib(packageName, routine2)

   in the `NAMESPACE` file, instead of separate (wrong) useDynLib statements as
   before.

* All namespace import directives now behave in the same way as the export
  directives, producing multiple single directives instead one multiple
  directive: `@importClassesFrom pkg a b` now produces
  `importClassesFrom(pkg, a)` and `importClassesFrom(pkg, b)`

* In example files included with `@example` you can now use infix operators
  (e.g. %*%) or other things with %, because they will be preceded by a
  backslash in the Rd file. This behaviour was already in place for examples
  directly included with `@examples`.

* Aliases are no longer quoted, and % is escaped with a backslash (Fixes #24).
  Names also have % escaped (Fixes #50)

* Replacement functions (e.g. `foo<-`) now get correct usage statements:
  `foo() <- value` instead of `foo()<-value`. (Fixes #38)

* Functions with no arguments now correctly get usage statements (Fixes #35)

* Indentation in examples now preserved (Fixes #27)

* roxygen2 will replace characters that are not valid in filenames with a
  character substitute, e.g. `[]` becomes `sub`, `<-` becomes `set` (Fixes #6)

* Usage strings use non-breaking spaces to prevent string default values
  containing whitespace to be split across multiple lines. This may cause
  problems in the unlikely event that you have default value containing a
  non-breaking space (`"\uA0"')  (Fixes #21)

* Functions with quoted names now get correct usage statements (Fixes #41)

* Objects that no longer exist are not documented (Fixes #42)

* Errors now display file name and line number of roxygen block to help you
  find the problem. Thanks to code contributions from Renaud Gaujoux.
  (Fixes #13)

* Documentation with no untagged text but with `@title`, `@description` and
  `@details` tags now produces correct output.

# roxygen2 2.1

## New features

* package dependencies loaded automatically

* added support for the `@source` tag

## Bug fixes

* `NAMESPACE` file no longer needs to exist

* `Collate` field in `DESCRIPTION` no longer needs to exist

* `=` now recognised as way of assigning functions

* `x$y <- function() {...}` no longer causes error

* `@example` no longer added extra new-lines.

* Correct directory normalisation under windows fixes broken test.

* A special thanks goes to Yihui Xie who contributed all of the fixes and
  improvements (bar one) in this version!

# roxygen2 2.0

## Major changes

* now works with run-time details to give more accurate output. This requires
  that the source code that roxygen is documenting be loaded prior to
  documentation. roxygen will attempt to do so, but you need to ensure
  required packages are loaded.

    Run-time data fixes some long standing bugs where roxygen couldn't correctly
    figure out function usage. We are not aware of any cases where you still
    need to use the `@usage` tag.

* written in idiomatic R, and uses S3 instead of a homegrown class system.

* roclets build up an internal data structure instead of writing to disk
  directly. This means that you can now use the `@rdname` tag to merge
  documentation for multiple functions into one file, and that only unique
  namespace directives are written to `NAMESPACE` (which makes `@importFrom`
  much more useful).

* some features have been removed, and may or may not (based on your feedback)
  be reincluded. These include the callgraph roclet, and `R CMD roxygen`,
  which only worked on some systems.

* a templating system: use the `@template` tag to insert a `brew` template
  stored in `man-roxygen`. Template variables can be set using `@templateVar
  name value` and retrieved from within the template with `<%= name %>`

* extensive use of caching to make repeated runs as fast as possible. To clear
  caches and guarantee a complete rebuild, use `clear_caches()`.

* parsing of "introduction" (the text before the first tag) has changed. Now
  the title consists of the first paragraph (i.e. all text before the first
  empty line), the second paragraph is the description and all others are put
  in the details. Any component can be overridden with `@title`,
  `@description` and `@details` as appropriate.

## Minor changes

* `@name` is always output as an alias, even if `@aliases` are used.

* `@export` correctly uses `@method` to generate `S3method` namespace
  directive

## New tags

* `@rdname filename` sets the output filename (without extension). Use for
  functions non-alphanumeric functions (e.g. `[<-`) or if you want to document
  multiple functions in one file

* `@template templatename` includes a documentation template (see above)

* `@section Section title: contents` includes a section with any title. Don't
  forget the colon! That separates the title of the section from its contents.

* `@description` and `@details` tags allow you to specify description and
  details components in a template

* `@family family name` automatically adds see-also cross-references between
  all functions in a family. A function can belong to multiple families.

* `@inheritParams name` allows you to inherit the documentation for parameters
  from another function, either within the current package (`function`) or in
  any other installed package (`package:function`). Currently only supports
  single inheritance (i.e. you can't inherit from a function that inherits
  from another function), but you can have multiple @inheritParams tags.

* `@format` has been implemented; it existed in the roxygen package but was
  actually ignored
