
# renv 0.13.2

* `renv::install("user/repo/subdir with spaces")` now works as expected. (#694)

* `renv` can now parse package dependencies declared by
  `targets::tar_option_set(packages = <...>)`. (#698)

* `renv` no longer performs an automatic snapshot following a user-canceled
  `renv` action -- for example, if `renv::restore()` is canceled, the next
  automatic snapshot will be suppressed. (#697)

* Added the `vcs.ignore.local` project setting, to control whether the
  project's `renv/local` folder is added to `renv`'s VCS ignore file
  (e.g. `renv/.gitignore`). (#696)

* Fixed an issue where `renv`'s bootstrapping code could inadvertently bootstrap
  with the wrong version of `renv`, if the source and binary versions of `renv`
  on CRAN were not in sync. (#695)
  
* Fixed an issue where `renv::status()` could provide a misleading message
  for packages which are recorded in the lockfile, but not explicitly
  required by the project. (#684)

# renv 0.13.1

* `renv::clean()` gains the `actions` argument, allowing the caller to control
  which specific actions are taken during a `clean()` operation.

* `renv` no longer performs an automatic snapshot after a call to
  `renv::status()`. (#651)

* Fixed an issue where attempts to transform RSPM repository URLs could
  fail if the copy of R was installed without a local `CRAN_mirrors.csv`
  file.

* Fixed an issue where `renv::init()` could fail when passed a relative
  path to a directory. (#673)

* Fixed an issue where `renv::dependencies()` would miss dependencies in
  R Markdown YAML headers containing multiple output formats. (#674)

* `renv::install()` now better respects the `Remotes` field in a project
  `DESCRIPTION` file, if available. (#670)

* `RENV_DOWNLOAD_METHOD` is now treated as an alias for
  `RENV_DOWNLOAD_FILE_METHOD`.

* Fixed an issue where `renv` would fail to load if the `~/.Rprofile` existed
  but was an empty file.

# renv 0.13.0

* `renv::snapshot()` no longer creates an `renv/activate.R` file in the project
  folder if one does not already exist. (#655)
  
* The `renv::hydrate()` function gains the `update` argument, used to control
  whether `renv::hydrate()` chooses to update packages when invoked. When set
  to `TRUE`, if the version of a package installed in the source library is
  newer than that of the project library, then `renv` will copy that version
  of the package into the project library. (#647)
  
* The `RENV_PATHS_PREFIX_AUTO` environment variable can now be set to instruct
  `renv` to include an OS-specific component as part of the library and
  cache paths. This is primarily useful for Linux systems, where one might
  want to share a global cache with multiple different operating systems.
  The path component is constructed from the `ID` and `VERSION_CODENAME` /
  `VERSION_ID` components of the system's `/etc/os-release` file.
  
* `renv`'s dependency discovery machinery now has preliminary support
  for packages imported via the [box](https://github.com/klmr/box) package;
  e.g. `box::use(dplyr[...])`.

* Multiple cache paths can now be specified, with each cache path separated
  by either a `;` or `:`. This can be useful when you'd like to use multiple
  package caches within the same project; for example, because you'd like to
  share a read-only cache with a set of projects. (#653, @vandenman)

* Fixed an issue where `renv` could fail to discover dependencies in directories
  with very large `.gitignore` or `.renvignore` files. (#652)

* `renv` gains a new configuration option, `install.shortcuts`. When enabled,
  if `renv` discovers that a package to be installed is already available in
  the user or site libraries, `renv` will instead install a copy of that package
  into the project library. (#636)

* `renv` gains a new function, `renv::use()`, used to download, install, and
  load a set of packages directly within an R script. `renv::use()` can make it
  easier to share a standalone R script, with the packages required to install
  that script embedded directly in the script. It is inspired in part by the
  [groundhog](https://groundhogr.com/) package.

* `renv::install(".")` can now be used to install a package from sources within
  the current working directory. (#634)

* Fixed an issue where `renv::update()` could fail if a package installed from
  GitHub was missing the `RemoteHost` field in its DESCRIPTION file. (#632)

* `renv` now has support for custom project profiles. Profiles can be used to
  activate different sets of project libraries + lockfiles for different workflows
  in a given project. See `vignette("profiles", package = "renv")` for more
  details.
  
* Fixed an issue where attempts to initialize an `renv` project in a path
  containing non-ASCII characters could fail on Windows. (#629)

* Fixed an issue where `renv::install("<package>")` could fail if `renv` chose
  to install the package from MRAN rather than from one of the active package
  repositories. (#627)

* `renv` again defaults to using the project's `renv/staging` folder for staged
  / transactional installs. Use the `RENV_PATHS_LIBRARY_STAGING` environment
  variable if more granular control over the staging library path is required.
  This fixes issues on Windows with creating junction points to the global
  package cache on Windows. (#584)
  
* `renv` no longer skips downloading a requested source package if an existing
  cached download exists and appears to be valid. This should help avoid issues
  when attempting to install a package whose associated tarball has changed
  remotely. (#504)
  
* During bootstrap, `renv` will now attempt to download and unpack a binary
  copy of `renv` if available from the specified package repositories.

* `renv` now always attempts to bootstrap itself from the R Project's
  Cloud package repository, as a fallback in case no other repository
  is available. (#613)

* `renv::rebuild(<package>)` now uses the latest-available version of the
  requested package(s) if those packages are not currently installed.

* Fixed an issue where `renv::restore(library = "/path/to/lib")` would fail to
  restore packages, if those packages were already installed on the active
  library paths (as reported by `.libPaths()`). (#612)
  
* `renv::snapshot()` gains the `reprex` argument. Set this to `TRUE` if you'd
  like to embed an `renv` lockfile as part of a reproducible example, as
  generated by the [`reprex`](https://www.tidyverse.org/help/#reprex-pkg)
  package.
  
* `renv::status()` now reports packages that are referenced in a project's
  scripts, but are neither installed in the project library nor recorded in the
  lockfile. (#588)

* Fixed an issue where package installation could fail if the `configure.vars`
  option was set to be a named character, rather than a named list. (#609)

# renv 0.12.5

* Fixed an issue where `renv` would fail to bootstrap. (#608)

# renv 0.12.4

* `renv` now invalidates the available packages cache if the `https_proxy`
  environment variable changes. (#579)
  
* `renv::install(<pkg>)` will now install the latest-available version of
  that package from local sources, if that package is available and newer than
  any package available on the active package repositories. (#591)

* The configuration option `startup.quiet` has been added, allowing one to
  control whether `renv` will display the typical startup banner when a
  project is loaded.
  
* `renv` now better handles being unloaded and reloaded within the
  same R session. In particular, warnings related to a corrupted
  lazy-load database should no longer occur. (#600)

* `renv` no longer re-installs packages that are already installed and
  up-to-date in bare calls to `renv::install()`.

* `renv` now uses the R temporary directory for staging, when performing
  transactional restores / installs. If you need to control the path used
  for staged installs, please set the `RENV_PATHS_LIBRARY_STAGING` environment
  variable.

* The `install.verbose` configuration option has been added. When set to
  `TRUE`, `renv` will stream the output generated by R when performing a
  package installation. This can be helpful in some cases when diagnosing
  a failed restore / install. (#330)
  
* Fixed an issue where `renv` could fail to parse R Markdown chunk headers
  with an empty label. (#598)

* The environment variable `RENV_PATHS_LIBRARY_ROOT_ASIS` can now be used
  to control whether the project name should be used as-is when forming the
  library path within the `RENV_PATHS_LIBRARY_ROOT` folder. Set this to
  `"TRUE"` if you would prefer `renv` did not append a unique identifier
  to your project's library path. (#593)
  
* Fixed an issue where GitLab references were not URL encoded. (#590)

* `renv` no longer emits warnings when parsing multi-mode R files that make
  use of re-used knitr chunks (those specified as `<<label>>`). (#586)

* The library used for staged installs can now be configured via the
  `RENV_PATHS_LIBRARY_STAGING` environment variable. (#584)
  
* Fixed an issue where bootstrapping an older version of `renv` could
  fail if the R repositories had not been appropriately set.

# renv 0.12.3

* Fixed an issue where `renv::dependencies()` could give an error if called
  with a `path` argument of length > 1.

* `renv::restore()` gains the `rebuild` argument, allowing users to control
  whether packages should be rebuilt on `restore()` rather than installed
  via links or copies from the cache, or other sources providing
  already-installed packages.

* `renv` will now attempt to bootstrap itself from CRAN, in addition to any
  repositories declared via `getOption("repos")`. If you'd prefer to disable
  this behavior, you can set `options(renv.bootstrap.repos = character())`.

* The `renv` setting `r.version` has been added. This can be set if you'd like
  to associate a particular project with a specific version of R, independent
  of the version of R actually used when subsequent lockfiles are created via
  `renv::snapshot()`. For example, setting `renv::settings$r.version("4.0")`
  will ensure that R version `"4.0"` is encoded in the lockfile for future
  calls to `renv::snapshot()` in a project. (#254)

* `renv::dependencies()` now detects the usage of R packages within dotfiles;
  e.g. the project `.Rprofile`. (#569)

* `renv::status()` gains the `cache` argument, used to control whether
  `renv::status()` also performs diagnostics on the global package cache. (#570)

* Fixed an issue where `renv::status()` would make an un-necessary call to
  `renv::dependencies()`. (#570)

* Fixed an issue where `renv::install("bioc::<package>", rebuild = TRUE)` would
  fail to install the requested package. (#565)

* Fixed an issue where the repository name for a package installed from
  an R package repository was sometimes incorrect. (#402)

* When `RENV_PATHS_LIBRARY_ROOT` is set, `renv` will now disambiguate library
  paths based on a hash of the project's path. (#564)

# renv 0.12.2

* `renv` no longer errs when running tests with `_R_CHECK_SUGGESTS_ONLY_=false`.

# renv 0.12.1

* `renv` now ensures all of its dependencies are loaded eagerly when running
  tests, to avoid issues with missing + lazily-loaded packages.

* `renv::snapshot()` now accepts library paths specified with a relative
  path. (#562)

* `renv::snapshot()` no longer excludes the project itself, for `R` package
  projects that use [golem](https://engineering-shiny.org/). (#538)
  
* The `renv` configuration option `cache.symlinks` can now be used to control
  whether `renv` used symlinks into the cache, as opposed to full package
  copies. Please see `?renv::config` for more details. (#556)

* `renv::snapshot()` gains the `packages` argument, to be used when creating a
  lockfile that captures a specific set of packages and their dependencies.
  `renv` will use the currently-installed versions of those packages when
  determining the package records to be written to the lockfile. (#554)
  
* `renv::dependencies()` now accepts an R function as the first argument,
  for finding the packages used by a particular function. Currently,
  package usages must be prefixed with `::` to be detected. (#554)

* `renv::record(<package>)` now ensures that the latest-available version of
  that package is recorded in the lockfile. Previously, a package record
  without any specified version was added instead. For existing records
  without a recorded version, the latest-available version on the package
  repositories will be used during `restore()` instead. (#540)

* `renv` now reads the default branch tagged for repositories created on GitHub,
  ensuring that calls of the form `renv::install("<user>/<repo>")` resolve to
  the declared default branch, rather than always defaulting to `"master"`.
  (#557)

* `renv` now only installs packages from sources if it detects that build tools
  are available. This determination is done by checking whether `make` is
  available on the `PATH`. (#552)
  
* Warnings related to unknown sources can now be suppressed by setting
  `options(renv.warnings.unknown_sources = FALSE)`. (#546)

* `renv` now ignores chunks with the parameter `exercise=TRUE` set, under the
  assumption that such chunks might contain errors and so otherwise be
  un-parsable.

* `renv` now warns if sandbox generation takes a long time (> 30 seconds).

* `renv` now provides an optional locking mechanism, to help minimize the
  chance of interprocess conflicts when multiple R processes need to use the
  same `renv` project. The option is currently disabled by default; it can be
  enabled by setting `options(renv.config.locking.enabled = TRUE)` in an
  appropriate R startup file. (#525)

# renv 0.12.0

* `renv` now uses R's internal tar implementation by default on Windows. This is
  done to avoid issues that may occur when a version of `tar.exe` on the `PATH`
  exists, but does not accept Windows-style paths. The `TAR` environment
  variable can be set if one needs to explicitly force the use of a particular
  `tar.exe` executable. (#521)

* `renv` now prepends `renv (<version>)` to the user agent string. This should
  help ensure that package binaries are located when installing packages from
  RSPM outside of RStudio. (#520)

* `renv` now uses a task callback to detect mutations to the project library
  when the `auto.snapshot` configuration option is enabled. This will help
  ensure that automatic snapshots occur when packages are installed via a
  mechanism not explicitly understood by `renv`. (#501)

* `renv` now treats the user + site libraries as package sources during a
  restore. If `renv` sees that a package already installed in one of these
  libraries is compatible with the record requested via `renv::install()` or
  `renv::restore()`, that copy of the package will be copied and used. (#492)
  
* `renv` now performs a lighter-weight check as to whether the project lockfile
  is synchronized with the project library on load. The default value for the
  `synchronized.check` config option has been changed back to `TRUE`. (#496)

* `renv` now handles the `remotes` syntax for installing packages lying within
  the sub-directory of a GitHub repository; that is,
  `renv::install("user/repo/subdir")` should work as expected. (#497)

* Fixed an issue where `renv` did not construct the correct URL for packages to
  be installed from Bitbucket remotes. (#494)

* Fixed an issue where the `RENV_PATHS_PREFIX` environment variable was
  inappropriately normalized when `renv` was loaded. (#465)
  
# renv 0.11.0

* Fixed an issue where `renv::install(..., type = "binary")` would
  still attempt to install packages from sources in some cases. (#461)
  
* `renv` now always writes `renv/.gitignore`, to ensure that the appropriate
  directories are ignored for projects which initialize `git` after `renv`
  itself is initialized. (#462)

* R Markdown documents with the `.Rmarkdown` file extension are now parsed for
  dependencies.

* Fixed an issue where setting the `external.libraries` configuration option
  would trigger a warning. (#452)

* Improved handling of unicode paths on Windows. (#451)

* `renv::snapshot(project = <path>)` now properly respects `.gitignore` /
  `.renvignore` files, even when that project has not yet been explicitly
  initialized yet. (#439)
  
* The default value of the `synchronized.check` option has been changed from
  TRUE to FALSE.

* Fixed an issue where packages downloaded from Bitbucket and GitLab did not
  record the associated commit hash.

* Fixed an issue where attempting to install packages from GitLab could fail
  to install the correct version of the package. (#436)

* `renv::snapshot()` now preserves records in a lockfile that are only
  available for a different operating system. This should make it easier
  to share lockfiles that make use of platform-specific packages. (#419)

* `renv` better handles files that are removed during an invocation to
  `renv::dependencies()`. (#429)

* The configuration option `install.staged` has been renamed to
  `install.transactional`, to better reflect its purpose. `install.staged`
  remains supported as a deprecated alias.

* Fixed an issue where `renv` could fail to parse non-ASCII content on Windows.
  (#421)

* `renv::update()` gains the `exclude` argument, useful in cases where one
  would like to update all packages used in a project, except for a small
  subset of excluded packages. (#425)

* `renv::update()` now respects the project `ignored.packages` setting. (#425)

* Fixed an issue where RSPM binary URL transformations could fail for
  Ubuntu Trusty. (#423)

* `renv` now records the `OS_type` reported in a package's `DESCRIPTION` file
  (if any), and ignores packages incompatible with the current operating
  system during restore. (#394)

# renv 0.10.0

* `renv::install()` gains the `type` argument, used to control whether `renv`
  should attempt to install packages from sources (`"source"`) or using
  binaries (`"binary"`).

* `renv` now knows how to find and activate Rtools40, for R 4.0.0 installations
  on Windows.

* The `RENV_PATHS_PREFIX` environment variable can now be used to prepend an
  optional path component to the project library and global cache paths.
  This is primarily useful for users who want to share the `renv` cache across
  multiple operating systems on Linux, but need to disambigutate these paths
  according to the operating system in use. See `?renv::paths` for more details.
  
* Fixed an issue where `renv::install()` could fail for packages from GitHub
  whose DESCRIPTION files contained Windows-style line endings. (#408)

* `renv::update()` now also checks and updates any Bioconductor packages
  used within a project. (#392)

* `renv` now properly parses negated entries within a `.gitignore`; e.g.
  `!script.R` will indicate that `renv` should include `script.R` when
  parsing dependencies. (#403)

* Fixed an issue where packages which had only binaries available on a
  package repository were not detected as being from a package repository.
  (#402)

* Fixed an issue where calls of the form `p_load(char = <vctr>)` caused a
  failure when enumerating dependencies. (#401)

* Fixed an issue where `renv::install()` could fail when multiple versions
  of a package are available from a single repository, but some versions of
  those packages are incompatible with the current version of R. (#252)

* Fixed an issue where downloads could fail when the associated pre-flight
  HEAD request failed as well. (#390)

* Fixed an issue where empty records within a DESCRIPTION file could cause
  `renv::dependencies()` to fail. (#382)

* renv will now download binaries of older packages from MRAN when possible.

* renv will now attempt to re-generate the system library sandbox if it is
  deleted while a session is active. (#361)

* Fixed an issue where Python packages referenced using `reticulate::import()`
  were incorrectly tagged as R package dependencies. Similarly, `renv` now only
  considers calls to `modules::import()` if those calls occur within a call to
  `modules::module()`. (#359)

* `renv::scaffold()` now also generates a lockfile when invoked. (#351)

* The argument `confirm` has been renamed to `prompt` in all places where it
  is used. `confirm` remains supported for backwards compatibility, but is no
  longer explicitly documented. (#347)

* The continuous integration `renv` vignette now also contains a template for
  using `renv` together with GitLab CI. (#348, @artemklevtsov)

* `renv` now properly resets the session library paths when calling
  `renv::deactivate()` from within RStudio. (#219)
  
* `renv::init()` now restores the associated project library when called in a
  project containing a lockfile but no project library nor any pre-existing
  project infrastructure.

* Fixed an issue on Windows where attempts to download packages from package
  repositories referenced with a `file://` scheme could fail.
  
* The configuration option `dependency.errors` has been added, controlling how
  errors are handled during dependency enumeration. This is used, for
  example, when enumerating dependencies during a call to `renv::snapshot()`.
  By default, errors are reported, and (for interactive sessions) the user is
  prompted to continue. (#342)
  
* `renv::dependencies()` gains two new arguments: the `progress` argument
  controls whether `renv` reports progress while enumerating dependencies,
  and `errors` controls how `renv` handles and reports errors encountered
  during dependency discovery. The `quiet` argument is now soft-deprecated,
  but continues to be supported for backwards compatibility. Specifying
  `quiet = TRUE` is equivalent to specifying `progress = FALSE` and
  `errors = "ignored"`. Please see the documentation in `?dependencies`
  for more details. (#342)
  
* The environment variable `RENV_PATHS_LIBRARY_ROOT` can now be set, to
  instruct `renv` to use a particular directory as a host for any project
  libraries that are used by `renv`. This can be useful for certain cases
  where it is cumbersome to include the project library within the project
  itself; for example, when developing an R package. (#345)

* The code used to bootstrap `renv` (that is, the code used to install `renv`
  into a project) has been overhauled. (#344)

* `renv` no longer unsets an error handler set within the user profile when
  loading a project. (#343)

* `renv` gains the "explicit" snapshot type, wherein only packages explicitly
  listed as dependencies within the project `DESCRIPTION` file (and those
  package's transitive dependencies) will enter the lockfile when
  `renv::snapshot()` is called. (#338)

* `renv` will now transform RSPM source URLs into binary URLs as appropriate,
  allowing `renv` to use RSPM's binary repositories during restore. See
  `?config` for more details. (#124)

* `renv` will now infer a dependency on `hexbin` in projects that make
  use of the `ggplot2::geom_hex()` function.

* `renv` now tries to place Rtools on the PATH when a package is installed
  with the `install.packages()` hook active. (#335)

# renv 0.9.3

* Fixed an issue where attempts to specify `RENV_PATHS_RTOOLS` would
  be ignored by `renv`. (#335)

* Fixed an issue where downloads could fail when using the `wininet`
  downloader, typically with a message of the form
  "InternetOpenUrl failed: 'The requested header was not found'".

* `renv` better handles projects containing special characters on Windows.
  (#334)

* `renv` better handles unnamed repositories. (#333)

* `renv` gains the config option `hydrate.libpaths`, allowing one to control
  the library paths used by default for `renv::hydrate()`. (#329)

* `renv::hydrate()` gains the `sources` argument, used to control the library
  paths used by `renv` when hydrating a project. (#329)
  
* `renv` now sandboxes the system library by default on Windows.

* `renv` now validates that the Xcode license has been accepted before
  attempting to install R packages from sources. (#296)
  
* The R option `renv.download.override` can now be used to override the
  machinery used by `renv` when downloading files. For example, setting
  `options(renv.download.override = utils::download.file)` would instruct
  `renv` to use R's own downloader when downloading files from the internet.
  This can be useful when configuration of `curl` is challenging or
  intractable in your environment, or you've already configured the base
  R downloader suitably.

* `renv::use_python("~/path/to/python")` now works as expected.

* `renv` now properly expands `R_LIBS_SITE` and `R_LIBS_USER` when set within a
  startup `.Renviron` file. (#318)

* The `renv.download.headers` option can now be used to provide arbitrary HTTP
  headers when downloading files. See the **Authentication** section in
  `vignette("renv")` for more details. (#307)

* `renv` gains the project setting `package.dependency.fields`, for controlling
  which fields in an R package's `DESCRIPTION` file are examined when
  discovering recursive package dependencies. This can be useful when you'd like
  to instruct `renv` to track, for example, the `Suggests` dependencies of the
  packages used in your project. (#315)

* `renv` now better handles repositories referenced using file URIs.

* Packages installed from GitHub using `renv::install()` will now also have
  `Github*` fields added, in addition to the default `Remote*` fields. This
  should help fix issues when attempting to deploy projects to RStudio Connect
  requiring packages installed by `renv`. (#397)
  
* `renv` now prefers using a RemoteType field (if any) when attempting to
  determine a package's source. (#306)

* `renv` gains a new function `renv::scaffold()`, for generating `renv` project
  infrastructure without explicitly loading the project. (#303)

* `renv` now updates its local `.gitignore` file, when part of a git repository
  whose git root lives in a parent directory. (#300)

# renv 0.9.2

* Fixed an issue in invoking `find` on Solaris.

# renv 0.9.1

* Fixed an issue in invoking `cp` on Solaris.

# renv 0.9.0

* `renv` gains a new function `renv::record()`, for recording new packages
  within an existing lockfile. This can be useful when one or more of the
  recorded packages need to be modified for some reason.

* An empty `.renvignore` no longer erroneously ignores all files within a
  directory. (#286)

* `renv` now warns if the version of `renv` loaded within a project does not
  match the version declared within the `renv` autoloader. (#285)

* `renv` gains a new function `renv::run()`, for running R scripts within
  a particular project's context inside an R subprocess. (#126)

* The algorithm used by `renv` for hashing packages has changed. Consider 
  using `renv::rehash()` to migrate packages from the old `renv` cache to
  the new `renv` cache.

* `renv::status()` now reports packages which are referenced in your project
  code, but are not currently installed. (#271)

* `renv` is now able to restore packages with a recorded URL remote. (#272)

* `renv::dependencies()` can now parse R package dependencies used as custom
  site generator in an Rmd yaml header. (#269, @cderv)

* `renv` now properly respects a downloader requested by the environment
  variable `RENV_DOWNLOAD_FILE_METHOD`.

* `renv` no longer sources the user profile (normally located at `~/.Rprofile`)
  by default. If you desire this behavior, you can opt-in by setting
  `RENV_CONFIG_USER_PROFILE = TRUE`; e.g. within your project or user
  `.Renviron` file. (#261)

* `renv::restore()` gains the `packages` argument, to be used to restore
  a subset of packages recorded within the lockfile. (#260)

* `renv` now tries harder to preserve the existing structure in infrastructure
  files (e.g. the project `.Rprofile`) that it modifies. (#259)

* `renv` now warns if any Bioconductor packages used in the project appear
  to be from a different Bioconductor release than the one currently active
  and stored in the lockfile. (#244)

* `renv` now normalizes any paths set in the `RENV_PATHS_*` family of
  environment variables when `renv` is loaded.

* Fixed an issue where `renv` would not properly clean up after a failed
  attempt to call `Sys.junction()`. (#251)

* Fixed an issue where `renv` would, in some cases, copy rather than link from
  the package cache when the library path had been customized with the
  `RENV_PATHS_LIBRARY` environment variable. (#245)

* The method `renv` uses when copying directories can now be customized. When
  copying directories, `renv` now by default uses `robocopy` on Windows, and
  `cp` on Unix. This should improve robustness when attempting to copy files
  in some contexts; e.g. when copying across network shares.

* `renv` now tracks the version of Bioconductor used within a project
  (if applicable), and uses that when retrieving the set of repositories
  to be used during `renv::restore()`.

* `renv::dependencies()` can now parse R package dependencies declared and
  used by the `modules` package. (#238, @labriola)

* Fixed an issue where `renv::restore()` could fail in Docker environments,
  usually with an error message like 'Invalid cross-device link'. (#243)

* `renv::install()` disables staged package install when running with the
  Windows Subsystem for Linux. (#239)

# renv 0.8.3

* `renv::dependencies()` gains a new argument `dev`, indicating whether
  development dependencies should also be included in the set of discovered
  package dependencies. By default, only runtime dependencies will be reported.

* `renv` has gained the function `renv::diagnostics()`, which can occasionally
  be useful in understanding and diagnosing `renv` (mis)behaviors.

* `renv::equip()` can now be used on macOS to install the R LLVM toolchain
  normally used when compiling packages from source. `renv` will also use
  this toolchain as appropriate when building packages from source.

* `renv::install()` now provides a custom Makevars when building packages on
  macOS with Apple Clang, to avoid issues due to the use of '-fopenmp' during
  compilation.

* `renv::install()` now respects explicit version requests when discovered
  in a project's DESCRIPTION file. (#233)

* Fixed an issue where `renv:::actions()` would fail to report any actions if
  the project lockfile was empty. (#232)

* When using `renv` for R package development, `renv` will no longer attempt to
  write the package being developed to the lockfile. (#231)

* Fixes for checks run on CRAN.

* renv will now search for Rtools in more locations. (#225)

* `renv::load()` now ensures that the version of `renv` associated with
  the loaded project is loaded when possible. In addition, experimental
  support for switching between projects with `renv::load()` has been
  implemented. (#229)

* `renv::dependencies()` no longer treats folders named with the extension
  `.Rmd` as though they were regular files. (#228)

* It is now possible to install source packages contained within `.zip`
  archives using `renv::install()`.

* Fixed an issue where attempts to call `renv::restore()` with the path to the
  lockfile explicitly provided would fail. (#227)

# renv 0.8.2

* Further fixes for checks run on CRAN.

# renv 0.8.1

* Fixes for checks run on CRAN.

# renv 0.8.0

* Initial CRAN release.
