# remotes 2.4.0

* Re-license as MIT. (#551)

* skip tests for `download.file(method = "internal")`, on R > 4.1, since that method is now defunct on those versions.

* `system_requirements()` now works as intended if only the `os` argument is used (@mdneuzerling, #609)

* `remote_package_name.git2r_remote` and `remote_package_name.xgit_remote` now get correct package name from HTTP(S) git repo's `DESCRIPTION` file, and thus package's `DESCRIPTION` file's `Remotes` field could have `git::http(s)://<host>/<username>/<repo>[.git][@ref]` items that install remote packages using git via HTTP(S) protocal (@niheaven, #603).

# remotes 2.3.0

## Major changes

* `install_*()` functions will no longer fail by default if there warnings from `install.packages()`. Concretely the default value of `R_REMOTES_NO_ERRORS_FROM_WARNINGS` has changed to `true` from the previous value of `false`. (#403)

## Minor improvements and fixes

* `install_bioc()` now respects the environment variable R_BIOC_VERSION, and will use the git branch corresponding to this Bioconductor version (@bbimber, #580).

* remotes without package names are now unconditionally installed (#532, @jakubkovac)

* It is now possible to specify a custom host for dependencies listed in the `DESCRIPTION` file with `Remotes: <type>[@host]::<username>/<repo>[@ref]`. The `ref` now supports `/` in it for `GitLab` repositories as it did for `GitHub`repositories. (@dagola, #448)

* Internal `package2remote()` function now supports local remotes created by pak.

* `github_pat()` will now check if `GITHUB_TOKEN` is set if it cannot find `GITHUB_PAT`. (@coatless)

* `system_requirements()` now supports querying released packages as well as development dependencies (#545)

* `system_requirements()` now supports OS name + version in the `os` argument (#549, @krlmlr).

# remotes 2.2.0

##  New functions and features

* New `system_requirements()` function to query the Public RStudio Package Manager for system requirements for a package (and its dependencies)

* Remotes functions can now install dependencies from additional DESCRIPTION fields, e.g. passing `dependencies = "Config/Needs/website"` will install the dependencies listed in the `Config/Needs/website: ` field in the package's DESCRIPTION.
  Prefixing fields with `Config/Needs` allows them to pass `R CMD check` without a NOTE, so it is the recommended format for these extra dependencies.

* `install_*()` family of functions now use the default branch in the repository, not the `master` branch (@MyKo101,#508).

## Minor improvements and fixes

* Internal functions `remote_download()`, `remote_metadata()`, `remote_package_name()` and `remote_sha()` are now exported, so 3rd party packages could provide methods for new remote types (#509, #56)

* Internal functions `add_metadata()`, `github_remote()` are now exported. They are mainly for 3rd party extensions and should not be used by most users (#485).

* `install_version()` now keeps searching subsequent repositories for the requested version, rather than failing if the version it finds in an early repository is unsuitable. (#305, @kenahoo)

* `install_version()` now understands specifications like '>= 1.0' or '>= 1.12.0, < 1.14' to install the first version of the package it can find that satisfies the criteria. (#305, @kenahoo)

* `install_version()` now avoids use of `base::url()`, as prior to R 3.6.2 it had a bug when downloading large files (#463)

* `parse_submodules()` internal regular expression is now PCRE 2 compatible (#502, @jan-glx)

* `update_packages()` argument `force` has been deprecated and no longer has any effect (#521)

* Another fix for the mixed binary and source dependency issue, it should hopefully be fully squashed now (#296)

* The upgrade menu is now interruptible in RStudio (#489).

* Internal GitHub functions now correctly handle cases when characters are not representable in the default locale, but are representable in UTF-8 (#492).

# remotes 2.1.1

## Minor improvements and fixes

* Installing mixed binary and source dependencies when the latest versions of
  some packages do not have binaries yet should now install dependencies in the
  correct order to prevent load failures (#296)

* `github_error()` now also works when a GitHub (Enterprise) server does not
  return information about the rate limit (@dpprdan, #396, #413).

* `install_gitlab` passes the `quiet` argument on to `gitlab_pat` (@michaelchirico, #437)

* `remotes` is now resilient against installed packages that declare
  `RemoteType: standard` but do not include a `RemoteRepos` or `RemotePkgType`
  field. In such a case, the values for `getOption("repos")` and
  `getOption("pkgType")` will be used (respectively).

* `install_gitlab()` now installs from repositories in subgroups and with dots
  in their name. `subdir` is now an explicit argument instead of implicit in
  `repo` (@robertdj, #259, #420).

* `install()` now passes the ellipsis `...` to `install_deps()` (@Neil-Schneider, #411)

* The tests have been updated to work with newer versions of callr and R 4.0

# remotes 2.1.0

## New features

* `install_*()` functions gain `build_manual` and `build_vignette` arguments
  that previously existed in devtools versions < 2.0 (#353).

* The interactive menu has been modified to provide more clear instructions on
  the skipping behavior (#207)

* Credentials are now passed via HTTP headers, to reduce exposure when requests
  fail (#391).

## Minor improvements and fixes

* `download()` with the external curl download method now always uses `-L` to
  follow redirects. (#350)

* `update_packages()` now has a more informative error message when the update
  fails (#223, #232)

* `install_git()` now can take credentials from the global option
  `remotes.git_credentials` (#378).

* `install_git()` now works with SHA references and external git (#389).

* GitHub remotes that point to branches no longer fail when the branch is later
  deleted (#274).

* Local remotes whose original location has been moved will no longer error
  when updating (#370).

* `update_deps()` no longer sorts the dependencies alphabetically (#296, #301)

* `github_resolve_ref()` now takes a `host` parameter (#284)

* Remotes specific environment variables now accept 0 and 1 as valid values (#238)

* remotes now uses locking by default when installing binary packages, which avoids
  issues when installing binaries that are already open in other R processes
  (#368)

* `update_deps()` no longer fails if a local package no longer exists (#289)

* `install_version()` now errors with a more informative message when `type` is
  not 'source' (#323)

* Bioc `remote_sha()` now always returns a character result (#379)

* Fix API call for private repositories in `install_gitlab`
  (@aornugent, #359, #363)

* git submodules now work if the submodule file is empty (@muschellij2, #234)

* git submodules now work if the R package is stored in a subfolder (@pommedeterresautee, #233)

* `install_gitlab()` no longer adds the access token twice to the request
  (@aornugent, #363).

* Bitbucket dependencies now actually use the `BITBUCKET_USER` and
  `BITBUCKET_PASSWORD` environment variables (@antoine-sachet, #347).

* `parse_deps()` now ignores trailing whitespaces around comparison operators
  in DESCRIPTION fields (@LiNk-NY, #366)

# remotes 2.0.4

* `update.package_dependencies()` now uses the pkg_type for the cran remote
  rather than a global type attribute, fixing errors when this global attribute
  is lost (#291, #304).

* Credentials are no longer passed to dependencies, as this breaks dependencies
  which use different credentials or hosts. If you have relied on this behavior
  a more robust way to provide the credentials is using the appropriate
  environment variables, e.g. `GITHUB_PAT`, `BITBUCKET_USER` etc.
  (@antoine-sachet, #345).

* The hash of bitbucket hosts is now correctly retrieved (@antoine-sachet, #344)

* Fix parsing of Additional_Repositories which have a leading newline
  (@tmelliott, #251).

# remotes 2.0.3

* The order of choices for `upgrade = "ask"` now puts the stable ones 'All',
  'CRAN only', 'none' first, so they always have the same numbers (#287).

* `update_submodules()` now works with empty .gitmodules files (@jsilve24, #329).

* remotes now understands the "standard" remote type, as produced by packages
  installed from CRAN using `pak` (#309)

* `install_dev()` now supports ref/pull format, e.g.
  `install_dev('shiny@v1.2-rc')` (@mkearney, #279).

* Fix return type of `install_remote()` when there is a circular dependency (#225)

* `remote_package_name.github_remote()` now works properly on Windows (#248)

* `install_bioc()` repositories now updated for the Bioconductor 3.8 release.
  (#239)

* `install_*` functions now set the `R_LIBS*` environment variables for
  child processes correctly on Windows (@HenrikBengtsson, #253)

* `install_*` functions now support the `R_REMOTES_UPGRADE` environment
  variable, to set the default for the `upgrade` argument. See README for
  details (@kevinushey, #240).

* `install_*` functions perform basic HTTP authentication using HTTP
  headers now. This fixes an issue with `install_bitbucket()` and private
  repos (#255).

* `install_*` functions now respect the `download.file.method` option,
  if `download_file()` is used for HTTP.

* `install_*` functions now use the _libcurl_ method, if the
  `download.file.method` option is not set to a different one, and libcurl
  is available. Before, the _wininet_ method was preferred on Windows.
  If you rely on the proxy configuration of _wininet_, then you might
  want to set the `download.file.method` option, or use another way to
  set up proxies, see `?download.file`.
* Remotes without package names are now unconditionally installed (#246).

* `install_github()` now includes a more informative error message when the
  status code is 404, asking the user to check that they have spelled the
  repo owner and repo correctly (included in the error message), and that
  they have the required permissions to access the repository.

* `install_*` functions (via the underlying private `install` function) now set
  `RGL_USE_NULL="TRUE"` in order to avoid errors when running headless
  and installing any package using `rgl` (@jefferis, ##333)

# remotes 2.0.2

* `install_deps()` now installs un-installed remotes packages even when
  `upgrade = "never"` (@ankane, #227)

* `install_version()` now removes metadata added as a byproduct of using
  `install_url()` internally() (#224)

* `install()` now avoids converting warnings to errors if
  `R_REMOTES_NO_ERRORS_FROM_WARNINGS` is unset and
  `_R_CHECK_FORCE_SUGGESTS_=false`. This avoids failures due to Suggested
  packages potentially being missing.

* `install_bitbucket()` now works properly with packages in subdirectories
  (#220)

* `install_deps()` now installs un-installed packages even when `upgrade =
  "never"` (#218)

# remotes 2.0.1

* `install_github()` now bundles a GitHub PAT which is used on Travis to avoid
  hitting the rate limit too frequently. It also contains a more informative
  error message when the rate limit is hit on how to setup a GitHub personal
  access token.

* The dialog when `upgrade = "ask"` now has a 'CRAN only' option, to update
  only CRAN packages.

* No longer include project specific .Rprofile code in the temporary .Rprofile when
  `R_REMOTES_NO_ERRORS_FROM_WARNINGS=false` (the default).

* `update.package_deps()` no longer prompts to install uninstalled
  dependencies, they are always installed (#203).

* `available_packages()`, `available_packages_set()` and
  `available_packges_reset()` added to allow caching of the
  `available.packages()` database.

# remotes 2.0.0

## Breaking changes

* `install_github()`'s previously deprecated `username` argument has been
  removed. (#142)

* `install_deps()`'s `threads` argument has been removed, use the `Ncpus`
  argument instead (#153, #154)

* `install_git()`'s `branch` argument has been renamed to `ref` and `branch`
  has been deprecated.

## New features

* remotes now builds packages by default before installing them. This step
  uses the pkgbuild package, if available. If not, it calls `R CMD build`
  directly.

* New `install_dev()` to install the development version of a CRAN package,
  based on the URL and BugReports fields in the DESCRIPTION file (#144).

* `install_()*` functions now temporally put Rtools on the PATH when necessary,
  as long as the pkgbuild package is installed.

* remotes can be forced to use only its internal code by setting the
  environment variable `R_REMOTES_STANDALONE` = "true". This is useful when
  installing optional dependencies of remotes on Windows, such as curl or git2r
  (#147)

* When installing, remotes now errors on warnings, to catch cases
  where packages are only partially installed. This often happens on
  windows when the package dll is opened in another R process (#113).

* `install_()` functions now pass arguments, including authentication
  information and upgrade down to dependencies (#53, #86, #87).

* `install_()` functions allow the selection of a subset of packages to
  upgrade, in interactive mode, when `upgrade = "ask"`.

* `install_git()` now supports passing credentials, when it is used with
  `git = "git2r"` (#106)

* `install_()` functions now return the name of the package(s) which were
  installed (#55).

* git submodules are now installed if they exist and a git client is
  available (#138, #133, #103, #82).

* New `install_gitlab()` and `install_bioc()` functions, to install
  `gitlab` and  `bioc` remote types.

* remotes now uses the same SHA updating logic for remotes as devtools,
  including checking if the SHA of the remote has changed since the last
  installation. (#135)

* `install_url()` can now install package binaries on windows
  (r-lib/devtools#1765)

## Minor improvements and fixes

* `install_deps()` et al. now do not rewrite the `type` argument from `both`
  to `binary` to allow falling back to `source`. This fixes various
  installation failures.

* remotes now looks up GitHub package names locally, if possible, and
  uses the GitHub REST API (if the curl package is available, and not in
  standalone mode). This makes the remote lookup about 10x faster when the
  remote package has not changed since the last install.

* Using a GITHUB_PAT no longer prints diagnostic messages by
  default (r-lib/devtools#1752).

* remotes now always uses https URLs for R versions that support them
  (@ankane, #139)

* Do not include the BioCextra repository in versions after it was deprecated
  (R 3.5+, Bioc 3.6+).

* `install_()` functions now download tarballs (.tar.gz) files rather than zip
  archives (.zip). This results in generally smaller files and avoids issues
  with script permissions being lost and strange behavior of some external
  unzip programs on Windows (#96).

* Dependency parsing is now more robust to whitespace around the dependency
  specifications (#73).

* `standardise_dep()` exported, for use in devtools.

* `install_local()` now defaults to the current directory.

* `install_bitbucket()` now correctly supports authentication, and the
  `subdir` argument.

* `install_()` functions give a helpful warning when the package has long
  path names, on Windows. In this case building the package usually fails.
  (#84, #178).

* `install_()` functions have now a more robust way of handling various
  tar programs on Windows (#172).

* `install_()` functions now give a helpful warning on older R versions,
  on Windows, if `R.home()` contains a space character. Installation
  usually fails in this case.

* GitHub API errors now give better error messages, including data about
  the API rate limits.

# remotes 1.1.1

* Accept HTTPS, SSH, or various browser URLs in GitHub repo specification,
  @jennybc, #90, #109, #112

# remotes 1.1.0

* URL encode GitHub references, to allow installing from non-alphanumeric
  branch or tags, @krlmlr #38

* Better cooperation with proxy servers, and better download method
  selection on Windows, @cderv, #45, #46

* `install_deps()` supports tar balls, #47

* Allow training slash in GitHub repo specification, #54

* Work around on some Linux systems, where unzip is set to the empty
  string, @HenrikBengtsson, #57

* Check for circular dependencies while installing, #31

* Updated Bioconductor repo URLs for newer BioC versions

# remotes 1.0.0

First public release.
