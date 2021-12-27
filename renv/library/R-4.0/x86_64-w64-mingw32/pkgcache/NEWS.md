
# pkgcache 1.2.1

No user visible changes.

# pkgcache 1.2.0

* New `repo_add()`, `repo_get()`, `repo_resolve()` and `with_repo()`
  functions to query and manipulate repositories.

* `meta_cache_*()` functions now handle `getOption("repos")` changes
  correctly.

* Failed metadata downloads now do not trigger metadata updates (#52).

* New `bioc_release_version()`, `bioc_devel_version()`, `bioc_repos()`
  helper functions to deal with Bioconductor repositories.

* Metadata cache functions, e.g. `meta_cache_deps()` etc. now allow
  specifying the dependency types in all lowercase (#54).

# pkgcache 1.1.1

* `package_cache` now does not fail if the web server does not send an
  `Etag` header when downloading packages.

* `package_cache` has now much relaxed HTTP timeouts, and handles
  downloading many packages (slowly) much better.

* The package download progress bar can now be supressed by setting
  the `pkg.show_progress` option to `FALSE`.

# pkgcache 1.1.0

* New `repo_status()` function to query the status and response time
  of CRAN-like repositories.

* New `bioc_version()` and `bioc_version_map()` functions to query
  Bioconductor repositories.

* pkgcache now does not fail if some repositories do not provide
  some package types.

* New `current_r_platform()`, `default_cran_mirror()` and
  `default_platforms()` functions.

* pkgcache now works for R 4.0.x macOS binaries.

# pkgcache 1.0.7

* Metadata is now cached in RDS version 2 formats, so metadata written
  by newer R version can be used by older R versions as well (#36).

# pkgcache 1.0.6

* HTTP timeouts are now much better, and by default they are defined
  in terms of download speed, instead of total download time (#29).

* pkgcache now tries to download metadata from the `PACKAGES` file, if it
  cannot find `PACKAGES.gz` (@timmsm, #27).

* pkgcache is now less verbose when updating or loading metadata.

# pkgcache 1.0.5

* Fix a bug in the download functions, that broke pak downloads.

# pkgcache 1.0.4

* Fix handling of Bioconducor versions and repositories, see
  README for the details.

* Now different versions of pkgcache, that potentially have different
  metadata format, can share the same metadata cache directory.

# pkgcache 1.0.3

* Fix concurrency issues when the async API is used multiple times in the
  same event loop.

* Make package compatible with tibble >= 2.0.0.

* Add `meta_cache_summary()` and a `summary()` method for
  `cranlike_metadata_cache`. Return information about a metadata cache
  instance.

# pkgcache 1.0.2

* First public release
