# 1.1.1

* `package_info()` and `session_info()` now detect locally installed packages 
  correctly if they have an empty `biocViews` field in `DESCRIPTION (@llrs, #25)

* `package_info()` and `session_info()` now handle the case when a loaded
  package was removed from the disk.

# 1.1.0

* `package_info()` now has a `dependencies` argument, to filter the type
  of dependent packages in the output (#22).

* `session_info()` and `package_info()` now show the library search path,
  and also which library each package was loaded from. They also warn
  if the on-disk version of the package has a different path than the
  loaded version (#9, #20).

* `package_info()`'s `ondiskversion` entry is now correct.

* `session_info()` and `package_info()` now verify the MD5 hashes of DLL
  files on Windows, and warns for micmatches, as these are usually
  broken packages (#12, #16).

* We use now the cli package, instead of clisymbols, and this fixes
  printing bugs in LaTeX documents (#14).

* `session_info()` and `platform_info()` now include the `LC_CTYPE`
  locale category (@patperry, #11)

* `session_info()` and `package_info()` now print source of the CRAN
  packages in uppercase, always, even if they were installed by devtools.

* `session_info()` and `platform_info()` now handle the case when
  `utils::sessionInfo()$running` is `NULL` (@HenrikBengtsson, #7).

* `session_info()` and `package_info()` now only list loaded versions
  for namespaces which are already loaded. This only makes a difference
  if the `pkgs` argument is given (#4).

* Do not consult the `max.print` option, for platform and package info
  (@jennybc, #13).

# 1.0.0

First public release.
