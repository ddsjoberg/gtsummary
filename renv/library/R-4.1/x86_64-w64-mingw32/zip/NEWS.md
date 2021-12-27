# 2.2.0

* Header values (of version made by and external attributes) are now
  correctly read and written on big-endian systems (#68).

* `zip_list()` now also returns `crc32` and `offset` (#74, @jefferis).

# 2.1.1

This version has no user visible changes.

# 2.1.0

* `unzip_process()` now does not fail randomly on Windows (#60).

* Now all functions handle Unicode paths correctly, on Windows
  as well (#42, #53).

* `unzip_process()` now works when R library is on different drive
  than `exdir` on Windows (#45)

* zip functions now have a `mode` argument to choose how files and
  directories are assembled into the archive. See the docs for
  details.

* zip functions now have a `root` argument, zip changes the working
  directory to this before creating the archive, so all files are
  relative to `root`.

* `zip()` and `zip_append()` are not deprecated any more, as it was
  hard to achieve the same functionality with the other zip functions.

# 2.0.4

* `unzip_process()` prints better error messages to the standard error,
  and exits with a non-zero status, on error.

# 2.0.3

* `zipr()` and `zipr_append()` get an `include_directories = TRUE`
  argument, that can be used to omit directory entries from the zip
  archive. These entries may cause problems in MS Office docx files (#34).

# 2.0.2

* `zip_process()` and `unzip_process()` can now pass extra arguments to
  `processx::process` (#32).

* `unzip_process()` now makes sure the `exdir` path is created with
  forward slashes on Windows, mixing forward and backward slashes can
  cause errors.

# 2.0.1

* `zip()` and `zip_append()` are now soft-deprecated, please use
  `zipr()` and `zipr_append()` instead.

# 2.0.0

* New `zipr()` and `zipr_append()`, they always store relative file names
  in the archive.

* New `unzip()` function for uncompressing zip archives.

* New `zip_process()` and `unzip_process()` functions to create or
  uncompress an archive in a background process.

* `zip()`, `zipr()`, `zip_append()` and `zipr_append()` all include
  directories in the archives, empty ones as well.

* `zip()`, `zipr()`, `zip_append()` and `zipr_append()` all add time stamps
  to the archive and `zip_list()` returns then in the `timestamp` column.

* `zip()`, `zipr()`, `zip_append()` and `zipr_append()` all add file
  and directory permissions to the archive on Unix systems, and
  `zip_list()` returns them in the `permissions` column.

* `zip_list()` now correctly reports the size of large files in the archive.

* Use miniz 2.0.8 internally.

# 1.0.0

First public release.
