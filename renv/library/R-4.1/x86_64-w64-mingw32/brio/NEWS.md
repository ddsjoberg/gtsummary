# brio 1.1.2

* Input filenames are now automatically converted to UTF-8 from the native encoding (@gaborcsardi, #15)

* `read_file_raw()` now closes file handles (@pbarber, #16)

# brio 1.1.1

* `file_line_endings()` now works as expected on ARM systems (#8)

# brio 1.1.0

* New `write_file()` function to write an entire file (#7)

* `read_lines()` no longer leaks file handles.

* Added a `NEWS.md` file to track changes to the package.

# brio 1.0.0

* Initial release
