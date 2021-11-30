# blob 1.2.1

- Inline prettyunits.
- `vec_ptype2.hms.default()` forwards to `vec_default_ptype2()` for compatibility with vctrs 0.2.1.


# blob 1.2.0

## Breaking changes

- The `blob` class is now based on `list_of(raw())` from the vctrs package (#11). This adds support for `vec_cast()` and `vec_ptype2()`. Some operations (such as subset assignment) are now stricter. The `new_blob()` constructor permits safe and fast construction of `blob` objects from a list, and `validate_blob()` checks an existing object for conformity with the rules.

- The new `is_blob()` deprecates the existing `is.blob()`. `as.blob()` is deprecated in favor of `vec_cast()` or the new `as_blob()` (which is just a thin wrapper around `vec_cast()`).

- Indexing a vector of blobs out of bounds now raises an error. Use `NA` as index to create a `NULL` blob.


# blob 1.1.1 (2018-03-24)

- Now suggesting *pillar* instead of importing *tibble*, and using colored
  formatting with the *prettyunits* package with `B` instead of `b` as units
  (#7, #9).

- The blob class can now be used for S4 dispatch.

- Calling `c()` on blob objects returns a blob.


# blob 1.1.0 (2017-06-17)

- New maintainer: Kirill Müller.

- Added `as.blob.blob()`and `as.data.frame.blob()` methods (#3).

- Size of very large blobs is displayed correctly.


# blob 1.0.0

- Initial release.
