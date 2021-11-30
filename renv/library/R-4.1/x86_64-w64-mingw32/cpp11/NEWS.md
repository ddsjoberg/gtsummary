# cpp11 0.4.0

## New Features

* New opt-in message formatting with the {fmt} C++ library for `cpp11::messages()` `cpp11::stop()` and `cpp11::warning()`. 
  Set the `CPP11_USE_FMT` macro to use this feature in your package. (@sbearrows, #169, #208)
* New `as_double()` and `as_integer()` methods to coerce integers to doubles and doubles to integers to doubles (@sbearrows, #46)
* `cpp11::matrix` iterators can now be used either row-wise or column-wise (the default) depending on the user's choice (@alyst, #229)

## Improvements and fixes

* Read-only matrix accessors are now marked const (#234)
* `writable::r_vector` default constructors now return a 0 length vector when converted to `SEXP` (#166)
* Read-only `r_vector` constructors now disallow implicit construction with named arguments (#237)
* Read-only `r_vector.attr()` methods now return const objects, so it is a compile time error to try to assign to them (#237)
* Fixed `+` and `+=` operators of `r_vector::[const_]iterator` to conform the *iterators* concept:
  `+=` updates the iterator, and `+` returns the updated copy, while keeping the original unchanged (@alyst, #231)
* Remove undefined behavior when constructing global `cpp11::sexp`s (#224)
* Removed redundant `.Call calls` in cpp11.cpp file (@sbearrows, #170)
* Error messages now output original file name rather than the temporary file name (@sbearrows, #194)
* `cpp_register()` now includes `attribute_visible` in the init function, so packages compiled with `C_VISIBILITY` will find the init function.
* Fixed bug when running `cpp_source()` on the same file more than once (@sbearrows, #202)
* Allow cpp11 decorators of the form `cpp11::linking_to` (@sbearrows, #193)
* Removed internal instances of `cpp11::stop()` and replaced with C++ exceptions (@sbearrows, #203)
* Names of named lists are now resized along with the list elements (@sbearrows, #206)

# cpp11 0.3.1

* Fix stringop-truncation warning from generated wrapping code.

# cpp11 0.3.0


## New functions and features
* New `x.empty()` method to check if a vector is empty (@sbearrows, #182)
* New `x.named()` method to check if a vector is named (@sbearrows, #186)
* New `na()` free function to return the NA sentinels for R objects (@sbearrows, #179)

## Major fixes
* Memory no longer inadvertently leaks when move constructing vectors (#173)

## Minor improvements and fixes
* Incorrectly formatted cpp11 decorators now output a more informative error message (@sbearrows, #127)
* Generated registration code now uses C collation to avoid spurious changes from `tools::package_native_routine_registration_skeleton()` (@sbearrows, #171)
* Makevars files which include filenames now handle spaces in paths properly (@klmr, #160)

# cpp11 0.2.7

* Fix a transient memory leak for functions that return values from `cpp11::unwind_protect()` and `cpp11::safe` (#154)
* `cpp_source()` now gets an argument `dir` to allow customized temporary directory to store generated source files.
  It makes it easier to debug C++ source files in non-package project via source mapping. (@renkun-ken, #156)

# cpp11 0.2.6

* `cpp_register()` now uses symbols exclusively in the `.Call()` interface. This allows it to be more robust in interactive use with the pkgload package.

# cpp11 0.2.5

* `cpp_source()` gains a `cxx_std` argument to control which C++ standard is used.
  This allows you to use code from `C++14` and later standards with cpp_source(). (#100)
* The cpp11 knitr engine now allows you to set the `cxx_std` chunk option to control the C++ standard used.
* `cpp_source()` now has much more informative error messages when compilation fails (#125, #139)
* `cpp_source()` now uses a unique name for the DLL, so works when run multiple times on the same source file on Windows (#143)
* `writable::list_of<T>` now supports modification of vectors as intended (#131).
* Errors when running `tools::package_native_routine_registration_skeleton()` are no longer swallowed (#134)
* `cpp_source()` can now accept a source file called `cpp11.cpp` (#133)
* `named_arg` now explicitly protect their values, avoiding protection issues when using large inputs. [tidyverse/readr#1145](https://github.com/tidyverse/readr/issues/1145)
* `r_string(std::string)` now uses `Rf_mkCharLenCE()` instead of `Rf_mkChar()`, which avoids the performance cost of checking the string length.
* Writable vector classes now properly set their lengths as intended when being copied to a read only class (#128).

# cpp11 0.2.4

* The preserve list is now more robust to invalid values, such as when the XPtr has no address or if non-xptr's are stored in the option. This fixes errors when reloading packages using cpp11 and RStudio's session restores.
* The preserve list is now more robust to invalid values, such as null pointers when the XPtr is serialized. This situation occurs during 'Install and Restart' in RStudio (#121)

# cpp11 0.2.3

* `r_vector::const_iterator::operator*` is now a const method (#113, @bkietz, @xhochy)
* The preserve list is now stored in an XPtr, rather than an environment, to avoid issues when serializing the preserve environment, which happens implicitly when RStudio or RStudio Cloud saves all options when resuming a session (#116)

# cpp11 0.2.2

* `r_bool` added as an adapter between `bool` and `Rboolean` values (#57, @bkietz)

* `data_frame()` objects now have the number of rows correctly set as real length, not the reserved length (#91)

* Fixed potential memory leak in cpp11::writable classes.

# cpp11 0.2.1

* Ensures backwards compatibility with code generation from cpp11 0.1.0 (#88)

* `push_back()` now works more consistently with named arguments (#86)

# cpp11 0.2.0

## New features

* cpp11 is now able to compile on gcc 4.8.5 (#69, @bkietz)

* `cpp_source()`, `cpp_function()` and `cpp_eval()` now support `[[cpp11::linking_to()]]` syntax to link to third party packages with C++ headers. (#48)

## Minor improvements and fixes

* `as_cpp<E>()` now works with enumeration types (#52, @bkietz)

* `as_cpp<int>()` and `as_cpp<double>()` now implicitly coerce between all 3 types of single NA values (#53).

* `list::const_iterator::operator*()` added so iterators could be used on list objects (#60, @romainfrancois)

* `safe[]` can now work with functions that return any type (#70, @bkietz)

* The `END_CPP` macro now includes a `catch(...)` block to catch all C++ exceptions that do not inherit from `std::exception` (#47).

* Improve consistency of inserting NA values in r_string objects (#45)

* Added a `NEWS.md` file to track changes to the package.

# cpp11 0.1.0

* Initial release
