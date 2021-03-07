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
