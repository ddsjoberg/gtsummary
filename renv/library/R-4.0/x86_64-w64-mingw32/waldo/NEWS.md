# waldo 0.2.5

* On platforms without UTF-8 support, strings that differ only in their
  encoding are now correctly considered to be identical (#66).

# waldo 0.2.4

* Additional arguments to `compare()` generate a more informative warning 
  (#58).

* Numbers use a better algorithm for picking the number of decimal places to  
  show (#63).

* ASTs with identical deparsed strings now show exactly how the AST differs.
  Source references are now more comprehensively stripped using
  `rlang::zap_srcrefs()`

* S3 objects now show the base type, and no longer fails when the types are
  incompatible.

# waldo 0.2.3

* `compare()` gains a new `max_diffs` argument that allows you to control
  the maximum number of differences shown. Set `max_diffs = Inf` to 
  see all differences (#49)

* Logical vectors fall back to element-by-element comparison in more cases 
  (#51).

* Long-form diff no longer confuses additions and deletions (#52, @krlmlr).

# waldo 0.2.2

* Handle S4 objects that have attributes that are not slots.

* Additions are now coloured blue and deletions yellow (instead of the 
  opposite).

# waldo 0.2.1

* `compare()` now labels output as `old` and `new`, since that's the most
  natural way to use it.

* `compare()` can selectively ignore attributes by providing vector to 
  `ignore_attr` (#45).

* `print()` method gets `n` argument to allow explicitly specifying number of 
  differences to show (@mnazarov).

* Improvements to comparison display:

    * Zero length vectors compare robustly (#39)

    * Line-by-line comparisons show modifications as deletion then addition, 
      rather than addition then deletion (#44).

    * Differences between numeric vectors are more robust, particularly in the
      presence of missing values (#43). The number of digits selected has also 
      been slightly improved so that you're more likely to get exactly the 
      number of digits needed.

# waldo 0.2.0

* All objects: class (#26) and names (#31) are ignored when ignoring attributes.

* Numeric and logical vectors: clearer display of differences. Numbers 
  are right-aligned, and we show the numbers not the differences.
  
* Character vectors: a trailing newline is no longer ignored (#37).

* Lists: all elements of the unnamed lists are compared, not just the last! (#32)

* Lists: unclassed prior to comparison (#21).

* Data frames: The internal representation of row names is no longer used; 
  instead we use the same result of `rownames()` (#23).

* Environments: New `ignore_formula_env` and `ignore_function_env` arguments to 
  ignore formula and function environments for compatibility with `all.equal()`
  (#24).

* Expression objects: can now be compared (#29).

* Calls: srcrefs and attributes are ignored.

---

* `compare_proxy()` is now exported so that you can provide methods if your
  objects need special handling (particularly needed for objects that contain
  external pointers) (#22).

* Fixed a partial argument name in `as.list()`.

# waldo 0.1.0

* Added a `NEWS.md` file to track changes to the package.
