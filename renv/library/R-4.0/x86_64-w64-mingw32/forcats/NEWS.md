# forcats 0.5.1

* Re-license as MIT (#277).

* `fct_lump_n()` no longer uses a partial argument name (@malcolmbarrett, #276).

# forcats 0.5.0

* `as_factor()` gains a logical method that always returns a factor with
  levels "FALSE" and "TRUE" (#185).

* `fct_c()`, `fct_collapse()` and `fct_recode()` are now explicitly
  documented as using [dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html) 
  (@labouz, #234).

* `fct_collapse()` now accepts a `other_level` argument, to allow a 
   user-specified `Other` level (@gtm19, #194). It now correctly collapses 
   factors when `other_level` is not `NULL` (#172), and makes `"Other"` the 
   last level (#202) (@gtm19, #172 & #202)

* `fct_count()` no longer converts implicit NAs into explicit NAs (#151).

* `fct_inseq()` behaves more robustly when factor levels aren't all numbers
  (#221).

* `fct_lump()` has been split up into three new functions: `fct_lump_prop()`, 
  `fct_lump_n()`, and `fct_lump_lowfreq()`. (@jonocarroll, #167, #142). 
  All `fct_lump_()` functions check their inputs more carefully 
  (@robinson_es, #169)

* `fct_reorder2()` gains a helper function `first2()`, that sorts `.y` by the 
  first value of `.x` (@jtr13).
  
# forcats 0.4.0

## New features

* `fct_collapse()` gains a `group_other` argument to allow you to group all 
  un-named levels into `"Other"`. (#100, @AmeliaMN)

* `fct_cross()` creates a new factor containing the combined levels from two 
  or more input factors, similar to `base::interaction` (@tslumley, #136)

* `fct_inseq()` reorders labels in numeric order, if possible (#145, @kbodwin).

* `fct_lump_min()` preserves levels that appear at least `min` times (can also 
  be used with the `w` weighted argument) (@robinsones, #142).

* `fct_match()` performs validated matching, providing a safer alternative to
  `f %in% c("x", "y")` which silently returns `FALSE` if `"x"` or `"y"` 
  are not levels of `f` (e.g. because of a typo) (#126, @jonocarroll).

* `fct_relevel()` can now level factors using a function that is passed the
  current levels (#117).

* `as_factor()` now has a numeric method. By default, orders factors in numeric 
  order, unlike the other methods which default to order of appearance. 
  (#145, @kbodwin)

## Minor bug fixes and improvements

* `fct_count()` gains a parameter to also compute the proportion 
  (@zhiiiyang, #146). 

* `fct_lump()` now does not change the label if no lumping occurs 
  (@zhiiiyang, #130). 

* `fct_relabel()` now accepts character input.

* `fct_reorder()` and `fct_reorder2()` no longer require that the summary 
  function return a numeric vector of length 1; instead it can return any
  orderable vector of length 1 (#147).

* `fct_reorder()`, `fct_reorder2()` and `as_factor()` now use the ellipsis
  package to warn if you pass in named components to `...` (#174).

# forcats 0.3.0

## API changes

* `fct_c()` now requires explicit splicing with `!!!` if you have a
  list of factors that you want to combine. This is consistent with an emerging
  standards for handling `...` throughout the tidyverse.

* `fct_reorder()` and `fct_reorder2()` have renamed `fun` to `.fun` to
  avoid spurious matching of named arguments.

## New features

* All functions that take `...` use "tidy" dots: this means that you use can
  `!!!` to splice in a list of values, and trailing empty arguments are 
  automatically removed. Additionally, all other arguments gain a `.` prefix
  in order to avoid unhelpful matching of named arguments (#110).

* `fct_lump()` gains `w` argument (#70, @wilkox) to weight value
  frequencies before lumping them together (#68).

## Improvements to NA handling

* `as_factor()` and `fct_inorder()` accept NA levels (#98).

* `fct_explicit_na()` also replaces NAs encoded in levels.

* `fct_lump()` correctly accounts for `NA` values in input (#41)

* `lvls_revalue()` preserves NA levels.

## Minor improvements and bug fixes

* Test coverage increased from 80% to 99%.

* `fct_drop()` now preserves attributes (#83).

* `fct_expand()` and `lvls_expand()` now also take character vectors (#99).

* `fct_relabel()` now accepts objects coercible to functions 
  by `rlang::as_function` (#91, @alistaire47)

# forcats 0.2.0

## New functions

* `as_factor()` which works like `as.factor()` but orders levels by
  appearance to avoid differences between locales (#39).

* `fct_other()` makes it easier to convert selected levels to "other" (#40)

* `fct_relabel()` allows programmatic relabeling of levels (#50, @krlmlr).

## Minor improvements and bug fixes

* `fct_c()` can take either a list of factors or individual factors (#42).

* `fct_drop()` gains `only` argument to restrict which levels are dropped (#69)
  and no longer adds `NA` level if not present (#52).

* `fct_recode()` is now checks that each new value is of length 1 (#56).

* `fct_relevel()` gains `after` argument so you can also move levels
  to the end (or any other position you like) (#29).

* `lvls_reorder()`, `fct_inorder()`, and `fct_infreq()` gain an `ordered`
   argument, allowing you to override the existing "ordered" status (#54).

# forcats 0.1.1

* Minor fixes for R CMD check

* Add package docs
