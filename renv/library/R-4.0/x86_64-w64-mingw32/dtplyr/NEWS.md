# dtplyr 1.1.0

## New features

* All verbs now have (very basic) documentation pointing back to the
  dplyr generic, and providing a (very rough) description of the translation
  accompanied with a few examples.

* Passing a data.table to a dplyr generic now converts it to a `lazy_dt()`,
  making it a little easier to move between data.table and dplyr syntax.

* dtplyr has been bought up to compatibility with dplyr 1.0.0. This includes
  new translations for:

  * `across()`, `if_any()`, `if_all()` (#154).

  * `count()` (#159).

  * `relocate()` (@smingerson, #162).

  * `rename_with()` (#160)

  * `slice_min()`, `slice_max()`, `slice_head()`, `slice_tail()`, and
    `slice_sample()` (#174).

  And `rename()` and `select()` now support dplyr 1.0.0 tidyselect syntax 
  (apart from predicate functions which can't easily work on lazily evaluated
  data tables).

* We have begun the process of add translations for tidyr verbs beginning
  with `pivot_wider()` (@markfairbanks, #189).

## Translation improvements

* `compute()` now creates an intermediate assignment within the translation. 
  This will generally have little impact on performance but it allows you to 
  use intermediate variables to simplify complex translations.

* `case_when()` is now translated to `fcase()` (#190).

* `cur_data()` (`.SD`), `cur_group()` (`.BY`), `cur_group_id()` (`.GRP`), 
   and `cur_group_rows() (`.I`) are now tranlsated to their data.table 
   equivalents (#166).

* `filter()` on grouped data nows use a much faster translation using on `.I`
  rather than `.SD` (and requiring an intermediate assignment) (#176). Thanks 
  to suggestion from @myoung3 and @ColeMiller1.

* Translation of individual expressions:

  * `x[[1]]` is now translated correctly.
  
  * Anonymous functions are now preserved (@smingerson, #155)
  
  * Environment variables used in the `i` argument of `[.data.table` are
    now correctly inlined when not in the global environment (#164).

  * `T` and `F` are correctly translated to `TRUE` and `FALSE` (#140).

## Minor improvements and bug fixes

* Grouped filter, mutate, and slice no longer affect ordering of output (#178).

* `as_tibble()` gains a `.name_repair` argument (@markfairbanks).

* `as.data.table()` always calls `[]` so that the result will print (#146). 

* `print.lazy_dt()` shows total rows, and grouping, if present.

* `group_map()` and `group_walk()` are now translated (#108).

# dtplyr 1.0.1

* Better handling for `.data` and `.env` pronouns (#138).

* dplyr verbs now work with `NULL` inputs (#129).

* joins do better job at determining output variables in the presence of 
  duplicated outputs (#128). When joining based on different variables in `x` 
  and `y`, joins consistently preserve column from `x`, not `y` (#137).

* `lazy_dt()` objects now have a useful `glimpse()` method (#132).

* `group_by()` now has an `arrange` parameter which, if set to `FALSE`, sets 
  the data.table translation to use `by` rather than `keyby` (#85).

* `rename()` now works without `data.table` attached, as intended 
  (@michaelchirico, #123).

* dtplyr has been re-licensed as MIT (#165).  

# dtplyr 1.0.0

*   Converted from eager approach to lazy approach. You now must use `lazy_dt()`
    to begin a translation pipeline, and must use `collect()`, `as.data.table()`, 
    `as.data.frame()`, or `as_tibble()` to finish the translation and actually
    perform the computation (#38).
    
    This represents a complete overhaul of the package replacing the eager 
    evaluation used in the previous releases. This unfortunately breaks all
    existing code that used dtplyr, but frankly the previous version was 
    extremely inefficient so offered little of data.table's impressive speed,
    and was used by very few people.

* dtplyr provides methods for data.tables that warning you that they use the
  data frame implementation and you should use `lazy_dt()` (#77)

* Joins now pass `...` on to data.table's merge method (#41).

* `ungroup()` now copies its input (@christophsax, #54).

* `mutate()` preserves grouping (@christophsax, #17).

* `if_else()` and `coalesce()` are mapped to data.table's `fifelse()` and 
  `fcoalesce()` respectively (@michaelchirico, #112).

# dtplyr 0.0.3

- Maintenance release for CRAN checks.

- `inner_join()`, `left_join()`, `right_join()`, and `full_join()`: new `suffix`
  argument which allows you to control what suffix duplicated variable names
  receive, as introduced in dplyr 0.5 (#40, @christophsax).

- Joins use extended `merge.data.table()` and the `on` argument, introduced in
  data.table 1.9.6. Avoids copy and allows joins by different keys (#20, #21,
  @christophsax).

# dtplyr 0.0.2

- This is a compatibility release. It makes dtplyr compatible with
  dplyr 0.6.0 in addition to dplyr 0.5.0.


# dtplyr 0.0.1

- `distinct()` gains `.keep_all` argument (#30, #31).

- Slightly improve test coverage (#6).

- Install `devtools` from GitHub on Travis (#32).

- Joins return `data.table`. Right and full join are now implemented (#16, #19).

- Remove warnings from tests (#4).

- Extracted from `dplyr` at revision e5f2952923028803.
