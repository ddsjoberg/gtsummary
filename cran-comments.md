## Test environments
* Ubuntu 18.04 LTS (on github actions), devel, release, oldrel-1, oldrel-2, oldrel-3, oldrel-4
* Windows Server 2019 (on github actions), release
* macOS (on github actions), release
* win-builder devel

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## revdepcheck results

We checked 21 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 3 new problems
 * We failed to check 3 packages

Issues with CRAN packages are summarised below.

All 3 authors of packages with new problems were contacted on 2024-07-01 to alert them of the breaking changes.
For 2 of these 3 packages, I submitted fixes to their public code repositories with the fixes.

For the 3 packages that failed to be checked, I contacted them on 2024-07-01 to alert them of a _potential_ breaking change.

### New problems
(This reports the first line of each new failure)

* gtreg
  checking examples ... ERROR

* IPDFileCheck
  checking examples ... ERROR
  checking tests ...

* pubh
  checking examples ... ERROR
  checking running R code from vignettes ...

### Failed to check

* brms.mmrm    (NA)
* equatiomatic (NA)
* MiscMetabar  (NA)

## Additional Comments

Thank you for your time.
