## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Reverse dependency checks

We checked 28 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 potentially new problem.
     - I use two tools to check reverse dependency: one tool reported no issues, and the second reported an issue with the guideR package.
     - I cloned the guideR to investigate and R CMD Check passed without errors, warnings, or notes. Therefore, I think this is a false positive.
     - Regardless, I know the guideR maintainer, and I've reached out to let them know and they are aware of the upcoming gtsummary release, and are ready to make a release if needed.
 * We failed to check 3 packages (brms.mmrm, equatiomatic, MiscMetabar)

## Additional Comments

Thank you for your time!
