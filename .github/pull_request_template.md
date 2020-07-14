**What changes are proposed in this pull request?**


**If there is an GitHub issue associated with this pull request, please provide link.**


--------------------------------------------------------------------------------

Checklist for PR reviewer

- [ ] PR branch has pulled the most recent updates from master branch. Ensure the pull request branch and your local version match and both have the latest updates from the master branch.
- [ ] NEWS.md has been updated with the changes from this pull request under the heading "`# gtsummary (development version)`". If there is an issue associated with the pull request, reference it in parantheses at the end update (see NEWS.md for examples).
- [ ] `usethis::use_spell_check()` runs with no spelling errors in documentation
- [ ] If a new function was added, function included in `pkgdown.yml`
- [ ] If a bug was fixed, a unit test was added for the bug check
- [ ] Run `pkgdown::build_site()`. Check the R console for errors, and review the rendered website.
- [ ] Code coverage is suitable for any new functions/features. Review coverage with `covr::report()`. Before you run, set `Sys.setenv(NOT_CRAN = TRUE)` and begin in a fresh R session without any packages loaded. 
- [ ] R CMD Check runs without errors, warnings, and notes
- [ ] When the branch is ready to be merged into master, increment the version number using `usethis::use_version(which = "dev")`, run `codemetar::write_codemeta()`, approve, and merge the PR.

