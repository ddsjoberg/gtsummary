- What changes are proposed in this pull request?

- If there is an GitHub issue associated with this pull request, please provide link.


--------------------------------------------------------------------------------

Checklist for PR reviewer

- [ ] PR branch has pulled the most recent updates from master branch 
- [ ] `usethis::use_spell_check()` runs with no spelling errors in documentation
- [ ] If a new function was added, was function included in `pkgdown.yml`
- [ ] R CMD Check runs without errors, warnings, and notes
- [ ] Code coverage is suitable for any new functions/features. 
- [ ] NEWS.md has been updated under the heading "`# gtsummary (development version)`"?
- [ ] When the branch is ready to be merged into master, bump the version number using `usethis::use_version(which = "dev")`, approve, and merge the PR.

