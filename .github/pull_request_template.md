- What changes are proposed in this pull request?

- If there is an GitHub issue associated with this pull request, please provide link.


--------------------------------------------------------------------------------

Checklist for PR reviewer

- [ ] PR branch has pulled the most recent updates from dev branch 
- [ ] `usethis::use_spell_check()` runs with no spelling errors in documentation
- [ ] R CMD Check runs without errors, warnings, and notes
- [ ] Code coverage is suitable for any new functions/features. 
- [ ] NEWS.md has been updated under the heading "`# mskRvis (development version)`"?
- [ ] When the branch is ready to be merged into dev, bump the version number using `usethis::use_version(which = "dev")`, approve, and merge the PR.

\* If this is PR into the master branch, use [this checklist](https://github.mskcc.org/raw/datadojo/mskRutils/master/.github/pull_request_into_master_checklist.md).
