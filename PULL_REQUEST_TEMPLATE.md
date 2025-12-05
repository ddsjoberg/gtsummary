# NA

**What changes are proposed in this pull request?** \<\< Insert text
here that can be directly copied into NEWS.md by your reviewer. \>\>

**If there is an GitHub issue associated with this pull request, please
provide link.**

------------------------------------------------------------------------

Reviewer Checklist (if item does not apply, mark is as complete)

PR branch has pulled the most recent updates from main branch.

If a bug was fixed, a unit test was added.

Run
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html).
Check the R console for errors, and review the rendered website.

Code coverage is suitable for any new functions/features:
`devtools::test_coverage()`

`usethis::use_spell_check()` runs with no spelling errors in
documentation

**All** GitHub Action workflows pass with a ✅

When the branch is ready to be merged into master: - \[ \] Update
`NEWS.md` with the changes from this pull request under the heading
“`# gtsummary (development version)`”. If there is an issue associated
with the pull request, reference it in parentheses at the end update
(see `NEWS.md` for examples). - \[ \] Increment the version number using
`usethis::use_version(which = "dev")` - \[ \] Run
`usethis::use_spell_check()` again - \[ \] Approve Pull Request - \[ \]
Merge the PR. Please use “Squash and merge”.
