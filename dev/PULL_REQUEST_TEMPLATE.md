# NA

**What changes are proposed in this pull request?**

**If there is a GitHub issue associated with this pull request, please
provide a link.**

------------------------------------------------------------------------

Submitter Checklist

My local clone (and fork, if I use one) was created **on or after
2026-11-21**. *The repository history was rewritten on 2026-11-21 to
remove ~670 MB of previously committed files. Branches from older clones
or forks still contain the old history, and merging them would put those
files back. If needed, delete your fork/clone and re-fork or re-clone
before opening a PR.*

Added a bullet to `NEWS.md` under the heading
“`# gtsummary (development version)`” describing the change. If there is
an associated issue, reference it in parentheses at the end of the
bullet (see `NEWS.md` for examples).

PR title begins with `perf` if this change may affect package
performance (this triggers the benchmark workflow).

Ran `devtools::document()` if roxygen comments or exports changed (no
hand edits to `man/` or `NAMESPACE`).

Any snapshot changes in `tests/testthat/_snaps/` are intentional and
explained above (performance-only changes must not change snapshots).

Code follows the tidyverse style guide; `styler` was applied to changed
code only.

[`spelling::spell_check_package()`](https://docs.ropensci.org/spelling/reference/spell_check_package.html)
runs with no spelling errors in documentation.

Reviewer Checklist (if an item does not apply, mark it as complete)

If a bug was fixed, a unit test was added (in the existing `test-*.R`
file for that function).

Code coverage is suitable for any new functions/features:
`devtools::test_coverage()`

The `NEWS.md` entry is present and accurate.

**All** GitHub Action workflows pass with a ✅

When the branch is ready to be merged into main: - \[ \] Approve Pull
Request - \[ \] Merge the PR. Please use “Squash and merge”.

See
[CONTRIBUTING.md](https://github.com/ddsjoberg/gtsummary/blob/main/.github/CONTRIBUTING.md)
for full details.
