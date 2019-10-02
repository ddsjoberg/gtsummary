--------------------------------------------------------------------------------

Checklist for PR reviewer when merging into the master branch

- Pre-approval Checklist
  - [ ] The PR branch has pulled the most recent updates from master before approving merge? Documentation updates may have been pushed directly to master from a fork and not repesented in the dev branch.
  - [ ] `usethis::use_spell_check()` runs with no spelling errors in documentation
  - [ ] R CMD Check runs without errors, warnings, and notes
  - [ ] Code coverage suitable.  Run `covr::report()` for a line-by-line report of code coverage.
  - [ ] NEWS.md has been updated under the heading `# mskRvis (development version)` for each update.

- When PR is ready to be approved
  - [ ] Bump the version number using `usethis::use_version()`, and select the appropriate bump in version number (i.e. major, minor, or patch).
  - [ ] Check that the NEWS.md file version header has been appropriately updated (this should automatically be done by `usethis::use_version()`). 
  - [ ] pkgdown website  
      - [ ] All new functions have been added to the pkgdown YAML
      - [ ] All new vignettes have been added to the pkgdown YAML
      - [ ] Re-build the website with `pkgdown::build_site()`
  - [ ] Approve and merge PR into master branch
  - [ ] Go the the GitHub page and create a release.

- Final Steps
    - [ ] Return to the dev branch and run `usethis::use_dev_version()`. This will bump the version number to end with .9000, and the header "`# mskRvis (development version)`" will be added to NEWS.md.