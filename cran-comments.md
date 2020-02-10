## Test environments
* local Windows 10 R installation, R 3.6.2
* Ubuntu 16.04.6 LTS 16.04 (on travis-ci), 3.4, oldrel, release, devel
* Windows Server 2012 R2 x64 (on appveyor), release, devel
* win-builder oldrel, release, devel

## R CMD check results
Maintainer: 'Daniel D. Sjoberg <danield.sjoberg@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  github (46:49)
  gtsummary (47:5)
  rstudio (46:57)
  sha (47:19)

Suggests or Enhances not in mainstream repositories:
  gt
Availability using Additional_repositories specification:
  gt   yes   http://ddsjoberg.github.io/drat

## Additional Comments

The notes from R CMD Check regarding possibly mis-spelled words in DESCRIPTION are ok, and the words should remain as they appear.

The note about the gt package is expected and has appeared in every previous CRAN release.

In this release we lowered the minimum R version required to 3.4, and added Travis CI checks for the version.

Thank you for your time.
