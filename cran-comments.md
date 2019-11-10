## Test environments
* local Windows 10 R installation, R 3.6.1
* Ubuntu 16.04.6 LTS 16.04 (on travis-ci), devel, release, oldrel
* Windows Server 2012 R2 x64 (on appveyor), devel, release
* win-builder devel, release, oldrel

## R CMD check results
  Maintainer: 'Daniel D. Sjoberg <danield.sjoberg@gmail.com>'
  Suggests or Enhances not in mainstream repositories:
    gt
  Package which this enhances but not available for checking: 'gt'

## Additional Comments

Submission 2 gtsummary v1.2.2:
Thank you for the review.
The previously listed NOTE about a potential broken link has been resolved.

Submission 1 gtsummary v1.2.2:
Thank you for reviewing this update to my package.
The package lists the gt package in the Enhances field. 
The gt package is not available on CRAN, and can be installed from GitHub (rstudio/gt). 
As the gt package is not yet in a mainstream repository, we get the NOTE about mainstream repositories in the R CMD Check.
The optional installation instructions for the gt package appear in the package description.
