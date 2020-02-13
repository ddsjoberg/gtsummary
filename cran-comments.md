## Test environments
* local Windows 10 R installation, R 3.6.2
* Ubuntu 16.04.6 LTS 16.04 (on travis-ci), 3.4, oldrel, release, devel
* Windows Server 2012 R2 x64 (on appveyor), release, devel
* win-builder oldrel, release, devel

## R CMD check results
> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Daniel D. Sjoberg <danield.sjoberg@gmail.com>'
  
  Suggests or Enhances not in mainstream repositories:
    gt
  Availability using Additional_repositories specification:
    gt   yes   http://ddsjoberg.github.io/drat

## Additional Comments

Apologies for this submission so shortly after the last release: an important bug fix didn't make into the last release. Thank you for your time! Much appreciated!

The note about the gt package not in a mainstream repo is expected and has appeared in all previous releases.
