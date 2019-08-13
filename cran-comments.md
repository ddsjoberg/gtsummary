## Test environments
* local Windwos 10 install, R 3.6.1
* Ubuntu 16.04.6 LTS (on travis-ci), (devel, release, and oldrel)
* win-builder (devel, release, and oldrel)

## R CMD check results

0 errors | 0 warnings | 1 note

Suggests or Enhances not in mainstream repositories:
  gt

## Additional Comments

Thank you for reviewing this update to my package.  
The package is Enhanced by the gt package, which is now only available on GitHub (rstudio/gt).  
As the gt package is not yet in a mainstream repository, we get the NOTE in the R CMD Check.  
The optional installation instructions appear in the package description and on the README page.
