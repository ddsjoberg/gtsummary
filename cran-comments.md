## Resubmission Comments

Thank you for reviewing the package submitted 2019-08-19. 
Based on your comments the following changes were made.
1. The 'gt' package is referred to within single quotes in the DESCRIPTION file.
2. The full URL is now provided for the link in the README.md file (rather than the relative link).


## Test environments
* local Windows 10 install, R 3.6.1
* Ubuntu 16.04.6 LTS (on travis-ci), (devel, release, and oldrel)
* win-builder (devel, release, and oldrel)

## R CMD check results

0 errors | 0 warnings | 1 note

Suggests or Enhances not in mainstream repositories:
  gt

## Additional Comments

Thank you for reviewing this update to my package.  
The package lists the gt package in the Enhances field, and is not available on CRAN.  
The gt package is available on GitHub (rstudio/gt).  
As the gt package is not yet in a mainstream repository, we get the NOTE about mainstream repositories in the R CMD Check.  
The optional installation instructions for the gt package appear in the package description.
