## Comments

This submission is in response to an email I received from CRAN.  A package vignette included code that installed a package to the default system folder violating CRAN policy.  The package installation code has been removed.  The full text of the email received from CRAN is pasted below.

## Test environments
* local Windows 10 install, R 3.6.1
* Ubuntu 16.04.6 LTS (on travis-ci), (devel, release, and oldrel)
* win-builder (devel, release, and oldrel)

## R CMD check results

0 errors | 0 warnings | 1 note

Suggests or Enhances not in mainstream repositories:
  gt

## Additional Comments

The package lists the gt package in the Enhances field, and is not available on CRAN.  
The gt package is available on GitHub (rstudio/gt).  
As the gt package is not yet in a mainstream repository, we get the NOTE about mainstream repositories in the R CMD Check.  
The optional installation instructions for the gt package appear in the package description.

## Email from CRAN from Kurt Hornik

Dear maintainer,

Pls see
<https://cran.r-project.org/web/checks/check_results_gtsummary.html>.

The check problems on the Debian systems are caused by attempts to write
to the user library to which all packages get installed before checking
(and which now is remounted read-only for checking).

Having package code which is run as part of the checks and attempts to
write to the user library violates the CRAN Policy's

  Packages should not write in the user’s home filespace (including
  clipboards), nor anywhere else on the file system apart from the R
  session’s temporary directory (or during installation in the location
  pointed to by TMPDIR: and such usage should be cleaned up).

In your case, you need to teach remotes::install_github("rstudio/gt") to
install to a temporary library and use it from there.

Please correct before 2019-09-03 to safely retain your package on CRAN.

-k
