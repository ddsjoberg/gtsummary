## Test environments
* local Windows 10 R installation, R 3.6.1
* Ubuntu 16.04.6 LTS 16.04 (on travis-ci), devel, release, oldrel
* Windows Server 2012 R2 x64 (on appveyor), devel, release
* win-builder devel, release, oldrel

## R CMD check results
Maintainer: 'Daniel D. Sjoberg <danield.sjoberg@gmail.com>'
Days since last update: 2
Suggests or Enhances not in mainstream repositories:
  gt
Availability using Additional_repositories specification:
  gt   yes   http://ddsjoberg.github.io/drat

## Additional Comments

Submission 1 gtsummary v1.2.3:

The NOTE about gt not being in a marinstream reposiroty remains as in all previous versions of the package. The package documentation includes instructions to install it from GitHub.

The previous version of gtsummary was accepted on Nov. 10. Soon after the release I recieved an email from Dr Brian Ripley indicating there was an issue on the Solaris build (a copy of the email is pasted below). A package vignette installs a package from GitHub, and there was an error during its installation. While I cannot replicate the installation issue on any builds available to me, I suspect the error occured becuase the GitHub package was being installed to a temporary folder.

In the submitted version, rather than installing the package directly from GitHub, I've added the built package to a repository hosted on GitHub and referenced its location with the `Additional_repositories:` tag in the `DESCRIPTION` file. I hope this change addresses the installation issues that occured on the Solaris build.

Thank you for taking the time to review.

Email from CRAN:
See https://cran.r-project.org/web/checks/check_results_gtsummary.html .

This attempts to install a package, and fails because it does not
respect that package's SystemRequirements.  Here:

if (curl::has_internet()) {
   # adding tmpdir to libPath
   temp_path <- file.path(tempdir(), "gt_folder")
   dir.create(temp_path)
   lib_path <-.libPaths()
   .libPaths(c(lib_path, temp_path))

   # installing gt
   remotes::install_github("rstudio/gt", lib = temp_path)
}

which apparently also attempts to install sass.

Why?  Packages should be making use of already installed packages (sass
is installed on that system, but may need to be declared).

Please correct ASAP and before Nov 17 to retain the package on CRAN.
