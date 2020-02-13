pkgname <- "biostatR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('biostatR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("biostatR_conflicts")
### * biostatR_conflicts

flush(stderr()); flush(stdout())

### Name: biostatR_conflicts
### Title: Conflicts between the biostatR and other packages
### Aliases: biostatR_conflicts

### ** Examples

biostatR_conflicts()



cleanEx()
nameEx("biostatR_logo")
### * biostatR_logo

flush(stderr()); flush(stdout())

### Name: biostatR_logo
### Title: The biostatR logo, using ASCII or Unicode characters
### Aliases: biostatR_logo

### ** Examples

biostatR_logo()



cleanEx()
nameEx("biostatR_packages")
### * biostatR_packages

flush(stderr()); flush(stdout())

### Name: biostatR_packages
### Title: List all packages in biostatR
### Aliases: biostatR_packages

### ** Examples

biostatR_packages()



cleanEx()
nameEx("project_template")
### * project_template

flush(stderr()); flush(stdout())

### Name: project_template
### Title: Biostatistics project template
### Aliases: project_template
### Keywords: datasets

### ** Examples

mskRutils::create_msk_project(
  path = file.path(tempdir(), "Sjoberg New Project"),
  template = biostatR::project_template
)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
