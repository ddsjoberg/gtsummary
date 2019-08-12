do_package_checks()

if (ci_on_travis()) {
  # installing gt from GitHub
  get_stage("install") %>%
    add_step(step_install_github("rstudio/gt"))

  if (ci_is_env("TRAVIS_R_VERSION_STRING", "release")) {
    do_pkgdown()
  }
}
