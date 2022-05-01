.onAttach <- function(lib, pkg) {
  if (stats::runif(1) > .8) {
    msg <- c("#BlackLivesMatter" = 2, "#Uighur" = 1, "#StandWithUkraine" = 1)
    msg <- rep(names(msg), msg)
    packageStartupMessage(sample(msg, size = 1))
  }
}
