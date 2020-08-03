.onAttach <- function(lib, pkg) {
  if (stats::runif(1) > .8) {
    msgs <- c("#BlackLivesMatter", "#BlackLivesMatter", "#LGBTQ")
    packageStartupMessage(sample(msgs, size = 1))
  }
}
