rvector <- function(n, mode = c("logical", "double", "integer"),
                    range = c(-100, +100), na_prob = 0) {
  mode <- match.arg(mode)
  if (mode == "logical") {
    x <- sample(c(FALSE, TRUE), size = n, replace = TRUE)
  } else {
    x <- runif(n, min = range[1], max = range[2])
  }
  storage.mode(x) <- mode
  if (na_prob > 0) x[sample(n, size = na_prob * n)] <- NA
  x
} # rvector()


rvectors <- function(scale = 10, seed = 1, ...) {
  set.seed(seed)
  data <- list()
  data[[1]] <- rvector(n = scale * 1e2, ...)
  data[[2]] <- rvector(n = scale * 1e3, ...)
  data[[3]] <- rvector(n = scale * 1e4, ...)
  data[[4]] <- rvector(n = scale * 1e5, ...)
  data[[5]] <- rvector(n = scale * 1e6, ...)
  names(data) <- sprintf("n = %d", sapply(data, FUN = length))
  data
}
