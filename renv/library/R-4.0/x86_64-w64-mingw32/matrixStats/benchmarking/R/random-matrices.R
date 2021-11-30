rmatrix <- function(nrow, ncol,
                    mode = c("logical", "double", "integer", "index"),
                    range = c(-100, +100), na_prob = 0) {
  mode <- match.arg(mode)
  n <- nrow * ncol
  if (mode == "logical") {
    x <- sample(c(FALSE, TRUE), size = n, replace = TRUE)
  } else if (mode == "index") {
    x <- seq_len(n)
    mode <- "integer"
  } else {
    x <- runif(n, min = range[1], max = range[2])
  }
  storage.mode(x) <- mode
  if (na_prob > 0) x[sample(n, size = na_prob * n)] <- NA
  dim(x) <- c(nrow, ncol)
  x
}


rmatrices <- function(scale = 10, seed = 1, ...) {
  set.seed(seed)
  data <- list()
  data[[1]] <- rmatrix(nrow = scale *   1, ncol = scale *  1, ...)
  data[[2]] <- rmatrix(nrow = scale *  10, ncol = scale * 10, ...)
  data[[3]] <- rmatrix(nrow = scale * 100, ncol = scale *  1, ...)
  data[[4]] <- t(data[[3]])
  data[[5]] <- rmatrix(nrow = scale * 10, ncol = scale * 100, ...)
  data[[6]] <- t(data[[5]])
  names(data) <- sapply(data, FUN = function(x) paste(dim(x), collapse = "x"))
  data
}
