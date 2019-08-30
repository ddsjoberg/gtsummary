#' @templateVar class crr
#' @template title_desc_tidy
#'
#' @param x A `crr2` object returned from [cmprsk2::crr2()].
#' @template param_confint
#' @template param_exponentiate
#' @template param_unused_dots
#'
#' @evalRd return_tidy(
#'   "estimate",
#'   "std.error",
#'   "statistic",
#'   "p.value"
#' )
#'
#' @examples
#'
#' library(survival)
#'
#' cfit <- coxph(Surv(time, status) ~ age + sex, lung)
#'
#' tidy(cfit)
#' tidy(cfit, exponentiate = TRUE)
#'
#' lp <- augment(cfit, lung)
#' risks <- augment(cfit, lung, type.predict = "risk")
#' expected <- augment(cfit, lung, type.predict = "expected")
#'
#' glance(cfit)
#'
#' # also works on clogit models
#' resp <- levels(logan$occupation)
#' n <- nrow(logan)
#' indx <- rep(1:n, length(resp))
#' logan2 <- data.frame(
#'   logan[indx,],
#'   id = indx,
#'   tocc = factor(rep(resp, each=n))
#' )
#'
#' logan2$case <- (logan2$occupation == logan2$tocc)
#'
#' cl <- clogit(case ~ tocc + tocc:education + strata(id), logan2)
#' tidy(cl)
#' glance(cl)
#'
#' library(ggplot2)
#'
#' ggplot(lp, aes(age, .fitted, color = sex)) +
#'   geom_point()
#'
#' ggplot(risks, aes(age, .fitted, color = sex)) +
#'   geom_point()
#'
#' ggplot(expected, aes(time, .fitted, color = sex)) +
#'   geom_point()
#'
#'
#' @aliases coxph_tidiers
#' @export
#' @seealso [tidy()], [survival::coxph()]
#' @family crr tidiers
#' @family cmprsk tidiers
#'

# strip rownames from a data frame
# not exported from broom so had to replicate here
unrowname <- function(x) {
  rownames(x) <- NULL
  x
}


# remove NULL items in a vector or list
# not exported from broom so had to replicate here
compact <- function(x) Filter(Negate(is.null), x)


# copied from modeltests. re-export if at some we Import modeltests rather
# than suggest it
has_rownames <- function(df) {
  if (tibble::is_tibble(df))
    return(FALSE)
  any(rownames(df) != as.character(1:nrow(df)))
}

#' Ensure an object is a data frame, with rownames moved into a column
#'
#' @param x a data.frame or matrix
#' @param newnames new column names, not including the rownames
#' @param newcol the name of the new rownames column
#'
#' @return a data.frame, with rownames moved into a column and new column
#' names assigned
#'
#' @export
fix_data_frame <- function(x, newnames = NULL, newcol = "term") {
  if (!is.null(newnames) && length(newnames) != ncol(x)) {
    stop("newnames must be NULL or have length equal to number of columns")
  }

  if (all(rownames(x) == seq_len(nrow(x)))) {
    # don't need to move rownames into a new column
    ret <- data.frame(x, stringsAsFactors = FALSE)
    if (!is.null(newnames)) {
      colnames(ret) <- newnames
    }
  }
  else {
    ret <- data.frame(
      ...new.col... = rownames(x),
      unrowname(x),
      stringsAsFactors = FALSE
    )
    colnames(ret)[1] <- newcol
    if (!is.null(newnames)) {
      colnames(ret)[-1] <- newnames
    }
  }
  as_tibble(ret)
}


tidy.crr <- function(x, exponentiate = FALSE, conf.int = FALSE,
                       conf.level = .95, ...) {
  # backward compatibility (in previous version, conf.int was used instead of conf.level)
  if (is.numeric(conf.int)) {
    conf.level <- conf.int
    conf.int <- TRUE
  }

  if (conf.int) {
    s <- summary(x, conf.int = conf.level)
  } else {
    s <- summary(x, conf.int = FALSE)
  }
  co <- s$coef
  nn <- c("estimate", "std.error", "statistic", "p.value")
  ret <- fix_data_frame(co[, -2, drop = FALSE], nn)

  if (exponentiate) {
    ret$estimate <- exp(ret$estimate)
  }
  if (!is.null(s$conf.int)) {
    CI <- as.matrix(unrowname(s$conf.int[, 3:4, drop = FALSE]))
    colnames(CI) <- c("conf.low", "conf.high")
    if (!exponentiate) {
      CI <- log(CI)
    }
    ret <- cbind(ret, CI)
  }

  as_tibble(ret)
}

