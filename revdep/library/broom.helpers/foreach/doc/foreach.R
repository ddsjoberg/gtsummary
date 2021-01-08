## ----loadLibs-----------------------------------------------------------------
library(foreach)

## ----ex1----------------------------------------------------------------------
x <- foreach(i=1:3) %do% sqrt(i)
x

## ----ex2----------------------------------------------------------------------
x <- foreach(a=1:3, b=rep(10, 3)) %do% (a + b)
x

## ----ex3----------------------------------------------------------------------
x <- foreach(a=1:3, b=rep(10, 3)) %do% {
  a + b
}
x

## ----ex4----------------------------------------------------------------------
x <- foreach(a=1:1000, b=rep(10, 2)) %do% {
  a + b
}
x

## ----ex5----------------------------------------------------------------------
x <- foreach(i=1:3, .combine='c') %do% exp(i)
x

## ----ex6----------------------------------------------------------------------
x <- foreach(i=1:4, .combine='cbind') %do% rnorm(4)
x

## ----ex7----------------------------------------------------------------------
x <- foreach(i=1:4, .combine='+') %do% rnorm(4)
x

## ----ex7.1--------------------------------------------------------------------
cfun <- function(a, b) NULL
x <- foreach(i=1:4, .combine='cfun') %do% rnorm(4)
x

## ----ex7.2--------------------------------------------------------------------
cfun <- function(...) NULL
x <- foreach(i=1:4, .combine='cfun', .multicombine=TRUE) %do% rnorm(4)
x

## ----ex7.3--------------------------------------------------------------------
cfun <- function(...) NULL
x <- foreach(i=1:4, .combine='cfun', .multicombine=TRUE, .maxcombine=10) %do% rnorm(4)
x

## ----ex7.4--------------------------------------------------------------------
foreach(i=4:1, .combine='c') %dopar% {
  Sys.sleep(3 * i)
  i
}
foreach(i=4:1, .combine='c', .inorder=FALSE) %dopar% {
  Sys.sleep(3 * i)
  i
}

## ----ex8----------------------------------------------------------------------
library(iterators)
x <- foreach(a=irnorm(4, count=4), .combine='cbind') %do% a
x

## ----ex9----------------------------------------------------------------------
set.seed(123)
x <- foreach(a=irnorm(4, count=1000), .combine='+') %do% a
x

## ----ex10---------------------------------------------------------------------
set.seed(123)
x <- numeric(4)
i <- 0
while (i < 1000) {
  x <- x + rnorm(4)
  i <- i + 1
}
x

## ----ex11---------------------------------------------------------------------
set.seed(123)
x <- foreach(icount(1000), .combine='+') %do% rnorm(4)
x

## ----ex12.data----------------------------------------------------------------
x <- matrix(runif(500), 100)
y <- gl(2, 50)

## ----ex12.load----------------------------------------------------------------
library(randomForest)

## ----ex12.seq-----------------------------------------------------------------
rf <- foreach(ntree=rep(250, 4), .combine=combine) %do%
  randomForest(x, y, ntree=ntree)
rf

## ----ex12.par-----------------------------------------------------------------
rf <- foreach(ntree=rep(250, 4), .combine=combine, .packages='randomForest') %dopar%
  randomForest(x, y, ntree=ntree)
rf

## ----ex13.orig----------------------------------------------------------------
applyKernel <- function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  ans <- vector("list", d2)
  for(i in 1:d2) {
    tmp <- FUN(array(newX[,i], d.call, dn.call), ...)
    if(!is.null(tmp)) ans[[i]] <- tmp
  }
  ans
}
applyKernel(matrix(1:16, 4), mean, 4, 4)

## ----ex13.first---------------------------------------------------------------
applyKernel <- function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  foreach(i=1:d2) %dopar%
    FUN(array(newX[,i], d.call, dn.call), ...)
}
applyKernel(matrix(1:16, 4), mean, 4, 4)

## ----ex13.second--------------------------------------------------------------
applyKernel <- function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  foreach(x=iter(newX, by='col')) %dopar%
    FUN(array(x, d.call, dn.call), ...)
}
applyKernel(matrix(1:16, 4), mean, 4, 4)

## ----ex13.iter, results="hide"------------------------------------------------
iblkcol <- function(a, chunks) {
  n <- ncol(a)
  i <- 1

  nextElem <- function() {
    if (chunks <= 0 || n <= 0) stop('StopIteration')
    m <- ceiling(n / chunks)
    r <- seq(i, length=m)
    i <<- i + m
    n <<- n - m
    chunks <<- chunks - 1
    a[,r, drop=FALSE]
  }

  structure(list(nextElem=nextElem), class=c('iblkcol', 'iter'))
}
nextElem.iblkcol <- function(obj) obj$nextElem()

## ----ex13.third---------------------------------------------------------------
applyKernel <- function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  foreach(x=iblkcol(newX, 3), .combine='c', .packages='foreach') %dopar% {
    foreach(i=1:ncol(x)) %do% FUN(array(x[,i], d.call, dn.call), ...)
  }
}
applyKernel(matrix(1:16, 4), mean, 4, 4)

## ----when---------------------------------------------------------------------
x <- foreach(a=irnorm(1, count=10), .combine='c') %:% when(a >= 0) %do% sqrt(a)
x

## ----qsort--------------------------------------------------------------------
qsort <- function(x) {
  n <- length(x)
  if (n == 0) {
    x
  } else {
    p <- sample(n, 1)
    smaller <- foreach(y=x[-p], .combine=c) %:% when(y <= x[p]) %do% y
    larger  <- foreach(y=x[-p], .combine=c) %:% when(y >  x[p]) %do% y
    c(qsort(smaller), x[p], qsort(larger))
  }
}

qsort(runif(12))

