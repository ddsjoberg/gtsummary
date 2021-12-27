#!/usr/bin/r -t
##
## Copyright (C) 2017 - 2021  Binxiang Ni and Dirk Eddelbuettel
##
## This file is part of RcppArmadillo. It is based on the documentation
## of package Matrix, slam, SparseM, spam and SciPy, which are
## respectively created by developers of the packages: Douglas Bates,
## Martin Maechler; Kurt Hornik, David Meyer, Christian Buchta; Roger
## Koenker, Pin Ng; Reinhard Furrer, Florian Gerber, Daniel Gerber,
## Kaspar Moesinger, Youcef Saad, Esmond G. Ng, Barry W. Peyton, Joseph
## W.H. Liu, Alan D. George; the developers of SciPy. It is also
## modified by Binxiang Ni.
##
## RcppArmadillo is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## RcppArmadillo is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

## Reference:
## [Matrix]: https://cran.r-project.org/web/packages/Matrix/Matrix.pdf
## [slam]: https://cran.r-project.org/web/packages/slam/slam.pdf
## [SparseM]: https://cran.r-project.org/web/packages/SparseM/SparseM.pdf
## [spam]: https://cran.r-project.org/web/packages/spam/spam.pdf
## [SciPy]: https://docs.scipy.org/doc/scipy/reference/sparse.html

if (!requireNamespace("Matrix", quietly=TRUE)) exit_file("No Matrix package")

## It now (Nov 2020) appears to fail on Windows starting around line 115
.onWindows <- .Platform$OS.type == "windows"

library(RcppArmadillo)

Rcpp::sourceCpp("cpp/sparse.cpp")

## setting up an example matrix -- using the fact that the as<sp_mat>
## converter prefers sparse matrix objects create by the Matrix package
suppressMessages({
    library(Matrix)
    library(stats)
    ## Per email with Martin Maechler, hard to suppress such messages on
    ## first (and only) use of particular dispatches.  So simply running
    ## twice: once silent, and again to test and possibly fail visibly.
    kronecker(Diagonal(3), Matrix(0+0:5, 3, 2))
    ##
    n1 <- 10
    p <- 5
    a <- rnorm(n1*p)
    a[abs(a)<0.5] <- 0
    A <- matrix(a,n1,p)
    RA <- as(A, "dgRMatrix")
    dgt <- RA %x% matrix(1:4,2,2)
})

#test.as.dgc2dgc <- function() {
## [Matrix] p10 (dgCMatrix)
set.seed(7)
m <- matrix(0, 5, 5)
m[sample(length(m), size = 14)] <- rep(1:9, length=14)
mm <- as(m, "CsparseMatrix")
expect_equal(mm, asSpMat(mm))#, msg="dgC2dgC_1")

## [Matrix] p36 (dgCMatrix)
m <- Matrix(c(0,0,2:0), 3,5)
expect_equal(m, asSpMat(m))#, msg="dgC2dgC_2")

## [Matrix] p74 (dgCMatrix)
set.seed(27)
IM1 <- as(sample(1:20, 100, replace=TRUE), "indMatrix")
set.seed(27)
IM2 <- as(sample(1:18, 100, replace=TRUE), "indMatrix")
c12 <- crossprod(IM1,IM2)
expect_equal(c12, asSpMat(c12))#, msg="dgC2dgC_3")

## [Matrix] p87 (dgCMatrix)
m <- Matrix(c(0,0,2:0), 3,5, dimnames=list(LETTERS[1:3],NULL))
m <- unname(m)
expect_equal(m, asSpMat(m))#, msg="dgC2dgC_4")

## [Matrix] p118 (dgCMatrix)
f1 <- gl(5, 3, labels = LETTERS[1:5])
X <- as(f1, "sparseMatrix")
X <- unname(X)
expect_equal(X, asSpMat(X))#, msg="dgC2dgC_5")

## [Matrix] p142 (dgCMatrix)
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
A <- sparseMatrix(i, j, x = x)
expect_equal(A, asSpMat(A))#, msg="dgC2dgC_6")

## [slam] p4 (dgCMatrix)
x <- matrix(c(1, 0, 0, 2, 1, 0), nrow = 3)
SM <- Matrix(x, sparse = TRUE)
expect_equal(SM, asSpMat(SM))#, msg="dgC2dgC_7")

## [slam] p9 (dgCMatrix)
x <- matrix(c(1, 0, 0, 2, 1, NA), nrow = 2)
SM <- Matrix(x, sparse = TRUE)
expect_equal(SM, asSpMat(SM))#, msg="dgC2dgC_8")

if (.onWindows) exit_file("Skipping remainder on Windows")

## [slam] p12 (dgCMatrix)
if (utils::packageVersion("Matrix") >= "1.3.0") {
    x <- matrix(c(1, 0, 0, 2), nrow = 2)
    SM <- Matrix(x, sparse = TRUE, doDiag=FALSE)
    dgc <- as(SM, "dgCMatrix")
    expect_equal(dgc, asSpMat(SM))#, msg="dgC2dgC_9")
}

## [SparseM] p21 (dgCMatrix)
set.seed(21)
a <- rnorm(20*5)
A <- matrix(a,20,5)
A[row(A)>col(A)+4|row(A)<col(A)+3] <- 0
CA <- as(A, "dgCMatrix")
expect_equal(CA, asSpMat(CA))#, msg="dgC2dgC_10")

set.seed(22)
b <- rnorm(20*5)
B <- matrix(b,20,5)
B[row(A)>col(A)+2|row(A)<col(A)+2] <- 0
CB <- as(B, "dgCMatrix")
expect_equal(CB, asSpMat(CB))#, msg="dgC2dgC_11")

cp <- crossprod(CA, CB)
expect_equal(cp, asSpMat(cp))#, msg="dgC2dgC_12")

## [SciPy] (dgCMatrix)
mtxt <- c("1 2 0",
          "0 0 3",
          "4 0 5")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
A <- Matrix(M, sparse = TRUE)
expect_equal(A, asSpMat(A))#, msg="dgC2dgC_13")

V <- c(1, 0, -1)
expect_equal((A * V), asSpMat(A * V))#, msg="dgC2dgC_14")


#test.as.dtc2dgc <- function() {
## [Matrix] p42 (dtCMatrix)
I4 <- .sparseDiagonal(4, shape="t")
dgc <- as(I4, "dgCMatrix")
expect_equal(dgc, asSpMat(I4))#, msg="dtC2dgC_1")

## [Matrix] p49 (dtCMatrix)
t5 <- new("dtCMatrix", Dim = c(5L, 5L), uplo = "L",
          x = c(10, 1, 3, 10, 1, 10, 1, 10, 10),
          i = c(0L,2L,4L, 1L, 3L,2L,4L, 3L, 4L),
          p = c(0L, 3L, 5L, 7:9))
dgc <- as(t5, "dgCMatrix")
expect_equal(dgc, asSpMat(t5))#, msg="dtC2dgC_2")

## [Matrix] p56 (dtCMatrix)
t1 <- new("dtTMatrix", x= c(3,7), i= 0:1, j=3:2, Dim= as.integer(c(4,4)))
tu <- t1 ; tu@diag <- "U"
cu <- as(tu, "dtCMatrix")
dgc <- as(cu, "dgCMatrix")
expect_equal(dgc, asSpMat(cu))#, msg="dtC2dgC_3")

## [Matrix] p56 (dtCMatrix)
U5 <- new("dtCMatrix", i= c(1L, 0:3), p=c(0L,0L,0:2, 5L), Dim = c(5L, 5L),
          x = rep(1, 5), diag = "U")
dgc <- as(U5, "dgCMatrix")
expect_equal(dgc, asSpMat(U5))#, msg="dtC2dgC_4")

## [Matrix] p142 (dtCMatrix)
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
tA <- sparseMatrix(i, j, x = x, triangular= TRUE)
dgc <- as(tA, "dgCMatrix")
expect_equal(dgc, asSpMat(tA))#, msg="dtC2dgC_5")


#test.as.dsc2dgc <- function() {
## [Matrix] p42 (dsCMatrix)
S <- crossprod(Matrix(rbinom(60, size=1, prob=0.1), 10,6))
dgc <- as(S, "dgCMatrix")
expect_equal(dgc, asSpMat(S))#, msg="dsC2dgC_1")

SI <- S + 10*.symDiagonal(6)
dgc <- as(SI, "dgCMatrix")
expect_equal(dgc, asSpMat(SI))#, msg="dsC2dgC_2")

## [Matrix] p50 (dsCMatrix)
mm <- Matrix(toeplitz(c(10, 0, 1, 0, 3)), sparse = TRUE)
dgc <- as(mm, "dgCMatrix")
expect_equal(dgc, asSpMat(mm))#, msg="dsC2dgC_3")

mm <- t(mm)
dgc <- as(mm, "dgCMatrix")
expect_equal(dgc, asSpMat(mm))#, msg="dsC2dgC_4")

## [Matrix] p51 (dsCMatrix)
mm <- Matrix(toeplitz(c(10, 0, 1, 0, 3)), sparse = TRUE)
mT <- as(mm, "dgTMatrix")
symM <- as(mT, "symmetricMatrix")
symC <- as(symM, "CsparseMatrix")
expect_equal(dgc, asSpMat(symC))#, msg="dsC2dgC_5")

sC <- Matrix(mT, sparse=TRUE, forceCheck=TRUE)
expect_equal(dgc, asSpMat(sC))#, msg="dsC2dgC_6")

## [Matrix] p129 (dsCMatrix)
S9 <- rsparsematrix(9, 9, nnz = 10, symmetric=TRUE)
dgc <- as(S9, "dgCMatrix")
expect_equal(dgc, asSpMat(S9))#, msg="dsC2dgC_7")

## [Matrix] p142 (dsCMatrix)
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
sA <- sparseMatrix(i, j, x = x, symmetric = TRUE)
dgc <- as(sA, "dgCMatrix")
expect_equal(dgc, asSpMat(sA))#, msg="dsC2dgC_8")


#test.as.dgt2dgc <- function() {
## [Matrix] p40 (dgTMatrix)
m <- Matrix(0+1:28, nrow = 4)
m[-3,c(2,4:5,7)] <- m[ 3, 1:4] <- m[1:3, 6] <- 0
mT <- as(m, "dgTMatrix")
dgc <- as(m, "dgCMatrix")
expect_equal(dgc, asSpMat(mT))#, msg="dgT2dgC_1")

## [Matrix] p40 (dgTMatrix)
T2 <- new("dgTMatrix",
          i = as.integer(c(1,1,0,3,3)),
          j = as.integer(c(2,2,4,0,0)), x=10*1:5, Dim=4:5)
dgc <- as(T2, "dgCMatrix")
expect_equal(dgc, asSpMat(T2))#, msg="dgT2dgC_2")

## [Matrix] p42 (dgTMatrix)
M1 <- Matrix(0+0:5, 2,3)
M <- kronecker(Diagonal(3), M1)
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(M))#, msg="dgT2dgC_3")

## [Matrix] p49 (dgTMatrix)
m <- spMatrix(10,20, i= 1:8, j=2:9, x = c(0:2,3:-1))
dgc <- as(m, "dgCMatrix")
expect_equal(dgc, asSpMat(m))#, msg="dgT2dgC_4")
expect_equal(drop0(m), asSpMat(drop0(m)))#, msg="dgT2dgC_5")

## [Matrix] p51 (dgTMatrix)
mm <- Matrix(toeplitz(c(10, 0, 1, 0, 3)), sparse = TRUE)
mT <- as(mm, "dgTMatrix")
dgc <- as(mm, "dgCMatrix")
expect_equal(dgc, asSpMat(mT))#, msg="dgT2dgC_6")

## [Matrix] p77 (dgTMatrix)
A <- spMatrix(10,20, i = c(1,3:8),
              j = c(2,9,6:10),
              x = 7 * (1:7))
dgc <- as(A, "dgCMatrix")
expect_equal(dgc, asSpMat(A))#, msg="dgT2dgC_7")

## [Matrix] p129 (dgTMatrix)
if (utils::packageVersion("Matrix") >= "1.3.0") {
    set.seed(129)
    T2 <- rsparsematrix(40, 12, nnz = 99, repr="T")
    dgc <- as(T2, "dgCMatrix")
    expect_equal(dgc, asSpMat(T2))#, msg="dgT2dgC_8")
}

## [Matrix] p152 (dgTMatrix)
A <- spMatrix(10,20, i = c(1,3:8),
              j = c(2,9,6:10),
              x = 7 * (1:7))
dgc <- as(A, "dgCMatrix")
expect_equal(dgc, asSpMat(A))#, msg="dgT2dgC_9")

## [SparseM] p21 (dgTMatrix)
set.seed(21)
a <- rnorm(20*5)
A <- matrix(a,20,5)
A[row(A)>col(A)+4|row(A)<col(A)+3] <- 0
TA <- as(A, "dgTMatrix")
CA <- as(A, "dgCMatrix")
expect_equal(CA, asSpMat(TA))#, msg="dgT2dgC_10")

set.seed(22)
b <- rnorm(20*5)
B <- matrix(b,20,5)
B[row(A)>col(A)+2|row(A)<col(A)+2] <- 0
TB <- as(B, "dgTMatrix")
CB <- as(B, "dgCMatrix")
expect_equal(CB, asSpMat(TB))#, msg="dgT2dgC_11")

## (dgTMatrix)
n1 <- 10
p <- 5
a <- rnorm(n1*p)
a[abs(a)<0.5] <- 0
A <- matrix(a,n1,p)
RA <- as(A, "dgRMatrix")
dgt <- RA %x% matrix(1:4,2,2)
dgc <- as(dgt, "dgCMatrix")
expect_equal(dgc, asSpMat(dgt))#, msg="dgT2dgC_12")

## [spam] p4 (dgTMatrix)
x <- matrix(c(1, 0, 0, 2, 1, 0), nrow = 3)
dgt <- as(x, "dgTMatrix")
dgc <- as(x, "dgCMatrix")
expect_equal(dgc, asSpMat(dgt))#, msg="dgT2dgC_13")

## [spam] p12 (dgTMatrix)
x <- matrix(c(1, 0, 0, 2), nrow = 2)
dgt <- as(x, "dgTMatrix")
dgc <- as(x, "dgCMatrix")
expect_equal(dgc, asSpMat(dgt))#, msg="dgT2dgC_14")

## [spam] p14 (dgTMatrix)
x <- matrix(c(1, 0, 0, 2, 1, NA), nrow = 3)
dgt <- as(x, "dgTMatrix")
dgc <- as(x, "dgCMatrix")
expect_equal(dgc, asSpMat(dgt))#, msg="dgT2dgC_15")

## [SciPy] (dgTMatrix)
dgt <- new("dgTMatrix", x = c(4, 5, 7, 9), i = as.integer(c(0, 3, 1, 0)), j = as.integer(c(0, 3, 1, 2)), Dim= as.integer(c(4,4)))
dgc <- as(dgt, "dgCMatrix")
expect_equal(dgc, asSpMat(dgt))#, msg="dgT2dgC_16")

## [SciPy] (dgTMatrix)
dgt <- new("dgTMatrix", x = rep(1, 7), i = as.integer(c(0, 0, 1, 3, 1, 0, 0)), j = as.integer(c(0, 2, 1, 3, 1, 0, 0)), Dim= as.integer(c(4, 4)))
dgc <- as(dgt, "dgCMatrix")
expect_equal(dgc, asSpMat(dgt))#, msg="dgT2dgC_17")

## (dgTMatrix)
mtxt <- c("11   0   0  14   0  16",
          " 0  22   0   0  25  26",
          " 0   0  33  34   0  36",
          "41   0  43  44   0  46")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
SM <- Matrix(M, sparse=TRUE)
dgt <- as(SM, "dgTMatrix")
expect_equal(SM, asSpMat(dgt))#, msg="dgT2dgC_18")


#test.as.dtt2dgc <- function() {
## [Matrix] p56 (dtTMatrix)
t1 <- new("dtTMatrix", x= c(3,7), i= 0:1, j=3:2, Dim= as.integer(c(4,4)))
mtxt <- c("0 0 0 3",
          "0 0 7 0",
          "0 0 0 0",
          "0 0 0 0")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(t1))#, msg="dtT2dgC_1")

tu <- t1 ; tu@diag <- "U"
mtxt <- c("1 0 0 3",
          "0 1 7 0",
          "0 0 1 0",
          "0 0 0 1")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(tu))#, msg="dtT2dgC_2")

## [Matrix] p56 (dtTMatrix)
t1[1,2:3] <- -1:-2
mtxt <- c("0 -1 -2 3",
          "0  0  7 0",
          "0  0  0 0",
          "0  0  0 0")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(t1))#, msg="dtT2dgC_3")

diag(t1) <- 10*c(1:2,3:2)
mtxt <- c("10 -1 -2 3",
          "0  20  7 0",
          "0  0  30 0",
          "0  0  0 20")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(t1))#, msg="dtT2dgC_4")


#test.as.dst2dgc <- function() {
## [Matrix] p51 (dsTMatrix)
mm <- Matrix(toeplitz(c(10, 0, 1, 0, 3)), sparse = TRUE)
mT <- as(mm, "dgTMatrix")
symM <- as(mT, "symmetricMatrix")
dgc <- as(mm, "dgCMatrix")
expect_equal(dgc, asSpMat(symM))#, msg="dsT2dgC_1")

## [Matrix] p51 (dsTMatrix)
symC <- as(symM, "CsparseMatrix")
sym2 <- as(symC, "TsparseMatrix")
expect_equal(dgc, asSpMat(sym2))#, msg="dsT2dgC_2")


#test.as.dgr2dgc <- function() {
## [SparseM] p23 (dgRMatrix)
n1 <- 10
p <- 5
a <- rnorm(n1*p)
a[abs(a)<0.5] <- 0
A <- matrix(a,n1,p)
RA <- as(A, "dgRMatrix")
CA <- as(A, "dgCMatrix")
expect_equal(CA, asSpMat(RA))#, msg="dgR2dgC_1")

## [SparseM] p25 (dgRMatrix)
n1 <- 10
n2 <- 10
p <- 6
set.seed(16)
a <- rnorm(n1*p)
a[abs(a) < 0.5] <- 0
A <- matrix(a,n1,p)
RA <- as(A, "dgRMatrix")
CA <- as(A, "dgCMatrix")
expect_equal(CA, asSpMat(RA))#, msg="dgR2dgC_2")

set.seed(25)
b <- rnorm(n2*p)
b[abs(b)<1.0] <- 0
B <- matrix(b,n2,p)
RB <- as(B, "dgRMatrix")
CB <- as(B, "dgCMatrix")
expect_equal(CB, asSpMat(RB))#, msg="dgR2dgC_3")

## [SciPy] (dgRMatrix)
dgt <- new("dgTMatrix", x = rep(1, 7), i = as.integer(c(0, 0, 1, 3, 1, 0, 0)), j = as.integer(c(0, 2, 1, 3, 1, 0, 0)), Dim= as.integer(c(4, 4)))
dgr <- as(as(dgt, "matrix"), "dgRMatrix")
dgc <- as(dgt, "dgCMatrix")
expect_equal(dgc, asSpMat(dgr))#, msg="dgR2dgC_4")

## (dgRMatrix)
mtxt <- c("11   0   0  14   0  16",
          " 0  22   0   0  25  26",
          " 0   0  33  34   0  36",
          "41   0  43  44   0  46")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
SM <- Matrix(M, sparse=TRUE)
dgr <- as(M, "dgRMatrix")
expect_equal(SM, asSpMat(dgr))#, msg="dgR2dgC_5")


#test.as.dtr2dgc <- function() {
## [Matrix] p59 (dtRMatrix)
m2 <- new("dtRMatrix", Dim = c(2L,2L),
          x = c(5, 1:2), p = c(0L,2:3), j= c(0:1,1L))
mtxt <- c("5 1",
          "0 2")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(m2))#, msg="dtR2dgC_1")

m3 <- as(Diagonal(2), "RsparseMatrix")
mtxt <- c("1 0",
          "0 1")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(m3))#, msg="dtR2dgC_2")

## (dtRMatrix)
mtxt <- c("0 0 0 3",
          "0 0 7 0",
          "0 0 0 0",
          "0 0 0 0")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dtc <- Matrix(M, sparse=TRUE)
dgc <- methods::as(dtc, "dgCMatrix")
dtr <- methods::as(dtc, "RsparseMatrix")
expect_equal(dgc, asSpMat(dtr))#, msg="dtR2dgC_3")

dtc@diag <- "U"
dgc <- methods::as(dtc, "dgCMatrix")
dtr <- methods::as(dtc, "RsparseMatrix")
expect_equal(dgc, asSpMat(dtr))#, msg="dtR2dgC_4")


#test.as.dsr2dgc <- function() {
## [Matrix] p53 (dsRMatrix)
m2 <- new("dsRMatrix", Dim = c(2L,2L),
          x = c(3,1), j = c(1L,1L), p = 0:2)
mtxt <- c("0 3",
          "3 1")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(m2))#, msg="dsR2dgC_1")

ds2 <- forceSymmetric(diag(2))
dR <- as(ds2, "RsparseMatrix")
mtxt <- c("1 0",
          "0 1")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(dR))#, msg="dsR2dgC_2")

## (dsRMatrix)
mtxt <- c("10  0  1  0  3",
          "0  10  0  1  0",
          "1  0  10  0  1",
          "0  1  0  10  0",
          "3  0  1  0  10")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dsc <- Matrix(M, sparse=TRUE)
dgc <- methods::as(dsc, "dgCMatrix")
dsr <- methods::as(dsc, "RsparseMatrix")
expect_equal(dgc, asSpMat(dsr))#, msg="dsR2dgC_3")

dsc <- t(dsc)
dgc <- methods::as(dsc, "dgCMatrix")
dsr <- methods::as(dsc, "RsparseMatrix")
expect_equal(dgc, asSpMat(dsr))#, msg="dsR2dgC_4")


#test.as.p2dgc <- function() {
## [Matrix] p74 (pMatrix)
p1 <- as(c(2,3,1), "pMatrix")
mtxt <- c("0 1 0",
          "0 0 1",
          "1 0 0")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(p1))#, msg="p2dgC_1")

## [Matrix] p74 (pMatrix)
I2 <- as(7:1, "pMatrix")
dgc <- as(as(I2, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(I2))#, msg="p2dgC_2")

## [Matrix] p116 (pMatrix)
pm1 <- as(as.integer(c(2,3,1)), "pMatrix")
dgc <- as(as(pm1, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(pm1))#, msg="p2dgC_3")

pm2 <- t(pm1)
dgc <- as(as(pm2, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(pm2))#, msg="p2dgC_4")

## [Matrix] p116 (pMatrix)
set.seed(11)
p10 <- as(sample(10),"pMatrix")
dgc <- as(as(p10, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(p10))#, msg="p2dgC_5")


#test.as.ddi2dgc <- function() {
## [Matrix] p35 (ddiMatrix)
d2 <- Diagonal(x = c(10, 1))
mtxt <- c("10 0",
          "0 1")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(d2))#, msg="ddi2dgC_1")

cd2 <- chol(d2)
dgc <- as(chol(dgc), "dgCMatrix")
expect_equal(dgc, asSpMat(cd2))#, msg="ddi2dgC_2")

                                        # [Matrix] p42 (ddiMatrix)
mtxt <- c("1 0 0",
          "0 1 0",
          "0 0 1")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(Diagonal(3)))#, msg="ddi2dgC_3")

mtxt <- c("1000 0   0",
          "0    100 0",
          "0    0   10")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(Diagonal(x = 10^(3:1))))#, msg="ddi2dgC_4")

## [Matrix] p43 (ddiMatrix)
I5 <- Diagonal(5)
mtxt <- c("1 0 0 0 0",
          "0 1 0 0 0",
          "0 0 1 0 0",
          "0 0 0 1 0",
          "0 0 0 0 1")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(I5))#, msg="ddi2dgC_5")

D5 <- Diagonal(x = 10*(1:5))
mtxt <- c("10 0 0 0 0",
          "0 20 0 0 0",
          "0 0 30 0 0",
          "0 0 0 40 0",
          "0 0 0 0 50")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(D5))#, msg="ddi2dgC_6")

D6 <- Diagonal(x = c(10, 20, 40, 50))
mtxt <- c("0.1 0    0     0",
          "0   0.05 0     0",
          "0   0    0.025 0",
          "0   0    0     0.02")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(solve(D6)))#, msg="ddi2dgC_7")

## [Matrix] p85 (ddiMatrix)
lM <- Diagonal(x = c(TRUE,FALSE,FALSE))
ddi <- crossprod(lM)
mtxt <- c("1 0 0",
          "0 0 0",
          "0 0 0")
M <- as.matrix(read.table(text=mtxt))
dimnames(M) <- NULL
dgc <- as(M, "dgCMatrix")
expect_equal(dgc, asSpMat(ddi))#, msg="ddi2dgC_8")


#test.as.ind2dgc <- function() {
## [Matrix] p74 (indMatrix)
sm1 <- as(rep(c(2,3,1), e=3), "indMatrix")
dgc <- as(as(sm1, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(sm1))#, msg="ind2dgc_1")

set.seed(27)
s10 <- as(sample(10, 30, replace=TRUE),"indMatrix")
dgc <- as(as(s10, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(s10))#, msg="ind2dgc_2")

set.seed(27)
IM1 <- as(sample(1:20, 100, replace=TRUE), "indMatrix")
dgc <- as(as(IM1, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(IM1))#, msg="ind2dgc_3")

set.seed(27)
IM2 <- as(sample(1:18, 100, replace=TRUE), "indMatrix")
dgc <- as(as(IM2, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(IM2))#, msg="ind2dgc_4")

## [Matrix] p74 (indMatrix)
ind <- as(2:4, "indMatrix")
dgc <- as(as(ind, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(ind))#, msg="ind2dgc_5")

ind <- as(list(2:4, 5), "indMatrix")
dgc <- as(as(ind, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(ind))#, msg="ind2dgc_6")

## [Matrix] p74 (indMatrix)
ind <- s10[1:4, ]
dgc <- as(as(ind, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(ind))#, msg="ind2dgC_7")

I1 <- as(c(5:1,6:4,7:3), "indMatrix")
dgc <- as(as(I1, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(I1))#, msg="ind2dgC_8")

## [Matrix] p116 (indMatrix)
set.seed(11)
p10 <- as(sample(10),"pMatrix")
ind <- p10[1:3, ]
dgc <- as(as(ind, "matrix"), "dgCMatrix")
expect_equal(dgc, asSpMat(ind))#, msg="ind2dgC_9")

if (suppressMessages(require(slam))) {
    ## simple_triplet_matrix from package slam
    test.as.stm2dgc <- function() {
        stm <- as.simple_triplet_matrix(diag(2))
        dgc <- as(as(stm, "matrix"), "dgCMatrix")
        expect_equal(dgc, asSpMat(stm))#, msg="stm2dgc")
    }
    test.as.stm2stm <- function() {
        stm <- as.simple_triplet_matrix(diag(2))
        expect_equal(stm, asStm(stm))#, msg="stm2stm")
    }
}

## this fails persistently at CRAN on the Fedora box, but shouldn't
## giving up and commenting out
## https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-fedora-clang/RcppArmadillo-00check.html
## test.stop <- function() {
##     ## [Matrix] p87 (lgCMatrix)
##     m <- Matrix(c(0,0,2:0), 3,5, dimnames=list(LETTERS[1:3],NULL))
##     lm <- (m > 1)
##     checkException(asSpMat(lm))

##     # [Matrix] p152 (lgTMatrix)
##     L <- spMatrix(9, 30, i = rep(1:9, 3), 1:27,
##                   (1:27) %% 4 != 1)
##     checkException(asSpMat(L))

##     ## [Matrix] p111 (ngCMatrix)
##     m <- Matrix(c(0,0,2:0), 3,5, dimnames=list(LETTERS[1:3],NULL))
##     dimnames(m) <- NULL
##     nm <- as(m, "nsparseMatrix")
##     checkException(asSpMat(nm))

##     ## [Matrix] p74 (ngTMatrix)
##     sm1 <- as(rep(c(2,3,1), e=3), "indMatrix")
##     ngt <- as(sm1, "ngTMatrix")
##     checkException(asSpMat(ngt))

##     ## [Matrix] p85 (ntTMatrix)
##     lM <- Diagonal(x = c(TRUE,FALSE,FALSE))
##     nM <- as(lM, "nMatrix")
##     checkException(asSpMat(nM))

##     ## [Matrix] p85 (nsCMatrix)
##     nsc <- crossprod(nM)
##     checkException(asSpMat(nsc))

##     ## [Matrix] p42 (ldiMatrix)
##     ldi <- Diagonal(x = (1:4) >= 2)
##     checkException(asSpMat(ldi))
## }

## test.as.lgc2dgc <- function() {
##     ## [Matrix] p87 (lgCMatrix) (To be continued)
##     lm <- (m > 1)
##
##     ## [Matrix] p111 (lgCMatrix) (To be continued)
##     m <- Matrix(c(0,0,2:0), 3,5, dimnames=list(LETTERS[1:3],NULL))
##     dimnames(m) <- NULL
##     nm <- as(m, "nsparseMatrix")
##     nnm <- !nm     # no longer sparse
##     nnm <- as(nnm, "sparseMatrix")
## }
##
## test.as.lgt2dgc <- function() {
##     ## [Matrix] p152 (lgTMatrix) (To be continued)
##     L <- spMatrix(9, 30, i = rep(1:9, 3), 1:27,
##                   (1:27) %% 4 != 1)
## }
##
## test.as.ngc2dgc <- function() {
##     ## [Matrix] p111 (ngCMatrix) (To be continued)
##     m <- Matrix(c(0,0,2:0), 3,5, dimnames=list(LETTERS[1:3],NULL))
##     dimnames(m) <- NULL
##     nm <- as(m, "nsparseMatrix")
##
##     ## [Matrix] p129 (ngCMatrix) (To be continued)
##     n7 <- rsparsematrix(5, 12, nnz = 10, rand.x = NULL)
## }
##
## test.as.ngt2dgc <- function() {
##     # [Matrix] p74 (ngTMatrix) (To be continued)
##     sm1 <- as(rep(c(2,3,1), e=3), "indMatrix")
##     ngt <- as(sm1, "ngTMatrix")
##     mtxt <- c("0 1 0",
##               "0 1 0",
##               "0 1 0",
##               "0 0 1",
##               "0 0 1",
##               "0 0 1",
##               "1 0 0",
##               "1 0 0",
##               "1 0 0")
##     M <- as.matrix(read.table(text=mtxt))
##     dimnames(M) <- NULL
##     dgc <- as(M, "dgCMatrix")
##     expect_equal(dgc, asSpMat(ngt))#, msg="ngT2dgC")
##
##     set.seed(27)
##     s10 <- as(sample(10, 30, replace=TRUE),"indMatrix")
##     ngt <- s10[1:7, 1:4]
##     mtxt <- c("0 0 0 0",
##               "1 0 0 0",
##               "0 0 0 0",
##               "0 0 0 1",
##               "0 0 1 0",
##               "0 0 0 0",
##               "1 0 0 0")
##     M <- as.matrix(read.table(text=mtxt))
##     dimnames(M) <- NULL
##     dgc <- as(M, "dgCMatrix")
##     expect_equal(dgc, asSpMat(ngt))#, msg="ngT2dgC")
##
##     ## [Matrix] p116 (ngTMatrix) (To be continued)
##     pm1 <- as(as.integer(c(2,3,1)), "pMatrix")
##     as(pm1, "ngTMatrix")
##     set.seed(11)
##     p10 <- as(sample(10),"pMatrix")
##     p10[1:7, 1:4]
## }
##
## test.as.ntt2dgc <- function() {
##     ## [Matrix] p85 (ntTMatrix) (To be continued)
##     lM <- Diagonal(x = c(TRUE,FALSE,FALSE))
##     nM <- as(lM, "nMatrix")
##     expect_equal(dgc, asSpMat(nM))#, msg="ntT2dgC")
## }
##
## test.as.nsc2dgc <- function() {
##     ## [Matrix] p85 (nsCMatrix) (To be continued)
##     lM <- Diagonal(x = c(TRUE,FALSE,FALSE))
##     nM <- as(lM, "nMatrix")
##     nsc <- crossprod(nM)
##     expect_equal(dgc, asSpMat(nsc))#, msg="nsC2dgC")
## }
##
## test.as.ldi2dgc <- function() {
##     ## [Matrix] p42 (ldiMatrix) (To be continued)
##     Diagonal(x = (1:4) >= 2)
##
##     ## [Matrix] p85 (ldiMatrix) (To be continued)
##     lM <- Diagonal(x = c(TRUE,FALSE,FALSE))
## }
