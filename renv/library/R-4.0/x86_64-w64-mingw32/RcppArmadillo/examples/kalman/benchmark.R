
suppressMessages(library(utils))
suppressMessages(library(RcppArmadillo))
suppressMessages(library(rbenchmark))
suppressMessages(library(compiler))

source("FirstKalmanR.R")
source("KalmanR.R")
source("KalmanRimp.R")
Rcpp::sourceCpp("Kalman.cpp")

FirstKalmanRC <- cmpfun(FirstKalmanR)
KalmanRC <- cmpfun(KalmanR)
KalmanRimpC <- cmpfun(KalmanRimp)

## Read data, ensure identical results
pos <- as.matrix(read.table("pos.txt", header=FALSE,
                            col.names=c("x","y")))
stopifnot(all.equal(KalmanR(pos), KalmanRC(pos)),
          all.equal(KalmanRC(pos), KalmanCpp(pos)),
          all.equal(KalmanCpp(pos), FirstKalmanRC(pos)),
          all.equal(KalmanCpp(pos), FirstKalmanR(pos)),
          all.equal(KalmanCpp(pos), KalmanRimp(pos)),
          all.equal(KalmanCpp(pos), KalmanRimpC(pos)))

res <- benchmark(KalmanR(pos), KalmanRC(pos),
                 KalmanRimp(pos), KalmanRimpC(pos),
                 FirstKalmanR(pos), FirstKalmanRC(pos),
                 KalmanCpp(pos),
                 columns = c("test", "replications",
                             "elapsed", "relative"),
                 order="relative",
                 replications=500)

print(res[,1:4])
