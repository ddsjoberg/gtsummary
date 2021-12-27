#!/usr/bin/r -t
#
##  Copyright (C) 2012 - 2019  Christian Gunning
##  Copyright (C) 2013 - 2019  Romain Francois
##  Copyright (C) 2019         Dirk Eddelbuettel
##
##
##  This file is part of RcppArmadillo.
##
##  RcppArmadillo is free software: you can redistribute it and/or modify it
##  under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 2 of the License, or
##  (at your option) any later version.
##
##  RcppArmadillo is distributed in the hope that it will be useful, but
##  WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

library(RcppArmadillo)

Rcpp::sourceCpp("cpp/sample.cpp")

#test.sample <- function() {
## set up S3 dispatching,
## simplifies lapply(tests, ...) below
csample <- function(x, ...) UseMethod("csample")
csample.numeric <- csample_numeric
csample.integer <- csample_integer
csample.complex <- csample_complex
csample.character <- csample_character
csample.logical <- csample_logical

## Seed needs to be reset to compare R to C++
seed <- 441
## Input vectors to sample
N <- 100
## Number of samples
## works for size == N?!
size <- N%/%2

## all atomic vector classes except raw
## for each list element, check that sampling works
## with and without replacement, with and without prob
tests <- list()
tests <- within(tests, {
    int <- 1:N
    num <- (1:N)/10
    cpx <- (1:N)/10 + 1i
    char <-rep(letters, 4)[1:N]
    bool <- rep(c(T,F), times=N/2)
})

## Un-normalized probs
probs <- seq(from=0, to=1, length.out=N)
##probs <- probs/sum(probs)

## Needed for a change in R 3.6.0 reducing a bias in very large samples
suppressWarnings(RNGversion("3.5.0"))

## Run the S3 generic function csample
## and associated R function on each data type
## ReplaceYN.ProbsYN
lapply(tests, function(dat) {
    .class <- class(dat)
    set.seed(seed)
    ## R
    r.no.no <- sample(dat, size, replace=F)
    set.seed(seed)
    r.yes.no <- sample(dat, size, replace=T)
    set.seed(seed)
    r.no.yes <- sample(dat, size, replace=F, prob=probs)
    set.seed(seed)
    r.yes.yes <- sample(dat, size, replace=T, prob=probs)
    ## C
    set.seed(seed)
    c.no.no <- csample(dat, size, replace=F)
    set.seed(seed)
    c.yes.no <- csample(dat, size, replace=T)
    set.seed(seed)
    c.no.yes <- csample(dat, size, replace=F, prob=probs)
    set.seed(seed)
    c.yes.yes <- csample(dat, size, replace=T, prob=probs)
    ## comparisons
    expect_equal(r.no.no, c.no.no)#, msg=sprintf("sample.%s.no_replace.no_prob",.class))
    expect_equal(r.yes.no, c.yes.no)#, msg=sprintf("sample.%s.replace.no_prob",.class))
    ## the following don't pass
    expect_equal(r.no.yes, c.no.yes)#, msg=sprintf("sample.%s.no_replace.prob",.class))
    expect_equal(r.yes.yes, c.yes.yes)#, msg=sprintf("sample.%s.replace.prob",.class))
})

## Walker Alias method test
## With replacement, >200 "nonzero" probabilities
## Not implemented, see below
walker.N <- 1e3
walker.sample <- (1:walker.N)/10
walker.probs <- rep(0.1, walker.N)
## uncomment following 5 lines if/when walker alias method is implemented
set.seed(seed)
r.walker <- sample( walker.sample, walker.N, replace=T, prob=walker.probs)
set.seed(seed)
c.walker <- csample( walker.sample, walker.N, replace=T, prob=walker.probs)
expect_equal(r.walker, c.walker)#, msg=sprintf("Walker Alias method test"))
## Walker Alias method is not implemented.
## For this problem (replace, >200 non-zero probs) R is much faster
## So throw an error and refuse to proceed
##walker.error <- try( csample( walker.sample, walker.N, replace=T, prob=walker.probs), TRUE)
##expect_equal(inherits(walker.error, "try-error"), TRUE, msg=sprintf("Walker Alias method test"))
