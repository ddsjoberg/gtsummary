// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// rng.cpp: RcppArmadillo unit test code for fallback RNG
//
// Copyright (C) 2014 - 2019  Dirk Eddelbuettel
//
// This file is part of RcppArmadillo.
//
// RcppArmadillo is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RcppArmadillo is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
int setSeed(int val) {
    arma::arma_rng::set_seed(val); // should trigger warning
    return 0;
}

// [[Rcpp::export]]
arma::vec randu(int n) {
    return arma::randu<arma::vec>(n);
}

// [[Rcpp::export]]
arma::ivec randi(int n) {
    return arma::randi<arma::ivec>(n);
}

// [[Rcpp::export]]
arma::vec randn(int n) {
    return arma::randn<arma::vec>(n);
}
