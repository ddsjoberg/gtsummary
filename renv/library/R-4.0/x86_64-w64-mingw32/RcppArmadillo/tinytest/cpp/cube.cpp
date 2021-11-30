// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// cube.cpp: RcppArmadillo unit test code for cube types
//
// Copyright (C) 2015 - 2019  Dirk Eddelbuettel and Nathan Russell
// Copyright (C) 2019         Dirk Eddelbuettel
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
arma::cube cube_test(const arma::cube& x) {
    return arma::pow(x, 2);
}

// [[Rcpp::export]]
arma::fcube fcube_test(const arma::fcube& x) {
    return arma::pow(x, 2);
}

// [[Rcpp::export]]
arma::icube icube_test(const arma::icube& x) {
    return arma::pow(x, 2);
}

// [[Rcpp::export]]
arma::ucube ucube_test(const arma::ucube& x) {
    return arma::pow(x, 2);
}

// [[Rcpp::export]]
arma::cx_cube cx_cube_test(const arma::cx_cube& x) {
    return arma::pow(x, 2);
}

// [[Rcpp::export]]
arma::cx_fcube cx_fcube_test(const arma::cx_fcube& x) {
    return arma::pow(x, 2);
}

// [[Rcpp::export]]
arma::cube as_cube(Rcpp::NumericVector x) {
    arma::cube y = Rcpp::as<arma::cube>(x);
    return arma::pow(y, 2);
}

// [[Rcpp::export]]
arma::fcube as_fcube(Rcpp::NumericVector x) {
    arma::fcube y = Rcpp::as<arma::fcube>(x);
    return arma::pow(y, 2);
}

// [[Rcpp::export]]
arma::icube as_icube(Rcpp::IntegerVector x) {
    arma::icube y = Rcpp::as<arma::icube>(x);
    return arma::pow(y, 2);
}

// [[Rcpp::export]]
arma::ucube as_ucube(Rcpp::IntegerVector x) {
    arma::ucube y = Rcpp::as<arma::ucube>(x);
    return arma::pow(y, 2);
}

// [[Rcpp::export]]
arma::cx_cube as_cx_cube(Rcpp::ComplexVector x) {
    arma::cx_cube y = Rcpp::as<arma::cx_cube>(x);
    return arma::pow(y, 2);
}

// [[Rcpp::export]]
arma::cx_fcube as_cx_fcube(Rcpp::ComplexVector x) {
    arma::cx_fcube y = Rcpp::as<arma::cx_fcube>(x);
    return arma::pow(y, 2);
}
