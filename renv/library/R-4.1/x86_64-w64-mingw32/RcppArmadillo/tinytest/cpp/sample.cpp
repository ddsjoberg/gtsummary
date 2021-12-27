// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// sample.cpp: RcppArmadillo unit test code for sample() function
//
// Copyright (C) 2012 - 2013  Christian Gunning and Dirk Eddelbuettel
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

#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

// function defns exported to R -- instantiate templated sample
// functions are identical except for class of sampled vector and return val

// [[Rcpp::export]]
IntegerVector csample_integer( IntegerVector x, int size, bool replace, 
			       NumericVector prob = NumericVector::create()) {
    RNGScope scope;
    IntegerVector ret = RcppArmadillo::sample(x, size, replace, prob);
    return ret;
}

// [[Rcpp::export]]
NumericVector csample_numeric( NumericVector x, int size, bool replace, 
			       NumericVector prob = NumericVector::create()) {
    RNGScope scope;
    NumericVector ret = RcppArmadillo::sample(x, size, replace, prob);
    return ret;
}

// [[Rcpp::export]]
ComplexVector csample_complex( ComplexVector x, int size, bool replace, 
			       NumericVector prob = NumericVector::create()) {
    RNGScope scope;
    ComplexVector ret = RcppArmadillo::sample(x, size, replace, prob);
    return ret;
}
 
// [[Rcpp::export]]
CharacterVector csample_character( CharacterVector x, int size, bool replace, 
				   NumericVector prob = NumericVector::create()) {
    RNGScope scope;
    CharacterVector ret = RcppArmadillo::sample(x, size, replace, prob);
    return ret;
}

// [[Rcpp::export]]
LogicalVector csample_logical( LogicalVector x, int size, bool replace, 
			       NumericVector prob = NumericVector::create()) {
    RNGScope scope;
    LogicalVector ret = RcppArmadillo::sample(x, size, replace, prob);
    return ret;
}
