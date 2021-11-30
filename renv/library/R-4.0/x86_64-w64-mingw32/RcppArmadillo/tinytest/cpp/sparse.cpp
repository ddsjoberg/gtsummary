// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// sparse.cpp: RcppArmadillo unit test code for sparse matrices 
//
// Copyright (C) 2014  Dirk Eddelbuettel
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
arma::sp_mat asSpMat(SEXP S) {
    return Rcpp::as<arma::sp_mat>(S);
}
// [[Rcpp::export]]
Rcpp::List asStm(SEXP S) {
    return Rcpp::simple_triplet_matrix(Rcpp::as<arma::sp_mat>(S));
}

// [[Rcpp::export]]
arma::sp_mat sparseAddition(arma::sp_mat SM) {
    return SM + SM;
}

// [[Rcpp::export]]
arma::sp_mat sparseMultiplication(arma::sp_mat SM, int k) {
    return k * SM;
}

// [[Rcpp::export]]
arma::sp_mat fromTriplet(arma::urowvec ri, arma::urowvec ci, arma::colvec values) {
    arma::umat loc = arma::join_vert(ri, ci);// form 2*N 'locations' matrix
    arma::sp_mat sp(loc, values);            // create sparse from locations and values
    return sp;
}

// [[Rcpp::export]]
arma::sp_mat sparseTranspose(arma::sp_mat SM) {
    return SM.t();
}

// [[Rcpp::export]]
arma::sp_mat sparseSqrt(arma::sp_mat SM) {
    return arma::sqrt(SM);
}

// [[Rcpp::export]]
arma::sp_mat sparseSquare(arma::sp_mat SM) {
    return arma::square(SM);
}

// [[Rcpp::export]]
arma::sp_mat sparseIterators(arma::sp_mat SM, double val) {
    arma::sp_mat::iterator begin = SM.begin();
    arma::sp_mat::iterator end   = SM.end();
    
    for (arma::sp_mat::iterator it = begin; it != end; ++it)
      (*it) += val;
    
    return SM;
}

// [[Rcpp::export]]
Rcpp::List sparseList(Rcpp::List l) {
    arma::sp_mat mat1 = l[0];
    arma::sp_mat mat2 = l[0];
    
    return Rcpp::List::create(mat1, mat2);
}

// [[Rcpp::export]]
arma::sp_mat speye(int nrow, int ncol) {
    return arma::speye(nrow, ncol);
}
