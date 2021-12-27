// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// complex.cpp: RcppArmadillo unit tests for complex vectors and matrices
//
// Copyright (C) 2013 - 2019  Baptiste Auguie and Dirk Eddelbuettel
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

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List complexCppTests(const arma::mat& A,
                     const arma::mat& B,
                     const arma::cx_colvec& V,
                     const arma::mat& S) {

    arma::cx_mat C(A, B); 				// create complex matrix
    arma::cx_mat Cst = strans(C), Ct = trans(C);	// transpose
    arma::cx_mat conjC = conj(C); 			// conjugate
    arma::mat absC = abs(C); 				// modulus

    arma::cx_colvec CtV = C * V; 			// multiply matrix-vector
    arma::cx_mat CtS = C * S; 				// multiply matrix-matrix
    arma::cx_mat CC = C % C; 				// element-wise multiplication
    arma::cx_mat CdC = C / C; 				// division
    arma::cx_mat CpC = C + C; 				// addition
    arma::cx_mat CmC = C - C; 				// subtraction

    return List::create(_["C"]  = C,
                        _["Cst"]  = Cst,
                        _["Ct"]  = Ct,
                        _["conjC"]  = conjC,
                        _["absC"]  = absC,
                        _["CV"]  = CtV,
                        _["CS"]  = CtS,
                        _["CC"]  = CC,
                        _["CdC"]  = CdC,
                        _["CpC"]  = CpC,
                        _["CmC"]  = CmC
                        );
}
