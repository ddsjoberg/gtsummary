// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// Rlapack.cpp: RcppArmadillo unit tests for borked Lapack
//
// Copyright (C) 2018 Keith O'Hara and Dirk Eddelbuettel
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

// #define ARMA_EXTRA_DEBUG

#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::cx_mat cx_eig_pair_test(const int n)
{
    arma::cx_mat A = arma::randu<arma::cx_mat>(n,n);
    arma::cx_mat B = arma::randu<arma::cx_mat>(n,n);

    arma::cx_vec eigval;
    arma::cx_mat eigvec;

    arma::eig_pair(eigval, eigvec, A, B);

    return A*eigvec - B*eigvec*arma::diagmat(eigval);
}

// [[Rcpp::export]]
arma::cx_mat cx_qz_test(const int n)
{
    // test qz in complex matrix case
    arma::cx_mat A = arma::randu<arma::cx_mat>(n,n);
    arma::cx_mat B = arma::randu<arma::cx_mat>(n,n);

    arma::cx_mat AA;
    arma::cx_mat BB;
    arma::cx_mat Q;
    arma::cx_mat Z;

    arma::qz(AA,BB,Q,Z,A,B);

    return A - Q.t()*AA*Z.t();
}

// [[Rcpp::export]]
int cx_rank_test(const int n)
{
    // test svd_dc
    arma::cx_mat A = arma::randu<arma::cx_mat>(n,n+1);
    
    int rA = arma::rank(A);

    return rA;
}

// [[Rcpp::export]]
arma::cx_mat cx_pinv_test(const int n)
{
    // test svd_dc
    arma::cx_mat A = arma::randu<arma::cx_mat>(n,n+1);
    
    arma::cx_mat B = arma::pinv(A);

    return A*B;
}

// [[Rcpp::export]]
arma::cx_mat cx_schur_test(const int n)
{
    arma::cx_mat A = arma::randu<arma::cx_mat>(n,n);
    arma::cx_mat U;
    arma::cx_mat S;

    arma::schur(U,S,A);

    return A - U*S*U.t();
}

// [[Rcpp::export]]
arma::cx_mat cx_solve_test(const int n)
{
    arma::cx_mat A = arma::randu<arma::cx_mat>(n,n);
    arma::cx_vec b = arma::randu<arma::cx_vec>(n);
    arma::cx_mat B = arma::randu<arma::cx_mat>(n,n);

    arma::cx_vec x1 = solve(A, b);

    arma::cx_vec x2;
    solve(x2, A, b);

    arma::cx_mat X1 = solve(A, B);
    arma::cx_mat X2 = solve(A, B, arma::solve_opts::fast);  // enable fast mode

    // next for non-square matrices; to test solve_approx_svd

    arma::cx_mat C = arma::randu<arma::cx_mat>(n,n+1);
    
    arma::cx_vec x3 = solve(C, b);

    return C*x3 - b;
}

// [[Rcpp::export]]
arma::cx_mat cx_solve_band_test(const int n)
{
    // trigger solve_tridiag_refine

    int n_tri_rows = std::min(34,n);
    arma::cx_mat A_tri = arma::zeros<arma::cx_mat>(n_tri_rows,n_tri_rows);

    A_tri.diag() = arma::randu<arma::cx_mat>(n_tri_rows,1);
    A_tri.diag(1) = arma::randu<arma::cx_mat>(n_tri_rows-1,1);
    A_tri.diag(-1) = arma::randu<arma::cx_mat>(n_tri_rows-1,1);

    arma::cx_vec b_tri = arma::randu<arma::cx_vec>(n_tri_rows);
    arma::cx_vec x_tri = solve(A_tri, b_tri);

    return A_tri*x_tri - b_tri;
}
