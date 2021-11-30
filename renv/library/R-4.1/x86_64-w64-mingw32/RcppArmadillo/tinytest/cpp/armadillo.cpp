// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// armadillo.cpp: RcppArmadillo unit test code
//
// Copyright (C) 2010 - 2019  Dirk Eddelbuettel, Romain Francois and Douglas Bates
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

using namespace Rcpp;

// [[Rcpp::export]]
List wrap_() {

    // using the Named(.) = . notation
    List cols = List::create(Named( "Col<double>" ) = arma::zeros<arma::mat>(5,1),
                             Named( "Col<float>" )  = arma::zeros<arma::fmat>(5,1));

    // using the Named(., .)  notation
    List rows = List::create(Named( "Row<double>",  arma::zeros<arma::mat>(1,5)  ),
                             Named( "Row<float>" ,  arma::zeros<arma::fmat>(1,5) ));

    // using the _[.] = . notation
    List matrices = List::create(_["Mat<int>"]          = arma::eye<arma::imat>( 3,3 ),
                                 _["Mat<double>"]       = arma::eye<arma::mat>( 3,3 ),
                                 _["Mat<float>"]        = arma::eye<arma::fmat>( 3, 3 ),
                                 _["Mat<unsigned int>"] = arma::eye<arma::umat>( 3, 3 ));

    // creating an empty list and grow it on demand
    List fields;
    arma::field<int> f1( 2, 2 );
    f1( 0, 0 ) = 0;
    f1( 1, 0 ) = 1;
    f1( 0, 1 ) = 2;
    f1( 1, 1 ) = 3;
    fields["field<int>"] = f1;

    arma::field<std::string> f2(2,2);
    f2( 0, 0 ) = "a";
    f2( 1, 0 ) = "b";
    f2( 0, 1 ) = "c";
    f2( 1, 1 ) = "d";
    fields["field<std::string>"] = f2;

    arma::field<arma::colvec> f3(2,2);
    f3(0,0) = arma::zeros<arma::mat>(5,1);
    f3(1,0) = arma::zeros<arma::mat>(4,1);
    f3(0,1) = arma::zeros<arma::mat>(3,1);
    f3(1,1) = arma::zeros<arma::mat>(2,1);
    fields["field<colvec>"] = f3;

    List output = List::create(_["matrices : Mat<T>"]  = matrices,
                               _["rows : Row<T>"]      = rows,
                               _["columns : Col<T>"]   = cols,
                               _["fields  : field<T>"] = fields );

    return output;
}

// [[Rcpp::export]]
List wrapGlue_() {
    arma::mat m1 = arma::eye<arma::mat>( 3, 3 );
    arma::mat m2 = arma::eye<arma::mat>( 3, 3 );

    List res;
    res["mat+mat"] = m1 + m2;
    return res;
}

// [[Rcpp::export]]
List wrapOp_() {
    arma::mat m1 = arma::eye<arma::mat>( 3, 3 );

    List res;
    res["- mat"] = - m1;
    return res;
}

// [[Rcpp::export]]
List asMat_(List input) {
    arma::imat m1 = input[0]; /* implicit as */
    arma::mat  m2 = input[1]; /* implicit as */
    arma::umat m3 = input[0]; /* implicit as */
    arma::fmat m4 = input[1]; /* implicit as */

    List res = List::create(arma::accu( m1 ),
                            arma::accu( m2 ),
                            arma::accu( m3 ),
                            arma::accu( m4 ) );

    return res;
}

// [[Rcpp::export]]
List asCol_(List input) {
    arma::icolvec m1 = input[0]; /* implicit as */
    arma::colvec  m2 = input[1]; /* implicit as */
    arma::ucolvec m3 = input[0]; /* implicit as */
    arma::fcolvec m4 = input[1]; /* implicit as */

    List res = List::create(arma::accu( m1 ),
                            arma::accu( m2 ),
                            arma::accu( m3 ),
                            arma::accu( m4 ) );
    return res;
}

// [[Rcpp::export]]
List asRow_(List input) {
    arma::irowvec m1 = input[0]; /* implicit as */
    arma::rowvec  m2 = input[1]; /* implicit as */
    arma::urowvec m3 = input[0]; /* implicit as */
    arma::frowvec m4 = input[1]; /* implicit as */

    List res = List::create(arma::accu( m1 ),
                            arma::accu( m2 ),
                            arma::accu( m3 ),
                            arma::accu( m4 ) );
    return res;
}

// [[Rcpp::export]]
List cxMat_() {
    arma::cx_mat m1  = arma::eye<arma::cx_mat> ( 3, 3 );
    arma::cx_fmat m2 = arma::eye<arma::cx_fmat>( 3, 3 );
    return List::create( _["double"] = m1, _["float"] = m2 );
}

// [[Rcpp::export]]
ComplexMatrix mtOp_() {
    std::complex<double> x( 1.0, 2.0 );
    arma::mat m1  = arma::eye<arma::mat> ( 3, 3 );

    return wrap( x * m1 );
}

// [[Rcpp::export]]
NumericMatrix mtGlue_() {
    arma::imat m2 = arma::eye<arma::imat> ( 3, 3 );
    arma::mat m1  = arma::eye<arma::mat> ( 3, 3 );

    return wrap( m1 + m2 );
}

// [[Rcpp::export]]
NumericMatrix sugar_(NumericVector xx) {
    arma::mat m = xx + xx;
    return wrap( m );
}

// [[Rcpp::export]]
ComplexMatrix sugarCplx_(ComplexVector xx) {
    arma::cx_mat m = exp( xx );
    return wrap( m );
}

// [[Rcpp::export]]
List sugarCtor_(NumericVector xx) {
    arma::mat m = xx + xx;
    arma::colvec co = xx;
    arma::rowvec ro = xx;
    return List::create(_["mat"] = m + m,
                        _["rowvec"] = ro,
                        _["colvec"] = co);
}

double norm( double x, double y){
    return ::sqrt( x*x + y*y );
}

// [[Rcpp::export]]
List sugarMatrixCtor_(NumericVector xx) {
    NumericVector yy = NumericVector::create( 1 );
    arma::mat m = diag( xx );
    arma::colvec co = outer( xx, yy, ::norm );
    arma::rowvec ro = outer( yy, xx, ::norm );
    return List::create(_["mat"] = m + m ,
                        _["rowvec"] = ro,
                        _["colvec"] = co);
}

// test.armadillo.rtti.check <- function() {

//     inc <- '
//     void blah(arma::mat& X) {
//          X.set_size(5,5);
//     }
//     '
//     src <- '
//     arma::vec V;
//     blah(V); // if blah() worked, we have a problem
//     '
//     fun <- cxxfunction(signature(), body=src, inc=inc, plugin = "RcppArmadillo")

//     checkException(fun(), msg="RTTI check on matrix constructor exception")

// }


// [[Rcpp::export]]
int mat_plain(arma::mat x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int mat_const(const arma::mat x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int mat_ref(arma::mat & x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int mat_const_ref(const arma::mat & x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int vec_plain(arma::vec x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int vec_const(const arma::vec x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int vec_ref(arma::vec & x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int vec_const_ref(const arma::vec & x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int cx_mat_plain(arma::cx_mat x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int cx_mat_const(const arma::cx_mat x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int cx_mat_ref(arma::cx_mat & x) {
    return x.n_elem;
}

// [[Rcpp::export]]
int cx_mat_const_ref(const arma::cx_mat & x) {
    return x.n_elem;
}

// [[Rcpp::export]]
arma::uvec uvec_test(arma::uvec v) { return(v); }

// [[Rcpp::export]]
arma::uvec c_uvec_test(const arma::uvec v) { return(v); }

// [[Rcpp::export]]
arma::uvec r_uvec_test(arma::uvec& v) { return(v); }

// [[Rcpp::export]]
arma::uvec cr_uvec_test(const arma::uvec& v) { return(v); }

// [[Rcpp::export]]
arma::umat umat_test(arma::umat v) { return(v); }

// [[Rcpp::export]]
arma::umat c_umat_test(const arma::umat v) { return(v); }

// [[Rcpp::export]]
arma::umat r_umat_test(arma::umat& v) { return(v); }

// [[Rcpp::export]]
arma::umat cr_umat_test(const arma::umat& v) { return(v); }

// [[Rcpp::export]]
arma::vec vecc_test(arma::vec v) { return(v); }

// [[Rcpp::export]]
arma::rowvec vecr_test(arma::rowvec v) { return(v); }
