// [[Rcpp::depends(RcppArmadillo)]]
#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#define RCPP_ARMADILLO_RETURN_ROWVEC_AS_VECTOR
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec vecc_as_v_test(arma::vec v) { return(v); }

// [[Rcpp::export]]
arma::rowvec vecr_as_v_test(arma::rowvec v) { return(v); }
