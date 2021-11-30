// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/rmultinom.h>
#include <RcppArmadilloExtensions/fixprob.h>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rmultinomC(int n, int size, NumericVector prob) {
    IntegerMatrix draws(prob.size(), n);
    // FixProb modifies in-place
    arma::colvec fixprob(prob.begin(), prob.size()); // forced copy
    RcppArmadillo::FixProb(fixprob, 1, true);
    NumericVector newprob(Rcpp::wrap(fixprob));
    RNGScope scope;
    for (int ii=0; ii<n; ii++) {
        draws(_, ii) = RcppArmadillo::rmultinom(size, newprob);
    }
    return draws;
}
