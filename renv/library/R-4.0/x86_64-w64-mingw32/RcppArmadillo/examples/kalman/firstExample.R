
library(RcppArmadillo)

cppFunction(code='
Rcpp::List g(arma::colvec v) {
     arma::mat op = v * v.t();
     double ip = arma::as_scalar(v.t() * v);
     return Rcpp::List::create(Rcpp::Named("outer")=op,
                               Rcpp::Named("inner")=ip);
}
', depends="RcppArmadillo")

g(7:11)
