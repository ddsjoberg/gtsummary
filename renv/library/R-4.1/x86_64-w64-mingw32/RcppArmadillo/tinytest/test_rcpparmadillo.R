#!/usr/bin/r -t
#
# Copyright (C) 2010 - 2019  Dirk Eddelbuettel, Romain Francois and Douglas Bates
# Copyright (C) 2019         Dirk Eddelbuettel
#
# This file is part of RcppArmadillo.
#
# RcppArmadillo is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# RcppArmadillo is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

library(RcppArmadillo)

Rcpp::sourceCpp("cpp/armadillo.cpp")

## test.wrap.R
fx <- wrap_
res <- fx()

expect_equal( res[[1]][[1]], matrix(as.integer((diag(3))),nr=3))#, msg = "eye<imat>(3,3)" )
expect_equal( res[[1]][[2]], diag(3))#, msg = "eye<mat>(3,3)" )
expect_equal( res[[1]][[3]], diag(3))#, msg = "eye<fmat>(3,3)" )
expect_equal( res[[1]][[4]], matrix(as.integer((diag(3))),nr=3))#, msg = "eye<umat>(3,3)" )

expect_equal( res[[2]][[1]], matrix(0, ncol = 5, nrow=1))#, msg = "zeros<mat>(5,1)" )
expect_equal( res[[2]][[2]], matrix(0, ncol = 5, nrow=1))#, msg = "zeros<fmat>(5,1)" )

expect_equal( res[[3]][[1]], matrix(0, ncol = 1, nrow=5))#, msg = "zeros<mat>(1,5)" )
expect_equal( res[[3]][[2]], matrix(0, ncol = 1, nrow=5))#, msg = "zeros<mat>(1,5)" )

expect_equal( res[[4]][[1]], matrix(0:3, ncol = 2, nrow=2))#, msg = "field<int>" )
expect_equal( res[[4]][[2]], matrix(letters[1:4], ncol = 2, nrow=2))#, msg = "field<std::string>" )


# test.wrap.Glue <- function(){
fx <- wrapGlue_
res <- fx()
expect_equal( res[[1]], 2*diag(3))#, msg = "wrap(Glue)" )


# test.wrap.Op <- function(){
fx <- wrapOp_
res <- fx()
expect_equal( res[[1]], -1*diag(3))#, msg = "wrap(Op)" )


# test.as.Mat <- function(){
fx <- asMat_
integer_mat <- matrix( as.integer(diag(4)), ncol = 4, nrow = 4 )
numeric_mat <- diag(5)
res <- fx( list( integer_mat, numeric_mat ) )
expect_equal( unlist( res), c(4L, 5L, 4L, 5L ))#, msg = "as<Mat>" )
#}

#test.as.Col <- function(){
fx <- asCol_
res <- fx( list( 1:10, as.numeric(1:10) ) )
expect_equal( unlist( res ), rep(55.0, 4 ))#, msg = "as<Col>" )
#}

#test.as.Row <- function(){
fx <- asRow_
res <- fx( list( 1:10, as.numeric(1:10) ) )
expect_equal( unlist( res ), rep(55.0, 4 ))#, msg = "as<Row>" )


#test.cxmat <- function(){
fx <- cxMat_
expect_equal(fx(), list(double=(1+0i)*diag(3), float=(1+0i)*diag(3))) #,msg = "support for complex matrices" )

#test.mtOp <- function(){
fx <- mtOp_
expect_equal(fx(), (1+2i)*diag(3))#, msg = "support for mtOp" )

#test.mtGlue <- function(){
fx <- mtGlue_
expect_equal(fx(), 2.0 * diag(3))# , msg = "support for mtGlue" )

#test.sugar <- function(){
fx <- sugar_
expect_equal(fx(1:10), matrix( 2*(1:10), nrow = 10 ))# , msg = "RcppArmadillo and sugar" )

#test.sugar.cplx <- function(){
fx <- sugarCplx_
x <- 1:10*(1+1i)
expect_equal(fx(x), matrix( exp(x), nrow = 10 ))# , msg = "RcppArmadillo and sugar (complex)" )

#test.armadillo.sugar.ctor <- function(){
fx <- sugarCtor_
expect_equal(fx(1:10), list(mat = matrix( 4*(1:10), nrow = 10 ),
                            rowvec = matrix( 1:10, nrow = 1 ),
                            colvec = matrix( 1:10, ncol = 1 ))) #,msg = "Mat( sugar expression )" )

#test.armadillo.sugar.matrix.ctor <- function(){
fx <- sugarMatrixCtor_
res <- fx(1:10)
norm <- function(x, y) sqrt( x*x + y*y )
expect_equal(res, list(mat = diag(2*(1:10)),
                       rowvec = outer( 1, 1:10, norm ),
                       colvec = outer( 1:10, 1, norm ))) #msg = "Mat( sugar expression )" )

## test.armadillo.rtti.check <- function() {

##     inc <- '
##     void blah(arma::mat& X) {
##          X.set_size(5,5);
##     }
##     '
##     src <- '
##     arma::vec V;
##     blah(V); // if blah() worked, we have a problem
##     '
##     fun <- cxxfunction(signature(), body=src, inc=inc, plugin = "RcppArmadillo")

##     checkException(fun(), msg="RTTI check on matrix constructor exception")

## }

#test.armadillo.mat.plain <- function() {
fx <- mat_plain
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Plain Matrix function signature" )

#test.armadillo.mat.const <- function() {
fx <- mat_const
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Const Matrix function signature" )

#test.armadillo.mat.ref <- function() {
fx <- mat_ref
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Reference Matrix function signature" )

#test.armadillo.mat.const.ref <- function() {
fx <- mat_const_ref
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Const Reference Matrix function signature" )

#test.armadillo.vec.plain <- function() {
fx <- vec_plain
m <- 1:9
expect_equal(fx(m), 9)#, msg = "Plain Vector function signature" )

#test.armadillo.vec.const <- function() {
fx <- vec_const
m <- 1:9
expect_equal(fx(m), 9)#, msg = "Const Vector function signature" )

#test.armadillo.vec.ref <- function() {
fx <- vec_ref
m <- 1:9
expect_equal(fx(m), 9)#, msg = "Reference Vector function signature" )

#test.armadillo.vec.const.ref <- function() {
fx <- vec_const_ref
m <- 1:9
expect_equal(fx(m), 9)#, msg = "Const Reference Vector function signature" )

#test.armadillo.mat.plain <- function() {
fx <- cx_mat_plain
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Plain Matrix function signature" )

#test.armadillo.mat.const <- function() {
fx <- cx_mat_const
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Const Matrix function signature" )

#test.armadillo.mat.ref <- function() {
fx <- cx_mat_ref
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Reference Matrix function signature" )

#test.armadillo.mat.const.ref <- function() {
fx <- cx_mat_const_ref
m <- matrix(1:9, 3, 3)
expect_equal(fx(m), 9)#, msg = "Const Reference Matrix function signature" )


Rcpp::sourceCpp("cpp/colrow_as_vec.cpp")

vec <- as.matrix(1:3)
expect_equal(vec, uvec_test(vec))
expect_equal(vec, c_uvec_test(vec))
expect_equal(vec, r_uvec_test(vec))
expect_equal(vec, cr_uvec_test(vec))

mat <- matrix(1:4, nrow=2)
expect_equal(mat, umat_test(mat))
expect_equal(mat, c_umat_test(mat))
expect_equal(mat, r_umat_test(mat))
expect_equal(mat, cr_umat_test(mat))


Rcpp::sourceCpp("cpp/any_as_vec.cpp")

vec <- 1:3
vecc <- as.matrix(1:3)
vecr <- t(vecc)

expect_equal(vecc, vecc_test(vec))#, msg="legacy vec")
expect_equal(vecr, vecr_test(vecr))#, msg="legacy rowvec")

expect_equal(vec, vecc_as_v_test(vec))#, msg="vec as vector")
expect_equal(vec, vecr_as_v_test(vec))#, msg="rowvec as vector")

expect_equal(vec, veccany_as_v_test(vec))#, msg="vec (by any) as vector")
expect_equal(vec, vecrany_as_v_test(vec))#, msg="rowvec (by any) as vector")
