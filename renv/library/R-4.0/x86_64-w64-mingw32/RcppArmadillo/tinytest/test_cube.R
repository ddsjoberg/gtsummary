#!/usr/bin/r -t
#
# Copyright (C) 2015 - 2021  Dirk Eddelbuettel and Nathan Russell
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

Rcpp::sourceCpp("cpp/cube.cpp")

.onWindows <- .Platform$OS.type == "windows"
critTol <- if (.onWindows) 1.0e-6 else 1.5e-7

## test arrays
dbl_cube <- array(1.5:27.5, rep(3, 3))
int_cube <- array(1L:27L, rep(3, 3))
cplx_cube <- array(1.5:27.5 + 2i, rep(3, 3))

## check cube (Cube<double>) and fcube (Cube<float>)
expect_equal(cube_test(dbl_cube), (dbl_cube ** 2)) #, "cube_test")
expect_equal(fcube_test(dbl_cube), (dbl_cube ** 2)) #, "fcube_test")

## check icube (Cube<sword>) and ucube (Cube<uword>)
expect_equal(icube_test(int_cube), (int_cube ** 2)) #, "icube_test")
expect_equal(ucube_test(int_cube), (int_cube ** 2)) #, "ucube_test")

## check cx_cube (Cube<cx_double>) and cx_fcube (Cube<cx_float>)
expect_equal(cx_cube_test(cplx_cube), (cplx_cube ** 2)) #, "cx_cube_test")
expect_equivalent(cx_fcube_test(cplx_cube), (cplx_cube ** 2), #"cx_fcube_test",
                  tolerance = critTol)


## test that exception is thrown with dims(x) != 3
dbl_cube <- array(1.5:16.5, rep(2, 4))
int_cube <- array(1L:16L, rep(2, 4))
cplx_cube <- array(1.5:16.5 + 2i, rep(2, 4))

## cube_test and fcube_test should throw here
expect_error(cube_test(dbl_cube))  #"cube_test bad dimensions")
expect_error(fcube_test(dbl_cube)) #"fcube_test bad dimensions")

## icube_test and ucube_test should throw here
expect_error(icube_test(int_cube)) #"icube_test bad dimensions")
expect_error(ucube_test(int_cube)) #"ucube_test bad dimensions")

## cx_cube_test and cx_fcube_test should throw here
expect_error(cx_cube_test(cplx_cube))  #"cx_cube_test bad dimensions")
expect_error(cx_fcube_test(cplx_cube)) #"cx_fcube_test bad dimensions")


## sanity check for explicit calls to Rcpp::as< arma::Cube<T> >
dbl_cube <- array(1.5:27.5, rep(3, 3))
int_cube <- array(1L:27L, rep(3, 3))
cplx_cube <- array(1.5:27.5 + 2i, rep(3, 3))

## check cube (Cube<double>) and fcube (Cube<float>)
expect_equal(as_cube(dbl_cube), (dbl_cube ** 2))#, "as_cube")
expect_equal(as_fcube(dbl_cube), (dbl_cube ** 2))#, "as_fcube")

## check icube (Cube<sword>) and ucube (Cube<uword>)
expect_equal(as_icube(int_cube), (int_cube ** 2))#, "as_icube")
expect_equal(as_ucube(int_cube), (int_cube ** 2))#, "as_ucube")

## check cx_cube (Cube<cx_double>) and cx_fcube (Cube<cx_float>)
expect_equal(as_cx_cube(cplx_cube), (cplx_cube ** 2))#, "as_cx_cube")
expect_equivalent(as_cx_fcube(cplx_cube), (cplx_cube ** 2), #"as_cx_fcube",
                  tolerance = critTol)
