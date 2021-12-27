#!/usr/bin/r -t
#
# Copyright (C) 2018  Keith O'Hara and Dirk Eddelbuettel
# Copyright (C) 2019  Dirk Eddelbuettel
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

Rcpp::sourceCpp("cpp/Rlapack.cpp")

set.seed(123)

## create variables

n <- 5 # size the of matrices generated in tests
n_tri <- 50 # size the of matrices generated in tridiagonal tests

## Basic operations

rl1 <- norm(cx_eig_pair_test(n),"2")
rl2 <- norm(cx_qz_test(n),"2")
rl3 <- cx_rank_test(n)                # should equal n
rl4 <- norm(cx_solve_test(n),"2")
rl5 <- norm(cx_solve_band_test(n_tri),"2")
rl6 <- norm(cx_pinv_test(n),"2")      # should be (approx) an identity matrix
rl7 <- norm(cx_schur_test(n),"2")

expect_equal(rl1,  0)#,  msg="eig_pair for complex matrices")
expect_equal(rl2,  0)#,  msg="qz for complex matrices")
expect_equal(rl3,  n)#,  msg="rank complex matrices")
expect_equal(rl4,  0)#,  msg="solve for complex matrices")
expect_equal(rl5,  0)#,  msg="solve for band complex matrices")
expect_equal(rl6,  1)#,  msg="pinv for complex matrices")
expect_equal(rl7,  0)#,  msg="schur for complex matrices")
