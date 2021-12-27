#!/usr/bin/r -t
#
# Copyright (C) 2013 - 2019 Baptiste Auguie and Dirk Eddelbuettel
# Copyright (C) 2019 - 2020 Dirk Eddelbuettel
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

## It now (Apr 2020) appears to fail on 32-bit Windows
.onWindows <- .Platform$OS.type == "windows"
.is32bit <- .Platform$r_arch == "i386"

if (.onWindows && .is32bit) exit_file("Do not bother on 32-bit Windows")

Rcpp::sourceCpp("cpp/complex.cpp")

set.seed(123)

## create variables

A <- matrix(rnorm(9), 3)
B <- matrix(rnorm(9), 3)
C <- A + 1i * B

V <- rnorm(3) + 1i * rnorm(3)
S <- matrix(rnorm(5*3), nrow=3)

## Basic operations

rl <- complexCppTests(A, B, V, S)   # returns results list from C++

expect_equal(rl[["C"]],     C)#,          msg="complex matrix")
expect_equal(rl[["Cst"]],   t(C))#,       msg="complex matrix transpose")
expect_equal(rl[["Ct"]],    Conj(t(C)))#, msg="complex matrix transpose conjugated")
expect_equal(rl[["conjC"]], Conj(C))#,    msg="complex matrix conjugated")
expect_equal(rl[["absC"]],  Mod(C))#,     msg="complex matrix mod")
expect_equal(rl[["CV"]],    C %*% V)#,    msg="complex matrix product")
expect_equal(rl[["CS"]],    C %*% S)#,    msg="complex matrix times vector")
expect_equal(rl[["CC"]],    C * C)#,      msg="complex matrix ops mult")
expect_equal(rl[["CdC"]],   C / C)#,      msg="complex matrix ops div")
expect_equal(rl[["CpC"]],   C + C)#,      msg="complex matrix ops plus")
expect_equal(rl[["CmC"]],   C - C)#,      msg="complex matrix ops minus")
