#!/usr/bin/r -t
#
# Copyright (C) 2014 - 2019  Dirk Eddelbuettel
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

Rcpp::sourceCpp("cpp/rng.cpp")

#test.randu.seed <- function() {
set.seed(123)
a <- randu(10)
set.seed(123)
b <- randu(10)
expect_equal(a, b)#, msg="randu seeding")

#test.randi.seed <- function() {
set.seed(123)
a <- randi(10)
set.seed(123)
b <- randi(10)
expect_equal(a, b)#, msg="randi seeding")

#test.randn.seed <- function() {
set.seed(123)
a <- randn(10)
set.seed(123)
b <- randn(10)
expect_equal(a, b)#, msg="randn seeding")

#test.randu <- function() {
set.seed(123)
a <- randu(10)
expect_true(min(a) > 0)#, msg="randu min")
expect_true(max(a) < 1)#, msg="randu max")

#test.randi <- function() {
set.seed(123)
a <- randi(10)
expect_true(min(a) > 0)#, msg="randi min")
expect_true(typeof(a) == "integer")#, msg="randi type")

#test.randn <- function() {
set.seed(123)
a <- randn(10)
expect_true(min(a) > -4)#, msg="randn min")
expect_true(max(a) <  4)#, msg="randn max")
expect_true(typeof(a) == "double")#, msg="randi type")
