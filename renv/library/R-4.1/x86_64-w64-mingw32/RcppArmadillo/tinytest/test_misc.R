#!/usr/bin/r -t
#
# Copyright (C) 2021         Dirk Eddelbuettel
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

## -- src/RcppArmadillo.cpp
arma <- armadillo_version(FALSE)
expect_equal(length(arma), 3)           # major minor patch
expect_equal(names(arma), c("major","minor","patch"))
arma <- armadillo_version(TRUE)
expect_equal(class(arma), "integer")
expect_equal(length(arma), 1L)

## no tests as we have no (current) accessor as we prefer R RNGs
expect_warning(armadillo_set_seed_random())
armadillo_set_seed(42L)                 # no test as we have no (current) accessor as we prefer R RNGs


## -- R/flags.R
cxxflags <- RcppArmadillo:::RcppArmadilloCxxFlags()
expect_true(is.character(cxxflags))
expect_stdout(RcppArmadillo:::CxxFlags())
