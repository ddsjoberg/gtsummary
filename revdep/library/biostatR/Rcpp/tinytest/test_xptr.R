
##  Copyright (C) 2009 - 2019  Dirk Eddelbuettel and Romain Francois
##
##  This file is part of Rcpp.
##
##  Rcpp is free software: you can redistribute it and/or modify it
##  under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 2 of the License, or
##  (at your option) any later version.
##
##  Rcpp is distributed in the hope that it will be useful, but
##  WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

if (Sys.getenv("RunAllRcppTests") != "yes") exit_file("Set 'RunAllRcppTests' to 'yes' to run.")

Rcpp::sourceCpp("cpp/XPtr.cpp")

#    test.XPtr <- function(){
xp <- xptr_1()
expect_equal(typeof( xp ), "externalptr", info = "checking external pointer creation" )

front <- xptr_2(xp)
expect_equal( front, 1L, info = "check usage of external pointer" )

xptr_self_tag(xp)
expect_equal(xptr_has_self_tag(xp), T, info = "check external pointer tag preserved")

expect_true(xptr_release(xp), info = "check release of external pointer")

expect_true(xptr_access_released(xp), info = "check access of released external pointer")

expect_error(xptr_use_released(xp), info = "check exception on use of released external pointer")
