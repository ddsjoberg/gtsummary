
##  Copyright (C) 2010 - 2019  John Chambers, Dirk Eddelbuettel and Romain Francois
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

Rcpp::sourceCpp("cpp/modref.cpp")

#    test.modRef <- function() {
ww <- new(ModRefWorld)
wg <- ModRefWorld$new()

expect_equal(ww$greet(), wg$greet())
wgg <- wg$greet()

ww$set("Other")

## test independence of ww, wg
expect_equal(ww$greet(), "Other")
expect_equal(wg$greet(), wgg)

ModRefWorld$methods(twice = function() paste(greet(), greet()))

expect_equal(ww$twice(), paste(ww$greet(), ww$greet()))
