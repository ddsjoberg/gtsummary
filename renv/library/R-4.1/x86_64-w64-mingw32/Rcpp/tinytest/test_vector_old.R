
##  Copyright (C) 2014 - 2019  Dirk Eddelbuettel, Romain Francois and Kevin Ushey
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

Rcpp::sourceCpp("cpp/VectorOld.cpp")

#    test.IntegerVector.comma <- function(){
fun <- integer_comma
expect_equal( fun(), 0:3, info = "IntegerVector comma initialization" )

#    test.CharacterVector.comma <- function(){
fun <- character_comma
expect_equal( fun(), c("foo","bar", "bling" ), info = "CharacterVector comma operator" )
