
// message.h: Rcpp R/C++ interface class library -- Wrapper for base::message
//
// Copyright (C) 2021  Dirk Eddelbuettel
//
// This file is part of Rcpp.
//
// Rcpp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// Rcpp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

#ifndef Rcpp_api_meat_message_h
#define Rcpp_api_meat_message_h

namespace Rcpp {

    inline void message(SEXP s) {
        Rcpp::Function msg = Rcpp::Environment::base_env()["message"];
        msg(s);
    }

}

#endif
