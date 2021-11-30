// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// RcppArmadilloConfig.h: Rcpp/Armadillo glue
//
// Copyright (C)  2010 - 2016  Dirk Eddelbuettel, Romain Francois and Douglas Bates
//
// This file is part of RcppArmadillo.
//
// RcppArmadillo is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RcppArmadillo is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

#ifndef RcppArmadillo__RcppArmadilloConfig__h
#define RcppArmadillo__RcppArmadilloConfig__h

#if !defined(ARMA_USE_LAPACK)
#define ARMA_USE_LAPACK
#endif

#if !defined(ARMA_USE_BLAS)
#define ARMA_USE_BLAS
#endif

#define ARMA_HAVE_STD_ISFINITE
#define ARMA_HAVE_STD_ISINF
#define ARMA_HAVE_STD_ISNAN
#define ARMA_HAVE_STD_SNPRINTF


/* TODO: we might need to undef this on other platforms as well */
#if defined(__GNUC__) && defined(_WIN64) || defined(__FreeBSD__)
#undef ARMA_HAVE_STD_SNPRINTF
#endif

/* 
   suncc does not have std::isfinite (which is not standard)
   so we tell armadillo not to use it, and comment out a few 
   others while we are at it
*/
#if defined(__SUNPRO_CC)
#undef ARMA_HAVE_STD_ISFINITE
#undef ARMA_HAVE_STD_SNPRINTF
#undef ARMA_HAVE_LOG1P
#undef ARMA_HAVE_STD_ISINF
#undef ARMA_HAVE_STD_ISNAN
#endif

// Let's be careful for now and undef this as not all compilers support this
//#if defined(ARMA_USE_CXX11)
//#undef ARMA_USE_CXX11
//#endif

// If C++11 has been selected at the R package level, use it for Armadillo too
// This is actually not needed, if the proper switch is set via -std=... then
// Armadillo will know (cf compilation with -DARMA_EXTRA_DEBUG set)
// #if defined(USE_CXX1X)
// #define ARMA_USE_CXX11
// #endif

// We can use R as the RNG provider, see RcppArmadilloForward.h which
// enables inclusion of the appropriate headers. Alternatively, the
// C++11 RNG can be used by commenting out the current default and
// selecting the C++11 RNG instead. Lastly, one could (but should not)
// fall back to the C++98 RNG (often from the C library) by defining neither.

// Rcpp has its own stream object which cooperates more nicely with R's i/o
// And as of Armadillo 2.4.3, we can use this stream object as well
//
// As of Armadillo 8.100.1, this has been renamed to ARMA_COUT_STREAM and
// ARMA_CERR_STREAM was added
// 
#if !defined(ARMA_COUT_STREAM)
#define ARMA_COUT_STREAM Rcpp::Rcout
#endif
#if !defined(ARMA_CERR_STREAM)
#define ARMA_CERR_STREAM Rcpp::Rcerr
#endif


// R now defines NDEBUG which suppresses a number of useful Armadillo tests
// Users can still defined it later, and/or define ARMA_NO_DEBUG
#if defined(NDEBUG)
#undef NDEBUG
#endif

// On Windows do not read autoconf-updated header
#if defined(WIN32) || defined(_WIN32)
  // R can be built with its own Rlapack library, or use an external
  // one. Only the latter has zgesdd, a complex-valued SVD using divide-and-conquer 
  // on Windows we do not assume ZGESDD
  #define ARMA_CRIPPLED_LAPACK 1
  // on Windows we can now assume OpenMP with Rtools / gcc 4.9.3
  // note that performance is said to still be poor
  // cf https://cran.r-project.org/doc/manuals/r-devel/R-admin.html#The-MinGW_002dw64-toolchain
  #define ARMA_USE_OPENMP
#else
  // on the other OSs we test via LAPACK_LIBS (in configure) which
  // updates this include file
  #include <RcppArmadilloConfigGenerated.h>
#endif

// Many client packages do not set the OpenMP compiler flag in their src/Makevars
// (ie add $(SHLIB_OPENMP_CXXFLAGS) to the PKG_CXXFLAGS line as we do) so to tone
// down the line noise we ask Armadillo not to print the warning
#if defined(ARMA_USE_OPENMP)
  //#if !defined(_OPENMP)
  //  #pragma message("NOTE: To enable OpenMP-based speedups, add -fopenmp to the compiler flags. See Writing R Extension (Sec 1.2.1) and R Inst. + Admin. (Sec 6.3.3) for details.")
  //#endif
  #define ARMA_DONT_PRINT_OPENMP_WARNING 1
#endif


// Under C++11 and C++14, Armadillo now defaults to using int64_t for
// integers.  This prevents us from passing integer vectors to R as
// only used int32_t -- so we select the shorter representation here.
// Unless int64_t is explicitly required during compilation.
#if !defined(ARMA_64BIT_WORD)
  #define ARMA_32BIT_WORD 1
#endif

// To return arma::vec or arma::rowvec as R vector (i.e. dimensionless),
// one of the following macro can be defined before including
// RcppArmadillo.h. "ANYVEC" applys for both col- and row-vec.
//#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
//#define RCPP_ARMADILLO_RETURN_ROWVEC_AS_VECTOR
//#define RCPP_ARMADILLO_RETURN_ANYVEC_AS_VECTOR


#endif

