\name{NEWS}
\title{News for Package \pkg{RcppArmadillo}}
\newcommand{\ghpr}{\href{https://github.com/RcppCore/RcppArmadillo/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/RcppCore/RcppArmadillo/issues/#1}{##1}}

\section{Changes in RcppArmadillo version 0.10.6.0.0 (2021-07-16)}{
  \itemize{
    \item Upgraded to Armadillo release 10.6.0 (Keep Calm)
    \itemize{
    \item expanded \code{chol()} to optionally use pivoted decomposition
    \item expanded vector, matrix and cube constructors to allow element
    initialisation via \code{fill::value(scalar)}, eg. \code{mat X(4,5,fill::value(123))}
    \item faster loading of CSV files when using OpenMP
    \item added \code{csv_opts::semicolon} option to allow saving/loading of
      CSV files with semicolon (;) instead of comma (,) as the separator
    }
  }
}

\section{Changes in RcppArmadillo version 0.10.5.3.0 (2021-07-01)}{
  \itemize{
    \item Upgraded to Armadillo release 10.5.3 (Antipodean Fortress)
    \item GitHub-only release
    \item Extended test coverage with several new tests, added a coverage badge.
  }
}

\section{Changes in RcppArmadillo version 0.10.5.0 (2021-05-21)}{
  \itemize{
    \item Upgraded to Armadillo release 10.5 (Antipodean Fortress)
    \itemize{
      \item added \code{.clamp()} member function
      \item expanded the standalone \code{clamp()} function to handle complex values
      \item more efficient use of OpenMP
      \item vector, matrix and cube constructors now initialise elements
      to zero by default; use the \code{fill::none} specifier,
      eg. \code{mat X(4,5,fill::none)}, to disable element initialisation
    }
    \item Added \code{codecov.yml} to exclude Armadillo from coverage analysis
  }
}

\section{Changes in RcppArmadillo version 0.10.4.1.0 (2021-04-23)}{
  \itemize{
    \item Upgraded to Armadillo release 10.4.1 (Pressure Cooker)
    \item GitHub-only release
  }
}

\section{Changes in RcppArmadillo version 0.10.4.0.0 (2021-04-12)}{
  \itemize{
    \item Upgraded to Armadillo release 10.4.0 (Pressure Cooker)
    \itemize{
      \item faster handling of triangular matrices by \code{log_det()}
      \item added \code{log_det_sympd()} for log determinant of
      symmetric positive matrices
      \item added ARMA_WARN_LEVEL configuration option, to control the
      degree of emitted warning messages
      \item reduced the default degree of warning messages, so that
      failed decompositions, failed saving/loading, etc, no longer emit
      warnings
    }
    \item Apply one upstream corrections for \code{arma::randn} draws
    when using alternative (here R) generator, and \code{arma::randg}.
  }
}

\section{Changes in RcppArmadillo version 0.10.3.0.0 (2021-03-10)}{
  \itemize{
    \item Upgraded to Armadillo release 10.3 (Sunrise Chaos)
    \itemize{
      \item faster handling of symmetric positive definite matrices by
      \code{pinv()}
      \item expanded \code{.save()} / \code{.load()} for dense matrices
      to handle coord_ascii format
      \item for out of bounds access, element accessors now throw the
      more nuanced \code{std::out_of_range} exception, instead of only
      \code{std::logic_error}
      \item improved quality of random numbers
    }
  }
}

\section{Changes in RcppArmadillo version 0.10.2.2.0 (2021-03-09)}{
  \itemize{
    \item Upgraded to Armadillo release 10.2.2 (Cicada Swarm)
    \itemize{
      \item faster handling of subcubes
      \item added \code{tgamma()}
      \item added \code{.brief_print()} for abridged printing of matrices & cubes
      \item expanded forms of \code{trimatu()} and \code{trimatl()} with
      diagonal specification to handle sparse matrices
      \item expanded \code{eigs_sym()} and \code{eigs_gen()} with optional shift-invert mode
    }
    \item Removed \code{debian/} directory from repository as packaging
    is on salsa.debian.org.
    \item Relaxed tolerance on two \code{cube} tests on Windows to
    accomodate new 'gcc10-UCRT' builder.
  }
}

\section{Changes in RcppArmadillo version 0.10.2.1.0 (2021-02-09)}{
  \itemize{
    \item Upgraded to Armadillo release 10.2.1 (Cicada Swarm)
    \itemize{
      \item faster handling of subcubes
      \item added \code{tgamma()}
      \item added \code{.brief_print()} for abridged printing of matrices & cubes
      \item expanded forms of \code{trimatu()} and \code{trimatl()} with
      diagonal specification to handle sparse matrices
      \item expanded \code{eigs_sym()} and \code{eigs_gen()} with optional shift-invert mode
    }
  }
}

\section{Changes in RcppArmadillo version 0.10.1.2.2 (2021-01-08)}{
  \itemize{
    \item Correct one unit test for \pkg{Matrix} 1.3.0-caused changed
    (Binxiang in \ghpr{319} and Dirk in \ghpr{322}).
    \item Suppress one further warning from \pkg{Matrix} (Dirk)
    \item Apply an upstream \code{NaN} correction (Conrad in \ghpr{321})
    \item Added GitHub Actions CI using \code{run.sh} from r-ci (Dirk)
  }
}

\section{Changes in RcppArmadillo version 0.10.1.2.0 (2020-11-15)}{
  \itemize{
    \item Upgraded to Armadillo release 10.1.2 (Orchid Ambush)
    \item Remove three unused int constants (\ghit{313})
    \item Include main armadillo header using quotes instead of brackets
    \item Rewrite version number use in old-school mode because gcc 4.8.5
    \item Skipping parts of sparse conversion on Windows as win-builder fails
  }
}

\section{Changes in RcppArmadillo version 0.10.1.0.0 (2020-10-09)}{
  \itemize{
    \item Upgraded to Armadillo release 10.1.0 (Orchid Ambush)
    \itemize{
      \item C++11 is now the minimum required C++ standard
      \item faster handling of compound expressions by \code{trimatu()}
      and \code{trimatl()}
      \item faster sparse matrix addition, subtraction and element-wise
      multiplication
      \item expanded sparse submatrix views to handle the non-contiguous
      form of \code{X.cols(vector_of_column_indices)}
      \item expanded \code{eigs_sym()} and \code{eigs_gen()} with optional
      fine-grained parameters (subspace dimension, number of iterations,
      eigenvalues closest to specified value)
      \item deprecated form of \code{reshape()} removed from Cube and
      SpMat classes
      \item ignore and warn on use of the \code{ARMA_DONT_USE_CXX11} macro
    }
    \item Switch Travis CI testing to focal and BSPM
  }
}

\section{Changes in RcppArmadillo version 0.9.900.3.0 (2020-09-02)}{
   \itemize{
     \item Upgraded to Armadillo release 9.900.3 (Nocturnal Misbehaviour)
     \itemize{
       \item More efficient code for initialising matrices with \code{fill::zeros}
       \item Fixes for various error messages
     }
   }
 }

\section{Changes in RcppArmadillo version 0.9.900.2.0 (2020-07-17)}{
  \itemize{
    \item Upgraded to Armadillo release 9.900.2 (Nocturnal Misbehaviour)
    \itemize{
      \item In \code{sort()}, fixes for inconsistencies between checks
      applied to matrix and vector expressions
      \item In \code{sort()}, remove unnecessary copying when applied in-place to vectors
      function when applied in-place to vectors
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.900.1.0 (2020-06-08)}{
  \itemize{
    \item Upgraded to Armadillo release 9.900.1 (Nocturnal Misbehaviour)
    \itemize{
      \item faster \code{solve()} for under/over-determined systems
      \item faster \code{eig_gen()} and \code{eig_pair()} for large matrices
      \item expanded \code{eig_gen()} and \code{eig_pair()} to
      optionally provide left and right eigenvectors
    }
    \item Switch Travis CI testing to R 4.0.0, use bionic as base distro
    and test R 3.6.3 and 4.0.0 in a matrix (Dirk in \ghpr{298}).
    \item Add two badges to README for indirect use and the CSDA paper.
    \item Adapt \code{RcppArmadillo.package.skeleton()} to a change in R
    4.0.0 affecting what it exports in \code{NAMESPACE}.
  }
}

\section{Changes in RcppArmadillo version 0.9.880.1.0 (2020-05-15)}{
  \itemize{
    \item Upgraded to Armadillo release 9.880.1 (Roasted Mocha Detox)
    \itemize{
      \item expanded \code{qr()} to optionally use pivoted decomposition
      \item updated physical constants to NIST 2018 CODATA values
      \item added \code{ARMA_DONT_USE_CXX11_MUTEX} confguration option
      to disable use of \code{std::mutex}
    }
    \item OpenMP capability is tested explicitly (Kevin Ushey and Dirk
    in \ghpr{294}, \ghpr{295}, and \ghpr{296} all fixing \ghit{290}).
  }
}

\section{Changes in RcppArmadillo version 0.9.870.2.0 (2020-04-24)}{
  \itemize{
    \item Upgraded to Armadillo release 9.870.2 (Roasted Mocha Retox)
    \itemize{
      \item faster handling of matrix multiplication expressions by
      \code{diagvec()} and \code{diagmat()}
      \item added \code{trimatu_ind()} and \code{trimatl_ind()}
      \item more consistent detection of sparse vector expressions
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.860.2.0 (2020-04-13)}{
  \itemize{
    \item Upgraded to Armadillo release 9.860.2 (Roasted Mocha Fix)
    \itemize{
      \item added \code{powmat()}
      \item faster access to columns in sparse submatrix views
      \item faster handling of relational expressions by \code{accu()}
      \item faster handling of sympd matrices by \code{expmat()}, \code{logmat()}, \code{sqrtmat()}
      \item workaround for save/load issues with HDF5 v1.12
    }
    \item Vignettes are now pre-made and include (\ghpr{285})
    \item Two test files are now skipped on 32-bit Windows
  }
}

\section{Changes in RcppArmadillo version 0.9.850.1.0 (2020-02-09)}{
  \itemize{
    \item Upgraded to Armadillo release 9.850.1 (Pyrocumulus Wrath)
    \itemize{
      \item faster handling of compound expressions by \code{diagmat()}
      \item expanded \code{.save()} and \code{.load()} to handle CSV files with headers via
        csv_name(filename,header) specification
      \item added \code{log_normpdf()}
      \item added \code{.is_zero()}
      \item added \code{quantile()}
    }
    \item The sparse matrix test using scipy, if available, is now
    simplified thanks to recently added \CRANpkg{reticulate} conversions.
  }
}

\section{Changes in RcppArmadillo version 0.9.800.4.0 (2020-01-24)}{
  \itemize{
    \item Upgraded to Armadillo release 9.800.4 (Horizon Scraper)
    \itemize{
      \item fixes for incorrect type promotion in \code{normpdf()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.800.3.0 (2019-12-04)}{
  \itemize{
    \item Upgraded to Armadillo release 9.800.3 (Horizon Scraper)
    \itemize{
      \item fixes for matrix row iterators
      \item better detection of non-hermitian matrices by
      \code{eig_sym()}, \code{inv_sympd()}, \code{chol()},
      \code{expmat_sym()}
    }
    \item The \code{sample} function passes the prob vector as const allowing
    subsequent calls (Christian Gunning in \ghpr{276} fixing \ghit{275})
  }
}

\section{Changes in RcppArmadillo version 0.9.800.1.0 (2019-10-09)}{
  \itemize{
    \item Upgraded to Armadillo release 9.800 (Horizon Scraper)
    \itemize{
      \item faster \code{solve()} in default operation; iterative
      refinement is no longer applied by default; use
      \code{solve_opts::refine} to explicitly enable refinement
      \item faster \code{expmat()}
      \item faster handling of triangular matrices by \code{rcond()}
      \item added \code{.front()} and \code{.back()}
      \item added \code{.is_trimatu()} and \code{.is_trimatl()}
      \item added \code{.is_diagmat()}
    }
    \item The package now uses \pkg{tinytest} for unit tests (Dirk in
    \ghpr{269}).
    \item The \code{configure.ac} script is now more careful about shell
    portability (Min Kim in \ghpr{270}).
  }
}

\section{Changes in RcppArmadillo version 0.9.700.2.0 (2019-09-01)}{
  \itemize{
    \item Upgraded to Armadillo release 9.700.2 (Gangster Democracy)
    \itemize{
      \item faster handling of cubes by \code{vectorise()}
      \item faster faster handling of sparse matrices by \code{nonzeros()}
      \item faster row-wise \code{index_min()} and \code{index_max()}
      \item expanded \code{join_rows()} and \code{join_cols()} to handle joining up to 4 matrices
      \item expanded \code{.save()} and \code{.load()} to allow storing sparse matrices in CSV format
      \item added \code{randperm()} to generate a vector with random
      permutation of a sequence of integers
    }
    \item Expanded the list of known good \code{gcc} and \code{clang}
    versions in \code{configure.ac}
  }
}

\section{Changes in RcppArmadillo version 0.9.600.4.0 (2019-07-14)}{
  \itemize{
    \item Upgraded to Armadillo release 9.600.4 (Napa Invasion)
    \itemize{
      \item faster handling of sparse submatrices
      \item faster handling of sparse diagonal views
      \item faster handling of sparse matrices by \code{symmatu()} and \code{symmatl()}
      \item faster handling of sparse matrices by \code{join_cols()}
      \item expanded \code{clamp()} to handle sparse matrices
      \item added \code{.clean()} to replace elements below a threshold with zeros
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.500.2.0 (2019-06-11)}{
  \itemize{
    \item Upgraded to Armadillo release 9.500.2 (Riot Compact)
    \itemize{
      \item Expanded \code{solve()} with \code{solve_opts::likely_sympd}
      to indicate that the given matrix is likely positive definite
      \item more robust automatic detection of positive definite
      matrices by \code{solve()} and \code{inv()}
      \item faster handling of sparse submatrices
      \item expanded \code{eigs_sym()} to print a warning if the given
      matrix is not symmetric
      \item extended LAPACK function prototypes to follow Fortran
      passing conventions for so-called "hidden arguments", in order
      to address GCC Bug 90329; to use previous LAPACK function
      prototypes without the "hidden arguments",
      \code{#define ARMA_DONT_USE_FORTRAN_HIDDEN_ARGS before
        #include <armadillo> }
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.400.3.0 (2019-05-09)}{
  \itemize{
    \item Upgraded to Armadillo release 9.400.3 (Surrogate Miscreant)
    \itemize{
      \item check for symmetric / hermitian matrices (used by
      decomposition functions) has been made more robust
      \item \code{linspace()} and \code{logspace()} now honour requests
      for generation of vectors with zero elements
      \item fix for vectorisation / flattening of complex sparse matrices
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.400.2.0 (2019-04-28)}{
  \itemize{
    \item Upgraded to Armadillo release 9.400.2 (Surrogate Miscreant)
    \itemize{
      \item faster \code{cov()} and \code{cor()}
      \item added \code{.as_col()} and \code{.as_row()}
      \item expanded \code{.shed_rows()} / \code{.shed_cols()} /
      \code{.shed_slices()} to remove rows/columns/slices specified in a
      vector
      \item expanded \code{vectorise()} to handle sparse matrices
      \item expanded element-wise versions of \code{max()} and
      \code{min()} to handle sparse matrices
      \item optimised handling of sparse matrix expressions:
      \code{sparse \% (sparse +- scalar)} and \code{sparse / (sparse +- scalar)}
      \item expanded \code{eig_sym()}, \code{chol()},
      \code{expmat_sym()}, \code{logmat_sympd()}, \code{sqrtmat_sympd()},
      \code{inv_sympd()} to print a warning if the given matrix is not
      symmetric
      \item more consistent detection of vector expressions
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.300.2.0 (2019-03-21)}{
  \itemize{
    \item Upgraded to Armadillo release 9.300.2 (Fomo Spiral)
    \itemize{
      \item Faster handling of compound complex matrix expressions by
      \code{trace()}
      \item More efficient handling of element access for inplace
      modifications in sparse matrices
      \item Added \code{.is_sympd()} to check whether a matrix is
      symmetric/hermitian positive definite
      \item Added \code{interp2()} for 2D data interpolation
      \item Added \code{expm1()} and \code{log1p()}
      \item Expanded \code{.is_sorted()} with options "strictascend" and
      "strictdescend"
      \item Expanded \code{eig_gen()} to optionally perform balancing
      prior to decomposition
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.200.7.1 (2019-03-08)}{
  \itemize{
    \item Explicit setting of \code{RNGversion("3.5.0")} in one unit
    test to accomodate the change in \code{sample()} in R 3.6.0
    \item Back-ported a fix to the Wishart RNG from upstream (Dirk in
    \ghpr{248} fixing \ghit{247})
  }
}

\section{Changes in RcppArmadillo version 0.9.200.7.0 (2019-01-17)}{
  \itemize{
    \item Upgraded to Armadillo release 9.200.7 (Carpe Noctem)
    \item Fixes in 9.200.7 compared to 9.200.5:
    \itemize{
      \item handling complex compound expressions by \code{trace()}
      \item handling \code{.rows()} and \code{.cols()} by the
      \code{Cube} class
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.200.5.0 (2018-11-27)}{
  \itemize{
    \item Upgraded to Armadillo release 9.200.5 (Carpe Noctem)
    \item Changes in this release
    \itemize{
      \item linking issue when using fixed size matrices and vectors
      \item faster handling of common cases by \code{princomp()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.9.200.4.0 (2018-11-09)}{
  \itemize{
    \item Upgraded to Armadillo release 9.200.4 (Carpe Noctem)
    \itemize{
      \item faster handling of symmetric positive definite matrices by
      \code{rcond()}
      \item faster transpose of matrices with size ≥ 512x512
      \item faster handling of compound sparse matrix expressions by
      \code{accu()},  \code{diagmat()}, \code{trace()}
      \item faster handling of sparse matrices by \code{join_rows()}
      \item expanded \code{sign()} to handle scalar arguments
      \item expanded operators (\code{*}, \code{\%}, \code{+}, \code{−})
      to handle sparse matrices with differing element types
      (eg. multiplication of complex matrix by real matrix)
      \item expanded \code{conv_to()} to allow conversion between sparse
      matrices with differing element types
      \item expanded \code{solve()} to optionally allow keeping
      solutions of systems singular to working precision
      \item workaround for \code{gcc} and \code{clang} bug in C++17 mode
    }
    \item Commented-out sparse matrix test consistently failing on the
    fedora-clang machine CRAN, and only there. No fix without access.
    \item The 'Unit test' vignette is no longer included.
  }
}

\section{Changes in RcppArmadillo version 0.9.100.5.0 (2018-08-16)}{
  \itemize{
    \item Upgraded to Armadillo release 9.100.4 (Armatus Ad Infinitum)
    \itemize{
      \item faster handling of symmetric/hermitian positive definite
      matrices by \code{solve()}
      \item faster handling of \code{inv_sympd()} in compound expressions
      \item added \code{.is_symmetric()}
      \item added \code{.is_hermitian()}
      \item expanded \code{spsolve()} to optionally allow keeping
      solutions of systems singular to working precision
      \item new configuration options \code{ARMA_OPTIMISE_SOLVE_BAND}
      and \code{ARMA_OPTIMISE_SOLVE_SYMPD}
      smarter use of the element cache in sparse matrices
      \item smarter use of the element cache in sparse matrices
    }
    \item Aligned OpenMP flags in the RcppArmadillo.package.skeleton
    used Makevars{,.win} to not use one C and C++ flag.
  }
}

\section{Changes in RcppArmadillo version 0.8.600.0.0 (2018-06-28)}{
  \itemize{
    \item Upgraded to Armadillo release 8.600.0 (Sabretooth Rugrat)
    \itemize{
      \item added \code{hess()} for Hessenberg decomposition
      \item added \code{.row()}, \code{.rows()}, \code{.col()},
      \code{.cols()} to subcube views
      \item expanded \code{.shed_rows()} and \code{.shed_cols()} to
      handle cubes
      \item expanded \code{.insert_rows()} and \code{.insert_cols()} to
      handle cubes
      \item expanded subcube views to allow non-contiguous access to
      slices
      \item improved tuning of sparse matrix element access operators
      \item faster handling of tridiagonal matrices by \code{solve()}
      \item faster multiplication of matrices with differing element
      types when using OpenMP
    }
  }
}

\section{Changes in RcppArmadillo version 0.8.500.1.1 (2018-05-17) [GH only]}{
  \itemize{
    \item Upgraded to Armadillo release 8.500.1 (Caffeine Raider)
    \itemize{
      \item bug fix for banded matricex
    }
    \item Added \code{slam} to Suggests: as it is used in two unit test
    functions [CRAN requests]
    \item The \code{RcppArmadillo.package.skeleton()} function now works
    with \code{example_code=FALSE} when \CRANpkg{pkgKitten} is present
    (Santiago Olivella in \ghpr{231} fixing \ghpr{229})
    \item The LAPACK tests now cover band matrix solvers (Keith O'Hara
    in \ghpr{230}).
  }
}

\section{Changes in RcppArmadillo version 0.8.500.0 (2018-04-21)}{
  \itemize{
    \item Upgraded to Armadillo release 8.500 (Caffeine Raider)
    \itemize{
      \item faster handling of sparse matrices by \code{kron()} and \code{repmat()}
      \item faster transpose of sparse matrices
      \item faster element access in sparse matrices
      \item faster row iterators for sparse matrices
      \item faster handling of compound expressions by \code{trace()}
      \item more efficient handling of aliasing in submatrix views
      \item expanded \code{normalise()} to handle sparse matrices
      \item expanded \code{.transform()} and \code{.for_each()} to handle sparse matrices
      \item added \code{reverse()} for reversing order of elements
      \item added \code{repelem()} for replicating elements
      \item added \code{roots()} for finding the roots of a polynomial
    }
    \item Fewer LAPACK compile-time guards are used, new unit tests for underlying
    features have been added (Keith O'Hara in \ghpr{211} addressing \ghit{207}).
    \item The configure check for LAPACK features has been updated
    accordingly (Keith O'Hara in \ghpr{214} addressing \ghit{213}).
    \item The compile-time check for \code{g++} is now more robust to
    minimal shell versions (\ghpr{217} addressing \ghit{216}).
    \item Compiler tests to were added for macOS (Keith O'Hara in \ghpr{219}).
  }
}

\section{Changes in RcppArmadillo version 0.8.400.0.0 (2018-02-19)}{
  \itemize{
    \item Upgraded to Armadillo release 8.400.0 (Entropy Bandit)
    \itemize{
      \item faster handling of sparse matrices by \code{repmat()}
      \item faster loading of CSV files
      \item expanded \code{kron()} to handle sparse matrices
      \item expanded \code{index_min()} and \code{index_max()} to handle cubes
      \item expanded \code{randi()}, \code{randu()}, \code{randn()},
      \code{randg()} to output single scalars
      \item added submatrix & subcube iterators
      \item added \code{normcdf()}
      \item added \code{mvnrnd()}
      \item added \code{chi2rnd()}
      \item added \code{wishrnd()} and \code{iwishrnd()}
    }
    \item The \code{configure} generated header settings for LAPACK and
    OpenMP can be overridden by the user.
    \item This release was preceded by two release candidates which were
    tested extensively.
  }
}

\section{Changes in RcppArmadillo version 0.8.300.1.0 (2017-12-04)}{
  \itemize{
    \item Upgraded to Armadillo release 8.300.1 (Tropical Shenanigans)
    \itemize{
      \item faster handling of band matrices by \code{solve()}
      \item faster handling of band matrices by \code{chol()}
      \item faster \code{randg()} when using OpenMP
      \item added \code{normpdf()}
      \item expanded \code{.save()} to allow appending new datasets to existing HDF5 files
    }
    \item Includes changes made in several earlier GitHub-only releases
    (versions 0.8.300.0.0, 0.8.200.2.0 and 0.8.200.1.0).
    \item Conversion from \code{simple_triplet_matrix} is now supported
    (Serguei Sokol in \ghpr{192}).
    \item Updated configure code to check for g++ 5.4 or later to enable OpenMP.
    \item Updated the skeleton package to current packaging standards
    \item Suppress warnings from Armadillo about missing OpenMP support
    and \code{-fopenmp} flags by setting \code{ARMA_DONT_PRINT_OPENMP_WARNING}
  }
}

\section{Changes in RcppArmadillo version 0.8.100.1.0 (2017-10-10)}{
  \itemize{
    \item Upgraded to Armadillo release 8.100.1 (Feral Pursuits)
    \itemize{
      \item faster incremental construction of sparse matrices via
      element access operators
      \item faster diagonal views in sparse matrices
      \item expanded \code{SpMat} to save/load sparse matrices in coord
      format
      \item expanded \code{.save()},\code{.load()} to allow
      specification of datasets within HDF5 files
      \item added \code{affmul()} to simplify application of affine
      transformations
      \item warnings and errors are now printed by default to the
      \code{std::cerr} stream
      \item added \code{set_cerr_stream()} and \code{get_cerr_stream()}
      to replace \code{set_stream_err1()}, \code{set_stream_err2()},
      \code{get_stream_err1()}, \code{get_stream_err2()}
      \item new configuration options \code{ARMA_COUT_STREAM} and
      \code{ARMA_CERR_STREAM}
    }
    \item Constructors for sparse matrices of types \code{dgt},
    \code{dtt} amd \code{dst} now use Armadillo code for improved
    performance (Serguei Sokol in \ghpr{175} addressing \ghit{173})
    \item Sparse matrices call \code{.sync()} before accessing internal
    arrays (Binxiang Ni in \ghpr{171})
    \item The sparse matrix vignette has been converted to Rmarkdown
    using the pinp package, and is now correctly indexed. (\ghpr{176})
  }
}

\section{Changes in RcppArmadillo version 0.7.960.1.2 (2017-08-29)}{
  \itemize{
    \item On macOS, OpenMP support is now turned off (\ghpr{170}).
    \item The package is now compiling under the C++11 standard (\ghpr{170}).
    \item The vignette dependency are correctly set (James and Dirk in
    \ghpr{168} and \ghpr{169})
  }
}

\section{Changes in RcppArmadillo version 0.7.960.1.1 (2017-08-20)}{
  \itemize{
    \item Added improved check for inherited S4 matrix classes
    (\ghpr{162} fixing \ghit{161})
    \item Changed \code{fastLm} C++ function to \code{fastLm_impl} to not
    clash with R method (\ghpr{164} fixing \ghpr{163})
    \item Added OpenMP check for \code{configure} (\ghpr{166} fixing
    \ghit{165})
  }
}

\section{Changes in RcppArmadillo version 0.7.960.1.0 (2017-08-11)}{
  \itemize{
    \item Upgraded to Armadillo release 7.960.1 (Northern Banana Republic Deluxe)
    \itemize{
      \item faster \code{randn()} when using OpenMP (NB: usually omitted
      when used fromR)
      \item faster \code{gmm_diag} class, for Gaussian mixture models
      with diagonal covariance matrices
      \item added \code{.sum_log_p()} to the \code{gmm_diag} class
      \item added \code{gmm_full} class, for Gaussian mixture models
      with full covariance matrices
      \item expanded \code{.each_slice()} to optionally use OpenMP for
      multi-threaded execution
    }
    \item Upgraded to Armadillo release 7.950.0 (Northern Banana Republic)
    \itemize{
      \item expanded \code{accu()} and \code{sum()} to use OpenMP for
      processing expressions with computationally expensive element-wise
      functions
      \item expanded \code{trimatu()} and \code{trimatl()} to allow
      specification of the diagonal which delineates the boundary of the
      triangular part
    }
    \item Enhanced support for sparse matrices (Binxiang Ni as part of
    Google Summer of Code 2017)
    \itemize{
      \item Add support for \code{dtCMatrix} and \code{dsCMatrix}
      (\ghpr{135})
      \item Add conversion and unit tests for \code{dgT}, \code{dtT} and
      \code{dsTMatrix} (\ghpr{136})
      \item Add conversion and unit tests for \code{dgR}, \code{dtR} and
      \code{dsRMatrix} (\ghpr{139})
      \item Add conversion and unit tests for \code{pMatrix} and
      \code{ddiMatrix} (\ghpr{140})
      \item Rewrite conversion for \code{dgT}, \code{dtT} and
      \code{dsTMatrix}, and add file-based tests (\ghpr{142})
      \item Add conversion and unit tests for \code{indMatrix} (\ghpr{144})
      \item Rewrite conversion for \code{ddiMatrix} (\ghpr{145})
      \item Add a warning message for matrices that cannot be converted
      (\ghpr{147})
      \item Add new vignette for sparse matrix support (\ghpr{152}; Dirk
      in \ghpr{153})
      \item Add support for sparse matrix conversion from Python SciPy
      (\ghpr{158} addressing \ghit{141})
    }
    \item Optional return of row or column vectors in collapsed form if
    appropriate \code{#define} is set (Serguei Sokol in \ghpr{151} and
    \ghpr{154})
    \item Correct \code{speye()} for non-symmetric cases (Qiang Kou in
    \ghpr{150} closing \ghit{149}).
    \item Ensure tests using Scientific Python and reticulate are properly
    conditioned on the packages being present.
    \item Added \code{.aspell/} directory with small local directory now
    supported by R-devel.
  }
}

\section{Changes in RcppArmadillo version 0.7.900.2.0 (2017-06-02)}{
  \itemize{
    \item Upgraded to Armadillo release 7.900.2 (Evil Banana Republic)
    \itemize{
      \item Expanded \code{clamp()} to handle cubes
      \item Computationally expensive element-wise functions (such as
      \code{exp()}, \code{log()}, \code{cos()}, etc) can now be
      automatically sped up via OpenMP; this requires a C++11/C++14
      compiler with OpenMP 3.0+ support for GCC and clang compilers
      \item One caveat: when using GCC, use of \code{-march=native} in
      conjunction with \code{-fopenmp} may lead to speed regressions on
      recent processors
    }
    \item Added gcc 7 to support compiler check (James Balamuta in \ghpr{128}
    addressing \ghit{126}).
    \item A unit test helper function for \code{rmultinom} was
    corrected (\ghpr{133}).
    \item OpenMP support was added to the skeleton helper in \code{inline.R}
  }
}

\section{Changes in RcppArmadillo version 0.7.800.2.0 (2017-04-12)}{
  \itemize{
    \item Upgraded to Armadillo release 7.800.2 (Rogue State Redux)
    \itemize{
      \item The Armadillo license changed to Apache License 2.0
    }
    \item The \code{DESCRIPTION} file now mentions the Apache License
    2.0, as well as the former MPL2 license used for earlier releases.
    \item A new file \code{init.c} was added with calls to
    \code{R_registerRoutines()} and \code{R_useDynamicSymbols()}
    \item Symbol registration is enabled in \code{useDynLib}
    \item The \code{fastLm} example was updated
  }
}

\section{Changes in RcppArmadillo version 0.7.700.0.0 (2017-02-07)}{
  \itemize{
    \item Upgraded to Armadillo release 7.700.0 (Rogue State)
    \itemize{
      \item added \code{polyfit()} and \code{polyval()}
      \item added second form of \code{log_det()} to directly return the
      result as a complex number
      \item added \code{range()} to statistics functions
      \item expanded \code{trimatu()}/\code{trimatl()} and
      \code{symmatu()}/\code{symmatl()} to handle sparse matrice
    }
  }
}

\section{Changes in RcppArmadillo version 0.7.600.2.0 (2017-01-05)}{
  \itemize{
    \item Upgraded to Armadillo release 7.600.2 (Coup d'Etat Deluxe)
    \itemize{
      \item Bug fix to memory allocation for \code{fields}
    }
  }
}

\section{Changes in RcppArmadillo version 0.7.600.1.0 (2016-12-16)}{
  \itemize{
    \item Upgraded to Armadillo release 7.600.1 (Coup d'Etat Deluxe)
    \itemize{
      \item more accurate \code{eigs_sym()} and \code{eigs_gen()}
      \item expanded \code{floor()}, \code{ceil()}, \code{round()},
      \code{trunc()}, \code{sign()} to handle sparse matrices
      \item added \code{arg()}, \code{atan2()}, \code{hypot()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.7.500.1.0 (2016-11-11)}{
  \itemize{
    \item Upgraded to Armadillo release 7.500.1
    \item Small improvement to return value treatment
    \item The \code{sample.h} extension was updated to the newer
    Armadillo interface. (Closes \ghit{111})
  }
}

\section{Changes in RcppArmadillo version 0.7.500.0.0 (2016-10-20)}{
  \itemize{
    \item Upgraded to Armadillo release 7.500.0 (Coup d'Etat)
    \itemize{
      \item Expanded \code{qz()} to optionally specify ordering of the Schur form
      \item Expanded \code{each_slice()} to support matrix multiplication
    }
  }
}

\section{Changes in RcppArmadillo version 0.7.400.2.0 (2016-08-24)}{
  \itemize{
    \item Upgraded to Armadillo release 7.400.2 (Feral Winter Deluxe)
    \itemize{
      \item added \code{expmat_sym()}, \code{logmat_sympd()},
        \code{sqrtmat_sympd()}
      \item added \code{.replace()}
      }
  }
}

\section{Changes in RcppArmadillo version 0.7.300.1.0 (2016-07-30)}{
  \itemize{
    \item Upgraded to Armadillo release 7.300.1
    \itemize{
      \item added \code{index_min()} and \code{index_max()} standalone functions
      \item expanded \code{.subvec()} to accept \code{size()} arguments
      \item more robust handling of non-square matrices by \code{lu()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.7.200.2.0 (2016-07-22)}{
  \itemize{
    \item Upgraded to Armadillo release 7.200.2
    \item The sampling extension was rewritten to use Armadillo vector types
    instead of Rcpp types (PR \ghpr{101} by James Balamuta)
  }
}

\section{Changes in RcppArmadillo version 0.7.200.1.0 (2016-06-06)}{
  \itemize{
    \item Upgraded to Armadillo release 7.200.1
    \itemize{
      \item added \code{.index_min()} and \code{.index_max()}
      \item expanded \code{ind2sub()} to handle vectors of indices
      \item expanded \code{sub2ind()} to handle matrix of subscripts
      \item expanded \code{expmat()}, \code{logmat()} and
      \code{sqrtmat()} to optionally return a bool indicating success
      \item faster handling of compound expressions by \code{vectorise()}
    }
    \item The \code{configure} code now (once again) sets the values for
    the LAPACK feature \code{#define} correctly.
  }
}

\section{Changes in RcppArmadillo version 0.7.100.3.0 (2016-05-25)}{
  \itemize{
    \item Upgraded to Armadillo test release 7.100.3
    \itemize{
      \item added \code{erf()}, \code{erfc()}, \code{lgamma()}
      \item added \code{.head_slices()} and \code{.tail_slices()} to
      subcube views
      \item \code{spsolve()} now requires SuperLU 5.2
      \item \code{eigs_sym()}, \code{eigs_gen()} and \code{svds()} now
      use a built-in reimplementation of ARPACK for real (non-complex)
      matrices (code contributed by Yixuan Qiu)
    }
    \item The \code{configure} code now checks against old \code{g++}
    version which are no longer sufficient to build the package.
  }
}

\section{Changes in RcppArmadillo version 0.6.700.6.0 (2016-05-05)}{
  \itemize{
    \item Upgraded to Armadillo 6.700.6 (Catabolic Amalgamator Deluxe)
    \itemize{
      \item fix for handling empty matrices by \code{kron()}
      \item fix for clang warning in advanced matrix constructors
      \item fix for false deprecated warning in \code{trunc_log()} and
      \code{trunc_exp()}
      \item fix for gcc-6.1 warning about misleading indentation
      \item corrected documentation for the \code{solve()} function
    }
    \item Added support for \code{int64_t} (\code{ARMA_64BIT_WORD}) when
    required during compilation time. (PR \ghpr{90} by George G. Vega
    Yon, fixing \ghpr{88})
    \item Fixed bug in \code{SpMat} exporter (PR \ghpr{91} by George G. Vega Yon,
    fixing \ghit{89} and \ghit{72})
  }
}

\section{Changes in RcppArmadillo version 0.6.700.3.0 (2016-04-05)}{
  \itemize{
    \item Upgraded to Armadillo 6.700.3 (Catabolic Amalgamator Deluxe)
    \itemize{
      \item added \code{logmat()} for calcuating the matrix logarithm
      \item added \code{regspace()} for generating vectors with
      regularly spaced elements
      \item added \code{logspace()} for generating vectors with
      logarithmically spaced elements
      \item added \code{approx_equal()} for determining approximate equality
      \item added \code{trapz()} for numerical integration
      \item expanded \code{.save()} and \code{.load()} with
      hdf5_binary_trans file type, to save/load data with columns
      transposed to rows
    }
  }
}

\section{Changes in RcppArmadillo version 0.6.600.4.0 (2016-03-15)}{
  \itemize{
    \item Upgraded to Armadillo 6.600.4 (Catabolic Amalgamator)
    \itemize{
      \item expanded \code{sum()}, \code{mean()}, \code{min()}, \code{max()} to handle cubes
      \item expanded \code{Cube} class to handle arbitrarily sized empty cubes (eg. 0x5x2)
      \item added \code{shift()} for circular shifts of elements
      \item added \code{sqrtmat()} for finding the square root of a matrix
      \item fix for \code{gmm_diag} when using Mahalanobis distance
    }
    \item The \code{configure} script now reflects the full LAPACK
    fallback offered by R 3.3.0 or later (PR \ghpr{81})
  }
}

\section{Changes in RcppArmadillo version 0.6.500.4.0 (2016-01-26)}{
  \itemize{
    \item Upgraded to Armadillo 6.500.4 (Gourmet Electron Jumper)
    \itemize{
      \item added \code{conv2()} for 2D convolution
      \item added stand-alone \code{kmeans()} function for clustering data
      \item added \code{trunc()}
      \item extended \code{conv()} to optionally provide central convolution
      \item faster handling of multiply-and-accumulate by \code{accu()}
      when using Intel MKL, ATLAS or OpenBLAS
    }
    \item The \code{configure} script now uses \code{#!/usr/bin/env
      bash} to cope with systems that do not have \code{#!/bin/bash} (PR
    \ghpr{75} fixing issue \ghpr{74})
    \item RcppArmadillo now defines ARMA_32BIT_WORD to ensure we always
    use integer vectors that be passed to R
  }
}

\section{Changes in RcppArmadillo version 0.6.400.2.0 (2015-12-15)}{
  \itemize{
    \item Upgraded to Armadillo 6.400.2 ("Flying Spaghetti Monster Deluxe")
    \itemize{
      \item expanded \code{each_col()}, \code{each_row()} and
      \code{each_slice()} to handle C++11 lambda functions
      \item added \code{ind2sub()} and \code{sub2ind()}
      \item fixes for corner cases in gmm_diag class
    }
  }
}

\section{Changes in RcppArmadillo version 0.6.300.2.2 (2015-12-12)}{
  \itemize{
    \item Upgraded to Armadillo 6.300.3-test ("Flying Spaghetti Monster")
    \itemize{
      \item Additional test in \code{auxlib_meat.hpp} for limited LAPACK
    }
    \item Updated test and \code{#define} for limited LAPACK version R
    might be built with on Unix-alike systems
  }
}

\section{Changes in RcppArmadillo version 0.6.300.2.0 (2015-12-03)}{
  \itemize{
    \item Upgraded to Armadillo 6.300.2 ("Flying Spaghetti Monster")
    \itemize{
      \item expanded \code{solve()} to find approximate solutions for rank-deficient systems
      \item faster handling of non-contiguous submatrix views in compound expressions
      \item added \code{.for_each()} to Mat, Row, Col, Cube and field classes
      \item added \code{rcond()} for estimating the reciprocal condition number
      \item fixes for \code{spsolve()}, \code{eigs_sym()}, \code{eigs_gen()}, \code{svds()}
    }
    \item Added support for \code{Cube} types via \code{as<>} converters
    (PR \ghpr{64} by Nathan Russell, fixing \ghit{63} and \ghit{42})
  }
}

\section{Changes in RcppArmadillo version 0.6.200.2.0 (2015-10-31)}{
  \itemize{
    \item Upgraded to Armadillo 6.200.0 ("Midnight Blue Deluxe")
    \itemize{
      \item expanded \code{diagmat()} to handle non-square matrices and arbitrary diagonals
      \item expanded \code{trace()} to handle non-square matrices
    }
  }
}

\section{Changes in RcppArmadillo version 0.6.100.0.0 (2015-10-03)}{
  \itemize{
    \item Upgraded to Armadillo 6.100.0 ("Midnight Blue")
    \itemize{
      \item faster \code{norm()} and \code{normalise()} when using ATLAS or OpenBLAS
      \item added Schur decomposition: \code{schur()}
      \item stricter handling of matrix objects by \code{hist()} and \code{histc()}
      \item advanced constructors for using auxiliary memory by Mat,
      Col, Row and Cube now have the default of \emph{strict = false}
      \item Cube class now delays allocation of .slice() related structures until needed
      \item expanded \code{join_slices()} to handle joining cubes with matrices
    }
  }
}

\section{Changes in RcppArmadillo version 0.6.000.1.0 (2015-09-25)}{
  \itemize{
    \item Upgraded to Armadillo test / bug-fix release 0.6.000.1-test
    \item Non-CRAN release
  }
}

\section{Changes in RcppArmadillo version 0.5.600.2.0 (2015-09-19)}{
  \itemize{
    \item Upgraded to Armadillo 5.600.2 ("Molotov Cocktail Deluxe")
    \itemize{
      \item expanded \code{.each_col()} and \code{.each_row()} to handle out-of-place operations
      \item added \code{.each_slice()} for repeated matrix operations on each slice of a cube
      \item faster handling of compound expressions by \code{join_rows()} and \code{join_cols()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.5.500.2.0 (2015-09-03)}{
  \itemize{
    \item Upgraded to Armadillo 5.500.2 ("Molotov Cocktail")
    \itemize{
      \item expanded object constructors and generators to handle \code{size()} based specification of dimensions
      \item faster handling of submatrix rows
      \item faster \code{clamp()}
      \item fixes for handling sparse matrices
    }
  }
}

\section{Changes in RcppArmadillo version 0.5.400.2.0 (2015-08-17)}{
  \itemize{
    \item Upgraded to Armadillo 5.400.2 ("Plutocracy Incorporated Deluxe")
    \itemize{
      \item added \code{find_unique()} for finding indices of unique values
      \item added \code{diff()} for calculating differences between consecutive elements
      \item added \code{cumprod()} for calculating cumulative product
      \item added \code{null()} for finding the orthonormal basis of null space
      \item expanded \code{interp1()} to handle repeated locations
      \item expanded \code{unique()} to handle complex numbers
      \item faster \code{flupud()}
      \item faster row-wise \code{cumsum()}
      \item fix for k-means clustering in gmm_diag class
    }
    \item corrected use of \code{kitten()} thanks to Grant Brown
  }
}

\section{Changes in RcppArmadillo version 0.5.300.4 (2015-08-03)}{
  \itemize{
    \item Upgraded to Armadillo 5.300.4 ("Plutocrazy Incorporated")
    \itemize{
      \item added generalised Schur decomposition: \code{qz()}
      \item added \code{.has_inf()} and \code{.has_nan()}
      \item expanded \code{interp1()} to handle out-of-domain locations
      \item expanded sparse matrix class with \code{.set_imag()} and \code{.set_real()}
      \item expanded \code{imag()}, \code{real()} and \code{conj()} to handle sparse matrices
      \item expanded \code{diagmat()}, \code{reshape()} and \code{resize()} to handle sparse matrices
      \item faster sparse \code{sum()}
      \item faster row-wise \code{sum()}, \code{mean()}, \code{min()}, \code{max()}
      \item updated physical constants to NIST 2014 CODATA values
      \item fixes for handling sparse submatrix views
      \item Armadillo can make use of GPUs by linking with NVIDIA NVBLAS
      (a GPU-accelerated implementation of BLAS), or by linking with AMD
      ACML (which can use GPUs via OpenCL)
    }
    \item Added \code{importFrom} statements for R functions not from base
    \item Added explicit \code{Rcpp::sourceCpp()} reference as well
    \item Updated one formatting detail in vignette to please TeXlive2015
  }
}

\section{Changes in RcppArmadillo version 0.5.200.1.0 (2015-06-04)}{
  \itemize{
    \item Upgraded to Armadillo release 5.200.1 ("Boston Tea Smuggler")
    \itemize{
      \item added \code{orth()} for finding the orthonormal basis of the range space of a matrix
      \item expanded element initialisation to handle nested initialiser lists (C++11)
      \item workarounds for bugs in GCC, Intel and MSVC C++ compilers
    }
    \item Added another example to \code{inst/examples/fastLm.r}
  }
}

\section{Changes in RcppArmadillo version 0.5.100.2.0 (2015-05-12)}{
  \itemize{
    \item Upgraded to Armadillo test / bug-fix release 5.100.2
    \item Non-CRAN release
  }
}

\section{Changes in RcppArmadillo version 0.5.100.1.0 (2015-05-01)}{
  \itemize{
    \item Upgraded to Armadillo release 5.100.1  ("Ankle Biter Deluxe")
    \itemize{
      \item added \code{interp1()} for 1D interpolation
      \item added \code{.is_sorted()} for checking whether a vector or matrix has sorted elements
      \item updated physical constants to NIST 2010 CODATA values
    }
  }
}

\section{Changes in RcppArmadillo version 0.5.000.0 (2015-04-12)}{
  \itemize{
    \item Upgraded to Armadillo release Version 5.000 ("Ankle Biter")
    \itemize{
      \item added \code{spsolve()} for solving sparse systems of linear equations
      \item added \code{svds()} for singular value decomposition of sparse matrices
      \item added \code{nonzeros()} for extracting non-zero values from matrices
      \item added handling of diagonal views by sparse matrices
      \item expanded \code{repmat()} to handle sparse matrices
      \item expanded \code{join_rows()} and \code{join_cols()} to handle sparse matrices
      \item \code{sort_index()} and \code{stable_sort_index()} have been
      placed in the delayed operations framework for increased efficiency
      \item use of 64 bit integers is automatically enabled when using a C++11 compiler
      \item workaround for a bug in recent releases of Apple Xcode
      \item workaround for a bug in LAPACK 3.5
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.999.1.0 (2015-04-04)}{
  \itemize{
    \item Upgraded to Armadillo release preview 4.999.1
    \item Non-CRAN test release
  }
}

\section{Changes in RcppArmadillo version 0.4.650.1.1 (2015-02-25)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.650.1 ("Intravenous Caffeine Injector")
    \itemize{
      \item added \code{randg()} for generating random values from gamma distributions (C++11 only)
      \item added \code{.head_rows()} and \code{.tail_rows()} to submatrix views
      \item added \code{.head_cols()} and \code{.tail_cols()} to submatrix views
      \item expanded \code{eigs_sym()} to optionally calculate
      eigenvalues with smallest/largest algebraic values
      fixes for handling of sparse matrices
    }
    \item Applied small correction to main header file to set up C++11
    RNG whether or not the alternate RNG (based on R, our default) is used
  }
}

\section{Changes in RcppArmadillo version 0.4.600.4.0 (2015-01-23)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.600.4 (still "Off The Reservation")
    \itemize{
      \item Speedups in the transpose operation
      \item Small bug fixes
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.600.0 (2014-12-27)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.600 ("Singapore Sling Deluxe")
    \itemize{
      \item added \code{.head()} and \code{.tail()} to submatrix views
      \item faster matrix transposes within compound expressions
      \item faster \code{accu()} and \code{norm()} when compiling with
      -O3 -ffast-math -march=native (gcc and clang)
      \item workaround for a bug in GCC 4.4
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.550.2.0 (2014-12-02)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.550.2 ("Singapore Sling Deluxe")
    \itemize{
      \item Bug fixes for implicit template initiation for \code{std::pow()} seen with the old g++ 4.4* series
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.550.1.0 (2014-11-26)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.550.1 ("Singapore Sling Deluxe")
    \itemize{
      \item added matrix exponential function: \code{expmat()}
      \item faster \code{.log_p()} and \code{.avg_log_p()} functions in the \code{gmm_diag} class
      when compiling with OpenMP enabled
      \item faster handling of in-place addition/subtraction of expressions
      with an outer product
      \item applied correction to \code{gmm_diag} relative to the 4.550 release
    }
    \item The Armadillo Field type is now converted in \code{as<>} conversions
  }
}

\section{Changes in RcppArmadillo version 0.4.500.0 (2014-10-30)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.500 ("Singapore Sling")
    \itemize{
      \item faster handling of complex vectors by \code{norm()}
      \item expanded \code{chol()} to optionally specify output matrix
      as upper or lower triangular
      \item better handling of non-finite values when saving matrices as text files
    }
    \item The \code{sample} functionality has been extended to provide
    the Walker Alias method (including new unit tests) via a pull request
    by Christian Gunning
  }
}

\section{Changes in RcppArmadillo version 0.4.450.1.0 (2014-09-21)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.450.1 (Spring Hill Fort)
    \itemize{
      \item faster handling of matrix transposes within compound
      expressions
      \item expanded \code{symmatu()}/\code{symmatl()} to optionally
      disable taking the complex conjugate of elements
      \item expanded \code{sort_index()} to handle complex vectors
      \item expanded the \code{gmm_diag} class with functions to generate random samples
    }
    \item A new random-number implementation for Armadillo uses the RNG
    from R as a fallback (when C++11 is not selected so the C++11-based
    RNG is unavailable) which avoids using the older C++98-based \code{std::rand}
    \item The \code{RcppArmadillo.package.skeleton()} function was
    updated to only set an "Imports:" for Rcpp, but not RcppArmadillo
    which (as a template library) needs only LinkingTo:
    \item The \code{RcppArmadillo.package.skeleton()} function will now
    prefer \code{pkgKitten::kitten()} over \code{package.skeleton()} in
    order to create a working package which passes \code{R CMD check}.
    \item The \CRANpkg{pkgKitten} package is now a \code{Suggests:}
    \item A manual page was added to provide documentation for the
    functions provided by the skeleton package.
    \item A small update was made to the package manual page.
  }
}

\section{Changes in RcppArmadillo version 0.4.400.0 (2014-08-19)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.400 (Winter Shark Alley)
    \itemize{
      \item added \code{gmm_diag} class for statistical modelling using Gaussian Mixture Models;
      includes multi-threaded implementation of k-means and Expectation-Maximisation for parameter estimation
      \item added \code{clamp()} for clamping values to be between lower and upper limits
      \item expanded batch insertion constructors for sparse matrices to add values at repeated locations
      \item faster handling of subvectors by \code{dot()}
      \item faster handling of aliasing by submatrix views
    }
    \item Corrected a bug (found by the g++ Address Sanitizer) in sparse
    matrix initialization where space for a sentinel was allocated, but
    the sentinel was not set; with extra thanks to Ryan Curtin for help
    \item Added a few unit tests for sparse matrices
  }
}

\section{Changes in RcppArmadillo version 0.4.320.0 (2014-07-03)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.320 (Daintree Tea Raider)
    \itemize{
      \item expanded \code{eigs_sym()} and \code{eigs_gen()} to use an
      optional tolerance parameter
      \item expanded \code{eig_sym()} to automatically fall back to standard
      decomposition method if divide-and-conquer fails
      \item automatic installer enables use of C++11 random number generator
      when using gcc 4.8.3+ in C++11 mode
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.300.8.0 (2014-05-31)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.300.8 (Medieval Cornea Scraper)
    \itemize{
      \item More robust \code{norm}-related functions
      \item Fixes between interactions between \code{cube} and
      \code{vector} types.
    }
    \item Adds a \code{#define ARMA_DONT_USE_CXX11} to provide an option
    to turn C++11 off for Armadillo (but client packages may still use it)
    \item More robust Windows detection by using \code{_WIN32} as
    well as \code{WIN32} as the latter gets diabled by MinGW with C++11
    \item On Windows, C++11 is turned off as the Armadillo code base uses
    more features of C++11 than g++ 4.6.2 version in Rtools implements
  }
}

\section{Changes in RcppArmadillo version 0.4.300.5.0 (2014-05-19)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.300.5 (Medieval Cornea Scraper)
    \itemize{
      \item Handle possible underflows and overflows in \code{norm()},
      \code{normalise()}, \code{norm_dot()}
      \item Fix for handling of null vectors by \code{norm_dot()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.300.2.0 (2014-05-13)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.300.2 (Medieval Cornea Scraper)
    \itemize{
      \item faster \code{find()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.300.0 (2014-05-04)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.300 (Medieval Cornea Scraper)
    \itemize{
      \item faster \code{find()}
      \item added \code{find_finite()} and \code{find_nonfinite()} for
      finding indices of finite and non-finite elements
      \item expressions \code{X=inv(A)*B*C} and \code{X=A.i()*B*C} are
      automatically converted to X=solve(A,B*C)
    }
    \item Corrected conversion to \code{unsigned int} vectors and
    matrices
    \item Configure script now checks for R version 3.0.3 or newer to
    enable complex divide-and-conquer SVD in case of R-supplied LAPACK
  }
}

\section{Changes in RcppArmadillo version 0.4.200.0 (2014-04-07)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.200 (Flintlock Swoop)
    \itemize{
      \item faster transpose of sparse matrices
      \item more efficient handling of aliasing during matrix multiplication
      \item faster inverse of matrices marked as diagonal
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.100.2 (2014-03-07)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.100.2
    \itemize{
      \item fix for handling null vectors by \code{normalise()}
      \item fix for memory handling by sparse matrices
    }
    \item Correct use of \code{[[ depends()]]} in skeleton example file
    \item Prepare \code{src/Makevars} for C++11 support from R 3.1.0 by
    defining \code{USE_CXX11} which is currently commented out
    \item In the Armadillo configuration, turn on C++11 support if
    \code{USE_CXX11} is defined
  }
}

\section{Changes in RcppArmadillo version 0.4.100.0 (2014-02-28)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.100.0 (Dirt Cruiser)
    \itemize{
      \item added \code{normalise()} for normalising vectors to unit p-norm
      \item extended the \code{field} class to handle 3D layout
      \item extended \code{eigs_sym()} and \code{eigs_gen()} to obtain
      eigenvalues of various forms (eg. largest or smallest magnitude)
      \item automatic SIMD vectorisation of elementary expressions
      (eg. matrix addition) when using Clang 3.4+ with -O3 optimisation
      \item faster handling of sparse submatrix views
      \item workaround for a bug in LAPACK 3.4
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.000.4 (2014-02-19)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.000.4
    \itemize{
      \item fix for \code{randi()} generating out-of-interval values
      \item fix for saving field objects
      \item workaround for a bug in the Intel compiler
    }
    \item Updated for \CRANpkg{Rcpp} (>= 0.11.0) by removing linking step
    from build process, added appropriate dependency on \CRANpkg{Rcpp}
    \item Updated \code{RcppArmadillo.package.skeleton} function
    accordingly to use proper \code{NAMESPACE} import
    \item Rewritten \code{rcpparma_hello_world} (which is used by the
    \code{RcppArmadillo.package.skeleton} function) to use Rcpp
    Attributes, and added more examples
    \item Added two functions to set Armadillo's RNGs (ie the system
    RNG) from a given value and to randomize it, as suggested by Gábor
    Csárdi. Note that these do not work within RStudio (which itself
    also uses the same system RNG).
  }
}

\section{Changes in RcppArmadillo version 0.4.000.2 (2014-01-21)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.000.2
    \itemize{
      \item fix for \code{randi()} generating out-of-interval values
      \item workaround for a bug in the Intel compiler
    }
  }
}

\section{Changes in RcppArmadillo version 0.4.000 (2014-01-05)}{
  \itemize{
    \item Upgraded to Armadillo release Version 4.000 (Feral Steamroller)
    \itemize{
      \item added eigen decompositions of sparse matrices:
      \code{eigs_sym()} and \code{eigs_gen()} [ but this requires
      linking against ARPACK which \CRANpkg{RcppArmadillo} as a
      pure-template package does not do, and R is not linked against
      ARPACK either. ]
      \item added eigen decomposition for pair of matrices: \code{eig_pair()}
      \item added simpler forms of \code{eig_gen()}
      \item added condition number of matrices: \code{cond()}
      \item expanded \code{find()} to handle cubes
      \item expanded subcube views to access elements specified in a vector
      \item template argument for \code{running_stat_vec} expanded to
      accept vector types
      \item more robust fast inverse of 4x4 matrices
      \item faster divide-and-conquer decompositions are now used by
      default for \code{eig_sym()}, \code{pinv()}, \code{princomp()},
      \code{rank()}, \code{svd()}, \code{svd_econ()}
      \item the form \code{inv(sympd(X))} no longer assumes that X is
      positive definite; use \code{inv_sympd()} instead
      \item added MEX connector for interfacing Octave/Matlab with
      Armadillo matrices (contributed by George Yammine)
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.930.1 (2013-12-09)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.930.1
    \itemize{
      \item Armadillo falls back to standard complex svd if the more
      performant divide-and-conquer variant is unavailable
    }
    \item Added detection for Lapack library and distinguish between R's
    own version (withhout zgesdd) and system Lapack; a preprocessor define is
    set accordingly
  }
}

\section{Changes in RcppArmadillo version 0.3.930.0 (2013-12-06)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.930 ("Dragon's Back")
    \itemize{
      \item added divide-and-conquer variant of \code{svd_econ()}, for
      faster SVD
      \item added divide-and-conquer variant of \code{pinv()}, for
      faster pseudo-inverse
      \item added element-wise variants of \code{min()} and \code{max()}
      \item added \code{size()} based specifications of submatrix view
      sizes
      \item added \code{randi()} for generating matrices with random
      integer values
      \item added more intuitive specification of sort direction in
      \code{sort()} and \code{sort_index()}
      \item added more intuitive specification of method in
      \code{det()}, \code{.i()}, \code{inv()} and \code{solve()}
      \item added more precise timer for the \code{wall_clock} class
      when using C++11
    }
    \item New unit tests for complex matrices and vectors
  }
}

\section{Changes in RcppArmadillo version 0.3.920.3 (2013-11-20)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.920.3
    \itemize{
      \item fix for handling of tiny matrices by \code{.swap()}
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.920.1 (2013-09-27)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.920.1 (Agencia
    Nacional Stasi)
    \itemize{
      \item faster \code{.zeros()}
      \item faster \code{round()}, \code{exp2()} and \code{log2()} when using C++11
      \item added signum function: \code{sign()}
      \item added move constructors when using C++11
      \item added 2D fast Fourier transform: \code{fft2()}
      \item added \code{.tube()} for easier extraction of vectors and subcubes from
      cubes
      \item added specification of a fill type during construction of Mat, Col,
      Row and Cube classes, eg. \code{mat X(4, 5, fill::zeros)}
    }
    \item Initial implementation of \code{wrap<subview>}
    \item Improved implementation of \code{as<>()} and \code{wrap()}
    for sparse matrices
    \item Converted main vignette from \code{LaTeX} style \code{minted}
    to \code{lstlisting} which permits builds on CRAN; removed
    set \code{BuildVignettes: FALSE}.
  }
}

\section{Changes in RcppArmadillo version 0.3.910.0 (2013-08-12)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.910.0 (Pyrenees)
    \itemize{
      \item faster multiplication of a matrix with a transpose of
      itself, ie. \code{X*X.t()} and \code{X.t()*X}
      \item added \code{vectorise()} for reshaping matrices into vectors
      \item added \code{all()} and \code{any()} for indicating presence
      of elements satisfying a relational condition
    }
    \item Added conversion support for sparse matrices (of type double)
    created by the \CRANpkg{Matrix} package as class \code{dgCMatrix}
    \item Moved vignette sources from \code{inst/doc} to \code{vignettes};
    set \code{BuildVignettes: FALSE} as the \code{minted} mode for
    \code{LaTeX} upsets the CRAN builders.
  }
}

\section{Changes in RcppArmadillo version 0.3.900.7 (2013-08-02)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.900.7 (Bavarian
    Sunflower)
    \itemize{
      \item minor fix for inplace \code{reshape()}
      \item minor corrections for compilation issues under GCC 4.8+ and MSVC
    }
    \item Corrected setting of \code{vec_stats} in intialization of row,
    vector and matrix objects
    \item The \pkg{inline} package is no longer used in the examples and
    unit tests which have all been converted to using Rcpp attributes
  }
}

\section{Changes in RcppArmadillo version 0.3.900 (2013-06-04)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.900.0 (Bavarian
    Sunflower)
    \itemize{
      \item added automatic SSE2 vectorisation of elementary expressions
      (eg. matrix addition) when using GCC 4.7+ with -O3 optimisation
      \item added support for saving & loading of cubes in HDF5 format,
      contributed by Szabolcs Horvat
      \item faster \code{median()}, contributed by Ruslan Shestopalyuk
      \item faster handling of compound expressions with transposes of
      submatrix rows
      \item faster handling of compound expressions with transposes of
      complex vectors
    }
    \item Kalman filter example switched from inline to \code{sourceCpp},
    which simplifies / streamlines the C++ side a little
  }
}

\section{Changes in RcppArmadillo version 0.3.820 (2013-05-12)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.820 (Mt Cootha)
    \itemize{
      \item faster \code{as_scalar()} for compound expressions
      \item faster transpose of small vectors
      \item faster matrix-vector product for small vectors
      \item faster multiplication of small fixed size matrices
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.810.2 (2013-04-30)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.810.2
    \itemize{
      \item minor fix for initialisation of sparse matrices
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.810.0 (2013-04-19)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.810.0 (Newell Highway)
    \itemize{
      \item added fast Fourier transform: \code{fft()}
      \item added handling of \code{.imbue()} and \code{.transform()} by
      submatrices and subcubes
      \item added batch insertion constructors for sparse matrices
      \item minor fix for multiplication of complex sparse matrices
    }
    \item Updated sample() function and test again contributed by
    Christian Gunning
  }
}

\section{Changes in RcppArmadillo version 0.3.800.1 (2013-03-12)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.800.1 (Miami Beach)
    \itemize{
      \item workaround for a bug in ATLAS 3.8 on 64 bit systems
      \item faster matrix-vector multiply for small matrices
    }
    \item Added new sample() function and tests contributed by Christian Gunning
    \item Refactored unit testing code for faster unit test performance
  }
}

\section{Changes in RcppArmadillo version 0.3.800.0 (2013-03-01)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.800.0 (Miami Beach)
    \itemize{
      \item Armadillo is now licensed using the Mozilla Public License 2.0
      \item added \code{.imbue()} for filling a matrix/cube with values provided by a functor or lambda expression
      \item added \code{.swap()} for swapping contents with another matrix
      \item added \code{.transform()} for transforming a matrix/cube using a functor or lambda expression
      \item added \code{round()} for rounding matrix elements towards nearest integer
      \item faster \code{find()}
      \item fixes for handling non-square matrices by \code{qr()} and \code{qr_econ()}
      \item minor fixes for handling empty matrices
      \item reduction of pedantic compiler warnings
    }
    \item Updated vignette to paper now in press at CSDA
    \item Added CITATION file with reference to CSDA paper
  }
}

\section{Changes in RcppArmadillo version 0.3.6.3 (2013-02-20)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.6.3
    \itemize{
      \item faster \code{find()}
      \item minor fix for non-contiguous submatrix views to handle empty vectors of indices
      \item reduction of pedantic compiler warnings
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.6.2 (2013-01-29)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.6.2
    \itemize{
      \item faster determinant for matrices marked as diagonal or triangular
      \item more fine-grained handling of 64 bit integers
    }
    \item Added a new example of a Kalman filter implementation in R, and C++
    using Armadillo via RcppArmadillo, complete with timing comparison
  }
}

\section{Changes in RcppArmadillo version 0.3.6.1 (2012-12-17)}{
  \itemize{
    \item Upgraded to Armadillo release Version 3.6.1 (Piazza del Duomo)
    \itemize{
      \item faster \code{trace()}
      \item fix for handling sparse matrices by \code{dot()}
      \item fixes for interactions between sparse and dense matrices
    }
    \item Now throws compiler error if \code{Rcpp.h} is included before
    \code{RcppArmadillo.h} (as the former is included automatically by the
    latter anyway, but template logic prefers this ordering).
  }
}

\section{Changes in RcppArmadillo version 0.3.4.3 (2012-10-04)}{
  \itemize{
    \item Upgraded to Armadillo release 3.4.3
    \itemize{
      \item fix for aliasing issue in \code{diagmat()}
      \item fix for \code{speye()} signature
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.4.2 (2012-09-25)}{
  \itemize{
    \item Upgraded to Armadillo release 3.4.2
    \itemize{
      \item minor fixes for handling sparse submatrix views
      \item minor speedups for sparse matrices
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.4.1 (2012-09-18)}{
  \itemize{
    \item Upgraded to Armadillo release 3.4.1
    \itemize{
      \item workaround for a bug in the Mac OS X accelerate framework
      \item fixes for handling empty sparse matrices
      \item added documentation for saving & loading matrices in HDF5 format
      \item faster dot() and cdot() for complex numbers
    }
  }
}

\section{Changes in RcppArmadillo version 0.3.4.0 (2012-09-06)}{
  \itemize{
    \item Upgraded to Armadillo release 3.4.0 (Ku De Ta)
    \itemize{
      \item added economical QR decomposition: qr_econ()
      \item added .each_col() & .each_row() for vector operations repeated on each column or row
      \item added preliminary support for sparse matrices, contributed by Ryan Curtin et al. (Georgia Institute of Technology)
      \item faster singular value decomposition via divide-and-conquer algorithm
      \item faster .randn()
    }
    \item NEWS file converted to Rd format
  }
}
\section{Changes in RcppArmadillo version 0.3.3.91 (2012-08-30)}{
  \itemize{
    \item Upgraded to Armadillo release 3.3.91
    \itemize{
      \item faster singular value decomposition via "divide and conquer" algorithm
      \item added economical QR decomposition: qr_econ()
      \item added .each_col() & .each_row() for vector operations repeated on each column or row
      \item added preliminary support for sparse matrices, contributed by Ryan Curtin, James Cline and Matthew Amidon (Georgia Institute of Technology)
    }
    \item Corrected summary method to deal with the no intercept case when using a formula; also display residual summary() statistics
    \item Expanded unit tests for fastLm
  }
}
\section{Changes in RcppArmadillo version 0.3.2.4 (2012-07-11)}{
  \itemize{
    \item Upgraded to Armadillo release 3.2.4
    \itemize{
      \item workaround for a regression (bug) in GCC 4.7.0 and 4.7.1
    }
  }
}
\section{Changes in RcppArmadillo version 0.3.2.3 (2012-07-01)}{
  \itemize{
    \item Upgraded to Armadillo release 3.2.3
    \itemize{
      \item minor correction for declaration of fixed size vectors and matrices
      \item Reverted three header files \{Mat,Row,Col\}_bones.hpp back to previous release due to compilation failures under g++-4.7
      \item Added new vignette 'RcppArmadillo-intro' based on a just-submitted introductory paper (by Eddelbuettel and Sanderson) about RcppArmadillo
      \item Change from release 3.2.2 which we skipped as it did not really affect builds under R:
      \itemize{
	\item minor fix for compiling without debugging enabled (aka release mode)
	\item better detection of ATLAS during installation on Fedora and Red Hat systems
      }
      \item Small enhancement to fastLm
    }
  }
}
\section{Changes in RcppArmadillo version 0.3.2.0 (2012-05-21)}{
  \itemize{
    \item Upgraded to Armadillo release 3.2.0 "Creamfields"
    \itemize{
      \item faster eigen decomposition via "divide and conquer" algorithm
      \item faster transpose of vectors and compound expressions
      \item faster handling of diagonal views
      \item faster handling of tiny fixed size vectors (≤ 4 elements)
      \item added unique(), for finding unique elements of a matrix
    }
  }
}
\section{Changes in RcppArmadillo version 0.3.1.94 (2012-05-15)}{
  \itemize{
    \item Upgraded to Armadillo release 3.1.94 "v3.2 beta 2"
    \itemize{
      \item added unique(), for finding unique elements of a matrix
      \item faster eigen decomposition via "divide and conquer" algorithm
      \item faster transpose of vectors and compound expressions
      \item faster handling of tiny fixed size vectors (≤ 4 elements)
    }
  }
}
\section{Changes in RcppArmadillo version 0.3.1.92 (2012-05-10)}{
  \itemize{
    \item Upgraded to Armadillo release 3.1.92 "v3.2 beta 2"
    \itemize{
      \item added unique(), for finding unique elements of a matrix
      \item faster eigen decomposition via optional use of "divide and conquer" by eig_sym()
      \item faster transpose of vectors and compound expressions
    }
  }
}
\section{Changes in RcppArmadillo version 0.3.0.3 (2012-05-03)}{
  \itemize{
    \item Upgraded to Armadillo release 3.0.3
    \itemize{
      \item fixes for inplace transpose of complex number matrices
      \item fixes for complex number version of svd_econ()
      \item fixes for potential aliasing issues with submatrix views
    }
    \item New example script fastLm
  }
}
\section{Changes in RcppArmadillo version 0.3.0.2 (2012-04-19)}{
  \itemize{
    \item Upgraded to Armadillo release 3.0.2
    \itemize{
      \item fixes for handling diagonal matrices
    }
    \item Undefine NDEBUG if it has been set (as R does) as this prevents a number of useful debugging checks. Users can still define it or define ARMA_NO_DEBUG if they want a 'non-development' build
  }
}
\section{Changes in RcppArmadillo version 0.3.0.1 (2012-04-12)}{
  \itemize{
    \item Upgraded to Armadillo release 3.0.1
    \itemize{
      \item fixes for compilation errors
      \item fixes for potential aliasing issues
    }
  }
}
\section{Changes in RcppArmadillo version 0.3.0 (2012-04-10)}{
  \itemize{
    \item Upgraded to Armadillo release 3.0.0 "Antarctic Chilli Ranch"
    \itemize{
      \item added non-contiguous submatrix views
      \item added shorthand for inverse: .i()
      \item added hist() and histc()
      \item faster repmat()
      \item faster handling of submatrix views with one row or column
      \item faster generation of random numbers
      \item faster element access in fixed size matrices
      \item better detection of vector expressions by sum(), cumsum(), prod(), min(), max(), mean(), median(), stddev(), var()
      \item expressions X=A.i()*B and X=inv(A)*B are automatically converted to X=solve(A,B)
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.40 (2012-04-04)}{
  \itemize{
    \item Upgraded to Armadillo release 2.99.4 "Antarctic Chilli Ranch (Beta 4)"
    \itemize{
      \item fixes for handling expressions with fixed size matrices
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.39 (2012-04-02)}{
  \itemize{
    \item Upgraded to Armadillo release 2.99.3 "Antarctic Chilli Ranch (Beta 3)"
    \itemize{
      \item faster repmat()
      \item workarounds for braindead compilers (eg. Visual Studio)
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.38 (2012-03-28)}{
  \itemize{
    \item Upgraded to Armadillo release 2.99.2 "Antarctic Chilli Ranch (Beta 2)"
    \itemize{
      \item added .i()
      \item much faster handling of .col() and .row()
      \item expressions X=A.i()*B and X=inv(A)*B are automatically converted to X=solve(A,B)
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.37 (2012-03-19)}{
  \itemize{
    \item Upgraded to Armadillo release 2.99.1 "Antarctic Chilli Ranch (Beta 1)"
    \itemize{
      \item added non-contiguous submatrix views
      \item added hist() and histc()
      \item faster handling of submatrix views
      \item faster generation of random numbers
      \item faster element access in fixed size matrices
      \item better detection of vector expressions by sum(), cumsum(), prod(), min(), max(), mean(), median(), stddev(), var()
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.36 (2012-03-05)}{
  \itemize{
    \item Upgraded to Armadillo release 2.4.4
    \itemize{
      \item fixes for qr() and syl()
      \item more portable wall_clock class
      \item faster relational operators on submatrices
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.35 (2012-02-17)}{
  \itemize{
    \item Upgraded to Armadillo release 2.4.3
    \itemize{
      \item Support for ARMA_DEFAULT_OSTREAM using Rcpp::Rcout added
      \item Minor bug fix release improving corner cases affecting builds:
      \itemize{
	\item Missing semicolon added in Mat_meat (when in C++0x mode), with thanks to Teo Guo Ci
	\item Armadillo version vars now instantiated in RcppArmadillo.cpp which helps older g++ versions, with thanks to Gershon Bialer
	\item Thanks also to Martin Renner for testing these changes
	\item Unit tests output fallback directory changed per Brian Ripley's request to not ever use /tmp
	\item Minor update to version numbers in RcppArmadillo-package.Rd
      }
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.34 (2011-12-12)}{
  \itemize{
    \item Upgraded to Armadillo release 2.4.2
    \itemize{
      \item clarified documentation for .reshape()
      \item fix for handling of empty matrices by .resize()
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.33 (2011-12-07)}{
  \itemize{
    \item Upgraded to Armadillo release 2.4.1
    \itemize{
      \item added .resize()
      \item fix for vector initialisation
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.32 (2011-12-04)}{
  \itemize{
    \item Upgraded to Armadillo test release 2.4.0 "Loco Lounge Lizard"
    \item Minimal changes relative to 0.2.31 based on 2.3.92, next section is relative to the previous stable release series 2.2.* of Armadillo
    \itemize{
      \item added shorter forms of transposes: .t() and .st()
      \item added optional use of 64 bit indices, allowing matrices to have more than 4 billion elements
      \item added experimental support for C++11 initialiser lists
      \item faster pinv()
      \item faster inplace transpose
      \item faster handling of expressions with diagonal views
      \item fixes for handling expressions with aliasing and submatrices
      \item fixes for linking on Ubuntu and Debian systems
      \item fixes for inconsistencies in interactions between matrices and cubes
      \item refactored code to eliminate warnings when using the Clang C++ compiler
      \item .print_trans() and .raw_print_trans() are deprecated
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.31 (2011-11-28)}{
  \itemize{
    \item Upgraded to Armadillo test release 2.3.92 "Loco Lounge Lizard (Beta 2)"
    \itemize{
      \item fixes for linking on Ubuntu and Debian systems
      \item fixes for inconsistencies in interactions between matrices and cubes
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.30 (2011-11-19)}{
  \itemize{
    \item Upgraded to Armadillo test release 2.3.91 "Loco Lounge Lizard (Beta 1)"
    \itemize{
      \item added shorter forms of transposes: .t() and .st()
      \item added optional use of 64 bit indices, allowing matrices to have more than 4 billion elements
      \item added experimental support for C++11 initialiser lists
      \item faster pinv()
      \item faster inplace transpose
      \item bugfixes for handling expressions with aliasing and submatrices
      \item refactored code to eliminate warnings when using the Clang C++ compiler
      \item .print_trans() and .raw_print_trans() are deprecated
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.29 (2011-09-01)}{
  \itemize{
    \item Upgraded to Armadillo release 2.2.3
    \itemize{
      \item Release fixes a speed issue in the as_scalar() function.
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.28 (2011-08-02)}{
  \itemize{
    \item Upgraded to Armadillo release 2.2.1 "Blue Skies Debauchery"
    \itemize{
      \item faster multiplication of small matrices
      \item faster trans()
      \item faster handling of submatrices by norm()
      \item added economical singular value decomposition: svd_thin()
      \item added circ_toeplitz()
      \item added .is_colvec() & .is_rowvec()
      \item fixes for handling of complex numbers by cov(), cor(), running_stat_vec
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.27 (2011-07-22)}{
  \itemize{
    \item Upgraded to Armadillo release 2.1.91 "v2.2 beta 1"
    \itemize{
      \item faster multiplication of small matrices
      \item faster trans()
      \item faster handling of submatrices by norm()
      \item added economical singular value decomposition: svd_thin()
      \item added circ_toeplitz()
      \item added .is_colvec() & .is_rowvec()
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.26 (2011-07-17)}{
  \itemize{
    \item Upgraded to Armadillo release 2.0.2
    \itemize{
      \item fix for handling of conjugate transpose by as_scalar()
      \item fix for handling of aliasing by diagmat()
      \item fix for handling of empty matrices by symmatu()/symmatl()
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.25 (2011-06-30)}{
  \itemize{
    \item Upgraded to Armadillo 2.0.1 which fixes two minor compilation issues
  }
}
\section{Changes in RcppArmadillo version 0.2.24 (2011-06-29)}{
  \itemize{
    \item Upgraded to Armadillo release 2.0.0 "Carnivorous Sugar Glider"
    \itemize{
      \item faster multiplication of tiny matrices (≤ 4x4)
      \item faster compound expressions containing submatrices
      \item faster inverse of symmetric positive definite matrices
      \item faster element access for fixed size matrices
      \item added handling of arbitrarily sized empty matrices (eg. 5x0)
      \item added loading & saving of matrices as CSV text files
      \item added .count() member function to running_stat and running_stat_vec
      \item added syl(), strans(), symmatu()/symmatl()
      \item added submatrices of submatrices
      \item det(), inv() and solve() can be forced to use more precise
      \item algorithms for tiny matrices (≤ 4x4)
      \item htrans() has been deprecated; use trans() instead
      \item API change: trans() now takes the complex conjugate when transposing a complex matrix
      \item API change: .is_vec() now outputs true for empty vectors (eg. 0x1)
      \item API change: forms of chol(), eig_sym(), eig_gen(), inv(), lu(), pinv(), princomp(), qr(), solve(), svd(), syl() that do not return a bool indicating success now throw std::runtime_error exceptions when failures are detected
      \item API change: princomp_cov() has been removed; princomp() in conjunction with cov() can be used instead
      \item API change: set_log_stream() & get_log_stream() have been replaced by set_stream_err1() & get_stream_err1()
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.23 (2011-06-23)}{
  \itemize{
    \item Upgraded to Armadillo release 1.99.5 "v2.0 beta 5"
    \itemize{
      \item Forms of chol(), eig_sym(), eig_gen(), inv(), lu(), pinv(), princomp(), qr(), solve(), svd(), syl() that do not return a bool indicating success now throw std::runtime_error exceptions when failures are detected
      \item princomp_cov() has been removed; princomp() in conjunction with cov() can be used instead
      \item set_log_stream() & get_log_stream() have been replaced by set_stream_err1() & get_stream_err1()
      \item det(), inv() and solve() can be forced to use more precise algorithms for tiny matrices (≤ 4x4)
      \item Added loading & saving of matrices as CSV text files
    }
    \item fastLmPure() now uses same argument order as R's lm.fit()
    \item Export and document S3 methods in NAMESPACE and manual page as such
  }
}
\section{Changes in RcppArmadillo version 0.2.22 (2011-06-06)}{
  \itemize{
    \item Upgraded to Armadillo release 1.99.4 "v2.0 beta 4"
    \itemize{
      \item fixes for handling of tiny matrices
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.21 (2011-05-27)}{
  \itemize{
    \item Upgraded to Armadillo release 1.99.3 "v2.0 beta 3"
    \itemize{
      \item stricter size checking for row and column vectors
      \item added .count() member function to running_stat and running_stat_vec
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.20 (2011-05-25)}{
  \itemize{
    \item Upgraded to Armadillo release 1.99.2 "v2.0 beta 2" (and 1.99.1 before)
    \itemize{
      \item faster inverse of symmetric matrices
      \item faster element access for fixed size matrices
      \item faster multiplication of tiny matrices (eg. 4x4)
      \item faster compund expressions containing submatrices
      \item added handling of arbitrarily sized empty matrices (eg. 5x0)
      \item added syl()
      \item added strans()
      \item added symmatu()/symmatl()
      \item added submatrices of submatrices
      \item htrans() has been deprecated; use trans() instead
      \item trans() now takes the complex conjugate when transposing a complex matrix
      \item .is_vec() now outputs true for empty matrices
      \item most functions with matrix inputs no longer throw exceptions when given empty matrices (eg. 5x0)
    }
    \item Added a new subdirectory examples/ seeded with a nice Vector Autoregression simulation simulation example by Lance Bachmeier
    \item Rewrote armadillo_version as to no longer require an instance of arma::arma_version, with tanks to Conrad for the suggestion
  }
}
\section{Changes in RcppArmadillo version 0.2.19 (2011-04-18)}{
  \itemize{
    \item Upgraded to Armadillo version 1.2.0 "Unscrupulous Carbon Emitter"
    \itemize{
      \item Added ability to use Blas & Lapack libraries with capitalised function names
      \item Reduction of pedantic compiler warnings
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.18 (2011-04-03)}{
  \itemize{
    \item Upgraded to Armadillo version 1.1.92  "Jurassic Barbecue"
    \itemize{
      \item Bugfix in cor()
      \item Automatic installation now requires CMake >= 2.6
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.17 (2011-03-22)}{
  \itemize{
    \item Upgraded to Armadillo version 1.1.90  "Inside Job"
    \itemize{
      \item Added .min() & .max(), which can provide the extremum's location
      \item More robust mean(), var(), stddev()
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.16 (2011-03-10)}{
  \itemize{
    \item Upgraded to Armadillo version 1.1.8   "Kangaroo Steak"
    \itemize{
      \item Added floor() and ceil()
      \item Added “not a number”: math::nan()
      \item Added infinity: math::inf()
      \item Added standalone is_finite()
      \item Faster min(), max(), mean()
      \item Bugfix for a corner case with NaNs in min() and max()
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.15 (2011-03-04)}{
  \itemize{
    \item Upgraded to Armadillo version 1.1.6   “Baby Carpet Shark”
    \itemize{
      \item fixed size matrices and vectors can use auxiliary (external) memory
      \item .in_range() can use span() arguments
      \item subfields can use span() arguments
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.14 (2011-03-02)}{
  \itemize{
    \item Support Run-Time Type Information (RTTI) on matrices by setting the state variable vec_state in Row and Col instantiation, with thanks to Conrad Sanderson for the hint
    \item fastLm code simplified further by instantiating the Armadillo matrix and vector directly from the SEXP coming from R
    \item inst/doc/Makefile now respects $R_HOME environment variable
  }
}
\section{Changes in RcppArmadillo version 0.2.13 (2011-02-18)}{
  \itemize{
    \item Upgraded to Armadillo version 1.1.4   “Manta Lodge”
    \itemize{
      \item Faster sort()
      \item Updated installation to detect recent versions of Intel's MKL
      \item Added interpretation of arbitrary "flat" subcubes as matrices
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.12 (2011-02-15)}{
  \itemize{
    \item Upgraded to Armadillo version 1.1.2   “Flood Kayak”
    \itemize{
      \item Faster prod()
      \item Faster solve() for compound expressions
      \item Fix for compilation using GCC's C++0x mode
      \item Fix for matrix handling by subcubes
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.11 (2011-01-06)}{
  \itemize{
    \item Upgraded to Armadillo version 1.1.0   “Climate Vandal”
    \itemize{
      \item Extended submatrix views, including access to elements whose indices are specified in a separate vector
      \item Added handling of raw binary files by save/load functions
      \item Added cumsum()
      \item Added interpretation of matrices as triangular via trimatu()/trimatl()
      \item Faster solve(), inv() via explicit handling of triangular matrices
      \item The stream for logging of errors and warnings can now be changed
    }
    \item New unexported R function SHLIB, a small wrapper around R CMD SHLIB, which can be used as   Rscript -e "RcppArmadillo:::SHLIB('foo.cpp')"
  }
}
\section{Changes in RcppArmadillo version 0.2.10 (2010-11-25)}{
  \itemize{
    \item Upgraded to Armadillo 1.0.0 "Antipodean Antileech"
    \itemize{
      \item After 2 1/2 years of collaborative development, we are proud to release the 1.0 milestone version.
      \item Many thanks are extended to all contributors and bug reporters.
    }
    \item R/RcppArmadillo.package.skeleton.R: Updated to no longer rely on GNU make for builds of packages using RcppArmadillo
    \item summary() for fastLm() objects now returns r.squared and adj.r.squared
  }
}
\section{Changes in RcppArmadillo version 0.2.9 (2010-11-11)}{
  \itemize{
    \item Upgraded to Armadillo 0.9.92 "Wall Street Gangster":
    \itemize{
      \item Fixes for compilation issues under the Intel C++ compiler
      \item Added matrix norms
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.8 (2010-10-16)}{
  \itemize{
    \item Upgraded to Armadillo 0.9.90 "Water Dragon":
    \itemize{
      \item Added unsafe_col()
      \item Speedups and bugfixes in lu()
      \item Minimisation of pedantic compiler warnings
    }
    \item Switched NEWS and ChangeLog between inst/ and the top-level directory so that NEWS (this file) gets installed with the package
  }
}
\section{Changes in RcppArmadillo version 0.2.7 (2010-09-25)}{
  \itemize{
    \item Upgraded to Armadillo 0.9.80 "Chihuahua Muncher":
    \itemize{
      \item Added join_slices(), insert_slices(), shed_slices()
      \item Added in-place operations on diagonals
      \item Various speedups due to internal architecture improvements
    }
  }
}
\section{Changes in RcppArmadillo version 0.2.6 (2010-09-12)}{
  \itemize{
    \item Upgraded to Armadillo 0.9.70 "Subtropical Winter Safari"
    \item arma::Mat, arma::Row and arma::Col get constructor that take vector or matrix sugar expressions. See the unit test "test.armadillo.sugar.ctor" and "test.armadillo.sugar.matrix.ctor" for examples.
  }
}
\section{Changes in RcppArmadillo version 0.2.5 (2010-08-05)}{
  \itemize{
    \item Upgraded to Armadillo 0.9.60 "Killer Bush Turkey"
  }
}
\section{Changes in RcppArmadillo version 0.2.4 (2010-07-27)}{
  \itemize{
    \item Upgraded to Armadillo 0.9.52 'Monkey Wrench'
    \item src/fastLm.cpp: Switch from inv() to pinv() as inv() now tests for singular matrices and warns and returns an empty matrix which stops the example fastLm() implementation on the manual page -- and while this is generally reasonably it makes sense here to continue which the Moore-Penrose pseudo-inverse allows us to do this
  }
}
\section{Changes in RcppArmadillo version 0.2.3 (2010-06-14)}{
  \itemize{
    \item Better configuration to detect suncc (which does not have std::isfinite)
  }
}
\section{Changes in RcppArmadillo version 0.2.2 (2010-06-09)}{
  \itemize{
    \item Added RcppArmadillo:::CxxFlags for cases where RcppArmadillo is not used via a package
    \item Upgraded to Armadillo 0.9.10 'Chilli Espresso'
    \item Wrap support for mtOp, i.e. operations involving mixed types such as a complex and an arma::mat, which have been introduced in armadillo 0.9.10
    \item Wrap support for mtGlue, i.e. operations involving matrices of mixed types such as an arma::mat and an arma::imat, which have been introduced in armadillo 0.9.10
    \item Included an inline plugin to support the plugin system introduced in inline 0.3.5. The unit tests have moved from the src directory to the unit test directory (similar to Rcpp) using cxxfunction with the RcppArmadillo plugin.
  }
}
\section{Changes in RcppArmadillo version 0.2.1 (2010-05-19)}{
  \itemize{
    \item Bug-fix release permitting compilation on Windows
  }
}
\section{Changes in RcppArmadillo version 0.2.0 (2010-05-18)}{
  \itemize{
    \item fastLm() is now generic and has a formula interface as well as methods for print, summary, predict to behave like a standard model fitting function
    \item Armadillo sources (using release 0.9.8) are now included in the package using a standardized build suitable for our purposes (not assuming Boost or Atlas) -- see ?RcppArmadillo for details
    \item New R function RcppArmadillo.package.skeleton, similar to Rcpp::Rcpp.package.skeleton, but targetting use of RcppArmadillo
  }
}
\section{Changes in RcppArmadillo version 0.1.0 (2010-03-11)}{
  \itemize{
    \item the fastLm() implementation of a bare-bones lm() fit (using Armadillo's solve() function) provides an example of how efficient code can be written compactly using the combination of Rcpp, RcppAramadillo and Armadillo
    \item support for Rcpp implicit wrap of these types : Mat<T>, Col<T>, Row<T>, Cube<T> where T is one of : int, unsigned int, double, float
    \item support for Rcpp implicit as of these types : Mat<T>, Col<T>, Row<T> where R is one of : int, unsigned int, double, float
  }
}
