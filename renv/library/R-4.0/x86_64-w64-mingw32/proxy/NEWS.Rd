\name{NEWS}
\title{News for Package \pkg{proxy}}
\section{Changes in version 0.4-24}{
  \itemize{
    \item fix tests in apply.R to comply to new default behavior
    of encoding character strings in data frames
   }
}
\section{Changes in version 0.4-23}{
  \itemize{
       \item smaller changes in C code to prevent false positives
             from automated CRAN code checking
   }
}
\section{Changes in version 0.4-21.1}{
  \itemize{
    \item Change registry class name to \code{proxy_registry} to
         prevent name clashes with registry package.
  }
}
\section{Changes in version 0.4-21}{
  \itemize{
    \item  some more C-code \code{PROTECT}ing.
  }
}
\section{Changes in version 0.4-20}{
  \itemize{
    \item print method for \code{simil} objects added that prints
    \code{NA} instead of 0 for diagonal elements, if requested.
  }
}
\section{Changes in version 0.4-19}{
  \itemize{
    \item fix for binary distance (C-code): now has same behavior as
          the corresponding distance method in the \pkg{stats} package
          in case of missing values.
  }
}
\section{Changes in version 0.4-17}{
  \itemize{
    \item \code{pr_simil2dist} now computes \eqn{1 - abs(x)} instead of \eqn{1 - x}.
  }
}
\section{Changes in version 0.4-15}{
  \itemize{
    \item add imports from recommended packages.
  }
}
\section{Changes in version 0.4-14}{
  \itemize{
    \item export as.matrix() generic.
  }
}
\section{Changes in version 0.4-11}{
  \itemize{
    \item Make binary similarities work with binary data in case of
    list data (by coercing list to a matrix).
  }
}
\section{Changes in version 0.4-10}{
  \itemize{
    \item Bug in Fager/McGown measure fixed.
  }
}
\section{Changes in version 0.4-9}{
  \itemize{
    \item Author/Maintainer removed.
  }
}
\section{Changes in version 0.4-8}{
  \itemize{
    \item PACKAGE="proxy" argument added to all
    .External-calls. "PACKAGE" field added to the registry, defaulting
    to "proxy", and used by .Call().
  }
}
\section{Changes in version 0.4-7}{
  \itemize{
    \item the method argument of \code{dist()} now also accepts a
      registry entry to avoid unnecessary lookups in repeated calls to
      \code{dist()}.
  }
}
\section{Changes in version 0.4-6}{
  \itemize{
    \item bugfix: \code{names<-.dist} did not accept \code{NULL} as assignment.
  }
}
\section{Changes in version 0.4-5}{
  \itemize{
    \item small vignette with an overview of existing methods added.
  }
}
\section{Changes in version 0.4-4}{
  \itemize{
    \item Podani, Chord, geodesic, Hellinger and Whittaker dissimilarities added.
    \item Fager/McGown anf Faith similarities added.
  }
}
\section{Changes in version 0.4-3}{
  \itemize{
    \item \code{pr_dist2simil} now computes \eqn{1 / (1 + x)} instead of \eqn{1 / (1 - x)}
    \item fix labeling bug for list data with named components
    \item the C-level dist functions now conform with those in package \pkg{stats}
      with respect to NA handling (pairwise-complete observations).
  }
}
\section{Changes in version 0.4-2}{
  \itemize{
    \item \code{as.matrix.dist} by default now uses 0 for diagonal elements
  }
}
\section{Changes in version 0.4}{
  \itemize{
    \item fix a bug in Kullback-Leibler-distance
  }
}
\section{Changes in version 0.3}{
  \itemize{
    \item fixed Gower-dissimilarity
    \item fixed dist/simil subsetting
    \item self-proximities now default to NA
  }
}
\section{Changes in version 0.2}{
  \itemize{
    \item data frames are auto-converted to matrices if all variables are 
      either numeric, logical, or complex (to be compatible to 
      \code{stats::dist})
    \item pairwise distances can now be computed
    \item Registry is now case-insensitive
  }
}
\section{Changes in version 0.1}{
  \itemize{
    \item Initial release
  }
}
