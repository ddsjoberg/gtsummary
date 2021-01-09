
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

void F77_NAME(mvtdst)(int *n, int *nu, double *lower, double *upper,
                      int *infin, double *corr, double *delta,
                      int *maxpts, double *abseps, double *releps,
                      double *error, double *value, int *inform);

void F77_NAME(tvtlrcall)(int *NU, double *H, double *R, double *EPSI, double *TVTL);
void C_tvtlr            (int *NU, double *H, double *R, double *EPSI, double *TVTL);

void F77_NAME(bvtlrcall)(int *NU, double *DH, double *DK, double *R, double *BVTL);
void C_bvtlr            (int *NU, double *DH, double *DK, double *R, double *BVTL);

void C_mvtdst(int *n, int *nu, double *lower, double *upper,
              int *infin, double *corr, double *delta,
              int *maxpts, double *abseps, double *releps,
              double *error, double *value, int *inform, int *rnd);

SEXP C_miwa(SEXP steps, SEXP corr, SEXP upper, SEXP lower, SEXP infin);
