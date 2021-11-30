#ifndef VCTRS_H
#define VCTRS_H

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

extern bool (*vec_is_vector)(SEXP);
extern R_len_t (*short_vec_size)(SEXP);
extern SEXP (*short_vec_recycle)(SEXP, R_len_t);

void vctrs_init_api();

#endif
