#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP clear_matrix(SEXP R_matrix, SEXP R_rows, SEXP R_cols);

void R_init_rEMM(DllInfo *info) {

    const R_CallMethodDef callMethods[] = {
	    {"clear_matrix", (DL_FUNC) &clear_matrix, 3},
	    {NULL, NULL, 0}
    };

    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
