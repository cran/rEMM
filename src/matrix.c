#include <R.h>
#include <Rdefines.h>
#include "matrix_pos.h"
#include <string.h>	    /* for memcpy */


/*
 * clears (sets to 0) cells in matrix given by row and col indices
 */

SEXP clear_matrix(SEXP R_matrix, SEXP R_rows, SEXP R_cols) {

    SEXP res;

    int *rows = INTEGER(R_rows);
    int *cols = INTEGER(R_cols);
    int len = LENGTH(R_cols);

    double *mat;
    int n = INTEGER(getAttrib(R_matrix, R_DimSymbol))[0];
    int m = INTEGER(getAttrib(R_matrix, R_DimSymbol))[1];
    //int max = n*m;

    int i=0;

    if (len != LENGTH(R_rows)) error("Length of row and col does not match!");

    /* copy matrix */
    PROTECT(res = allocMatrix(REALSXP, n, m));
    memcpy(REAL(res), REAL(R_matrix), sizeof(double)*LENGTH(R_matrix));

    mat = REAL(res);

    for (i=0; i<len; i++) {
	if(rows[i] > n || rows[i] < 1 || cols[i] > m || cols[i] < 1) error("Index out of bounds!");
	mat[M_POS(n, rows[i]-1, cols[i]-1)] = 0.0;
    }

    UNPROTECT(1);
    return res;
}

