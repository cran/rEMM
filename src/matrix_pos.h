
/* M_POS to access R matrices by M. Hahsler 
 * rows ... number of rows
 * i,j ... row [0, (n-1)] and col index 
 * 
 * Note: does not cover the case i==j!
 */

/*
 * access for matrix 
 */

#ifndef M_POS
#define M_POS(rows, i, j)   \
    (i) + (j)*(rows) 
#endif


