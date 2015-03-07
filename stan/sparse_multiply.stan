
// This function implements the multiplication of a sparse m by n
// matrix, X, by the dense vector b.  The sparse matrix X is represented by a vector
// of values and three indexing vectors which allow for fast sparse
// matrix by dense vector multiplication.
//
// The matrix X is represented by the vector w (of values), the integer array v
// (containing one-based row index of each value), the integer array u
// (containing one-based indexes of where each column starts in w), and
// the integer array z (containing the number of non-zero entries in
// each column of w). The matrix dimensions, m and n are also passed.
//
// This outer loop runs over columns of the matrix X.  With the column
// index in hand, The inner loop runs over the section of w and v for
// each column.  The inner loop first calculates the row index 'i' for
// each non-zero value in w, and then calculates how the associated non-zero  
// value from w is scaled by the approriate entry of b to contribute to the
// appropriate entry of the output column vector.
//
// The function works by incrementing the entries of the entries of the
// output vector, y, because the outer loop is iterating column-wise
// over X.


vector sparse_multiply(
	int m, int n,
	vector w, int[] v, int[] u, int[] z,
	vector b
) {
	vector[m] y;
	y <- rep_vector(0.0,m);
	for (j in 1:n) {
		int i;
		for (q in u[j]:(u[j]+z[j]-1)) {
			i <- v[q]; 
			y[i] <- y[i] + w[q] * b[j];
		}
	}
	return y;
}










