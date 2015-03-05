
dgCMatrix_to_Eigen <- function(X) {
	o <- list(
		values = X@x,
		inner_indices = X@i,
		outer_starts = X@p,
		inner_nnzs = diff(X@p)
	)
	names(o) <- c('w','v','u','z')
	o[['m']] <- dim(X)[1]
	o[['n']] <- dim(X)[2]
	return(o)
}

## This is how sparse matrix by dense vector multiplication
## can be implemented for the CSC format in Eigen.  
Eigen_format_multiplication <- function(X,b) {
	Xb <- vector(mode='numeric', length=X$m)
	w <- X$w; v <- X$v; u <- X$u; z <- X$z; m <- X$m; n <- X$n
	for (j in 1:n) {
  	if (z[j] != 0) {
    	for (q in u[j]:(u[j]+z[j]-1)) {
      	i = v[q+1];
	      Xb[i+1] = Xb[i+1] + w[q+1]*b[j];
			}
		}
	}
	return(Xb)
}

## This function takes a matrix in R's native CSC format, turns
## it into an Eigen version, and then calls the sparse multiplication
## implementation 
sparse_multiply <- function(X,b) {
	X <- dgCMatrix_to_Eigen(X)
	Xb <- Eigen_format_multiplication(X,b)
	return(Xb)
}

random_sparse <- function(m,n,NNZ,f=rnorm) {
	values <- f(NNZ)
	locations <- sample.int(n=m*n, size=NNZ)
	ii <- locations %% m + 1
	jj <- locations %/% m + 1
	o <- sparseMatrix(i=ii, j=jj, x=values, dims=c(m,n))
	return(o)
}

rr_sparse <- function(m_max=1000, n_max=1000, NNZ_max=2000) {
	m <- sample.int(m_max,1)
	n <- sample.int(n_max,1)
	NNZ <- sample.int(NNZ_max,1)
	A <- random_sparse(m,n,NNZ)
	return(A)
}


## For testing that exactly equivalent results can be gotten for R's
## native sparse multiplication and our looping implementation.
success <- replicate(n=10, expr={
	X <- rr_sparse()
	b <- rnorm(ncol(X))
	t1 <- system.time(Xb1 <- X %*% b)['user.self']
	t2 <- system.time(Xb2 <- sparse_multiply(X,b))['user.self']
	if (!all(Xb1 == Xb2)) {
		stop("Unequal answers.")
	}  
	c(core=t1,implemented=t2)
})













