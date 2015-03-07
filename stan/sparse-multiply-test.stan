functions {

	vector sparse_multiply (
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
	
}

data {
	int m;
	int n;
	int p;
	vector[p] w;
	int v[p];
	int u[n+1];
	int z[n];

//	matrix[m,n] X;

	vector[m] y;
}

parameters {
	vector[n] b;
}

model {
	y ~ normal(sparse_multiply(m,n,w,v,u,z,b),1);
}


