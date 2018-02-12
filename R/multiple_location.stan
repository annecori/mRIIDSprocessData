data {
  int <lower = 1> T; // Time points
  int <lower = 1> N; // Number of locations
  int <lower = 0>  I[T, N];
  row_vector[T + 1]    SI;
  int <lower = 0> rindex[T, N];
  int num_Rjt ;
  real pstay;
}

parameters {
 real <lower = 0> R[num_Rjt];
}

model {
  for( t in 2:T){
    for( j in 1:N){
      real mu = 0;
      // Calculate mu[ t, j]
      for( i in 1:N){
        real tmp = 0;
        for( s in 1:t){
	  tmp = tmp + I[ s, i] * SI[ t - s + 1];
	}
	if(i == j){
          tmp = tmp * pstay * R[ rindex[ t, i]];
	}  else {
          tmp = tmp * (1 - pstay)
	            * R[ rindex[ t, i]];
        }		    
	mu = mu + tmp;
      } // end of computing mu[ t, j]
      target += poisson_lpmf(I[ t, j] | mu);
    }
  }
}


