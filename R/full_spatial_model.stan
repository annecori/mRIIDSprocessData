data {
  int <lower = 1> T; // Time points
  int <lower = 1> N; // Number of locations
  int <lower = 0>  I[T, N];
  row_vector[T + 1]    SI;
  int <lower = 0> rindex[T, N];
  int num_Rjt ;
  real population[N];
  real dist_mat[N, N];
  real alpha;
  real beta;
  real K;
}

parameters {
 real <lower = 0> R[num_Rjt];
 real <lower = 0> gamma;
 real <lower = 0, upper = 1> pstay;
}

model {
// For a given value of gamma, first calculate
// pmovement. Then proceed as before.
  real flow[N, N];
  real row_total;
  real pmovement[N, N];
  for(r in 1:N){
    for(c in 1:N){
      if(r == c){
         flow[r, c] = 0;
      } else {
         flow[r, c] = (K * population[r]^alpha *
                           population[r]^beta) /
		           dist_mat[r, c]^gamma;
      }		   
    }
  }

  // Relative flow;
  for(r in 1:N){
   row_total = sum(flow[r]);
   for(c in 1:N){
     flow[r, c] = flow[r, c]/row_total;
   }
  }


  for(r in 1:N){
    for(c in 1:N){
      if(r == c){
         pmovement[r, c] = pstay;
      } else {
         pmovement[r, c] = flow[r, c] *
	                   (1 - pstay);
      }		   
    }
  }

  for( t in 2:T){
    for( j in 1:N){
      real mu = 0;
      // Calculate mu[ t, j]
      for( i in 1:N){
        real tmp = 0;
        for( s in 1:t){
	  tmp = tmp + I[ s, i] * SI[ t - s + 1];
	}
        tmp = tmp * pmovement[i, j] * R[ rindex[ t, i]];
	mu = mu + tmp;
      } // end of computing mu[ t, j]
      target += poisson_lpmf(I[ t, j] | mu);
    }
  }
}
