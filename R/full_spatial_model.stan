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
  real prior_mean;
  real prior_std;
}

parameters {
 real <lower = 0> R[num_Rjt];
 real <lower = 1, upper = 2> gamma;
 real <lower = 0, upper = 1> pstay;
}

model {

  real flow[N, N];
  real row_total;
  real pmovement[N, N];
  real a = ( prior_mean / prior_std)^2;
  real b = ( prior_std ^ 2)/ prior_mean;
  R ~ gamma(a, b);  
// For a given value of gamma, first calculate
// pmovement. Then proceed as before.

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
