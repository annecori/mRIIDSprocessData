data {
  int <lower = 1> N;       // Number of locations
  int <lower = 1> T;       // Number of time points at which we have data
  matrix[N, N] pop_prod ;  // product of populations
  matrix[N, N] distances;  // distance matrix
  matrix[T, N] R;
  int  I[T, N];
  row_vector[T]    SI;
}

parameters {
 real pstay;
 real alpha; // exponent of product of populations
 real gamma; // exponent of distance
}

transformed parameters {
  matrix[N, N] pmovement;
  real total;
  for (i in 1:N){
   for (j in 1:N){
     pmovement[i, j] =
         if_else(i == j, 0,
			 pop_prod[i, j]^alpha)/distances[i, j]^gamma;
   }

   total = sum(pmovement[i, ]);
   pmovement[i, ] = (1 - pstay) * (pmovement[i, ]/total);
  }
  for (i in 1:N){
    pmovement[i, i] = 1 - pstay; 
  }

}

model {
    for(t in 2:T) {
     int i_t[t, N];
     row_vector[t] w_t;
     row_vector[N] r_t;
     row_vector[N] mu;

     i_t = I[1:t, ];
     w_t = SI[T:(T - t + 1)];
     r_t = R[t, ];
     mu  = ((w_t * i_t) .* r_t) * pmovement;
     for(i in 1:N){
       I[t, i] ~ poisson(mu[i]);
     }
   }
}    

