data {
  int <lower = 1> N;       // Number of locations
  int <lower = 1> T;       // Number of time points at which we have data
  matrix[N, N] pop_prod ;  // product of populations
  matrix[N, N] distances;  // distance matrix
  matrix[T, N] R;
  matrix[T, N] I;
  vector[T]    SI;
}

parameters {
 real pstay;
 real alpha; // exponent of product of populations
 real gamma; // exponent of distance
}

model {
  matrix[N, N] pmovement;
  for (i in 1:N){
   for (j in 1:N){
     pmovement[i, j] = (pop_prod[i, j]^alpha)/distances[i, j]^gamma
   }
   real total;
   total = sum(pmovement[i, ])
   pmovement[i, ] = pmovement[i, ]/total
  }
  
  for (i in 1:N){
    for(t in 2:T){
     i_t = I[1:i, ]
     w_t = SI[t - i + 1:t]
     pij = pmovement[i, ] * (1 - pstay)
     
     mu  = ((w_t * i_t) .* R[i, ]) * pij
     I[i, t] ~ poisson(mu)
    }
  }
}