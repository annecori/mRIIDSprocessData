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

// here we calculate mu for each j at time t.
// We can do this in the inference module although not in the projection module.

 matrix[T, N] mu;
 for(t in 2:T){
   for(j in 1:N){
    mu[t, j] = 0;
    for(i in 1:N){
     real total2 = 0;
     for(s in 1:t){
      total2 = total2 + I[i, s] * SI[t - s + 1];
     }
     mu[t, j] = mu[t, j] + pmovement[i, j] * R[i, t] * total2;
    }
   }
}
print("mu = ", mu);
// And now we compute the likelihood      
  for(t in 2:T){
   for(j in 1:N){
      I[t, j] ~ poisson(mu[t, j]);
   }
  }
}

