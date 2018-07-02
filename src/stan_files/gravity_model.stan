data {
  int N;
  real flow[N];
  real pop_prod[N];
  real dist[N];
}

parameters {
 real <lower = 1, upper = 3> gamma;
 real <lower = 0, upper = 5> K;
 real <lower = 0> sigma;
}

model{
  for(i in 1:N){
      flow[i] ~ normal(K * pop_prod[i] / (dist[i]^gamma), sigma);
  }    
}
