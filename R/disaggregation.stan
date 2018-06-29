data {
  int <lower = 1> T; // Time points
  int <lower = 1> N; // Number of locations
  int <lower = 0>  I[T];
  simplex[N] probs;
}
parameters {
  real <lower = 0> lambda[N];
}
transformed parameters {
  real <lower = 0> cum_lambda;
  cum_lambda = 0;
  for(i in 1:N){
      cum_lambda += probs[i] * lambda[i];
  }

}
model {
  for(t in 1:T){
    I[t] ~ poisson(cum_lambda);
  }
}
