data {
  int <lower = 1> T;       // Number of time points at which we have data
  int <lower = 0> I[T];
  row_vector[T]    SI;
}

parameters {
 real <lower = 0, upper = 4> R;
}


model {
  for(t in 2:T){
      real mu = 0;
      for(s in 1:t){
        mu = mu + I[s] * SI[t - s + 1];
      }
      mu = mu * R;
      target += poisson_lpmf(I[t] | mu);
  }
}
