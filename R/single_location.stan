data {
  int <lower = 1> T;       // Number of time points at which we have data
  int <lower = 0> I[T];
  row_vector[T]    SI;
  int num_windows; //How many windows must T be split into? There is 1 R_t for each window.
  int windows_end[num_windows] ; //end index of each window
}

transformed data{
  int <lower =1, upper = num_windows> ridx[T];
  int start = 1;
  for(i in 1:num_windows){
    int end = windows_end[i];
    while(start <= end){
     ridx[start] = i;
     start = start + 1;
    }
  }
  print("ridx = ",ridx);
}

parameters {
 real <lower = 0> R[num_windows];
}


model {
  for(t in 2:T){
      real mu = 0;
      for(s in 1:t){
        mu = mu + I[s] * SI[t - s + 1];
      }
      mu = mu * R[ridx[t]];
      target += poisson_lpmf(I[t] | mu);
  }
}
