data {
  int <lower = 1> T; // Time points
  int <lower = 1> N; // Number of locations
  matrix<lower = 0>[T, N]  I;
  row_vector[T]    SI;
  // For each location, how many windows must T be split 
  // into? There is 1 R_t for each window.  
  int num_windows[N];
  // For each location, end index of each window
  int windows_end[sum(num_windows)] ; 
  
}
