data {
  int<lower=0> N; // rows in data
  int<lower=0> K; // num features
  vector[N] pitches_thrown;
  vector[N] tot_swings;
  vector[N] tot_contact;
  vector[N] tot_hh;
  matrix[N, K] X; 
}

transformed data {
  matrix[N, K] x_std;  // Standardized predictor matrix
  for (k in 1:K) {
    real mean_k = mean(col(x, k));
    real sd_k = sd(col(x, k));
    for (n in 1:N) {
      x_std[n, k] = (x[n, k] - mean_k) / sd_k;
    }
  }
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  
  
  
}
