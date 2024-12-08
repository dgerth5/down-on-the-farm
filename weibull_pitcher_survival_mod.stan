data {
  int<lower=0> N; // number of observations
  int<lower=0> P; // number of predictors + intercept
  vector[N] y; // MLB innings thrown after rookie year
  int<lower=0, upper=1> censor[N];
  matrix[N, P] X; // model matrix
}

parameters {
  real alpha ; //shape
  vector[P] beta ; // set of scale predictors
}

model {
  
  // priors
  alpha ~ normal(0.9, 1) ; // scale of weibull distribution fit by ML
  beta[1] ~ normal(10, 2) ; // intercept
  
  for (p in 2:P) {
    beta[P] ~ normal(0, 1);      // Prior for beta[2] to beta[K]
  }
  
  // model
  
  for (i in 1:N){
    if (censor[i] == 0){
      target += weibull_lpdf(y[i] | alpha, exp(X * beta));
    }
    else{
      target += weibull_lccdf(y[i] | alpha, exp(X * beta));
    }
    
  }
  
}
