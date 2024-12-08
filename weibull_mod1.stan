data {
  int<lower=0> N;  // number of observations
  int<lower=0> P;  // number of predictors
  vector[N] surv_innings;  // survival time
  vector[N] censor;  // censoring indicator (1 = observed, 0 = censored)
  matrix[N, P] X;  // predictor matrix
}

parameters {
  real<lower=0> shape;  // Weibull shape parameter
  vector[P] beta;  // coefficients for predictors
  real alpha;  // intercept
}

model {
  vector[N] mu = alpha + X * beta;
  vector[N] lambda = exp(-mu / shape);
  
  // Priors
  shape ~ gamma(1, 1);  // weakly informative prior
  beta ~ normal(0, 1);  // weakly informative prior
  alpha ~ normal(2, 1);  // weakly informative prior
  
  // Likelihood
  for (i in 1:N) {
    if (censor[i] == 1)
      target += weibull_lpdf(surv_innings[i] | shape, lambda[i]);
    else
      target += weibull_lccdf(surv_innings[i] | shape, lambda[i]);
  }
}

generated quantities {
  vector[N] y_rep;  // replicated data for posterior predictive check
  vector[N] log_lik;  // log-likelihood for LOO-CV
  vector[N] mu = alpha + X * beta;
  vector[N] lambda = exp(-mu / shape);

  for (i in 1:N) {
    // Generate new data
    y_rep[i] = weibull_rng(shape, lambda[i]);
    
    // Compute log-likelihood
    if (censor[i] == 1)
      log_lik[i] = weibull_lpdf(surv_innings[i] | shape, lambda[i]);
    else
      log_lik[i] = weibull_lccdf(surv_innings[i] | shape, lambda[i]);
  }
}
