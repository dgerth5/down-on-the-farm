data {
  int<lower=0> N;              // Number of observations
  vector[N] surv_innings;     // Survival time
  int<lower=0, upper=1> curr_player[N]; // Censoring indicator (1 if censored, 0 if event)
  vector[N] Age;              // Covariate Age
  vector[N] IP;               // Covariate Innings Pitched
  vector[N] FIP_minus;        // Covariate FIP-
  vector[N] weighted_fb;      // Covariate weighted fastballs

  // Prior hyperparameters can be added here if needed
}

parameters {
  real<lower=0> alpha;        // Weibull shape parameter
  real beta_age;              // Coefficient for Age
  real beta_ip;               // Coefficient for Innings Pitched
  real beta_fip;              // Coefficient for FIP-
  real beta_fb;               // Coefficient for weighted fastballs
  real<lower=0> sigma;        // Scale parameter for Weibull, equivalent to 1/lambda
}

model {
  // Priors (assuming normal priors for coefficients and a gamma prior for alpha)
  beta_age ~ normal(0, 1);
  beta_ip ~ normal(0, 1);
  beta_fip ~ normal(0, 1);
  beta_fb ~ normal(0, 1);
  alpha ~ gamma(0.001, 0.001);
  sigma ~ normal(0, 1);

  // Likelihood
  for (i in 1:N) {
    if (curr_player[i] == 0) {  // Event observed
      target += weibull_lpdf(surv_innings[i] | alpha, exp(beta_age * Age[i] + beta_ip * IP[i] + beta_fip * FIP_minus[i] + beta_fb * weighted_fb[i]) / sigma);
    } else {                    // Right-censored data
      target += weibull_lccdf(surv_innings[i] | alpha, exp(beta_age * Age[i] + beta_ip * IP[i] + beta_fip * FIP_minus[i] + beta_fb * weighted_fb[i]) / sigma);
    }
  }
}
