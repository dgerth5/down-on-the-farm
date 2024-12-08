data {
  int<lower=0> N; 
  int<lower=0> ip[N];
  int<lower=0> bf[N];
  int<lower=0> er[N];
  int<lower=0> k[N];
  int<lower=0> bb[N];
  real<lower=0> alpha1;          
  real<lower=0> beta1;          
  real<lower=0> alpha2;          
  real<lower=0> beta2; 
}

parameters {
  vector[4] beta;
  vector<lower=0, upper=1>[N] k_pi;
  vector<lower=0, upper=1>[N] bb_pi;
}

model {
  beta ~ normal(0,10);
  
  k_pi ~ beta(alpha1, beta1);
  bb_pi ~ beta(alpha2, beta2);
  
  k ~ binomial(bf, k_pi);
  bb ~ binomial(bf, bb_pi);
  
  // for (i in 1:N){
  //   er[i] ~ poisson(exp(beta[1] + beta[2]*k_pi[i] + beta[3]*bb_pi[i] + beta[4]*ip[i]));
  // }
  
  er ~ poisson_log(beta[1] + beta[2] * k_pi + beta[3] * bb_pi + beta[4] * to_vector(ip));

}
