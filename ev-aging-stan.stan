data {
  int<lower=0> N; // number of observations
  int<lower=0> B; // ncol of spline matrix
  int<lower=0> P; // number of players
  vector[N] y; // ev90
  vector[N] ev90_t1; // ev90 t-1
  matrix[N,B] spline_mat;
  int<lower=0,upper=P> batter[N];
  
}

parameters {
  real alpha ; // intercept
  real beta1 ; // ev90 t-1 coefficient
  real<lower=0> batter_sigma;
  real<lower=0> sigma;
//  real<lower=0> nu;
  vector[B] spline_coefs ;
  vector[P] batter_coefs ;
  
}

transformed parameters {
  vector[N] spline; 
  spline = spline_mat*spline_coefs;
  
}

model {
  
  // priors
  
  batter_sigma ~ normal(0,1);
  sigma ~ normal(2,0.5);
  
  alpha ~ normal(0,5);
  beta1 ~ normal(1,0.25);
  spline_coefs ~ normal(0,1);
  batter_coefs ~ normal(0,batter_sigma);

 // nu ~ gamma(10,0.1);
  
  // model
  for (i in 1:N){
    y[i] ~ normal(alpha + beta1*ev90_t1[i] + spline[i] + batter_coefs[batter[i]], sigma);
   // y[i] ~ normal(alpha + beta1*ev90_t1[i], sigma);
  // y[i] ~ student_t(nu, alpha + beta1*ev90_t1[i] + spline[i] + batter_coefs[batter[i]], sigma);
   
  }
}

generated quantities {
  vector[N] y_sim;
  for (i in 1:N) {
    y_sim[i] = normal_rng(alpha + beta1 * ev90_t1[i] + spline[i] + batter_coefs[batter[i]], sigma);
    //y_sim[i] = normal_rng(alpha + beta1 * ev90_t1[i], sigma);
    //y_sim[i] = student_t_rng(nu, alpha + beta1 * ev90_t1[i] + spline[i] + batter_coefs[batter[i]], sigma);

  }
}

