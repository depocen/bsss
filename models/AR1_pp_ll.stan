// AR(1) Model with posterior predictions and log likelihood
data {
  int<lower=0> N;   // number of observations
  real y[N];        // time series data
}
parameters {
  real<lower=-1,upper=1> rho;   // parameter on lag term
  real alpha;                   // constant term
  real<lower=0> sigma;          // sd of error
}
model {
  // likelihood
  for (n in 2:N)
    target+= normal_lpdf(y[n] | alpha + rho * y[n-1], sigma);
  // priors
  target+= normal_lpdf(rho | 0 , 0.5);
  target+= normal_lpdf(alpha | 0 , 1);
}
generated quantities {
  vector[N-1] y_rep;
  vector[N-1] log_lik;
  for (n in 1:(N-1)) {
    // posterior predictions
    y_rep[n] = normal_rng(alpha + rho * y[n], sigma);
    // log likelihood
    log_lik[n] = normal_lpdf(y[n] | alpha + rho * y[n], sigma);
  }
}
