// AR(1) Model
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
