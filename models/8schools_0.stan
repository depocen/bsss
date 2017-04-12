// Full Pooling
data {
  int<lower=1> N;   // dimensions of data
  real y[N];        // treatment effect
  real<lower=0> sigma[N];    // se of treatment effect
}
parameters {
  real theta;       // scalar parameter
}
model {
  // likelihood
  target += normal_lpdf(y | theta, sigma);
}
