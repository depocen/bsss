// Partial Pooling
data {
  int<lower=1> N;   // dimensions of data
  real y[N];        // treatment effect
  real<lower=0> sigma[N];    // se of treatment effect
}
parameters {
  real theta[N];    // array of parameters
  real mu;          // hyperparameter (location)
}
model {
  // likelihood
  target += normal_lpdf(y | theta, sigma);
  // prior
  target += normal_lpdf(theta| mu, 5);
  // hyperprior
  target += normal_lpdf(mu | 0, 1);
}
