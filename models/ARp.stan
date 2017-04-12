// AR(p) model
data {
  int<lower=0> N;         // length of series
  int<lower=1> P;         // number of lags
  vector[N] y;            // time series
  real loc_alpha;         // prior loc on alpha
  real scale_alpha;       // prior scale on alpha
  vector[P] loc_rho;      // prior loc on rho
  vector[P] scale_rho;    // prior scale on rho
}
transformed data {
  // transform data to accommodate P-lag process
  vector[N-P] y_trans;    // outcome of series
  matrix[N-P, P] Y;       // matrix of (lagged) predictors
  for (n in 1:(N-P)) {
    y_trans[n] = y[n+P];
    for (p in 1:P)
      Y[n,p] = y[(P + n) - p];
  }
}
parameters {
  real alpha;                         // intercept
  vector<lower=-1,upper=1>[P] rho;    // vector of parameters
  real<lower=0> sigma;                // sd of error
}
model {
  // likelihood
  target+= normal_lpdf(y_trans | alpha + Y * rho, sigma);
  // priors
  target+= normal_lpdf(rho | loc_rho, scale_rho);
  target+= normal_lpdf(alpha | loc_alpha, scale_alpha);
  target+= normal_lpdf(sigma | 0, 1);
}
generated quantities {
  vector[N-P] y_rep;
  for (n in 1:(N-P))
    y_rep[n] = normal_rng(alpha + Y[n,] * rho, sigma);
}
