##################
### BASIC BRMS ###
##################
#### Imad Ali ####
#### 03.29.17 ####
##################

# This serves as a brief overview of how to generate Stan
# models/data with the brms package and how to use brm()
# to fit models.

### PREAMBLE ------------------------------------------------------------
library(rstan)
library(brms)

### GENERATE DATA -------------------------------------------------------
remove(list = ls())
# inverse logit function
invlogit <- function(x) {
  return(exp(x)/(1+exp(x)))
}
# coefficients generated from prior distributions
b10 <- rnorm(1,0,1)  # group 1 intercept
b11 <- rnorm(1,1,1)  # group 1 slope

b20 <- rnorm(1,0,1)  # group 2 intercept
b21 <- rnorm(1,1,1)  # group 2 slope

b30 <- rnorm(1,0,1)  # group 2 intercept
b31 <- rnorm(1,1,1)  # group 2 slope
# for ease of comparison post-estimation
parameters <- rbind("(Intercept)" = c(b10, b20, b30),
                    "x1" = c(b11, b21, b31))
colnames(parameters) <- c("model1", "model2", "model3")
# generate data
n <- 900
X <- data.frame(matrix(c(c(rep(1,n/3),rep(2,n/3),rep(3,n/3)),rnorm(n,0,1)),ncol = 2))
names(X) <- c("group","x1")
y1 <- invlogit(b10+b11*X[which(X$group==1),2])
y2 <- invlogit(b20+b21*X[which(X$group==2),2])
y3 <- invlogit(b30+b31*X[which(X$group==3),2])
y <- c(y1,y2,y3)
dat <- data.frame(cbind(rbinom(n,1,y),X))
names(dat)[1] <- "y"
head(dat)

### USING VARIOUS brms FUNCTIONS --------------------------------------------

# fitting a hierarchical model with group specific intercepts.
fit1 <- brm(y ~ x1 + (1 | group), data = dat, family = "bernoulli", chains = 4, iter = 500)
fixef(fit1)
ranef(fit1)
coef(fit1)

# generate a stan model.
hier_stan_model <- brms::make_stancode(y ~ x1 + (1 | group), data = dat, family = "bernoulli")
# compile the model.
hier_stan_model_comp <- stan_model(model_code = hier_stan_model)
# convert the data.
hier_stan_data <- brms::make_standata(y ~ x1 + (1 | group), data = dat, family = "bernoulli")
# fit the model using rstan.
fit2 <- rstan::sampling(hier_stan_model_comp, data = hier_stan_data, chains = 4, iter = 500)
rstan::traceplot(fit2)
