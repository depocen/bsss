#################
### MCMC CODE ###
#################

remove(list = ls())

# RANDOM WALK

loc <- c("up", "down", "left", "right")
dat <- matrix(NA, ncol = 2, nrow = 1e5/2)
dat[1,] <- c(0,0)

for(i in 2:nrow(dat)) {
  rand <- sample(loc, 1, prob = rep(0.25,4))
  if(rand == "up") {
    dat[i,] <- c(0,1) + dat[i-1,]
  }
  else if(rand == "down") {
    dat[i,] <- c(0,-1) + dat[i-1,]
  }
  else if(rand == "left") {
    dat[i,] <- c(1,0) + dat[i-1,]
  }
  else if(rand == "right") {
    dat[i,] <- c(-1,0) + dat[i-1,]
  }
  else {
    dat[i,] <- c(NA,NA)
  }
}

plot(dat[1:5e3,1], dat[1:5e3,2], col = rgb(125, 125, 125, alpha = 100, max = 255),
     type = "l", bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "")
plot(dat[,1], dat[,2], type = "l", bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     col = rgb(125, 125, 125, alpha = 100, max = 255))
lines(dat[1:5e3,1], dat[1:5e3,2], col = "#ff6688")

# MARKOV CHAIN FUNCTION

#' Construct a Markov Chain governing movement through 2D space
#' @param transition_matrix A symmetric matrix of probabilities. Rows must sum to one.
#' @param iter Number of steps to take over the 2D space.
#' @param init Initial position in the 2D space.
#' @param state Initial state to start the Markov Chain.
#' @return The probabilities (prob) that are used to calculate the movement (move) and the data (dat) that
#' describes the movemement over 2D space.
markov_chain <- function(transition_matrix, iter = 1e3, init = c(0,0), state = c(1,0,0,0)) {
  # setup containers
  dat <- array(NA, c(iter, 2))  
  prob <- array(NA, c(iter,4))
  move <- array(NA, iter)
  # initial parameterizations
  loc <- colnames(transition_matrix)
  dat[1,] <- init
  # loop to contstruct the chain
  for(i in 2:iter) {
    state <- state%*%transition_matrix
    rand <- sample(loc, 1, prob = state)
    
    prob[i,] <- c(state)
    move[i] <- c(rand)
    
    if(rand == "up") {
      dat[i,] <- c(0,1) + dat[i-1,]
    }
    else if(rand == "down") {
      dat[i,] <- c(0,-1) + dat[i-1,]
    }
    else if(rand == "left") {
      dat[i,] <- c(-1,0) + dat[i-1,]
    }
    else if(rand == "right") {
      dat[i,] <- c(1,0) + dat[i-1,]
    }
    else {
      dat[i,] <- c(NA,NA)
    }
  }
  # return containers as a list
  return(list("dat" = dat, "prob" = prob, "move" = move))
}

# MARKOV CHAIN (as a random walk)

trans_prob <- matrix(0.25, ncol = 4, nrow = 4)
colnames(trans_prob) <- row.names(trans_prob) <- c("up", "down", "left", "right")
iter <- 1e5

mc_rw <- markov_chain(trans_prob, iter = iter)

plot(mc_rw$dat[,1], mc_rw$dat[,2], col = rgb(125, 125, 125, alpha = 100, max = 255), type = "l",
     main = "Markov Chain as a Random Walk", xlab = "x position", ylab = "y position",
     bty = "n")
points(0,0, col = "#ff6688", cex = 1, pch = 20)
points(mc_rw$dat[iter,1], mc_rw$dat[iter,2], col = "#40d2fe", cex = 1, pch = 20)

# MARKOV CHAIN (with center biased transition matrix)

# trans_prob <- rbind(c(0, 0.9, 0, 0.1),
#                     c(0.9, 0, 0.1, 0),
#                     c(0.9, 0, 0, 0.1),
#                     c(0, 0.9, 0.1, 0))

trans_prob <- rbind(c(0, 0.9, 0, 0.1),
                    c(0.9, 0, 0.1, 0),
                    c(0.1, 0, 0, 0.9),
                    c(0, 0.1, 0.9, 0))

# trans_prob <- rbind(c(0, 0.1, 0, 0.9),
#                     c(0.1, 0, 0.9, 0),
#                     c(0.9, 0, 0, 0.1),
#                     c(0, 0.9, 0.1, 0))

colnames(trans_prob) <- row.names(trans_prob) <- c("up", "down", "left", "right")

mc <- markov_chain(trans_prob, iter = iter)

y_range <- range(range(mc$dat[,2]), range(mc_rw$dat[,2]))
x_range <- range(range(mc$dat[,1]), range(mc_rw$dat[,1]))

plot(mc$dat[,1], mc$dat[,2], col = rgb(125, 125, 125, alpha = 100, max = 255), type = "l",
     main = "Markov Chain", xlab = "x position", ylab = "y position",
     xlim = x_range, ylim = y_range,
     bty = "n")
points(0,0, col = "#ff6688", cex = 1, pch = 20)
points(mc$dat[iter,1], mc$dat[iter,2], col = "#40d2fe", cex = 1, pch = 20)

# compare markov chains on same plot
plot(mc_rw$dat[,1], mc_rw$dat[,2], col = rgb(125, 125, 125, alpha = 100, max = 255), type = "l",
     xaxt = "n", yaxt = "n",
     main = "Markov Chain", xlab = "x position", ylab = "y position",
     xlim = x_range, ylim = y_range,
     bty = "n")
lines(mc$dat[,1], mc$dat[,2], col = rgb(255, 102, 136, alpha = 150, max = 255))
points(mc$dat[1,1], mc$dat[1,2], cex = 1, col = "black", pch = 20)
points(mc_rw$dat[1,1], mc_rw$dat[1,2], cex = 1, col = "black", pch = 20)
legend("topright", c("Center Biased", "Random Walk", "Initial Value"),
       col = c(rgb(255, 102, 136, alpha = 150, max = 255), rgb(125, 125, 125, alpha = 100, max = 255), "black"),
       lwd = c(1,1,NA), pch = c(NA,NA,20), cex = 0.5)

# MONTE CARLO - APPROXIMATE PI

circle <- function(x) {
  sqrt(1 - x^2)
}

domain <- seq(0,1, length.out = 1e3)
iter <- 1e4 * 2
accept <- matrix(NA, ncol = 2, nrow = iter)
reject <- matrix(NA, ncol = 2, nrow = iter)
for(i in 1:iter) {
  x <- runif(1, 0, 1)
  y <- runif(1, 0, 1)
  if(x^2 + y^2 <= 1)
    accept[i,] <- c(x,y)
  else
    reject[i,] <- c(x,y)
}
(nrow(na.omit(accept)) / iter) * 4

# plot(domain, circle(domain), type = "l", lwd = 5, col = "#40d2fe",
#      main = expression("Estimating" ~ pi ~ "Using Monte Carlo Simulation"),
#      xlab = "x", ylab = "y")
plot(0, type = "n", xlim = range(accept[,1], na.rm = TRUE), ylim = range(accept[,2], na.rm = TRUE),
     main = expression("Estimating" ~ pi ~ "Using Monte Carlo Simulation"),
     xlab = "x", ylab = "y")
points(accept, pch = 20, col = "#e63d95", cex = 0.5)
points(reject, pch = 20, col = "#808080", cex = 0.5)
lines(domain, circle(domain), type = "l", lwd = 5, col = rgb(64, 210, 254, alpha = 150, max = 255))
rect(0, 0, 0.4, 0.1, border = NA, col = rgb(255, 255, 255, alpha = 200, max = 255))
text(0.2,0.05, bquote(pi %~~% ~ .((nrow(na.omit(accept)) / iter) * 4)), col = "black")

# MONTE CARLO - REJECTION SAMPLING

#' A Rejection Sampling algorithm where proposals are generated from Unif(0,1).
#' @param f Target distribution.
#' @param g Candidate distribution.
#' @param support Support of the target distribution.
#' @param M Bound on the likelihood ratio f/g.
#' @param iter Number of iterations to run the algorithm through (iter = accept + reject).
#' @return A list containing accepted/rejected values.
accept_reject <- function(f, g, support = c(0,1), M = 1, iter = 1e3) {
  out <- list()
  
  for (i in 1:iter) {
    # containers
    y <- runif(1, support[1], support[2])  # proposal
    u <- runif(1, 0, 1)                    # stochastic condition
    out$y[i] <- y
    out$u[i] <- u
    # likelihood ratio
    ratio <- f(y)/(M*g(y, support[1], support[2]))
    # accept-reject condition
    if(ratio > u)
      out$accept[i] <- y
    else
      out$reject[i] <- y
  }
  return(out)
}

beta_sample1 <- accept_reject(f = function(x){dbeta(x, 2, 2)},
                             g = function(x, a, b){dunif(x, a, b)},
                             M = 1, iter = 1e4*4)
beta_sample2 <- accept_reject(f = function(x){dbeta(x, 2, 2)},
                              g = function(x, a, b){dunif(x, a, b)},
                              M = 2, iter = 1e4*4)

par(mfrow=c(1,3))
hist(rbeta(1e4*4, 2, 2), freq = F, breaks = 50, col = "#808080", border = FALSE, ylim = c(0,5),
     main = "Beta(2,2) Histogram", xlab = "x")
hist(na.omit(beta_sample1$reject), freq = F, breaks = 50, col = rgb(64, 210, 254, alpha = 150, max = 255),
     border = FALSE, ylim = c(0,5),
     main = "Accept Reject for Beta(2,2) with M = 1", xlab = "x")
hist(na.omit(beta_sample1$accept), freq = F, breaks = 50, col = rgb(230, 61, 149, alpha = 150, max = 255),
     border = FALSE, ylim = c(0,2), add = T)
abline(h = 1, col = "black", lty = 2)
hist(na.omit(beta_sample2$reject), freq = F, breaks = 50, col = rgb(64, 210, 254, alpha = 150, max = 255),
     border = FALSE, ylim = c(0,5),
     main = "Accept Reject for Beta(2,2) with M = 2", xlab = "x")
hist(na.omit(beta_sample2$accept), freq = F, breaks = 50, col = rgb(230, 61, 149, alpha = 150, max = 255),
     border = FALSE, ylim = c(0,2), add = T)
abline(h = 2, col = "black", lty = 2)
dev.off()

table(is.na(beta_sample1$accept))
table(is.na(beta_sample2$accept))

# METROPOLIS ALGORITHM

# 1. generate x* from g(x_{n}) (where g is a symmetric candidate distribution, e.g. normal)
# 2. compute the acceptance ratio alpha = f(x*)/f(x_{n}) (where f is the target distribution)
# 3. accept x* with probability min(1, alpha)
# 4. if x* is accepted then collect x_{n+1} = x* else collect x_{n+1} = x_{n}
# 5. repeat

metropolis <- function(f, iter = 1e3, chains = 4, prop_scale = 1) {
  init <- rnorm(chains, 0, 20)
  out <- array(NA, c(iter, chains))
  out[1,] <- init
  
  sampler <- function(x) {
    alpha <- 0
    u <- 1
    while(alpha < u) {
      u <- runif(1, 0, 1)
      x_star <- rnorm(1, x, 1 * prop_scale)
      alpha <- exp(f(x_star)-f(x))
    }
    return(x_star)
  }
  
  for(i in 2:iter) {
    for(j in 1:chains) {
      out[i,j] <- sampler(out[i-1,j]) 
    }
  }
  
  out
}

cols <- colorRampPalette(c("#ff6688","#336688"))(4)

dat <- rnorm(1, rnorm(10, 5, 5), 1)
post_func <- function(x) {
  lik <- sum(dnorm(dat, x, 1, log = T))
  prior <- dnorm(x, 5, 1, log = T)
  lik + prior
}
metro <- metropolis(f = post_func, chains = 4, iter = 2e3)

mean(dat)
mean(metro)

warmup <- (dim(metro)[1])/2
plot(0, type = "n", xlab = "", ylab = "", xlim = c(0,warmup), ylim = range(metro[-seq(1,warmup),dim(metro)[2]]),
     main = expression("Convergence of " ~ mu))
for(i in 1:4){
  lines(metro[-(1:(dim(metro)[1]/2)),i], col = cols[i], lwd = 2) 
}

plot(0, type = "n", xlab = "", ylab = "", xlim = c(0,100), ylim = range(metro),
     main = expression("Convergence of " ~ mu))
for(i in 1:4){
  lines(metro[1:100,i], col = cols[i], lwd = 2) 
}

hist(metro[-(1:(dim(metro)[1]/2)),], col = "#808080", border = FALSE, breaks = 50, freq = FALSE,
     main = expression("Distribution of " ~ mu), xlab = expression(mu))
abline(v = mean(metro), col = "red")

# METROPOLIS-HASTINGS ALGORITHM

# rbinorm <- function(n, mu = c(0,0), sigma = diag(2)) {
#   out <- array(NA, c(n, 2))
#   z1 <- rnorm(n, 0, 1)
#   z2 <- rnorm(n, 0, 1)
#   out[,1] <- sigma[1,1] * z1 + mu[1]
#   out[,2] <- sigma[2,2] * (sigma[1,2] * z1 + sqrt(1 - sigma[1,2]^2) * z2) + mu[2]
#   return(out)
# }

library(mvtnorm)

metropolis_hastings <- function(f, iter = 1e3, chains = 4, init = NULL, prop_scale = 1, prop_loc = c(0,0)) {
  out <- array(NA, c(iter, 2, chains))
  
  for(p in 1:2) {
    if(is.null(init))
      out[1,p,] <- rnorm(chains, 0, 1)
    else
      out[1,p,] <- init[,p]
  }

  prop_covmat <- diag(1,2)*prop_scale
  
  sampler <- function(x) {
    alpha <- 0
    u <- 1
    reject <- -1
    while(alpha < u) {
      reject <- reject + 1
      u <- runif(1, 0, 1)
      x_star <- rmvnorm(1, x, prop_covmat)
      alpha <- exp((f(x_star)-f(x)) +
                     (dmvnorm(x, x_star, prop_covmat, log = TRUE) - dmvnorm(x_star, x, prop_covmat, log = TRUE)))
      # cat("alpha = ", alpha, "; ", "u = ", u, "\n")
    }
    return(list("post" = x_star, "reject" = reject))
  }
  
  for(j in 1:chains) {
    cat(":: chain", j, "::", "")
    reject <- 0
    for(i in 2:iter) {
      sampler_stuff <- sampler(out[i-1,,j])
      reject <- reject + sampler_stuff$reject
      out[i,,j] <- sampler_stuff$post  # important! the rest is just print aesthetics
      if(i%%(iter/10) == 0)
        cat(".")
      if(i == iter)
        cat("", "accept rate =", iter,"/", reject + iter, "=", round(iter/(reject+iter),4), "\n")
      
    }
  }
  
  out
}

# covmat <- rbind(c(1, 0.5), c(0.5, 3))
covmat <- diag(1,2)
N <- 5
priors <- cbind(rnorm(N, 0, 3), rnorm(N, 0, 3))
dat <- array(NA, c(N, 2))
for(i in 1:N) {
  dat[i,] <- rmvnorm(1, mean = priors[i,], sigma = covmat) 
}
colMeans(dat)
plot(dat[,1], dat[,2], col = "darkgrey")

init_val <- rbind(c(1,1),
                  c(-1,-1),
                  c(1,-1),
                  c(-1,1))
init_val <- init_val * 4

posterior_func <- function(pars) {
  lik <- sum(dmvnorm(dat, pars, sigma = covmat, log = TRUE))
  prior <- dnorm(pars, 0, 0.1, log = TRUE)
  lik + sum(prior)
}
metro_hasti <- metropolis_hastings(f = posterior_func,
                                   iter = 2e3, chains = 4, init = init_val,
                                   prop_scale = 0.1, prop_loc = colMeans(dat))

# metro_hasti
colMeans(dat)
rowMeans(colMeans(metro_hasti))
range(metro_hasti)

cols <- colorRampPalette(c("#ff6688","#336688"))(4)
cols <- c(rgb(255, 102, 136, alpha = 150, max = 255),
          rgb(187, 102, 136, alpha = 150, max = 255),
          rgb(119, 102, 136, alpha = 150, max = 255),
          rgb(51, 102, 136, alpha = 150, max = 255))

plot(0, type = "n", xlim = range(metro_hasti), ylim = range(metro_hasti), xlab = expression(mu[1]), ylab = expression(mu[2]))
for(i in 1:4) {
  lines(metro_hasti[,1,i], metro_hasti[,2,i], col = cols[i], lwd = 2)
}
plot(0, type = "n", xlim = c(-0.4,0.4), ylim = c(-0.4,0.4), xlab = expression(mu[1]), ylab = expression(mu[2]))
for(i in 1:4) {
  lines(metro_hasti[,1,i], metro_hasti[,2,i], col = cols[i], lwd = 2)
}

plot(0, type = "n", xlim = c(0,dim(metro_hasti)[1]), ylim = range(metro_hasti[,1,]),
     main = expression("Traceplot for" ~ mu[1]), ylab = expression(mu[1]), xlab = "iteration")
for(i in 1:4) {
  lines(metro_hasti[,1,i], col = cols[i], lwd = 2)
}
rect(-50, range(metro_hasti[,2,])[1]-4, dim(metro_hasti)[1]/2, range(metro_hasti[,2,])[2]+4,
     col = rgb(128,128,128, alpha = 50, max = 255), border = NA)

plot(0, type = "n", xlim = c(0,dim(metro_hasti)[1]), ylim = range(metro_hasti[,2,]),
     main = expression("Traceplot for" ~ mu[2]), ylab = expression(mu[2]), xlab = "iteration")
for(i in 1:4) {
  lines(metro_hasti[,2,i], col = cols[i], lwd = 2)
}
rect(-50, range(metro_hasti[,1,])[1]-4, dim(metro_hasti)[1]/2, range(metro_hasti[,1,])[2]+4,
     col = rgb(128,128,128, alpha = 50, max = 255), border = NA)

warmup <- seq(1,(dim(metro_hasti)[1]/2))
hist(metro_hasti[-warmup,1,], breaks = 40, col = "darkgrey", border = FALSE, freq = FALSE,
     main = expression("Histogram for" ~ mu[1]))
hist(metro_hasti[-warmup,2,], breaks = 40, col = "darkgrey", border = FALSE, freq = FALSE,
     main = expression("Histogram for" ~ mu[2]))

# mle approach
optim(c(-4,4), f <- function(pars){sum(dmvnorm(dat, pars, covmat, log = TRUE))}, control = list(fnscale = -1))

# HAMILTONIAN MONTE CARLO ALGORITHM

library(mvtnorm)

# covmat <- diag(1,2)
covmat <- rbind(c(1,0.9),c(0.9,1))
N <- 1e3
priors <- cbind(rnorm(N, 0, 1), rnorm(N, 0, 1))
# priors <- cbind(rep(0,N), rep(0,N))  # fixed
dat <- array(NA, c(N, 2))
for(i in 1:N) {
  dat[i,] <- rmvnorm(1, mean = priors[i,], sigma = covmat) 
}
colMeans(dat)
plot(dat, col = "#808080")

post_func <- function(theta) {
  lik <- sum(dmvnorm(dat, theta, covmat, log = TRUE))
  prior <- dnorm(theta, 0, 0.1, log = TRUE)
  return(lik + sum(prior))
}

init_val <- cbind(c(1,1),
                  c(-1,-1),
                  c(1,-1),
                  c(-1,1))
init_val <- init_val * 4

hmc <- function(f, iter = 1e3, chains = 4, init_val = NULL, epsilon_init = 0.1, M = diag(1,2), L_0 = 10) {
  out <- array(NA, c(iter, 2, chains))
  phi <- array(NA, c(iter, 2, chains))
  if(is.null(init_val))
    out[1,,] <- t(rmvnorm(chains, c(0,0), diag(1,2)))
  else
    out[1,,] <- init_val
  phi[1,,] <- rmvnorm(chains, c(0,0), M)
  
  gradient <- function(theta) {
    e <- 1e-4
    out <- array(NA, length(theta))
    for(t in 1:length(theta)) {
      # browser()
      theta_hi <- theta_lo <- theta
      theta_hi[t] <- theta[t] + e
      theta_lo[t] <- theta[t] - e
      out[t] <- (f(theta_hi) - f(theta_lo)) / (2 * e)
      # browser()
    }
    out
  }
  
  sampler <- function(theta) {
    phi <- c(rmvnorm(1, mean = c(0,0), sigma = M))
    grad <- gradient(theta)
    epsilon <- runif(1, 0, 2 * epsilon_init)
    L <- ceiling(2 * L_0 * runif(1,0,1))
    for (l in 1:L) {
      phi <- phi + 0.5 * epsilon * grad
      theta <- theta + epsilon * solve(M) %*% phi
      phi <- phi + 0.5 * epsilon * grad 
    }
    return(list("theta" = c(theta), "phi" = c(phi)))
  }
  
  accept_reject <- function(theta_star, theta, phi_star, phi) {
    dens_theta_star <- f(theta_star)
    dens_theta <- f(theta)
    dens_phi_star <- dmvnorm(phi_star, c(0,0), M, log = TRUE)
    dens_phi <- dmvnorm(phi, c(0,0), M, log = TRUE)
    alpha <- exp((dens_theta_star + dens_phi_star) - (dens_theta + dens_phi))
    alpha
  }
  
  cat(paste0("['.' = ", iter/10, " iterations]\n"))
  
  for(j in 1:chains) {
    cat(paste0("[ chain ", j, " ]"))
    for(i in 2:iter) {
      alpha <- 0
      u <- 1
      while(alpha < u) {
        u <- runif(1, 0, 1)
        sample <- sampler(out[i-1,,j])
        alpha <- accept_reject(sample$theta, out[i-1,,j], sample$phi, phi[i-1,,j])
      }
      phi[i,,j] <- sample$phi
      out[i,,j] <- sample$theta
      if(i%%(iter/10) == 0)
        cat(".")
      if(i == iter)
        cat("[ complete ]\n")
    } 
  }
  out
}

hmc_sim <- hmc(f = post_func, iter = 1e3, init_val = init_val, epsilon_init = ((1/15)^2), M = diag(1,2), L_0 = 10)
colMeans(dat)
rowMeans(colMeans(hmc_sim))

plot(0, type = "n", xlim = c(1,dim(hmc_sim)[1]), ylim = range(hmc_sim), 
     ylab = "Parameter Value", xlab = "Iteration", main = expression("Traceplot for" ~ mu[1]))
for(i in 1:dim(hmc_sim)[3]) {
  lines(hmc_sim[,1,i], col = cols[i], lwd = 2)
}
plot(0, type = "n", xlim = c(1,dim(hmc_sim)[1]), ylim = range(hmc_sim), 
     ylab = "Parameter Value", xlab = "Iteration", main = expression("Traceplot for" ~ mu[2]))
for(i in 1:dim(hmc_sim)[3]) {
  lines(hmc_sim[,2,i], col = cols[i], lwd = 2)
}

x <- y <- seq(-4,4,0.01)
z <- outer(x, y, FUN = function(x, y){dmvnorm(cbind(x,y), colMeans(dat), covmat)})
plot(0, type = "n", xlim = range(hmc_sim[,1,]), ylim = range(hmc_sim[,2,]), 
     ylab = expression(mu[2]), xlab = expression(mu[1]), main = expression("Convergence of" ~ mu[1] ~ "and" ~ mu[2]))
contour(x, y, z, add = TRUE, col = "#808080")
for(i in 1:dim(hmc_sim)[3]) {
  lines(hmc_sim[,1,i], hmc_sim[,2,i], col = cols[i], lwd = 2)
}

plot(0, type = "n", xlim = range(hmc_sim[,1,])/30, ylim = range(hmc_sim[,2,])/30, 
     ylab = expression(mu[2]), xlab = expression(mu[1]), main = expression("Convergence of" ~ mu[1] ~ "and" ~ mu[2]))
x <- y <- seq(-0.2,0.2,0.01)
z <- outer(x, y, FUN = function(x, y){dmvnorm(cbind(x,y), colMeans(dat), covmat)})
contour(x, y, z, add = TRUE, col = "#808080")
for(i in 1:dim(hmc_sim)[3]) {
  lines(hmc_sim[,1,i], hmc_sim[,2,i], col = cols[i], lwd = 2)
}

params <- rbind(hmc_sim[,,1],
                hmc_sim[,,2],
                hmc_sim[,,3],
                hmc_sim[,,4])


hmc_draws <- t(apply(params, 1, function(x){rmvnorm(1, x, covmat)}))
binorm_draws <- rmvnorm(4e3, colMeans(dat), covmat)
plot(binorm_draws, cex = 0.5, col = "#80808096", xlim = c(-6, 6), ylim = c(-6, 6),
     main = "Comparing Random Samples", xlab = expression(x[1]), ylab = expression(x[2]))
points(hmc_draws, col = "#cc000096", cex = 0.5)
legend("bottomright", c("HMC","Draws"), col = c("#cc000096","#80808096"), pch = rep(21,2), pt.lwd = rep(2,2), cex = 0.6)

# NUTS
