remove(list=ls())

# simple grid approx example

theta <- seq(0,1,by=0.01)
lik <- dbinom(5, 10, theta)
prior <- dunif(theta, 0, 1)
post <- lik * prior / sum(lik * prior)
samples <- sample(theta, size = 1e4, replace = TRUE, prob = post)

# samples from posterior
hist(samples, col = "darkgrey", border = T, freq = F, breaks = 50,
     main = "Posterior Distribution of Theta", xlab = expression(theta))
# posterior distribution
lines(theta, post, col = "red", lwd = 2)

# looking at norm the histogram will match for certain break values (e.g. 100).

x <- rnorm(1e4, 0, 1)

hist(x, col = "darkgrey", border = T, breaks = 100, freq = F,
     main = "Standard Normal Distribution | breaks = 100", xlab = "X")
lines(seq(-6,6,by=0.01),dnorm(seq(-6,6,by=0.01), 0, 1), col = "red", lwd = 2)
legend(2, 0.4, "actual\ndensity", col = "red", lwd = 2, lty = 1, bty = "n")


hist(x, col = "darkgrey", border = T, breaks = 3000, freq = F,
     main = "Standard Normal Distribution | breaks = 2000", xlab = "X")
lines(seq(-6,6,by=0.01),dnorm(seq(-6,6,by=0.01), 0, 1), col = "red", lwd = 2)
legend(2, 0.7, "actual\ndensity", col = "red", lwd = 2, lty = 1, bty = "n")

