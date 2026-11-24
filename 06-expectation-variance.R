source("_common.R")

set.seed(2027)
n <- 5000
roll <- sample(1:6, n, replace = TRUE)
net <- c(0, 0, 0, 0, 4, 10)[roll] - 2
ggplot(data.frame(n = 1:n, avg = cumsum(net) / (1:n)), aes(n, avg)) + geom_line(colour = ie_navy) +
  geom_hline(yintercept = 14 / 6 - 2, linetype = 2, colour = ie_coral) + labs(x = "games played", y = "average net gain")
c(simulated = mean(net), exact = 14 / 6 - 2, sd = sd(net))

x <- 0:3; p <- c(0.1, 0.3, 0.4, 0.2)
sum(x * p)

integrate(function(x) x * 2 * x, 0, 1)$value

set.seed(2027)
n <- 5000
d <- data.frame(n = rep(1:n, 2),
  mean = c(cumsum(rcauchy(n)) / (1:n), cumsum(rnorm(n)) / (1:n)),
  dist = rep(c("Cauchy", "Normal"), each = n))
ggplot(d, aes(n, mean, colour = dist)) + geom_line() +
  scale_colour_manual(values = c(ie_coral, ie_blue)) + labs(x = "n", y = "running mean", colour = NULL)

f <- function(x) 2 * x
c(integrate(function(x) x^2 * f(x), 0, 1)$value,
  integrate(function(x) sqrt(x) * f(x), 0, 1)$value,
  integrate(function(x) exp(x) * f(x), 0, 1)$value)
# simulation: X = sqrt(U) has density 2x on [0,1]
set.seed(1); x <- sqrt(runif(1e6)); c(mean(x^2), mean(sqrt(x)), mean(exp(x)))

set.seed(2027)
n <- 10
fixed <- replicate(20000, sum(sample(n) == 1:n))   # number of fixed points of a random permutation
mean(fixed)

c(sum(x^2 * p) - sum(x * p)^2, sum((x - sum(x * p))^2 * p))   # two formulas agree
integrate(function(x) x^2 * 2 * x, 0, 1)$value - (2 / 3)^2; 1 / 18
w <- c(0, 0, 0, 0, 4, 10); c(mean(w^2) - mean(w)^2, sqrt(mean(w^2) - mean(w)^2))

mom <- function(f, lo, hi) {
  m <- integrate(function(x) x * f(x), lo, hi)$value
  v <- integrate(function(x) x^2 * f(x), lo, hi)$value - m^2
  c(mean = m, var = v)
}
rbind(unif_2_8 = mom(function(x) dunif(x, 2, 8), 2, 8),          # 5, 3
      exp_rate_0.5 = mom(function(x) dexp(x, 0.5), 0, Inf),       # 2, 4
      gamma_3_2 = mom(function(x) dgamma(x, 3, 2), 0, Inf),       # 1.5, 0.75
      normal_1_4 = mom(function(x) dnorm(x, 1, 2), -Inf, Inf))    # 1, 4
k <- 1:2000; p <- 0.2
m <- sum(k * dgeom(k - 1, p)); c(mean = m, var = sum(k^2 * dgeom(k - 1, p)) - m^2)   # 5, 20
k <- 0:200; m <- sum(k * dpois(k, 3)); c(mean = m, var = sum(k^2 * dpois(k, 3)) - m^2)

cc <- 2:5
data.frame(cc, chebyshev_bound = 1 / cc^2, normal = 2 * (1 - pnorm(cc)),
           exponential = 1 - pexp(1 + cc, 1) + pexp(pmax(1 - cc, 0), 1))   # Exp(1): mean 1, sd 1

t <- 0.3; lambda <- 3
c(integrate(function(x) exp(t * x) * dexp(x, 2), 0, Inf)$value, 2 / (2 - t))
c(sum(exp(t * (0:100)) * dpois(0:100, lambda)), exp(lambda * (exp(t) - 1)))
