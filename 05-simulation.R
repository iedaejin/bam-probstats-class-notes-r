source("_common.R")

set.seed(2027)
n <- 10000
d1 <- sample(1:6, n, replace = TRUE)
d2 <- sample(1:6, n, replace = TRUE)
wins <- pmax(d1, d2) <= 4
mean(wins)            # relative frequency of wins
(4 / 6)^2             # exact

# geometric: first six on the 4th roll or later
p <- 1 / 6
1 - pgeom(2, p)                      # P(X >= 4) = P(R-variable >= 3) = 1 - P(R <= 2)
(1 - p)^3                            # by hand
dgeom(4 - 1, p); (1 - p)^3 * p       # P(X = 4)
# exponential: mean 4 minutes means rate 1/4
pexp(2, rate = 1 / 4)
# normal: N(50, 25) has sd 5
pnorm(58, mean = 50, sd = 5, lower.tail = FALSE)

qnorm(1 - 0.025)                       # z_{0.025}
qnorm(c(0.05, 0.5, 0.95), mean = 170, sd = 8)
qbinom(0.9, size = 20, prob = 0.3)      # smallest x with P(X <= x) >= 0.9
qexp(0.5, rate = 0.005); log(2) / 0.005 # median of Exp(0.005)
qgamma(0.9, shape = 3, rate = 2)

library(gridExtra)
k <- 0:10
g1 <- ggplot(data.frame(k, p = dbinom(k, 10, 0.3)), aes(k, p)) +
  geom_col(fill = ie_blue, alpha = .7) + labs(x = "k", y = "P(X = k)")
q90 <- qgamma(0.9, 3, 2)
g2 <- ggplot(data.frame(x = c(0, 6)), aes(x)) +
  stat_function(fun = dgamma, args = list(shape = 3, rate = 2), colour = ie_navy, linewidth = 1) +
  geom_vline(xintercept = q90, colour = ie_coral, linetype = 2) + labs(x = "x", y = "density")
grid.arrange(g1, g2, ncol = 2)

k <- 0:6
round(rbind(binomial = dbinom(k, 100, 0.03), poisson = dpois(k, 3)), 4)
1 - pbinom(5, 100, 0.03)    # P(X >= 6) exactly
1 - ppois(5, 3)             # with the Poisson approximation

set.seed(1); runif(3)
set.seed(1); runif(3)     # same numbers
runif(3)                  # new numbers: the generator moved on

set.seed(10)
rbinom(5, size = 10, prob = 0.3)
rpois(5, lambda = 3)
round(rexp(3, rate = 0.5), 3)
round(rnorm(3, mean = 100, sd = 15), 2)
sample(1:6, size = 5, replace = TRUE)          # five dice
sample(c("H", "T"), 8, replace = TRUE, prob = c(0.7, 0.3))   # biased coin
sample(1:10)                                   # a random permutation (no replacement)

x <- rgamma(5000, shape = 3, rate = 2)
ggplot(data.frame(x), aes(x)) + geom_histogram(aes(y = after_stat(density)), bins = 40,
  fill = ie_blue, alpha = .5, colour = "white") +
  stat_function(fun = dgamma, args = list(shape = 3, rate = 2), colour = ie_coral, linewidth = 1) +
  labs(x = "x", y = "density")

set.seed(2027)
N <- 10000
has_match <- replicate(N, any(duplicated(sample(1:365, 23, replace = TRUE))))
p_hat <- mean(has_match)
se <- sqrt(p_hat * (1 - p_hat) / N)
c(estimate = p_hat, se = se, lower = p_hat - 1.96 * se, upper = p_hat + 1.96 * se)
1 - prod((365 - 0:22) / 365)       # exact

set.seed(5)
n <- 2000
u <- runif(n); v <- runif(n)
inside <- u^2 + v^2 <= 1
ggplot(data.frame(u, v, inside), aes(u, v, colour = inside)) + geom_point(size = 0.6) +
  scale_colour_manual(values = c(ie_coral, ie_blue)) + coord_fixed() + theme(legend.position = "none") +
  labs(x = NULL, y = NULL)
4 * mean(inside)

set.seed(2027)
n <- 5000
d <- do.call(rbind, lapply(1:5, function(r) {
  x <- rexp(n, rate = 2)
  data.frame(n = 1:n, running = cumsum(x) / (1:n), run = factor(r))
}))
ggplot(d, aes(n, running, colour = run)) + geom_line(alpha = .8) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_colour_manual(values = ie_pal[c(1:4, 6)]) + scale_x_log10() +
  labs(x = "n (log scale)", y = "running mean", colour = "run") + theme(legend.position = "none")

set.seed(3)
tosses <- rbinom(1e5, 1, 0.5)
n <- c(100, 1000, 10000, 100000)
data.frame(n, proportion = cumsum(tosses)[n] / n, heads_minus_tails = 2 * cumsum(tosses)[n] - n)

set.seed(7)
u <- runif(1e5)
x <- -log(u) / 2
ggplot(data.frame(x), aes(x)) + geom_histogram(aes(y = after_stat(density)), bins = 50,
  fill = ie_blue, alpha = .5, colour = "white") +
  stat_function(fun = dexp, args = list(rate = 2), colour = ie_coral, linewidth = 1) +
  coord_cartesian(xlim = c(0, 3)) + labs(x = "x", y = "density")
c(mean(x), 1 / 2)                       # sample mean vs mean
c(mean(x > 1), exp(-2))                 # P(X > 1)

set.seed(8)
x <- runif(1e5)^(-1 / 2)
c(median(x), sqrt(2))
c(mean(x > 3), 1 / 9)

set.seed(9)
p <- 0.25
u <- runif(1e5)
x <- ceiling(log(u) / log(1 - p))                # trials until the first success
rbind(simulated = as.numeric(table(factor(x, levels = 1:6))) / 1e5,
      exact = dgeom(0:5, p))                     # dgeom(k - 1, p), k = 1,...,6

set.seed(11)
s <- sample(1:6, 1e5, TRUE) + sample(1:6, 1e5, TRUE)
d <- data.frame(s = 2:12, sim = as.numeric(table(factor(s, levels = 2:12))) / 1e5,
                exact = c(1:6, 5:1) / 36)
ggplot(d, aes(s)) + geom_col(aes(y = sim), fill = ie_blue, alpha = .6) +
  geom_point(aes(y = exact), colour = ie_coral, size = 2.5) + labs(x = "sum", y = "probability")
round(d, 4)

set.seed(12)
x1 <- rbinom(1e5, 10, 0.3)
x2 <- colSums(matrix(rbinom(10 * 1e5, 1, 0.3), nrow = 10))
rbind(rbinom = mean(x1 == 3), sum_of_bernoulli = mean(x2 == 3), exact = dbinom(3, 10, 0.3))

set.seed(13)
s <- rexp(1e5, 2) + rexp(1e5, 2) + rexp(1e5, 2)
ggplot(data.frame(s), aes(s)) + geom_histogram(aes(y = after_stat(density)), bins = 50,
  fill = ie_blue, alpha = .5, colour = "white") +
  stat_function(fun = dgamma, args = list(shape = 3, rate = 2), colour = ie_coral, linewidth = 1) +
  labs(x = "sum", y = "density")
c(mean(s <= 2), pgamma(2, 3, 2))
c(mean(s), 3 / 2); c(var(s), 3 / 4)

set.seed(14)
s12 <- colSums(matrix(runif(12 * 1e5), nrow = 12))
c(mean(s12), var(s12))
c(mean(s12 > 7), 1 - pnorm(7, 6, 1))   # simulation vs normal approximation (Chapter 10)

# set.seed(2027)
# one_run <- function() {
#   # simulate one repetition of the experiment; return TRUE/FALSE or a number
# }
# N <- 10000
# res <- replicate(N, one_run())
# est <- mean(res)
# se  <- sd(res) / sqrt(N)       # for 0/1 results this is sqrt(est * (1 - est) / N) up to N/(N-1)
# c(estimate = est, se = se)
