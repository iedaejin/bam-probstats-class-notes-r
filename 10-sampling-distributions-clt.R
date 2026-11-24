source("_common.R")

mu_r <- mean(rivers); sigma_r <- sqrt(mean((rivers - mu_r)^2))
c(N = length(rivers), mu = mu_r, sigma = sigma_r)
ggplot(data.frame(x = rivers), aes(x)) +
  geom_histogram(bins = 25, fill = ie_blue, colour = "white") +
  geom_vline(xintercept = mu_r, colour = ie_coral, linewidth = 1) +
  labs(x = "length (miles)", y = "count")

set.seed(2027)
sim_means <- function(n, B = 5000) replicate(B, mean(sample(rivers, n, replace = TRUE)))
d <- rbind(data.frame(xbar = sim_means(10), n = "n = 10"),
           data.frame(xbar = sim_means(40), n = "n = 40"))
ggplot(d, aes(xbar)) + geom_histogram(bins = 40, fill = ie_navy, colour = "white") +
  geom_vline(xintercept = mu_r, colour = ie_coral, linewidth = 1) +
  facet_wrap(~n) + labs(x = "sample mean (miles)", y = "count")
tapply(d$xbar, d$n, sd)

pnorm(498.5, mean = 500, sd = 8 / sqrt(64))
pnorm(2) - pnorm(-2)

set.seed(2027)
n <- 1000
d <- do.call(rbind, lapply(1:3, function(r) {
  x <- sample(1:6, n, replace = TRUE)
  data.frame(n = 1:n, runmean = cumsum(x) / seq_len(n), run = factor(r))
}))
ggplot(d, aes(n, runmean, colour = run)) + geom_line() +
  geom_hline(yintercept = 3.5, linetype = 2) +
  scale_colour_manual(values = c(ie_navy, ie_coral, ie_green)) +
  labs(x = "number of rolls n", y = "running mean", colour = "run")

set.seed(2027)
B <- 4000
gen <- list(
  "Exponential(1)" = list(f = function(n) rexp(n, 1), mu = 1, sd = 1),
  "Bernoulli(0.1)" = list(f = function(n) rbinom(n, 1, 0.1), mu = 0.1, sd = sqrt(0.09)),
  "Rivers" = list(f = function(n) sample(rivers, n, replace = TRUE), mu = mu_r, sd = sigma_r))
d <- do.call(rbind, lapply(names(gen), function(nm) {
  g <- gen[[nm]]
  do.call(rbind, lapply(c(1, 5, 30, 100), function(n) {
    z <- replicate(B, (mean(g$f(n)) - g$mu) / (g$sd / sqrt(n)))
    data.frame(z = z, pop = nm, n = factor(paste("n =", n), levels = paste("n =", c(1, 5, 30, 100))))
  }))
}))
ggplot(d, aes(z)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = ie_blue, colour = "white") +
  stat_function(fun = dnorm, colour = ie_coral) +
  facet_grid(pop ~ n, scales = "free_y") + coord_cartesian(xlim = c(-4, 4)) +
  labs(x = "standardised sample mean", y = "density")

set.seed(2027)
x <- rcauchy(5000)
ggplot(data.frame(n = 1:5000, m = cumsum(x) / seq_along(x)), aes(n, m)) +
  geom_line(colour = ie_coral) + labs(x = "n", y = "running mean")

1 - pnorm(210, mean = 80 * 2.5, sd = 1.2 * sqrt(80))

1 - pnorm(0.30, mean = 0.25, sd = sqrt(0.25 * 0.75 / 300))

c(exact = 1 - pbinom(14, 60, 0.2),
  corrected = 1 - pnorm(14.5, 12, sqrt(9.6)),
  uncorrected = 1 - pnorm(15, 12, sqrt(9.6)))

k <- 0:35
d <- data.frame(k, p = dbinom(k, 60, 0.2))
ggplot(d, aes(k, p)) +
  geom_col(aes(fill = k >= 15), width = 1, colour = "white", show.legend = FALSE) +
  scale_fill_manual(values = c(ie_blue, ie_coral)) +
  stat_function(fun = dnorm, args = list(mean = 12, sd = sqrt(9.6)), colour = ie_navy, linewidth = 1) +
  geom_vline(xintercept = 14.5, linetype = 2) + labs(x = "k", y = "probability")

c(clt = 1 - pnorm((1.5 - 1) * sqrt(30)), exact = 1 - pgamma(45, shape = 30, rate = 1))

library(gridExtra)
p1 <- ggplot(data.frame(x = c(0, 25)), aes(x)) +
  stat_function(fun = dchisq, args = list(df = 3), colour = ie_navy) +
  stat_function(fun = dchisq, args = list(df = 8), colour = ie_coral) +
  stat_function(fun = dchisq, args = list(df = 15), colour = ie_green) +
  labs(x = "x", y = "density", title = "chi-square: 3, 8, 15 df")
p2 <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, colour = "grey40", linetype = 2) +
  stat_function(fun = dt, args = list(df = 2), colour = ie_coral) +
  stat_function(fun = dt, args = list(df = 10), colour = ie_navy) +
  labs(x = "x", y = "density", title = "t: 2, 10 df (dashed: N(0,1))")
p3 <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = df, args = list(df1 = 5, df2 = 10), colour = ie_navy) +
  stat_function(fun = df, args = list(df1 = 20, df2 = 15), colour = ie_coral) +
  labs(x = "x", y = "density", title = "F: (5,10) and (20,15) df")
grid.arrange(p1, p2, p3, ncol = 3)

alpha <- 0.05
c(z = qnorm(1 - alpha / 2),
  t5 = qt(1 - alpha / 2, 5), t30 = qt(1 - alpha / 2, 30), t100 = qt(1 - alpha / 2, 100))
c(chisq_upper = qchisq(1 - alpha / 2, 9), chisq_lower = qchisq(alpha / 2, 9))  # chi2_{9,.025}, chi2_{9,.975}
c(F_10_15 = qf(1 - alpha, 10, 15), F_15_10_lower_check = 1 / qf(alpha, 15, 10))

pchisq(22.5, df = 15, lower.tail = FALSE)
set.seed(2027)
mean(replicate(20000, var(rnorm(16, 0, 2)) > 6))   # simulation check
