source("_common.R")

y <- as.numeric(discoveries)      # great discoveries per year, 1860-1959
c(n = length(y), mean = mean(y), median = median(y), variance = var(y))

set.seed(2027)
est <- replicate(5000, { x <- rpois(30, 3); c(mean = mean(x), median = median(x)) })
d <- data.frame(value = c(est["mean", ], est["median", ]),
                estimator = rep(c("mean", "median"), each = 5000))
ggplot(d, aes(value, fill = estimator)) +
  geom_histogram(position = "identity", alpha = .5, bins = 40, colour = "white") +
  scale_fill_manual(values = c(ie_blue, ie_coral)) + labs(x = "estimate", y = "count")
apply(est, 1, function(v) c(mean = mean(v), sd = sd(v)))

set.seed(2027)
x <- rpois(2000, 3)
n <- 1:2000
d <- data.frame(n, running = cumsum(x) / n, ends = (x[1] + x) / 2)
ggplot(d, aes(n)) + geom_point(aes(y = ends), colour = "grey70", size = .6) +
  geom_line(aes(y = running), colour = ie_navy, linewidth = .8) +
  geom_hline(yintercept = 3, colour = ie_coral, linetype = 2) + labs(x = "n", y = "estimate")

set.seed(2027)
r <- replicate(20000, { x <- rnorm(50); c(mean(x), median(x)) })
v <- apply(r, 1, var)
c(var_mean = v[1], var_median = v[2], efficiency_of_median = unname(v[1] / v[2]), theory = 2 / pi)

set.seed(2027)
x <- rgamma(200, shape = 3, rate = 0.5)
s2 <- mean(x^2) - mean(x)^2
c(alpha_hat = mean(x)^2 / s2, lambda_hat = mean(x) / s2)    # true values 3 and 0.5

set.seed(2027)
xu <- runif(5, 0, 4)
th <- seq(1, 8, length.out = 400)
L <- ifelse(th >= max(xu), th^(-5), 0)
ggplot(data.frame(th, L), aes(th, L)) + geom_line(colour = ie_navy, linewidth = 1) +
  geom_vline(xintercept = max(xu), linetype = 2, colour = ie_coral) +
  labs(x = expression(theta), y = "likelihood")

set.seed(2027)
theta <- 5; n <- 10
sim <- t(replicate(10000, { x <- runif(n, 0, theta)
  c(MoM = 2 * mean(x), MLE = max(x), corrected = (n + 1) / n * max(x)) }))
res <- data.frame(bias = colMeans(sim) - theta, variance = apply(sim, 2, var))
res$MSE <- res$variance + res$bias^2
res
exact <- c(theta^2 / (3 * n), 2 * theta^2 / ((n + 1) * (n + 2)), theta^2 / (n * (n + 2)))
exact
d <- data.frame(value = as.vector(sim), estimator = rep(colnames(sim), each = nrow(sim)))
ggplot(d, aes(value, colour = estimator)) + geom_density(linewidth = 1) +
  geom_vline(xintercept = theta, linetype = 2) +
  scale_colour_manual(values = c(ie_green, ie_coral, ie_navy)) + labs(x = "estimate", y = "density")

negll <- function(lambda) -sum(dpois(y, lambda, log = TRUE))
optimize(negll, interval = c(0.01, 20))$minimum    # compare with mean(y) = 3.1

negll_g <- function(par) -sum(dgamma(precip, shape = exp(par[1]), rate = exp(par[2]), log = TRUE))
fit <- optim(c(log(2), log(0.1)), negll_g)
exp(fit$par)                                        # MLE of (alpha, lambda)
MASS::fitdistr(precip, "gamma")$estimate            # same, from MASS
m <- mean(precip); s2 <- mean(precip^2) - m^2
c(alpha_mom = m^2 / s2, lambda_mom = m / s2)        # method-of-moments for comparison
