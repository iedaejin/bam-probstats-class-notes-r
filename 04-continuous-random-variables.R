source("_common.R")

mk <- function(w) {
  k <- seq(0, 10 - w, by = w)
  data.frame(x = k + w / 2, w = w, dens = (w / 10) / w, bin = paste("width", w))
}
d <- rbind(mk(2), mk(1), mk(0.5))
ggplot(d, aes(x, dens, width = w)) + geom_col(fill = ie_blue, alpha = .6, colour = "white") +
  facet_wrap(~ factor(bin, levels = unique(bin)), nrow = 1) +
  geom_hline(yintercept = 0.1, colour = ie_coral) +
  labs(x = "waiting time (minutes)", y = "probability / width")

f <- function(x) ifelse(x >= 0 & x <= 4, 3 / 32 * x * (4 - x), 0)
integrate(f, 0, 4)$value          # total area
integrate(f, 1, 3)$value          # P(1 <= X <= 3)
11 / 16
x <- seq(0, 4, length.out = 300)
d <- data.frame(x, y = f(x))
ggplot(d, aes(x, y)) + geom_line(colour = ie_navy, linewidth = 1) +
  geom_area(data = subset(d, x >= 1 & x <= 3), fill = ie_coral, alpha = .4) +
  labs(x = "x", y = "density")

fd <- function(x) ifelse(x < 0 | x > 2, 0, ifelse(x <= 1, x, 2 - x))
Fd <- function(x) ifelse(x < 0, 0, ifelse(x <= 1, x^2 / 2, ifelse(x <= 2, 1 - (2 - x)^2 / 2, 1)))
x <- seq(-0.5, 2.5, length.out = 400)
d <- data.frame(x = rep(x, 2), y = c(fd(x), Fd(x)),
                what = rep(c("density f(x)", "CDF F(x)"), each = length(x)))
ggplot(d, aes(x, y)) + geom_line(colour = ie_navy, linewidth = 1) + facet_wrap(~ what, scales = "free_y") +
  labs(x = "x", y = NULL)
c(1 - Fd(1.5), Fd(1.5) - Fd(0.5))

uniroot(function(x) Fd(x) - 0.9, c(1, 2))$root
2 - sqrt(0.2)

x <- runif(1e5, -1, 1)
y <- x^2
ggplot(data.frame(y), aes(y)) + geom_histogram(aes(y = after_stat(density)), bins = 40,
       fill = ie_blue, alpha = .5, colour = "white") +
  stat_function(fun = function(y) 1 / (2 * sqrt(y)), colour = ie_coral, linewidth = 1) +
  coord_cartesian(ylim = c(0, 4)) + labs(x = "y", y = "density")
mean(y <= 0.25); sqrt(0.25)      # P(Y <= 1/4) = sqrt(1/4)

c(1 - punif(10, 0, 15), punif(7, 0, 15) - punif(3, 0, 15))
(1 - punif(10, 0, 15)) / (1 - punif(5, 0, 15))

pnorm(1:3) - pnorm(-(1:3))
c(qnorm(0.975), qnorm(0.95), qnorm(0.995))
z <- seq(-4, 4, length.out = 500)
d <- data.frame(z, y = dnorm(z))
ggplot(d, aes(z, y)) +
  geom_area(data = subset(d, abs(z) <= 3), fill = ie_blue, alpha = .2) +
  geom_area(data = subset(d, abs(z) <= 2), fill = ie_blue, alpha = .3) +
  geom_area(data = subset(d, abs(z) <= 1), fill = ie_blue, alpha = .5) +
  geom_line(colour = ie_navy, linewidth = 1) +
  annotate("text", x = 0, y = 0.2, label = "68%", colour = "white") +
  annotate("text", x = 1.5, y = 0.04, label = "95%") +
  annotate("text", x = 2.6, y = 0.012, label = "99.7%") +
  labs(x = "z", y = "density")

1 - pnorm(130, 100, 15)
pnorm(115, 100, 15) - pnorm(85, 100, 15)
qnorm(0.9, 100, 15)

pexp(4, rate = 1/6); exp(-10/6)
(1 - pexp(13, 1/6)) / (1 - pexp(3, 1/6))

set.seed(1)
gaps <- rexp(15, rate = 2)
times <- cumsum(gaps)
ggplot(data.frame(t = times, y = 0), aes(t, y)) + geom_segment(aes(xend = t, yend = 1), colour = ie_blue) +
  geom_point(colour = ie_coral) + labs(x = "time", y = NULL) + theme(axis.text.y = element_blank())
c(mean_gap = mean(gaps), theory = 1/2)

x <- seq(0, 14, length.out = 400)
d <- do.call(rbind, lapply(c(1, 2, 5), function(a)
  data.frame(x, y = dgamma(x, shape = a, rate = 1), shape = factor(a))))
ggplot(d, aes(x, y, colour = shape)) + geom_line(linewidth = 1) +
  scale_colour_manual(values = c(ie_navy, ie_coral, ie_green)) + labs(x = "x", y = "density", colour = "shape")

pgamma(0.5, shape = 4, rate = 6)
1 - ppois(3, lambda = 6 * 0.5)

n <- 200; p <- 0.3; s <- sqrt(n * p * (1 - p))
c(exact = pbinom(50, n, p), with_cc = pnorm(50.5, n * p, s), without_cc = pnorm(50, n * p, s))

k <- 35:85
d <- data.frame(k, pmf = dbinom(k, n, p))
ggplot(d, aes(k, pmf)) + geom_col(fill = ie_blue, alpha = .5) +
  stat_function(fun = dnorm, args = list(mean = n * p, sd = s), colour = ie_coral, linewidth = 1) +
  geom_vline(xintercept = 50.5, linetype = 2) + labs(x = "k", y = "probability")
