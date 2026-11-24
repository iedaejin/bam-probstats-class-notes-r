source("_common.R")

set.seed(2027)
mu <- 50
ci <- t(replicate(40, t.test(rnorm(10, mu, 8))$conf.int[1:2]))
d <- data.frame(i = 1:40, lo = ci[, 1], hi = ci[, 2])
d$miss <- d$lo > mu | d$hi < mu
ggplot(d, aes(y = i, colour = miss)) +
  geom_segment(aes(x = lo, xend = hi, yend = i)) +
  geom_vline(xintercept = mu, linetype = 2) +
  scale_colour_manual(values = c(ie_navy, ie_coral), guide = "none") +
  labs(x = "95% confidence interval", y = "sample number")
sum(d$miss)

set.seed(2027)
cover <- replicate(10000, { x <- rnorm(10, mu, 8); ci <- t.test(x)$conf.int; ci[1] < mu & mu < ci[2] })
mean(cover)

n <- 32; xbar <- mean(mtcars$mpg); sigma <- 6
xbar + c(-1, 1) * qnorm(0.975) * sigma / sqrt(n)

x <- mtcars$mpg
c(n = length(x), mean = mean(x), sd = sd(x), se = sd(x) / sqrt(length(x)), tcrit = qt(0.975, 31))
t.test(x)$conf.int
t.test(x, conf.level = 0.90)$conf.int

n <- 3:100
ggplot(data.frame(n, w = 2 * qt(0.975, n - 1) / sqrt(n)), aes(n, w)) +
  geom_line(colour = ie_navy, linewidth = 1) +
  labs(x = "sample size n", y = "interval width / s")

ceiling((qnorm(0.975) * 6 / 1)^2); ceiling((qnorm(0.975) * 6 / 0.5)^2)

x <- 87; n <- 250; phat <- x / n; se <- sqrt(phat * (1 - phat) / n)
phat + c(-1, 1) * qnorm(0.975) * se                  # Wald
prop.test(x, n, correct = FALSE)$conf.int            # Wilson
prop.test(x, n)$conf.int                             # Wilson with continuity correction

set.seed(2027)
cover <- function(n, p, B = 10000) {
  x <- rbinom(B, n, p); ph <- x / n; se <- sqrt(ph * (1 - ph) / n)
  wald <- ph - qnorm(0.975) * se < p & p < ph + qnorm(0.975) * se
  wil <- mapply(function(xi) { ci <- prop.test(xi, n, correct = FALSE)$conf.int; ci[1] < p & p < ci[2] }, x)
  c(wald = mean(wald), wilson = mean(wil))
}
cover(30, 0.1)

oj <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
vc <- ToothGrowth$len[ToothGrowth$supp == "VC"]
t.test(oj, vc)$conf.int
t.test(oj, vc, var.equal = TRUE)$conf.int

g1 <- sleep$extra[sleep$group == 1]; g2 <- sleep$extra[sleep$group == 2]
d <- g2 - g1
c(mean = mean(d), sd = sd(d))
t.test(d)$conf.int
t.test(g2, g1, paired = TRUE)$conf.int    # same
t.test(g2, g1)$conf.int                   # wrongly ignoring the pairing

prop.test(c(45, 30), c(100, 100), correct = FALSE)$conf.int

x <- mtcars$mpg; n <- length(x); s2 <- var(x)
lims <- (n - 1) * s2 / qchisq(c(0.975, 0.025), n - 1)   # (lower, upper)
lims; sqrt(lims)

t.test(x, alternative = "greater")$conf.int
t.test(x, alternative = "less")$conf.int
(n - 1) * s2 / qchisq(0.05, n - 1, lower.tail = FALSE)  # 95% lower bound for sigma^2
(n - 1) * s2 / qchisq(0.95, n - 1, lower.tail = FALSE)  # 95% upper bound for sigma^2
