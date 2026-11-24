source("_common.R")

x <- seq(-4, 4, length.out = 400)
d <- data.frame(x, f = dnorm(x))
ggplot(d, aes(x, f)) + geom_line(colour = ie_navy) +
  geom_area(data = subset(d, x <= qnorm(0.05)), fill = ie_coral, alpha = 0.5) +
  geom_vline(xintercept = -1.75, linetype = 2) +
  labs(x = "z", y = "density")

z <- (246.5 - 250) / (12 / sqrt(36))
c(z = z, p_left = pnorm(z), p_two_sided = 2 * pnorm(-abs(z)))

mu0 <- 250; sigma <- 12; n <- 36; se <- sigma / sqrt(n)
crit <- mu0 - qnorm(0.95) * se
c(crit = crit, power_247 = pnorm((crit - 247) / se), power_245 = pnorm((crit - 245) / se))
ceiling(((qnorm(0.95) + qnorm(0.90)) * sigma / 3)^2)

mu <- seq(240, 252, by = 0.1)
d <- do.call(rbind, lapply(c(16, 36, 100), function(n) {
  crit <- 250 - qnorm(0.95) * 12 / sqrt(n)
  data.frame(mu, power = pnorm((crit - mu) / (12 / sqrt(n))), n = paste("n =", n))
}))
ggplot(d, aes(mu, power, colour = n)) + geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.05, linetype = 3) +
  scale_colour_manual(values = c(ie_coral, ie_navy, ie_green)) +
  labs(x = expression(true~mean~mu), y = "power", colour = NULL)

set.seed(2027)
mean(replicate(10000, t.test(rnorm(36, mean = 247, sd = 12), mu = 250,
                             alternative = "less")$p.value < 0.05))
power.t.test(n = 36, delta = 3, sd = 12, sig.level = 0.05,
             type = "one.sample", alternative = "one.sided")$power

set.seed(2027)
mean(replicate(10000, t.test(rnorm(20), mu = 0)$p.value < 0.05))

x <- mtcars$mpg
t.test(x, mu = 18)
t.test(x, mu = 18, alternative = "greater")$p.value

prop.test(44, 150, p = 0.25, alternative = "greater", correct = FALSE)$p.value
prop.test(44, 150, p = 0.25, correct = FALSE)     # two-sided; X-squared = z^2
binom.test(44, 150, p = 0.25, alternative = "greater")$p.value   # exact binomial

oj <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
vc <- ToothGrowth$len[ToothGrowth$supp == "VC"]
t.test(oj, vc)$p.value
t.test(oj, vc, var.equal = TRUE)$p.value

g1 <- sleep$extra[sleep$group == 1]; g2 <- sleep$extra[sleep$group == 2]
t.test(g2, g1, paired = TRUE)$p.value
t.test(g2, g1)$p.value      # wrong here: ignores the pairing

x <- mtcars$mpg; n <- length(x)
stat <- (n - 1) * var(x) / 25
c(stat = stat, crit = qchisq(0.95, n - 1), p_right = pchisq(stat, n - 1, lower.tail = FALSE))

a <- mtcars$mpg[mtcars$am == 1]; b <- mtcars$mpg[mtcars$am == 0]
var.test(a, b)
qf(c(0.025, 0.975), 12, 18)
var.test(oj, vc)$p.value          # tooth growth: OJ vs VC
