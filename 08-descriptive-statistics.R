source("_common.R")

head(mtcars[, c("mpg", "cyl", "hp", "wt")])
str(mtcars[, c("mpg", "cyl", "wt")])

tab <- table(mtcars$cyl)
tab
round(prop.table(tab), 3)

ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = ie_blue) +
  labs(x = "cylinders", y = "number of cars")

p1 <- ggplot(faithful, aes(waiting)) + geom_histogram(bins = 8, fill = ie_blue, colour = "white") +
  labs(title = "8 bins", x = "waiting time (min)")
p2 <- ggplot(faithful, aes(waiting)) + geom_histogram(bins = 30, fill = ie_blue, colour = "white") +
  labs(title = "30 bins", x = "waiting time (min)")
gridExtra::grid.arrange(p1, p2, ncol = 2)

ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot(fill = ie_blue, alpha = .4, colour = ie_navy) +
  labs(x = "cylinders", y = "miles per gallon")

ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = ie_navy, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = ie_coral) +
  labs(x = "weight (1000 lb)", y = "miles per gallon")

mpg <- mtcars$mpg
c(mean = mean(mpg), median = median(mpg), trimmed10 = mean(mpg, trim = 0.1))

mode_of <- function(x) { t <- table(x); as.numeric(names(t)[t == max(t)]) }
mode_of(mtcars$cyl)
mode_of(mtcars$mpg)   # ties: several values appear twice

x <- c(4, 7, 7, 9, 13)
c(var = var(x), sd = sd(x), range = diff(range(x)), IQR = IQR(x), MAD = mad(x))
c(mpg_sd = sd(mpg), mpg_IQR = IQR(mpg), mpg_MAD = mad(mpg))

skew <- function(x) { d <- x - mean(x); mean(d^3) / mean(d^2)^1.5 }
kurt <- function(x) { d <- x - mean(x); mean(d^4) / mean(d^2)^2 }
oz <- na.omit(airquality$Ozone)
rbind(ozone = c(mean = mean(oz), median = median(oz), skew = skew(oz), kurt = kurt(oz)),
      normal_sample = { z <- rnorm(5000); c(mean(z), median(z), skew(z), kurt(z)) })

ggplot(data.frame(oz), aes(oz)) + geom_histogram(bins = 25, fill = ie_blue, alpha = .6, colour = "white") +
  geom_vline(xintercept = median(oz), colour = ie_navy, linewidth = 1) +
  geom_vline(xintercept = mean(oz), colour = ie_coral, linewidth = 1, linetype = 2) +
  labs(x = "ozone (ppb)", y = "count")

a <- c(10, 12, 13, 15, 16); b <- c(a, 100)
rbind(without = c(mean = mean(a), median = median(a), sd = sd(a), IQR = IQR(a), MAD = mad(a)),
      with    = c(mean(b), median(b), sd(b), IQR(b), mad(b)))

summary(mpg)
q <- quantile(mpg, c(.25, .75)); iqr <- diff(q)
fences <- c(q[1] - 1.5 * iqr, q[2] + 1.5 * iqr)
fences
mpg[mpg < fences[1] | mpg > fences[2]]
boxplot.stats(mtcars$hp)$out

z <- scale(mpg)[, 1]               # (x - mean) / sd
c(mean = mean(z), sd = sd(z))
mtcars[abs(z) > 2, "mpg", drop = FALSE]

xx <- c(1, 2, 3, 4); yy <- c(2, 3, 5, 6)
c(cov = cov(xx, yy), cor = cor(xx, yy))
cor(mtcars[, c("mpg", "wt", "hp")])

set.seed(2027)
u <- rnorm(30); v <- 0.9 * u + rnorm(30, sd = 0.4)
d <- data.frame(u, v)
d2 <- rbind(d, data.frame(u = 4, v = -4))
p1 <- ggplot(d, aes(u, v)) + geom_point(colour = ie_navy) + labs(title = sprintf("r = %.2f", cor(d$u, d$v)))
p2 <- ggplot(d2, aes(u, v)) + geom_point(colour = ie_navy) +
  geom_point(data = tail(d2, 1), colour = ie_coral, size = 3) + labs(title = sprintf("r = %.2f", cor(d2$u, d2$v)))
gridExtra::grid.arrange(p1, p2, ncol = 2)

set.seed(2027)
s <- rnorm(25)
ggplot(data.frame(s), aes(s)) + stat_ecdf(geom = "step", colour = ie_navy) +
  stat_function(fun = pnorm, colour = ie_coral) + labs(x = "x", y = "F(x)")
Fn <- ecdf(c(2, 3, 3, 5, 8)); Fn(c(3, 4, 5))
