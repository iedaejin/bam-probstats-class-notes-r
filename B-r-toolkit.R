source("_common.R")

x <- 7            # assignment: the arrow <- is preferred, = also works
x * 2 + 1         # arithmetic; ^ is a power, %% remainder, %/% integer division
sqrt(16); exp(1); log(100, base = 10)   # log() is the natural logarithm
5 > 3; 5 == 3     # comparisons give TRUE/FALSE; == tests equality, = assigns

v <- c(4, 8, 15, 16, 23, 42)
v / 2; v[v > 10]; length(v); sum(v); mean(v)
1:5; seq(0, 1, by = 0.25); rep(c("a", "b"), times = 3)

f <- factor(c("low", "high", "mid", "low"), levels = c("low", "mid", "high"))
levels(f); table(f)
df <- data.frame(id = 1:4, group = f, score = c(5.1, 7.3, 6.2, 4.8))
str(df)
l <- list(name = "sample", values = v, table = df)
names(l)

v[2]; v[c(1, 3)]; v[-1]
df[2, ]                     # second row
df[, "score"]               # a column, as a vector
df$score[df$score > 5]      # scores above 5
subset(df, group != "low", select = c(id, score))
l[["values"]][1:2]
which(v > 15); which.max(v)

tmp <- tempfile(fileext = ".csv")
write.csv(head(mtcars[, c("mpg", "cyl", "wt")], 5), tmp, row.names = FALSE)
cars5 <- read.csv(tmp)
cars5
str(cars5); summary(cars5$mpg)

se_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))        # the last value is returned
}
se_mean(mtcars$mpg)
ci_mean <- function(x, level = 0.95) {
  n <- length(x); a <- 1 - level
  mean(x) + c(-1, 1) * qt(1 - a / 2, n - 1) * sd(x) / sqrt(n)
}
ci_mean(mtcars$mpg)

sapply(mtcars[, c("mpg", "wt", "hp")], mean)
tapply(mtcars$mpg, mtcars$cyl, mean)
total <- 0
for (i in 1:5) total <- total + i^2
total

sim <- replicate(10000, sum(sample(1:6, 4, replace = TRUE) == 6) >= 2)
mean(sim)                          # Monte Carlo estimate
1 - pbinom(1, size = 4, prob = 1/6)  # exact value

xbar <- replicate(5000, mean(rexp(30, rate = 1)))
c(mean = mean(xbar), sd = sd(xbar), theory_sd = 1 / sqrt(30))

op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
hist(mtcars$mpg, breaks = 8, main = "Histogram", xlab = "mpg", col = "grey85")
boxplot(mpg ~ cyl, data = mtcars, main = "Boxplot", xlab = "cylinders", ylab = "mpg")
par(op)

library(gridExtra)
p1 <- ggplot(mtcars, aes(mpg)) + geom_histogram(bins = 10, fill = ie_navy, colour = "white")
p2 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot(fill = "grey90") + labs(x = "cylinders")
p3 <- ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = ie_coral) +
  geom_smooth(method = "lm", se = FALSE, colour = ie_navy, linewidth = 0.6)
p4 <- ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = ie_blue) + labs(x = "cylinders", y = "count")
grid.arrange(p1, p2, p3, p4, ncol = 2)

dbinom(3, 10, 0.5)                    # P(X = 3)
pbinom(3, 10, 0.5)                    # P(X <= 3)
1 - pnorm(115, mean = 100, sd = 15)   # P(X > 115)
pnorm(115, 100, 15, lower.tail = FALSE)
qnorm(c(0.025, 0.975))
qt(0.975, df = 9)                     # upper 2.5% point of t_9

t.test(mtcars$mpg, mu = 20)           # H0: mean mpg = 20

oj <- ToothGrowth$len[ToothGrowth$supp == "OJ"]
vc <- ToothGrowth$len[ToothGrowth$supp == "VC"]
t.test(oj, vc)                            # Welch
t.test(oj, vc, var.equal = TRUE)$p.value  # pooled

before <- sleep$extra[sleep$group == 1]; after <- sleep$extra[sleep$group == 2]
t.test(after, before, paired = TRUE)

prop.test(45, 100, p = 0.5)                 # one proportion
prop.test(c(45, 60), c(100, 110))           # two proportions
binom.test(7, 20, p = 0.5)                  # exact

var.test(len ~ supp, data = ToothGrowth)    # H0: ratio of variances = 1

tab <- table(mtcars$cyl, mtcars$am, dnn = c("cyl", "am"))
tab
chisq.test(tab)$expected               # check: expected counts should be >= 5
chisq.test(c(18, 55, 27), p = c(0.2, 0.5, 0.3))   # goodness of fit
fisher.test(table(mtcars$am, mtcars$vs))

fit_aov <- aov(weight ~ group, data = PlantGrowth)
summary(fit_aov)
TukeyHSD(fit_aov)

fit <- lm(mpg ~ wt, data = mtcars)
summary(fit)
confint(fit)
predict(fit, newdata = data.frame(wt = 3), interval = "confidence")
predict(fit, newdata = data.frame(wt = 3), interval = "prediction")
anova(fit)

set.seed(123); rnorm(3)
set.seed(123); rnorm(3)     # identical

R.version.string
packageVersion("ggplot2") >= "3.0.0"
