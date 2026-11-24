source("_common.R")

ggplot(cars, aes(speed, dist)) +
  geom_point(colour = ie_navy, alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = ie_coral) +
  labs(x = "Speed (mph)", y = "Stopping distance (ft)")

x <- 1:5; y <- c(2, 4, 5, 4, 5)
Sxx <- sum((x - mean(x))^2); Sxy <- sum((x - mean(x)) * (y - mean(y)))
c(b1 = Sxy / Sxx, b0 = mean(y) - Sxy / Sxx * mean(x))
coef(lm(y ~ x))

fit <- lm(dist ~ speed, data = cars)
coef(fit)

summary(fit)
confint(fit)

set.seed(15)
b1 <- replicate(2000, {
  ysim <- -17.58 + 3.93 * cars$speed + rnorm(50, sd = 15.38)
  coef(lm(ysim ~ cars$speed))[2]
})
c(mean = mean(b1), sd = sd(b1), theory_sd = 15.38 / sqrt(1370))
ggplot(data.frame(b1), aes(b1)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = ie_blue, alpha = 0.6, colour = "white") +
  stat_function(fun = dnorm, args = list(mean = 3.93, sd = 15.38 / sqrt(1370)), colour = ie_navy, linewidth = 0.9) +
  labs(x = expression(hat(beta)[1]), y = "density")

anova(fit)
c(R2 = summary(fit)$r.squared, r_squared = cor(cars$speed, cars$dist)^2)

new <- data.frame(speed = 15)
predict(fit, new, interval = "confidence")
predict(fit, new, interval = "prediction")

grid <- data.frame(speed = seq(4, 25, length.out = 100))
ci <- cbind(grid, predict(fit, grid, interval = "confidence"))
pi <- cbind(grid, predict(fit, grid, interval = "prediction"))
ggplot() +
  geom_ribbon(data = pi, aes(speed, ymin = lwr, ymax = upr), fill = ie_blue, alpha = 0.15) +
  geom_ribbon(data = ci, aes(speed, ymin = lwr, ymax = upr), fill = ie_navy, alpha = 0.35) +
  geom_line(data = ci, aes(speed, fit), colour = ie_coral) +
  geom_point(data = cars, aes(speed, dist), colour = ie_navy, alpha = 0.8) +
  labs(x = "Speed (mph)", y = "Stopping distance (ft)")

library(gridExtra)
dd <- data.frame(fitted = fitted(fit), resid = resid(fit), std = rstandard(fit))
d1 <- ggplot(dd, aes(fitted, resid)) + geom_point(colour = ie_navy) +
  geom_hline(yintercept = 0, linetype = 2, colour = ie_coral) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = ie_green, linewidth = 0.6) +
  labs(x = "Fitted values", y = "Residuals")
d2 <- ggplot(dd, aes(sample = std)) + stat_qq(colour = ie_navy) + stat_qq_line(colour = ie_coral) +
  labs(x = "Theoretical quantiles", y = "Standardised residuals")
grid.arrange(d1, d2, ncol = 2)

head(sort(cooks.distance(fit), decreasing = TRUE), 3)
head(sort(abs(rstandard(fit)), decreasing = TRUE), 3)
max(hatvalues(fit))

w1 <- lm(weight ~ height, women)
w2 <- lm(weight ~ height + I(height^2), women)
r1 <- ggplot(data.frame(fit = fitted(w1), res = resid(w1)), aes(fit, res)) +
  geom_point(colour = ie_navy) + geom_hline(yintercept = 0, linetype = 2, colour = ie_coral) +
  labs(title = "Linear", x = "Fitted", y = "Residual")
r2 <- ggplot(data.frame(fit = fitted(w2), res = resid(w2)), aes(fit, res)) +
  geom_point(colour = ie_navy) + geom_hline(yintercept = 0, linetype = 2, colour = ie_coral) +
  labs(title = "Quadratic", x = "Fitted", y = "Residual")
grid.arrange(r1, r2, ncol = 2)
c(R2_linear = summary(w1)$r.squared, R2_quadratic = summary(w2)$r.squared)

tr <- lm(log(Volume) ~ log(Girth) + log(Height), data = trees)
summary(tr)$coefficients
confint(tr)

X <- cbind(1, x); yy <- y                      # the five-point example
solve(t(X) %*% X) %*% t(X) %*% yy             # (X'X)^{-1} X'y

m2 <- lm(mpg ~ wt + hp, data = mtcars)
summary(m2)

predict(m2, data.frame(wt = 3, hp = 150), interval = "prediction")

pm <- lm(weight ~ group, data = PlantGrowth)
summary(pm)
head(model.matrix(pm)[c(1, 11, 21), ])

m3 <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
anova(m2, m3)

m1 <- lm(mpg ~ wt, data = mtcars)
AIC(m1, m2, m3)

full <- lm(Fertility ~ ., data = swiss)
selected <- step(full, trace = 0)
formula(selected)
c(AIC_full = AIC(full), AIC_selected = AIC(selected))
c(adjR2_full = summary(full)$adj.r.squared, adjR2_selected = summary(selected)$adj.r.squared)
summary(selected)$coefficients

vars <- c("wt", "disp", "hp")
vif <- sapply(vars, function(v) {
  r2 <- summary(lm(reformulate(setdiff(vars, v), v), data = mtcars))$r.squared
  1 / (1 - r2)
})
round(vif, 2)
m4 <- lm(mpg ~ wt + disp + hp, data = mtcars)
summary(m4)$coefficients

aq <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
summary(aq)$coefficients
c(n_used = nobs(aq), n_total = nrow(airquality), R2 = summary(aq)$r.squared)
