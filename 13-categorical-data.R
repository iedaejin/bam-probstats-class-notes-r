source("_common.R")

hair_eye <- margin.table(HairEyeColor, c(1, 2))
hair_eye
addmargins(hair_eye)

crit <- qchisq(0.95, 3)
x <- seq(0, 16, length.out = 400)
d <- data.frame(x, f = dchisq(x, 3))
ggplot(d, aes(x, f)) + geom_line(colour = ie_navy) +
  geom_area(data = subset(d, x > crit), fill = ie_coral, alpha = 0.5) +
  geom_vline(xintercept = crit, linetype = 2, colour = ie_coral) +
  labs(x = expression(X^2), y = "density")
crit

o <- c(18, 22, 16, 25, 14, 25)
sum((o - 20)^2 / 20)
qchisq(0.95, 5)
pchisq(5.5, 5, lower.tail = FALSE)
chisq.test(o)              # p = rep(1/6, 6) is the default

set.seed(13)
x2 <- replicate(5000, {
  counts <- tabulate(sample(1:6, 120, replace = TRUE), nbins = 6)
  sum((counts - 20)^2 / 20)
})
ggplot(data.frame(x2), aes(x2)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = ie_blue, alpha = 0.6, colour = "white") +
  stat_function(fun = dchisq, args = list(df = 5), colour = ie_navy, linewidth = 0.9) +
  geom_vline(xintercept = 5.5, colour = ie_coral, linetype = 2) +
  labs(x = expression(X^2), y = "density")
mean(x2 >= 5.5)   # simulated p-value

goals <- 0:4
obs <- c(35, 40, 18, 5, 2)
n <- sum(obs)
lambda_hat <- sum(goals * obs) / n
p <- c(dpois(0:2, lambda_hat), 1 - ppois(2, lambda_hat))   # pooled: 3 or more
e <- n * p
o <- c(obs[1:3], sum(obs[4:5]))
rbind(observed = o, expected = e)
x2 <- sum((o - e)^2 / e)
nu <- length(o) - 1 - 1
c(X2 = x2, df = nu, p.value = pchisq(x2, nu, lower.tail = FALSE))

test <- chisq.test(hair_eye)
test
round(test$expected, 1)     # E_ij = R_i C_j / n
qchisq(0.95, 9)

alc <- xtabs(cbind(ncases, ncontrols) ~ alcgp, data = esoph)
alc
test_alc <- chisq.test(alc)
test_alc
round(prop.table(alc, 1), 3)    # proportion of cases in each alcohol group

pilot <- matrix(c(8, 2, 3, 7), nrow = 2, byrow = TRUE,
                dimnames = list(method = c("new", "old"), result = c("pass", "fail")))
chisq.test(pilot)$expected
dhyper(8, 11, 9, 10)                  # probability of the observed table
fisher.test(pilot)

clicks <- matrix(c(60, 40, 45, 55), nrow = 2, byrow = TRUE)
chisq.test(clicks, correct = FALSE)$statistic
prop.test(c(60, 45), c(100, 100), correct = FALSE)$statistic
chisq.test(clicks)                 # default for 2x2: Yates continuity correction

round(test$stdres, 2)

res <- as.data.frame(as.table(test$stdres))
names(res) <- c("Hair", "Eye", "r")
ggplot(res, aes(Eye, Hair, fill = r)) + geom_tile(colour = "white") +
  geom_text(aes(label = round(r, 1)), size = 3.2) +
  scale_fill_gradient2(low = ie_blue, mid = "white", high = ie_coral, midpoint = 0) +
  labs(fill = "std. residual") + theme(legend.position = "right")

pooled <- margin.table(UCBAdmissions, c(1, 2))    # Admit x Gender
pooled
round(prop.table(pooled, 2), 3)                   # admission rate by sex
chisq.test(pooled, correct = FALSE)
round(apply(UCBAdmissions, 3, function(x) prop.table(x, 2)["Admitted", ]), 3)

n <- sum(hair_eye)
sqrt(unname(test$statistic) / (n * (min(dim(hair_eye)) - 1)))   # Cramer's V
