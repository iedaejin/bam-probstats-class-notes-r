source("_common.R")

head(PlantGrowth, 3)
aggregate(weight ~ group, PlantGrowth, function(x) c(n = length(x), mean = mean(x), sd = sd(x)))

ggplot(PlantGrowth, aes(group, weight)) +
  geom_boxplot(colour = ie_navy, fill = NA, outlier.shape = NA) +
  geom_jitter(width = 0.1, colour = ie_blue, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, colour = ie_coral) +
  geom_hline(yintercept = mean(PlantGrowth$weight), linetype = 2, colour = "grey40") +
  labs(x = "Condition", y = "Dry weight")

y <- PlantGrowth$weight; g <- PlantGrowth$group
n_i <- table(g); k <- nlevels(g); N <- length(y)
gm <- mean(y); m_i <- tapply(y, g, mean)
SSB <- sum(n_i * (m_i - gm)^2)
SSW <- sum((y - m_i[g])^2)
c(SSB = SSB, SSW = SSW, SST = sum((y - gm)^2))
Fobs <- (SSB / (k - 1)) / (SSW / (N - k))
c(F = Fobs, crit = qf(0.95, k - 1, N - k), p = pf(Fobs, k - 1, N - k, lower.tail = FALSE))

fit <- aov(weight ~ group, data = PlantGrowth)
summary(fit)

x <- seq(0, 8, length.out = 400)
d <- data.frame(x, f = df(x, 2, 27))
ggplot(d, aes(x, f)) + geom_line(colour = ie_navy) +
  geom_area(data = subset(d, x > Fobs), fill = ie_coral, alpha = 0.5) +
  geom_vline(xintercept = qf(0.95, 2, 27), linetype = 2, colour = "grey40") +
  geom_vline(xintercept = Fobs, colour = ie_coral) +
  labs(x = "F", y = "density")

s <- summary(fit)[[1]]
s[1, "Sum Sq"] / sum(s[, "Sum Sq"])

library(gridExtra)
res <- data.frame(fitted = fitted(fit), resid = resid(fit))
p1 <- ggplot(res, aes(fitted, resid)) + geom_jitter(width = 0.01, colour = ie_navy) +
  geom_hline(yintercept = 0, linetype = 2, colour = ie_coral) +
  labs(x = "Fitted value (group mean)", y = "Residual")
p2 <- ggplot(res, aes(sample = resid)) + stat_qq(colour = ie_navy) + stat_qq_line(colour = ie_coral) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")
grid.arrange(p1, p2, ncol = 2)

bartlett.test(weight ~ group, data = PlantGrowth)
pg <- PlantGrowth
pg$absdev <- abs(pg$weight - ave(pg$weight, pg$group, FUN = median))
summary(aov(absdev ~ group, data = pg))      # Brown-Forsythe / Levene test

b1 <- ggplot(InsectSprays, aes(spray, count)) + geom_boxplot(colour = ie_navy, fill = NA) +
  labs(x = "Spray", y = "Count")
b2 <- ggplot(InsectSprays, aes(spray, sqrt(count))) + geom_boxplot(colour = ie_navy, fill = NA) +
  labs(x = "Spray", y = "Square root of count")
grid.arrange(b1, b2, ncol = 2)
bartlett.test(count ~ spray, InsectSprays)$p.value
bartlett.test(sqrt(count) ~ spray, InsectSprays)$p.value
summary(aov(sqrt(count) ~ spray, InsectSprays))

oneway.test(weight ~ group, data = PlantGrowth)       # Welch's ANOVA
kruskal.test(weight ~ group, data = PlantGrowth)

TukeyHSD(fit)
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, p.adjust.method = "bonferroni")

tk <- as.data.frame(TukeyHSD(fit)$group)
tk$pair <- rownames(tk)
ggplot(tk, aes(diff, pair)) + geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.15, colour = ie_navy) +
  geom_point(colour = ie_coral, size = 2.5) + labs(x = "Difference of means", y = NULL)

sc <- data.frame(score = c(12,15,14, 14,18,17, 10,12,13, 16,20,19),
                 method = factor(rep(c("A", "B", "C"), 4)),
                 class = factor(rep(1:4, each = 3)))
summary(aov(score ~ class + method, data = sc))    # block model
summary(aov(score ~ method, data = sc))            # ignoring blocks (CRD analysis)

summary(aov(yield ~ N + P + K, data = npk))            # ignoring blocks
summary(aov(yield ~ block + N + P + K, data = npk))    # with blocks

ggplot(npk, aes(block, yield, colour = N)) + geom_point(size = 2.2, position = position_dodge(0.3)) +
  scale_colour_manual(values = c(ie_navy, ie_coral)) + labs(x = "Block", y = "Yield", colour = "Nitrogen")

tg <- ToothGrowth
tg$dose <- factor(tg$dose)
cells <- aggregate(len ~ dose + supp, tg, mean)
ggplot(tg, aes(dose, len, colour = supp, group = supp)) +
  geom_jitter(width = 0.08, alpha = 0.35) +
  geom_line(data = cells, linewidth = 1) + geom_point(data = cells, size = 3) +
  scale_colour_manual(values = c(ie_coral, ie_navy)) +
  labs(x = "Dose (mg/day)", y = "Tooth length", colour = "Supplement")

fit2 <- aov(len ~ supp * dose, data = tg)     # supp * dose = supp + dose + supp:dose
summary(fit2)

cm <- tapply(tg$len, list(tg$supp, tg$dose), mean)
gm <- mean(tg$len); n <- 10; a <- 2; b <- 3
SSA  <- b * n * sum((rowMeans(cm) - gm)^2)
SSB  <- a * n * sum((colMeans(cm) - gm)^2)
SSAB <- n * sum((cm - outer(rowMeans(cm), rep(1, b)) - outer(rep(1, a), colMeans(cm)) + gm)^2)
SSE  <- sum((tg$len - ave(tg$len, tg$supp, tg$dose))^2)
round(c(SSA = SSA, SSB = SSB, SSAB = SSAB, SSE = SSE), 2)
model.tables(fit2, "means")
