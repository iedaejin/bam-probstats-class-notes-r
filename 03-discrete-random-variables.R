source("_common.R")

s <- 2:12
pmf <- (6 - abs(s - 7)) / 36
d <- data.frame(s, pmf, tail = s >= 10)
ggplot(d, aes(s, pmf, fill = tail)) + geom_col(width = 0.7) +
  scale_fill_manual(values = c(ie_blue, ie_coral), guide = "none") +
  scale_x_continuous(breaks = s) +
  labs(x = "s", y = "P(S = s)")
c(sum = sum(pmf), P_S_ge_10 = sum(pmf[s >= 10]))

Fs <- cumsum(pmf)
steps <- data.frame(x = c(1, s), y = c(0, Fs))
ggplot(steps, aes(x, y)) +
  geom_step(colour = ie_navy, linewidth = 0.9) +
  geom_point(data = data.frame(x = s, y = Fs), colour = ie_navy, size = 2) +
  geom_segment(aes(x = 12, xend = 13, y = 1, yend = 1), colour = ie_navy, linewidth = 0.9) +
  scale_x_continuous(breaks = 1:13) +
  labs(x = "x", y = expression(F[S](x)))
round(Fs * 36)

dbinom(0, 12, 0.05)
1 - pbinom(1, 12, 0.05)           # P(X >= 2) = 1 - P(X <= 1)
pbinom(1, 12, 0.05, lower.tail = FALSE)   # same thing, more robust
c(mean = 12 * 0.05, sd = sqrt(12 * 0.05 * 0.95))

k <- 0:20
d <- do.call(rbind, lapply(c(0.1, 0.5, 0.8), function(p)
  data.frame(k, prob = dbinom(k, 20, p), p = paste0("p = ", p))))
ggplot(d, aes(k, prob)) + geom_col(fill = ie_blue, alpha = 0.8) +
  facet_wrap(~p) + labs(x = "k", y = "P(X = k)")

dgeom(3, 1/6)          # P(X = 4): note k - 1 = 3
(5/6)^3 / 6            # same, by hand
1 - pgeom(5, 1/6)      # P(X > 6) = 1 - P(X <= 6), pgeom(6 - 1, ...)
(5/6)^6                # by the tail formula
mean(rgeom(1e5, 1/6) + 1)   # simulated mean, close to 6

k <- 1:20
d <- rbind(data.frame(k, prob = dgeom(k - 1, 0.5), p = "p = 0.5"),
           data.frame(k, prob = dgeom(k - 1, 0.2), p = "p = 0.2"))
ggplot(d, aes(k, prob, fill = p)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c(ie_coral, ie_blue)) +
  labs(x = "k (trial of the first success)", y = "P(X = k)", fill = NULL)

choose(6, 2) * 0.4^3 * 0.6^4
dnbinom(7 - 3, size = 3, prob = 0.4)

c(hyper = dhyper(1, m = 8, n = 32, k = 5), binom = dbinom(1, 5, 0.2))

dpois(0, 2)
1 - ppois(3, 2)                # P(X >= 4) = 1 - P(X <= 3)
ppois(3, 2, lower.tail = FALSE)

k <- 0:25
d <- do.call(rbind, lapply(c(1, 4, 10), function(l)
  data.frame(k, prob = dpois(k, l), lambda = paste0("lambda = ", l))))
ggplot(d, aes(k, prob)) + geom_col(fill = ie_green, alpha = 0.8) +
  facet_wrap(~lambda) + labs(x = "k", y = "P(X = k)")

k <- 0:12
d <- data.frame(k, binom = dbinom(k, 100, 0.03), pois = dpois(k, 3))
ggplot(d, aes(k)) + geom_col(aes(y = binom), fill = ie_blue, alpha = 0.6) +
  geom_point(aes(y = pois), colour = ie_coral, size = 2) +
  labs(x = "k", y = "probability")
max(abs(dbinom(0:100, 100, 0.03) - dpois(0:100, 3)))   # largest gap

c(exact = dbinom(0, 500, 0.004), approx = dpois(0, 2))
c(exact = 1 - pbinom(2, 500, 0.004), approx = 1 - ppois(2, 2))

# P(X >= 2) for X ~ Bin(12, 0.05): the wrong and the right way
c(wrong = 1 - pbinom(2, 12, 0.05), right = 1 - pbinom(1, 12, 0.05))
qbinom(c(0.025, 0.5, 0.975), 12, 0.05)
