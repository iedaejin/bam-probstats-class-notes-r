source("_common.R")

set.seed(2027)
n <- 2000
runs <- do.call(rbind, lapply(1:3, function(r) {
  x <- rbinom(n, 1, 0.5)
  data.frame(run = factor(r), toss = 1:n, prop = cumsum(x) / seq_len(n))
}))
ggplot(runs, aes(toss, prop, colour = run)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_x_log10() +
  scale_colour_manual(values = ie_pal[1:3]) +
  labs(x = "number of tosses (log scale)", y = "proportion of heads", colour = "run")

Omega <- 1:6
E <- c(2, 4, 6); G <- c(4, 5, 6); Q <- c(2, 3, 5)
union(E, G); intersect(E, G); setdiff(Omega, Q); setdiff(E, G)
# De Morgan: complement of the union equals intersection of complements
setequal(setdiff(Omega, union(E, G)),
         intersect(setdiff(Omega, E), setdiff(Omega, G)))

grid <- expand.grid(i = 1:6, j = 1:6)
exact <- table(factor(grid$i + grid$j, levels = 2:12)) / 36
set.seed(2027)
sim <- table(factor(sample(1:6, 1e4, TRUE) + sample(1:6, 1e4, TRUE), levels = 2:12)) / 1e4
d <- data.frame(s = 2:12, exact = as.numeric(exact), sim = as.numeric(sim))
ggplot(d, aes(s)) +
  geom_col(aes(y = exact), fill = ie_blue, alpha = 0.6) +
  geom_point(aes(y = sim), colour = ie_coral, size = 2) +
  scale_x_continuous(breaks = 2:12) +
  labs(x = "sum of the two dice", y = "probability")
round(exact * 36)         # number of favourable pairs

factorial(5)                       # 5! = 120
choose(8, 3)                       # 56 committees of 3 from 8
factorial(8) / factorial(8 - 3)    # 336 ordered choices
choose(52, 5)                      # 2,598,960 poker hands
ncol(combn(8, 3))                  # same number, by enumeration
lchoose(1000, 500)                 # log of a huge binomial coefficient

1 / choose(49, 6)
choose(6, 3) * choose(43, 3) / choose(49, 6)
# the same number from R's hypergeometric function
dhyper(3, m = 6, n = 43, k = 6)

p_shared <- function(n) 1 - prod((365 - seq_len(n) + 1) / 365)
n <- 1:80
d <- data.frame(n, p = sapply(n, p_shared))
ggplot(d, aes(n, p)) +
  geom_line(colour = ie_navy, linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = ie_coral) +
  geom_vline(xintercept = 23, linetype = 3) +
  labs(x = "number of people n", y = "P(at least two share a birthday)")
p_shared(23)
min(which(d$p > 0.5))
pbirthday(23)   # built-in function agrees

set.seed(2027)
shared <- replicate(20000, anyDuplicated(sample(365, 23, replace = TRUE)) > 0)
mean(shared)     # close to 0.507
