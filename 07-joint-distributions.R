source("_common.R")

ggplot(faithful, aes(waiting, eruptions)) +
  geom_point(colour = ie_navy, alpha = 0.6) +
  labs(x = "waiting time (min)", y = "eruption duration (min)")
cor(faithful$waiting, faithful$eruptions)

P <- matrix(c(.10,.15,.10,
              .20,.15,.10,
              .10,.05,.05), nrow = 3, byrow = TRUE,
            dimnames = list(x = 0:2, y = 0:2))
sum(P)
x <- row(P) - 1; y <- col(P) - 1      # value of X and Y in each cell
sum(P[x == y])                         # P(X = Y)
sum(P[x + y <= 2])                     # P(X + Y <= 2)

f <- function(x, y) x + y
inner <- function(x) sapply(x, function(xx) integrate(function(y) f(xx, y), 0, 1 - xx)$value)
integrate(inner, 0, 1)$value          # P(X + Y <= 1) = 1/3

g <- expand.grid(x = seq(0, 1, length.out = 100), y = seq(0, 1, length.out = 100))
g$f <- g$x + g$y
tri <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1))
ggplot(g, aes(x, y)) + geom_raster(aes(fill = f)) +
  geom_polygon(data = tri, fill = NA, colour = ie_coral, linewidth = 1) +
  scale_fill_gradient(low = "white", high = ie_navy, name = "density") +
  coord_equal() + labs(x = "x", y = "y")

cumP <- function(a, b) sum(P[x <= a & y <= b])
cumP(1, 1)                                   # F(1,1) = 0.60
Fxy <- function(x, y) x * y * (x + y) / 2    # CDF of the density x + y
Fxy(.5, .5)                                  # P(X <= .5, Y <= .5) = 0.125

rowSums(P)    # p_X
colSums(P)    # p_Y

P[, "2"] / sum(P[, "2"])                                   # p(X | Y = 2)
integrate(function(y) y + 1/2, 0, 1/2)$value               # example (c)

outer(rowSums(P), colSums(P))   # what the table would be under independence
max(abs(P - outer(rowSums(P), colSums(P))))

EX <- sum(x * P); EY <- sum(y * P); EXY <- sum(x * y * P)
c(EX = EX, EY = EY, EXY = EXY)

d1 <- data.frame(x = c(-1, 0, 1), y = c(1, 0, 1))
xs <- runif(300, -1, 1)
d2 <- data.frame(x = xs, y = xs^2)
p1 <- ggplot(d1, aes(x, y)) + geom_point(colour = ie_coral, size = 4) + xlim(-1.2, 1.2)
p2 <- ggplot(d2, aes(x, y)) + geom_point(colour = ie_navy, alpha = .6)
gridExtra::grid.arrange(p1, p2, ncol = 2)
cor(d2$x, d2$y)

VX <- sum(x^2 * P) - EX^2; VY <- sum(y^2 * P) - EY^2
covXY <- EXY - EX * EY
c(cov = covXY, rho = covXY / sqrt(VX * VY))

vw <- function(rho) 0.36 * .04 + 0.16 * .09 + 2 * .6 * .4 * rho * .2 * .3
sqrt(vw(c(0.5, 0, -0.5)))

set.seed(2027)
n <- 1e5
X <- rbeta(n, 3, 1)           # density 3x^2 on (0,1)
Y <- runif(n, 0, X)           # Y | X ~ U(0, X)
c(mean_Y = mean(Y), theory = 3/8, var_Y = var(Y), theory_var = 19/320)

set.seed(2027)
Xs <- rnorm(1e5, 10, 2); Ys <- rnorm(1e5, 12, 3)
S <- Xs + Ys
ggplot(data.frame(S), aes(S)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = ie_blue, alpha = .5, colour = "white") +
  stat_function(fun = dnorm, args = list(mean = 22, sd = sqrt(13)), colour = ie_coral, linewidth = 1) +
  labs(x = "S = X + Y", y = "density")
c(mean = mean(S), var = var(S))

set.seed(2027)
T <- rpois(1e5, 3) + rpois(1e5, 5)
rbind(simulated = table(factor(T, levels = 5:10)) / 1e5,
      Poisson8  = dpois(5:10, 8))

set.seed(2027)
sim <- function(rho) {
  m <- MASS::mvrnorm(500, c(0, 0), matrix(c(1, rho, rho, 1), 2))
  data.frame(x = m[, 1], y = m[, 2], rho = paste("rho =", rho))
}
d <- rbind(sim(-0.7), sim(0), sim(0.9))
ggplot(d, aes(x, y)) + geom_point(colour = ie_navy, alpha = .5, size = 0.8) +
  facet_wrap(~rho) + coord_equal()
