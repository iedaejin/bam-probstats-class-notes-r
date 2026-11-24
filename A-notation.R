c(z = qnorm(1 - 0.025), t10 = qt(1 - 0.025, 10), chisq9 = qchisq(1 - 0.05, 9),
  F4_20 = qf(1 - 0.05, 4, 20))

p <- 0.2; k <- 4
c(by_formula = (1 - p)^(k - 1) * p, dgeom = dgeom(k - 1, p))
