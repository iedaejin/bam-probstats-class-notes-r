source("_common.R")

g <- expand.grid(i = 1:6, j = 1:6)
B <- g$i + g$j >= 10; A <- g$i == 6 | g$j == 6
mean(A)                 # P(A) = 11/36
sum(A & B) / sum(B)     # P(A | B) = 5/6

set.seed(2027)
urn <- rep(c("red", "blue"), c(4, 6))
draws <- replicate(50000, sample(urn, 3))      # 3 x 50000 matrix of draws
mean(colSums(draws == "red") == 3)             # close to 1/30
mean(draws[2, ] == "red")                      # close to 0.4

prev <- 0.01; sens <- 0.95; spec <- 0.90
p_pos <- sens * prev + (1 - spec) * (1 - prev)
c(P_pos = p_pos, P_D_given_pos = sens * prev / p_pos)
# the same, with natural frequencies for 10,000 people
N <- 10000
c(true_pos = N * prev * sens, false_pos = N * (1 - prev) * (1 - spec))

ppv <- function(pi, sens, spec) sens * pi / (sens * pi + (1 - spec) * (1 - pi))
pi <- seq(0.001, 0.5, length.out = 400)
d <- rbind(data.frame(pi, p = ppv(pi, 0.95, 0.90), test = "specificity 0.90"),
           data.frame(pi, p = ppv(pi, 0.95, 0.99), test = "specificity 0.99"))
ggplot(d, aes(pi, p, colour = test)) + geom_line(linewidth = 1) +
  geom_vline(xintercept = 0.01, linetype = 3) +
  scale_colour_manual(values = c(ie_blue, ie_coral)) +
  labs(x = "prevalence P(D)", y = "P(D | positive)", colour = NULL)
ppv(0.01, 0.95, c(0.90, 0.99))

set.seed(2027)
monty <- function(n) {
  prize <- sample(3, n, TRUE); pick <- 1
  # host opens a door that is neither the pick nor the prize
  host <- sapply(prize, function(p) { ok <- setdiff(1:3, c(pick, p)); ok[sample.int(length(ok), 1)] })
  switch_to <- 6 - pick - host
  c(stay = mean(prize == pick), switch = mean(prize == switch_to))
}
monty(1e5)

n <- 1:5
round(1 - 0.3^n, 4)
ceiling(log(0.001) / log(0.3))   # inspections needed for 99.9% detection

set.seed(2027)
n <- 200000
biased <- rbinom(n, 1, 0.5) == 1
p <- ifelse(biased, 0.8, 0.5)
h1 <- runif(n) < p; h2 <- runif(n) < p
c(P_H1 = mean(h1), P_H1H2 = mean(h1 & h2), P_H2_given_H1 = mean(h1 & h2) / mean(h1))
