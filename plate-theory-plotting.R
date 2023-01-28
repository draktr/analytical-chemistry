# Plate Theory plotting

# Equilibriation stages
t <- 120
# Fraction of amount of substance in mobile phase
mu <- 0.5
# Plates
p <- seq(0, 100, length.out = 101)

# The plate total
M_pt <- function(p, t, mu) {
  M_pt <- factorial(t) / (factorial(t - p) * factorial(p)) * mu^p * (1 - mu)^(t - p)
  return(M_pt)
}

# The plate total as $p, r \to \infty$
# as p and t approach infinity, M_pt approaches normal distribution
# (continuous exponential curve)
M_pt_cont <- function(p, t, mu) {
  M_pt_cont <- 1 / sqrt(2 * pi * mu * (1 - mu) * t) * exp(-(mu * t - p)^2 / (2 * mu * (1 - mu) * t))
  return(M_pt_cont)
}

M <- M_pt(p, t, mu)
plot(x = p, y = M, ylab = "M(p,t)", xlab = "p", col = "black")

M_cont <- M_pt_cont(p, t, mu)
plot(x = p, y = M_cont, type = "l")
