library("nleqslv")

# acid dissociation constant(s)
# (citric acid in this case)
Ka1 <- 7.5 * 10^-4
Ka2 <- 1.7 * 10^-5
Ka3 <- 4.0 * 10^-7

# water dissociation constant
Kw <- 1.0 * 10^-14

# Titration parameters
Ca <- 0.01 # starting acid concentration (M)
Cb <- 0
Va <- 0.005 # starting acid volume (L)
Vb <- 0

# initial states (guess)
H_0 <- 1
H3X_0 <- -log(Ca)
state <- c(H_0, H3X_0)

# problem to be solved
objective <- function(state) {
  # H and H3X in terms of the variables
  H <- 10^(-state[1])
  H3X <- 10^(-state[2])

  # the concentrations of the rest of the species
  H2X <- Ka1 * H3X / H
  HX <- Ka2 * H2X / H
  X <- Ka3 * HX / H
  OH <- Kw / H

  # the two equations to be solved
  a <- (H - OH - H2X - 2 * HX - 3 * X)
  b <- Va * Ca - ((Va + Vb) * (H3X + H2X + HX + X))

  return(c(a, b))
}

solver <- function(objective, state, n) {
  for (i in 1:n) {
    equilibrium <- nleqslv(state, objective) # solving the equations
    pH_list[i] <- equilibrium$x[1] # pH is the first value of the output
    state <- equilibrium$x # updating the state
  }
  return(list(state = state, pH_list = pH_list))
}

equilibrium <- solver(objective, state, 5)

# Equilibrium value after `n` iterations of the solver
equilibrium$state
# List of pH values of the equilibrium
equilibrium$pH_list
