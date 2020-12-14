library("nleqslv")

# acid dissociation constant(s)
# (citric acid in this case)
Ka1 = 7.5*10^-4
Ka2 = 1.7*10^-5
Ka3 = 4.0*10^-7

# water dissociation constant
Kw  = 1.0*10^-14

# parameters
Va = 0.005 # starting acid volume (L)
Ca = 0.01  # starting acid concentration (M)
Cb = 0
Vb = 0

# initial guesses 
H_0   = 1
H3X_0 = -log(Ca)
guess <- c(H_0, H3X_0) 

# problem to be solved
prob <- function(guess){
  # H and H3X in terms of the variables
  H   <- 10^(-guess[1])
  H3X <- 10^(-guess[2])
    
  # the concentrations of the rest of the species
  H2X <- Ka1*H3X/H
  HX  <- Ka2*H2X/H
  X   <- Ka3*HX/H
  OH  <- Kw/H
    
  # the two equations to be solved
  a <- (H - OH - H2X - 2*HX - 3*X)
  b <- Va*Ca-((Va+Vb)*(H3X + H2X + HX + X))
  
  return(c(a, b))
}

n = 5 # number of times to run the solver
for (i in 1:n){
  solution = nleqslv(guess, prob) # solving the equations
  pH = solution$x[1] # pH is the first value of the output
  pH_list[i] = pH # record into the list
  guess = solution$x # updating the guess
}
