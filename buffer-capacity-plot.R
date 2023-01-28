### Plotting Buffer Capacity ###


# Common pKa values
# (neg. base-10 log of acid dissociation constant):
#
# Citric acid: 3.13, 4.76, 6.4
# Acetic acid: 4.75
# KH2PO4: 7.2
# CHES: 9.3
# Borate: 9.24


# Calculates buffer capacity; B = dCb/dpH
buffer_capacity <- function(Kw, H, Ca, Cb, Ka) {
  capacity <- 2 * log(10) * (Kw / H + H + (Ca + Cb) * Ka * H / (Ka + H)^2)
  return(capacity)
}

# Calculates acid dissociation constant from pKa
get_Ka <- function(pKa) {
  Ka <- 10^(-pKa)
  return(Ka)
}

# List of pH
pH <- seq(1, 13, length.out = 200)

# pKa of Acetic acid
pKa_acetic <- 4.75

# Acetic acid dissociation constant
Ka <- get_Ka(pKa_acetic)

# Dissociation constant of water (at 25C)
Kw <- 1.023 * 10^-14

# H concentrations
H <- 10^(-pH)

# Acid concentration
Ca <- 0.05 # M
# Base concentration
Cb <- 0.05 # M

# Buffer capacity of acetic acid
capacity <- buffer_capacity(Kw, H, Ca, Cb, Ka)

plot(pH,
  capacity,
  type = "o",
  ylab = "Buffer Capacity (M)",
  ylim = c(0.00, 0.2)
)
points(x = c(pKa_acetic, pKa_acetic), y = c(0, 0.5), type = "l", col = "red")
