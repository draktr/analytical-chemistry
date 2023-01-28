################################
### Plotting Buffer Capacity ###
################################


#' Calculates buffer capacity.
#'
#' Buffer capacity quantifies
#' the resistance of a buffer solution to changes in pH after
#' addition of an acid or a base.
#'
#' \deqn{B = \frac{\partial C_b}{\partial pH}}
#'
#'
#' @param Kw Dissociation constant of water
#' @param H Hydrogen concentrations
#' @param Ca Acid concentrations
#' @param Cb Base concentrations
#' @param Ka Acid dissociation constant
#'
#' @return Buffer capacity
#' @export
#'
#' @examples
#'
#' buffer_capacity(Kw = 1.023 * 10^-14, H = 10^(-5), Ca = 0.05,
#'                 Cb = 0.05, Ka = 6.3 * 10^-8)
buffer_capacity <- function(Kw, H, Ca, Cb, Ka) {
  capacity <- 2 * log(10) * (Kw / H + H + (Ca + Cb) * Ka * H / (Ka + H)^2)
  return(capacity)
}

#' Calculates acid dissociation constant from pKa.
#'
#' \deqn{Ka = 10^{-pKa}}
#'
#' Common pKa values:
#' Citric acid: 3.13, 4.76, 6.4
#' Acetic acid: 4.75
#' KH2PO4: 7.2
#' CHES: 9.3
#' Borate: 9.24
#'
#' @param pKa Negative base-10 logarithm of acid dissociation constant
#'
#' @return Acid dissociation constant
#' @export
#'
#' @examples
#'
#' get_Ka(7.2)
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
