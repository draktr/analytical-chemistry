#############################
### Plate Theory plotting ###
#############################


t <- 120
mu <- 0.5
p <- seq(0, 100, length.out = 101)

#' Plate total
#'
#' @param p Plates
#' @param t Equilibriation stages
#' @param mu Fraction of amount of substance in mobile phase
#'
#' @return Plate total
#' @export
#'
#' @examples
M_pt <- function(p, t, mu) {
  M_pt <- factorial(t) / (factorial(t - p) * factorial(p)) * mu^p * (1 - mu)^(t - p)
  return(M_pt)
}

#' Plate total as \deqn{p, t \to \infty}
#'
#' As p and t approach infinity, M_pt approaches normal distribution
#' (continuous exponential curve)
#'
#' @param p Plates
#' @param t Equilibriation stages
#' @param mu Fraction of amount of substance in mobile phase
#'
#' @return Asymptotical plate total
#' @export
#'
#' @examples
M_pt_cont <- function(p, t, mu) {
  M_pt_cont <- 1 / sqrt(2 * pi * mu * (1 - mu) * t) * exp(-(mu * t - p)^2 / (2 * mu * (1 - mu) * t))
  return(M_pt_cont)
}

# Calculating and plotting
M <- M_pt(p, t, mu)
plot(x = p, y = M, ylab = "M(p,t)", xlab = "p", col = "black")
M_cont <- M_pt_cont(p, t, mu)
plot(x = p, y = M_cont, type = "l")
