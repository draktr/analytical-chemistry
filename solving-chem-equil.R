# group work

library("nleqslv")

fn <- function(y) {
  Ca <- 0.1 #mols
  Va <- 25 #mL
  Cb <- 0.1 #mols 
  Vb <- 5 #mL 
  
  x <- numeric(8)
  
  x[1] <- 10^-y[1]   # H3X 
  x[3]<- 10^-y[2] #H+ 
  x[7]<- 10^-y[3] #Na+
  
  a <- x[1]
  b <- x[3]
  c <- x[7]
  
  x[2] <- ((7.50*10^(-3))*a)/b #H2X
  x[4] <- ((6.20*10^(-8))*x[2])/b #HX2-
  x[5] <- ((4.80*10^(-13))*x[4])/b #X3- 
  x[6] <- (1.0*10^(-14))/b #OH-
  x[8] <- (c*x[6])/10^(20) #NaOH 
  
  Charge <- b+c-x[2]-2*x[4]-3*x[5]-x[6]
  X_moles <- Ca*Va-(Va+Vb)*(a+x[2]+x[4]+x[5])
  Na_moles <- Cb*Vb-(Va+Vb)*(x[8]+c)
  
  return(c(Charge,X_moles,Na_moles))
}

