# Plate Theory plotting
t = 120
mu = 0.5
p = seq(0,100,length.out = 101)

# the plate total
M_pt = factorial(t)/(factorial(t-p)*factorial(p))*mu^p*(1-mu)^(t-p)
plot(x=p, y=M_pt, ylab = 'M(p,t)', col='black')

# as p and t approach infinity, M_pt approaches normal distribution 
#(continuous exponential curve)
M_pt_cont = 1/sqrt(2*pi*mu*(1-mu)*t)*exp(-(mu*t-p)^2/(2*mu*(1-mu)*t))
plot(x=p, y=M_pt_cont, type = 'l')
