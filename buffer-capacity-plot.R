### Plotting Buffer Capacity ###

# dissociation constant of acid
"""
common pKa values
citric acid: 3.13, 4.76, 6.4
acetic acid: 4.75
KH2PO4: 7.2
CHES: 9.3
Borate: 9.24
"""
pKa = 4.75 # acetic acid
Ka = 10^(-pKa)   

# dissociation constant of water (at 25C)
Kw = 1.023*10^-14

# list of pH
pH = seq(1,13,length.out = 200)

# H concentrations
H = 10^(-pH)

# acid concentration
Ca = 0.05  # M
# base concentration
Cb = 0.05  # M

# buffer capacity; dCb/dpH
buffer_capacity = 2*log(10)*(Kw/H+H+(Ca+Cb)*Ka*H/(Ka+H)^2)

# plot
plot(pH, buffer_capacity, type = 'o', ylab='Buffer Capacity (M)', ylim=c(0.00,0.2))
points(x=c(pKa,pKa), y=c(0,0.5), type='l', col='red')

