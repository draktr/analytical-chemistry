library(OpenImageR)
library(tidyverse)

# Declaring images and variables used for analysis
bases <- c()
conc <- c()
imgs <- c()


# Image cropping parameters
w_from <- 250
w_to <- 1080
h_from <- 650
h_to <- 1300

# This function will get us the reference image
base_fun <- function(bases) {
  b <<- array(dim = c(length(bases), w_to-w_from+1, h_to-h_from+1, 3))
  for (i in seq(length(bases))) {
    mg_0_img <- readImage(bases[i])
    mg_0_img[ , , 2] = 0
    mg_0_img[ , , 3] = 0
    mg_0_img <- cropImage(mg_0_img, new_width = w_from:w_to, 
                          new_height = h_from:h_to, type = 'user_defined')
    b[i,,,] <<- mg_0_img
  }
  mg_0 <<- (b[1,,,] +  b[2,,,] +  b[3,,,])/3
}

# This function will prepare the calibration samples
fun <- function(imgs) {
  n_vals <<- data.frame(samples = imgs,
                        conc = conc,
                        n = numeric(length(imgs)))
  for (i in seq(length(imgs))) {
    img <- readImage(imgs[i])
    img[ , , 2] = 0
    img[ , , 3] = 0
    img <- cropImage(img, new_width = w_from:w_to, 
                     new_height = h_from:h_to, type = 'user_defined')
    
    n = sqrt(sum((img-mg_0)^2))
    n_vals$n[i] <<- n
  }
  return(n_vals)
}

# Running the functions
base_fun(bases)
fun(imgs)

# Plot with calibration samples only
samples_plot <- ggplot() +
  geom_point(aes(n_vals$conc[1:15], n_vals$n[1:15]), colour = "black", size = 4, alpha = 0.6) +
  ggtitle("[insert title") + 
  xlab("x-axis") +
  ylab("y-axis") + 
  theme_classic(base_size = 15) %+replace% 
  theme(axis.title.x = element_text(margin = margin(t = 0.8 * 5,
                                                    b = 0.8 * 5/2)),
        axis.title.y = element_text(angle = 90, 
                                    margin = margin(r = 0.8 * 5,
                                                    l = 0.8 * 5/2)),
        plot.title = element_text(size = 25, 
                                  margin = margin(b = 30)))

samples_plot

# Estimating the calibration curve parameters
cc <- lm(n_vals$n ~ n_vals$conc)
summary(cc)

# Plotting just the calibration curve (with error bars and with error ribbon commented out)
justccplot <- ggplot() +
  geom_point(aes(n_vals$conc[1:15], n_vals$n[1:15]), colour = "black", size = 4, alpha = 0.6) +
  geom_abline(intercept = cc$coefficients[1], slope = cc$coefficients[2], size = 1.5) +
  geom_errorbar(aes(x=n_vals$conc[1:15], y=predict(cc), ymax=predict(cc) + summary(cc)$sigma,
                   ymin=predict(cc) - summary(cc)$sigma), width = 4, size = 0.8) +
  #geom_ribbon(aes(x=n_vals$conc[1:15], y=predict(cc), ymax=predict(cc) + summary(cc)$sigma,
  #                ymin=predict(cc) - summary(cc)$sigma), alpha=0.2) + 
  ggtitle("[insert title]") + 
  xlab("x-axis") +
  ylab("y-axis") +
  theme_fivethirtyeight(base_size = 15) %+replace% 
  theme(axis.title.x = element_text(margin = margin(t = 0.8 * 5,
                                                    b = 0.8 * 5/2)),
        axis.title.y = element_text(angle = 90, 
                                    margin = margin(r = 0.8 * 5,
                                                    l = 0.8 * 5/2)),
        plot.title = element_text(size = 25, 
                                  margin = margin(b = 30)))


# This function calculates the values of x from the observed y
# and populates the n_vals dataframe
det_conc <- function(n_vals) {
  for (i in seq(length(n_vals$conc))){
    if (is.na(n_vals$conc[i])){
      n_vals$conc[i] <<- (n_vals$n[i]-cc$coefficients[1])/cc$coefficients[2]
    }
  }
}

det_conc(n_vals)

# Results
paste("Average value of the variable we are trying to estimate is", mean(n_vals$conc[16:18]))

# Plot with calibration samples and data samples
plot <- ggplot() +
  geom_point(aes(n_vals$conc[1:15], n_vals$n[1:15]), colour = "black", size = 4, alpha = 0.6) +
  geom_point(aes(n_vals$conc[16:18], n_vals$n[16:18]), colour = "orangered2", size = 4, alpha = 0.6) +
  geom_point(aes(n_vals$conc[19:21], n_vals$n[19:21]), colour = "palegreen3", size = 4, alpha = 0.6) +
  ggtitle("[insert title]") + 
  xlab("x-axis") +
  ylab("y-axis") + 
  theme_classic(base_size = 15) %+replace% 
  theme(axis.title.x = element_text(margin = margin(t = 0.8 * 5,
                                                    b = 0.8 * 5/2)),
        axis.title.y = element_text(angle = 90, 
                                    margin = margin(r = 0.8 * 5,
                                                    l = 0.8 * 5/2)),
        plot.title = element_text(size = 25, 
                                  margin = margin(b = 30)))

plot

# Plot with calibration samples, data samples and calibration curve
ccplot <- ggplot() +
  geom_point(aes(n_vals$conc[1:15], n_vals$n[1:15]), colour = "black", size = 4, alpha = 0.6) +
  geom_point(aes(n_vals$conc[16:18], n_vals$n[16:18]), colour = "orangered2", size = 4, alpha = 0.6) +
  geom_point(aes(n_vals$conc[19:21], n_vals$n[19:21]), colour = "palegreen3", size = 4, alpha = 0.6) +
  geom_abline(intercept = cc$coefficients[1], slope = cc$coefficients[2], size = 1.5) +
  ggtitle("[insert title]") + 
  xlab("x-axis") +
  ylab("y-axis") + 
  theme_classic(base_size = 15) %+replace% 
  theme(axis.title.x = element_text(margin = margin(t = 0.8 * 5,
                                                    b = 0.8 * 5/2)),
        axis.title.y = element_text(angle = 90, 
                                    margin = margin(r = 0.8 * 5,
                                                    l = 0.8 * 5/2)),
        plot.title = element_text(size = 25, 
                                  margin = margin(b = 30)))


ccplot



















