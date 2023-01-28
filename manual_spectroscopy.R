library(OpenImageR)
library(tidyverse)

# Declaring images and variables used for analysis
references <- c()
concentrations <- c()
samples <- c()

# `process_references` will get us the reference image
process_references <- function(references,
                               w_from = 250, w_to = 1080,
                               h_from = 650, h_to = 1300) {
  b <<- array(dim = c(length(references), w_to - w_from + 1, h_to - h_from + 1, 3))
  for (i in seq(length(references))) {
    mg_0_img <- readImage(references[i])
    mg_0_img[, , 2] <- 0
    mg_0_img[, , 3] <- 0
    mg_0_img <- cropImage(mg_0_img,
      new_width = w_from:w_to,
      new_height = h_from:h_to, type = "user_defined"
    )
    b[i, , , ] <<- mg_0_img
  }
  mg_0 <<- (b[1, , , ] + b[2, , , ] + b[3, , , ]) / 3
  return(mg_0)
}

# `process_samples` will prepare the calibration samples
process_samples <- function(samples,
                            w_from = 250, w_to = 1080,
                            h_from = 650, h_to = 1300) {
  n_vals <<- data.frame(
    samples = samples,
    conc = conc,
    n = numeric(length(samples))
  )
  for (i in seq(length(samples))) {
    img <- readImage(samples[i])
    img[, , 2] <- 0
    img[, , 3] <- 0
    img <- cropImage(img,
      new_width = w_from:w_to,
      new_height = h_from:h_to, type = "user_defined"
    )

    n <- sqrt(sum((img - mg_0)^2))
    n_vals$n[i] <<- n
  }
  return(n_vals)
}

# Estimating the calibration curve parameters
cc <- lm(n_vals$n ~ n_vals$conc)

# Plotting just the calibration curve (with error bars and with error ribbon commented out)
plot_calibration <- function(n_vals, cc) {
  calibration_plot <- ggplot() +
    geom_point(aes(n_vals$conc[1:15], n_vals$n[1:15]), colour = "black", size = 4, alpha = 0.6) +
    geom_abline(intercept = cc$coefficients[1], slope = cc$coefficients[2], size = 1.5) +
    geom_errorbar(aes(
      x = n_vals$conc[1:15], y = predict(cc), ymax = predict(cc) + summary(cc)$sigma,
      ymin = predict(cc) - summary(cc)$sigma
    ), width = 4, size = 0.7) +
    # geom_ribbon(aes(x=n_vals$conc[1:15], y=predict(cc), ymax=predict(cc) + summary(cc)$sigma,
    #                 ymin=predict(cc) - summary(cc)$sigma), alpha=0.2) +
    ggtitle("Calibration Plot") +
    xlab("Concentration [mg/L]") +
    ylab("Visual Difference") +
    theme_classic(base_size = 15) %+replace%
    theme(
      axis.title.x = element_text(margin = margin(
        t = 0.8 * 5,
        b = 0.8 * 5 / 2
      )),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(
          r = 0.8 * 5,
          l = 0.8 * 5 / 2
        )
      ),
      plot.title = element_text(
        size = 25,
        margin = margin(b = 30)
      )
    )
  return(calibration_plot)
}

# `det_conc` calculates the values of x from the observed y
# and populates the n_vals dataframe
det_conc <- function(n_vals) {
  for (i in seq(length(n_vals$conc))) {
    if (is.na(n_vals$conc[i])) {
      n_vals$conc[i] <<- (n_vals$n[i] - cc$coefficients[1]) / cc$coefficients[2]
    }
  }
  return(n_vals)
}

# Plot with calibration samples and data samples
scatterplot <- function(n_vals) {
  scatterplot <- ggplot() +
    geom_point(aes(n_vals$conc[1:15], n_vals$n[1:15]), colour = "black", size = 4, alpha = 0.6) +
    geom_point(aes(n_vals$conc[16:18], n_vals$n[16:18]), colour = "orangered2", size = 4, alpha = 0.6) +
    geom_point(aes(n_vals$conc[19:21], n_vals$n[19:21]), colour = "palegreen3", size = 4, alpha = 0.6) +
    ggtitle("Calibration and Data Samples") +
    xlab("Concentration [mg/L]") +
    ylab("Visual Difference") +
    theme_classic(base_size = 15) %+replace%
    theme(
      axis.title.x = element_text(margin = margin(
        t = 0.8 * 5,
        b = 0.8 * 5 / 2
      )),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(
          r = 0.8 * 5,
          l = 0.8 * 5 / 2
        )
      ),
      plot.title = element_text(
        size = 25,
        margin = margin(b = 30)
      )
    )
  return(scatterplot)
}

# Plot with calibration samples, data samples and calibration curve
plot <- function(n_vals) {
  plot <- ggplot() +
    geom_point(aes(n_vals$conc[1:15], n_vals$n[1:15]), colour = "black", size = 4, alpha = 0.6) +
    geom_point(aes(n_vals$conc[16:18], n_vals$n[16:18]), colour = "orangered2", size = 4, alpha = 0.6) +
    geom_point(aes(n_vals$conc[19:21], n_vals$n[19:21]), colour = "palegreen3", size = 4, alpha = 0.6) +
    geom_abline(intercept = cc$coefficients[1], slope = cc$coefficients[2], size = 1.5) +
    ggtitle("[insert title]") +
    xlab("x-axis") +
    ylab("y-axis") +
    theme_classic(base_size = 15) %+replace%
    theme(
      axis.title.x = element_text(margin = margin(
        t = 0.8 * 5,
        b = 0.8 * 5 / 2
      )),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(
          r = 0.8 * 5,
          l = 0.8 * 5 / 2
        )
      ),
      plot.title = element_text(
        size = 25,
        margin = margin(b = 30)
      )
    )
  return(plot)
}

# Running the functions
mg_0 <- process_references(references)
n_vals <- process_samples(samples)
summary(cc)
plot_samples(n_vals, cc)
n_vals <- det_conc(n_vals)
# Results
paste("Average value of the variable we are trying to estimate is", mean(n_vals$conc[16:18]))
