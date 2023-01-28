################################################
### Analysis part of the manual spectroscopy ###
################################################


library(OpenImageR)
library(tidyverse)

# Declaring images and variables used for analysis
references <- c()
concentrations <- c()
samples <- c()

#' Processes a reference images. These are used as references against
#' which to compare samples and estimate desired variables.
#'
#' @param references Vector of strings with paths to reference images
#' @param w_from Left image crop bound
#' @param w_to Right image crop bound
#' @param h_from Bottom image crop bound
#' @param h_to Top image crop bound
#'
#' @return Array of a mean reference
#' @export
#'
#' @examples
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

#' Processes sample images. Sample images are those that count as
#' spectroscopic measurements where variable of interest has been
#' manipulated.
#'
#' @param samples Vector of strings with paths to sample images
#' @param w_from Left image crop bound
#' @param w_to Right image crop bound
#' @param h_from Bottom image crop bound
#' @param h_to Top image crop bound
#'
#' @return Dataframe with sum of squares difference between each
#' of the sample images and the mean reference, concentrations
#' (independent variable) and sample names
#' @export
#'
#' @examples
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

#' Plots just the calibration curve
#' (with error bars and with error ribbon commented out)
#'
#' @param n_vals Dataframe with sum of squares difference between each
#' of the sample images and the mean reference, concentrations
#' (independent variable) and sample names
#' @param cc Calibration curve linear model `lm()` parameters
#'
#' @return Plot of calibration curve
#' @export
#'
#' @examples
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

#' Calculates the values of implied concentrations
#' and populates `n_vals` dataframe
#'
#' @param n_vals Dataframe with sum of squares difference between each
#' of the sample images and the mean reference, concentrations
#' (independent variable) and sample names
#'
#' @return Dataframe with sum of squares difference between each
#' of the sample images and the mean reference, concentrations
#' (independent variable) and sample names repopulated with
#' implied concentrations
#' @export
#'
#' @examples
determine_concentration <- function(n_vals) {
  for (i in seq(length(n_vals$conc))) {
    if (is.na(n_vals$conc[i])) {
      n_vals$conc[i] <<- (n_vals$n[i] - cc$coefficients[1]) / cc$coefficients[2]
    }
  }
  return(n_vals)
}

#' Plots calibration and data samples (without calibration curve)
#'
#' @param n_vals Dataframe with sum of squares difference between each
#' of the sample images and the mean reference, concentrations
#' (independent variable) and sample names
#'
#' @return Plot of calibration and data samples
#' @export
#'
#' @examples
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
#' Plots calibration samples, data samples and calibration curve
#'
#' @param n_vals Dataframe with sum of squares difference between each
#' of the sample images and the mean reference, concentrations
#' (independent variable) and sample names
#'
#' @return Plot with calibration samples, data samples and calibration curve
#' @export
#'
#' @examples
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

# Running the functions and conducting the analysis
mg_0 <- process_references(references)
n_vals <- process_samples(samples)
summary(cc)
plot_samples(n_vals, cc)
n_vals <- det_conc(n_vals)
calibration_plot <- plot_calibration(n_vals, cc)
data_plot <- scatterplot(n_vals)
full_plot <- plot(n_vals)
# Results
paste("Average value is:", mean(n_vals$conc[16:18]))
calibration_plot
data_plot
full_plot
