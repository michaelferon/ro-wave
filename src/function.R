rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggplot2)

load("/Users/jonathanyun/Desktop/ro-wave/data/data.Rdata")


# Water permeability Function
water_perm_coeff <- function(flux, feed_pressure, rej_cond){
  #conversion 
  rej_in_psi <- (rej_cond * 0.5) * 10
  
  water_coeff <- flux / (feed_pressure - rej_in_psi)
  return(water_coeff)
}

View(data)

data <- data %>%
          mutate(water_perm = water_perm_coeff(water_flux_lmh,
                                               feed_pressure_psi,
                                               rej_cond_ms))


