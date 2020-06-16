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

data <- data %>%
          mutate(water_perm = water_perm_coeff(water_flux_lmh,
                                               feed_pressure_psi,
                                               rej_cond_ms))




perm <- data %>%
          filter(experiment == 1) %>%
          select(perm_cond_low_us)
pressure <- data %>%
            filter(experiment == 1) %>%
            select(feed_pressure_psi)


data_ccf <- ccf(perm, pressure)

plot(data_ccf, main = "Permeate vs Pressure")

#Number of experiments 
N <- 8
OUTPUT <- TRUE
exp <- c("10A", "10B", "10C", "11A", "11B", "11C", "12A", "13")




for (i in 1:N){
  temp <- data %>%
    filter(experiment == i)
  
  perm <- temp %>%
          select(perm_cond_low_us)
  
  press <- temp %>%
          select(feed_pressure_psi)
  
  data_ccf <- ccf(perm, press)
  
  
  if(OUTPUT){
    png(file = paste('../plots/cross_correlation/permeate_pressure/permeatecond_pressure', i, '.png', sep = ''),
        height = 400, width = 1083.75)
  }
  
  plot(data_ccf, main = paste('Experiment ', exp[i], '\nCross Correlation in Permeate Conductivity and Feed Pressure'))

  if(OUTPUT){
    dev.off()
  }
}
 

for (i in 1:N){
  temp <- data %>% 
      filter(experiment == i)
  
  perm <- temp %>%
    select(perm_cond_low_us)
  
  water_coeff <- temp %>%
    select(water_perm_coef)
  
  data_ccf <- ccf(perm, water_coeff)
  
  if(OUTPUT){
    png(file = paste('../plots/cross_correlation/permeate_water_coeff/permeatecon_watercoeff', i, '.png', sep = ''),
        height = 400, width = 1083.75)
  }
  plot(data_ccf, main = paste('Experiment ', exp[i], '\nCross Correlation in Permeate Conductivity and Water Coefficient'))
  
  if(OUTPUT){
    dev.off()
  }
}


for (i in 1:N){
  temp <- data %>% 
    filter(experiment == i)
  
  press <- temp %>%
    select(feed_pressure_psi)
  
  water_coeff <- temp %>%
    select(water_perm_coef)
  
  data_ccf <- ccf(press, water_coeff)
  
  if(OUTPUT){
    png(file = paste('../plots/cross_correlation/pressure_water_coeff/pressure_watercoeff', i, '.png', sep = ''),
        height = 400, width = 1083.75)
  }
  plot(data_ccf, main = paste('Experiment ', exp[i], '\nCross Correlation in Feed Pressure and Water Coefficient'))
  
  if(OUTPUT){
    dev.off()
  }
}
