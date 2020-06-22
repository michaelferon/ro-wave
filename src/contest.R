rm(list = ls())

## Load libraries.
library(tidyverse)
library(ggplot2)
library(lubridate)
library(viridis)

## Load data.
load('../data/data.Rdata')
# data <- fullData
# rm(fullData)

## Number of experiments = 8.
N <- 8
OUTPUT <- TRUE
exp.names <- c('10A', '10B', '10C', '11A', '11B', '11C', '12A', '13')




## Scatterplot matrices.
for (i in 1:N) {
  if (OUTPUT) {
    pdf(file = paste('../plots/contest/corr/corr', i,
                     '.pdf', sep = ''), height = 10, width = 10)
    set.seed(1)
    data %>%
      filter(experiment == i) %>%
      slice(sample(1:nrow(.), 10000, replace = FALSE)) %>%
      select(feed_pressure_psi, water_flux_lmh, feed_pump_pow, feed_flow_lm,
             ac_current_a) %>%
      rename(
        Pressure = feed_pressure_psi,
        Flux = water_flux_lmh,
        Power = feed_pump_pow,
        Flow = feed_flow_lm,
        Current = ac_current_a
      ) %>%
      pairs(cex = 0.01, cex.labels = 2.5, xaxt = 'n', yaxt = 'n', ann = FALSE)
    dev.off()
    
    pdf(file = paste('../plots/contest/other/other', i,
                     '.pdf', sep = ''), height = 10, width = 10)
    set.seed(1)
    data %>%
      filter(experiment == i) %>%
      slice(sample(1:nrow(.), 10000, replace = FALSE)) %>%
      select(feed_pressure_psi, feed_temp_c,
             perm_cond_low_us, rej_cond_ms, water_perm_coef) %>%
      rename(
        Pressure = feed_pressure_psi,
        Temperature = feed_temp_c,
        Permeate = perm_cond_low_us,
        Reject = rej_cond_ms,
        Permeability = water_perm_coef
      ) %>%
      pairs(cex = 0.01, cex.labels = 2.25, xaxt = 'n', yaxt = 'n', ann = FALSE)
    dev.off()
  }
}

set.seed(1)
dfcorr <- data %>%
  filter(experiment == 6) %>%
  slice(sample(1:nrow(.), 10000, replace = FALSE)) %>%
  select(feed_pressure_psi, water_flux_lmh, feed_pump_pow, feed_flow_lm,
         ac_current_a) %>%
  rename(
    Pressure = feed_pressure_psi,
    Flux = water_flux_lmh,
    Power = feed_pump_pow,
    Flow = feed_flow_lm,
    Current = ac_current_a
  )

set.seed(1)
dfother <- data %>%
  filter(experiment == 1) %>%
  slice(sample(1:nrow(.), 10000, replace = FALSE)) %>%
  select(feed_pressure_psi, feed_temp_c,
         perm_cond_low_us, rej_cond_ms, water_perm_coef) %>%
  rename(
    Pressure = feed_pressure_psi,
    Temperature = feed_temp_c,
    Permeate = perm_cond_low_us,
    Reject = rej_cond_ms,
    Permeability = water_perm_coef
  )

pdf(file = '../plots/contest/final.pdf', height = 10, width = 10)
par(mfrow = c(1, 2))
dfcorr %>% pairs(cex=0.01, cex.labels=2.25, xaxt='n', yaxt='n', ann=FALSE)
dfother %>% pairs(cex=0.01, cex.labels=2.25, xaxt='n', yaxt='n', ann=FALSE)
dev.off()


  


